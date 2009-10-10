%%% s3d.erl - Andy B (neorab) <neorab@gmail.com> - Oct 8th, 2009
%%%============================================================================
%%% Module for reading in or saving archives in the s3d file format used by the
%%% EverQuest client.  Developed against s3d files from the original EverQuest
%%% release disc, this may or may not work with later resource files.
%%%
%%% The s3d file format is actually pretty straight forward, all of the 
%%% important information seems to be in 32bit chunks.  Each of these chunks
%%% will be called items throughout the documentation of the code.  
%%%
%%% I owe all of my knowledge to WindCatcher from the EQEmu forums, his work 
%%% make this file trivial to walk through and his provided code is the basis
%%% of this module.  His tool is called S3DSpy and can be found on the forums 
%%% still and was built with Delphi.  I'm not particularly fond of Delphi and
%%% I wanted to better understand the format and process better anyway. So...

-module(s3d).
-author("Andy B (neorab) <neorab@gmail.com>").

-export([unpack/1]).

%% Pack Header
%%-----------------------------------------------------------------------------
%% The file header is seen once at the begining of the file.  It contains three
%% values, of which one is of critical importance.  The first item is the file
%% listing offset.  This pointer references the location where the file listing
%% starts.  The second item is always 542328400 which is the string 'PDF ' with
%% one byte characters.  The third item is kind of an unknown.  WindCatcher was
%% of the opinion that it would always be 131072 and I can confirm that this
%% the case with the files from the original client.  I suspect that this item
%% contains a set of flags for the file, but cannot be certain.
-define(PFSString, 542328400).
-define(DefaultFileFlags, 131072).
-record(pack_header, {
  file_listing_offset = 0,
  pfs_string = ?PFSString,
  file_flags = ?DefaultFileFlags
}).

%% File Meta
%%-----------------------------------------------------------------------------
%% The bottom useful block of data is a collection of entries that each contain
%% three items.  The entries in this block are meta data for the files within
%% the package.  The first item is a checksum, from the CRC IEEE 802.3 Ethernet
%% CRC-32 spec (WindCatcher, wtg!) [CHECKSUM ON COMPRESSED OR INFLATED?]  The
%% second item is a pointer to where the data block starts for that file.  The
%% third item is the inflated size of the file.
-define(MetaBlockSize, 12).
-record(file_meta, {
  checksum = 0,
  offset = 0,
  file_size = 0
}).

%% File
%%-----------------------------------------------------------------------------
-record(data_file, {
  checksum = 0,
  offset = 0,
  file_size = 0,
  file_name = "",
  data = []
}).


unpack(Filename) ->
  {ok, S3D} = file:read_file(Filename),
  {ok, Header} = parse_header(S3D),
  {ok, FileList} = parse_file_listing(S3D, Header),
  ParseFileDataFun = fun(X) ->
    {ok, FileData} = parse_file_data(S3D, X),
    FileData
  end,
  Files = lists:map(ParseFileDataFun, FileList),
  process_directory(Files).



%% parse_header/1 -> {ok, Header} or {error, reason_atom}
%%-----------------------------------------------------------------------------
%% As noted already, the first three items make up the s3d file header.  So we
%% grab them out, populate a header record, verify that the PFS string is right
%% and return the header.  Easy.
parse_header(S3D) ->
  <<FileListingOffset:32/little, 
    PFSString:32/little, 
    FileFlags:32/little, 
    _/binary>> = S3D,

  Header = #pack_header{
    file_listing_offset = FileListingOffset,
    pfs_string = PFSString,
    file_flags = FileFlags
  },

  case Header#pack_header.pfs_string of
    ?PFSString ->
      {ok, Header};
    _ ->
      {error, bad_header}
  end.


%% parse_file_listing/2 -> {ok, Listing} Listing is_list
%%-----------------------------------------------------------------------------
%% Ok, so we have the header for the file, which includes a pointer to another
%% location in the file.  This location turns out to be two things.  The item
%% _at_ this location is an item that contains the number of files contained in
%% the archive.  Secondly, it is the start of the file listing section. Well,
%% the next item is, but at least we're already right there.
parse_file_listing(S3D, Header) ->
  {_, FileListing} = split_binary(S3D, Header#pack_header.file_listing_offset),
  <<EntryCount:32/little,
    FileList/binary>> = FileListing,
  
  meta_data_parse_loop(EntryCount, FileList, []).


%% meta_data_parse_loop/3 -> {ok, Files} Files is_list
%%-----------------------------------------------------------------------------
%% This function takes the File Listing block and pulls all of the meta data 
%% out of the block and puts it into a nice little list.
meta_data_parse_loop(0, _, Files) ->
  {ok, Files};
meta_data_parse_loop(NumLeft, BlockLeft, Files) ->
  {FileItem, MoreBlock} = split_binary(BlockLeft, ?MetaBlockSize),
  <<Checksum:32/little,
    Offset:32/little,
    FileSize:32/little>> = FileItem,

  ItemMeta = #file_meta{
    checksum = Checksum,
    offset = Offset,
    file_size = FileSize
  },

  AllFiles = [ItemMeta | Files],
  meta_data_parse_loop(NumLeft - 1, MoreBlock, AllFiles).


%% collect_file/2 -> {ok, #file}
%%-----------------------------------------------------------------------------
%% Now that we (presumably) have our list of file meta data entries, we now
%% pull files from the archive.  The data section is broken into smaller blocks
%% that are linked together to form the main file.  Each data block has a small
%% header that contains the deflated and inflated sizes.  A file may take more
%% than a single block to build.  We use the data block's inflated length and
%% use that to make sure we have a full file, since we have the file's total
%% inflated size.
parse_file_data(S3D, MetaData) ->
  {_, DataBlocks} = split_binary(S3D, MetaData#file_meta.offset),
  {ok, FileData} = collect_file(DataBlocks, MetaData#file_meta.file_size, []),
  DataFileRec = #data_file{
    checksum = MetaData#file_meta.checksum,
    offset = MetaData#file_meta.offset,
    file_size = MetaData#file_meta.file_size,
    data = FileData
  },
  {ok, DataFileRec}.

%% collect_file/3 -> {ok, File}
%%-----------------------------------------------------------------------------
%% Reads all of the datablocks for a file into a single list of bytes and
%% returns it.  Using the inflated size data to detirmine when to stop.
collect_file(_, 0, FileData) ->
  {ok, FileData};
collect_file(DataBlocks, BytesLeft, FileData) ->
  <<BlockDeflatedSize:32/little,
    BlockInflatedSize:32/little,
    HeaderlessData/binary>> = DataBlocks,
  {ThisDataBlock, MoreDataBlocks} = split_binary(HeaderlessData, BlockDeflatedSize),
  ThisDataList = binary_to_list(ThisDataBlock),
  NewFileData = lists:append(FileData, ThisDataList),
  collect_file(MoreDataBlocks, BytesLeft - BlockInflatedSize, NewFileData).


process_directory(Files) ->
  OffsetList = [ X#data_file.offset || X <- Files],
  DirListOffset = lists:max(OffsetList),
  {value, DirList} = lists:keysearch(DirListOffset, #data_file.offset, Files),
  Binary = list_to_binary(DirList#data_file.data),
  FileList = binary_to_list(zlib:uncompress(Binary)).
  
