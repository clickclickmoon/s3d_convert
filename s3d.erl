%%% s3d.erl - Andy B (neorab) <neorab@gmail.com> - Oct 8th, 2009
%%%============================================================================
%%% Module for reading in data archives in the s3d file format used by the
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

%% Data File
%%-----------------------------------------------------------------------------
%% Nested records cause unreadable code in erlang, in my honest opinion so I 
%% flattened the meta data into the data file as seen below.  The first three
%% items are obviously from the file_meta record while the file_name and data 
%% are added as they code finds them available.
-record(data_file, {
  checksum = 0,
  offset = 0,
  file_size = 0,
  file_name = [],
  data = []
}).


%% unpack/1 [exported]
%%-----------------------------------------------------------------------------
unpack(Filename) ->
  % First, Read in the given archive file.
  {ok, S3D} = file:read_file(Filename),

  % Parse the header file and store it for later use.
  {ok, Header} = parse_header(S3D),
  
  % Collect the list of file meta data from the meta block
  {ok, MetaList} = parse_meta_listing(S3D, Header),

  % Collect and unzip the data blocks into files
  {ok, DataFiles} = collect_data_files(S3D, MetaList),

  % Parse the directory and Save the Filename list
  {ok, FileDirectory} = process_directory(DataFiles),

  % Attach the file names to all of the data files.
  {ok, FileList} = attach_names_to_files(FileDirectory, DataFiles),

  % Create the folder name by removing the .s3d from the given archive
  {FolderName, _Extension} = lists:splitwith(fun(X) -> X /= 46 end, Filename),
  file:make_dir(FolderName),

  % This is a fun to make the file and fill it with the data file data
  PrintFilesFun = fun(X) ->
    file:write_file(FolderName ++ "/" ++ X#data_file.file_name, X#data_file.data)
  end,

  % Which we run on each file, creating the unpacked archive
  lists:map(PrintFilesFun, FileList),
  io:format("Exported files to: ~s/~n", [FolderName]).


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


%% parse_meta_listing/2 -> {ok, Listing} Listing is_list
%%-----------------------------------------------------------------------------
%% Ok, so we have the header for the file, which includes a pointer to another
%% location in the file.  This location turns out to be two things.  The item
%% _at_ this location is an item that contains the number of files contained in
%% the archive.  Secondly, it is the start of the file listing section.
%% Jump to the position pointed to by the header, grab the first number there
%% and save the rest of the meta block.  Then we start a parsing loop, telling
%% it how many meta blocks we expect to find along with the whole meta block.
parse_meta_listing(S3D, Header) ->
  {_, FileListing} = split_binary(S3D, Header#pack_header.file_listing_offset),
  <<EntryCount:32/little, FileList/binary>> = FileListing,
  meta_data_parse_loop(EntryCount, FileList).


%% meta_data_parse_loop/ -> {ok, Files} Files is_list
%%-----------------------------------------------------------------------------
%% Wrapper function to kick start the real loop for just a bit of clarity in
%% the callin function.
meta_data_parse_loop(EntryCount, FileList) ->
  meta_data_parse_loop(EntryCount, FileList, []).

%% meta_data_parse_loop/3 -> {ok, Files} Files is_list
%%-----------------------------------------------------------------------------
%% First, split the data block into two sections, the current file and the rest
%% of the files.  With the current meta block, pull out the information and add
%% it to a record and place it in the list of blocks.  Then recurse through
%% using the remaining blocks and pass the list to the next level.  We use the
%% entry count (decrementing it each time) to check that we are done.  When it
%% gets to zero, return {ok, Files}.
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


%% collect_data_files/2
%%-----------------------------------------------------------------------------
%% We take the S3D file and the meta data list.  First we construct a fun to
%% call parse_file_data on a single meta data block, returning a file record
%% that contains the data and header merged together.
collect_data_files(S3D, MetaList) ->
  ParseFileDataFun = fun(X) ->
    {ok, FileData} = parse_file_data(S3D, X),
    FileData
  end,
  DataFiles = lists:map(ParseFileDataFun, MetaList),
  {ok, DataFiles}.


%% parse_file_data/2 -> {ok, #file}
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
%% returns.  Using the inflated size data to detirmine when to stop.  You may
%% notice that each block is uncompressed as it is read in and joined without
%% any header data.  This does cause a rather large memory load.
collect_file(_, 0, FileData) ->
  {ok, FileData};
collect_file(DataBlocks, BytesLeft, FileData) ->
  <<BlockDeflatedSize:32/little,
    BlockInflatedSize:32/little,
    HeaderlessData/binary>> = DataBlocks,
  {ThisDataBlock, MoreDataBlocks} = split_binary(HeaderlessData, BlockDeflatedSize),
  ThisDataList = binary_to_list(zlib:uncompress(ThisDataBlock)),
  NewFileData = lists:append(FileData, ThisDataList),
  collect_file(MoreDataBlocks, BytesLeft - BlockInflatedSize, NewFileData).


%% process_directory/1 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% The last file (by offset) is a file that contains the list of filenames that
%% are in the archive.  So first we have to find the file directory.  To do 
%% that we create a list of the offsets, find the max and search for the file
%% that matches that max offset.  After that we drop the first 3 bytes of the 
%% file, I'm not sure what it is, but it's garbage.  We give what we have left
%% to pull_file_list, which will give us a list of strings with filenames.
process_directory(Files) ->
  OffsetList = [ X#data_file.offset || X <- Files],
  DirListOffset = lists:max(OffsetList),
  {value, DirList} = lists:keysearch(DirListOffset, #data_file.offset, Files),

  {_, RealFileList} = lists:split(3, DirList#data_file.data),

  pull_file_list(RealFileList).


%% pull_file_list/1 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% Convienence wrapper so for clarity in caller.
pull_file_list(FileListData) ->
  pull_file_list(FileListData, []).

%% pull_file_list/2 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% This is a loop function that will eat the first 5 bytes, which is separater
%% data.  It will then take the file name, which is found by taking all the 
%% chracters until a 0 is found (the common string terminator) and attaches
%% that file name to a collection (list) of them.  When it's down to a single
%% terminator left to parse, it returns the list, reversing it first so that
%% the list is the same order as the files are listed in the directory.
pull_file_list([0], FileList) ->
  {ok, lists:reverse(FileList)};
pull_file_list(RawFileList, FileList) ->
  {_, FirstByteCharList} = lists:split(5, RawFileList),
  {ThisFile, RestFileList} = lists:splitwith(fun(X) -> X /= 0 end, FirstByteCharList),
  NewFileList = [ ThisFile | FileList ],
  pull_file_list(RestFileList, NewFileList).


%% attach_names_to_files/2 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% Given the list of data files and the list of filenames, the goal here is to
%% attach the correct filename to each file.  To do that we have to sort the 
%% data files first.  The data files are in filename CRC order, we need them in
%% offset order.  Then we remove the last file in the list, which is the file
%% that contains the file names, which doesn't have a file name itself.  Then
%% we send the two lists to a function that will merge them and return the data
%% files with filenames attached.
attach_names_to_files(Filenames, Files) ->
  SortByOffsetFun = fun(X, Y) -> X#data_file.offset < Y#data_file.offset end,
  SortedFiles = lists:sort(SortByOffsetFun, Files),
  {DataFiles, _DirFileNames} = lists:split(length(Files) - 1, SortedFiles),
  merge_files_and_names(Filenames, DataFiles).


%% merge_files_and_names/2 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% Convienence wrapper so for clarity in caller.
merge_files_and_names(Filenames, DataFiles) ->
  merge_files_and_names(Filenames, DataFiles, []).

%% merge_files_and_names/3 -> {ok, FileList} FileList is_list
%%-----------------------------------------------------------------------------
%% Pluck the first item off of each list, merge the file name into the data 
%% file, build up the list, and return it when we're out of files.
merge_files_and_names([], [], Merged)->
  {ok, Merged};
merge_files_and_names(FileNames, Files, Merged) ->
  [FirstName | RestNames] = FileNames,
  [FirstFile | RestFiles] = Files,
  NewFile = FirstFile#data_file{ file_name = FirstName },
  NewMerged = [NewFile | Merged],
  merge_files_and_names(RestNames, RestFiles, NewMerged).
