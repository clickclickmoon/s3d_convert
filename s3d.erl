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

%% File Header
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
-record(file_header, {
  file_listing_offset = 0,
  pfs_string = ?PFSString,
  file_flags = ?DefaultFileFlags
}).


unpack(Filename) ->
  {ok, S3D} = file:read_file(Filename),
  {ok, Header} = parse_header(S3D).  


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

  Header = #file_header{
    file_listing_offset = FileListingOffset,
    pfs_string = PFSString,
    file_flags = FileFlags
  },

  case Header#file_header.pfs_string of
    ?PFSString ->
      {ok, Header};
    _ ->
      {error, bad_header}
  end.


%% parse_entry_count/2 -> {ok, EntryCount}
%%-----------------------------------------------------------------------------
%% Ok, so the 
