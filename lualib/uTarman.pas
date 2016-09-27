unit uTarman;

{
  TAR File Format Management Lua library
  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Lua, SysUtils, Classes;

procedure RegisterTarman(L: plua_State);
function luaopen_Tarman(L: plua_State): integer; cdecl;
function Lua_TARToDir(L: plua_State): integer; cdecl;
function Lua_DirToTAR(L: plua_State): integer; cdecl;

implementation

uses CatFiles, CatStrings, LibTar, CatStringLoop;

procedure RegisterTarman(L: plua_State);
begin
  luaopen_Tarman(L);
end;

procedure GetAllFiles(list: tstringlist; mask: string);
var
  search: TSearchRec;
  directory: string;
begin
  directory := ExtractFilePath(mask);
  // find all files
  if FindFirst(mask, $23, search) = 0 then
  begin
    repeat
      // add the files to the list
      list.Add(directory + search.Name);
      // Inc(Count);
    until FindNext(search) <> 0;
  end;

  // Subdirectories
  if FindFirst(directory + '*.*', faDirectory, search) = 0 then
  begin
    repeat
      if ((search.Attr and faDirectory) = faDirectory) and
        (search.Name[1] <> '.') then
        GetAllFiles(list, directory + search.Name + '\' +
          ExtractFileName(mask));
    until FindNext(search) <> 0;
    FindClose(search);
  end;
end;

procedure DirToTAR(directory, tarfilename: string; mask: string = '*.*');
var
  tw: TTarWriter;
  slp: TStringLoop;
begin
  slp := TStringLoop.create;
  if endswith(directory, '\') = false then
    directory := directory + '\';
  GetAllFiles(slp.list, directory + mask);
  // writeln(slp.list.text);
  tw := TTarWriter.create(tarfilename);
  while slp.Found do
  begin
    // writeln('file:'+directory+slp.current);
    // writeln('tarfile:'+slp.current);
    tw.AddFile(slp.Current, ansistring(after(slp.Current, directory)));
  end;
  // TW.AddFile('Readme.htm','Test\Test.txt');
  tw.Free;
  slp.Free;
end;

procedure TARToDir(tarfilename, directory: string);
var
  ta: TTarArchive;
  DirRec: TTarDirRec;
begin
  if endswith(directory, '\') = false then
    directory := directory + '\';
  ta := TTarArchive.create(tarfilename);
  ta.Reset;
  while ta.FindNext(DirRec) do
  begin
    // writeln(extractfilepath(directory+DirRec.Name));
    forcedir(ExtractFilePath(directory + replacestr(string(DirRec.Name), '/', '\')));
    ta.ReadFile(directory + string(DirRec.Name));
    // writeln(directory+DirRec.Name)
  end;
  ta.Free;
end;

function Lua_TARToDir(L: plua_State): integer; cdecl;
begin
  TARToDir(lua_tostring(L, 1), lua_tostring(L, 2));
  result := 1;
end;

function Lua_DirToTAR(L: plua_State): integer; cdecl;
begin
  if lua_isnone(L, 3) then
    DirToTAR(lua_tostring(L, 1), lua_tostring(L, 2))
  else
    DirToTAR(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function luaopen_Tarman(L: plua_State): integer; cdecl;
begin
  lua_register(L, 'dir2tar', Lua_DirToTAR);
  lua_register(L, 'tar2dir', Lua_TARToDir);
  result := 0;
end;

end.
