unit CatTarman;

{
  TAR File Format Management Lua library
  Copyright (c) 2013-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  SysUtils, Classes;

procedure DirToTAR(directory, tarfilename: string; mask: string = '*.*');
procedure TARToDir(tarfilename, directory: string);
procedure ExtractTARFileToStream(const tarfilename, filename: string;
  ms: TMemoryStream);
function TAR_GetDirList(tarfilename:string):string;
function TAR_GetFileList(tarfilename:string; includedirs:boolean=false):string;

implementation

uses CatFiles, CatStrings, LibTar, CatStringLoop;

function FixTarPath(const path:string):string;
begin
  result := replacestr(path, '\', '/');
end;

function ReverseTarPath(const path:string):string;
begin
  result := replacestr(path, '/', '\');
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
      if List.IndexOf(directory + search.Name) = -1 then
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

// Creates a TAR file with the contents of a directory
// Mask can be single one or multiple separated by pipe like: *.txt|*.etc
procedure DirToTAR(directory, tarfilename: string; mask: string = '*.*');
var
  tw: TTarWriter;
  slp: TStringLoop;
  masks: TSepStringLoop;
begin
  slp := TStringLoop.create;
  if endswith(directory, '\') = false then
    directory := directory + '\';

  masks := TSepStringLoop.Create(mask);
  while masks.Found do
  GetAllFiles(slp.list, directory + masks.Current);
  masks.Free;
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
    forcedir(ExtractFilePath(directory + ReverseTarPath(string(DirRec.Name))));
    ta.ReadFile(directory + string(DirRec.Name));
    // writeln(directory+DirRec.Name)
  end;
  ta.Free;
end;

function TAR_GetFileList(tarfilename:string; includedirs:boolean=false):string;
var
  ta: TTarArchive;
  DirRec: TTarDirRec;
  sl : TStringList;
begin
  sl := TStringList.Create;
  ta := TTarArchive.create(tarfilename);
  ta.Reset;
  while ta.FindNext(DirRec) do
  begin
    if DirRec.FileType = ftNormal then
     sl.Add(DirRec.Name);
    if (DirRec.FileType = ftDirectory) and (includedirs = true) then
     sl.Add(DirRec.Name);
  end;
  result := sl.Text;
  ta.Free;
  sl.Free;
end;

function TAR_GetDirList(tarfilename:string):string;
var
  ta: TTarArchive;
  DirRec: TTarDirRec;
  sl : TStringList;
begin
  sl := TStringList.Create;
  ta := TTarArchive.create(tarfilename);
  ta.Reset;
  while ta.FindNext(DirRec) do
  begin
    if (DirRec.FileType = ftDirectory) then
     sl.Add(DirRec.Name);
  end;
  result := sl.Text;
  ta.Free;
  sl.Free;
end;

procedure ExtractTARFileToStream(const tarfilename, filename: string;
  ms: TMemoryStream);
var
  ta: TTarArchive;
  DirRec: TTarDirRec;
  sl : TStringList;
  afilename:string;
begin
  afilename := FixTarPath(filename);
  sl := TStringList.Create;
  ta := TTarArchive.create(tarfilename);
  ta.Reset;
  while ta.FindNext(DirRec) do
  begin
    if (afilename = DirRec.Name) and (DirRec.FileType = ftNormal) then begin
      ta.ReadFile(ms);
      ms.Position := 0;
      break;
    end;
  end;
  ta.Free;
  sl.Free;
end;

end.
