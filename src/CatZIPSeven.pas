unit CatZipSeven;

{
  7Z Multi Compressed File Format Manipulation library
  Copyright (c) 2013-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  SysUtils, Classes, SevenZIP;

type
  TCat7Z = class
  private
    fClassID: TGUID;
  public
    procedure ExtractFileToStream(const pakfilename, filename: string;
      ms: TMemoryStream);
    function GetDirList(const pakfilename: string): string;
    function GetFileList(const pakfilename: string;
      includedirs: boolean = false): string;
    function CountFileExt(const pakfilename: string;
      extlist: array of string): integer;
    constructor CreateZIP;
    constructor Create7Z;
    constructor CreateTAR;
    // classid is something like CLSID_CFormatTar
    constructor Create(const classid: TGUID);
    destructor Destroy; override;
  end;

procedure Z7_ExtractFileToStream(const classid: TGUID;
  const pakfilename, filename: string; ms: TMemoryStream);
function Z7_GetDirList(const classid: TGUID; pakfilename: string): string;
function Z7_GetFileList(const classid: TGUID; pakfilename: string;
  includedirs: boolean = false): string;
function Z7_CountFileExt(const classid: TGUID; const pakfilename: string;
  extlist: array of string): integer;


implementation

uses CatFiles, CatStrings, CatStringLoop;

function FixPackPath(const path: string): string;
begin
  result := replacestr(path, '\', '/');
end;

function ReversePackPath(const path: string): string;
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
      if list.IndexOf(directory + search.Name) = -1 then
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

function Z7_GetFileList(const classid: TGUID; pakfilename: string;
  includedirs: boolean = false): string;
var
  sl: tstringlist;
  i: integer;
begin
  sl := tstringlist.Create;
  with CreateInArchive(classid) do
  begin
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if ItemIsFolder[i] = true then
      begin
        if (includedirs = true) then
          sl.Add(ItemPath[i]);
      end
      else
      begin
        sl.Add(ItemPath[i]);
      end;
  end;
  result := sl.text;
  sl.Free;
end;

function Z7_GetDirList(const classid: TGUID; pakfilename: string): string;
var
  sl: tstringlist;
  i: integer;
begin
  sl := tstringlist.Create;
  with CreateInArchive(classid) do
  begin
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if ItemIsFolder[i] = true then
        sl.Add(ItemPath[i]);
  end;
  result := sl.text;
  sl.Free;
end;

procedure Z7_ExtractFileToStream(const classid: TGUID;
  const pakfilename, filename: string; ms: TMemoryStream);
var
  i: integer;
  afilename: string;
begin
  afilename := FixPackPath(filename);

  with CreateInArchive(classid) do
  begin
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if (ItemIsFolder[i] = false) and (ItemPath[i] = filename) then
      begin
        ExtractItem(i, ms, false);
        ms.Position := 0;
        break;
      end;
  end;
end;

function Z7_CountFileExt(const classid: TGUID; const pakfilename: string;
  extlist: array of string): integer;
var
  slp: TStringLoop;
  ext: string;
begin
  result := 0;
  slp := TStringLoop.Create;
  slp.list.text := Z7_GetFileList(classid, pakfilename);
  slp.Reset;
  while slp.Found do
  begin
    ext := extractfileext(slp.Current);
    if MatchStrInArray(ext, extlist) then
      inc(result);
  end;
  slp.Free;
end;

{ ------------------------------------------------------------------------------ }

{ TCat7Z }

procedure TCat7Z.ExtractFileToStream(const pakfilename, filename: string;
  ms: TMemoryStream);
begin
  Z7_ExtractFileToStream(fClassID, pakfilename, filename, ms);
end;

function TCat7Z.GetDirList(const pakfilename: string): string;
begin
  result := Z7_GetDirList(fClassID, pakfilename);
end;

function TCat7Z.GetFileList(const pakfilename: string;
  includedirs: boolean = false): string;
begin
  result := Z7_GetFileList(fClassID, pakfilename, includedirs);
end;

function TCat7Z.CountFileExt(const pakfilename: string;
  extlist: array of string): integer;
begin
  result := Z7_CountFileExt(fClassID, pakfilename, extlist);
end;

constructor TCat7Z.Create(const classid: TGUID);
begin
  inherited Create;
  fClassID := classid;
end;

constructor TCat7Z.CreateZIP;
begin
  inherited Create;
  fClassID := CLSID_CFormatZip;
end;

constructor TCat7Z.Create7Z;
begin
  inherited Create;
  fClassID := CLSID_CFormat7Z;
end;

constructor TCat7Z.CreateTAR;
begin
  inherited Create;
  fClassID := CLSID_CFormatTar;
end;

destructor TCat7Z.Destroy;
begin
  inherited;
end;

end.
