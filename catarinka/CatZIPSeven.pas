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
    fPassword: string;
  public
    function ExtractFileToStr(const pakfilename, filename: string):string;
    procedure ExtractFileToStream(const pakfilename, filename: string;
      ms: TMemoryStream);
    procedure UnpackToDir(const pakfilename, outdirname: string);
    function FileExists(const pakfilename, filename: string):boolean;
    function GetDirList(const pakfilename: string): string;
    function GetFileList(const pakfilename: string;
      includedirs: boolean = false): string;
    function CountFileExt(const pakfilename: string;
      extlist: array of string): integer;
    function CountFiles(const pakfilename: string):integer;
    constructor CreateZIP;
    constructor Create7Z;
    constructor CreateTAR;
    // classid is something like CLSID_CFormatTar
    constructor Create(const classid: TGUID);
    function SetClassByExt(const ext:string):boolean;
    destructor Destroy; override;
    property Password:string read fPassword write fPassword;
  end;

procedure Z7_PackDir(const dirname, outfilename: string;
  mask:string='*.*'; password:string='');
procedure Z7_PackDirEx(const format:string;const dirname, outfilename: string;
  mask:string='*.*'; password:string='');
procedure Z7_UnpackToDir(const pakfilename, outdirname: string; password:string='');
procedure Z7_UnpackToDirEx(const classid: TGUID;const pakfilename, outdirname: string;
  password:string='');

function Z7_FileExists(const classid: TGUID;
  const pakfilename, filename: string; password:string=''):boolean;
procedure Z7_ExtractFileToStream(ms: TMemoryStream; const classid: TGUID;
  const pakfilename, filename: string; password:string='');
function Z7_ExtractFileToStr(const classid: TGUID;
  const pakfilename, filename: string; password:string=''):string;

function Z7_FileExtToClassID(const ext:string):TGUID;
function Z7_GetDirList(const classid: TGUID; pakfilename: string): string;
function Z7_GetFileList(const classid: TGUID; pakfilename: string;
  includedirs: boolean = false): string;
function Z7_CountFiles(const classid: TGUID; const pakfilename: string):integer;
function Z7_CountFileExt(const classid: TGUID; const pakfilename: string;
  extlist: array of string): integer;


implementation

uses CatFiles, CatStrings, CatStringLoop;

type
  TExtGuid = record
    ext: array of string;
    id: TGUID;
  end;

function Z7_FileExtToClassID(const ext:string):TGUID;
  const
   clist : array [1..21] of TExtGuid =
   (
   (ext:['.zip','.jar','.xpi'];id:'{23170F69-40C1-278A-1000-000110010000}'),
   (ext:['.bz2','.bzip2','.tbz2','.tbz'];id:'{23170F69-40C1-278A-1000-000110020000}'),
   (ext:['.rar','.r00'];id:'{23170F69-40C1-278A-1000-000110030000}'),
   (ext:['.arj'];id:'{23170F69-40C1-278A-1000-000110040000}'),
   (ext:['.z','.taz'];id:'{23170F69-40C1-278A-1000-000110050000}'),
   (ext:['.lzh','.lha'];id:'{23170F69-40C1-278A-1000-000110060000}'),
   (ext:['.7z'];id:'{23170F69-40C1-278A-1000-000110070000}'),
   (ext:['.cab'];id:'{23170F69-40C1-278A-1000-000110080000}'),
   (ext:['.lzma','.lzma86'];id:'{23170F69-40C1-278A-1000-0001100A0000}'),
   (ext:['.xar'];id:'{23170F69-40C1-278A-1000-000110E10000}'),
   (ext:['.dmg'];id:'{23170F69-40C1-278A-1000-000110E40000}'),
   (ext:['.msi','.doc','.xls','.ppt'];id:'{23170F69-40C1-278A-1000-000110E50000}'),
   (ext:['.wim','.swm'];id:'{23170F69-40C1-278A-1000-000110E60000}'),
   (ext:['.iso'];id:'{23170F69-40C1-278A-1000-000110E70000}'),
   (ext:['.chm','.chi','.chq','.chw','.hxs','.hxi','.hxr','.hxq','.hxw','.lit'];id:'{23170F69-40C1-278A-1000-000110E90000}'),
   (ext:['.001'];id:'{23170F69-40C1-278A-1000-000110EA0000}'),
   (ext:['.rpm'];id:'{23170F69-40C1-278A-1000-000110EB0000}'),
   (ext:['.deb'];id:'{23170F69-40C1-278A-1000-000110EC0000}'),
   (ext:['.cpio'];id:'{23170F69-40C1-278A-1000-000110ED0000}'),
   (ext:['.tar'];id:'{23170F69-40C1-278A-1000-000110EE0000}'),
   (ext:['.gz','.gzip','.tgz','.tpz'];id:'{23170F69-40C1-278A-1000-000110EF0000}')
   );
var
 aext:string;
 i:integer;
begin
  result := TGUID.Empty;
  aext := lowercase(ext);
  for i := low(clist) to high(clist) do begin
    if matchstrinarray(aext, clist[i].ext, true) then begin
      result := clist[i].id;
      break;
    end;
  end;
end;

procedure Z7_UnpackToDirEx(const classid: TGUID;const pakfilename, outdirname: string;
  password:string='');
var
  Arch: I7zInArchive;
  i:integer;
begin
  Arch := CreateInArchive(classid);
  if password <> emptystr then begin
    Arch.SetPassword(password);
  end;
  with arch do
  begin
    OpenFile(pakfilename);
    ExtractTo(outdirname);
    Close;
  end;
end;

procedure Z7_PackDirEx(const format:string;const dirname, outfilename: string;
  mask:string='*.*'; password:string='');
var
  Arch: I7zOutArchive;
  i: integer;
  df: TStringLoop;
  guid:TGUID;
begin
  guid := Z7_FileExtToClassID(format);
  df := TStringLoop.Create;
  Arch := CreateOutArchive(guid);
  if password <> emptystr then begin
    Arch.SetPassword(password);
    SetEncryptionMethod(arch, emAES256);
  end;
  GetFiles(dirname + '\' + mask, df.List);
  while df.Found do
  begin
    Arch.AddFile(dirname + '\' + df.current, df.current);
  end;
  df.Free;
  // Arch.AddStream(ms, soReference, faArchive, CurrentFileTime, CurrentFileTime, infilename, false, false);
  Arch.SaveToFile(outfilename);
end;

procedure Z7_PackDir(const dirname, outfilename: string; mask:string='*.*';
  password:string='');
begin
  Z7_PackDirEx(extractfileext(outfilename), dirname, outfilename, mask, password);
end;

procedure Z7_UnpackToDir(const pakfilename, outdirname: string; password:string='');
begin
  Z7_UnpackToDirEx(Z7_FileExtToClassID(extractfileext(pakfilename)), pakfilename,
    outdirname);
end;

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
    Close;
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
    Close;
  end;
  result := sl.text;
  sl.Free;
end;

function Z7_FileExists(const classid: TGUID;
  const pakfilename, filename: string; password:string=''):boolean;
var
  i: integer;
  afilename: string;
begin
  result := false;
  afilename := FixPackPath(filename);

  with CreateInArchive(classid) do
  begin
    if password <> emptystr then
      SetPassword(password);
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if (ItemIsFolder[i] = false) and (ItemPath[i] = filename) then
      begin
        Result := true;
        break;
      end;
    Close;
  end;
end;

procedure Z7_ExtractFileToStream(ms: TMemoryStream; const classid: TGUID;
  const pakfilename, filename: string; password:string='');
var
  i: integer;
  afilename: string;
begin
  afilename := FixPackPath(filename);

  with CreateInArchive(classid) do
  begin
    if password <> emptystr then
      SetPassword(password);
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if (ItemIsFolder[i] = false) and (ItemPath[i] = filename) then
      begin
        ExtractItem(i, ms, false);
        ms.Position := 0;
        break;
      end;
    Close;
  end;
end;

function Z7_ExtractFileToStr(const classid: TGUID;
  const pakfilename, filename: string; password:string=''):string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  Z7_ExtractFileToStream(ms, classid, pakfilename, filename, password);
  result := MemStreamToStr(ms);
  ms.Free;
end;

function Z7_CountFiles(const classid: TGUID; const pakfilename: string):integer;
var
  i: integer;
begin
  result := 0;
  with CreateInArchive(classid) do
  begin
    OpenFile(pakfilename);
    for i := 0 to NumberOfItems - 1 do
      if (ItemIsFolder[i] = false) then
      Inc(result);
    Close;
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
  Z7_ExtractFileToStream(ms, fClassID, pakfilename, filename, fPassword);
end;

procedure TCat7Z.UnpackToDir(const pakfilename, outdirname: string);
begin
  Z7_UnpackToDirEx(fClassID,pakfilename,outdirname,fPassword);
end;

function TCat7Z.ExtractFileToStr(const pakfilename, filename: string):string;
begin
  result := Z7_ExtractFileToStr(fClassID, pakfilename, filename, fPassword);
end;

function TCat7Z.FileExists(const pakfilename, filename: string):boolean;
begin
  result := Z7_FileExists(fClassID, pakfilename, filename, fPassword);
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

function TCat7Z.CountFiles(const pakfilename: string):integer;
begin
 result := Z7_CountFiles(fClassID, pakfilename);
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
  fPassword := emptystr;
end;

function TCat7Z.SetClassByExt(const ext:string):boolean;
var cl:TGUID;
begin
  result := false;
  cl := Z7_FileExtToClassID(ext);
  if cl <> TGUID.Empty then begin
    result := true;
    fClassID := cl;
  end;
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

