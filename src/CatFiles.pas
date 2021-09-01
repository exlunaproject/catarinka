unit CatFiles;
{
  Catarinka - File System functions

  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  CopyAfterFirstLine and WipeFile functions by Peter Below
  FileCopy function by David Stidolph
}

interface

{$I Catarinka.inc}

uses
{$IF CompilerVersion > 22}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShellAPI,
  System.IOUtils;
{$ELSE}
  Windows, Classes, SysUtils, ShellAPI;
{$IFEND}
function CleanFilename(const filename: string;
  const invCharRep: Char = '_'): string;
function DeleteFolder(const dir: string): boolean;
function DirExists(const dir: string): boolean;
function DirIsEmpty(const dir: string): boolean;
function FileCanBeOpened(const filename: String): boolean;
function FileCopy(const source, dest: string): boolean;
function FilenameToMimeType(const filename: string): string;
function ForceDir(const dir: string): boolean;
function GetDirAge(const Dir: string): TDateTime;
function GetDiskSerialNumber(const drive: string): string;
function GetDllFilename:string;
function GetFileSize(const filename: string): Int64;
function GetFileToStr(const filename: string): string;
function GetFileVersion(const filename: string;
  const ResFormat: string = '%d.%d.%d.%d'): string;
function GetSizeDescription(const bytes: int64): string;
function GetSizeDescriptionBytes(const desc: string): int64;
function GetTextFileLinesCount(const filename: string): integer;
function GetTempFile(const ext: string): string;
function GetWindowsTempDir: string;
function SendToLog(const filename: TFilename; const s: string): boolean;
function SL_LoadFromFile(const SL: TStrings; const filename: string)
  : boolean;
function SL_SaveToFile(const SL: TStrings; const filename: string): boolean;
procedure CatReadLn(const f: Text; var s: string);
procedure CopyAfterFirstLine(const sourcefile, targetfile: string;
  appendln: boolean = false; lnstr: string = '');
procedure GetDirs(const dir: string; const Result: TStrings;
  SortResult: boolean = true);
procedure GetFiles(const dir: string; const Result: TStrings;
  const IncludeDir: boolean = false; const IncludeExt: boolean = true);
procedure GetFilesRecursive(const Result: TStrings; Dir, Mask: string);
procedure WipeFile(const filename: string);

implementation

uses
  CatStrings;

procedure CatReadLn(const f: Text; var s: string);
var
  c: Char;
  tempStr: string;
begin
  tempStr := emptystr;
  while not Eof(f) do
  begin
    read(f, c);
    case Ord(c) of
      10:
        Break;
      13:
        begin
          read(f, c);
          if Ord(c) = 13 then
          begin // hides H2077 compiler warning
          end;
          Break;
        end;
    else
      tempStr := tempStr + c;
    end;
  end;
  s := tempStr;
end;

// Deletes a directory and its sub directories
function DeleteFolder(const dir: string): boolean;
var
  sdir: string;
  st: TSHFileOpStruct;
begin
  sdir := dir;
  if LastChar(sdir) = '\' then
    sdir := copy(sdir, 1, Length(sdir) - 1);
  try
    FillChar(st, SizeOf(st), #0);
    sdir := sdir + #0#0;
    with st do
    begin
      Wnd := 0;
      wFunc := FO_DELETE;
      pFrom := PChar(sdir);
      fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    end;
    Result := (SHFileOperation(st) = 0);
  except
    Result := false;
  end;
end;

function DirExists(const dir: string): boolean;
begin
  Result := DirectoryExists(dir);
end;

// Returns true if a directory is empty, false otherwise. If the directory does
// not exists, also returns true
function DirIsEmpty(const dir: string): boolean;
var
  i: Integer;
  sr: TSearchRec;
begin
  if DirExists(dir) = true then begin
    Result := false;
    FindFirst(IncludeTrailingPathDelimiter(dir) + '*', faAnyFile, sr);
    for i := 1 to 2 do
      if (sr.Name = '.') or (sr.Name = '..') then
        Result := FindNext(sr) <> 0;
    FindClose(sr);
  end else
  Result := true;
end;

function FileCanBeOpened(const filename: string): boolean;
var
  h: integer;
begin
  try
    h := FileOpen(filename, fmOpenRead or fmShareDenyNone);
    if h > 0 then
    begin
      Result := true;
      FileClose(h);
    end
    else
      Result := false;
  except
    Result := false
  end;
end;

function FilenameToMimeType(const filename: string): string;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(filename));
  if Length(ext) > 1 then
    ext := copy(ext, 2, Length(ext));
  if ext = 'css' then
    Result := 'text/css'
  else if (ext = 'htm') or (ext = 'html') then
    Result := 'text/html'
  else if ext = 'bmp' then
    Result := 'image/bmp'
  else if ext = 'gif' then
    Result := 'image/gif'
  else if (ext = 'jpg') or (ext = 'jpeg') then
    Result := 'image/jpeg'
  else if ext = 'js' then
    Result := 'text/javascript'
  else if (ext = 'png') then
    Result := 'image/png'
  else if ext = 'txt' then
    Result := 'text/plain'
  else
    Result := 'application/octet-stream'; // Unknown Type
end;

function CleanFilename(const filename: string;
  const invCharRep: Char = '_'): string;
const
  invChars = ['\', ':', '*', '?', '"', '<', '>', '|', '/'];
begin
  Result := ReplaceChars(filename, invChars, invCharRep);
end;

function ForceDir(const dir: string): boolean;
var
  d: string;
begin
  d := ReplaceStr(dir, '\\', '\');
  d := ReplaceStr(d, '//', '/');
  if d <> emptystr then
  Result := ForceDirectories(d) else
  Result := false;
end;

function GetDirAge(const Dir: string): TDateTime;
var
  FileSpecs: TGetFileExInfoLevels;
  DirData: TWin32FileAttributeData;
  FileTime: TSystemTime;
begin
  Result := 0;
  fillchar(FileSpecs, sizeof(FileSpecs), 0);
  FileSpecs := GetFileExInfoStandard;
  fillchar(DirData, sizeof(DirData), 0);
  if GetFileAttributesEx({$IFDEF UNICODE}pwidechar{$ELSE}pchar{$ENDIF}(string(Dir)), FileSpecs, @DirData) then
  begin
    FileTimeToSystemTime(DirData.ftCreationTime, FileTime);
    Result := SystemTimeToDateTime(FileTime);
  end;
end;

procedure GetDirs(const dir: string; const Result: TStrings;
  SortResult: boolean = true);
var
  SL: TStringList;
  sr: TSearchRec;
begin
  SL := TStringList.Create;
  try
    if FindFirst(dir + '*.*', faDirectory, sr) = 0 then
    begin
      repeat
        if ((sr.Attr and faDirectory) = faDirectory) and (sr.name <> '.') and
          (sr.name <> '..') then
          SL.Add(sr.name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if SortResult = true then
      SL.sort;
    Result.Text := SL.Text;
  finally
    SL.Free;
  end;
end;

function GetDiskSerialNumber(const drive: string): string;
var
  sn, len, flags: DWORD;
begin
  GetVolumeInformation(PChar(drive), nil, 0, @sn, len, flags, nil, 0);
  Result := IntToHex(HiWord(sn), 4) + '-' + IntToHex(LoWord(sn), 4);
end;

// Returns the DLL filename (like Application.Exename would for the EXE)
function GetDllFilename:string;
var FileName : array[0..MAX_PATH] of char;
begin
 FillChar(FileName, sizeof(FileName), #0);
 GetModuleFileName(hInstance, filename, sizeof(filename));
 result:=filename;
end;

procedure GetFiles(const dir: string; const Result: TStrings;
  const IncludeDir: boolean = false; const IncludeExt: boolean = true);
var
  rc: integer;
  tmpPath, ffound: string;
  sr: TSearchRec;
begin
  if Result = nil then
    exit;
  tmpPath := IncludeTrailingPathDelimiter(ExtractFilePath(dir));
  rc := FindFirst(dir, faAnyFile, sr);
  while rc = 0 do
  begin
    ffound := sr.name;
    if IncludeExt = false then
      ffound := changefileext(ffound, emptystr);
    if IncludeDir then
      Result.Add(tmpPath + ffound)
    else
      Result.Add(ffound);
    rc := FindNext(sr);
  end;
  FindClose(sr);
end;

procedure GetFilesRecursive(const Result: TStrings; Dir, Mask: string);
var
  dirs: TStrings;
  SR: TSearchRec;
  f: boolean;
  i: integer;
begin
  if LastChar(Dir) <> PathDelim then
    Dir := Dir + PathDelim;

  f := FindFirst(Dir + Mask, faAnyFile - faDirectory, SR) = 0;
  while f = true do
  begin
    Result.Add(Dir + SR.Name);
    f := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Makes a list of the subdirectories
  dirs := TStringList.Create;
  f := FindFirst(Dir + '*.*', faAnyFile, SR) = 0;
  while f do
  begin
    if ((SR.Attr and faDirectory) <> 0) and (SR.Name[1] <> '.') then
      dirs.Add(Dir + SR.Name);
    f := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Gets the list of files in each subdirectory
  for i := 0 to dirs.Count - 1 do
    GetFilesRecursive(Result, dirs[i], Mask);

  dirs.Free;
end;

function GetFileSize(const filename: string): Int64;
var
  f: TWin32FindData;
  h: THandle;
begin
  Result := -1;
  try
    if not FileExists(filename) then
      exit;
    h := FindFirstFile({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename), f);
    if h = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
    try
      Result := f.nFileSizeHigh shl 32 + f.nFileSizeLow;
    finally
{$IF CompilerVersion > 22}Winapi.{$IFEND}Windows.FindClose(h);
    end;
  except
  end;
end;

function GetFileToStr(const filename: string): string;
var
  SL: TStringList;
  f: TFileStream;
begin
  SL := TStringList.Create;
  f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  with f do
  begin
    try
      SL.LoadFromStream(f);
      Result := SL.Text;
    except
      Result := emptystr;
    end;
    Free;
  end;
  SL.Free;
end;

// Returns the version of a binary file (DLL, EXE, etc)
function GetFileVersion(const filename: string;
  const ResFormat: string = '%d.%d.%d.%d'): string;
var
  p, pi: Pointer;
  infosz, plen: DWORD;
  verinfo: VS_FIXEDFILEINFO;
begin
  Result := emptystr;
{$IFDEF MSWINDOWS}
  infosz := GetFileVersionInfoSize({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename), plen);
  FillChar(verinfo, SizeOf(verinfo), 0);
  if infosz > 0 then
  begin
    GetMem(p, infosz);
    GetFileVersionInfo({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename),
      0, infosz, p);
    VerQueryValue(p, '\', pi, plen);
    move(pi^, verinfo, SizeOf(verinfo));
    Result := Format(ResFormat, [verinfo.dwFileVersionMS shr 16,
      verinfo.dwFileVersionMS and 65535, verinfo.dwFileVersionLS shr 16,
      verinfo.dwFileVersionLS and 65535]);
    FreeMem(p);
  end;

{$ENDIF}
end;

function GetSizeDescription(const bytes: int64): string;
const
  cFF = '0.0';
begin
  if bytes < 1 then
    Result := '0 bytes'
  else
    case bytes of
      1 .. 1023:
        Result := InttoStr(bytes) + ' bytes';
      1024 .. 1048575:
        Result := FormatFloat(cFF, bytes / 1024) + ' KB';
      1048576 .. 1073741823:
        Result := FormatFloat(cFF, bytes / 1048576) + ' MB';
    else
      Result := FormatFloat(cFF, bytes / 1073741824) + ' GB';
    end;
end;

// Reverts a size description like 1mb or "1 megabyte" to the equivalent
// number of bytes
function GetSizeDescriptionBytes(const desc: string): int64;
var
  sz: double;
  szstr, szlet, adesc: string;
begin
  result := 0;
  adesc := replacechars(desc, ['(', ')'], ' ');
  adesc := replacestr(adesc, ' ', emptystr);
  adesc := replacestr(adesc, ',', '.');
  adesc := lowercase(adesc);
  if lastchar(adesc) = 's' then
    adesc := removelastchar(adesc);
  szstr := ExtractChars(adesc, ['0' .. '9', '.']);
  szlet := ExtractChars(adesc, ['A' .. 'Z', 'a'..'z']);
  try
    sz := StrToFloat(szstr);
    if szlet = 'b' then
      result := Trunc(sz)
    else if szlet = 'byte' then
      result := Trunc(sz)
    else if MatchStrInArray(szlet, ['kilobyte', 'kbyte', 'kb']) then
      result := Trunc(sz * 1024)
    else if MatchStrInArray(szlet, ['megabyte', 'mb', 'm']) then
      result := Trunc(sz * 1048576)
    else if MatchStrInArray(szlet, ['gigabyte', 'gb']) then
      result := Trunc(sz * 1073741824)
    else if MatchStrInArray(szlet, ['terabyte', 'tb']) then
      result := Trunc(sz * 1099511627776)
    else if MatchStrInArray(adesc, ['petabyte', 'pb']) then
      result := Trunc(sz * 1125899906842624);
  except
  end;
end;

// Returns a temporary filename (located in the Windows Temporary directory)
// This function will not create the temporary file, just return a filename suggestion
// Usage Example: ShowMessage(GetTempFile('.tmp'))
function GetTempFile(const ext: string): string;
var
  buf: array [0 .. MAX_PATH] of {$IFDEF UNICODE}WideChar{$ELSE}Char{$ENDIF};
begin
  GetTempPath({$IFDEF UNICODE}Length{$ELSE}SizeOf{$ENDIF}(buf) - 1, buf);
  GetTempFileName(buf, '~', 0, buf);
  Result := StrPas(buf);
  DeleteFile(result);
  if ext <> emptystr then // if the extension is empty will return a .tmp
    Result := changefileext(Result, ext);
end;

function GetTextFileLinesCount(const filename: string): integer;
var
  f: Textfile;
  s: string;
begin
  AssignFile(f, filename);
  Reset(f);
  Result := 0;
  while not seekeof(f) do
  begin
    Result := Result + 1;
    CatReadLn(f, s);
  end;
  Closefile(f);
end;

function GetWindowsTempDir: String;
var
  bufFolder: array [0 .. MAX_PATH] of
{$IFDEF UNICODE}WideChar{$ELSE}Char{$ENDIF};
begin
  GetTempPath({$IFDEF UNICODE}Length{$ELSE}SizeOf{$ENDIF}(bufFolder),
    bufFolder);
  Result := IncludeTrailingPathDelimiter(String(bufFolder));
end;

function SendToLog(const filename: TFilename; const s: String): boolean;
var
  f: Textfile;
begin
  try
    AssignFile(f, filename);
    if FileExists(filename) = false then
      ReWrite(f)
    else
    begin
      Reset(f);
      Append(f);
    end;
    WriteLn(f, s);
    Closefile(f);
    Result := true;
  except
    Result := false;
  end;
end;

function SL_SaveToFile(const SL: TStrings; const filename: string): boolean;
var
  fs: TStream;
begin
  Result := false;
  if filename = emptystr then
    exit;
  if FileExists(filename) = false then
  begin
    fs := TFileStream.Create(filename, fmCreate or fmOpenWrite or
      fmShareDenyWrite);
    fs.Free;
  end;

  fs := TFileStream.Create(filename, fmOpenWrite or fmShareDenyWrite);
  fs.Size := 0;
  try
    SL.SaveToStream(fs);
    Result := true;
  except
  end;
  fs.Free;
end;

function SL_LoadFromFile(const SL: TStrings; const filename: string)
  : boolean;
var
  fs: TStream;
begin
  Result := false;
  if filename = emptystr then
    exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SL.LoadFromStream(fs);
    Result := true;
  except
  end;
  fs.Free;
end;

// Copies a file, based on a source and destination filename, and returns true
// if successful or false if the operation failed
function FileCopy(const source, dest: string): boolean;
{$IF CompilerVersion < 22}
var
  fSrc, fDst, len: integer;
  Size: LongInt;
  buffer: packed array [0 .. 2047] of Byte;
{$IFEND}
begin
{$IF CompilerVersion > 22}
  Result := true;
  if FileExists(source) and FileExists(dest) then
    DeleteFile(dest);
  try
    TFile.copy(source, dest);
  except
    Result := false;
  end;
{$ELSE}
  // By David Stidolph, 21 Jun 1995
  Result := false;
  if source <> dest then
  begin
    fSrc := FileOpen(source, fmOpenRead);
    if fSrc >= 0 then
    begin
      Size := FileSeek(fSrc, 0, 2);
      FileSeek(fSrc, 0, 0);
      fDst := FileCreate(dest);
      if fDst >= 0 then
      begin
        while Size > 0 do
        begin
          len := FileRead(fSrc, buffer, SizeOf(buffer));
          FileWrite(fDst, buffer, len);
          Size := Size - len;
        end;

        FileSetDate(fDst, FileGetDate(fSrc));
        FileClose(fDst);
        FileSetAttr(dest, FileGetAttr(source));

        Result := true;
      end;
      FileClose(fSrc);
    end;
  end;
{$IFEND}
end;

// Peter Below ------------------------------------------------------------//

// Based on an example from PB (4/5/1998)
procedure CopyAfterFirstLine(const sourcefile, targetfile: string;
  appendln: boolean = false; lnstr: string = '');
var
  s: string;
  source, Target: Textfile;
begin
  AssignFile(source, sourcefile);
  AssignFile(Target, targetfile);
  Reset(source);
  try
    ReWrite(Target);
    try
      CatReadLn(source, s);
      while not Eof(source) do
      begin
        CatReadLn(source, s);
        WriteLn(Target, s);
      end;
      if appendln then
        WriteLn(lnstr);
    finally
      Closefile(Target);
    end;
  finally
    Closefile(source);
  end;
end;

{
  If you want to get rid of a file normally you just delete it.
  But someone else can undelete it if the file hasn't been wiped correctly.
  For security purposes, to insure that certain files are permanently
  gone, the WipeFile procedure writes over the data in the file with
  random characters and then erases it.
}
procedure WipeFile(const filename: string); // PB
var
  buffer: array [0 .. 4095] of Byte;
  max, n: LongInt;
  i: integer;
  fs: TFileStream;

  procedure RandomizeBuffer;
  var
    i: integer;
  begin
    for i := Low(buffer) to High(buffer) do
      buffer[i] := Random(256);
  end;

begin
  fs := TFileStream.Create(filename, fmOpenReadWrite or fmShareExclusive);
  try
    for i := 1 to 3 do
    begin
      RandomizeBuffer;
      max := fs.Size;
      fs.Position := 0;
      while max > 0 do
      begin
        if max > SizeOf(buffer) then
          n := SizeOf(buffer)
        else
          n := max;
        fs.Write(buffer, n);
        max := max - n;
      end;
      FlushFileBuffers(fs.Handle);
    end;
  finally
    fs.Free;
  end;
  DeleteFile(filename);
end;

// ------------------------------------------------------------------------//
end.
