unit CatRes;

{
  Catarinka - Catarinka Resources Library
  Useful functions for reading or saving resources

  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  GetResourceAsPointer is based on an example from the Pascal Newsletter #25
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, Winapi.Windows, vcl.imaging.Jpeg, System.Classes;
{$ELSE}
  SysUtils, Windows, Jpeg, Classes;
{$ENDIF}
function GetResourceAsJpeg(const ResName: string): TJPEGImage;
function GetResourceAsPointer(const ResName: string;
  out Size: longword): pointer;
function GetResourceAsString(const ResName: string): string;
function SaveResourceAsTempFile(const ResName: string): string;
procedure SaveResourceAsFile(const ResName, FileName: string);

{$IFDEF MSWINDOWS}
// Resource type other than RCDATA will not work under Linux
function GetResourceAsPointerT(const ResName, ResType: string;
  out Size: longword): pointer;
function GetResourceAsStringT(const ResName, ResType: string): string;
procedure SaveResourceAsFileT(const ResName, ResType, FileName: string);
{$ENDIF}

implementation

uses CatFiles, CatStrings;

{$IFDEF UNICODE}

function StrToResType(const s: string): PWideChar;
begin
  result := PWideChar(s);
end;
{$ELSE}

function StrToResType(const s: string): PAnsiChar;
begin
  result := PAnsiChar(AnsiString(s));
end;
{$ENDIF}

// Usage Example:
// Jpg := GetResourceAsJpeg('sample_jpg');
// Image1.Picture.Bitmap.Assign(Jpg);
function GetResourceAsJpeg(const ResName: string): TJPEGImage;
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    result := TJPEGImage.Create;
    result.LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;

// Example: Memo1.Lines.Text := GetResourceAsString('sample_txt');
function GetResourceAsString(const ResName: string): string;
var
  rd: PAnsiChar;
  sz: longword;
begin
  rd := GetResourceAsPointer(ResName, sz);
  SetString(result, rd, sz);
end;

procedure SaveResourceAsFile(const ResName, FileName: string);
begin
  with TResourceStream.Create(hInstance, ResName, RT_RCDATA) do
    try
      SaveToFile(FileName);
    finally
      Free;
    end;
end;

function SaveResourceAsTempFile(const ResName: string): string;
begin
  result := GetWindowsTempDir + 'temp_' + ResName;
  SaveResourceAsFile(ResName, result);
end;

function GetResourceAsPointer(const ResName: string;
  out Size: longword): pointer;
var
  ib: HRSRC; // InfoBlock
  gmb: HGLOBAL; // GlobalMemoryBlock
begin
  ib := FindResource(hInstance, StrToResType(ResName), RT_RCDATA);
  if ib = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  Size := SizeofResource(hInstance, ib);
  if Size = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  gmb := LoadResource(hInstance, ib);
  if gmb = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  result := LockResource(gmb);
  if result = nil then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

{
  Usage Example:
  procedure TForm1.FormCreate(Sender: TObject);
  var size: longword; sample_wav: pointer;
  begin
  sample_wav := GetResourceAsPointerT('sample_wav', 'wave', size);
  sndPlaySound(sample_wav, SND_MEMORY or SND_NODEFAULT or SND_ASYNC);
  end;
}

function GetResourceAsPointerT(const ResName, ResType: string;
  out Size: longword): pointer;
var
  ib: HRSRC; // InfoBlock
  gmb: HGLOBAL; // GlobalMemoryBlock
begin
  ib := FindResource(hInstance, StrToResType(ResName), StrToResType(ResType));
  if ib = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  Size := SizeofResource(hInstance, ib);
  if Size = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  gmb := LoadResource(hInstance, ib);
  if gmb = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  result := LockResource(gmb);
  if result = nil then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

// Example: Memo1.Lines.Text := GetResourceAsStringT('sample_txt', 'text');
function GetResourceAsStringT(const ResName, ResType: string): string;
var
  rd: PAnsiChar;
  sz: longword;
begin
  rd := GetResourceAsPointerT(ResName, ResType, sz);
  SetString(result, rd, sz);
end;

procedure SaveResourceAsFileT(const ResName, ResType, FileName: string);
begin
  with TResourceStream.Create(hInstance, ResName, StrToResType(ResType)) do
    try
      SaveToFile(FileName);
    finally
      Free;
    end;
end;

// ------------------------------------------------------------------------//
end.
