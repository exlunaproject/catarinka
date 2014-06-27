unit CatRes;

{
  Catarinka - Catarinka Resources Library
  Useful functions for reading or saving resources

  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, Winapi.Windows, vcl.imaging.Jpeg, System.Classes;
{$ELSE}
  SysUtils, Windows, Jpeg, Classes;
{$ENDIF}

type
  MyPWideChar = {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF};

function GetResourceAsPointer(const ResName, ResType: MyPWideChar;
  out Size: longword): pointer;
function GetResourceAsString(const ResName, ResType: MyPWideChar): string;
function GetResourceAsJpeg(const ResName: string): TJPEGImage;
procedure SaveResourceAsFile(const ResName: string; const ResType: MyPWideChar;
  const FileName: string);
function SaveResourceAsTempFile(const ResName: string;
  const ResType: MyPWideChar): string;

implementation

uses CatFiles;

// Usage Example:
// Jpg := GetResourceAsJpeg('sample_jpg');
// Image1.Picture.Bitmap.Assign(Jpg);
function GetResourceAsJpeg(const ResName: string): TJPEGImage;
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, ResName, 'JPEG');
  try
    Result := TJPEGImage.Create;
    Result.LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;

// Example: Memo1.Lines.Text := GetResourceAsString('sample_txt', 'text');
function GetResourceAsString(const ResName, ResType: MyPWideChar): string;
var
  rd: pansichar; // resource data
  sz: longword; // resource size
begin
  rd := GetResourceAsPointer(ResName, ResType, sz);
  SetString(Result, rd, sz);
end;

procedure SaveResourceAsFile(const ResName: string; const ResType: MyPWideChar;
  const FileName: string);
begin
  with TResourceStream.Create(hInstance, ResName, ResType) do
    try
      SaveToFile(FileName);
    finally
      Free;
    end;
end;

{ 
  Usage Example:
  procedure TForm1.FormCreate(Sender: TObject);
  var size: longword; sample_wav: pointer;
  begin
   sample_wav := GetResourceAsPointer('sample_wav', 'wave', size);
   sndPlaySound(sample_wav, SND_MEMORY or SND_NODEFAULT or SND_ASYNC);
  end;
} 
// Based on an example from the Pascal Newsletter #25
function GetResourceAsPointer(const ResName, ResType: MyPWideChar;
  out Size: longword): pointer;
var
  ib: HRSRC; // InfoBlock
  gmb: HGLOBAL; // GlobalMemoryBlock
begin
  ib := FindResource(hInstance, ResName, ResType);
  if ib = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  Size := SizeofResource(hInstance, ib);
  if Size = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  gmb := LoadResource(hInstance, ib);
  if gmb = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  Result := LockResource(gmb);
  if Result = nil then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

function SaveResourceAsTempFile(const ResName: string;
  const ResType: MyPWideChar): string;
begin
  Result := GetWindowsTempDir + 'temp_' + ResName;
  SaveResourceAsFile(ResName, ResType, Result);
end;

// ------------------------------------------------------------------------//
end.
