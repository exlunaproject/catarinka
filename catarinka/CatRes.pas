unit CatRes;

{
  Catarinka - Catarinka Resources Library
  Useful functions for reading or saving resources

  Copyright (c) 2003-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  GetResourceAsPointer is based on an example from the Pascal Newsletter #25
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  vcl.imaging.Jpeg, Vcl.Imaging.pngimage;
{$ELSE}
  SysUtils, Windows, Classes, Graphics, Controls, Jpeg, pngimage;
{$ENDIF}
procedure LoadPngIconToImageList(ImageList: TImageList; ResourceName: string);
function GetResourceAsJpeg(const ResName: string): TJPEGImage;
function GetResourceAsPointer(const ResName: string;
  out Size: longword): pointer;
function GetResourceAsString(const ResName: string): string;
procedure GetResourceToMemoryStream(const ResName: string;var stream:TMemoryStream);
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

procedure LoadPngIconToImageList(ImageList: TImageList; ResourceName: string);
var
  ResStream: TResourceStream;
  PngImage: TPngImage;
  Bitmap: TBitmap;
begin
  // Load the PNG icon from the resource
  ResStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    PngImage := TPngImage.Create;
    try
      PngImage.LoadFromStream(ResStream);

      // Convert the PNG image to a TBitmap
      Bitmap := TBitmap.Create;
      try
        Bitmap.Assign(PngImage);

        // Add the TBitmap to the TImageList
        ImageList.Add(Bitmap, nil);
      finally
        Bitmap.Free;
      end;
    finally
      PngImage.Free;
    end;
  finally
    ResStream.Free;
  end;
end;

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

procedure GetResourceToMemoryStream(const ResName: string;var stream:TMemoryStream);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    rs.SaveToStream(stream);
    stream.Position := 0;
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
