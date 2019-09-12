unit CatResMin;

{
  Minimal version of CatRes.pas with most used functions only

  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  GetResourceAsPointer is based on an example from the Pascal Newsletter #25
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.SysUtils;
{$ELSE}
  Windows, SysUtils;
{$ENDIF}

function GetResourceAsPointer(const ResName: string;
  out Size: longword): pointer;
function GetResourceAsString(const ResName: string): string;

implementation

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

// Example: Memo1.Lines.Text := GetResourceAsString('sample_txt');
function GetResourceAsString(const ResName: string): string;
var
  rd: PAnsiChar;
  sz: longword;
begin
  rd := GetResourceAsPointer(ResName, sz);
  SetString(result, rd, sz);
end;

//------------------------------------------------------------------------//
end.