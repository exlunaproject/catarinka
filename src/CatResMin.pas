unit CatResMin;

{
  Minimal version of CatRes.pas with most used functions only

  Copyright (c) 2003-2018 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

function GetResourceAsPointer(const ResName, ResType: string;
  out Size: longword): pointer;
function GetResourceAsString(const ResName, ResType: string): string;

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

// Based on an example from the Pascal Newsletter #25
function GetResourceAsPointer(const ResName, ResType: string;
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

// Example: Memo1.Lines.Text := GetResourceAsString('sample_txt', 'text');
function GetResourceAsString(const ResName, ResType: string): string;
var
  rd: PAnsiChar; // resource data
  sz: longword; // resource size
begin
  rd := GetResourceAsPointer(ResName, ResType, sz);
  SetString(result, rd, sz);
end;

//------------------------------------------------------------------------//
end.