unit CatCrypt;
{
  Catarinka Crypto library
  Quick AES encryption/decryption functions covering string, stream and file

  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  {$IFDEF USECROSSVCL}
  WinAPI.Windows,
  {$ENDIF}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}
function StrToAES(const s, key: string): string;
function AESToStr(const s, key: string): string;
procedure AES_EncryptFile(const filename, outfilename: string;
  const key: string);
procedure AES_DecryptFile(const filename, outfilename: string;
  const key: string);
procedure AES_EncryptStream(ms: TMemoryStream; const key: string);
procedure AES_DecryptStream(ms: TMemoryStream; const key: string);

// ansi functions
function AnsiStrToAES(const s, key: string): string;
function AESToAnsiStr(const s, key: string): string;

implementation

uses
  AES;

function StrToAES(const s, key: string): string;
begin
  result := EncryptString(s, key);
end;

function AESToStr(const s, key: string): string;
begin
  result := DecryptString(s, key);
end;

procedure AES_EncryptStream(ms: TMemoryStream; const key: string);
var
  src: TMemoryStream;
begin
  src := TMemoryStream.Create;
  src.CopyFrom(ms, ms.Size);
  src.position := 0;
  ms.clear;
  // encrypt the contents of the stream
  EncryptStream(src, ms, key, kb256);
  src.Free;
end;

procedure AES_DecryptStream(ms: TMemoryStream; const key: string);
var
  src: TMemoryStream;
begin
  src := TMemoryStream.Create;
  src.CopyFrom(ms, ms.Size);
  src.position := 0;
  ms.clear;
  // decrypt the contents of the stream
  DecryptStream(src, ms, key, kb256);
  src.Free;
end;

procedure AES_EncryptFile(const filename, outfilename: string;
  const key: string);
var
  s: TMemoryStream;
begin
  if fileexists(filename) = false then
    exit;
  s := TMemoryStream.Create;
  s.LoadFromFile(filename);
  s.position := 0;
  AES_EncryptStream(s, key);
  s.SaveToFile(outfilename);
  s.Free;
end;

procedure AES_DecryptFile(const filename, outfilename: string;
  const key: string);
var
  s: TMemoryStream;
begin
  if fileexists(filename) = false then
    exit;
  s := TMemoryStream.Create;
  s.LoadFromFile(filename);
  s.position := 0;
  AES_DecryptStream(s, key);
  s.SaveToFile(outfilename);
  s.Free;
end;

{ ansi functions }

function AnsiStrToAES(const s, key: string): string;
begin
  // force ANSI
  result := EncryptString(ansistring(s), ansistring(key));
end;

function AESToAnsiStr(const s, key: string): string;
begin
  result := DecryptString(ansistring(s), ansistring(key));
end;

// ------------------------------------------------------------------------//
end.
