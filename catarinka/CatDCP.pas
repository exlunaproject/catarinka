unit CatDCP;
{
  Catarinka Crypto library
  Quick AES encryption/decryption functions covering string, stream and file

  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils;
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
  DCPrijndael, DCPsha512;

function StrToAES(const s, key: string): string;
var
  c: TDCP_rijndael;
begin
  c := TDCP_rijndael.Create(nil);
  c.InitStr(key, TDCP_sha512);
  result := string(c.EncryptString(s));
  c.Burn;
  c.Free;
end;

function AESToStr(const s, key: string): string;
var
  c: TDCP_rijndael;
begin
  c := TDCP_rijndael.Create(nil);
  c.InitStr(key, TDCP_sha512);
  result := string(c.DecryptString(s));
  c.Burn;
  c.Free;
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

procedure AES_EncryptStream(ms: TMemoryStream; const key: string);
var
  src: TMemoryStream;
  c: TDCP_rijndael;
begin
  src := TMemoryStream.Create;
  src.CopyFrom(ms, ms.Size);
  src.position := 0;
  ms.clear;

  c := TDCP_rijndael.Create(nil);
  // initialize the cipher with a hash of the passphrase
  c.InitStr(key, TDCP_sha512);
  // encrypt the contents of the stream
  c.EncryptStream(src, ms, src.Size);
  c.Burn;
  c.Free;

  src.Free;
end;

procedure AES_DecryptStream(ms: TMemoryStream; const key: string);
var
  src: TMemoryStream;
  c: TDCP_rijndael;
begin
  src := TMemoryStream.Create;
  src.CopyFrom(ms, ms.Size);
  src.position := 0;
  ms.clear;

  c := TDCP_rijndael.Create(nil);
  // initialize the cipher with a hash of the passphrase
  c.InitStr(key, TDCP_sha512);
  // decrypt the contents of the stream
  c.DecryptStream(src, ms, src.Size);
  c.Burn;
  c.Free;

  src.Free;
end;

{ ansi functions }

function AnsiStrToAES(const s, key: string): string;
var
  c: TDCP_rijndael;
begin
  c := TDCP_rijndael.Create(nil);
  // force ANSI
  c.InitStr(ansistring(key), TDCP_sha512);
  result := string(c.EncryptString(ansistring(s)));
  c.Burn;
  c.Free;
end;

function AESToAnsiStr(const s, key: string): string;
var
  c: TDCP_rijndael;
begin
  c := TDCP_rijndael.Create(nil);
  // force ANSI
  c.InitStr(ansistring(key), TDCP_sha512);
  result := string(c.DecryptString(ansistring(s)));
  c.Burn;
  c.Free;
end;

// ------------------------------------------------------------------------//
end.
