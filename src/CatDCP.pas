unit CatDCP;
{
  Catarinka - Quick AES string encryption/decryption functions
  
  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

function StrToAES(const s, key: string): string;
function AESToStr(const s, key: string): string;

// ansi functions
function AnsiStrToAES(const s, key: string): string;
function AESToAnsiStr(const s, key: string): string;

implementation

uses
  DCPrijndael, DCPsha512;

function StrToAES(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  Cipher.InitStr(key, TDCP_sha512);
  result := string(Cipher.EncryptString(s));
  Cipher.Burn;
  Cipher.Free;
end;

function AESToStr(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  Cipher.InitStr(key, TDCP_sha512);
  result := string(Cipher.DecryptString(s));
  Cipher.Burn;
  Cipher.Free;
end;

{ ansi functions }

function AnsiStrToAES(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  // force ANSI
  Cipher.InitStr(ansistring(key), TDCP_sha512);
  result := string(Cipher.EncryptString(ansistring(s)));
  Cipher.Burn;
  Cipher.Free;
end;

function AESToAnsiStr(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  // force ANSI
  Cipher.InitStr(ansistring(key), TDCP_sha512);
  result := string(Cipher.DecryptString(ansistring(s)));
  Cipher.Burn;
  Cipher.Free;
end;

// ------------------------------------------------------------------------//
end.
