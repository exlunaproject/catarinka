unit CatDCP;
{
  Catarinka - Quick AES string encryption/decryption functions
  
  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

function StrToAES(const s, key: string): string;
function AESToStr(const s, key: string): string;

implementation

uses
  DCPrijndael, DCPsha512;

function StrToAES(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  Cipher.InitStr(key, TDCP_sha512);
  result := Cipher.EncryptString(s);
  Cipher.Burn;
  Cipher.Free;
end;

function AESToStr(const s, key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  Cipher.InitStr(key, TDCP_sha512);
  result := Cipher.DecryptString(s);
  Cipher.Burn;
  Cipher.Free;
end;

// ------------------------------------------------------------------------//
end.
