unit CatDCP;
{
  Catarinka - Quick AES string encryption/decryption functions
  
  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

function StrToAES(const s, key: string; Unicode: boolean = true): string;
function AESToStr(const s, key: string; Unicode: boolean = true): string;

implementation

uses
  DCPrijndael, DCPsha512;

function StrToAES(const s, key: string; Unicode: boolean = true): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  if Unicode then
  begin
    Cipher.InitStr(key, TDCP_sha512);
    result := string(Cipher.EncryptString(s));
  end
  else
  begin // force ANSI
    Cipher.InitStr(ansistring(key), TDCP_sha512);
    result := string(Cipher.EncryptString(ansistring(s)));
  end;
  Cipher.Burn;
  Cipher.Free;
end;

function AESToStr(const s, key: string; Unicode: boolean = true): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  if Unicode then
  begin
    Cipher.InitStr(key, TDCP_sha512);
    result := string(Cipher.DecryptString(s));
  end
  else
  begin // force ANSI
    Cipher.InitStr(ansistring(key), TDCP_sha512);
    result := string(Cipher.DecryptString(ansistring(s)));
  end;
  Cipher.Burn;
  Cipher.Free;
end;

// ------------------------------------------------------------------------//
end.
