unit CatCryptSyno;
{
  Catarinka Crypto Utils library
  This library uses and enhances functions from Synopse's Syncrypt library

  Copyright (c) 2020-2023 Felipe Daragon
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
function RandomPassword(const len: integer): string;
function RandomKey: string;
function SecureRandom(const positiveOnly:boolean=false):integer;
function SecureRandom64(const positiveOnly:boolean=false):int64;
function SHA256(const s: string): string;
function SHA384(const s: string): string;
function SHA512(const s: string): string;

implementation

uses
  SynCrypto, SynCommons, CatStrings;

function SHA512(const s: string): string;
begin
{$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA512(rawbytestring(s)));
{$ELSE}
  result := string(SynCrypto.SHA512(s));
{$ENDIF}
end;

function SHA384(const s: string): string;
begin
{$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA384(rawbytestring(s)));
{$ELSE}
  result := string(SynCrypto.SHA384(s));
{$ENDIF}
end;

function SHA256(const s: string): string;
begin
{$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA256(rawbytestring(s)));
{$ELSE}
  result := string(SynCrypto.SHA256(s));
{$ENDIF}
end;

// Generates a random password securely while making sure at least 1 random
// number is part of the newly generated password (if length is 5 or superior)
function RandomPassword(const len: integer): string;
var
  p: TAESPRNG;
begin
  p := TAESPRNG.Create;
  if len > 4 then
  begin
    repeat
      result := string(p.RandomPassword(len));
    until (ContainsAnyOfChars(result, ['0' .. '9']) = true);
  end
  else
  begin
    result := string(p.RandomPassword(len));
  end;
  p.Free;
end;

// Generates a random key securely while making sure at least 1 random number
// is part of the newly generated key
function RandomKey: string;
var
  p: TAESPRNG;
begin
  p := TAESPRNG.Create;
  repeat
    result := StrToHex(string(p.RandomPassword(32)));
  until (ContainsAnyOfChars(result, ['0' .. '9']) = true);
  p.Free;
end;

// Generates a random integer securely
function SecureRandom(const positiveOnly:boolean=false):integer;
var
  p: TAESPRNG;
 block: THash128Rec;
begin
 p := TAESPRNG.Create;
 p.Main.FillRandom(block.b);
 result := block.i0 xor block.i1 xor block.i2 xor block.i3;
 p.Free;
 if positiveonly = true then
   result := abs(result);
end;

// Generates a random int64 securely
function SecureRandom64(const positiveOnly:boolean=false):int64;
var
  p: TAESPRNG;
begin
 p := TAESPRNG.Create;
 result := p.Random64;
 p.Free;
 if positiveonly = true then
   result := abs(result);
end;

// ------------------------------------------------------------------------//
end.
