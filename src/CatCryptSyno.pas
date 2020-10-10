unit CatCryptSyno;
{
  Catarinka Crypto Utils library
  This uses functions from Synopse's Syncrypt library

  Copyright (c) 2020 Felipe Daragon
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

function RandomPassword(const len:integer):string;
function RandomKey:string;
function SHA256(const s:string):string;
function SHA384(const s:string):string;
function SHA512(const s:string):string;


implementation

uses
  SynCrypto, CatStrings;

function SHA512(const s:string):string;
begin
 {$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA512(rawbytestring(s)));
 {$ELSE}
  result := string(SynCrypto.SHA512(s));
 {$ENDIF}
end;

function SHA384(const s:string):string;
begin
 {$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA384(rawbytestring(s)));
 {$ELSE}
  result := string(SynCrypto.SHA384(s));
 {$ENDIF}
end;

function SHA256(const s:string):string;
begin
 {$IFDEF DXE2_OR_UP}
  result := string(SynCrypto.SHA256(rawbytestring(s)));
 {$ELSE}
  result := string(SynCrypto.SHA256(s));
 {$ENDIF}
end;

function RandomPassword(const len:integer):string;
var p:TAESPRNG;
begin
  p := TAESPRNG.Create;
  result := string(p.RandomPassword(len));
  p.Free;
end;

function RandomKey:string;
var p:TAESPRNG;
begin
  p := TAESPRNG.Create;
  result := StrToHex(string(p.RandomPassword(32)));
  p.Free;
end;

// ------------------------------------------------------------------------//
end.
