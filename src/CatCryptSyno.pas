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

implementation

uses
  SynCrypto, CatStrings;

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
