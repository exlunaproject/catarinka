unit CatCEFCache;

{
  Catarinka - Chromium Cache Reader functions
  Copyright (c) 2013-2014 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Dialogs;
{$ELSE}
  Windows, Classes, SysUtils, Dialogs;
{$ENDIF}

function ChromeCacheToString(const HTML: string): string;
function GetChromeCacheResponseHeaders(const HTML: string): string;
function GetChromeCacheRawData(const HTML: string): string;
function IsContentGzipped(const HTML: string): boolean;
procedure ChromeCacheExtract(const HTML, OutFilename: string);

implementation

uses
  CatHTTP, CatStringLoop, CatStrings, CatZIP;

const
  cHexPos = '00000000:';

procedure SaveHexStringToFile(HexStr: string; DestFileName: string;
  gunzip: boolean = false);
var
  BinaryStream: TMemoryStream;
begin
  BinaryStream := TMemoryStream.Create;
  try
    BinaryStream.Size := Length(HexStr) div 2;
    if BinaryStream.Size > 0 then
    begin
      HexToBin(PAnsiChar(ansistring(HexStr)), BinaryStream.Memory,
        BinaryStream.Size);
      if gunzip then
        GUnZipStream(BinaryStream);
      BinaryStream.SaveToFile(DestFileName)
    end;
  finally
    BinaryStream.Free;
  end;
end;

function GUnzipHexStr(HexStr: string): string;
var
  BinaryStream: TMemoryStream;
  res: TStringList;
begin
  result := emptystr;
  res := TStringList.Create;
  BinaryStream := TMemoryStream.Create;
  try
    BinaryStream.Size := Length(HexStr) div 2;
    if BinaryStream.Size > 0 then
    begin
      HexToBin(PAnsiChar(ansistring(HexStr)), BinaryStream.Memory,
        BinaryStream.Size);
      GUnZipStream(BinaryStream);
      res.LoadFromStream(BinaryStream);
      result := res.Text;
    end;
  finally
    BinaryStream.Free;
    res.Free;
  end;
end;

function GetChromeCacheRawData(const HTML: string): string;
var
  slp: TStringLoop;
  foundheader, foundcontent: boolean;
  hline, resstr: string;
begin
  foundcontent := false;
  foundheader := false;
  slp := TStringLoop.Create;
  slp.LoadFromString(HTML);
  while slp.Found do
  begin
    if beginswith(slp.current, cHexPos) then
    begin
      if foundheader = true then
        foundcontent := true; // starts with second occurrence
      foundheader := true;
    end;
    if foundcontent then
    begin
      hline := slp.current;
      if resstr = emptystr then
        resstr := hline
      else
        resstr := resstr + crlf + hline;
    end;
  end;
  result := resstr;
  slp.Free;
end;

function ChromeCacheToHexStr(HTML: string): string;
var
  slp: TStringLoop;
  foundheader, foundcontent: boolean;
  hline, resstr: string;
begin
  foundcontent := false;
  foundheader := false;
  slp := TStringLoop.Create;
  slp.LoadFromString(HTML);
  while slp.Found do
  begin
    if beginswith(slp.current, cHexPos) then
    begin
      if foundheader = true then
        foundcontent := true; // starts with second occurrence
      foundheader := true;
    end;
    if foundcontent then
    begin
      hline := slp.current;
      hline := after(hline, ':');
      hline := trim(hline);
      hline := copy(hline, 1, 47); // previous CEF lib was col 62
      hline := replacestr(hline, ' ', emptystr);
      resstr := resstr + hline;
    end;
  end;
  resstr := replacestr(resstr, crlf, emptystr);
  result := resstr;
  slp.Free;
end;

function IsContentGzipped(const HTML: string): boolean;
begin
  result := false;
  if trim(getfield('Content-Encoding', HTML)) = 'gzip' then
    result := true;
end;

function GetChromeCacheResponseHeaders(const HTML: string): string;
var
  slp: TStringLoop;
  hdr: string;
  isheader: boolean;
begin
  hdr := emptystr;
  isheader := false;
  slp := TStringLoop.Create;
  slp.LoadFromString(HTML);
  while slp.Found do
  begin
    if beginswith(slp.current, 'HTTP/') then
      isheader := true;
    if beginswith(slp.current, cHexPos) then
    begin
      slp.Stop;
    end
    else
    begin
      if isheader then
      begin
        if hdr = emptystr then
          hdr := slp.current
        else
          hdr := hdr + crlf + slp.current;
      end;
    end;
  end;
  slp.Free;
  result := hdr;
end;

function ChromeCacheToString(const HTML: string): string;
var
  h: string;
begin
  h := ChromeCacheToHexStr(HTML);
  if IsContentGzipped(HTML) = false then
    result := hextostr(h)
  else
    result := GUnzipHexStr(h);
end;

procedure ChromeCacheExtract(const HTML, OutFilename: string);
begin
  SaveHexStringToFile(ChromeCacheToHexStr(HTML), OutFilename,
    IsContentGzipped(HTML));
end;

end.
