unit CatZIP;
{
  Catarinka - ZIP Compression/Decompression

  Copyright (c) 2013-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}
procedure DirToZIP(basedirectory, outzipfilename: string; mask: string = '*';
  password: string = '');
procedure ExtractZIPFile(const zipname, filename, outfilename: string);
procedure ExtractZIPFileToStream(const zipname, filename: string;
  ms: TMemoryStream);
procedure GUnZipStream(Document: TMemoryStream);
procedure GZipAFile(const outzipname, infilename, filetozip: string);
procedure GZipStream(Document: TStream);
function GetTextFileFromZIP(const zipname, filename: string): string;
function ZIPFileExists(const zipname, filename: string): boolean;

implementation

uses CatStrings, CatStringLoop, CatFiles, AbZipKit, AbZipper, AbArcTyp, AbUtils, AbGzTyp;

procedure GUnZipStream(Document: TMemoryStream);
var
  kit: TAbGzipStreamHelper;
  outms: TMemoryStream;
begin
  kit := TAbGzipStreamHelper.Create(Document);
  if kit.FindFirstItem then
  begin
    outms := TMemoryStream.Create;
    kit.ExtractItemData(outms);
    outms.Position := 0;
    Document.Clear;
    Document.LoadFromStream(outms);
    outms.free;
  end;
  kit.free;
end;

procedure GZipStream(Document: TStream);
var
  kit: TAbZipKit;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromStream(Document);
  kit := TAbZipKit.Create(nil);
  kit.archivetype := atGzip;
  kit.forcetype := true;
  kit.Stream := Document; // outstream
  kit.AddFromStream('Untitled', ms);
  ms.free;
  kit.free;
end;

procedure GZipAFile(const outzipname, infilename, filetozip: string);
var
  kit: TAbZipKit;
  ms, outstream: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(filetozip);
  outstream := TMemoryStream.Create;
  kit := TAbZipKit.Create(nil);
  kit.archivetype := atGzip;
  kit.forcetype := true;
  kit.Stream := outstream;
  kit.AddFromStream(infilename, ms);
  outstream.SaveToFile(outzipname);
  outstream.free;
  ms.free;
  kit.free;
end;

// Creates a ZIP file with the contents of a directory
procedure DirToZIP(basedirectory, outzipfilename: string; mask: string = '*';
  password: string = '');
var
  z:TAbZipper;
begin
  z := TAbZipper.Create(nil);
 try
  z.FileName := outzipfilename;
  z.Password := password;
  z.BaseDirectory := basedirectory;
  z.StoreOptions := [soRecurse];
  z.AddFiles(mask, faAnyFile);
  z.Save;
 finally
  z.Free;
  end;
end;

procedure ExtractZIPFileToStream(const zipname, filename: string;
  ms: TMemoryStream);
var
  kit: TAbZipKit;
  f: string;
begin
  f := replacestr(filename, '\', '/');
  kit := TAbZipKit.Create(nil);
  kit.OpenArchive(zipname);
  if kit.FindFile(f) <> -1 then
    kit.ExtractToStream(f, ms);
  kit.free;
  ms.Position := 0;
end;

procedure ExtractZIPFile(const zipname, filename, outfilename: string);
var
  kit: TAbZipKit;
  ms: TMemoryStream;
  f: string;
begin
  f := replacestr(filename, '\', '/');
  ms := TMemoryStream.Create;
  kit := TAbZipKit.Create(nil);
  kit.OpenArchive(zipname);
  if kit.FindFile(f) <> -1 then
    kit.ExtractToStream(f, ms);
  kit.free;
  ms.Position := 0;
  ms.SaveToFile(outfilename);
  ms.free;
end;

function GetTextFileFromZIP(const zipname, filename: string): string;
var
  kit: TAbZipKit;
  sl: tstringlist;
  ms: TMemoryStream;
  f: string;
begin
  f := replacestr(filename, '\', '/');
  ms := TMemoryStream.Create;
  sl := tstringlist.Create;
  kit := TAbZipKit.Create(nil);
  kit.OpenArchive(zipname);
  if kit.FindFile(f) <> -1 then
    kit.ExtractToStream(f, ms);
  kit.free;
  ms.Position := 0;
  sl.LoadFromStream(ms);
  result := sl.text;
  sl.free;
  ms.free;
end;

function ZIPFileExists(const zipname, filename: string): boolean;
var
  kit: TAbZipKit;
  f: string;
begin
  result:=false;
  f := replacestr(filename, '\', '/');
  kit := TAbZipKit.Create(nil);
  kit.OpenArchive(zipname);
  if kit.FindFile(f) <> -1 then
    result:=true;
  kit.free;
end;

end.
