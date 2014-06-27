unit CatStorage;
{
  Catarinka Storage Object

  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Vcl.Forms, System.SyncObjs, Vcl.Dialogs,
{$ELSE}
  Classes, SysUtils, Forms, SyncObjs, Dialogs,
{$ENDIF}
  GpStructuredStorage;

type
  TCatStorage = class
  private
    fCriticalSection: TCriticalSection;
    fDeleteWhenFreeing: boolean;
    fFilename: string;
    fVFS: IGPStructuredStorage;
  public
    function CachedFileExists(const f: string): boolean;
    function GetFilename: string;
    function GetTextFile(const f: string): string;
    procedure Clear;
    procedure LoadFromFile(const cachefilename: string);
    procedure MakeTemporary;
    procedure New(const cachefilename: string);
    procedure Open(const cachefilename: string);
    procedure ExtractFile(const f, outfilename: string);
    procedure SaveToFile(const cachefilename: string);
    procedure SaveFolderToFile(const cachefilename: string; const folder: string = '/');
    procedure StoreFile(const vfs_filename, fs_filename: string);
    procedure StoreString(const f, content: string);
    constructor Create;
    destructor Destroy; override;
    // properties
    property Filename: string read fFilename write fFilename;
  end;

implementation

uses CatFiles, CatDCP, CatStrings, CatZIP, CatStringLoop;

// Resets the cache
procedure TCatStorage.Clear;
begin
  fVFS := nil;
  fVFS := CreateStructuredStorage;
  fVFS.Initialize(Filename, fmCreate);
end;

function TCatStorage.CachedFileExists(const f: string): boolean;
begin
  result := fVFS.FileExists(f);
end;

// Extracts a cached file
procedure TCatStorage.ExtractFile(const f, outfilename: string);
var
  v: TStream;
  fs: TFileStream;
begin
  v := fVFS.OpenFile(f, fmOpenRead);
  fs := TFileStream.Create(outfilename, fmCreate);
  fs.CopyFrom(v, v.Size);
  fs.Free;
  FreeAndNil(v);
end;

// Gets the contents of a cached text file
function TCatStorage.GetTextFile(const f: string): string;
var
  v: TStream;
  sl: tstringlist;
begin
  sl := tstringlist.Create;
  v := fVFS.OpenFile(f, fmOpenRead);
  sl.LoadFromStream(v);
  result := sl.Text;
  FreeAndNil(v);
  sl.Free;
end;

// Stores a disk file in the cache
procedure TCatStorage.StoreFile(const vfs_filename, fs_filename: string);
var
  v: TStream;
  fs: TFileStream;
begin
  fs := TFileStream.Create(fs_filename, fmOpenRead);
  v := fVFS.OpenFile(vfs_filename, fmCreate);
  v.CopyFrom(fs, fs.Size);
  FreeAndNil(v);
  FreeAndNil(fs);
end;

// Stores a string as a file in the cache
procedure TCatStorage.StoreString(const f, content: string);
var
  v: TStream;
  sl: tstringlist;
begin
  fCriticalSection.Enter;
  sl := tstringlist.Create;
  sl.Text := content;
  v := fVFS.OpenFile(f, fmCreate);
  sl.SaveToStream(v);
  FreeAndNil(v);
  sl.Free;
  // showmessage('storing: '+f+' size:'+inttostr(length(content)));
end;

procedure TCatStorage.LoadFromFile(const cachefilename: string);
begin
  if FileExists(cachefilename) = false then
    exit;
  fVFS := nil; // releases the storage file or we cannot copy the file
  FileCopy(cachefilename, Filename);
  fVFS := CreateStructuredStorage;
  fVFS.Initialize(Filename, fmOpenReadWrite);
end;

procedure TCatStorage.SaveToFile(const cachefilename: string);
begin
  fVFS := nil; // releases the storage or we cannot copy the file
  FileCopy(Filename, cachefilename);
  fVFS := CreateStructuredStorage;
  fVFS.Initialize(Filename, fmOpenReadWrite); // reopens the storage file
end;

procedure TCatStorage.SaveFolderToFile(const cachefilename: string;
  const folder: string = '/');
var
  slp: TStringLoop;
  exportvfs: IGPStructuredStorage;
  source, dest: TStream;
  tempcachefilename: string;
begin
  tempcachefilename := cachefilename + '.tmp';
  exportvfs := CreateStructuredStorage;
  exportvfs.Initialize(tempcachefilename, fmCreate);
  slp := TStringLoop.Create;
  fVFS.FileNames(folder, slp.List);
  while slp.Found do
  begin
    dest := exportvfs.OpenFile(folder + slp.current, fmCreate);
    source := fVFS.OpenFile(folder + slp.current, fmOpenRead);
    dest.CopyFrom(source, source.Size);
    FreeAndNil(source);
    FreeAndNil(dest);
  end;
  slp.Free;
  exportvfs := nil;
  FileCopy(tempcachefilename, cachefilename);
  deletefile(tempcachefilename);
end;

function TCatStorage.GetFilename: string;
begin
  result := fFilename;
end;

procedure TCatStorage.New(const cachefilename: string);
begin
  Filename := cachefilename;
  fVFS.Initialize(Filename, fmCreate);
end;

procedure TCatStorage.Open(const cachefilename: string);
begin
  if FileExists(cachefilename) = false then
    exit;
  fFilename := cachefilename;
  fVFS.Initialize(cachefilename, fmOpenReadWrite);
end;

procedure TCatStorage.MakeTemporary;
begin
  fDeleteWhenFreeing := true;
end;

constructor TCatStorage.Create;
begin
  inherited Create;
  fCriticalSection := TCriticalSection.Create;
  fDeleteWhenFreeing := false;
  fVFS := CreateStructuredStorage;
end;

destructor TCatStorage.Destroy;
begin
  fVFS := nil;
  if fDeleteWhenFreeing then
    deletefile(fFilename);
  fCriticalSection.Free;
  inherited;
end;

// ------------------------------------------------------------------------//
end.
