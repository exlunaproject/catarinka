{
  Catarinka TStringListCache
  Caches multiple string lists, allowing them to be loaded or saved together
  to an external JSON file
  
  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

unit CatStringListCache;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, unitObjectCache, CatJSON;
{$ELSE}
  Classes, SysUtils, unitObjectCache, CatJSON;
{$ENDIF}

type
  TStringListCache = class
  protected
    fCache: TObjectCache;
    fNameList: TStringList;
  public
    function GetList(const AName:string):TStringList;
    procedure ClearLists; overload;
    procedure ClearLists(const Key:array of string); overload;
    procedure LoadFromFile(const AFilename:string);
    procedure SaveToFile(const AFilename:string);
    constructor Create(ACapacity : Integer = 1000; OwnsObjects : boolean = true);
    destructor Destroy; override;
    property Cache: TObjectCache read fCache;
    property Lists[const AName: string]: TStringList read GetList; default;
  end;

type
  TCachedStringList = class (TStringList)
  private
    ID: string;
  end;

implementation

uses CatStringLoop;

const
  cCacheKeyPrefix = 'cache.';
  cIDListKey = 'idlist';

procedure TStringListCache.ClearLists;
var
  csl: TCachedStringList;
  m, c: integer;
begin
  m := fCache.Count;
  for c := m - 1 downto 0 do
  begin
    csl := fCache.ObjectAt(c) as TCachedStringList;
    csl.Clear;
  end;
end;

procedure TStringListCache.ClearLists(const Key:array of string);
var X: Byte;
begin
for X := Low(Key) to High(Key) do
    if not (Key[X] = '') then
 getlist(Key[X]).clear;
end;

procedure TStringListCache.LoadFromFile(const AFilename:string);
var slp:TStringLoop; sl:TStringList; j:TCatJSON;
begin
  fCache.Clear;
  fNameList.Clear;
  j := TCatJSON.Create;
  j.LoadFromFile(AFilename);
  slp := TStringLoop.Create;
  slp.LoadFromString(j[cIDListKey]);
  while slp.Found do begin
    sl := GetList(slp.Current);
    sl.Text:= j[cCacheKeyPrefix+slp.Current];
  end;
  slp.Free;
  j.Free;
end;

procedure TStringListCache.SaveToFile(const AFilename:string);
var
  slp:TStringLoop; j:TCatJSON;
  csl: TCachedStringList;
  m, c: integer;
begin
  j := TCatJSON.Create;
  j.SetValue(cIDListKey,fNameList.Text);
  m := fCache.Count;
  for c := m - 1 downto 0 do
  begin
    csl := fCache.ObjectAt(c) as TCachedStringList;
    j.SetValue(cCacheKeyPrefix+csl.ID, csl.text);
  end;
  j.SaveToFile(AFilename);
  j.Free;
end;

function TStringListCache.GetList(const AName:string):TStringList;
var
  csl: TCachedStringList;
  m, c: integer;
begin
  result:= nil;
  m :=  fCache.Count;
  for c := m - 1 downto 0 do
  begin
    csl := fCache.ObjectAt(c) as TCachedStringList;
    if csl.ID = aName then
    result:= csl;
  end;
  if result = nil then begin
    csl := TCachedStringList.Create;
    csl.id := AName;
    fCache.Add(csl);
    fNameList.Add(AName);
    result := csl;
  end;
end;

constructor TStringListCache.Create(ACapacity : Integer = 1000; OwnsObjects : boolean = true);
begin
  fCache := TObjectCache.Create(ACapacity, OwnsObjects);
  fNameList:= TStringList.Create;
end;

destructor TStringListCache.Destroy;
begin
  fNameList.Free;
  fCache.free;
  inherited;
end;

end.
