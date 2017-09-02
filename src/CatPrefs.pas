unit CatPrefs;

{
  Catarinka Preferences (TCatPreferences)

  Copyright (c) 2013-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.Variants,
{$ELSE}
  Classes, SysUtils, Variants,
{$ENDIF}
  CatJSON;

type
  TCatPrefsCustomOption = record
    HideInOptionList: boolean;
    Tags: string; // comma separated tags
  end;

type
  TCatPreferences = class
  private
    fCurrent: TCatJSON;
    fDefault: TCatJSON;
    fFilename: string;
    fOptionList: TStringList;
    fTags: TCatJSON;
    function Encrypt(const CID: string; const Value: Variant): Variant;
    function Decrypt(const CID: string; const Value: Variant): Variant;
    function FGetValue(const CID: string): Variant;
    function GetCIDList: string;
  public
    function GetCIDListByTag(const aTag: string): string;
    function GetValue(const CID: string): Variant; overload;
    function GetValue(const CID: string; const DefaultValue: Variant)
      : Variant; overload;
    function GetRegValue(const CID: string;
      const DefaultValue: Variant): Variant;
    procedure SetValue(const CID: string; const Value: Variant);
    procedure SetValues(const CIDs: array of string; const Value: Variant);
    procedure SetValuesByTag(const Tag: string; const Value: Variant);
    procedure LoadFromFile(const f: string);
    procedure LoadFromString(const s: string);
    procedure RegisterDefault(const CID: string;
      const DefaultValue: Variant); overload;
    procedure RegisterDefault(const CID: string; const DefaultValue: Variant;
      const Tags: array of string); overload;
    procedure RegisterDefault(const CID: string; const DefaultValue: Variant;
      const Custom: TCatPrefsCustomOption); overload;
    procedure RestoreDefaults;
    procedure SaveToFile(const f: string);
    constructor Create;
    destructor Destroy; override;
    // properties
    property CIDList: string read GetCIDList;
    property Current: TCatJSON read fCurrent;
    property Default: TCatJSON read fDefault;
    property Filename: string read fFilename write fFilename;
    property OptionList: TStringList read fOptionList;
    property Values[const CID: string]: Variant read FGetValue
      write SetValue; default;
  end;

implementation

uses CatDCP, CatStrings, CatStringLoop, CatDCPKey;

function IsEncryptedCID(CID: string): boolean;
begin
  result := false;
  if endswith(CID, '.password') then
    result := true
  else if endswith(CID, '.encrypted') then
    result := true;
end;

function TCatPreferences.Encrypt(const CID: string;
  const Value: Variant): Variant;
begin
  if IsEncryptedCID(CID) then
    result := ansistrtoaes(Value, GetDCPKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

function TCatPreferences.Decrypt(const CID: string;
  const Value: Variant): Variant;
begin
  if IsEncryptedCID(CID) then
    result := aestoansistr(Value, GetDCPKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

// Returns the CID of all available options
function TCatPreferences.GetCIDList: string;
begin
  result := fOptionList.text;
end;

// Returns a CID list by a specific tag
function TCatPreferences.GetCIDListByTag(const aTag: string): string;
var
  CID: TStringLoop;
  Tag: TSepStringLoop;
  list: TStringList;
  cidtags: string;
begin
  list := TStringList.Create;
  CID := TStringLoop.Create(fOptionList);
  while CID.Found do
  begin
    cidtags := fTags.GetValue(CID.Current, emptystr);
    if cidtags <> emptystr then
    begin
      Tag := TSepStringLoop.Create(cidtags, ',');
      while Tag.Found do
      begin
        if Tag.Current = aTag then
          list.Add(CID.Current);
      end;
      Tag.Free;
    end;
  end;
  CID.Free;
  result := list.text;
  list.Free;
end;

function TCatPreferences.FGetValue(const CID: string): Variant;
begin
  result := fCurrent.GetValue(CID, fDefault[CID]);
  result := Decrypt(CID, result);
end;

function TCatPreferences.GetValue(const CID: string): Variant;
begin
  result := FGetValue(CID);
end;

function TCatPreferences.GetValue(const CID: string;
  const DefaultValue: Variant): Variant;
begin
  result := fCurrent.GetValue(CID, DefaultValue);
  result := Decrypt(CID, result);
end;

function TCatPreferences.GetRegValue(const CID: string;
  const DefaultValue: Variant): Variant;
begin
  RegisterDefault(CID, DefaultValue);
  result := fCurrent.GetValue(CID, DefaultValue);
  result := Decrypt(CID, result);
end;

procedure TCatPreferences.SetValue(const CID: string; const Value: Variant);
begin
  fCurrent[CID] := Encrypt(CID, Value);
end;

procedure TCatPreferences.SetValues(const CIDs: array of string;
  const Value: Variant);
var
  b: Byte;
begin
  for b := Low(CIDs) to High(CIDs) do
    if (CIDs[b] <> emptystr) then
      SetValue(CIDs[b], Value);
end;

procedure TCatPreferences.SetValuesByTag(const Tag: string;
  const Value: Variant);
var
  CID: TStringLoop;
begin
  CID := TStringLoop.Create(GetCIDListByTag(Tag));
  while CID.Found do
    SetValue(CID.Current, Value);
  CID.Free;
end;

// Reverts to the default configuration
procedure TCatPreferences.RestoreDefaults;
begin
  fCurrent.text := fDefault.text;
end;

procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant);
begin
  if fOptionList.indexof(CID) = -1 then
    fOptionList.Add(CID);
  fDefault[CID] := DefaultValue;
end;

procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant; const Tags: array of string);
var
  b: Byte;
  taglist: string;
begin
  RegisterDefault(CID, DefaultValue);
  taglist := emptystr;
  for b := Low(Tags) to High(Tags) do
    if (Tags[b] <> emptystr) then
    begin
      if taglist = emptystr then
        taglist := Tags[b]
      else
        taglist := taglist + ',' + Tags[b];
    end;
  fTags[CID] := taglist;
end;

procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant; const Custom: TCatPrefsCustomOption);
begin
  if Custom.HideInOptionList = false then
  begin
    if fOptionList.indexof(CID) = -1 then
      fOptionList.Add(CID);
  end;
  if Custom.Tags <> emptystr then
    fTags[CID] := Custom.Tags;
  fDefault[CID] := DefaultValue;
end;

procedure TCatPreferences.LoadFromFile(const f: string);
begin
  Filename := f;
  fCurrent.LoadFromFile(f);
end;

procedure TCatPreferences.LoadFromString(const s: string);
begin
  if s = emptystr then
    fCurrent.text := EmptyJSONStr
  else
    fCurrent.text := s;
end;

procedure TCatPreferences.SaveToFile(const f: string);
begin
  fCurrent.SaveToFile(f);
end;

constructor TCatPreferences.Create;
begin
  inherited Create;
  fCurrent := TCatJSON.Create;
  fDefault := TCatJSON.Create;
  fTags := TCatJSON.Create;
  fOptionList := TStringList.Create;
end;

destructor TCatPreferences.Destroy;
begin
  fOptionList.Free;
  fTags.Free;
  fDefault.Free;
  fCurrent.Free;
  inherited;
end;

end.
