unit CatPrefs;

{
  Catarinka Preferences (TCatPreferences)

  Copyright (c) 2013-2019 Felipe Daragon
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
    fCurrentBackup: TCatJSON;
    fDefault: TCatJSON;
    fDefaultValueTypes: TCatJSON;
    fFilename: string;
    fOptionList: TStringList;
    fTags: TCatJSON;
    function Encrypt(const CID: string; const Value: Variant): Variant;
    function Decrypt(const CID: string; const Value: Variant): Variant;
    function FGetValue(const CID: string): Variant;
    function GetCIDList: string;
  public
    function ContainsTag(const CID, aTag:string):boolean;
    function GetCIDListByTag(const aTag: string): string;
    function GetValue(const CID: string): Variant; overload;
    function GetValue(const CID: string; const DefaultValue: Variant)
      : Variant; overload;
    function GetDefaultValue(const CID: string): Variant;
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
    procedure RestoreBackup;
    procedure RestoreBackupByTag(const Tag: string; const Condition:boolean = false);
    procedure RestoreDefaults;
    procedure SaveToFile(const f: string);
    procedure UpdateBackup;
    constructor Create;
    destructor Destroy; override;
    // properties
    property Backup: TCatJSON read fCurrentBackup;
    property CIDList: string read GetCIDList;
    property Current: TCatJSON read fCurrent;
    property Default: TCatJSON read fDefault;
    property Filename: string read fFilename write fFilename;
    property OptionList: TStringList read fOptionList;
    property Values[const CID: string]: Variant read FGetValue
      write SetValue; default;
  end;

implementation

uses CatStrings, CatStringLoop, CatRegex,
{$IFDEF MSWINDOWS}
  CatDCP,
{$ELSE}
  CatCrypt,
{$ENDIF}
  CatCryptKey;

function VariantTypeIDToVariantType(ID:string):word;
begin
  result := varEmpty;
  case ID[1] of
    's': result := varString;
    'b': result := varBoolean;
    'i': result := varInteger;
    'd': result := varDouble;
  end;
end;

function GetVariantTypeID(Value:variant):string;
begin
  case TVarData(Value).vType of
    varEmpty:
      result := emptystr;
    varString, {$IFDEF UNICODE}varUString, {$ENDIF}varOleStr:
      result := 's';
    varBoolean:
      result := 'b';
    varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord,
    {$IFDEF UNICODE}varUInt64, {$ENDIF} varInt64:
      result := 'i';
    varDouble:
      result := 'd';
  end;
end;

// Returns true if the value of a CID must be encrypted, false otherwise
function IsEncryptedCID(CID: string): boolean;
begin
  result := false;
  if endswith(CID, '.password') then
    result := true
  else if endswith(CID, '.encrypted') then
    result := true;
end;

// Encrypts the value of a CID if necessary
function TCatPreferences.Encrypt(const CID: string;
  const Value: Variant): Variant;
begin
  if IsEncryptedCID(CID) then
    result := ansistrtoaes(Value, GetCatKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

// Decrypts the value of a CID if necessary
function TCatPreferences.Decrypt(const CID: string;
  const Value: Variant): Variant;
begin
  if IsEncryptedCID(CID) then
    result := aestoansistr(Value, GetCatKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

// Returns the CID of all available options
function TCatPreferences.GetCIDList: string;
begin
  result := fOptionList.text;
end;

function TCatPreferences.ContainsTag(const CID, aTag:string):boolean;
var
  tags: string;
begin
  tags := fTags.GetValue(CID, emptystr);
  result := MatchStrInSepStr(aTag, tags);
end;

// Returns a CID list by a specific tag
function TCatPreferences.GetCIDListByTag(const aTag: string): string;
var
  CID: TStringLoop;
  list: TStringList;
begin
  list := TStringList.Create;
  CID := TStringLoop.Create(fOptionList);
  while CID.Found do
  begin
    if ContainsTag(CID.Current, aTag) = true then
      list.Add(CID.Current);
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

// Gets the default value of a CID.
// A default CID value must be set using the RegisterValue() or GetRegValue()
// methods
function TCatPreferences.GetDefaultValue(const CID: string): Variant;
begin
  result := fDefault[CID];
  result := Decrypt(CID, result);
end;

// Gets the value of a CID
function TCatPreferences.GetValue(const CID: string): Variant;
begin
  result := FGetValue(CID);
end;

// Gets the value of a CID. DefaultValue is returned if the CID is not set
function TCatPreferences.GetValue(const CID: string;
  const DefaultValue: Variant): Variant;
begin
  result := fCurrent.GetValue(CID, DefaultValue);
  result := Decrypt(CID, result);
end;

// Gets and registers a value at the same time
// DefaultValue is returned if the CID is not set
function TCatPreferences.GetRegValue(const CID: string;
  const DefaultValue: Variant): Variant;
begin
  RegisterDefault(CID, DefaultValue);
  result := fCurrent.GetValue(CID, DefaultValue);
  result := Decrypt(CID, result);
end;

// Sets the value of a CID
procedure TCatPreferences.SetValue(const CID: string; const Value: Variant);
begin
  fCurrent[CID] := Encrypt(CID, Value);
end;

// Sets the value of multiple CIDs at the same time
procedure TCatPreferences.SetValues(const CIDs: array of string;
  const Value: Variant);
var
  b: Byte;
begin
  for b := Low(CIDs) to High(CIDs) do
    if (CIDs[b] <> emptystr) then
      SetValue(CIDs[b], Value);
end;

// Sets the value of multiple CIDs by their tag
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

// Reverts to the backup configuration
procedure TCatPreferences.RestoreBackup;
begin
  fCurrent.text := fCurrentBackup.text;
end;

// Reverts the value of tagged CIDs to the value of the backup configuration,
// This makes the values in backup to overwrite the current configuration values
// if the value in Condition is met
procedure TCatPreferences.RestoreBackupByTag(const Tag: string;
  const Condition:boolean = false);
var
  CID: TStringLoop;
begin
  CID := TStringLoop.Create(GetCIDListByTag(Tag));
  while CID.Found do
    if fCurrentBackup.GetValue(CID.Current, fDefault[CID.Current]) = Condition then
    SetValue(CID.Current, Condition);
  CID.Free;
end;

// Reverts to the default configuration
procedure TCatPreferences.RestoreDefaults;
begin
  fCurrent.text := fDefault.text;
end;

// Registers the default value of a CID with custom actions (like hiding the
// option from the options list, associating tags, etc)
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
  fDefaultValueTypes[CID] := GetVariantTypeID(DefaultValue);
end;

// Registers the default value of a CID
procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant);
var
  opt: TCatPrefsCustomOption;
begin
  opt.HideInOptionList := false;
  opt.Tags := emptystr;
  RegisterDefault(CID, DefaultValue, opt);
end;

// Registers the default value of a CID with tags
procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant; const Tags: array of string);
var
  b: Byte;
  taglist: string;
  opt: TCatPrefsCustomOption;
begin
  taglist := emptystr;
  for b := Low(Tags) to High(Tags) do
    if (Tags[b] <> emptystr) then
    begin
      if taglist = emptystr then
        taglist := Tags[b]
      else
        taglist := taglist + ',' + Tags[b];
    end;
  opt.HideInOptionList := false;
  opt.Tags := taglist;
  RegisterDefault(CID, DefaultValue, opt);
end;

procedure TCatPreferences.UpdateBackup;
begin
  fCurrentBackup.Text := fCurrent.Text;
end;

// Load the preferences from a file
procedure TCatPreferences.LoadFromFile(const f: string);
begin
  Filename := f;
  fCurrent.LoadFromFile(f);
  UpdateBackup;
end;

// Load the preferences from a string
procedure TCatPreferences.LoadFromString(const s: string);
begin
  if s = emptystr then
    fCurrent.text := EmptyJSONStr
  else
    fCurrent.text := s;
  UpdateBackup;
end;

// Save the preferences to a file
procedure TCatPreferences.SaveToFile(const f: string);
begin
  fCurrent.SaveToFile(f);
end;

constructor TCatPreferences.Create;
begin
  inherited Create;
  fCurrent := TCatJSON.Create;
  fCurrentBackup := TCatJSON.Create;
  fDefault := TCatJSON.Create;
  fDefaultValueTypes := TCatJSON.Create;
  fTags := TCatJSON.Create;
  fOptionList := TStringList.Create;
end;

destructor TCatPreferences.Destroy;
begin
  fOptionList.Free;
  fTags.Free;
  fDefaultValueTypes.Free;
  fDefault.Free;
  fCurrentBackup.Free;
  fCurrent.Free;
  inherited;
end;

end.
