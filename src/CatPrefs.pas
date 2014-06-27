unit CatPrefs;

{
  Catarinka Preferences (TCatPreferences)

  Copyright (c) 2013-2014 Felipe Daragon
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
  TCatPreferences = class
  private
    fCurrent: TCatJSON;
    fDefault: TCatJSON;
    fFilename: string;
    fOptionList: TStringList;
    function Encrypt(const CID: string; const Value: Variant): Variant;
    function Decrypt(const CID: string; const Value: Variant): Variant;
    function FGetValue(const CID: string): Variant;
    function GetCIDList: string;
  public
    function GetValue(const CID: string): Variant; overload;
    function GetValue(const CID: string; const DefaultValue: Variant)
      : Variant; overload;
    function GetRegValue(const CID: string;
      const DefaultValue: Variant): Variant;
    procedure SetValue(const CID: string; const Value: Variant);
    procedure LoadFromFile(const f: string);
    procedure LoadFromString(const s: string);
    procedure RegisterDefault(const CID: string; const DefaultValue: Variant;
      const AddToOptionList: boolean = true);
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

uses CatDCP, CatStrings, CatDCPKey;

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
    result := strtoaes(Value,GetDCPKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

function TCatPreferences.Decrypt(const CID: string;
  const Value: Variant): Variant;
begin
  if IsEncryptedCID(CID) then
    result := aestostr(Value,GetDCPKey(CATKEY_PASSWORD))
  else
    result := Value;
end;

// Returns the CID of all available options
function TCatPreferences.GetCIDList: string;
begin
  result := fOptionList.text;
end;

function TCatPreferences.FGetValue(const CID: string): Variant;
begin
  result := fCurrent.GetValue(CID, fdefault [CID]);
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

// Reverts to the default configuration
procedure TCatPreferences.RestoreDefaults;
begin
  fCurrent.text := fdefault.text;
end;

procedure TCatPreferences.RegisterDefault(const CID: string;
  const DefaultValue: Variant; const AddToOptionList: boolean = true);
begin
  if AddToOptionList then
  begin
    if fOptionList.indexof(CID) = -1 then
      fOptionList.Add(CID);
  end;
  fDefault [CID] := DefaultValue;
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
  fOptionList := TStringList.Create;
end;

destructor TCatPreferences.Destroy;
begin
  fOptionList.free;
  fDefault.free;
  fCurrent.free;
  inherited;
end;

end.
