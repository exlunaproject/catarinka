unit CatJSON;

{
  Catarinka TCatJSON - JSON Manipulation Object
  Copyright (c) 2010-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  23.07.2017:
  - Added Count property and fixed handling of integer types
  - Added SetValues() for setting the value of multiple paths at the same time
  - Renamed the SetVal() to SetValue() and added result to IncValue()
  25.11.2015:
  - Set empty JSON string when calling SetText('')
  2013:
  - Added the HasPath method
  - Changed the default property from string to variant
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.Variants,
{$ELSE}
  Classes, SysUtils, Variants,
{$ENDIF}
  SuperObject;

const
  EmptyJSONStr = '{}';

type
  TCatJSON = class
  private
    fObject: ISuperObject;
    function GetCount:integer;
    function GetText: string;
    function GetTextUnquoted: string;
    function GetValue_(const Name: string): Variant;
    procedure SetText(const Text: string);
  public
    function GetValue(const Name: string; DefaultValue: Variant): Variant;
    function HasPath(const Name: string): Boolean;
    function IncValue(const Name: string; Int: integer = 1) : Integer;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure SetValue(const Name: string; const Value: Variant);
    procedure SetValues(const Name:array of string; const Value: Variant);
    procedure Clear;
    constructor Create(JSON: string = '');
    destructor Destroy; override;
    // properties
    property Count: integer read GetCount;
    property sObject: ISuperObject read fObject;
    property Text: string read GetText write SetText;
    property TextUnquoted:string read GetTextUnquoted; // JSON with UnquotedKeys
    property Values[const Name: string]: Variant read GetValue_ write SetValue; default;
  end;

function GetJSONVal(JSON, Name: string;DefaultValue: Variant): Variant;
function IsValidJSONName(const S: string): Boolean;

implementation

uses CatFiles, CatStrings;

function IsValidJSONName(const s: string): Boolean;
const
  cJSONChars = ['-', '_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9'];
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(S) do
    if not(CharInSet(S[i], cJSONChars)) then
    begin
      Result := False;
      Break;
    end;
end;

function GetJSONVal(JSON, Name: string;DefaultValue: Variant): Variant;
var
  d: TCatJSON;
begin
  d := TCatJSON.Create(JSON);
  Result := d.GetValue(Name, DefaultValue);
  d.Free;
end;

function TCatJSON.GetCount:integer;
var
  ite: TSuperObjectIter;
begin
  Result := 0;
  if ObjectFindFirst(fObject, ite) then
    repeat
      Inc(Result)
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
end;

function TCatJSON.GetTextUnquoted: string;
var
  ite: TSuperObjectIter;
begin
  Result := '{';
  if ObjectFindFirst(fObject, ite) then
    repeat
      Result := Result + crlf + ite.key + ': ' + ite.Val.AsJson + ',';
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
  Result := Result + '}';
end;

function TCatJSON.GetText: string;
begin
  Result := fObject.AsJson(True);
end;

procedure TCatJSON.LoadFromFile(const Filename: string);
var
  sl: tstringlist;
begin
  sl := tstringlist.Create;
  SL_LoadFromFile(sl, Filename);
  SetText(sl.Text);
  sl.Free;
end;

procedure TCatJSON.SaveToFile(const Filename: string);
var
  sl: tstringlist;
begin
  sl := tstringlist.Create;
  sl.Text := GetText;
  SL_SaveToFile(sl, Filename);
  sl.Free;
end;

procedure TCatJSON.SetText(const Text: string);
var
  JSON: string;
begin
  JSON := Text;
  if JSON = emptystr then
    JSON := EmptyJSONStr;
  fObject := nil;
  fObject := TSuperObject.ParseString(StrToPWideChar(JSON), False);
end;

procedure TCatJSON.Clear;
begin
  fObject.Clear;
end;

constructor TCatJSON.Create(JSON: string = '');
begin
  fObject := TSuperObject.Create(stObject);
  Text := JSON;
end;

destructor TCatJSON.Destroy;
begin
  fObject := nil;
  inherited;
end;

function TCatJSON.IncValue(const Name: string; Int: integer = 1):integer;
begin
  result := GetValue(Name,0) + Int;
  fObject.I[name] := result;
end;

procedure TCatJSON.SetValue(const Name: string; const Value: Variant);
begin
  case TVarData(Value).vType of
    varString, {$IFDEF UNICODE}varUString, {$ENDIF}varOleStr:
      fObject.S[name] := Value;
    varBoolean:
      fObject.b[name] := Value;
    varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord,
    {$IFDEF UNICODE}varUInt64, {$ENDIF} varInt64:
      fObject.i[name] := Value;
    varDouble:
      fObject.d[name] := Value;
  end;
end;

procedure TCatJSON.SetValues(const Name:array of string; const Value: Variant);
var b: Byte;
begin
  for b := Low(Name) to High(Name) do
    if (Name[b] <> emptystr) then
      SetValue(Name[b],Value);
end;

function TCatJSON.HasPath(const Name: string): Boolean;
begin
  Result := False;
  if fObject.O[name] <> nil then
    Result := True;
end;

function TCatJSON.GetValue(const Name: string;
  DefaultValue: Variant): Variant;
begin
  Result := DefaultValue;
  if HasPath(Name) then
  begin
    case fObject.O[name].DataType of
      stNull:
        Result := DefaultValue;
      stBoolean:
        Result := fObject.b[Name];
      stDouble:
        Result := fObject.d[Name];
      stInt:
        Result := fObject.i[Name];
      stString:
        Result := fObject.S[Name];
      stObject, stArray, stMethod:
        Result := DefaultValue;
    end;
  end;
end;

function TCatJSON.GetValue_(const Name: string): Variant;
begin
  Result := GetValue(name, null);
end;

end.
