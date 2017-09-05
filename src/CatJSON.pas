unit CatJSON;

{
  Catarinka TCatJSON - JSON Manipulation Object
  Copyright (c) 2010-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  23.07.2017:
  - Added Count property and fixed handling of integer types
  - Added SetValues() for setting the value of multiple paths at the same time
    Renamed the SetVal() to SetValue() and added result to IncValue()
  - Added TCatJSON_Bool and TCatJSON_Int classes (practical if you want to 
    manipulate a JSON string that will only or mostly contain boolean or
    integer types)
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
    fDefaultValue: Variant;
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
    constructor Create(const JSON: string = '');
    destructor Destroy; override;
    // properties
    property Count: integer read GetCount;
    property GlobalDefaultValue: Variant read fDefaultValue write fDefaultValue;
    property sObject: ISuperObject read fObject;
    property Text: string read GetText write SetText;
    property TextUnquoted:string read GetTextUnquoted; // JSON with UnquotedKeys
    property Values[const Name: string]: Variant read GetValue_ write SetValue; default;
  end;
  
type
  TCatJSON_Bool = class
  private
    fDefaultValue: boolean;
    fJSON: TCatJSON;
    function GetValue_(const Name: string): boolean;
  public
    function GetValue(const Name: string; const DefaultValue: boolean): boolean;
    procedure Clear;
    procedure SetValue(const Name: string; const DefaultValue: boolean);
    constructor Create(JSON: string = ''); overload;
    constructor Create(const DefaultValue: boolean;
      const JSON: string = ''); overload;
    destructor Destroy; override;
    // properties
    property JSON: TCatJSON read fJSON;
    property Values[const Name: string]: boolean read GetValue_
      write SetValue; default;
  end;

type
  TCatJSON_Int = class
  private
    fDefaultValue: integer;
    fJSON: TCatJSON;
    function GetValue_(const Name: string): integer;
  public
    function GetValue(const Name: string; const DefaultValue: integer): integer;
    function IncValue(const Name: string; Int: integer = 1): integer;
    procedure Clear;
    procedure SetValue(const Name: string; const DefaultValue: integer);
    constructor Create(JSON: string = ''); overload;
    constructor Create(const DefaultValue: integer;
      const JSON: string = ''); overload;
    destructor Destroy; override;
    // properties
    property JSON: TCatJSON read fJSON;
    property Values[const Name: string]: integer read GetValue_
      write SetValue; default;
  end;

function GetJSONVal(const JSON, Name: string;const DefaultValue: Variant): Variant;
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

function GetJSONVal(const JSON, Name: string;const DefaultValue: Variant): Variant;
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
  fObject := TSuperObject.ParseString(PWideChar(WideString(JSON)), False);
end;

procedure TCatJSON.Clear;
begin
  fObject.Clear;
end;

constructor TCatJSON.Create(const JSON: string = '');
begin
  fObject := TSuperObject.Create(stObject);
  fDefaultValue := null;
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
  Result := GetValue(name, fDefaultValue);
end;

// ----------------------------------------------------------------------------//

procedure TCatJSON_Bool.Clear;
begin
  fJSON.Clear;
end;

procedure TCatJSON_Bool.SetValue(const Name: string;
  const DefaultValue: boolean);
begin
  fJSON.SetValue(Name, DefaultValue);
end;

function TCatJSON_Bool.GetValue(const Name: string;
  const DefaultValue: boolean): boolean;
begin
  result := fJSON.GetValue(Name, DefaultValue)
end;

function TCatJSON_Bool.GetValue_(const Name: string): boolean;
begin
  result := fJSON.GetValue(Name, fDefaultValue);
end;

constructor TCatJSON_Bool.Create(JSON: string = '');
begin
  inherited Create;
  fJSON := TCatJSON.Create(JSON);
  fDefaultValue := false;
end;

constructor TCatJSON_Bool.Create(const DefaultValue: boolean;
  const JSON: string = '');
begin
  Create(JSON);
  fDefaultValue := DefaultValue;
end;

destructor TCatJSON_Bool.Destroy;
begin
  fJSON.Free;
  inherited;
end;

// ----------------------------------------------------------------------------//

procedure TCatJSON_Int.Clear;
begin
  fJSON.Clear;
end;

procedure TCatJSON_Int.SetValue(const Name: string;
  const DefaultValue: integer);
begin
  fJSON.SetValue(Name, DefaultValue);
end;

function TCatJSON_Int.GetValue(const Name: string;
  const DefaultValue: integer): integer;
begin
  result := fJSON.GetValue(Name, DefaultValue)
end;

function TCatJSON_Int.GetValue_(const Name: string): integer;
begin
  result := fJSON.GetValue(Name, fDefaultValue);
end;

function TCatJSON_Int.IncValue(const Name: string; Int: integer = 1): integer;
begin
  result := fJSON.IncValue(Name, Int)
end;

constructor TCatJSON_Int.Create(JSON: string = '');
begin
  inherited Create;
  fJSON := TCatJSON.Create(JSON);
  fDefaultValue := 0;
end;

constructor TCatJSON_Int.Create(const DefaultValue: integer;
  const JSON: string = '');
begin
  Create(JSON);
  fDefaultValue := DefaultValue;
end;

destructor TCatJSON_Int.Destroy;
begin
  fJSON.Free;
  inherited;
end;

end.
