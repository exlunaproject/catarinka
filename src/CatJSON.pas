unit CatJSON;

{
  Catarinka TCatJSON - JSON Manipulation Object
  Copyright (c) 2010-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  25.11.2015:
  - Set empty JSON string when calling SetText('')

  2013:
  - Added the HasPath method
  - Changed the default property from string to variant

  TODO:
  - Count property not fully implemented.
  - IncVal(), & SetValInt() may need revision
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
    fCount: integer;
    fObject: ISuperObject;
    function GetText: string;
    function GetTextUnquoted: string;
  public
    function GetVal(const Name: string): Variant;
    function GetValue(const Name: string; DefaultValue: Variant): Variant;
    function HasPath(const Name: string): Boolean;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure SetText(const Text: string);
    procedure SetVal(const Name: string; const Value: Variant);
    procedure IncVal(const Name: string; Int: integer = 1);
    procedure SetValInt(const Name: string; const Value: integer);
    procedure Clear;
    constructor Create(JSON: string = '');
    destructor Destroy; override;
    // properties
    property Count: integer read fCount;
    property IntVal[const Name: string]: integer write SetValInt;
    property sObject: ISuperObject read fObject;
    property Text: string read GetText write SetText;
    property TextUnquoted:string read GetTextUnquoted; // JSON with UnquotedKeys
    property Val[const Name: string]: Variant read GetVal write SetVal; default;
  end;

function CatVariant(Text, ValName: string): Variant;
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

function CatVariant(Text, ValName: string): Variant;
var
  d: TCatJSON;
begin
  d := TCatJSON.Create;
  d.Text := Text;
  Result := d[ValName];
  d.Free;
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
  fCount := 0;
  fObject := TSuperObject.ParseString(StrToPWideChar(JSON), False);
end;

procedure TCatJSON.Clear;
begin
  fObject.Clear;
  fCount := 0;
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

procedure TCatJSON.IncVal(const Name: string; Int: integer = 1);
var
  i: integer;
begin
  if fObject.S[name] = '' then
    i := 0
  else
    i := strtoint(fObject.S[name]);
  i := i + Int;
  fObject.S[name] := inttostr(i);
  inc(fCount);
end;

procedure TCatJSON.SetVal(const Name: string; const Value: Variant);
begin
  case TVarData(Value).vType of
    varString, {$IFDEF UNICODE}varUString, {$ENDIF}varOleStr:
      fObject.S[name] := Value;
    varBoolean:
      fObject.b[name] := Value;
    varInteger, varInt64:
      fObject.i[name] := Value;
    varDouble:
      fObject.d[name] := Value;
  end;
  inc(fCount);
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

function TCatJSON.GetVal(const Name: string): Variant;
begin
  Result := GetValue(name, null);
end;

procedure TCatJSON.SetValInt(const Name: string; const Value: integer);
begin
  SetVal(name, inttostr(Value));
end;

end.
