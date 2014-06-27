unit CatJINI;
{
  Catarinka TJIniList - JSON INIList-Like component

  Copyright (c) 2010-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This component was made to replace the TIniList component.
  It is also similar to TStringList (with the property Values, Strings and Count).

  TODO: Needs some cleanup
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, System.Classes, Winapi.Windows,
{$ELSE}
  Classes, SysUtils, Windows,
{$ENDIF}
  Superobject;

type
  TJIniList = class
  private
    fBackup: Boolean;
    fFileName: string;
    fModified: Boolean;
    fObject: ISuperObject;
    function GetVersion: string;
    function GetJSONLines: string;
    procedure SetJSONLines(json: string);
    function GetValue(const Key: string): string;
    procedure SetValue(const Key: string; const Value: string);
    function GetCount: integer;
    function FixKeyValue(s: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function SaveJSON: Boolean; overload;
    function SaveJSON(const FileName: string): Boolean; overload;
    procedure Clear;
    function LoadJSON: Boolean; overload;
    function LoadJSON(const FileName: string): Boolean; overload;
    function ReadString(Section, Key, default: string): string;
    procedure WriteString(Section, Key, Value: string; Format: string = '');
    function ReadInteger(const Section, Key: string; default: integer): integer;
    procedure WriteInteger(const Section, Key: string; Value: integer);
    function ReadBool(const Section, Key: string; default: Boolean): Boolean;
    procedure WriteBool(const Section, Key: string; Value: Boolean);
    procedure DeleteSection(Section: string);
    procedure DeleteSectionKey(Section, Key: string);
    procedure AddString(const Section, Key, Value: String;
      AddIfRepeated: Boolean);
    // properties
    property Backup: Boolean read fBackup write fBackup;
    property Count: integer read GetCount;
    property FileName: string read fFileName write fFileName;
    property Text: string read GetJSONLines write SetJSONLines;
    property Values[const Key: string]: string read GetValue
      write SetValue; default;
    property Version: string read GetVersion;
    property sObject: ISuperObject read fObject;
  end;

implementation

uses CatStrings;

const
  cBase64 = 'base64';
  cFormatKey = '.format';
  cValuesSection = 'data';
  cVersion = '1.0';

  { TJIniList }

function TJIniList.FixKeyValue(s: string): string;
begin
  Result := ReplaceStr(s, '.', '_dot_'); // dots not allowed
  Result := lowercase(Result);
end;

function TJIniList.GetValue(const Key: string): string;
begin
  Result := ReadString(cValuesSection, Key, emptystr);
end;

procedure TJIniList.SetValue(const Key: string; const Value: string);
begin
  WriteString(cValuesSection, Key, Value);
end;

function TJIniList.GetJSONLines: string;
begin
  Result := fObject.AsJson(true);
end;

procedure TJIniList.SetJSONLines(json: string);
begin
  fObject := nil;
  fObject := TSuperObject.ParseString(StrToPWideChar(json), false)
end;

constructor TJIniList.Create;
begin
  inherited Create;
  fBackup := false;
  fObject := TSuperObject.Create(stObject);
end;

procedure TJIniList.Clear;
begin
  fObject.Clear;
end;

destructor TJIniList.Destroy;
begin
  fObject := nil;
  inherited;
end;

function TJIniList.LoadJSON: Boolean;
begin
  Result := LoadJSON(fFileName);
end;

function TJIniList.LoadJSON(const FileName: string): Boolean;
var
  Stream: TStream;
  FLines: TStringlist;
begin
  Result := false;
  FLines := TStringlist.Create;
  if FileName <> emptystr then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FLines.LoadFromStream(Stream);
      SetJSONLines(FLines.Text);
      Result := true;
    except
      Result := false;
    end;
    Stream.Free;
  end;
  FLines.Free;
end;

function TJIniList.GetVersion: string;
begin
  Result := cVersion;
end;

function TJIniList.ReadBool(const Section, Key: string;
  default: Boolean): Boolean;
begin
  Result := Boolean(ReadInteger(Section, Key, integer(default)));
end;

function TJIniList.ReadInteger(const Section, Key: string;
  default: integer): integer;
begin
  Result := StrToInt(ReadString(Section, Key, IntToStr(default)));
end;

function TJIniList.SaveJSON: Boolean;
begin
  Result := SaveJSON(fFileName);
end;

function TJIniList.SaveJSON(const FileName: string): Boolean;
var
  SL: TStringlist;
  Stream: TStream;
begin
  Result := false;
  if fBackup then
    CopyFile(
{$IFDEF UNICODE}PWideChar{$ELSE}Pchar{$ENDIF}(FileName),
{$IFDEF UNICODE}PWideChar{$ELSE}Pchar{$ENDIF}(FileName + '.bak'), false);
  if FileName = emptystr then
    exit;
  if fileexists(FileName) = false then
  begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenWrite or
      fmShareDenyWrite);
    Stream.Free;
  end;
  SL := TStringlist.Create;
  SL.Text := fObject.AsJson(true);
  Stream := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite);
  Stream.size := 0;
  try
    SL.SaveToStream(Stream);
    Result := true;
  except
    Result := false;
  end;
  Stream.Free;
  SL.Free;

  fModified := false;
end;

procedure TJIniList.WriteBool(const Section, Key: string; Value: Boolean);
begin
  WriteInteger(Section, Key, integer(Value));
end;

procedure TJIniList.WriteInteger(const Section, Key: string; Value: integer);
begin
  WriteString(Section, Key, IntToStr(Value));
end;

procedure TJIniList.WriteString(Section, Key, Value: string;
  Format: string = '');
var
  sk: string;
begin
  Section := FixKeyValue(Section);
  Key := FixKeyValue(Key);
  sk := Section + '.' + Key;
  if ReadString(Section, Key, emptystr) = Value then
    exit;
  if Format <> emptystr then
    fObject.s[sk + cFormatKey] := Format;
  if Format = cBase64 then
    Value := Base64EnCode(Value);
  fObject.s[sk] := Value;
  fModified := true;
end;

function TJIniList.ReadString(Section, Key, default: string): string;
var
  fmt: string;
  sk: string;
begin
  Section := FixKeyValue(Section);
  Key := FixKeyValue(Key);
  sk := Section + '.' + Key;
  Result := default;
  if fObject.s[sk] <> emptystr then
    Result := fObject.s[sk]
  else
    Result := default;
  if fObject.s[sk + cFormatKey] <> emptystr then
  begin
    fmt := fObject.s[sk + cFormatKey];
    if fmt = cBase64 then
      Result := Base64DeCode(Result);
  end;
end;

procedure TJIniList.AddString(const Section, Key, Value: String;
  AddIfRepeated: Boolean);
var
  SL: TStringlist;
begin
  SL := TStringlist.Create;
  SL.commatext := ReadString(Section, Key, emptystr);
  if AddIfRepeated = true then
    SL.Add(Value)
  else
  begin
    if SL.indexof(Value) = -1 then
      SL.Add(Value);
  end;
  WriteString(Section, Key, SL.commatext);
  SL.Free;
end;

procedure TJIniList.DeleteSection(Section: string);
begin
  Section := FixKeyValue(Section);
  fObject.o[Section].Clear;
  fModified := true;
end;

procedure TJIniList.DeleteSectionKey(Section, Key: string);
begin
  Section := FixKeyValue(Section);
  Key := FixKeyValue(Key);
  fObject.o[Section + '.' + Key].Clear;
  fModified := true;
end;

function TJIniList.GetCount: integer;
begin
  Result := 0;
  // TODO: not implemented yet
end;

end.
