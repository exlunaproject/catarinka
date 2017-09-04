unit CatJINI;
{
  Catarinka TJIniList - JSON-Based INI-Like component

  Copyright (c) 2010-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This component was made as an alternative to the TIniList component (by
  Simon Reinhardt) and, for this reason, it shares many of its properties.
  Like TIniList, it holds all the values in memory, not in a file. With
  SaveToFile and LoadFromFile the values are written or read to/from a file
  using the JSON format.
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
    fCaseSensitive: Boolean;
    fFileName: string;
    fModified: Boolean;
    fObject: ISuperObject;
    fVersion: string;
    function GetJSON: string;
    procedure SetJSON(json: string);
    function GetValue(const Key: string): string;
    procedure SetValue(const Key: string; const Value: string);
    function GetCount: integer;
    function FilterPath(s: string): string;
    procedure GetSectionKeyPath(var Section, Key, Path: string);
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToFile: Boolean; overload;
    function SaveToFile(const FileName: string): Boolean; overload;
    function SectionExists(Section: string): Boolean;
    function SectionKeyExists(Section, Key: string): Boolean;
    procedure Clear;
    function LoadFromFile: Boolean; overload;
    function LoadFromFile(const FileName: string): Boolean; overload;
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
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
    property Count: integer read GetCount;
    property FileName: string read fFileName write fFileName;
    property Text: string read GetJSON write SetJSON;
    property Values[const Key: string]: string read GetValue
      write SetValue; default;
    property Version: string read fVersion;
    property sObject: ISuperObject read fObject;
  end;

implementation

uses CatStrings, CatFiles;

const
  cBackupExt = '.bak';
  cBase64 = 'base64';
  cFormatKey = '.format';
  cKeySeparator = '.';
  cValuesSection = 'data';
  cVersion = '1.0';

  { TJIniList }

function TJIniList.FilterPath(s: string): string;
begin
  Result := ReplaceStr(s, cKeySeparator, '_dot_'); // dots not allowed
  Result := ReplaceStr(result, '"', emptystr); // quote not allowed
  if fCaseSensitive = false then
    Result := lowercase(Result);
end;

procedure TJIniList.GetSectionKeyPath(var Section, Key, Path: string);
begin
  Section := FilterPath(Section);
  Key := FilterPath(Key);
  Path := Section + cKeySeparator + Key;
end;

function TJIniList.GetValue(const Key: string): string;
begin
  Result := ReadString(cValuesSection, Key, emptystr);
end;

procedure TJIniList.SetValue(const Key: string; const Value: string);
begin
  WriteString(cValuesSection, Key, Value);
end;

function TJIniList.GetJSON: string;
begin
  Result := fObject.AsJson(true);
end;

procedure TJIniList.SetJSON(json: string);
begin
  fObject := nil;
  fObject := TSuperObject.ParseString(StrToPWideChar(json), false)
end;

procedure TJIniList.Clear;
begin
  fObject.Clear;
end;

function TJIniList.LoadFromFile: Boolean;
begin
  Result := LoadFromFile(fFileName);
end;

function TJIniList.LoadFromFile(const FileName: string): Boolean;
var
  sl: TStringlist;
begin
  Result := false;
  sl := TStringlist.Create;
  if SL_LoadFromFile(sl, FileName) then
  begin
    SetJSON(sl.Text);
    Result := true;
  end;
  sl.Free;
end;

function TJIniList.ReadBool(const Section, Key: string;
  default: Boolean): Boolean;
begin
  Result := Boolean(ReadInteger(Section, Key, integer(default)));
end;

function TJIniList.ReadInteger(const Section, Key: string;
  default: integer): integer;
var
  s:string;
begin
  s := ReadString(Section, Key, IntToStr(default));
 // if not an integer, return default
  Result := StrToIntDef(s, default);
end;

function TJIniList.SaveToFile: Boolean;
begin
  Result := SaveToFile(fFileName);
end;

function TJIniList.SaveToFile(const FileName: string): Boolean;
var
  sl: TStringlist;
begin
  Result := false;
  if FileName = emptystr then
    exit;
  if fBackup and FileExists(FileName) then
    FileCopy(FileName, FileName + cBackupExt);

  sl := TStringlist.Create;
  sl.Text := fObject.AsJson(true);
  if SL_SaveToFile(sl, FileName) then
  begin
    Result := true;
    fModified := false;
  end;
  sl.Free;
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
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  if ReadString(Section, Key, emptystr) = Value then
    exit;
  if Format <> emptystr then
    fObject.s[Path + cFormatKey] := Format;
  if Format = cBase64 then
    Value := Base64EnCode(Value);
  fObject.s[Path] := Value;
  fModified := true;
end;

function TJIniList.ReadString(Section, Key, default: string): string;
var
  fmt: string;
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  Result := default;
  if fObject.s[Path] <> emptystr then
    Result := fObject.s[Path]
  else
    Result := default;
  if fObject.s[Path + cFormatKey] <> emptystr then
  begin
    fmt := fObject.s[Path + cFormatKey];
    if fmt = cBase64 then
      Result := Base64DeCode(Result);
  end;
end;

function TJIniList.SectionExists(Section: string): Boolean;
begin
  Section := FilterPath(Section);
  Result := false;
  if fObject.O[Section] <> nil then
    Result := true;
end;

function TJIniList.SectionKeyExists(Section, Key: string): Boolean;
var
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  Result := false;
  if fObject.O[Section] <> nil then
    Result := true;
end;

procedure TJIniList.AddString(const Section, Key, Value: String;
  AddIfRepeated: Boolean);
var
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  sl.commatext := ReadString(Section, Key, emptystr);
  if AddIfRepeated = true then
    sl.Add(Value)
  else
  begin
    if sl.indexof(Value) = -1 then
      sl.Add(Value);
  end;
  WriteString(Section, Key, sl.commatext);
  sl.Free;
end;

procedure TJIniList.DeleteSection(Section: string);
begin
  Section := FilterPath(Section);
  fObject.O[Section].Clear;
  fModified := true;
end;

procedure TJIniList.DeleteSectionKey(Section, Key: string);
var
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  fObject.O[Path].Clear;
  fModified := true;
end;

function TJIniList.GetCount: integer;
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

constructor TJIniList.Create;
begin
  inherited Create;
  fBackup := false;
  fCaseSensitive := false;
  fVersion := cVersion;
  fObject := TSuperObject.Create(stObject);
end;

destructor TJIniList.Destroy;
begin
  fObject := nil;
  inherited;
end;

end.
