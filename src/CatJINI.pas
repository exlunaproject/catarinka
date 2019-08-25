unit CatJINI;
{
  Catarinka TJIniList - JSON-Based INI-Like component

  Copyright (c) 2010-2019 Felipe Daragon
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
  CatJSON;

type
  TJIniList = class
  private
    fBackup: Boolean;
    fCaseSensitive: Boolean;
    fFileName: string;
    fModified: Boolean;
    fCurrent: TCatJSON;
    fStoreBoolAsInteger: Boolean;
    fVersion: string;
    function GetJSON: string;
    procedure SetJSON(json: string);
    function GetValue(const Key: string): string;
    procedure SetValue(const Key: string; const Value: string);
    function GetCount: integer;
    function FilterPath(s: string): string;
    procedure GetSectionKeyPath(var Section, Key, Path: string);
    procedure SetFilename(s:string);
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
    property FileName: string read fFileName write SetFilename;
    property StoreBoolAsInteger: Boolean read fStoreBoolAsInteger write fStoreBoolAsInteger;
    property Text: string read GetJSON write SetJSON;
    property Values[const Key: string]: string read GetValue
      write SetValue; default;
    property Version: string read fVersion;
    property Current: TCatJSON read fCurrent;
  end;

implementation

uses CatStrings, CatFiles;

const
  cBackupExt = '.bak';
  cBase64 = 'base64';
  cFormatKey = '_format.';
  cKeySeparator = '.';
  cValuesSection = 'data';
  cVersion = '1.04';

  { TJIniList }

procedure TJIniList.SetFilename(s:string);
begin
 {$IFNDEF MSWINDOWS}
  s := ReplaceStr(s, '\', '/');
 {$ENDIF}
  fFileName := s;
end;

function TJIniList.FilterPath(s: string): string;
begin
  // Replaces some chars (dots, quote and spaces are not allowed)
  Result := ReplaceStr(s, cKeySeparator, '_dot_');
  Result := StripChars(result, ['"','(',')','[',']']);
  Result := ReplaceStr(result, ' ', '_');
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
  Result := fCurrent.Text;
end;

procedure TJIniList.SetJSON(json: string);
begin
  fCurrent.Text := json;
end;

procedure TJIniList.Clear;
begin
  fCurrent.Clear;
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
  if fStoreBoolAsInteger = false then
  Result := StrToBool(ReadString(Section, Key, BoolToStr(default))) else
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

 {$IFNDEF WINDOWS}
  forcedir(extractfilepath(filename));
 {$ENDIF}

  if fBackup and FileExists(FileName) then
    FileCopy(FileName, FileName + cBackupExt);

  sl := TStringlist.Create;
  sl.Text := fCurrent.Text;
  if SL_SaveToFile(sl, FileName) then
  begin
    Result := true;
    fModified := false;
  end;
  sl.Free;
end;

procedure TJIniList.WriteBool(const Section, Key: string; Value: Boolean);
begin
  if fStoreBoolAsInteger = false then
  WriteString(Section, Key, BoolToStr(Value)) else
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
    fCurrent.sObject.s[cFormatKey + Path] := Format;
  if Format = cBase64 then
    Value := Base64EnCode(Value);
  fCurrent.sObject.s[Path] := Value;
  fModified := true;
end;

function TJIniList.ReadString(Section, Key, default: string): string;
var
  fmt: string;
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  Result := default;
  if fCurrent.sObject.s[Path] <> emptystr then
    Result := fCurrent.sObject.s[Path]
  else
    Result := default;
  if fCurrent.sObject.s[cFormatKey + Path] <> emptystr then
  begin
    fmt := fCurrent.sObject.s[cFormatKey + Path];
    if fmt = cBase64 then
      Result := Base64DeCode(Result);
  end;
end;

function TJIniList.SectionExists(Section: string): Boolean;
begin
  Section := FilterPath(Section);
  Result := fCurrent.HasPath(Section);
end;

function TJIniList.SectionKeyExists(Section, Key: string): Boolean;
var
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  Result := fCurrent.HasPath(Path);
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
  fCurrent.RemovePath(Section);
  fModified := true;
end;

procedure TJIniList.DeleteSectionKey(Section, Key: string);
var
  Path: string;
begin
  GetSectionKeyPath(Section, Key, Path);
  fCurrent.RemovePath(Path);
  fModified := true;
end;

function TJIniList.GetCount: integer;
begin
  Result := fCurrent.Count;
end;

constructor TJIniList.Create;
begin
  inherited Create;
  fBackup := false;
  fCaseSensitive := false;
  fStoreBoolAsInteger := false;
  fVersion := cVersion;
  fCurrent := TCatJSON.Create;
end;

destructor TJIniList.Destroy;
begin
  fCurrent.Free;
  inherited;
end;

end.
