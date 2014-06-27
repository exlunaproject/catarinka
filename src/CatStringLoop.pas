{
  Catarinka TStringLoop
  Loops through a string list
  
  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

unit CatStringLoop;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

type
  TStringLoop = class
  protected
    fCSV: TStringList;
    fCurrent: string;
    fIsCSV: boolean;
    fList: TStringList;
    fPosition: integer;
    function GetCount: integer;
    function GetCountAsStr: string;
    function GetCurrentLower: string;
    function GetCurrentUpper: string;
    function GetLine(const l: integer): string;
    procedure SetCurrent(const s: string);
    procedure SetLine(const l: integer; const v: string);
  public
    constructor Create(const sl: tstrings = nil);
    destructor Destroy; override;
    procedure Load(const sl: tstrings);
    procedure LoadFromString(const s: string);
    procedure LoadFromFile(const filename: string);
    procedure Reset;
    procedure Stop;
    procedure Clear;
    function Found: boolean;
    function GetValue(const s: string): string;
    procedure SetValue(const s: string; const v: string);
    function Index(const zerostart: boolean = true): integer;
    function IndexAsStr: string;
    function IndexOf(const s: string): integer;
    function Contains(const s: string; const casesensitive: boolean = true): boolean;
    procedure Delete;
    property Lines[const l: integer]: string read GetLine write SetLine;
    property Values[const s: string]: string read GetValue
      write SetValue; default;
  // properties
    property Count: integer read GetCount;
    property CountAsStr:string read GetCountAsStr;
    property Current: string read fCurrent write SetCurrent;
    property CurrentLower: string read GetCurrentLower;
    property CurrentUpper: string read GetCurrentUpper;
    property IsCSV: boolean read fIsCSV write fIsCSV;
    property List: TStringList read FList;
  end;

implementation

function TStringLoop.GetCount: integer;
begin
  result := fList.Count;
end;

procedure TStringLoop.Delete;
begin
  fList.Delete(FPosition - 1);
end;

function TStringLoop.Contains(const s: string;
  const casesensitive: boolean = true): boolean;
  procedure check(substr, str: string);
  begin
    if pos(substr, str) <> 0 then
      result := true;
  end;

begin
  result := false;
  case casesensitive of
    false:
      check(uppercase(s), uppercase(FCurrent));
    true:
      check(s, FCurrent);
  end;
end;

procedure TStringLoop.Stop;
begin
  FPosition := fList.Count;
end;

function TStringLoop.GetCurrentUpper: string;
begin
  result := uppercase(FCurrent);
end;

function TStringLoop.GetCurrentLower: string;
begin
  result := lowercase(FCurrent);
end;

function TStringLoop.IndexAsStr: string;
begin
  result := inttostr(FPosition);
end;

function TStringLoop.GetCountAsStr: string;
begin
  result := inttostr(fList.Count);
end;

function TStringLoop.IndexOf(const s: string): integer;
begin
  result := fList.IndexOf(s);
end;

function TStringLoop.Index(const zerostart: boolean = true): integer;
begin
  result := FPosition;
  if zerostart then
    result := FPosition - 1;
end;

procedure TStringLoop.Clear;
begin
  fList.Clear;
  Reset;
end;

function TStringLoop.GetValue(const s: string): string;
begin
  if isCSV = false then
    fcsv.commatext := FCurrent;
  result := fcsv.Values[s];
end;

procedure TStringLoop.SetValue(const s, v: string);
begin
  if isCSV = false then
    fcsv.commatext := FCurrent;
  fcsv.Values[s] := v;
  SetCurrent(fcsv.commatext);
end;

function TStringLoop.GetLine(const l: integer): string;
begin
  if isCSV = false then
    fcsv.commatext := FCurrent;
  try
    result := fcsv[l];
  except
  end;
end;

procedure TStringLoop.SetLine(const l: integer; const v: string);
begin
  if isCSV = false then
    fcsv.commatext := FCurrent;
  try
    fcsv[l] := v;
  except
  end;
  SetCurrent(fcsv.commatext);
end;

procedure TStringLoop.SetCurrent(const s: string);
begin
  FCurrent := s;
  fList[FPosition - 1] := s;
end;

function TStringLoop.Found: boolean;
var
  i: integer;
begin
  result := false;
  i := FPosition;
  if i < fList.Count then
  begin
    result := true;
    FCurrent := fList[i];
    // If each line is a CSV string and the IsCSV property is set
    // to true, it should be faster to retrive a value (via GetValue).
    if isCSV then
      fcsv.commatext := FCurrent;
    FPosition := FPosition + 1;
  end;
end;

procedure TStringLoop.Reset;
begin
  FPosition := 0;
  FCurrent := emptystr;
  fcsv.Clear;
end;

procedure TStringLoop.LoadFromFile(const filename: string);
begin
  fList.LoadFromFile(filename);
  Reset;
end;

procedure TStringLoop.LoadFromString(const s: string);
begin
  fList.text := s;
  Reset;
end;

procedure TStringLoop.Load(const sl: tstrings);
begin
  LoadFromString(sl.text);
end;

constructor TStringLoop.Create(const sl: tstrings = nil);
begin
  isCSV := false;
  fList := tstringlist.Create;
  fcsv := tstringlist.Create;
  if sl <> nil then
    Load(sl);
  Reset;
end;

destructor TStringLoop.Destroy;
begin
  fList.free;
  fcsv.free;
  inherited;
end;

end.
