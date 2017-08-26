{
  Catarinka Loopers
    TStringLoop, TSepStringLoop, TCSVLoop
  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Tired of limited for-to loops? TStringLoop makes it easy to iterate over each
  string in a string list while, optionally, looking for defined patterns.

  Usage Examples:

  Use TStringLoop to iterate over each string in a string list:

    s := TStringLoop.Create(listbox1.items);
    while s.found do
      showmessage(s.current);
    s.free;

  Use TSepStringLoop instead to do the same with a separated string:

    s := TSepStringLoop.Create('str1;str2;str3',';');
    while s.found do
      showmessage(s.current);
    s.free;

  Call Stop, Stop(aVariant) or Break to end the loop:

    while s.found do
      if s.currentlower = 'somestring' then
        result := s.stop(true); // stops the loop, returning true

  Use the Pattern property to call a variety of chainable pattern matching
  methods:

    while s.found do
      if s.pattern.begins(['http://','https://']).ends('.com').match then
        showmessage(s.current);

  Call Reset if you want to repeat from the beginning:
    while s.found do
      showmessage(s.current);
    s.reset;
    while s.found do
      showmessage(s.current);

  ChangeLog:
    26.08.2017:
  - Moved TStringLoop's CSV methods to a separate class (TCSVLoop)
  - Reset TSepStringLoop when setting a new string
    05.08.2017:
  - Added Pattern property that allows to call pattern matching functions
  - Removed Contains() which now can be called through Pattern property.
    Example:
      Pattern.Contains().Match or .MatchI
  - Added overloaded Stop() with result.
}

unit CatStringLoop;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.Variants,
{$ELSE}
  Classes, SysUtils, Variants,
{$ENDIF}
  CatPatterns;

{ TStringLoop loops through a string list }
type
  TStringLoop = class
  protected
    fCurrent: string;
    fList: TStringList;
    fPattern: TStringPattern;
    fPosition: integer;
    function GetCount: integer;
    function GetCountAsStr: string;
    function GetCurrentLower: string;
    function GetCurrentUpper: string;
    procedure SetCurrent(const s: string); virtual;
    procedure SetCurrentLine(const s: string);
  public
    constructor Create(const sl: tstrings = nil); overload; virtual;
    constructor Create(const list:string); overload;
    destructor Destroy; override;
    procedure Load(const sl: tstrings);
    procedure LoadFromString(const s: string);
    procedure LoadFromFile(const filename: string);
    procedure Reset; virtual;
    procedure Stop; overload;
    function Stop(aReturn:Variant):Variant; overload;
    procedure Clear;
    function Found: boolean;
    function Index(const zerostart: boolean = true): integer;
    function IndexAsStr: string;
    function IndexOf(const s: string): integer;
    procedure Delete;
  // properties
    property Count: integer read GetCount;
    property CountAsStr:string read GetCountAsStr;
    property Current: string read fCurrent write SetCurrentLine;
    property CurrentLower: string read GetCurrentLower;
    property CurrentUpper: string read GetCurrentUpper;
    property List: TStringList read FList;
    property Pattern: TStringPattern read fPattern;
  end;

{ TSepStringLoop loops through a pipe-separated string.
  A different separator can be used }
type
  TSepStringLoop = class
  protected
    fCount: integer;
    fCurrent: string;
    fPattern: TStringPattern;
    fPos: integer;
    fTags: string; // a separated string
    fSeparator: string;
    procedure SetTags(const s: string);
    procedure SetCurrent(const s: string);
  public
    constructor Create(const tags: string = ''; const asep: string = '|');
    destructor Destroy; override;
    function Found: boolean;
    procedure Reset;
    procedure Stop; overload;
    function Stop(aReturn:Variant):Variant; overload;
  // properties
    property Count: integer read fCount;
    property Current: string read fCurrent;
    property Pattern: TStringPattern read fPattern;
    property Position: integer read fPos;
    property Separator: string read fSeparator write fSeparator;
    property Tags: string read fTags write SetTags;
  end;

{ TCSVLoop loops through a comma-separated value (CSV) list }
type
  TCSVLoop = class(TStringLoop)
  protected
    fCSV: TStringList;
    function GetLine(const l: integer): string;
    procedure SetLine(const l: integer; const v: string);
    procedure SetCurrent(const s: string); override;
    function GetValue(const s: string): string;
    procedure SetValue(const s: string; const v: string);
  public
    constructor Create(const sl: tstrings = nil); override;
    destructor Destroy; override;
    procedure Reset; override;
  // properties
    property Lines[const l: integer]: string read GetLine write SetLine;
    property Values[const s: string]: string read GetValue
      write SetValue; default;
  end;

implementation

uses CatStrings;

function TStringLoop.GetCount: integer;
begin
  result := fList.Count;
end;

procedure TStringLoop.Delete;
begin
  fList.Delete(FPosition - 1);
end;

procedure TStringLoop.Stop;
begin
  FPosition := fList.Count;
end;

function TStringLoop.Stop(aReturn:Variant):Variant;
begin
  Result := aReturn;
  Stop;
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

// Replaces the content of the current line with a new string
procedure TStringLoop.SetCurrentLine(const s: string);
begin
  SetCurrent(s);
  fList[FPosition - 1] := s;
end;

procedure TStringLoop.SetCurrent(const s: string);
begin
  FCurrent := s;
  FPattern.Text := s;
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
    SetCurrent(fList[i]);
    FPosition := FPosition + 1;
  end;
end;

procedure TStringLoop.Reset;
begin
  FPosition := 0;
  SetCurrent(emptystr);
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
  fList := tstringlist.Create;
  fPattern := TStringPattern.Create;
  fPattern.AllowLock := false;
  if sl <> nil then
    Load(sl);
  Reset;
end;

constructor TStringLoop.Create(const list:string);
begin
  Create;
  LoadFromString(list);
end;

destructor TStringLoop.Destroy;
begin
  fPattern.Free;
  fList.free;
  inherited;
end;

{------------------------------------------------------------------------------}

{TCSVLoop}

procedure TCSVLoop.Reset;
begin
  inherited Reset;
  fcsv.Clear;
end;

procedure TCSVLoop.SetCurrent(const s: string);
begin
  inherited SetCurrent(s);
  fcsv.commatext := FCurrent;
end;

function TCSVLoop.GetLine(const l: integer): string;
begin
  try
    result := fcsv[l];
  except
  end;
end;

procedure TCSVLoop.SetLine(const l: integer; const v: string);
begin
  try
    fcsv[l] := v;
  except
  end;
  SetCurrentLine(fcsv.commatext);
end;

function TCSVLoop.GetValue(const s: string): string;
begin
  result := fcsv.Values[s];
end;

procedure TCSVLoop.SetValue(const s, v: string);
begin
  fcsv.Values[s] := v;
  SetCurrentLine(fcsv.commatext);
end;

constructor TCSVLoop.Create(const sl: tstrings = nil);
begin
  fcsv := tstringlist.Create;
  inherited Create(sl);
end;

destructor TCSVLoop.Destroy;
begin
  inherited;
  fcsv.Free;
end;

{------------------------------------------------------------------------------}

{TSepStringLoop}

function TSepStringLoop.Found: boolean;
begin
  result := false;
  if fTags = emptystr then
    exit;
  fCount := occurs(fSeparator, fTags) + 1;
  if fPos < fCount then
  begin
    fPos := fPos + 1;
    SetCurrent(gettoken(fTags, fSeparator, fPos));
    result := true;
  end;
end;

procedure TSepStringLoop.SetTags(const s: string);
begin
  fTags := s;
  Reset;
end;

procedure TSepStringLoop.Reset;
begin
  SetCurrent(emptystr);
  fPos := 0;
end;

procedure TSepStringLoop.SetCurrent(const s: string);
begin
  fCurrent := s;
  fPattern.Text := s;
end;

procedure TSepStringLoop.Stop;
begin
  fPos := fCount;
end;

function TSepStringLoop.Stop(aReturn:Variant):Variant;
begin
  Result := aReturn;
  Stop;
end;

constructor TSepStringLoop.Create(const tags: string = '';
  const asep: string = '|');
begin
  fPattern := TStringPattern.Create;
  fPattern.AllowLock := false;
  SetTags(tags);
  fSeparator := asep;
end;

destructor TSepStringLoop.Destroy;
begin
  fPattern.Free;
  inherited;
end;

end.
