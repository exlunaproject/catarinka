unit CatPatterns;

{
  Catarinka Pattern Validation for Pascal/Delphi
  TStringPattern and TIntegerPattern
  Copyright (c) 2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  A flexible string and integer validation engine for Pascal/Delphi with
  support for case sensitive or insensitive reusable chained validations.
  Currently over 30 chainable checks and custom checks are supported.
  Compiles under Delphi XE down to Delphi 7, both 32-bit and 64-bit.

  The Match() function ignites the validation and returns true if all checks
  passed, but if a check fails, it breaks the chain (not really executing the
  rest) and returns false.

  It is possible to determine the check that failed using the Index property -
  if you have, for example, three chained checks and the second one failed,
  the Index value will be 2.

  This project is inspired by Sailor's Valua (Validation for Lua) and Respect
  Validation for PHP. Be sure to check them later at:
  https://github.com/sailorproject/valua
  https://github.com/Respect/Validation

  Usage Examples:

  // chain validation way:
  v := TStringPattern.Create;
  v.begins(['http:','https:']).ends('.com').match('http://someurl.com'); // true
  memo1.lines.add(v.value + '=' +v.resultasstr);

  // match using [] as shorthand to Match()
  if v.wild('Ca*ka').len(9)['Catarinka'] then // true
  if v.contains('some').len(10)['somestring'] then // true

  // determine where it has failed with the Index property
  if v.int.len(4)['301'] = false then begin
  case v.index of
  1: showmessage('must be a number!');
  2: showmessage('must have four digits!');
  end;
  end;

  // create with a single line and use Lock to make it reusable
  v := TStringPattern.Create.Begins('http://').Ends('.com').Lock;
  if v['http://someurl.com'] then
  showmessage('valid!'); // true
  if v['http://someurl.org'] then // false
  (...)
  if v['https://someurl.com'] then // false
  (...)
  v.free;

  // make it case insensitive just by calling MatchI(), instead of Match():
  v.Contains('some').Len(10).MatchI('SOMESTRING') // true
  v.Contains('some').Len(10).Match('SOMESTRING') // false

  // match against string lists
  if v.InList(listbox1.Items)['somestring'] then
  showmessage('true!');

  // match against an integer
  vi := TIntegerPattern.Create;
  if vi.positive.between(1, 255)[input] then
  (...)

  // use your own custom methods
  const
  VALCUST_ISCREDITCARD = 1;
  VALCUST_SOMETHINGELSE = 2;
  v := TStringPattern.Create;
  v.oncustommethod := yourcustommethod;
  if v.len(50).contains('str').custom(VALCUST_ISCREDITCARD)['somestring'] then
  (...)

  // and you can still use it the standard Pascal way if you prefer
  s := 'SOMESTRING';
  v := TStringPattern.Create;
  v.Contains('some');
  v.Len(10);
  if v.MatchI(s) then
  showmessage('matched!');
  v.free;
}

interface

{$I Catarinka.inc}

uses
{$IF CompilerVersion > 22}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$IFEND}

type
  TOnCustomValidationMethod = procedure(var aResult: boolean;
    const tag: integer; const value: string) of object;
  TOnCustomValidationMethodInt = procedure(var aResult: boolean;
    const tag, value: integer) of object;

type
  TValidationMethod = record
    ID: integer;
    Param_Str, Param_Str2: string;
    Param_Int, Param_Int2: integer;
    Param_CharSet: TSysCharSet;
    Param_StrArray: array of string;
    Param_IntArray: array of integer;
  end;

type
  TStringPattern = class
  private
    fAllowLock: boolean;
    fCustomMethod: TOnCustomValidationMethod;
    fFuncs: array of TValidationMethod;
    fFuncsTemp: array of TValidationMethod;
    fIgnoreCase: boolean;
    fLastIndex: integer;
    fLastMethod: TValidationMethod;
    fLocked: boolean;
    fMatched: boolean;
    fText : string;
    fValue: string;
    function AddCheck(const ID: integer; f: TValidationMethod): TStringPattern;
    function AddCheckS(const ID: integer; const param1: string = '')
      : TStringPattern;
    function InList_StringList(const value, listtext: string): boolean;
    function Contains_AnyOfStringList(const value, listtext: string): boolean;
    function FMatch(value: string): boolean;
    function GetLastMethod: TValidationMethod;
    function GetResultAsString: string;
    procedure MethodParamsToLower;
    procedure Reset;
  public
    // validation functions
    function Alpha: TStringPattern;
    function AlphaNum: TStringPattern;
    function Empty: TStringPattern;
    function Hex: TStringPattern;
    function Int: TStringPattern;
    function Upper: TStringPattern;
    function Lower: TStringPattern;
    function Begins(const s: string): TStringPattern; overload;
    function Begins(const aArray: array of string): TStringPattern; overload;
    function Contains(const s: string): TStringPattern;
    function ContainsAllOf(const aArray: array of string): TStringPattern;
    function ContainsAnyOf(const aArray: array of string)
      : TStringPattern; overload;
    function ContainsAnyOf(const sl: TStrings): TStringPattern; overload;
    function ContainsAnyOfChars(const aSet: TSysCharSet): TStringPattern;
    function Custom(const tag: integer): TStringPattern;
    function Date(const format: string = 'mm/dd/yyyy'): TStringPattern;
    function Email: TStringPattern;
    function Equals(const s: string): TStringPattern;
    function Ends(const s: string): TStringPattern; overload;
    function Ends(const aArray: array of string): TStringPattern; overload;
    function InList(const aArray: array of string): TStringPattern; overload;
    function InList(const sl: TStrings): TStringPattern; overload;
    function Len(const l: integer): TStringPattern; overload;
    function Len(const min, max: integer): TStringPattern; overload;
    function Occurs(const sub: string; const count: integer): TStringPattern;
    function Wild(const mask: string): TStringPattern;
    function NotContains(const s: string): TStringPattern;
    function NotEquals(const s: string): TStringPattern;
    function NotEmpty: TStringPattern;
    function NoWhite: TStringPattern;
    function RegEx(const re: string): TStringPattern;
    function Roman: TStringPattern;
    // main functions
    function Lock: TStringPattern;
    function Match: boolean; overload; // case sensitive
    function Match(value: string): boolean; overload; // case sensitive
    function MatchI: boolean; overload; // case insensitive
    function MatchI(value: string): boolean; overload; // case insensitive
    constructor Create(const aText:string='');
    destructor Destroy; override;
    // properties
    property AllowLock: boolean read fAllowLock write fAllowLock;
    property LastMethod: TValidationMethod read GetLastMethod;
    property OnCustomMethod: TOnCustomValidationMethod read fCustomMethod
      write fCustomMethod;
    property Index: integer read fLastIndex;
    property Result: boolean read fMatched;
    property ResultAsStr: string read GetResultAsString;
    // Value returns the latest string validated through Match functions
    property Value: string read fValue;
    // Text allows to set a string to be validated through Match or MatchI when
    // called with no arguments
    property Text:string read fText write fText;
    // Match() shorthand
    property _MatchShortHand[s: string]: boolean read FMatch; default;
  end;

type
  TIntegerPattern = class
  private
    fAllowLock: boolean;
    fCustomMethod: TOnCustomValidationMethodInt;
    fFuncs: array of TValidationMethod;
    fLastIndex: integer;
    fLastMethod: TValidationMethod;
    fLocked: boolean;
    fMatched: boolean;
    fValue: integer;
    function AddCheck(const ID: integer; f: TValidationMethod): TIntegerPattern;
    function AddCheckI(const ID: integer; const param1: integer = 0)
      : TIntegerPattern;
    function GetLastMethod: TValidationMethod;
    function GetResultAsString: string;
    procedure Reset;
  public
    // validation functions
    function Between(const amin, amax: integer): TIntegerPattern;
    function Custom(const tag: integer): TIntegerPattern;
    function Equals(const i: integer): TIntegerPattern;
    function Even: TIntegerPattern;
    function InList(const aArray: array of integer): TIntegerPattern;
    function Len(const l: integer): TIntegerPattern;
    function Odd: TIntegerPattern;
    function max(const i: integer): TIntegerPattern;
    function min(const i: integer): TIntegerPattern;
    function Negative: TIntegerPattern;
    function NotEqual(const i: integer): TIntegerPattern;
    function Positive: TIntegerPattern;
    function Prime: TIntegerPattern;
    // main functions
    function Match(const value: integer): boolean;
    function Lock: TIntegerPattern;
    constructor Create;
    destructor Destroy; override;
    // properties
    property AllowLock: boolean read fAllowLock write fAllowLock;
    property Index: integer read fLastIndex;
    property LastMethod: TValidationMethod read GetLastMethod;
    property OnCustomMethod: TOnCustomValidationMethodInt read fCustomMethod
      write fCustomMethod;
    property Result: boolean read fMatched;
    property ResultAsStr: string read GetResultAsString;
    property value: integer read fValue;
    // Match() shorthand
    property _MatchShortHand[const i: integer]: boolean read Match; default;
  end;

implementation

uses CatStrings, CatStringLoop, CatMatch, CatTime;

const
  VAL_ISEMPTY = 0;
  VAL_ISEMPTY_NOT = 1;
  VAL_ISINTEGER = 2;
  VAL_EQUAL = 3;
  VAL_EQUAL_NOT = 4;
  VAL_BEGINSWITH = 5;
  VAL_ENDSWITH = 6;
  VAL_INLIST_STRINGLIST = 7;
  VAL_INLIST_ARRAY = 8;
  VAL_MATCH_WILD = 9;
  VAL_HASLENGTH = 10;
  VAL_HASLENGTH_BETWEEN = 11;
  VAL_CONTAINS = 12;
  VAL_CONTAINS_NOT = 13;
  VAL_CONTAINS_ANYOFCHARS = 14;
  VAL_CONTAINS_ANYOFSTRINGS = 15;
  VAL_ISUPPER = 16;
  VAL_ISLOWER = 17;
  VAL_ISALPHANUM = 18;
  VAL_NOWHITESPACE = 19;
  VAL_ISDATE = 20;
  VAL_ISEMAIL = 21;
  VAL_ISHEX = 22;
  VAL_ISROMAN = 23;
  VAL_ISALPHA = 24;
  VAL_OCCURS = 25;
  VAL_CONTAINS_ALLOFSTRINGS = 26;
  VAL_CONTAINS_ANYOFSTRINGLIST = 27;
  VAL_BEGINSWITH_ARRAY = 28;
  VAL_ENDSWITH_ARRAY = 29;
  VAL_REGEX = 30;
  VAL_CUSTOMMETHOD = 31;

const
  VALINT_ISPOSITIVE = 0;
  VALINT_ISNEGATIVE = 1;
  VALINT_ISBETWEEN = 2;
  VALINT_ISEVEN = 3;
  VALINT_ISODD = 4;
  VALINT_EQUAL = 5;
  VALINT_EQUAL_NOT = 6;
  VALINT_ISPRIMENUMBER = 7;
  VALINT_INLIST_ARRAY = 8;
  VALINT_ISMIN = 9;
  VALINT_ISMAX = 10;
  VALINT_HASLENGTH = 11;
  VALINT_HASLENGTH_BETWEEN = 12;
  VALINT_CUSTOMMETHOD = 13;

  // Turns a TSysCharSet like ['A','Z'] into ['a','z']
function Lowercase_CharSet(Chars: TSysCharSet): TSysCharSet;
const
  u = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  i: integer;
begin
  for i := 0 to length(u) do
    if u[i] in Chars then
    begin
      Chars := Chars - [u[i]];
      Chars := Chars + [char(ord(u[i]) + 32)];
    end;
  Result := Chars;
end;

{ TStringPattern }

function TStringPattern.AddCheck(const ID: integer; f: TValidationMethod)
  : TStringPattern;
begin
  if fLocked = true then
    raise Exception.Create('Validation not reusable!');
  SetLength(fFuncs, length(fFuncs) + 1);
  f.ID := ID;
  fFuncs[High(fFuncs)] := f;
  Result := self;
end;

function TStringPattern.AddCheckS(const ID: integer; const param1: string = '')
  : TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_Str := param1;
  Result := AddCheck(ID, f);
end;

function TStringPattern.GetLastMethod: TValidationMethod;
begin
  Result := fLastMethod;
end;

function TStringPattern.GetResultAsString: string;
begin
  Result := BoolToStr(fMatched);
end;

function TStringPattern.Custom(const tag: integer): TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_Int := tag;
  Result := AddCheck(VAL_CUSTOMMETHOD, f);
end;

function TStringPattern.Empty: TStringPattern;
begin
  Result := AddCheckS(VAL_ISEMPTY);
end;

function TStringPattern.Hex: TStringPattern;
begin
  Result := AddCheckS(VAL_ISHEX);
end;

function TStringPattern.NotEmpty: TStringPattern;
begin
  Result := AddCheckS(VAL_ISEMPTY_NOT);
end;

function TStringPattern.Int: TStringPattern;
begin
  Result := AddCheckS(VAL_ISINTEGER);
end;

function TStringPattern.Upper: TStringPattern;
begin
  Result := AddCheckS(VAL_ISUPPER);
end;

function TStringPattern.Lower: TStringPattern;
begin
  Result := AddCheckS(VAL_ISLOWER);
end;

function TStringPattern.AlphaNum: TStringPattern;
begin
  Result := AddCheckS(VAL_ISALPHANUM);
end;

function TStringPattern.Alpha: TStringPattern;
begin
  Result := AddCheckS(VAL_ISALPHA);
end;

function TStringPattern.Begins(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_BEGINSWITH, s);
end;

function TStringPattern.ContainsAnyOfChars(const aSet: TSysCharSet)
  : TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_CharSet := aSet;
  Result := AddCheck(VAL_CONTAINS_ANYOFCHARS, f);
end;

function TStringPattern.Begins(const aArray: array of string): TStringPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_StrArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_StrArray[Low(f.Param_StrArray)],
    SizeOf(aArray));
  Result := AddCheck(VAL_BEGINSWITH_ARRAY, f);
end;

function TStringPattern.Ends(const aArray: array of string): TStringPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_StrArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_StrArray[Low(f.Param_StrArray)],
    SizeOf(aArray));
  Result := AddCheck(VAL_ENDSWITH_ARRAY, f);
end;

function TStringPattern.ContainsAnyOf(const aArray: array of string)
  : TStringPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_StrArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_StrArray[Low(f.Param_StrArray)],
    SizeOf(aArray));
  Result := AddCheck(VAL_CONTAINS_ANYOFSTRINGS, f);
end;

function TStringPattern.ContainsAllOf(const aArray: array of string)
  : TStringPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_StrArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_StrArray[Low(f.Param_StrArray)],
    SizeOf(aArray));
  Result := AddCheck(VAL_CONTAINS_ALLOFSTRINGS, f);
end;

function TStringPattern.InList(const aArray: array of string): TStringPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_StrArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_StrArray[Low(f.Param_StrArray)],
    SizeOf(aArray));
  Result := AddCheck(VAL_INLIST_ARRAY, f);
end;

function TStringPattern.InList(const sl: TStrings): TStringPattern;
begin
  Result := AddCheckS(VAL_INLIST_STRINGLIST, sl.Text);
end;

function TStringPattern.ContainsAnyOf(const sl: TStrings): TStringPattern;
begin
  Result := AddCheckS(VAL_CONTAINS_ANYOFSTRINGLIST, sl.Text);
end;

function TStringPattern.Contains(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_CONTAINS, s);
end;

function TStringPattern.NotContains(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_CONTAINS_NOT, s);
end;

function TStringPattern.Date(const format: string = 'mm/dd/yyyy')
  : TStringPattern;
begin
  Result := AddCheckS(VAL_ISDATE, format);
end;

function TStringPattern.Wild(const mask: string): TStringPattern;
begin
  Result := AddCheckS(VAL_MATCH_WILD, mask);
end;

function TStringPattern.Equals(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_EQUAL, s);
end;

function TStringPattern.NotEquals(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_EQUAL_NOT, s);
end;

function TStringPattern.NoWhite: TStringPattern;
begin
  Result := AddCheckS(VAL_NOWHITESPACE);
end;

function TStringPattern.Len(const l: integer): TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_Int := l;
  Result := AddCheck(VAL_HASLENGTH, f);
end;

function TStringPattern.Len(const min, max: integer): TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_Int := min;
  f.Param_Int2 := max;
  Result := AddCheck(VAL_HASLENGTH_BETWEEN, f);
end;

function TStringPattern.Occurs(const sub: string; const count: integer)
  : TStringPattern;
var
  f: TValidationMethod;
begin
  f.Param_Str := sub;
  f.Param_Int := count;
  Result := AddCheck(VAL_OCCURS, f);
end;

function TStringPattern.Email: TStringPattern;
begin
  Result := AddCheckS(VAL_ISEMAIL);
end;

function TStringPattern.Ends(const s: string): TStringPattern;
begin
  Result := AddCheckS(VAL_ENDSWITH, s);
end;

function TStringPattern.Lock: TStringPattern;
begin
  if fAllowLock then
    fLocked := true
  else
    raise Exception.Create('Lock not allowed for this object.');
  Result := self;
end;

function TStringPattern.RegEx(const re: string): TStringPattern;
begin
  Result := AddCheckS(VAL_REGEX, re);
end;

function TStringPattern.Roman: TStringPattern;
begin
  Result := AddCheckS(VAL_ISROMAN);
end;

procedure TStringPattern.Reset;
begin
  fIgnoreCase := false;
  SetLength(fFuncs, 0);
end;

constructor TStringPattern.Create(const aText:string='');
begin
  inherited Create;
  fMatched := true;
  fValue := emptystr;
  fLocked := false;
  fAllowLock := true;
  fText := aText;
  Reset;
end;

destructor TStringPattern.Destroy;
begin
  inherited;
end;

procedure TStringPattern.MethodParamsToLower;
var
  i, ip: integer;
begin
  for i := 0 to high(fFuncsTemp) do
  begin
    with fFuncsTemp[i] do
    begin
      Param_Str := lowercase(Param_Str);
      Param_Str2 := lowercase(Param_Str2);
      Param_CharSet := Lowercase_CharSet(Param_CharSet);
      for ip := 0 to high(Param_StrArray) do
        Param_StrArray[ip] := lowercase(Param_StrArray[ip]);
    end;
  end;
end;

function TStringPattern.InList_StringList(const value,
  listtext: string): boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := listtext;
  Result := sl.IndexOf(value) <> -1;
  sl.free;
end;

function TStringPattern.Contains_AnyOfStringList(const value,
  listtext: string): boolean;
var
  slp: TStringLoop;
begin
  Result := false;
  slp := TStringLoop.Create(listtext);
  while slp.Found do
    if pos(slp.current, value) <> 0 then
      Result := slp.Stop(true);
  slp.free;
end;

function TStringPattern.Match(value: string): boolean;
var
  f: TValidationMethod;
  i: integer;
begin
  Result := true;
  fLastMethod := f;
  fLastIndex := 0;
  fValue := value;

  // copy the methods array to a temporary one that we can modify if needed
  SetLength(fFuncsTemp, 0);
  for i := 0 to high(fFuncs) do
  begin
    SetLength(fFuncsTemp, length(fFuncsTemp) + 1);
    fFuncsTemp[i] := fFuncs[i];
  end;

  if fIgnoreCase = true then
  begin
    value := lowercase(value);
    MethodParamsToLower;
  end;
  for i := 0 to high(fFuncsTemp) do
  begin
    f := fFuncsTemp[i];
    fLastMethod := f;
    fLastIndex := i + 1;
    case f.ID of
      VAL_ISEMPTY:
        Result := (value = emptystr);
      VAL_ISEMPTY_NOT:
        Result := not(value = emptystr);
      VAL_ISHEX:
        Result := ishexstr(value);
      VAL_ISINTEGER:
        Result := CatStrings.IsInteger(value);
      VAL_ISUPPER:
        Result := isuppercase(value);
      VAL_ISLOWER:
        Result := islowercase(value);
      VAL_ISALPHA:
        Result := IsAlpha(value);
      VAL_ISALPHANUM:
        Result := isalphanumeric(value);
      VAL_ISDATE:
        Result := isvaliddate(value, f.Param_Str);
      VAL_CUSTOMMETHOD:
        if Assigned(fCustomMethod) then
          fCustomMethod(Result, f.Param_Int, value);
      VAL_EQUAL:
        Result := (value = f.Param_Str);
      VAL_EQUAL_NOT:
        Result := not(value = f.Param_Str);
      VAL_BEGINSWITH:
        Result := CatStrings.BeginsWith(value, f.Param_Str);
      VAL_ENDSWITH:
        Result := CatStrings.EndsWith(value, f.Param_Str);
      VAL_BEGINSWITH_ARRAY:
        Result := CatStrings.BeginsWith(value, f.Param_StrArray);
      VAL_ENDSWITH_ARRAY:
        Result := CatStrings.EndsWith(value, f.Param_StrArray);
      VAL_CONTAINS_ANYOFCHARS:
        Result := CatStrings.ContainsAnyOfChars(value, f.Param_CharSet);
      VAL_CONTAINS_ANYOFSTRINGS:
        Result := CatStrings.ContainsAnyofStrings(value, f.Param_StrArray);
      VAL_CONTAINS_ANYOFSTRINGLIST:
        Result := Contains_AnyOfStringList(value, f.Param_Str);
      VAL_CONTAINS_ALLOFSTRINGS:
        Result := CatStrings.ContainsAllofStrings(value, f.Param_StrArray);
      VAL_INLIST_ARRAY:
        Result := CatStrings.MatchStrInArray(value, f.Param_StrArray);
      VAL_INLIST_STRINGLIST:
        Result := InList_StringList(value, f.Param_Str);
      VAL_MATCH_WILD:
        Result := MatchWildcard(value, f.Param_Str);
      VAL_HASLENGTH:
        Result := length(value) = f.Param_Int;
      VAL_HASLENGTH_BETWEEN:
        Result := (length(value) >= f.Param_Int) and
          (length(value) <= f.Param_Int2);
      VAL_CONTAINS:
        Result := (pos(f.Param_Str, value) <> 0);
      VAL_CONTAINS_NOT:
        Result := (pos(f.Param_Str, value) = 0);
      VAL_REGEX:
        Result := (RegExpFind(value, f.Param_Str) <> emptystr);
      VAL_ISROMAN:
        Result := isroman(value);
      VAL_NOWHITESPACE:
        Result := not CatStrings.ContainsAnyofStrings(value, [' ', #9, crlf]);
      VAL_ISEMAIL:
        Result := IsValidEmail(value);
      VAL_OCCURS:
        Result := (CatStrings.Occurs(f.Param_Str, value) = f.Param_Int);
    end;
    if Result = false then
      break;
  end;
  if high(fFuncsTemp) = -1 then
    raise Exception.Create('No validation method defined.');
  if fLocked = false then
    Reset;

  fMatched := Result;
end;

function TStringPattern.Match: boolean;
begin
  result := Match(fText);
end;

function TStringPattern.MatchI(value: string): boolean;
begin
  fIgnoreCase := true;
  Result := Match(value);
  fIgnoreCase := false;
end;

function TStringPattern.MatchI: boolean;
begin
  result := MatchI(fText);
end;

function TStringPattern.FMatch(value: string): boolean;
begin
  result := Match(value);
end;

// ------------------------------------------------------------------------//

function IsPrimeNumber(const i: integer): boolean;
var
  fi: integer;
begin
  Result := true;
  for fi := 2 to i - 1 do
    if i mod fi = 0 then
    begin
      Result := false;
      break;
    end;
end;

{ TIntegerPattern }

function TIntegerPattern.Match(const value: integer): boolean;
var
  i: integer;
  f: TValidationMethod;
  function Odd(const i: integer): boolean;
  begin
    Result := System.Odd(i);
  end;

begin
  Result := true;
  fValue := value;
  fLastMethod := f;
  fLastIndex := 0;
  for i := 0 to high(fFuncs) do
  begin
    f := fFuncs[i];
    fLastMethod := f;
    fLastIndex := i + 1;
    case f.ID of
      VALINT_ISPOSITIVE:
        Result := (value > -1);
      VALINT_ISNEGATIVE:
        Result := (value < 0);
      VALINT_ISBETWEEN:
        Result := (value >= f.Param_Int) and (value <= f.Param_Int2);
      VALINT_ISEVEN:
        Result := not Odd(value);
      VALINT_ISODD:
        Result := Odd(value);
      VALINT_EQUAL:
        Result := (value = f.Param_Int);
      VALINT_EQUAL_NOT:
        Result := not(value = f.Param_Int);
      VALINT_ISPRIMENUMBER:
        Result := IsPrimeNumber(value);
      VALINT_ISMIN:
        Result := (value >= f.Param_Int);
      VALINT_ISMAX:
        Result := (value <= f.Param_Int);
      VALINT_HASLENGTH:
        Result := length(IntToStr(value)) = f.Param_Int;
      VALINT_CUSTOMMETHOD:
        if Assigned(fCustomMethod) then
          fCustomMethod(Result, f.Param_Int, value);
    end;
    if Result = false then
      break;
  end;
  if high(fFuncs) = -1 then
    raise Exception.Create('No validation method defined.');
  if fLocked = false then
    Reset;

  fMatched := Result;
end;

function TIntegerPattern.AddCheck(const ID: integer; f: TValidationMethod)
  : TIntegerPattern;
begin
  if fLocked = true then
    raise Exception.Create('Validation not reusable!');
  SetLength(fFuncs, length(fFuncs) + 1);
  f.ID := ID;
  fFuncs[High(fFuncs)] := f;
  Result := self;
end;

function TIntegerPattern.AddCheckI(const ID: integer; const param1: integer = 0)
  : TIntegerPattern;
var
  f: TValidationMethod;
begin
  f.Param_Int := param1;
  Result := AddCheck(ID, f);
end;

function TIntegerPattern.GetLastMethod: TValidationMethod;
begin
  Result := fLastMethod;
end;

function TIntegerPattern.GetResultAsString: string;
begin
  Result := BoolToStr(fMatched);
end;

function TIntegerPattern.Custom(const tag: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_CUSTOMMETHOD, tag);
end;

function TIntegerPattern.Equals(const i: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_EQUAL, i);
end;

function TIntegerPattern.Even: TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISEVEN);
end;

function TIntegerPattern.Len(const l: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_HASLENGTH, l);
end;

function TIntegerPattern.InList(const aArray: array of integer)
  : TIntegerPattern;
var
  f: TValidationMethod;
begin
  SetLength(f.Param_IntArray, length(aArray));
  Move(aArray[Low(aArray)], f.Param_IntArray[Low(f.Param_IntArray)],
    SizeOf(aArray));
  Result := AddCheck(VALINT_INLIST_ARRAY, f);
end;

function TIntegerPattern.max(const i: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISMAX, i);
end;

function TIntegerPattern.min(const i: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISMIN, i);
end;

function TIntegerPattern.Negative: TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISNEGATIVE);
end;

function TIntegerPattern.NotEqual(const i: integer): TIntegerPattern;
begin
  Result := AddCheckI(VALINT_EQUAL_NOT, i);
end;

function TIntegerPattern.Odd: TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISODD);
end;

function TIntegerPattern.Positive: TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISPOSITIVE);
end;

function TIntegerPattern.Prime: TIntegerPattern;
begin
  Result := AddCheckI(VALINT_ISPRIMENUMBER);
end;

function TIntegerPattern.Between(const amin, amax: integer): TIntegerPattern;
var
  f: TValidationMethod;
begin
  f.Param_Int := amin;
  f.Param_Int2 := amax;
  Result := AddCheck(VALINT_ISBETWEEN, f);
end;

function TIntegerPattern.Lock: TIntegerPattern;
begin
  if fAllowLock then
    fLocked := true
  else
    raise Exception.Create('Lock not allowed for this object.');
  Result := self;
end;

procedure TIntegerPattern.Reset;
begin
  SetLength(fFuncs, 0);
end;

constructor TIntegerPattern.Create;
begin
  inherited;
  fMatched := true;
  fValue := 0;
  fLocked := false;
  fAllowLock := true;
  Reset;
end;

destructor TIntegerPattern.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------//

end.
