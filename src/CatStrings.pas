unit CatStrings;
{
  Catarinka - String Operation functions

  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Base64 encode and decode functions by Lukas Gebauer (BSD license, included below)
  MD5 function by Stijn Sanders (MIT license, included at the end of this file)
  IScan, SplitString, GetTextBetweenTags functions by Peter Below
  MatchStrings and PosX functions by Arsne von Wyss

  Note: random functions included with this library are not suitable
  for cryptographic purposes.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.StrUtils, System.AnsiStrings;
{$ELSE}
  Classes, SysUtils, StrUtils;
{$ENDIF}

type
  TCatCaseLabel = record
    name: string;
    id: integer;
  end;

function After(const s, substr: string): string;
function ASCIIToInt(const s: string): integer;
function Base64Encode(const s: string): string;
function Base64Decode(const s: string): string;
function Before(const s, substr: string): string;
function BeginsWith(const s, prefix: string): Boolean;
function BoolToStr(const b: Boolean): string;
function BoolToYN(const b: Boolean): string;
function CatCaseOf(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer;
function CatWrapText(const text: string; const chars: integer): TStringList;
function CommaTextToStr(const s: string): string;
function ContainsAnyOfChars(const s: string; const aSet: TSysCharSet): Boolean;
function ContainsAnyOfStrings(s:string;aArray:array of string; IgnoreCase: Boolean = false):boolean;
function EndsWith(const s, prefix: string): Boolean;
function ExtractFromString(const s, startstr, endstr: string): string;
function ExtractFromTag(const s, tag: string): string;
function GetLineByPos(const s: string; const Position: integer): integer;
function GetToken(const aString, SepChar: string; const TokenNum: Byte): string;
function GetValidCompName(const s: string): string;
function HexToInt(const Hex: string; const WarnError: Boolean = false): integer;
function HexToStr(const s: string): string;
function Hex16ToStr(const s: string): string;
function IIF(const Cond: Boolean; const TrueStr: String;
  const FalseStr: string = ''): string; overload;
function IIF(const Cond: Boolean; const TrueInt: integer;
  const FalseInt: integer = 0): integer; overload;
function IScan(ch: Char; const s: string; fromPos: integer): integer;
function IsHexStr(const s: string): Boolean;
function IsInteger(const s: string): Boolean;
function LastChar(const s: string): Char;
function LeftPad(const s: string; const c: Char; const len: integer): string;
function LeftStr(s: string; c: longword): string;
function MatchStrInArray(s:string;aArray:array of string; IgnoreCase: Boolean = false):boolean;
function MatchStrings(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
function MD5Hash(s: UTF8String): UTF8String;
function Occurs(substr, s: string): integer;
function RandomCase(const S: string; const ToUpperSet: TSysCharSet=['a'..'z'];
  const ToLowerSet: TSysCharSet=['A'..'Z']): string;
function RandomString(const len: integer;
  const chars: string = 'abcdefghijklmnopqrstuvwxyz'): string;
function RemoveLastChar(const s: string): string;
function RemoveNumbers(const s: string): string;
function RemoveQuotes(const s: string): string;
function RemoveShortcuts(const s: string): string;
function RepeatString(const s: string; count: cardinal): string;
function ReplaceChars(const s: string; const aSet: TSysCharSet;
  const repwith: Char = '_'): string;
function ReplaceStr(const s, substr, repstr: string): string;
function RestStr(const s: string; const index: longword): string;
function RightPad(const s: string; const c: Char; const len: integer): string;
function StrDecrease(const s: string; const step: integer = 1): string;
function StrIncrease(const s: string; const step: integer = 1): string;
function StripChars(const s: string; const aSet: TSysCharSet): string;
function StrMaxLen(const s: string; const MaxLen: integer;
  const AddEllipsis: Boolean = false): string;
function StrToAlphaNum(const s: string): string;
function StrToBool(const s: string): Boolean;
function StrToCommaText(const s: string): string;
function StrToHex(const s: string): string;
function StrToHex16(const s: string): string;
function StrToIntSafe(const s: string; const FalseInt: integer = 0): integer;
function StrToPWideChar(const s: string): PWideChar;
function TitleCase(const s: string): string;
procedure GetTextBetweenTags(const s, tag1, tag2: string; const list: TStrings;
  const includetags: Boolean = false);
procedure MergeStrings(const dest, source: TStrings);
procedure SplitString(const s: string; separator: Char;
  substrings: TStringList);
procedure StripBlankLines(const sl: TStringList);

{$IFDEF CHARINSET_UNAVAILABLE}
function CharInSet(c: Char; CharSet: TSysCharSet): Boolean;
{$ENDIF}

const
  CRLF = #13 + #10;

implementation

{$IFDEF CHARINSET_UNAVAILABLE}

// Before D2009
function CharInSet(c: Char; CharSet: TSysCharSet): Boolean;
begin
  if c in CharSet then
    result := true
  else
    result := false;
end;
{$ENDIF}

function After(const s, substr: string): string;
var
  i: Byte;
begin
  i := pos(substr, s);
  if i = 0 then
    result := emptystr
  else
    result := Copy(s, i + length(substr), length(s));
end;

function ASCIIToInt(const s: string): integer;
var
  i, len: integer;
  c: Char;
begin
  result := 0;
  len := length(s);
  for i := len downto 1 do
  begin
    c := s[i];
    result := result + ord(c) shl ((len - i) shl 8);
  end;
end;

function Before(const s, substr: string): string;
var
  i: Byte;
begin
  i := pos(substr, s);
  if i = 0 then
    result := s
  else
    result := Copy(s, 1, i - 1);
end;

function BeginsWith(const s, prefix: string): Boolean;
var
  tmpstr: string;
begin
  tmpstr := s;
{$IFDEF DXE2_OR_UP}
  SetLength(tmpstr, System.AnsiStrings.StrLen
    (System.PAnsiChar(AnsiString(prefix))));
{$ELSE}
  SetLength(tmpstr, StrLen(PAnsiChar(prefix)));
{$ENDIF}
  result := AnsiCompareText(tmpstr, prefix) = 0;
end;

function BoolToStr(const b: Boolean): string;
begin
  if b = true then
    result := 'True'
  else
    result := 'False';
end;

function BoolToYN(const b: Boolean): string;
begin
  if b = true then
    result := 'Yes'
  else
    result := 'No';
end;

{
  A CaseOf function using arrays
  Usage Example:

procedure TForm1.Button1Click(Sender: TObject);
const
 ana = 1;
 roberto = 2;
 lucia = 3;
const
 labels : array [1..3] of TCatCaseLabel =
 (
 (name:'ana';id:ana),
 (name:'roberto';id:roberto),
 (name:'lucia';id:lucia)
 );
begin
  case CatCaseOf(edit1.text,labels) of
    ana: form1.caption:='ana!';
    roberto: form1.caption:='roberto!';
    lucia: form1.caption:='lucia!';
  else
    form1.Caption:=edit1.text+' not in case list!';
  end;
end;

}
function CatCaseOf(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer;
var
  i: integer;
begin
  result := -1; // label not found
  for i := 0 to high(labels) do
  begin
    if casesensitive then
    begin
      if s = labels[i].name then
        result := labels[i].id;
    end
    else
    begin
      if lowercase(s) = lowercase(labels[i].name) then
        result := labels[i].id;
    end;
    if result <> -1 then
      break;
  end;
end;

// Wraps a text and returns it as a stringlist
function CatWrapText(const text: string; const chars: integer): TStringList;
var
  sl: TStringList;
  P, ln: integer;
  s, newln: string;
begin
  sl := TStringList.Create;
  result := sl;
  if length(text) = 0 then
    Exit;
  ln := 0;
  sl.Add(emptystr);
  s := text + ' ';
  P := pos(' ', s);
  while P <> 0 do
  begin
    newln := Copy(s, 1, P);
    if (length(sl.Strings[ln]) + length(newln)) < (chars + 1) then
      sl.Strings[ln] := sl.Strings[ln] + newln
    else
    begin
      sl.Add(newln);
      inc(ln);
    end;
    Delete(s, 1, P);
    P := pos(' ', s);
  end;
end;

function ContainsAnyOfChars(const s: string; const aSet: TSysCharSet): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], aSet)) then
    begin
      result := true;
      break;
    end;
  end;
end;

function ContainsAnyOfStrings(s:string;aArray:array of string; IgnoreCase: Boolean = false):boolean;
var b: Byte;
begin
  Result:=false;
  if IgnoreCase then begin
      s:=lowercase(s);
      for b := Low(aArray) to High(aArray) do
      aArray[b]:=lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do begin
     if pos(aArray[b],s)<>0 then
     result:=True;
  end;
end;

function EndsWith(const s, prefix: string): Boolean;
begin
  result := AnsiEndsStr(prefix, s);
end;

function ExtractFromTag(const s, tag: string): string;
begin
  result := ExtractFromString(s, '<' + tag + '>', '</' + tag + '>');
end;

function GetLineByPos(const s: string; const Position: integer): integer;
var
  i, ln: integer;
begin
  result := -1;
  if (Position = -1) then
    Exit;

  i := 1;
  ln := 0;
  while i < Position do
  begin
    if (s[i] = #13) then
      ln := ln + 1;
    i := i + 1;
  end;
  result := ln;
end;

// Returns a valid Pascal component name (stripping invalid chars)
function GetValidCompName(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_'])) then
      result := result + Copy(s, i, 1);
  end;
end;

function HexToInt(const Hex: string; const WarnError: Boolean = false): integer;
begin
  if IsHexStr(Hex) then
    result := StrToInt('$' + Hex)
  else
  begin
    if WarnError = true then
      raise EConvertError.Create('Invalid character in hex string')
    else
      result := 0;
  end;
end;

function IIF(const Cond: Boolean; const TrueStr: String;
  const FalseStr: String = ''): string; overload;
begin
  if Cond = true then
    result := TrueStr
  else
    result := FalseStr;
end;

function IIF(const Cond: Boolean; const TrueInt: integer;
  const FalseInt: integer = 0): integer; overload;
begin
  if Cond = true then
    result := TrueInt
  else
    result := FalseInt;
end;

function IsInteger(const s: string): Boolean;
var
  v, c: integer;
begin
  Val(s, v, c);
  if v = 0 then
  begin // avoid compiler warning
  end;
  result := c = 0;
end;

// Returns true if the string contains valid hexadecimal digits
function IsHexStr(const s: string): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(s) do
    if not(CharInSet(s[i], ['0' .. '9', 'A' .. 'F', 'a' .. 'f'])) then
    begin
      result := false;
      break;
    end;
end;

function LastChar(const s: string): Char;
begin
  if s = emptystr then
    result := #0
  else
    result := s[length(s)];
end;

function LeftPad(const s: string; const c: Char; const len: integer): string;
var
  i: integer;
begin
  result := s;
  i := len - length(s);
  if i < 1 then
    Exit;
  result := s + StringOfChar(c, i);
end;

function RightPad(const s: string; const c: Char; const len: integer): string;
var
  i: integer;
begin
  result := s;
  i := len - length(s);
  if i < 1 then
    Exit;
  result := StringOfChar(c, i) + s;
end;

function LeftStr(s: string; c: longword): string;
begin
  result := Copy(s, 1, c);
end;

procedure MergeStrings(const dest, source: TStrings);
var
  i: integer;
begin
  for i := 0 to -1 + source.count do
    if dest.IndexOf(source[i]) = -1 then
      dest.Add(source[i]);
end;

function Occurs(substr, s: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to length(s) do
    if Copy(s, i, length(substr)) = substr then
      inc(result);
end;

function RandomCase(const S: string; const ToUpperSet: TSysCharSet=['a'..'z'];
  const ToLowerSet: TSysCharSet=['A'..'Z']): string;
var
  i : integer;
begin
  Randomize();
  Result := S;
  for I := 1 to Length(Result) do
    if Random(2) = 1 then
      if CharInset(Result[I], ToLowerSet) then
       Inc(Result[I], 32) else
      if CharInset(Result[I], ToUpperSet) then
       Dec(Result[I], 32);
end;

function RandomString(const len: integer;
  const chars: string = 'abcdefghijklmnopqrstuvwxyz'): string;
begin
  Randomize;
  result := emptystr;
  repeat
    result := result + chars[Random(length(chars)) + 1];
  until (length(result) = len);
end;

// Strips a quote pair off a string if it exists
// The leading and trailing quotes will only be removed if both exist
// Otherwise, the string is left unchanged
function RemoveQuotes(const s: string): string;
var
  i: integer;
begin
  result := s;
  i := length(s);
  if i = 0 then
    Exit;
  if (CharInSet(s[1], ['"', '''']) = true) and (s[1] = LastChar(s)) then
  begin
    Delete(result, 1, 1);
    SetLength(result, length(result) - 1);
  end;
end;

// Removes the last character from a string
function RemoveLastChar(const s: string): string;
var
  len: integer;
  astr: string;
begin
  astr := s;
  len := length(astr);
  if len > 0 then
    Delete(astr, len, 1);
  result := astr;
end;

function RemoveNumbers(const s: string): string;
var
  i, l: integer;
begin
  SetLength(result, length(s));
  l := 0;
  for i := 1 to length(s) do
    if not(CharInSet(s[i], ['0' .. '9'])) then
    begin
      inc(l);
      result[l] := s[i];
    end;
  SetLength(result, l);
end;

function RemoveShortcuts(const s: string): string;
begin
  result := ReplaceStr(s, '&', emptystr);
end;

function RepeatString(const s: string; count: cardinal): string;
var
  i: integer;
begin
  for i := 1 to count do
    result := result + s;
end;

function ReplaceStr(const s, substr, repstr: string): string;
begin
  result := stringreplace(s, substr, repstr, [rfReplaceAll]);
end;

function StrIncrease(const s: string; const step: integer = 1): string;
var
  i, c: integer;
  tmpstr: WideString;
begin
  tmpstr := '';
  for i := 1 to length(s) do
  begin
    c := ord(s[i]);
    inc(c, step);
    tmpstr := tmpstr + widechar(c);
  end;
  result := tmpstr;
end;

function StrDecrease(const s: string; const step: integer = 1): string;
var
  i, c: integer;
  tmpstr: WideString;
begin
  tmpstr := '';
  for i := 1 to length(s) do
  begin
    c := ord(s[i]);
    dec(c, step);
    tmpstr := tmpstr + widechar(c);
  end;
  result := tmpstr;
end;

function RestStr(const s: string; const index: longword): string;
var
  l: integer;
begin
  l := length(s);
  if l > 0 then
    result := Copy(s, index, l)
  else
    result := emptystr;
end;

procedure StripBlankLines(const sl: TStringList);
var
  i: integer;
begin
  for i := (sl.count - 1) downto 0 do
  begin
    if (Trim(sl[i]) = emptystr) then
      sl.Delete(i);
  end;
end;

function StrToCommaText(const s: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.text := s;
  result := sl.CommaText;
  sl.free;
end;

function StripChars(const s: string; const aSet: TSysCharSet): string;
var
  i, P: integer;
begin
  P := 0;
  SetLength(result, length(s));
  for i := 1 to length(s) do
  begin
    if not(CharInSet(s[i], aSet)) then
    begin
      inc(P);
      result[P] := s[i];
    end;
  end;
  SetLength(result, P);
end;

function ReplaceChars(const s: string; const aSet: TSysCharSet;
  const repwith: Char = '_'): string;
var
  i, P: integer;
begin
  P := 0;
  SetLength(result, length(s));
  for i := 1 to length(s) do
  begin
    inc(P);
    result[P] := s[i];
    if (CharInSet(s[i], aSet)) then
      result[P] := repwith;
  end;
  SetLength(result, P);
end;

function CommaTextToStr(const s: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.CommaText := s;
  result := sl.GetText;
  sl.free;
end;

function StrMaxLen(const s: string; const MaxLen: integer;
  const AddEllipsis: Boolean = false): string;
var
  i: integer;
begin
  result := s;
  if length(result) <= MaxLen then
    Exit;
  SetLength(result, MaxLen);
  if AddEllipsis = false then
    Exit;
  for i := MaxLen downto MaxLen - 2 do
    result[i] := '.';
end;

function StrToAlphaNum(const s: string): string;
var
  i: integer;
  tmpstr: string;
begin
  tmpstr := emptystr;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z'])) then
      tmpstr := tmpstr + Copy(s, i, 1);
  end;
  result := tmpstr;
end;

function StrToBool(const s: string): Boolean;
var
  tmpstr: string;
begin
  tmpstr := Trim(lowercase(s));
  if (tmpstr = 'true') or (tmpstr = '1') or (tmpstr = 'yes') or (tmpstr = 'y')
  then
    result := true
  else
    result := false;
end;

function StrToHex(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  for i := 1 to length(s) do
    result := result + IntToHex(ord(Copy(s, i, 1)[1]), 2);
end;

function StrToHex16(const s: string): string;
var
  i: Integer;
  str: string;
begin
  str := emptystr;
  for i := 1 to Length(s) do
    str := str + IntToHex(Integer(s[i]), 4);
  Result := str;
end;

function StrToIntSafe(const s: string; const FalseInt: integer = 0): integer;
begin
  if IsInteger(s) then
    result := StrToInt(s)
  else
    result := FalseInt;
end;

function StrToPWideChar(const s: string): PWideChar;
begin
  result := PWideChar(WideString(s));
end;

function HexToStr(const s: string): string;
var
  i: integer;
  h: string;
begin
  result := emptystr;
  try
    for i := 1 to length(s) div 2 do
    begin
      h := Copy(s, (i - 1) * 2 + 1, 2);
      result := result + Char(StrToInt('$' + h));
    end;
  except
    result := emptystr;
  end;
end;

function Hex16ToStr(const s: string): string;
var
  i: Integer;
  c: string;
begin
  Result := emptystr;
  i := 1;
  while i < Length(s) do
  begin
    c := Copy(s, i, 4);
    Result := Result + Chr(StrToInt('$' + c));
    Inc(i, 4);
  end;
end;

function TitleCase(const s: string): string;
var
  i: integer;
begin
  if s = emptystr then
    result := emptystr
  else
  begin
    result := Uppercase(s[1]);
    for i := 2 to length(s) do
      if s[i - 1] = ' ' then
        result := result + Uppercase(s[i])
      else
        result := result + lowercase(s[i]);
  end;
end;

// CONTRIBUTED ------------------------------------------------------------//

// Peter Below, 11.27.1996
function IScan(ch: Char; const s: string; fromPos: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := fromPos to length(s) do
  begin
    if s[i] = ch then
    begin
      result := i;
      break;
    end;
  end;
end;

// PB, 08.07.1997
procedure SplitString(const s: string; separator: Char;
  substrings: TStringList);
var
  i, n: integer;
begin
  if Assigned(substrings) and (length(s) > 0) then
  begin
    i := 1;
    repeat
      n := IScan(separator, s, i);
      if n = 0 then
        n := length(s) + 1;
      substrings.Add(Copy(s, i, n - i));
      i := n + 1;
    until i > length(s);
  end;
end;

// Based on an example by PB
procedure GetTextBetweenTags(const s, tag1, tag2: string; const list: TStrings;
  const includetags: Boolean = false);
var
  pScan, pEnd, pTag1, pTag2: PAnsiChar;
  foundText, searchtext: string;
begin
  searchtext := Uppercase(s);
  pTag1 := PAnsiChar(AnsiString(Uppercase(tag1)));
  pTag2 := PAnsiChar(AnsiString(Uppercase(tag2)));
  pScan := PAnsiChar(AnsiString(searchtext));
  repeat

{$IFDEF DXE2_OR_UP}
    pScan := System.AnsiStrings.StrPos(pScan, pTag1);
{$ELSE}
    pScan := StrPos(pScan, pTag1);
{$ENDIF}
    if pScan <> nil then
    begin
      inc(pScan, length(tag1));

{$IFDEF DXE2_OR_UP}
      pEnd := System.AnsiStrings.StrPos(pScan, pTag2);
{$ELSE}
      pEnd := StrPos(pScan, pTag2);
{$ENDIF}
      if pEnd <> nil then
      begin
        SetString(foundText, PAnsiChar(AnsiString(s)) +
          (pScan - PAnsiChar(AnsiString(searchtext))), pEnd - pScan);
        if includetags then
          list.Add(Uppercase(tag1) + foundText + Uppercase(tag2))
        else
          list.Add(foundText);
        list.text := list.GetText;
        pScan := pEnd + length(tag2);
      end
      else
        pScan := nil;
    end;
  until pScan = nil;
end;

// Based on an example by Mike Orriss
function ExtractFromString(const s, startstr, endstr: string): string;
var
  ps, pe: integer;
begin
  ps := pos(startstr, s);
  pe := pos(endstr, s);
  if (pe <= ps) or (ps = 0) then
    result := emptystr
  else
  begin
    inc(ps, length(startstr));
    result := Copy(s, ps, pe - ps);
  end;
end;

// Based on an example from Thomas Scheffczyk
function GetToken(const aString, SepChar: String; const TokenNum: Byte): String;
var
  Token, tmpstr: String;
  StrLen, Num, EndofToken: integer;
begin
  tmpstr := aString;
  StrLen := length(tmpstr);
  Num := 1;
  EndofToken := StrLen;
  while ((Num <= TokenNum) and (EndofToken <> 0)) do
  begin
    EndofToken := pos(SepChar, tmpstr);
    if EndofToken <> 0 then
    begin
      Token := Copy(tmpstr, 1, EndofToken - 1);
      Delete(tmpstr, 1, EndofToken);
      inc(Num);
    end
    else
      Token := tmpstr;
  end;
  if Num >= TokenNum then
    result := Token
  else
    result := emptystr;
end;

function PosX(const substr, s: string; Start: integer): integer;
var
  i, J, len: integer;
begin
  len := length(substr);
  if len = 0 then
  begin
    PosX := 1;
    Exit;
  end;
  for i := Start to Succ(length(s) - len) do
  begin
    J := 1;
    while J <= len do
    begin
      if not((substr[J] = '?') or (substr[J] = s[Pred(i + J)])) then
        break;
      inc(J);
    end;
    if J > len then
    begin
      PosX := i;
      Exit;
    end;
  end;
  PosX := 0;
end;

function MatchStrInArray(s:string;aArray:array of string; IgnoreCase: Boolean = false):boolean;
var b: Byte;
begin
  Result:=false;
  if IgnoreCase then begin
      s:=lowercase(s);
      for b := Low(aArray) to High(aArray) do
      aArray[b]:=lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do begin
     if s=aArray[b] then
     result:=True;
  end;
end;

{
  This function takes two strings and compares them. The first string
  can be anything, but should not contain pattern characters (* or ?).
  The pattern string can have as many of these pattern characters as you want.
  For example: MatchStrings('Pascal','*as*') would return True.

  Copyright (c) 1999 Arsne von Wyss
}
function MatchStrings(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
const
  WildSize = 0; { minimal number of characters representing a "*" }
var
  Min, Max, At, MaskSTart, MaskEnd: integer;
  T: string;
begin
  if IgnoreCase then
  begin
    for At := 1 to length(s) do
      s[At] := UpCase(s[At]);
    for At := 1 to length(Mask) do
      Mask[At] := UpCase(Mask[At]);
  end;
  s := s + #0;
  Mask := Mask + #0;
  Min := 1;
  Max := 1;
  MaskEnd := 0;
  while length(Mask) >= MaskEnd do
  begin
    MaskSTart := MaskEnd + 1;
    repeat
      inc(MaskEnd);
    until (MaskEnd > length(Mask)) or (Mask[MaskEnd] = '*');
    T := Copy(Mask, MaskSTart, MaskEnd - MaskSTart);
    At := PosX(T, s, Min);
    if (At = 0) or (At > Max) then
    begin
      MatchStrings := false;
      Exit;
    end;
    Min := At + length(T) + WildSize;
    Max := length(s);
  end;
  MatchStrings := true;
end;

// CONTRIBUTED ------------------------------------------------------------//
// Base64 encoder and decoder taken from Ararat Synapse's synacode.pas
{
  | Copyright (c)1999-2007, Lukas Gebauer                                        |
  | All rights reserved.                                                         |
  |                                                                              |
  | Redistribution and use in source and binary forms, with or without           |
  | modification, are permitted provided that the following conditions are met:  |
  |                                                                              |
  | Redistributions of source code must retain the above copyright notice, this  |
  | list of conditions and the following disclaimer.                             |
  |                                                                              |
  | Redistributions in binary form must reproduce the above copyright notice,    |
  | this list of conditions and the following disclaimer in the documentation    |
  | and/or other materials provided with the distribution.                       |
  |                                                                              |
  | Neither the name of Lukas Gebauer nor the names of its contributors may      |
  | be used to endorse or promote products derived from this software without    |
  | specific prior written permission.                                           |
  |                                                                              |
  | THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
  | AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
  | IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
  | ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
  | ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
  | DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
  | SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
  | CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
  | LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
  | OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
  | DAMAGE.                                                                      |
}
function Encode3to4(const Value, Table: AnsiString): AnsiString;
var
  c: Byte;
  n, l: integer;
  count: integer;
  DOut: array [0 .. 3] of Byte;
begin
  SetLength(result, ((length(Value) + 2) div 3) * 4);
  l := 1;
  count := 1;
  while count <= length(Value) do
  begin
    c := ord(Value[count]);
    inc(count);
    DOut[0] := (c and $FC) shr 2;
    DOut[1] := (c and $03) shl 4;
    if count <= length(Value) then
    begin
      c := ord(Value[count]);
      inc(count);
      DOut[1] := DOut[1] + (c and $F0) shr 4;
      DOut[2] := (c and $0F) shl 2;
      if count <= length(Value) then
      begin
        c := ord(Value[count]);
        inc(count);
        DOut[2] := DOut[2] + (c and $C0) shr 6;
        DOut[3] := (c and $3F);
      end
      else
      begin
        DOut[3] := $40;
      end;
    end
    else
    begin
      DOut[2] := $40;
      DOut[3] := $40;
    end;
    for n := 0 to 3 do
    begin
      if (DOut[n] + 1) <= length(Table) then
      begin
        result[l] := Table[DOut[n] + 1];
        inc(l);
      end;
    end;
  end;
  SetLength(result, l - 1);
end;

function Decode4to3Ex(const Value, Table: AnsiString): AnsiString;
var
  x, y, lv: integer;
  d: integer;
  dl: integer;
  c: Byte;
  P: integer;
begin
  lv := length(Value);
  SetLength(result, lv);
  x := 1;
  dl := 4;
  d := 0;
  P := 1;
  while x <= lv do
  begin
    y := ord(Value[x]);
    if y in [33 .. 127] then
      c := ord(Table[y - 32])
    else
      c := 64;
    inc(x);
    if c > 63 then
      continue;
    d := (d shl 6) or c;
    dec(dl);
    if dl <> 0 then
      continue;
    result[P] := AnsiChar((d shr 16) and $FF);
    inc(P);
    result[P] := AnsiChar((d shr 8) and $FF);
    inc(P);
    result[P] := AnsiChar(d and $FF);
    inc(P);
    d := 0;
    dl := 4;
  end;
  case dl of
    1:
      begin
        d := d shr 2;
        result[P] := AnsiChar((d shr 8) and $FF);
        inc(P);
        result[P] := AnsiChar(d and $FF);
        inc(P);
      end;
    2:
      begin
        d := d shr 4;
        result[P] := AnsiChar(d and $FF);
        inc(P);
      end;
  end;
  SetLength(result, P - 1);
end;

function Base64Encode(const s: string): string;
const
  TableBase64 =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
begin
  result := string(Encode3to4(AnsiString(s), TableBase64));
end;

function Base64Decode(const s: string): string;
const
  ReTablebase64 = #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$40 + #$3E + #$40 + #$40 + #$40 + #$3F + #$34 + #$35 + #$36 + #$37 + #$38 +
    #$39 + #$3A + #$3B + #$3C + #$3D + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$40 + #$00 + #$01 + #$02 + #$03 + #$04 + #$05 + #$06 + #$07 + #$08 + #$09 +
    #$0A + #$0B + #$0C + #$0D + #$0E + #$0F + #$10 + #$11 + #$12 + #$13 + #$14 +
    #$15 + #$16 + #$17 + #$18 + #$19 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$1A + #$1B + #$1C + #$1D + #$1E + #$1F + #$20 + #$21 + #$22 + #$23 + #$24 +
    #$25 + #$26 + #$27 + #$28 + #$29 + #$2A + #$2B + #$2C + #$2D + #$2E + #$2F +
    #$30 + #$31 + #$32 + #$33 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40;
begin
  result := string(Decode4to3Ex(AnsiString(s), ReTablebase64));
end;

{

  Taken from md5.pas v1.0.3
  Copyright 2012-2013 Stijn Sanders
  License: MIT (http://opensource.org/licenses/mit-license.php)

  https://github.com/stijnsanders/TMongoWire/blob/master/mongoAuth.pas

  Based on http://www.ietf.org/rfc/rfc1321.txt

}

function MD5Hash(s: UTF8String): UTF8String;
const
  roll1: array [0 .. 3] of cardinal = (7, 12, 17, 22);
  roll2: array [0 .. 3] of cardinal = (5, 9, 14, 20);
  roll3: array [0 .. 3] of cardinal = (4, 11, 16, 23);
  roll4: array [0 .. 3] of cardinal = (6, 10, 15, 21);
  base1: array [0 .. 15] of cardinal = ($D76AA478, $E8C7B756, $242070DB,
    $C1BDCEEE, $F57C0FAF, $4787C62A, $A8304613, $FD469501, $698098D8, $8B44F7AF,
    $FFFF5BB1, $895CD7BE, $6B901122, $FD987193, $A679438E, $49B40821);
  base2: array [0 .. 15] of cardinal = ($F61E2562, $C040B340, $265E5A51,
    $E9B6C7AA, $D62F105D, $02441453, $D8A1E681, $E7D3FBC8, $21E1CDE6, $C33707D6,
    $F4D50D87, $455A14ED, $A9E3E905, $FCEFA3F8, $676F02D9, $8D2A4C8A);
  base3: array [0 .. 15] of cardinal = ($FFFA3942, $8771F681, $6D9D6122,
    $FDE5380C, $A4BEEA44, $4BDECFA9, $F6BB4B60, $BEBFBC70, $289B7EC6, $EAA127FA,
    $D4EF3085, $04881D05, $D9D4D039, $E6DB99E5, $1FA27CF8, $C4AC5665);
  base4: array [0 .. 15] of cardinal = ($F4292244, $432AFF97, $AB9423A7,
    $FC93A039, $655B59C3, $8F0CCC92, $FFEFF47D, $85845DD1, $6FA87E4F, $FE2CE6E0,
    $A3014314, $4E0811A1, $F7537E82, $BD3AF235, $2AD7D2BB, $EB86D391);
  Hex: array [0 .. 15] of AnsiChar = '0123456789abcdef';
var
  a: cardinal;
  dl, i, J, k, l: integer;
  d: array of cardinal;
  g, h: array [0 .. 3] of cardinal;
begin
  a := length(s);
  dl := a + 9;
  if (dl and $3F) <> 0 then
    dl := (dl and $FFC0) + $40;
  i := dl;
  dl := dl shr 2;
  SetLength(d, dl);
  SetLength(s, i);
  J := a + 1;
  s[J] := #$80;
  while J < i do
  begin
    inc(J);
    s[J] := #0;
  end;
  Move(s[1], d[0], i);
  d[dl - 2] := a shl 3;
  h[0] := $67452301;
  h[1] := $EFCDAB89;
  h[2] := $98BADCFE;
  h[3] := $10325476;
  i := 0;
  while i < dl do
  begin
    g := h;
    J := i;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + ((h[(l + 1) and 3] and h[(l + 2) and 3]) or
        (not(h[(l + 1) and 3]) and h[(l + 3) and 3])) + d[J] + base1[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll1[k and 3]) or (a shr (32 - roll1[k and 3])));
      inc(J);
    end;
    J := 1;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + ((h[(l + 3) and 3] and h[(l + 1) and 3]) or
        (not(h[(l + 3) and 3]) and h[(l + 2) and 3])) + d[i or (J and $F)]
        + base2[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll2[k and 3]) or (a shr (32 - roll2[k and 3])));
      inc(J, 5);
    end;
    J := 5;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + (h[(l + 1) and 3] xor h[(l + 2) and 3] xor h[(l + 3) and
        3]) + d[i or (J and $F)] + base3[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll3[k and 3]) or (a shr (32 - roll3[k and 3])));
      inc(J, 3);
    end;
    J := 0;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + (h[(l + 2) and 3] xor (h[(l + 1) and 3] or
        not h[(l + 3) and 3])) + d[i or (J and $F)] + base4[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll4[k and 3]) or (a shr (32 - roll4[k and 3])));
      inc(J, 7);
    end;
    for k := 0 to 3 do
      inc(h[k], g[k]);
    inc(i, 16);
  end;
  SetLength(result, 32);
  for k := 0 to 31 do
    result[k + 1] := Hex[h[k shr 3] shr ((k xor 1) shl 2) and $F];
end;

// ------------------------------------------------------------------------//
end.
