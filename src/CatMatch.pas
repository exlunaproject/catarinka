unit CatMatch;
{
  Catarinka - Regular Expression and various other useful matching functions

  Copyright (c) 2003-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Uses the RegExpr library by Andrey V. Sorokin.
  MatchWildcard function by Arsne von Wyss
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, RegExpr;
{$ELSE}
  Classes, SysUtils, RegExpr;
{$ENDIF}

type
  TCatRegExpr = TRegExpr;

function RegExpFind(const s, re: string): string;
function RegExpReplace(const s, re, sreplacement: string): string; overload;
function RegExpReplace(const s, re: string; refunc: TRegExprReplaceFunction): string; overload;
function CatMatchSignature(const sigpattern, s: string): boolean;
function IsValidEmail(email: string): boolean;
function ExtractVersionFromString(s:string):string;
function MatchStrInSepStr(const s,tags:string;separator:string=','):boolean;
function MatchVersion(curver, vercheck:string):boolean;
function MatchVersionRange(curver, vercheck:string):boolean;
function MatchVersionEx(curver, vercheck:string):boolean;
function MatchWildcard(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
function MatchWildcardX(s, Mask: string; IgnoreCase: Boolean = false): Boolean;

implementation

uses CatStrings, CatStringLoop;

function RegExpReplace(const s, re, sreplacement: string): string;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := re;
    result := r.Replace(s, sreplacement, true);
  finally
    r.Free;
  end;
end;

function RegExpReplace(const s, re: string; refunc: TRegExprReplaceFunction): string;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := re;
    result := r.ReplaceEx(s, refunc);
  finally
    r.Free;
  end;
end;

function RegExpFind(const s, re: string): string;
var
  r: TRegExpr;
begin
  result := emptystr;
  r := TRegExpr.Create;
  try
    r.Expression := re;
    if r.Exec(s) then
      repeat
        result := result + r.Match[0] + ', ';
      until not r.ExecNext;
  finally
    r.Free;
  end;
end;

// Allows to match a string using different methods:
// [somestring] -- case sensitive contains
// wild:[somestring] -- case sensitive wild match
// regexp:[expression] -- regular expression
// icase:[somestring] -- case insensitive contains
// icase:wild:[somestring] -- case insensitive wild match (* and ?)
function CatMatchSignature(const sigpattern, s: string): boolean;
const
  cIgnoreCase = 'icase:';
  cReEx = 'regexp:';
  cWild = 'wild:';
var
  sig, content, pat: string;
begin
  sig := sigpattern;
  content := s;
  if BeginsWith(sig,cIgnoreCase) then
  begin
    sig := after(sig, cIgnoreCase);
    sig := lowercase(sig);
    content := lowercase(content);
  end;
  if BeginsWith(sig,cReEx) then
  begin
    pat := after(sig, cReEx);
    result := (RegExpFind(content, pat) <> emptystr);
  end
  else
  if BeginsWith(sig,cWild) then
  begin
    pat := after(sig, cWild);
    result := MatchWildcard(content, pat);
  end
  else
  begin
    result := (pos(sig, content) <> 0);
  end;
end;

// Thanks ffert2907
function IsValidEmail(email: string): boolean;
const
  charslist = ['_', '-', '.', '0'..'9', 'A'..'Z', 'a'..'z'];
var
  Arobasc, lastpoint : boolean;
  i, n : integer;
  c : char;
begin
  n := Length(email);
  i := 1;
  Arobasc := false;
  lastpoint := false;
  result := true;
  while (i <= n) do begin
    c := email[i];
    if c = '@' then
    begin
      if Arobasc then  // Only 1 Arobasc
      begin
        result := false;
        exit;
      end;
      Arobasc := true;
    end
    else if (c = '.') and Arobasc then  // at least 1 . after arobasc
    begin
      lastpoint := true;
    end
    else if not(c in charslist) then  // valid chars
    begin
      result := false;
      exit;
    end;
    inc(i);
  end;
  if not(lastpoint) or (email[n] = '.')then  // not finish by . and have a . after arobasc
    result := false;
end;

function ExtractVersionFromString(s:string):string;
begin
  result := RegExpFind(s, '\d+(\.\d+)+');
  if pos(',', result) <> 0 then
  result := before(result, ',');
end;

// Matches a string against a separated string (comma-based if separator is ommited)
function MatchStrInSepStr(const s,tags:string;separator:string=','):boolean;
var Tag: TSepStringLoop;
begin
  result := false;
  if tags <> emptystr then
  begin
      Tag := TSepStringLoop.Create(tags, separator);
      while Tag.Found do
      begin
        if Tag.Current = s then
          result := true;
      end;
      Tag.Free;
  end;
end;

// expects expression like: <3.4.0 or <=3.4.0
// when using just the equal sign, can be followed by a wildcard, ex:
// =* (matches any version), =3.*, =3.1.??
function MatchVersion(curver, vercheck:string):boolean;
var
  outver:string;
begin
  result := false;
  outver := ExtractVersionFromString(vercheck);
    if beginswith(vercheck, '>=') then begin
        if curver >= outver  then
        result := true;
    end else
    if beginswith(vercheck, '<=') then begin
        if curver <= outver  then
        result := true;
    end else
    if beginswith(vercheck, '<') then begin
        if curver < outver  then
        result := true;
    end else
    if beginswith(vercheck, '>') then begin
        if curver > outver  then
        result := true;
    end else
    if beginswith(vercheck, '=') then begin
        outver := after(vercheck, '=');
        if matchwildcard(curver, outver) = true  then
        result := true;
    end;
end;

// expects a version check range separated by spaces like:
// >=1.4.2 <1.6.2
// must match all to return true
function MatchVersionRange(curver, vercheck:string):boolean;
var
  s: TSepStringLoop;
begin
  result := true;
  s := TSepStringLoop.Create(vercheck,' ');
  while s.Found do begin
    if MatchVersion(curver,s.Current) = false then
        result := false;
  end;
  s.Free;
end;

// expects version check list separated by commas like:
// <1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0
// must match at least one of the checks to return true
// wildcards can be used when using just equal sign, ex:
// usage examples:
// MatchVersionEx('2.2.3','<1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0') -> true
// MatchVersionEx('3.0.1','<1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0') -> false
// MatchVersionEx('2.1.11','=2.1.??,=3.*') -> true
function MatchVersionEx(curver, vercheck:string):boolean;
var
  s: TSepStringLoop;
begin
  result := false;
  s := TSepStringLoop.Create(vercheck,',');
  while s.Found do begin
    if MatchVersionRange(curver,s.Current) = true then begin
        result := true;
        s.Stop;
    end;
  end;
  s.Free;
end;

function MatchWildcard_Pos(const substr, s: string; Start: integer): integer;
var
  i, J, len: integer;
begin
  len := length(substr);
  if len = 0 then
  begin
    result := 1;
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
      result := i;
      Exit;
    end;
  end;
  result := 0;
end;

{
  This function takes two strings and compares them. The first string
  can be anything, but should not contain pattern characters (* or ?).
  The pattern string can have as many of these pattern characters as you want.
  For example: MatchStrings('Pascal','*as*') would return True.

  Contributed by Arsne von Wyss (1999)
}
function MatchWildcard(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
const
  WildSize = 0; { minimal number of characters representing a "*" }
var
  Min, Max, At, MaskSTart, MaskEnd: integer;
  T: string;
begin
  if IgnoreCase then
  begin
    s := uppercase(s);
    mask := uppercase(mask);
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
    At := MatchWildcard_Pos(T, s, Min);
    if (At = 0) or (At > Max) then
    begin
      result := false;
      Exit;
    end;
    Min := At + length(T) + WildSize;
    Max := length(s);
  end;
  result := true;
end;

function MatchWildcardX_Pos(const substr, s: string; Start: integer): integer;
var
  i, J, len: integer;
begin
  len := length(substr);
  if len = 0 then
  begin
    result := 1;
    Exit;
  end;
  for i := Start to Succ(length(s) - len) do
  begin
    J := 1;
    while J <= len do
    begin
      if (substr[J] = '#') and (IsInteger(s[Pred(i + J)]) <> true) then
        break;
      if not(ContainsAnyOfChars(substr[J], ['?','#']) or (substr[J] = s[Pred(i + J)])) then
        break;
      inc(J);
    end;
    if J > len then
    begin
      result := i;
      Exit;
    end;
  end;
  result := 0;
end;

// Extended version of MatchWildcard() function that allows hash (#) to be used
// as part of mask to match a number
// by Felipe Daragon
function MatchWildcardX(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
const
  WildSize = 0; { minimal number of characters representing a "*" }
var
  Min, Max, At, MaskSTart, MaskEnd: integer;
  T: string;
begin
  if IgnoreCase then
  begin
    s := uppercase(s);
    mask := uppercase(mask);
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
    At := MatchWildcardX_Pos(T, s, Min);
    if (At = 0) or (At > Max) then
    begin
      result := false;
      Exit;
    end;
    Min := At + length(T) + WildSize;
    Max := length(s);
  end;
  result := true;
end;

// ------------------------------------------------------------------------//
end.
