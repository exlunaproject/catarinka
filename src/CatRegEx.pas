unit CatRegex;
{
  Catarinka - Regular Expression and some other useful matching functions

  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Uses the RegExpr library by Andrey V. Sorokin.
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
function CatMatch(const sigpattern, s: string): boolean;
function IsValidEmail(email: string): boolean;
function ExtractVersionFromString(s:string):string;
function MatchStrInSepStr(const s,tags:string;separator:string=','):boolean;
function MatchVersion(curver, vercheck:string):boolean;
function MatchVersionRange(curver, vercheck:string):boolean;
function MatchVersionEx(curver, vercheck:string):boolean;

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
function CatMatch(const sigpattern, s: string): boolean;
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
    result := MatchStrings(content, pat);
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
        if matchstrings(curver, outver) = true  then
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

// ------------------------------------------------------------------------//
end.
