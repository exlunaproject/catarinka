unit CatRegex;
{
  Catarinka - Regular Expression and some other useful matching functions

  Copyright (c) 2003-2017 Felipe Daragon
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

implementation

uses CatStrings;

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

// ------------------------------------------------------------------------//
end.
