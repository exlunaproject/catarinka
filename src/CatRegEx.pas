unit CatRegex;
{
  Catarinka - Regular Expression functions

  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Uses the RegExpr library by Andrey V. Sorokin.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}
function RegExpFind(const s, re: string): string;
function RegExpReplace(const s, re, sreplacement: string): string;
function CatMatch(const substr, s: string): boolean;

implementation

uses CatStrings, RegExpr;

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

function CatMatch(const substr, s: string): boolean;
const
  cReEx = 'regexp:';
var
  tmpsub: string;
begin
  result := false;
  tmpsub := substr;
  if (pos(cReEx, tmpsub) <> 0) then
  begin
    tmpsub := after(tmpsub, cReEx);
    if (RegExpFind(s, tmpsub) <> emptystr) then
      result := true;
  end
  else
  begin
    if (pos(tmpsub, s) <> 0) then
      result := true;
  end;
end;

// ------------------------------------------------------------------------//
end.
