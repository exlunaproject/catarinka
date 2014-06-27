unit CatCLUtils;
{
  Catarinka - Command-line parameters related functions

  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}
function GetCmdLine: string;
function GetCmdParam(const param: string; const def_value: string = ''): string;
function GetCmdParamQuoted(const param: string;
  const def_value: string = ''): string;
function HasCmdParam(const param: string): boolean;

implementation

uses
  CatStrings;

function GetCmdLine: string;
var
  i: integer;
begin
  result := emptystr;
  if ParamCount > 0 then
    for i := 1 to ParamCount do
      result := result + ' ' + (ParamStr(i));
end;

// eg: if hascmdparam('-test') then ...
function HasCmdParam(const param: string): boolean;
var
  i: integer;
begin
  result := false;
  if ParamCount = 0 then
    exit;
  for i := 1 to ParamCount do
  begin
    if lowercase(ParamStr(i)) = lowercase(param) then
      result := true;
  end;
end;

// if paramstr is: name:somestring
// eg: getCmdParam('name') will return "somestring"
function GetCmdParam(const param: string; const def_value: string = ''): string;
var
  i: integer;
  params: string;
begin
  result := emptystr;
  if ParamCount = 0 then
    exit;
  for i := 1 to ParamCount do
    params := params + ' ' + (ParamStr(i));
  params := params + ' ';
  result := after(params, param + ':');

  result := before(result, ' ');
  if result = emptystr then
    result := def_value;
end;

function GetCmdParamQuoted(const param: string;
  const def_value: string = ''): string;
var
  i: integer;
  params: string;
const
  quote = '"';
begin
  result := emptystr;
  if ParamCount = 0 then
    exit;
  for i := 1 to ParamCount do
    params := params + ' ' + (ParamStr(i));
  params := params + ' ';
  result := after(params, param + ':');

  if beginswith(result, quote) then
  begin
    result := after(result, quote);
    result := before(result, quote);
  end
  else
    result := before(result, ' ');
  if result = emptystr then
    result := def_value;
end;

// ------------------------------------------------------------------------//
end.
