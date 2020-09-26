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
  Winapi.Windows, System.SysUtils;
{$ELSE}
  Windows, SysUtils;
{$ENDIF}
function GetCmdLine(const start:integer=1): string;
function GetCmdParam(const param: string; const def_value: string = ''): string;
function GetCmdParamQuoted(const param: string;
  const def_value: string = ''): string;
function HasCmdParam(const param: string): boolean;

implementation

uses
  CatStrings;

function GetCmdLine(const start:integer=1): string;
var
  i: integer;
begin
  result := emptystr;
  if ParamCount > 0 then
    for i := start to ParamCount do
      result := result + ' ' + (ParamStr(i));
end;

// Example: hascmdparam('-test') or ('-test:anystr') returns true
function HasCmdParam(const param: string): boolean;
var
  i: integer;
  curparam: string;
begin
  result := false;
  if ParamCount = 0 then
    exit;
  for i := 1 to ParamCount do
  begin
    curparam := lowercase(ParamStr(i));
    if pos(':', curparam) <> 0 then
      curparam := before(curparam, ':');
    if curparam = lowercase(param) then
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
  result := def_value;
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
  result := def_value;
  if ParamCount = 0 then
    exit;
  params := GetCommandLine();
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
