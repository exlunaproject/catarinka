unit CatVariants;

{
  Catarinka - Variant Utils

  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, System.Variants;
{$ELSE}
  SysUtils, Variants;
{$ENDIF}
// Useful for returning a boolean value together with a string like an error
// message or an integer when calling a function
type
 TCatFuncResultV = record
   B: boolean;
   I: integer;
   V: Variant;
   S: string;
 end;


function VarTypeDescribe(const Value: Variant): string;

implementation

function VarTypeDescribe(const Value: Variant): string;
begin
  result := emptystr;
  case TVarData(Value).vType of
    varString, {$IFDEF UNICODE}varUString, {$ENDIF}varOleStr:
      result := 'string';
    varBoolean:
      result := 'boolean';
    varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord,
{$IFDEF UNICODE}varUInt64, {$ENDIF} varInt64:
      result := 'integer';
    varDouble:
      result := 'double';
  end;
end;

// ------------------------------------------------------------------------//
end.
