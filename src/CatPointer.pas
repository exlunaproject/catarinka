unit CatPointer;
{
  Catarinka - Pointer To String and vice-versa functions

  Copyright (c) 2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
{$IF CompilerVersion >= 23} // XE2 or higher
  System.SysUtils;
{$ELSE}
  SysUtils;
{$IFEND}
function PointerToStr(const P: Pointer): string;
function StrToPointer(const s: string): Pointer;

implementation

function PointerToStr(const P: Pointer): string;
var
  PP: Pointer;
  PC: ^Cardinal;
begin
  PP := @P;
  PC := PP;
  Result := string(pansichar(PC^));
end;

function StrToPointer(const s: string): Pointer;
var
  c: Cardinal;
  P: Pointer;
  PC: ^Cardinal;
  PP: ^Pointer;
  tStr: pansichar;
begin
  GetMem(tStr, 1 + Length(s));
  StrPCopy(tStr, ansistring(s));
  c := integer(tStr);
  PC := @c;
  P := PC;
  PP := P;
  Result := PP^;
end;

end.
