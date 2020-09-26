library Catarinka;

{
  Catarinka Lua Library
  Copyright (c) 2013-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

uses
  Lua,
  CtkCore in 'CtkCore.pas';


{$I CatCompactLib.inc}

{$R *.res}

function luaopen_Catarinka(L: plua_State): integer; cdecl;
begin
  Result := RegisterCatarinka(L);
end;

Exports
  luaopen_Catarinka;

begin

end.
