unit CtkTarman;

{
  TAR File Format Management Lua library
  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Lua, pLua, SysUtils, Classes;

procedure RegisterTarman(L: plua_State);
function luaopen_Tarman(L: plua_State): integer; cdecl;
function Lua_TARToDir(L: plua_State): integer; cdecl;
function Lua_DirToTAR(L: plua_State): integer; cdecl;

implementation

uses CatFiles, CatStrings, CatTarman, CatStringLoop;

procedure RegisterTarman(L: plua_State);
begin
  luaopen_Tarman(L);
end;

function Lua_TARToDir(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   TARToDir(lua_tostring(L, 1), lua_tostring(L, 2));
end;

function Lua_DirToTAR(L: plua_State): integer; cdecl;
begin
if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING],[vaOptional1]).OK then begin
   if lua_isnone(L, 3) then
     DirToTAR(lua_tostring(L, 1), lua_tostring(L, 2))
   else
     DirToTAR(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3));
  end;
end;

function luaopen_Tarman(L: plua_State): integer; cdecl;
begin
  lua_register(L, 'dir2tar', Lua_DirToTAR);
  lua_register(L, 'tar2dir', Lua_TARToDir);
  result := 0;
end;

end.
