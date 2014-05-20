unit CatLuaUtils;
{
  Lua Utils
  Functions for getting and setting local and global variables in Lua

  Copyright (c) 2003-2014 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
  
  Work in progress
}

interface

uses
  Lua, pLua, Variants, SysUtils;

type
  MyPAnsiChar = {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF};

function pLuaAnyToString(L: PLua_State; idx: integer): string;
function pLua_GetLuaVar(L: PLua_State; idx: integer): Variant;
function pLua_GetGlobal(L: PLua_State; valName: AnsiString): Variant;
function pLua_GetLocal(L: PLua_State; valName: AnsiString): Variant;
procedure pLua_SetGlobal(L: PLua_State; valName: AnsiString;
  const AValue: Variant);
procedure pLua_SetLocal(L: PLua_State; valName: AnsiString;
  const AValue: Variant);

// Gets a table field
function pLua_GetFieldStr(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: string = ''): string;
function pLua_GetFieldInt(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: integer): integer;
function pLua_GetFieldBool(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: boolean): boolean;
function pLua_GetFieldVariant(L: PLua_State; idx: integer;
  FieldName: MyPAnsiChar; ADefaultValue: Variant): Variant;

// Sets a table field
procedure pLua_SetFieldStr(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: string);
procedure pLua_SetFieldInt(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: integer);
procedure pLua_SetFieldBool(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: boolean);
procedure pLua_SetFieldVariant(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: Variant);

implementation

function pLua_GetFieldStr(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: string = ''): string;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    result := ADefaultValue
  else
    result := lua_tostring(L, -1);
end;

function pLua_GetFieldInt(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: integer): integer;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    result := ADefaultValue
  else
    result := lua_tointeger(L, -1);
end;

function pLua_GetFieldBool(L: PLua_State; idx: integer; FieldName: MyPAnsiChar;
  ADefaultValue: boolean): boolean;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    result := ADefaultValue
  else
    result := lua_toboolean(L, -1);
end;

function pLua_GetFieldVariant(L: PLua_State; idx: integer;
  FieldName: MyPAnsiChar; ADefaultValue: Variant): Variant;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    result := ADefaultValue
  else
    result := plua_tovariant(L, -1);
end;

function pLuaAnyToString(L: PLua_State; idx: integer): string;
var
  ltype: integer;
begin
  result := emptystr;
  ltype := lua_type(L, idx);
  case ltype of
    LUA_TSTRING:
      result := {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(lua_tostring(L, idx));
    LUA_TBOOLEAN:
      begin
        if lua_toboolean(L, idx) = true then
          result := 'true'
        else
          result := 'false';
      end;
    LUA_TNUMBER:
      begin
        if TVarData(plua_tovariant(L, idx)).vType = varDouble then
          result := floattostr(lua_tonumber(L, idx))
        else
          result := inttostr(lua_tointeger(L, idx));
      end;
  end;
end;

procedure pLua_SetFieldStr(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: string);
begin
  plua_pushstring(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure pLua_SetFieldInt(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: integer);
begin
  lua_pushinteger(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure pLua_SetFieldBool(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: boolean);
begin
  lua_pushboolean(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure pLua_SetFieldVariant(L: PLua_State; FieldName: MyPAnsiChar;
  AValue: Variant);
begin
  plua_pushvariant(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

function pLua_GetLuaVar(L: PLua_State; idx: integer): Variant;
var
  ltype: integer;
  v: Variant;
  s: string;
begin
  ltype := lua_type(L, idx);
  case ltype of
    LUA_TSTRING:
      begin
        s := {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(lua_tostring(L, idx));
        v := s;
      end;
    LUA_TBOOLEAN:
      v := lua_toboolean(L, idx);
    LUA_TNUMBER:
      v := lua_tointeger(L, idx);
  else
    v := plua_tovariant(L, idx);
  end;
  result := v;
end;

function pLua_GetGlobal(L: PLua_State; valName: AnsiString): Variant; // working
var
  v: Variant;
begin
  result := NULL;
  lua_pushstring(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(valName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  try
    // writeln('getting '+valname);
    v := pLua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
    result := v;
    // writeln('got '+valname+': '+result);
  finally
    lua_pop(L, 1);
  end;
end;

function pLua_GetLocal(L: PLua_State; valName: AnsiString): Variant;
var
  ar: plua_Debug; // use plua_debug, not lua_debug!
  VarName: MyPAnsiChar;
  VarValue: Variant;
  current, stack: integer;
  found: boolean;
  Name: string;
begin
  result := NULL;
  found := false;
  Name := valName;

  lua_getglobal(L, 'tostring'); // this fixes ocasional crash with lua_getstack
  if lua_getstack(L, 1, @ar) <> 1 then
  begin
    Exit;
  end;
  current := 1;
  VarName := lua_getlocal(L, @ar, current);
  while VarName <> nil do
  begin
    // lua_pop(L,1);
    // writeln('Matching var:'+varname);
    if VarName = Name then
    begin
      found := true;
      // writeln('Found var:'+varname);
      try
        VarValue := pLua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
      finally
        lua_pop(L, 1);
      end;
      // writeln('found!'+varname+';'+varvalue);
      result := VarValue;
      Exit;
    end;
    lua_pop(L, 1);
    VarName := lua_getlocal(L, @ar, current);
    inc(current);
  end;
  if found = false then
  begin // local not found, tries to get global with the same name
    // writeln('not found locally:'+valname);
    try
      VarValue := pLua_GetGlobal(L, valName);
    except
    end;
    result := VarValue;
    // writeln('global search for '+valname+' returned):'+result);
  end;
end;

procedure pLua_SetLocal(L: PLua_State; valName: AnsiString;
  const AValue: Variant);
var
  ar: plua_Debug;
  VarName: MyPAnsiChar;
  current: integer;
  found: boolean;
  Name: string;
  NewValue: Variant;
begin
  found := false;
  Name := valName;
  NewValue := AValue;

  if lua_getstack(L, 1, @ar) <> 1 then
  begin
    Exit;
  end;
  current := 1;
  VarName := lua_getlocal(L, @ar, current);
  while VarName <> nil do
  begin
    if VarName = Name then
    begin
      found := true;
      // writeln('Found var:'+varname+' changing to:'+newvalue);
      // lua_pop(L,1);
      try
        plua_pushvariant(L, NewValue);
        lua_setlocal(L, @ar, current);
      finally
        lua_pop(L, 1);
      end;
      // writeln('Changed var:'+varname+' to:'+newvalue);
      Exit;
    end;
    lua_pop(L, 1);
    inc(current);
    VarName := lua_getlocal(L, @ar, current);

  end;
  if found = false then
  begin // new, local not found, tries to set global with the same name
    // writeln('not found locally:'+valname);
    pLua_SetGlobal(L, valName, NewValue);
  end;
end;

procedure pLua_SetGlobal(L: PLua_State; valName: AnsiString;
  const AValue: Variant);
begin
  // writeln('setting glob:'+valname+'; new value: '+avalue);
  if VarIsType(AValue, varString) then
  begin
    lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(valName));
    lua_pushstring(L,
{$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(AnsiString(AValue)));
    lua_settable(L, LUA_GLOBALSINDEX);
  end
  else
  begin
    lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(valName));
    plua_pushvariant(L, AValue);
    lua_settable(L, LUA_GLOBALSINDEX);
  end;
end;

// ------------------------------------------------------------------------//
end.
