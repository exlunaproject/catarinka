unit uStrList;

{
  Catarinka Lua Library - String List Object
  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, SysUtils, Lua, pLua, LuaObject;

type
  { TCatarinkaStrList }
  TCatarinkaStrList = class(TLuaObject)
  private
  public
    obj: TStringList;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterCatarinkaStrList(L: PLua_State);

implementation

function method_add(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.add(lua_tostring(L, 2));
  result := 1;
end;

function method_insert(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.insert(lua_tointeger(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function method_delete(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.delete(lua_tointeger(L, 2));
  result := 1;
end;

function method_savetofile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.savetofile(lua_tostring(L, 2));
  result := 1;
end;

function method_loadfromfile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.loadfromfile(lua_tostring(L, 2));
  result := 1;
end;

function method_clear(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.clear;
  result := 1;
end;

function method_sort(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  ht.obj.sort;
  result := 1;
end;

function method_indexof(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  lua_pushinteger(L, ht.obj.indexof(lua_tostring(L, 2)));
  result := 1;
end;

function method_getvalue(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  lua_pushstring(L, ht.obj.values[lua_tostring(L, 2)]);
  result := 1;
end;

function method_getstringfromindex(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  lua_pushstring(L, ht.obj[lua_tointeger(L, 2)]);
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, 'add', @method_add, classTable);
  RegisterMethod(L, 'insert', @method_insert, classTable);
  RegisterMethod(L, 'indexof', @method_indexof, classTable);
  RegisterMethod(L, 'delete', @method_delete, classTable);
  RegisterMethod(L, 'clear', @method_clear, classTable);
  RegisterMethod(L, 'get', @method_getstringfromindex, classTable);
  RegisterMethod(L, 'loadfromfile', @method_loadfromfile, classTable);
  RegisterMethod(L, 'savetofile', @method_savetofile, classTable);
  RegisterMethod(L, 'sort', @method_sort, classTable);
end;

const
  ObjName = 'ctk_stringlist';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TCatarinkaStrList.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, ObjName, p);
end;

procedure RegisterCatarinkaStrList(L: PLua_State);
begin
  RegisterTLuaObject(L, ObjName, @Create, @register_methods);
end;

constructor TCatarinkaStrList.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TStringList.Create;
end;

function TCatarinkaStrList.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'count') = 0 then
    result := obj.Count
  else if CompareText(propName, 'text') = 0 then
    result := obj.Text
  else
    result := inherited GetPropValue(propName);
end;

function TCatarinkaStrList.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
  if CompareText(propName, 'text') = 0 then
    obj.Text := AValue
  else
    result := inherited SetPropValue(propName, AValue);
end;

destructor TCatarinkaStrList.Destroy;
begin
  obj.Free;
  inherited Destroy;
end;

end.
