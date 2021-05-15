unit CtkStrList;

{
  Catarinka Lua Library - String List Object
  Copyright (c) 2013-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, SysUtils, Lua, pLua, LuaObject, CatUtils, CatStrings, CatStringList;

type
  { TCatarinkaStrList }
  TCatarinkaStrList = class(TLuaObject)
  private
  public
    obj: TCatStringList;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

function RegisterCatarinkaStrList(L: PLua_State):TLuaObjectRegResult;

implementation

function method_add(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
    ht.obj.add(lua_tostring(L, 2));
end;

function method_insert(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TNUMBER, LUA_TSTRING]).OK then
   ht.obj.insert(lua_tointeger(L, 2), lua_tostring(L, 3));
end;

function method_delete(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TNUMBER]).OK then
    ht.obj.delete(lua_tointeger(L, 2));
end;

function method_savetofile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
    ht.obj.savetofile(lua_tostring(L, 2));
end;

function method_loadfromfile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
    ht.obj.loadfromfile(lua_tostring(L, 2));
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
  i: integer;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then begin
    i := ht.obj.indexof(lua_tostring(L, 2));
    plua_pushintnumber(L, i);
  end;
end;

function method_getvalue(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
  ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
   lua_pushstring(L, ht.obj.values[lua_tostring(L, 2)]);
end;

function method_getstringfromindex(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrList;
begin
 ht := TCatarinkaStrList(LuaToTLuaObject(L, 1));
 if plua_validatemethodargs(L, result, [LUA_TNUMBER]).OK then
  lua_pushstring(L, ht.obj[lua_tointeger(L, 2)]);
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, 'add', @method_add, classTable);
  RegisterMethod(L, 'insert', @method_insert, classTable);
  RegisterMethod(L, 'indexof', @method_indexof, classTable);
  RegisterMethod(L, 'delete', @method_delete, classTable);
  RegisterMethod(L, 'clear', @method_clear, classTable);
  RegisterMethod(L, 'get', @method_getstringfromindex, classTable);
  RegisterMethod(L, 'getvalue', @method_getvalue, classTable);
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

function RegisterCatarinkaStrList(L: PLua_State):TLuaObjectRegResult;
begin
  result := RegisterTLuaObjectAlt(L, ObjName, @Create, @register_methods);
end;

constructor TCatarinkaStrList.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TCatStringList.Create;
end;

const
 _commatext = 1;
 _count = 2;
 _text = 3;
const
 cProps : array [1..3] of TCatCaseLabel =
 (
   (name:'commatext';id:_commatext),
   (name:'count';id:_count),
   (name:'text';id:_text)
 );

function TCatarinkaStrList.GetPropValue(propName: String): Variant;
begin
   case CatCaseLabelOf(propname,cprops) of
    _commatext: result := obj.CommaText;
    _count: result := obj.Count;
    _text: result := obj.Text;
   else
    result := inherited GetPropValue(propName);
   end;
end;

function TCatarinkaStrList.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
   case CatCaseLabelOf(propname,cprops) of
    _commatext: obj.CommaText := AValue;
    _text: obj.Text := AValue;
   else
    result := inherited SetPropValue(propName, AValue);
   end;
end;

destructor TCatarinkaStrList.Destroy;
begin
  obj.Free;
  inherited Destroy;
end;

end.
