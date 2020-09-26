unit CtkStrListParser;

{
  Catarinka Lua Library - String List Parser Object
  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, SysUtils, Lua, pLua, LuaObject, CatStrings, CatStringLoop;

type
  { TCatarinkaStrListParser }
  TCatarinkaStrListParser = class(TLuaObject)
  private
  public
    obj: TStringLoop;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

function RegisterCatarinkaStrListParser(L: PLua_State):TLuaObjectRegResult;

implementation

function method_parsing(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  lua_pushboolean(L, ht.obj.found);
  result := 1;
end;

function method_clear(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.clear;
  result := 1;
end;

function method_stop(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.stop;
  result := 1;
end;

function method_reset(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.reset;
  result := 1;
end;

function method_reverse(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.Reverse;
  result := 1;
end;

function method_delete(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.delete;
  result := 1;
end;

function method_add(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.List.add(lua_tostring(L, 2));
  result := 1;
end;

function method_loadfromstr(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.loadfromstring(lua_tostring(L, 2));
  result := 1;
end;

function method_loadfromfile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.loadfromfile(lua_tostring(L, 2));
  result := 1;
end;

function method_savetofile(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  ht.obj.List.savetofile(lua_tostring(L, 2));
  result := 1;
end;

function method_getvalue(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser; sl: TStringList;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  sl := TStringList.Create;
  sl.CommaText := ht.obj.Current;
  lua_pushstring(L, sl.values[lua_tostring(L, 2)]);
  sl.Free;
  result := 1;
end;

function method_indexof(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  plua_pushintnumber(L, ht.obj.indexof(lua_tostring(L, 2)));
  result := 1;
end;

function method_getstringfromindex(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaStrListParser;
begin
  ht := TCatarinkaStrListParser(LuaToTLuaObject(L, 1));
  lua_pushstring(L, ht.obj.list.strings[lua_tointeger(L, 2)]);
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, 'indexof', @method_indexof, classTable);
  RegisterMethod(L, 'get', @method_getstringfromindex, classTable);
  RegisterMethod(L, 'load', @method_loadfromstr, classTable);
  RegisterMethod(L, 'loadfromfile', @method_loadfromfile, classTable);
  RegisterMethod(L, 'parsing', @method_parsing, classTable);
  RegisterMethod(L, 'savetofile', @method_savetofile, classTable);
  RegisterMethod(L, 'stop', @method_stop, classTable);
  RegisterMethod(L, 'reset', @method_reset, classTable);
  RegisterMethod(L, 'reverse', @method_reverse, classTable);
  RegisterMethod(L, 'clear', @method_clear, classTable);
  RegisterMethod(L, 'curgetvalue', @method_getvalue, classTable);
  RegisterMethod(L, 'curdelete', @method_delete, classTable);
  RegisterMethod(L, 'add', @method_add, classTable);
end;

const
  objname = 'ctk_listparser';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TCatarinkaStrListParser.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, objname, p);
end;

function RegisterCatarinkaStrListParser(L: PLua_State):TLuaObjectRegResult;
begin
  Result := RegisterTLuaObjectAlt(L, objname, @Create, @register_methods);
end;

constructor TCatarinkaStrListParser.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TStringLoop.Create;
end;

const
 _commatext = 1;
 _count = 2;
 _current = 3;
 _curindex = 4;
 _text = 5;
const
 cProps : array [1..5] of TCatCaseLabel =
 (
   (name:'commatext';id:_commatext),
   (name:'count';id:_count),
   (name:'current';id:_current),
   (name:'curindex';id:_curindex),
   (name:'text';id:_text)
 );

function TCatarinkaStrListParser.GetPropValue(propName: String): Variant;
begin
   case CatCaseLabelOf(propname,cprops) of
    _commatext: result := obj.List.CommaText;
    _count: result := obj.Count;
    _current: result := obj.Current;
    _curindex: result := obj.Index(false);
    _text: result := obj.List.Text;
   else
    result := inherited GetPropValue(propName);
   end;
end;

function TCatarinkaStrListParser.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
   case CatCaseLabelOf(propname,cprops) of
    _commatext: begin
        obj.List.CommaText := AValue;
        obj.reset;
      end;
    _current: obj.Current := AValue;
    _text: obj.loadfromstring(AValue);
   else
    result := inherited SetPropValue(propName, AValue);
   end;
end;

destructor TCatarinkaStrListParser.Destroy;
begin
  obj.free;
  inherited Destroy;
end;

end.
