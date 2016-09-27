unit uHTMLParser;

{
  Catarinka Lua Library - HTML Parser Object
  Copyright (c) 2013-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, SysUtils, Lua, LuaObject, CatHTMLParser;

type
  { TCatarinkaHTMLParser }
  TCatarinkaHTMLParser = class(TLuaObject)
  private
  public
    obj: TCatHTMLParser;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterCatarinkaHTMLParser(L: PLua_State);

implementation

uses pLua, CatStrings;

function method_parsing(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  lua_pushboolean(L, ht.obj.NextTag);
  result := 1;
end;

function method_clear(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  ht.obj.text := '';
  result := 1;
end;

function method_stop(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  ht.obj.GotoEnd;
  result := 1;
end;

function method_reset(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  ht.obj.GotoBeginning;
  result := 1;
end;

function method_getattrib(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
  s: string;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  s := ht.obj.tag.params.values[lua_tostring(L, 2)];
  s := removequotes(s);
  lua_pushstring(L, s);
  result := 1;
end;

function method_setattrib(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  ht.obj.tag.params.values[lua_tostring(L, 2)] := lua_tostring(L, 3);
  result := 1;
end;

function method_loadfromstr(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  ht.obj.text := lua_tostring(L, 2);
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, 'load', @method_loadfromstr, classTable);
  RegisterMethod(L, 'parsing', @method_parsing, classTable);
  RegisterMethod(L, 'stop', @method_stop, classTable);
  RegisterMethod(L, 'reset', @method_reset, classTable);
  RegisterMethod(L, 'clear', @method_clear, classTable);
  RegisterMethod(L, 'getattrib', @method_getattrib, classTable);
  RegisterMethod(L, 'setattrib', @method_setattrib, classTable);
end;

const
  objname = 'ctk_htmlparser';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TCatarinkaHTMLParser.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, objname, p);
end;

procedure RegisterCatarinkaHTMLParser(L: PLua_State);
begin
  RegisterTLuaObject(L, objname, @Create, @register_methods);
end;

constructor TCatarinkaHTMLParser.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TCatHTMLParser.Create;
end;

function TCatarinkaHTMLParser.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'pos') = 0 then
    result := obj.Pos
  else if CompareText(propName, 'tagpos') = 0 then
    result := obj.TagPos
  else if CompareText(propName, 'tagline') = 0 then
    result := obj.TagLine
  else if CompareText(propName, 'tagcontent') = 0 then
    result := obj.TextBetween
  else if CompareText(propName, 'tagname') = 0 then
    result := lowercase(obj.tag.Name)
  else
    result := inherited GetPropValue(propName);
end;

function TCatarinkaHTMLParser.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true; // 2013
  if CompareText(propName, 'tagcontent') = 0 then
    obj.TextBetween := AValue
  else if CompareText(propName, 'tagname') = 0 then
    obj.tag.Name := AValue
  else
    result := inherited SetPropValue(propName, AValue);
end;

destructor TCatarinkaHTMLParser.Destroy;
begin
  obj.free;
  inherited Destroy;
end;

end.
