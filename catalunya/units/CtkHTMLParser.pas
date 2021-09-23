unit CtkHTMLParser;

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

function RegisterCatarinkaHTMLParser(L: PLua_State):TLuaObjectRegResult;

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
  ht.obj.text := emptystr;
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
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then begin
   s := ht.obj.tag.params.values[lua_tostring(L, 2)];
   s := removequotes(s);
   lua_pushstring(L, s);
  end;
end;

function method_setattrib(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   ht.obj.tag.params.values[lua_tostring(L, 2)] := lua_tostring(L, 3);
end;

function method_loadfromstr(L: PLua_State): Integer; cdecl;
var
  ht: TCatarinkaHTMLParser;
begin
  ht := TCatarinkaHTMLParser(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
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

function RegisterCatarinkaHTMLParser(L: PLua_State):TLuaObjectRegResult;
begin
  Result := RegisterTLuaObjectAlt(L, objname, @Create, @register_methods);
end;

constructor TCatarinkaHTMLParser.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TCatHTMLParser.Create;
end;

const
 _pos = 1;
 _tagpos = 2;
 _tagline = 3;
 _tagcontent = 4;
 _tagname = 5;
const
 cProps : array [1..5] of TCatCaseLabel =
 (
   (name:'pos';id:_pos),
   (name:'tagpos';id:_tagpos),
   (name:'tagline';id:_tagline),
   (name:'tagcontent';id:_tagcontent),
   (name:'tagname';id:_tagname)
 );

function TCatarinkaHTMLParser.GetPropValue(propName: String): Variant;
begin
   case CatCaseLabelOf(propname,cprops) of
    _pos: result := obj.Pos;
    _tagpos: result := obj.TagPos;
    _tagline: result := obj.TagLine;
    _tagcontent: result := obj.TextBetween;
    _tagname: result := lowercase(obj.tag.Name);
   else
    result := inherited GetPropValue(propName);
   end;
end;

function TCatarinkaHTMLParser.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
   result := true;
   case CatCaseLabelOf(propname,cprops) of
    _tagcontent: obj.TextBetween := AValue;
    _tagname: obj.tag.Name := AValue;
   else
    result := inherited SetPropValue(propName, AValue);
   end;
end;

destructor TCatarinkaHTMLParser.Destroy;
begin
  obj.free;
  inherited Destroy;
end;

end.
