unit CatLuaObject;

{
  pLua's LuaObject.pas fork
 
  Copyright (c) 2010-2014 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same as the original code by Jeremy Darling.
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses

{$IF CompilerVersion >= 23} // XE2 or higher
  System.SysUtils, System.Classes, System.Variants,
{$ELSE}
  Classes, SysUtils, Variants,
{$IFEND}
  Lua, pLuaObject, pLua;

type
  TCatLuaObject = class;

  { TCatLuaObject }

  TCatLuaObject = class
  protected
    L : PLua_State;
    FLuaReference : integer;
    FParent : TCatLuaObject;
    FChildren : TList;
    
    function  GetLuaProp(PropName : AnsiString): Variant;
    procedure SetLuaProp(PropName : AnsiString; const AValue: Variant);
    function  GetPropValue(L : PLua_State; propName : AnsiString): Variant; virtual;
    function  GetPropObject(propName: AnsiString) : Boolean; virtual;
    function  SetPropValue(L : PLua_State; PropName : AnsiString; const AValue: Variant) : Boolean; virtual;
    function  SetPropObject(propName: AnsiString) : Boolean; virtual;
    function  PropIsObject(propName : AnsiString): Boolean; virtual;
    procedure CommonCreate(LuaState : PLua_State; AParent : TCatLuaObject = nil); virtual;
  public
    constructor Create(LuaState : PLua_State; AParent : TCatLuaObject = nil); overload; virtual;
    constructor Create(LuaState: PLua_State; LuaClassName, LuaName: AnsiString); overload; virtual; 
    destructor Destroy; override;

    procedure PushSelf;

    procedure CallEvent(EventName : AnsiString); overload;
    function  CallEvent(EventName : AnsiString; args : Array of Variant; Results: PVariantArray = nil) : Integer; overload;
    function  EventExists(EventName: AnsiString): Boolean;

    property LState : PLua_State read L;
    property LRef:integer read FLuaReference;

    property LuaProp[PropName : AnsiString] : Variant read GetLuaProp write SetLuaProp;
  end;

  TCatLuaObjectRegisterMethodsCallback = procedure(L : Plua_State; classTable : Integer);
  TCatLuaObjectNewCallback = function(L : PLua_State; AParent : TCatLuaObject=nil):TCatLuaObject;

var
  LuaObjects : TList;

procedure ClearObjects;
procedure LuaCopyTable(L: Plua_State; IdxFrom, IdxTo, MtTo : Integer);
function  LuaToTCatLuaObject(L: Plua_State; Idx : Integer) : TCatLuaObject;
procedure RegisterLuaObject(L: Plua_State);

procedure RegisterTCatLuaObject(L : Plua_State; ObjectName : AnsiString; CreateFunc : lua_CFunction; MethodsCallback : TCatLuaObjectRegisterMethodsCallback = nil);
procedure RegisterObjectInstance(L : Plua_State; aClassName, InstanceName : AnsiString; ObjectInstance : TCatLuaObject);
procedure RegisterMethod(L : Plua_State; TheMethodName : AnsiString; TheMethodAddress : lua_CFunction; classTable : Integer);
function  new_LuaObject(L : PLua_State; aClassName : AnsiString; NewCallback : TCatLuaObjectNewCallback) : Integer; cdecl;

procedure PushTCatLuaObject(L : PLua_State; ObjectInstance : TCatLuaObject);

function  new_TCatLuaObject(L : PLua_State) : Integer; cdecl;
function  index_TCatLuaObject(L : PLua_State) : Integer; cdecl;
function  newindex_TCatLuaObject(L : PLua_State) : Integer; cdecl;
function  gc_TCatLuaObject(L : PLua_State) : Integer; cdecl;
procedure RegisterClassTCatLuaObject(L : Plua_State);

implementation

uses
  typinfo;

const
  LuaTCatLuaObjectClassName = 'TCatLuaObject';

constructor TCatLuaObject.Create(LuaState : PLua_State; AParent : TCatLuaObject = nil);
begin
  CommonCreate(LuaState, nil);
  // Create a reference to the object table, this way lua won't GC its version
  FLuaReference := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference);
  LuaObjects.Add(Self);
end;

constructor TCatLuaObject.Create(LuaState: PLua_State; LuaClassName, LuaName: AnsiString);
begin
  CommonCreate(LuaState, nil);
  RegisterObjectInstance(LuaState, LuaClassName, LuaName, self);
end;

destructor TCatLuaObject.Destroy;
var
  lo : TCatLuaObject;
begin
  LuaObjects.Remove(Self);
  if assigned(FParent) then
    FParent.FChildren.Remove(Self);
  while FChildren.Count > 0 do
    begin
      lo := TCatLuaObject(FChildren[FChildren.Count-1]);
      FChildren.Delete(FChildren.Count-1);
      lo.Free;
    end;
  FChildren.Free;
  luaL_unref(L, LUA_REGISTRYINDEX, FLuaReference);
  inherited Destroy;
end;

procedure TCatLuaObject.PushSelf;
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, FLuaReference);
end;

procedure TCatLuaObject.CallEvent(EventName: AnsiString);
begin
  CallEvent(EventName, []);
end;

function TCatLuaObject.CallEvent(EventName : AnsiString; args: array of Variant; Results: PVariantArray) : Integer;
begin
  result := -1;
  if not EventExists(EventName) then
    exit;
  PushSelf;
  result := plua_callfunction(L, EventName, args, results, lua_gettop(L));
end;

function TCatLuaObject.EventExists(EventName: AnsiString): Boolean;
begin
  PushSelf;
  result := plua_functionexists(L, EventName, lua_gettop(L));
  lua_pop(L, 1);
end;

function TCatLuaObject.GetLuaProp(PropName : AnsiString): Variant;
var
  idx : Integer;
begin
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference); // Place our object on the stack
  idx := lua_gettop(L);
  lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(PropName)); // Place the event name on the stack
  lua_gettable(L, idx); // try to get the item
  result := plua_tovariant(L, lua_gettop(L));
  lua_pop(L, 2);
end;

procedure TCatLuaObject.SetLuaProp(PropName : AnsiString; const AValue: Variant);
var
  idx : Integer;
begin
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference); // Place our object on the stack
  idx := lua_gettop(L);
  lua_pushstring(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(propName));
  plua_pushvariant(L, AValue);
  lua_rawset(L, idx);
end;

function TCatLuaObject.GetPropValue(L : PLua_State; propName: AnsiString): Variant;
begin
  if IsPublishedProp(self, propName) then
    result := typinfo.GetPropValue(self, propName)
  else
    result := NULL;
end;

function TCatLuaObject.GetPropObject(propName: AnsiString) : Boolean;
begin
 result := false;
end;

function TCatLuaObject.SetPropValue(L : PLua_State; PropName: AnsiString; const AValue: Variant) : Boolean;
begin
  result := IsPublishedProp(self, propName);
  if result then
    typinfo.SetPropValue(self, propName, AValue);
end;

function TCatLuaObject.SetPropObject(propName: AnsiString) : Boolean;
begin
  result := false;
end;

function TCatLuaObject.PropIsObject(propName: AnsiString): Boolean;
begin
  result := false;
end;

procedure TCatLuaObject.CommonCreate(LuaState: PLua_State; AParent: TCatLuaObject);
begin
  L := LuaState;
  FParent := AParent;
  if assigned(FParent) then
    FParent.FChildren.Add(Self);
  FChildren := TList.Create;
end;

{ Global LUA Methods }

procedure LuaCopyTable(L: Plua_State; IdxFrom, IdxTo, MtTo : Integer);
var
  id:Integer;
  tbl : Integer;
  key, val : Variant;
  cf : lua_CFunction;
begin
  lua_pushnil(L);
  while(lua_next(L, IdxFrom)<>0)do
    begin
      key := plua_tovariant(L, -2);
      if CompareText(key, '__') = 1 then
        tbl := MtTo
      else
        tbl := IdxTo;
      case lua_type(L, -1) of
        LUA_TFUNCTION : begin
          cf := lua_tocfunction(L, -1);
          plua_pushvariant(L, key);
          lua_pushcfunction(L, cf);
          lua_rawset(L, tbl);
        end;
        LUA_TTABLE    : begin
          id := lua_gettop(L);
          LuaCopyTable(L, id, IdxTo, MtTo);
        end;
      else
        val := plua_tovariant(L, -1);
        plua_pushvariant(L, key);
        plua_pushvariant(L, val);
        lua_rawset(L, tbl);
      end;
      lua_pop(L, 1);
    end;
end;

function LuaToTCatLuaObject(L: Plua_State; Idx : Integer) : TCatLuaObject;
begin
  result := nil;
  if lua_type(L, Idx) = LUA_TTABLE then
    begin
      Idx := plua_absindex(L, Idx);
      lua_pushstring(L, '_Self');
      lua_gettable(L, Idx);
      result := TCatLuaObject(ptrint(lua_tointeger(L, -1)));
      lua_pop(L, 1);
    end
  else
    luaL_error(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}('Class table expected.'));
end;

procedure PushTCatLuaObject(L: PLua_State; ObjectInstance: TCatLuaObject);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ObjectInstance.FLuaReference);
end;

function new_TCatLuaObject(L : PLua_State) : Integer; cdecl;
var
  P, E : TCatLuaObject;
  n, idx, idx2, mt : Integer;
begin
  n := lua_gettop(L);
  if lua_type(L, 1) <> LUA_TTABLE then
    lua_remove(L, 1);
  if n = 1 then
    P := LuaToTCatLuaObject(L, 1)
  else
    P := nil;
    
  lua_newtable(L);
  E := TCatLuaObject.Create(L, P);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(E)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, LuaTCatLuaObjectClassName);
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);
  
  lua_pop(L, 1);

  result := 1;
end;

function index_TCatLuaObject(L : PLua_State) : Integer; cdecl;
var
  E : TCatLuaObject;
  propName : AnsiString;
  v : Variant;
begin
  E := LuaToTCatLuaObject(L, 1);
  lua_remove(L, 1);
  if E = nil then
    begin
      result := 0;
      exit;
    end;
  propName := plua_tostring(L, 1);
  index_TCatLuaObject := 1;
  if E.PropIsObject(propName) then
    begin
      if not E.GetPropObject(propName) then
        index_TCatLuaObject := 0;
    end
  else
    begin
      v := E.GetPropValue(L,propName);
      if v = NULL then
        index_TCatLuaObject := 0
      else
        plua_pushvariant(L, v);
    end;
end;

function newindex_TCatLuaObject(L : PLua_State) : Integer; cdecl;
var
  TableIndex, ValueIndex : Integer;
  E : TCatLuaObject;
  propName : AnsiString;
begin
  result := 0;
  E := LuaToTCatLuaObject(L, 1);
  if E = nil then
    begin
      exit;
    end;
  propName := plua_tostring(L, 2);
  if E.PropIsObject(propName) and E.SetPropObject(propName) then
  else if not E.SetPropValue(L,propName, plua_tovariant(L, 3)) then
    begin
    // This is a standard handler, no value was found in the object instance
    // so we push the value into the Lua Object reference.
      TableIndex := plua_absindex(L, 1);
      ValueIndex := plua_absindex(L, 3);
      lua_pushstring(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(propName));
      lua_pushvalue(L, ValueIndex);
      lua_rawset(L, TableIndex);
    end;
end;

function gc_TCatLuaObject(L : PLua_State) : Integer; cdecl;
var
  E : TCatLuaObject;
begin
  E := LuaToTCatLuaObject(L, 1);
  // Release the object
  if assigned(E) then
    E.Free;
  result := 0;
end;

procedure RegisterObjectInstance(L: Plua_State; aClassName, InstanceName: AnsiString; ObjectInstance : TCatLuaObject);
var
  idx, idx2, mt : Integer;
begin
  lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(InstanceName));
  lua_newtable(L);

  ObjectInstance.FLuaReference := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti (L, LUA_REGISTRYINDEX, ObjectInstance.FLuaReference);
  LuaObjects.Add(ObjectInstance);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(ObjectInstance)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(aClassName));
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);

  lua_pop(L, 1);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure RegisterMethod(L : Plua_State; TheMethodName : AnsiString; TheMethodAddress : lua_CFunction; classTable : Integer);
begin
  lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(TheMethodName));
  lua_pushcfunction(L, TheMethodAddress);
  lua_rawset(L, classTable);
end;

function new_LuaObject(L : PLua_State; aClassName : AnsiString; NewCallback : TCatLuaObjectNewCallback): Integer; cdecl;
var
  P, E : TCatLuaObject;
  n, idx, idx2, mt : Integer;
begin
  n := lua_gettop(L);
  if lua_type(L, 1) <> LUA_TTABLE then
    lua_remove(L, 1);
  if n > 1 then
    P := LuaToTCatLuaObject(L, 2)
  else
    P := nil;

  lua_newtable(L);
  E := NewCallback(L, P);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(E)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(aClassName));
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);

  lua_pop(L, 1);

  result := 1;
end;

procedure RegisterClassTCatLuaObject(L : Plua_State);
var
  classTable : Integer;
begin
  lua_pushstring(L, LuaTCatLuaObjectClassName);
  lua_newtable(L);
  classTable := lua_gettop(L);

  RegisterMethod(L, '__index', @index_TCatLuaObject, classTable);
  RegisterMethod(L, '__newindex', @newindex_TCatLuaObject, classTable);
  RegisterMethod(L, '__call', @new_TCatLuaObject, classTable);
  RegisterMethod(L, '__gc', @gc_TCatLuaObject, classTable);
  RegisterMethod(L, 'release', @gc_TCatLuaObject, classTable);
  RegisterMethod(L, 'new', @new_TCatLuaObject, classTable);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

{ Global Management Methods }

procedure RegisterTCatLuaObject(L: Plua_State; ObjectName : AnsiString;
  CreateFunc : lua_CFunction;
  MethodsCallback: TCatLuaObjectRegisterMethodsCallback);
var
  classTable : Integer;
begin
  lua_pushstring(L, {$IFDEF UNICODE}pAnsiChar{$ELSE}PChar{$ENDIF}(ObjectName));
  lua_newtable(L);
  classTable := lua_gettop(L);

  RegisterMethod(L, '__index', @index_TCatLuaObject, classTable);
  RegisterMethod(L, '__newindex', @newindex_TCatLuaObject, classTable);
  RegisterMethod(L, '__call', CreateFunc, classTable);
  RegisterMethod(L, '__gc', @gc_TCatLuaObject, classTable);
  RegisterMethod(L, 'release', @gc_TCatLuaObject, classTable);
  RegisterMethod(L, 'new', CreateFunc, classTable);

  if Assigned(MethodsCallback) then
    MethodsCallback(L, classTable);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure ClearObjects;
begin
  while LuaObjects.Count > 0 do
    TCatLuaObject(LuaObjects[LuaObjects.Count-1]).Free;
end;

procedure RegisterLuaObject(L: Plua_State);
begin
  RegisterClassTCatLuaObject(L);
end;

initialization
  LuaObjects := TList.Create;

finalization
  ClearObjects;
  LuaObjects.Free;

end.
