unit CtkTimer;

{
  Catarinka Lua Timeout Library
  Copyright (c) 2013-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  CatCSTimer,
{$ELSE}
  Classes,
{$ENDIF}
  Lua, pLua;

type
  TCatarinkaLuaTimer = class(TConsoleTimer)
  private
    fLuaState:plua_State;
    procedure OnTimer(Sender: TObject);
  public
    func: lua_CFunction;
    mode: integer;
    argcount: integer;
    script: string;
    args : array of TLuaVariantRec;
    constructor Create(L:plua_State);
  end;

function utils_settimeout(L: plua_State): integer; cdecl;

implementation

procedure TCatarinkaLuaTimer.OnTimer(Sender: TObject);
var
  i:integer;
begin
  case mode of
    LUA_TFUNCTION: begin
      lua_pushcfunction(fLuaState, func);
      for i := Low(args) to High(args) do begin
        if args[i].LuaType <> LUA_TNONE then
        plua_pushvariant(fLuaState, args[i].Value);
      end;
      lua_pcall(fLuaState, argcount, 0, 0);
    end;
    LUA_TSTRING: begin
      plua_dostring(fLuaState, script);
    end;
  end;
  Enabled := false;
  Free;
end;

constructor TCatarinkaLuaTimer.Create(L:plua_State);
begin
  inherited Create;
  fLuaState := L;
  OnTimerEvent := OnTimer;
end;

function utils_settimeout(L: plua_State): integer; cdecl;
var
  t:TCatarinkaLuaTimer;
begin
  t := TCatarinkaLuaTimer.Create(L);
  if (lua_type(L, 1) = LUA_TNUMBER) and (lua_type(L, 2) = LUA_TFUNCTION) then begin
     t.argcount := lua_gettop(L) - 2;
     t.mode := LUA_TFUNCTION;
     t.Interval := lua_tointeger(L, 1);
     t.func := lua_tocfunction(L, 2);
     // max 3 optional arguments allowed
     t.args := [plua_tovariantrec(L, 3), plua_tovariantrec(L, 4), plua_tovariantrec(L, 5)];
     t.Enabled := true;
  end else begin
    if plua_validateargs(L, result, [LUA_TNUMBER, LUA_TSTRING]).OK = true then begin
     t.mode := LUA_TSTRING;
     t.Interval := lua_tointeger(L, 1);
     t.script := lua_tostring(L, 2);
     t.Enabled := true;
    end;
  end;
end;

{function utils_settimeout_oldcallback(L: plua_State): integer; cdecl;
var
  script:string;
  func: lua_CFunction;
  t:TConsoleTimer;
begin
  if plua_validateargsets(L, result, [[LUA_TNUMBER], [LUA_TSTRING, LUA_TFUNCTION]]).OK = false then
    Exit;
  t := TConsoleTimer.Create;
  t.Interval := lua_tointeger(L, 1);
  t.Enabled := true;

  if lua_type(L, 2) = LUA_TFUNCTION then begin
    func := lua_tocfunction(L, 2);
    t.OnTimerCallBack := procedure()
     begin
       lua_pushcfunction(L, func);
       lua_pcall(L, 0, 0, 0);
       t.Enabled := false;
       t.Free;
     end;
  end else begin
    script := lua_tostring(L, 2);
    t.OnTimerCallBack := procedure()
     begin
       plua_dostring(L, script);
       t.Enabled := false;
       t.Free;
     end;
  end;
end; }

end.  