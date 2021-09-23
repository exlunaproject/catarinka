library Catalunya;

{
  Catalunya Lua (LuaJIT) Library
  Copyright (c) 2013-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

uses
  Lua,
  CtkCore in '..\units\CtkCore.pas',
  CtkFunctions in '..\units\CtkFunctions.pas',
  CtkHTMLParser in '..\units\CtkHTMLParser.pas',
  CtkJSON in '..\units\CtkJSON.pas',
  CtkStrList in '..\units\CtkStrList.pas',
  CtkStrListParser in '..\units\CtkStrListParser.pas',
  CtkTarman in '..\units\CtkTarman.pas',
  CtkTimer in '..\units\CtkTimer.pas';


{$I CatCompactLib.inc}

{$R *.res}

function luaopen_Catalunya(L: plua_State): integer; cdecl;
begin
  Result := RegisterCatalunya(L);
end;

Exports
  luaopen_Catalunya;

begin

end.
