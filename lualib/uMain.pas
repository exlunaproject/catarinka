unit uMain;

{
  Catarinka Lua Extension Library
  Copyright (c) 2013-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShellAPI,
  System.TypInfo,
{$ELSE}
  Windows, Classes, SysUtils, ShellAPI, TypInfo,
{$ENDIF}
  Lua;

function RegisterCatarinka(L: plua_State): integer; cdecl;

implementation

uses
  pLua, pLuaTable, uFunctions, uStrList, uStrListParser, uHTMLParser, uJSON,
  uTarman, CatStrings;

type
  TRegisterMethod = procedure(L: plua_State);

procedure push_catarinka_class(L: plua_State; name: string;
  regmethod: TRegisterMethod);
begin
  lua_pushstring(L, name);
  lua_rawget(L, LUA_GLOBALSINDEX);
  if lua_istable(L, -1) = false then
  begin
    regmethod(L); // register class if not registered yet
    lua_pushstring(L, name);
    lua_rawget(L, LUA_GLOBALSINDEX);
  end;
end;

type
  TStringFields = (s_after, s_before, s_beginswith, s_between, s_decrease,
    s_endswith, s_gettoken, s_increase, s_ishex, s_isint, s_lastchar, s_list,
    s_loop, s_match, s_maxlen, s_occur, s_random, s_replace, s_replacefirst,
    s_stripblanklines, s_stripquotes, s_trim);

function get_string(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TStringFields(GetEnumValue(TypeInfo(TStringFields),
    's_' + lowercase(lua_tostring(L, 2)))) of
    s_after:
      lua_pushcfunction(L, str_after);
    s_before:
      lua_pushcfunction(L, str_before);
    s_beginswith:
      lua_pushcfunction(L, str_beginswith);
    s_between:
      lua_pushcfunction(L, str_between);
    s_decrease:
      lua_pushcfunction(L, str_decrease);
    s_endswith:
      lua_pushcfunction(L, str_endswith);
    s_gettoken:
      lua_pushcfunction(L, str_gettoken);
    s_increase:
      lua_pushcfunction(L, str_increase);
    s_ishex:
      lua_pushcfunction(L, str_ishex);
    s_isint:
      lua_pushcfunction(L, str_isint);
    s_lastchar:
      lua_pushcfunction(L, str_lastchar);
    s_list:
      push_catarinka_class(L, 'ctk_stringlist', RegisterCatarinkaStrList);
    s_loop:
      push_catarinka_class(L, 'ctk_listparser', RegisterCatarinkaStrListParser);
    s_match:
      lua_pushcfunction(L, str_wildmatch);
    s_maxlen:
      lua_pushcfunction(L, str_maxlen);
    s_occur:
      lua_pushcfunction(L, str_occur);
    s_random:
      lua_pushcfunction(L, str_random);
    s_replace:
      lua_pushcfunction(L, str_replace);
    s_replacefirst:
      lua_pushcfunction(L, str_replace_first);
    s_stripblanklines:
      lua_pushcfunction(L, str_stripblanklines);
    s_stripquotes:
      lua_pushcfunction(L, str_stripquotes);
    s_trim:
      lua_pushcfunction(L, str_trim);
  else
    result := 0;
  end;
end;

type
  TJSONFields = (j_object);

function get_jsonfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TJSONFields(GetEnumValue(TypeInfo(TJSONFields),
    'j_' + lowercase(lua_tostring(L, 2)))) of
    j_object:
      push_catarinka_class(L, 'ctk_json', RegisterCatarinkaJSON);
  else
    result := 0;
  end;
end;

type
  THTMLFields = (h_beautifycss, h_beautifyjs, h_escape, h_gettagcontents,
    h_parser, h_striptags, h_unescape);

function get_htmlfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case THTMLFields(GetEnumValue(TypeInfo(THTMLFields),
    'h_' + lowercase(lua_tostring(L, 2)))) of
    h_beautifycss:
      lua_pushcfunction(L, str_beautifycss);
    h_beautifyjs:
      lua_pushcfunction(L, str_beautifyjs);
    h_escape:
      lua_pushcfunction(L, html_escape);
    h_gettagcontents:
      lua_pushcfunction(L, str_extracttagcontent);
    h_parser:
      push_catarinka_class(L, 'ctk_htmlparser', RegisterCatarinkaHTMLParser);
    h_striptags:
      lua_pushcfunction(L, html_striptags);
    h_unescape:
      lua_pushcfunction(L, html_unescape);
  else
    result := 0;
  end;
end;

type
  TB64Fields = (b64_encode, b64_decode);

function get_base64fields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TB64Fields(GetEnumValue(TypeInfo(TB64Fields),
    'b64_' + lowercase(lua_tostring(L, 2)))) of
    b64_encode:
      lua_pushcfunction(L, str_base64enc);
    b64_decode:
      lua_pushcfunction(L, str_base64dec);
  else
    result := 0;
  end;
end;

type
  TConvFields = (c_commastrtostr, c_hextoint, c_hextostr, c_inttohex,
    c_strtoalphanum, c_strtohex, c_strtocommastr);

function get_convertfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TConvFields(GetEnumValue(TypeInfo(TConvFields),
    'c_' + lowercase(lua_tostring(L, 2)))) of
    c_commastrtostr:
      lua_pushcfunction(L, conv_commatexttostr);
    c_hextoint:
      lua_pushcfunction(L, conv_hextoint);
    c_hextostr:
      lua_pushcfunction(L, conv_hextostr);
    c_inttohex:
      lua_pushcfunction(L, conv_inttohex);
    c_strtoalphanum:
      lua_pushcfunction(L, str_toalphanum);
    c_strtohex:
      lua_pushcfunction(L, conv_strtohex);
    c_strtocommastr:
      lua_pushcfunction(L, conv_strtocommatext);
  else
    result := 0;
  end;
end;

type
  TCryptoFields = (k_md5, k_sha1);

function get_cryptofields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TCryptoFields(GetEnumValue(TypeInfo(TCryptoFields),
    'k_' + lowercase(lua_tostring(L, 2)))) of
    k_md5:
      lua_pushcfunction(L, conv_strtomd5);
    k_sha1:
      lua_pushcfunction(L, conv_strtosha1);
  else
    result := 0;
  end;
end;

type
  TDirFields = (d_create, d_delete, d_exists, d_getdirlist, d_getfilelist,
    d_packtotar, d_unpackfromtar);

function get_dirfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TDirFields(GetEnumValue(TypeInfo(TDirFields),
    'd_' + lowercase(lua_tostring(L, 2)))) of
    d_create:
      lua_pushcfunction(L, file_mkdir);
    d_delete:
      lua_pushcfunction(L, file_deldir);
    d_exists:
      lua_pushcfunction(L, file_direxists);
    d_getdirlist:
      lua_pushcfunction(L, file_getdirs);
    d_getfilelist:
      lua_pushcfunction(L, file_getdirfiles);
    d_packtotar:
      lua_pushcfunction(L, Lua_DirToTAR);
    d_unpackfromtar:
      lua_pushcfunction(L, Lua_TARToDir);
  else
    result := 0;
  end;
end;

type
  TFileFields = (f_canopen, f_cleanname, f_copy, f_delete, f_exec, f_exechide, f_exists,
    f_getcontents, f_getname, f_getext, f_getsize, f_getver, f_getdir);

function get_filefields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TFileFields(GetEnumValue(TypeInfo(TFileFields),
    'f_' + lowercase(lua_tostring(L, 2)))) of
    f_canopen:
      lua_pushcfunction(L, file_canopen);
    f_cleanname:
      lua_pushcfunction(L, file_cleanname);
    f_copy:
      lua_pushcfunction(L, file_copy);
    f_delete:
      lua_pushcfunction(L, file_delete);
    f_exec:
      lua_pushcfunction(L, file_exec);
    f_exechide:
      lua_pushcfunction(L, file_exechidden);
    f_exists:
      lua_pushcfunction(L, file_exists);
    f_getcontents:
      lua_pushcfunction(L, file_gettostr);
    f_getdir:
      lua_pushcfunction(L, file_extractdir);
    f_getname:
      lua_pushcfunction(L, file_extractname);
    f_getsize:
      lua_pushcfunction(L, file_getsize);
    f_getext:
      lua_pushcfunction(L, file_getext);
    f_getver:
      lua_pushcfunction(L, file_getversion);
  else
    result := 0;
  end;
end;

type
  THttpFields = (ht_crackrequest, ht_getheader, ht_postdatatojson);

function get_httpfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case THttpFields(GetEnumValue(TypeInfo(THttpFields),
    'ht_' + lowercase(lua_tostring(L, 2)))) of
    ht_crackrequest:
      lua_pushcfunction(L, http_crackrequest);
    ht_getheader:
      lua_pushcfunction(L, http_gethdrfield);
    ht_postdatatojson:
      lua_pushcfunction(L, http_postdatatojson);
  else
    result := 0;
  end;
end;

type
  TNetFields = (n_nametoip, n_iptoname);

function get_netfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TNetFields(GetEnumValue(TypeInfo(TNetFields),
    'n_' + lowercase(lua_tostring(L, 2)))) of
    n_nametoip:
      lua_pushcfunction(L, net_nametoip);
    n_iptoname:
      lua_pushcfunction(L, net_iptoname);
  else
    result := 0;
  end;
end;

type
  TTaskFields = (tm_isrunning, tm_kill);

function get_taskfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TTaskFields(GetEnumValue(TypeInfo(TTaskFields),
    'tm_' + lowercase(lua_tostring(L, 2)))) of
    tm_isrunning:
      lua_pushcfunction(L, task_isrunning);
    tm_kill:
      lua_pushcfunction(L, task_kill);
  else
    result := 0;
  end;
end;

type
  TUrlFields = (u_changepath, u_combine, u_crack, u_decode, u_encode,
    u_encodefull, u_fileurltofilename, u_genfromhost, u_getfileext,
    u_getfilename, u_gettiny);

function get_urlfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TUrlFields(GetEnumValue(TypeInfo(TUrlFields),
    'u_' + lowercase(lua_tostring(L, 2)))) of
    u_changepath:
      lua_pushcfunction(L, url_changepath);
    u_combine:
      lua_pushcfunction(L, url_combine);
    u_crack:
      lua_pushcfunction(L, url_crack);
    u_decode:
      lua_pushcfunction(L, url_decode);
    u_encode:
      lua_pushcfunction(L, url_encode);
    u_encodefull:
      lua_pushcfunction(L, url_encodefull);
    u_fileurltofilename:
      lua_pushcfunction(L, file_fileurltofilename);
    u_genfromhost:
      lua_pushcfunction(L, hostporttourl);
    u_getfileext:
      lua_pushcfunction(L, url_getfileext);
    u_getfilename:
      lua_pushcfunction(L, url_getfilename);
    u_gettiny:
      lua_pushcfunction(L, url_gettiny);
  else
    result := 0;
  end;
end;

type
  TUtilFields = (ut_delay, ut_getarg, ut_hasarg, ut_hassoftware, ut_clipboard_gettext, ut_clipboard_settext);

function get_utilfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TUtilFields(GetEnumValue(TypeInfo(TUtilFields),
    'ut_' + lowercase(lua_tostring(L, 2)))) of
    ut_delay:
      lua_pushcfunction(L, utils_delay);
    ut_getarg:
      lua_pushcfunction(L, utils_getarg);
    ut_hasarg:
      lua_pushcfunction(L, utils_hasarg);
    ut_hassoftware:
      lua_pushcfunction(L, utils_hassoftwareinstalled);
    ut_clipboard_gettext:
      lua_pushcfunction(L, utils_clipboard_gettext);
    ut_clipboard_settext:
      lua_pushcfunction(L, utils_clipboard_settext);
  else
    result := 0;
  end;
end;

type
  TRegExFields = (re_find, re_match, re_replace);

function get_regexfields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TRegExFields(GetEnumValue(TypeInfo(TRegExFields),
    're_' + lowercase(lua_tostring(L, 2)))) of
    re_find:
      lua_pushcfunction(L, str_regexfind);
    re_match:
      lua_pushcfunction(L, str_regexmatch);
    re_replace:
      lua_pushcfunction(L, str_regexreplace);
  else
    result := 0;
  end;
end;

type
  TConsoleFields = (cs_printgreen, cs_printred, cs_printwhite);

function get_consolefields(L: plua_State): integer; cdecl;
begin
  result := 1;
  case TConsoleFields(GetEnumValue(TypeInfo(TConsoleFields),
    'cs_' + lowercase(lua_tostring(L, 2)))) of
    cs_printgreen:
      lua_pushcfunction(L, console_printgreen);
    cs_printred:
      lua_pushcfunction(L, console_printred);
    cs_printwhite:
      lua_pushcfunction(L, console_printwhite);
  else
    result := 0;
  end;
end;

function RegisterCatarinka(L: plua_State): integer; cdecl;
begin
  lua_newtable(L);
  plua_SetFieldValue(L, 'base64', @get_base64fields, nil);
  plua_SetFieldValue(L, 'convert', @get_convertfields, nil);
  plua_SetFieldValue(L, 'crypto', @get_cryptofields, nil);
  plua_SetFieldValue(L, 'cs', @get_consolefields, nil);
  plua_SetFieldValue(L, 'dir', @get_dirfields, nil);
  plua_SetFieldValue(L, 'file', @get_filefields, nil);
  plua_SetFieldValue(L, 'html', @get_htmlfields, nil);
  plua_SetFieldValue(L, 'http', @get_httpfields, nil);
  plua_SetFieldValue(L, 'json', @get_jsonfields, nil);
  plua_SetFieldValue(L, 'net', @get_netfields, nil);
  plua_SetFieldValue(L, 'task', @get_taskfields, nil);
  plua_SetFieldValue(L, 'url', @get_urlfields, nil);
  plua_SetFieldValue(L, 'utils', @get_utilfields, nil);
  plua_SetFieldValue(L, 're', @get_regexfields, nil);
  plua_SetFieldValue(L, 'string', @get_string, nil);
  result := 1;
end;

end.
