unit CatHTTP;
{
  Catarinka - HTTP and HTML related functions
  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  ColorToHTMLColor function by Ralf Mimoun

  4.19.2011, FD: GetHostFromURL, GetPortFromURL,
  GetDomainFromHost, GetPortFromHost are now IPv6 compatible
}

interface

uses
{$IF CompilerVersion >= 23}
  System.Classes, System.SysUtils, Vcl.Graphics;
{$ELSE}
  Classes, SysUtils, Graphics;
{$IFEND}
function BoolToDisplayState(const b: boolean): string;
function ColorToHTMLColor(const Color: TColor): string;
function ExtractUrlFileExt(const url: string): string;
function ExtractUrlFileName(const url: string): string;
function GetDomainFromHost(const host: string): string;
function GetField(const Field, ReqStr: string): ansistring;
function GetHeaderFromResponse(const r: string): string;
function GetHostFromURL(const url: string): string;
function GetPathFromRequest(const r: string): string;
function GetPathFromURL(const url: string;
  const includeparams: boolean = true): string;
function GetPortFromHost(const host: string): string;
function GetPortFromURL(const url: string): string;
function GetPostDataFromRequest(const r: string): string;
function GetStatusCodeFromResponse(const r: string): integer;
function HostPort2URL(const host: string; const port: integer): string;
function HtmlColorToColor(const Color: string): TColor;
function HtmlEntityDecode(const s: string): string;
function HtmlEscape(const s: string): string;
function HtmlUnescape(const s: string): string;
function Path_TitleCase(const s: string): string;
function PostDataToJSON(const s: string): string;
function RemoveHeaderFromResponse(const r: string): string;
function StripHTML(const s: string): string;
function StripPHPCode(const s: string): string;
function URLDecode(const s: string): string;
function URLEncode(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ']): string;
function URLEncodeFull(const s: string): string;

implementation

uses
  CatStrings, CatStringLoop, CatJSON;

function BoolToDisplayState(const b: boolean): string;
begin
  if b then
    result := 'block'
  else
    result := 'none';
end;

// By Ralf Mimoun
function ColorToHTMLColor(const Color: TColor): string;
var
  cl: LongInt;
begin
  cl := ColorToRGB(Color);
  result := format('#%6.6x', [((cl and $FF0000) shr 16) + ((cl and $00FF00)) +
    ((cl and $0000FF) shl 16)]);
end;

// TitleCase for URL paths
function Path_TitleCase(const s: string): string;
var
  i: integer;
begin
  result := s;
  for i := 1 to length(result) - 1 do
    if (result[i] in (['~', '/'] - ['.', '-', 'A' .. 'Z', 'a' .. 'z'])) then
      if result[i + 1] in ['a' .. 'z'] then
        result[i + 1] := Char(ord(result[i + 1]) and not $20);
end;

function PostDataToJSON(const s: string): string;
var
  d: TCatJSON;
  slp: TStringLoop;
  n, v: string;
begin
  d := TCatJSON.Create;
  slp := TStringLoop.Create;
  slp.LoadFromString(replacestr(s, '&', crlf));
  while slp.Found do
  begin
    n := before(slp.current, '=');
    v := after(slp.current, '=');
    v := URLDecode(v);
    if isValidJSONName(n) then
      d[n] := v;
  end;
  slp.free;
  result := d.Text;
  d.free;
end;

function GetStatusCodeFromResponse(const r: string): integer;
var
  rlines: tstringlist;
  st: string;
begin
  result := -1;
  rlines := tstringlist.Create;
  rlines.Text := r;
  if rlines.count <> 0 then
  begin
    st := after(rlines[0], ' '); // this is the status code
    st := before(st, ' ');
    if isinteger(st) then // confirm before returning
      result := StrToInt(st);
  end;
  rlines.free;
end;

function RemoveHeaderFromResponse(const r: string): string;
var
  i: integer;
  start: boolean;
begin
  result := emptystr;
  start := false;
  for i := 1 to length(r) do
  begin
    if start = false then
    begin
      if (r[i] = #10) and (r[i - 1] = #13) and (r[i - 2] = #10) and
        (r[i - 3] = #13) then
        start := true;
    end
    else
      result := result + r[i];
  end;
end;

function GetHeaderFromResponse(const r: string): string;
var
  i: integer;
  collected: boolean;
begin
  result := emptystr;
  collected := false;
  for i := 1 to length(r) do
  begin
    if collected = false then
      if (r[i] = #10) and (r[i - 1] = #13) and (r[i - 2] = #10) and
        (r[i - 3] = #13) then
        break
      else
        result := result + r[i];
  end;
end;

function HostPort2URL(const host: string; const port: integer): string;
var
  proto, sport: string;
begin
  if port = 443 then
    proto := 'https://'
  else
    proto := 'http://';
  if (port <> 80) and (port <> 443) then
    sport := ':' + inttostr(port);
  result := proto + host + sport;
end;

function StripHTML(const s: string): string;
var
  i: integer;
  strip: boolean;
begin
  result := emptystr;
  strip := false;
  for i := 1 to length(s) do
  begin
    if s[i] = '<' then
      strip := true;
    if strip then
    begin
      if s[i] = '>' then
      begin
        strip := false;
        Continue;
      end;
    end
    else
      result := result + s[i];
  end;
end;

function StripPHPCode(const s: string): string;
var
  i: integer;
  strip: boolean;
begin
  result := emptystr;
  strip := false;
  for i := 1 to length(s) do
  begin
    if (s[i] = '<') and (s[i + 1] = '?') then
      strip := true;
    if strip then
    begin
      if (s[i] = '>') and (s[i - 1] = '?') then
      begin
        strip := false;
        Continue;
      end;
    end
    else
      result := result + s[i];
  end;
end;

function HtmlEscape(const s: string): string;
begin
  result := replacestr(s, '&', '&amp;');
  result := replacestr(result, '<', '&lt;');
  result := replacestr(result, '>', '&gt;');
  result := replacestr(result, '"', '&quot;');
  result := replacestr(result, '''', '&#x27;');
end;

function HtmlUnescape(const s: string): string;
begin
  result := replacestr(s, '&amp;', '&');
  result := replacestr(result, '&lt;', '<');
  result := replacestr(result, '&gt;', '>');
  result := replacestr(result, '&quot;', '"');
  result := replacestr(result, '&#x27;', '''');
end;

// Returns the value of field from a request/response header
function GetField(const Field, ReqStr:
{$IFDEF UNICODE}string{$ELSE}ansistring{$ENDIF}): ansistring;
var
  slp: TStringLoop;
  afield: string;
begin
  result := emptystr;
  afield := lowercase(Field);
  if pos(afield, lowercase(ReqStr)) = 0 then
    exit; // not found
  slp := TStringLoop.Create;
  slp.LoadFromString(ReqStr);
  while slp.Found do
  begin
    if beginswith(trim(slp.CurrentLower), afield + ':') then
    begin // found
      result := trim(after(slp.current, ':'));
      slp.Stop;
    end;
  end;
  slp.free;
end;

function HtmlColorToColor(const Color: string): TColor;
var
  cl: string;
begin
  cl := Color;
  Delete(cl, 1, 1);
  result := StrToIntDef('$' + Copy(cl, 5, 2) + Copy(cl, 3, 2) + Copy(cl, 1, 2),
    $00FFFFFF);
end;

function ExtractUrlFileName(const url: string): string;
var
  i: integer;
begin
  result := url;
  if pos('?', result) <> 0 then
    result := before(result, '?');
  i := LastDelimiter('/', result);
  result := Copy(result, i + 1, length(result) - (i));
end;

function ExtractUrlFileExt(const url: string): string;
begin
  result := ExtractUrlFileName(url);
  if pos('?', result) <> 0 then
    result := before(result, '?');
  result := extractfileext(result);
end;

function GetHostFromURL(const url: string): string;
begin
  result := after(url, '://');
  result := before(result, '/');
  if beginswith(result, '[') then
  begin // ipv6 format
    result := after(result, '[');
    result := before(result, ']');
    result := '[' + result + ']';
  end
  else
  begin // ipv4 format
    if pos(':', result) <> 0 then
      result := before(result, ':');
  end;
end;

function GetPortFromURL(const url: string): string;
var
  temp: string;
begin
  result := '80'; // default
  if beginswith(lowercase(url), 'https://') then
    result := '443';
  temp := after(url, '://');
  temp := before(temp, '/');
  if pos(':', temp) <> 0 then
  begin // port provided via format [proto]://[host]:[port]/
    if beginswith(temp, '[') then
    begin // ipv6 format
      temp := after(temp, ']:');
    end
    else
    begin // ipv4 format
      temp := after(temp, ':');
    end;
    if isinteger(temp) then
      result := temp;
  end;
end;

function GetPathFromURL(const url: string;
  const includeparams: boolean = true): string;
begin
  result := after(url, '://');
  result := after(result, '/');
  if includeparams = false then
  begin
    if pos('?', result) <> 0 then
      result := before(result, '?');
  end;
end;

function GetPostDataFromRequest(const r: string): string;
var
  slp: TStringLoop;
  foundempty, postbegin: boolean;
  postdata: string;
begin
  postdata := emptystr;
  foundempty := false;
  postbegin := false;
  slp := TStringLoop.Create;
  slp.LoadFromString(r);
  while slp.Found do
  begin
    if foundempty then
    begin
      if trim(slp.current) <> emptystr then
        postbegin := true;
    end;
    if postbegin then
    begin
      if postdata = emptystr then
        postdata := slp.current
      else
        postdata := postdata + crlf + slp.current;
    end;
    if trim(slp.current) = emptystr then
      foundempty := true;
  end;
  result := postdata;
  slp.free;
end;

function GetDomainFromHost(const host: string): string;
begin
  result := GetHostFromURL('http://' + host + '/');
end;

function GetPortFromHost(const host: string): string;
begin
  result := GetPortFromURL('http://' + host + '/');
end;

function HtmlEntityDecode(const s: string): string;
begin
  result := replacestr(s, '&lt;', '<');
  result := replacestr(result, '&gt;', '>');
  result := replacestr(result, '&quot;', '"');
  result := replacestr(result, '&amp;', '&');
end;

function GetPathFromRequest(const r: string): string;
var
  sl: tstringlist;
begin
  result := '/';
  sl := tstringlist.Create;
  sl.Text := r;
  if sl.count <> 0 then
  begin
    result := after(sl[0], ' '); // path, after HTTP method
    result := before(result, ' '); // before HTTP version
  end;
  sl.free;
end;

function URLDecode(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  if length(s) = 0 then
    result := emptystr
  else
  begin
    i := 1;
    while i <= length(s) do
    begin
      if s[i] = '%' then
      begin
        result := result + Chr(HexToInt(s[i + 1] + s[i + 2]));
        Inc(i, 2);
      end
      else if s[i] = '+' then
        result := result + ' '
      else
        result := result + s[i];

      Inc(i);
    end;
  end;
end;

function URLEncode(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ']): string;
var
  i: integer;
  sp: string;
begin
  if length(s) = 0 then
    result := emptystr
  else
  begin
    if plus then
      sp := '+'
    else
      sp := '%20';
    for i := 1 to length(s) do
    begin
      if not(s[i] in preserve) then
        result := result + '%' + IntToHex(ord(s[i]), 2)
      else if (s[i] = ' ') then
        result := result + sp
      else
        result := result + s[i];
    end;
  end;
end;

function URLEncodeFull(const s: string): string;
begin
  result := URLEncode(s, false, []);
end;

// ------------------------------------------------------------------------//
end.
