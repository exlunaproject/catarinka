unit CatHTTP;
{
  Catarinka - HTTP and HTML related functions
  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  ColorToHTMLColor function by Ralf Mimoun
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Vcl.Graphics, CatStringLoop;
{$ELSE}
  Classes, SysUtils, Graphics, CatStringLoop;
{$ENDIF}

type
  TURLParts = record
    Filename: string;
    Fileext: string;
    Host: string;
    Path: string;
    Port: integer;
    Protocol: string;
    ProtocolHTTP: boolean;
  end;

type
  THTTPRequestParts = record
    Method: string;
    Path: string;
    Data: string; // POST data
  end;

type
  THTTPHostParts = record
    Name: string;
    Port: integer;
  end;
  
{
  THTTPHeaderLoop - Loops through a list of HTTP headers
  Expects a list like
  HTTP/1.1 200 OK
  Date: Sun, 09 Jul 2017 16:35:05 GMT
  Server: Apache
  (...)
}
type
  THTTPHeaderLoop = class(TStringLoop)
  private
    fFieldName:string;
    fFieldNameLower:string;
    fFieldValue:string;
  public
    constructor Create;
    function Found: boolean;
    procedure LoadFromResponse(const sl: tstrings); overload;
    procedure LoadFromResponse(const s:string); overload;
    property FieldName:string read fFieldName;
    property FieldNameLower:string read fFieldNameLower;
    property FieldValue:string read fFieldValue;
  end;

{
  TURLParamsLoop - Loops through URL params
  Expects a URL to be loaded with LoadFromURL()
  Example:
  obj.LoadFromURL('http://somehost/index.php?url=1&id=2');
}

type
  TURLParamsLoop = class(TStringLoop)
  private
    fParamName:string;
    fParamNameLower:string;
    fParamValue:string;
  public
    function Found: boolean;
    procedure LoadFromURL(const aURL:string);
    procedure LoadFromPOSTData(const PostData:string);
    property ParamName:string read fParamName;
    property ParamNameLower:string read fParamNameLower;
    property ParamValue:string read fParamValue;
  end;

// HTML functions
function BeginsWithHTTPProto(const url: string): boolean;
function BoolToDisplayState(const b: boolean): string;
function ColorToHTMLColor(const Color: TColor): string;
function DequoteHTMLAttribValue(const s: string): string;
function ExtractFromTag(const s, tag: string): string;
function HtmlColorToColor(const Color: string): TColor;
function HtmlEntityEncode(const s: string): string;
function HtmlEntityDecode(const s: string): string;
function HtmlEscape(const s: string): string;
function HtmlUnescape(const s: string): string;
function StripHTML(const s: string): string;
function StripPHPCode(const s: string): string;

// JSON functions
function JSONStringEscape(const s: string): string;

// HTTP functions
function CrackHTTPHost(const Host: string): THTTPHostParts;
function CrackHTTPRequest(const r: string): THTTPRequestParts;
function ExtractHTTPRequestPath(const r: string): string;
function ExtractHTTPRequestPostData(const r: string): string;
function ExtractHTTPResponseHeader(const r: string): string;
function ExtractHTTPResponseStatusCode(const r: string): integer;
function GetField(const Field, ReqStr: string): string;
function PostDataToJSON(const s: string): string;
function RemoveHeaderFromResponse(const r: string): string;

// URL functions
function CrackURL(const url: string): TURLParts;
function ChangeURLPath(const url, newpath: string): string;
function RemoveUrlPath(const url: string): string;
function RemoveURLFilename(const URL:string;ExtensionRequired:boolean=false):string;
function ExtractUrlFileExt(const url: string): string;
function ExtractUrlFileName(const url: string): string;
function ExtractUrlHost(const url: string): string;
function ExtractUrlPath(const url: string;
  const includeparams: boolean = true): string;
function ExtractUrlPort(const url: string): integer;
function FileUrlToFilename(const url: string): string;
function GenerateURL(const Host: string; const Port: integer): string;
function URLDecode(const s: string): string;
function URLEncode(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
function URLEncode_U(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
function URLEncodeFull(const s: string): string;
function URLPathTitleCase(const s: string): string;

implementation

uses
  CatStrings, CatJSON, CatMatch, CatEntities;
  
function BeginsWithHTTPProto(const url: string): boolean;
begin
  result := false;
  if BeginsWith(url, ['http:', 'https:'], true) then
    result := true;
end;  

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

function ChangeURLPath(const url, newpath: string): string;
var
  oldpath, reppath: string;
begin
  oldpath := '/' + ExtractUrlPath(url);
  reppath := newpath;
  result := replacestr(url + ' ', oldpath + ' ', reppath);
end;

// Expects Host[:Port] and will return its parts
// Example usage:
// CrackHost('127.0.0.1').port returns 80
// CrackHost('127.0.0.1:8080').port returns 8080
// CrackHost('[2001:4860:0:2001::68]:8080').port returns 8080
function CrackHTTPHost(const Host: string): THTTPHostParts;
var
  url: string;
begin
  url := 'http://' + Host + '/';
  result.Name := ExtractUrlHost(url);
  result.Port := ExtractUrlPort(url);
end;

function CrackHTTPRequest(const r: string): THTTPRequestParts;
begin
  result.Method := before(r, ' ');
  result.Path := ExtractHTTPRequestPath(r);
  result.Data := ExtractHTTPRequestPostData(r);
end;

function CrackURL(const url: string): TURLParts;
begin
  result.Fileext := ExtractUrlFileExt(url);
  result.Filename := ExtractUrlFileName(url);
  result.Host := ExtractUrlHost(url);
  result.Path := ExtractUrlPath(url);
  result.Port := ExtractUrlPort(url);
  result.Protocol := before(url, ':');
  result.ProtocolHTTP := BeginsWithHttpProto(url);
end;

// A special dequote for HTML attribute values
// "value" -> value, 'value' -> value
function DequoteHTMLAttribValue(const s: string): string;
const
  cQuotes = ['"', ''''];
var
  i: integer;
  last: char;
  firstisquote: boolean;
begin
  result := s;
  i := length(s);
  if i = 0 then
    Exit;
  last := LastChar(s);
  firstisquote := CharInSet(s[1], cQuotes);
  if (firstisquote = true) and (s[1] = last) then
  begin
    Delete(result, 1, 1);
    SetLength(result, length(result) - 1);
  end
  else if (firstisquote = false) and (ContainsAnyOfChars(s, cQuotes) = true) then
  begin
    // uncommon, but handling something like: value" target="new"
    if pos(' ', s) <> 0 then
      result := before(s, ' ');
  end;
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

function ExtractFromTag(const s, tag: string): string;
begin
  result := ExtractFromString(s, '<' + tag + '>', '</' + tag + '>');
end;

function ExtractHTTPResponseStatusCode(const r: string): integer;
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

function ExtractHTTPResponseHeader(const r: string): string;
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

function FileUrlToFilename(const url: string): string;
var
  f: string;
begin
  f := url;
  f := after(f, 'file://');
  f := replacestr(f, '/', '\\');
  result := f;
end;

// Generates an URL from a hostname
function GenerateURL(const Host: string; const Port: integer): string;
var
  proto, sport: string;
begin
  if Port = 443 then
    proto := 'https://'
  else
    proto := 'http://';
  if (Port <> 80) and (Port <> 443) then
    sport := ':' + inttostr(Port);
  result := proto + Host + sport;
end;

function StripHTML(const s: string): string;
begin
  result := StripEnclosed(s,'<', '>');
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

// Converts special characters to their HTML entity encoded counterparts
function HtmlEscape(const s: string): string;
begin
  result := replacestr(s, '&', '&amp;');
  result := replacestr(result, '<', '&lt;');
  result := replacestr(result, '>', '&gt;');
  result := replacestr(result, '"', '&quot;');
  result := replacestr(result, '''', '&#x27;'); // same as &apos;
  // The symbolic entity &apos; was originally not included in the HTML spec and
  // might therefore not be supported by all browsers
end;

// Reverts HTML escape of special characters
function HtmlUnescape(const s: string): string;
begin
  result := replacestr(s, '&lt;', '<');
  result := replacestr(result, '&gt;', '>');
  result := replacestr(result, '&quot;', '"');
  result := replacestr(result, '&apos;', '''');
  result := replacestr(result, '&#x27;', '''');
  result := replacestr(result, '&#39;', '''');
  result := replacestr(result, '&amp;', '&');
end;

function HtmlEntityDecode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Decode(s);
  d.Free;
end;

function HtmlEntityEncode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Encode(s);
  d.Free;
end;

// Returns the value of field from a request/response header
function GetField(const Field, ReqStr: string): string;
var
  slp: TStringLoop;
  afield: string;
begin
  result := emptystr;
  afield := lowercase(Field);
  if pos(afield, lowercase(ReqStr)) = 0 then
    Exit; // not found
  slp := TStringLoop.Create(ReqStr);
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

function ExtractUrlHost(const url: string): string;
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

function ExtractUrlPort(const url: string): integer;
var
  temp: string;
begin
  result := 80; // default
  if beginswith(lowercase(url), 'https://') then
    result := 443;
  temp := after(url, '://');
  temp := before(temp, '/');
  if pos(':', temp) <> 0 then
  begin // port provided via format [proto]://[host]:[port]/
    if beginswith(temp, '[') then // ipv6 format
      temp := after(temp, ']:')
    else // ipv4 format
      temp := after(temp, ':');
    if isinteger(temp) then
      result := StrToInt(temp);
  end;
end;

function ExtractUrlPath(const url: string;
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

// Converts JSON special characters to their escaped counterparts
function JSONStringEscape(const s: string): string;
const
  cEscapeChars: TSysCharSet = ['"', '/', '\', #8, #9, #10, #12, #13];
var
  i, p, len: integer;
begin
  Result := EmptyStr;
  p := 1;
  len := Length(s) + 1;
  for i := 1 to len do
  begin
    if CharInSet(s[i], cEscapeChars) then
    begin
      Result := Result+Copy(s, p, i - p);
      case s[i] of
        '\': Result := Result+ '\\';
        '/': Result := Result+ '\/';
        '"': Result := Result+ '\"';
        #8: Result := Result+ '\b';
        #9: Result := Result+ '\t';
        #10: Result := Result+ '\n';
        #12: Result := Result+ '\f';
        #13: Result := Result+ '\r';
      end;
      p := i + 1;
    end;
  end;
  Result := Result+ Copy(s, p, i - 1);
end;

// Removes the filename part of a URL, example:
// http://www.somehost.com/index.php -> http://www.somehost.com/
// If second parameter is true,
// filename is only removed if filename with extension is found in URL
function RemoveURLFilename(const URL:string;ExtensionRequired:boolean=false):string;
var
  fn:string;
  len:integer;
begin
 result := URL;
 fn := ExtractURLFileName(Url);
 if fn = emptystr then
  exit;
 if (ExtensionRequired = true) and (MatchWildcard(fn, '*.??*') = false) then
  exit;
 len := Length(Url)-Length(fn);
 result := Copy(URL, 1, len);
end;

// Removes the path from a URL, example:
// http://www.somehost.com/somepath -> http://www.somehost.com
function RemoveUrlPath(const url: string): string;
var proto:string;
begin
  if pos('://', url )<> 0 then begin
    proto := before(url, '://');
    result := after(url, '://');
  end;
  if proto = emptystr then
    proto := 'http';
  result := proto+ '://' +before(result, '/');
end;

function ExtractHTTPRequestPostData(const r: string): string;
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

function ExtractHTTPRequestPath(const r: string): string;
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
  ' ', '.']): string;
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
      if not(CharInSet(s[i], preserve)) then
        result := result + '%' + IntToHex(ord(s[i]), 2)
      else if (s[i] = ' ') then
        result := result + sp
      else
        result := result + s[i];
    end;
  end;
end;

function URLEncode_U(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
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
      sp := '%u0020';
    for i := 1 to length(s) do
    begin
      if not(CharInSet(s[i], preserve)) then
        result := result + '%u' + IntToHex(Integer(s[i]), 4)
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

// TitleCase function adapted to work with URL paths
function URLPathTitleCase(const s: string): string;
var
  i: integer;
begin
  result := s;
  for i := 1 to length(result) - 1 do
    if CharInSet(result[i], (['~', '/'] - ['.', '-', 'A' .. 'Z', 'a' .. 'z']))
    then
      if CharInSet(result[i + 1], ['a' .. 'z']) then
        result[i + 1] := char(ord(result[i + 1]) and not $20);
end;

{------------------------------------------------------------------------------}

constructor THTTPHeaderLoop.Create;
begin
  inherited Create(nil);
end;

procedure THTTPHeaderLoop.LoadFromResponse(const s:string);
var sl:tstringlist;
begin
  sl := TStringList.Create;
  sl.Text := s;
  Load(sl);
  sl.Free;
end;

procedure THTTPHeaderLoop.LoadFromResponse(const sl: tstrings);
var bs:string;
begin
    // Deletes the first line if it is not a field
    // Handles a line like: HTTP/1.1 200 OK, GET / HTTP/1.1, etc
    if sl.Count<>0 then begin
      if pos(' ',sl[0])<> 0 then begin
        bs := before(sl[0],' ');
        if endswith(bs,':') = false then
        sl.Delete(0);
      end;
    end;
    // If there is an empty line, delete it and what comes after it
    if pos(crlf+crlf, sl.text) <> 0 then
    sl.text := before(sl.Text,crlf+crlf);
  inherited Load(sl);
end;

function THTTPHeaderLoop.Found: boolean;
const
 cSep = ':';
begin
 result := inherited found;
 fFieldName:=emptystr;
 fFieldValue:=emptystr;
 fFieldNameLower:=emptystr;
 if pos(cSep,Current) <> 0 then begin
   fFieldName:=trim(before(current,cSep));
   fFieldValue:=trim(after(current,cSep));
   fFieldNameLower:=lowercase(FieldName);
 end;
end;

{------------------------------------------------------------------------------}

procedure TURLParamsLoop.LoadFromURL(const aURL:string);
var params,url:string;
begin
  url := trim(aURL);
  url := replacestr(url, ' ','%20');
  params := emptystr;
  if occurs('?', url) <> 0 then begin
    params := gettoken(url, '?', 2);
    params := replacestr(params,'&',crlf);
  end;
  LoadFromString(params);
  Reset;
end;

procedure TURLParamsLoop.LoadFromPOSTData(const POSTData:string);
var params:string;
begin
  params := trim(POSTData);
  params := replacestr(params, ' ','%20');
  if BeginsWith(params, '&') then
    params := After(params, '&');
  if occurs('&', params) <> 0 then begin
    params := replacestr(params,'&',crlf);
  end;
  LoadFromString(params);
  Reset;
end;

function TURLParamsLoop.Found: boolean;
const
 cSep = '=';
begin
 result := inherited found;
 fParamName:=emptystr;
 fParamValue:=emptystr;
 fParamNameLower:=emptystr;
 if pos(cSep,Current) <> 0 then begin
   fParamName:=trim(before(current,cSep));
   fParamValue:=trim(after(current,cSep));
   fParamNameLower:=lowercase(fParamName);
 end;
end;

// ------------------------------------------------------------------------//
end.
