unit CatHTTP;
{
  Catarinka - HTTP related functions
  Copyright (c) 2003-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
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

// HTTP functions
function CrackHTTPHost(const Host: string): THTTPHostParts;
function CrackHTTPRequest(const r: string): THTTPRequestParts;
function ExtractHTTPRequestPath(const r: string): string;
function ExtractHTTPRequestPostData(const r: string): string;
function ExtractHTTPResponseHeader(const r: string): string;
function ExtractHTTPResponseStatusCode(const r: string): integer;
function GetField(const Field, ReqStr: string): string;
function GetStatusCodeDescription(const sc:integer):string;
function PostDataToJSON(const s: string): string;
function RemoveHeaderFromResponse(const r: string): string;

implementation

uses
  CatStrings, CatJSON, CatHTML;

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

function GetStatusCodeDescription(const sc:integer):string;
var
  s:string;
begin
  s := emptystr;
  case sc of 
  100: s:='Continue';
  101: s:='Switching Protocols';
  200: s:='OK';
  201: s:='Created';
  202: s:='Accepted';
  203: s:='Non-Authoritative Information';
  204: s:='No Content';
  205: s:='Reset Content';
  206: s:='Partial Content';
  300: s:='Multiple Choices';
  301: s:='Moved Permanently';
  302: s:='Moved Temporarily';
  303: s:='See Other';
  304: s:='Not Modified';
  305: s:='Use Proxy';
  400: s:='Bad Request';
  401: s:='Unauthorized';
  402: s:='Payment Required';
  403: s:='Forbidden';
  404: s:='Not Found';
  405: s:='Method Not Allowed';
  406: s:='Not Acceptable';
  407: s:='Proxy Authentication Required';
  408: s:='Request Time-out';
  409: s:='Conflict';
  410: s:='Gone';
  411: s:='Length Required';
  412: s:='Precondition Failed';
  413: s:='Request Entity Too Large';
  414: s:='Request-URI Too Large';
  415: s:='Unsupported Media Type';
  500: s:='Internal Server Error';
  501: s:='Not Implemented';
  502: s:='Bad Gateway';
  503: s:='Service Unavailable';
  504: s:='Gateway Time-out';
  505: s:='HTTP Version not supported';
  end;
  result := s;
end;

// Returns the value of field from a request/response header
function GetField(const Field, ReqStr: string): string;
var
  slp: TStringLoop;
  afield, hdrline: string;
begin
  result := emptystr;
  afield := lowercase(Field);
  if pos(afield, lowercase(ReqStr)) = 0 then
    Exit; // not found
  slp := TStringLoop.Create(ReqStr);
  while slp.Found do
  begin
    hdrline := trim(slp.CurrentLower);
    // handles non-standard field with space after field name, like:
    // Access-Control-Allow-Methods : OPTIONS
    hdrline := stringreplace(hdrline, ' : ',': ', []);
    if beginswith(hdrline, afield + ':') then
    begin // found
      result := trim(after(slp.current, ':'));
      slp.Stop;
    end;
  end;
  slp.free;
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


// ------------------------------------------------------------------------//
end.
