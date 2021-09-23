unit CatInet;
{
  Catarinka - Internet related functions

  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  GetTinyUrl function by Rodrigo Ruz V., released under the MIT license
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Winapi.WinSock, System.SysUtils, System.Win.Registry,
  Winapi.WinInet;
{$ELSE}
  Windows, WinSock, SysUtils, Registry, WinInet;
{$ENDIF}

function ExtractIPAddresses(const s: string): string; overload;
function ExtractIPAddresses(const s: string;const exceptions: array of string): string; overload;
function GetAbsoluteURL(const baseURL, relURL: string): string;
function GetTinyUrl(const URL: string): string;
function IPAddrToName(const IP: string): string;
function IsValidIP(const IP: string): Boolean;
function NameToIPAddr(const name: string): string;
procedure DisableProxy(const agent: string);
procedure EnableProxy(const agent, proxy: string);
procedure IESettingsChanged(const agent: string);

implementation

uses CatStrings, RegExpr;

procedure DisableProxy(const agent: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey
      ('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', True)
    then
    begin
      Reg.WriteInteger('ProxyEnable', 0);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  IESettingsChanged(agent);
end;

procedure EnableProxy(const agent, proxy: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey
      ('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', True)
    then
    begin
      Reg.WriteString('ProxyServer', proxy); // format: proxy:proxyport
      Reg.WriteInteger('ProxyEnable', 1);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  IESettingsChanged(agent);
end;

// Usage example:
// GetAbsoluteURL('http://someurl.com/demo/','/index.html')
// will return http://someurl.com/index.html
function GetAbsoluteURL(const baseURL, relURL: string): string;
  procedure TruncateStr(var s: string);
  var
    i: Integer;
  begin
    for i := 1 to length(s) do
      if (s[i] = #0) then
      begin
        SetLength(s, i - 1);
        Exit;
      end;
  end;

var
  buflen: DWORD;
begin
  buflen := 50240;
  SetLength(Result, buflen);
  InternetCombineUrl(
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(baseURL),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(relURL),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(Result), buflen,
    ICU_BROWSER_MODE);
  TruncateStr(Result);
  // workaround, ipv6
  Result := replacestr(Result, '%5B', '[');
  Result := replacestr(Result, '%5D', ']');
end;

procedure IESettingsChanged(const agent: string);
var
  HInet: HINTERNET;
begin
  HInet := InternetOpen({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(agent),
    INTERNET_OPEN_TYPE_DIRECT, nil, nil, INTERNET_FLAG_OFFLINE);
  try
    if HInet <> nil then
      InternetSetOption(HInet, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
  finally
    InternetCloseHandle(HInet);
  end;
end;

function IPAddrToName(const IP: string): string;
var
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  Addr: Longint;
begin
  Result := EmptyStr;
  if WSAStartup(MakeWord(1, 1), WSAData) <> 0 then
    Exit;
  Addr := inet_addr(PAnsiChar(ansistring(IP)));
  HostEnt := gethostbyaddr(@Addr, 4, PF_INET);
  if HostEnt = nil then
    Exit;

  Result := string(HostEnt^.h_name);
  WSACleanup;
end;

function IsValidIP(const IP: string): Boolean;
begin
  Result := ((IP <> emptystr) and
    (inet_addr(PAnsiChar(ansistring(IP))) <>
    integer(INADDR_NONE)));
end;

function ExtractIPAddresses(const s: string;const exceptions: array of string): string;
var
  r: TRegExpr;
  function IsValid(const ip:string):boolean;
  begin
   result := IsValidIP(ip);
   if beginswith(ip, exceptions) = true then
     result := false;
  end;
begin
  result := emptystr;
   r := TRegExpr.Create;
   try
     r.Expression := '\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b';
     if r.Exec(s) then
       repeat
         if IsValid(r.Match[0]) = true then begin
           if result = emptystr then
             result := r.Match[0]
           else
             result := result+','+r.Match[0]
         end
       until not r.ExecNext;
   finally
     r.Free;
   end;
end;

function ExtractIPAddresses(const s: string): string;
begin
  result := ExtractIPAddresses(s, []);
end;

function NameToIPAddr(const name: string): string;
var
  p: PHostEnt;
  a: TInAddr;
  WSAData: TWSAData;
begin
  Result := '0.0.0.0';
  WSAStartup($101, WSAData);
  p := GetHostByName(PAnsiChar(AnsiString(name)));
  if Assigned(p) then
  begin
    A := PInAddr(p^.h_Addr_List^)^;
    Result := string(inet_ntoa(A));
  end;
end;

// By Rodrigo Ruz (MIT license)
function GetTinyUrl(const URL: string): string;
const
  tinyurl = 'http://tinyurl.com/api-create.php?url=%s';
  BuffSize = 2048;
var
  hInter, UrlHandle: HINTERNET;
  BytesRead: Cardinal;
  Buffer: Pointer;
begin
  Result := emptystr;
  hInter := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(hInter) then
  begin
    GetMem(Buffer, BuffSize);
    try
      UrlHandle := InternetOpenUrl(hInter, PChar(Format(tinyurl, [URL])), nil,
        0, INTERNET_FLAG_RELOAD, 0);
      if Assigned(UrlHandle) then
      begin
        InternetReadFile(UrlHandle, Buffer, BuffSize, BytesRead);
        if BytesRead > 0 then
          SetString(Result, PAnsiChar(Buffer), BytesRead);
        InternetCloseHandle(UrlHandle);
      end;
    finally
      FreeMem(Buffer);
    end;
    InternetCloseHandle(hInter);
  end
end;

// ------------------------------------------------------------------------//
end.
