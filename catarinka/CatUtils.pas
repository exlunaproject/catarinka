unit CatUtils;

{
  Catarinka - Utils

  Copyright (c) 2003-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Messages, System.SysUtils, System.Win.Registry, Vcl.Forms;
{$ELSE}
  Windows, Messages, SysUtils, Registry, Forms;
{$ENDIF}

type
  TCatUninstallProgInfo = record
    Available: boolean;
    IsUptodate: boolean;
    DisplayVersion: string;
    InstallLocation: string;
  end;

function GetProgramUninstallInfo(const key: string; const version: string = '')
  : TCatUninstallProgInfo;
function GetInstallLanguage(const defaultLang: integer = 409): integer;
function IsWindowsSeven: boolean;
function IsWindowsEight: boolean;
function IsSoftwareInstalled(const softwarekey:string): boolean;
function IsSoftwareInstalled_WindowApp(const softwarekey:string): boolean;
function RegistryKeyExists(RootKey: HKEY; key: string): boolean;
procedure CatDelay(const ms: integer);
procedure CatDelayAlt(const ms: integer);
procedure OutDebug(const s: string);
procedure SetShortDateFormat(const s: string);

implementation

uses
  CatMatch;

type
  // Creates a sub-class in scope which we can use in a typecast
  // to gain access to protected members of the target superclass

  TApplicationHelper = class(TApplication);

procedure CatDelay(const ms: integer);
var
  start: Int64;
  elapsed: integer;
begin
  start := Trunc(Now * 24 * 60 * 60 * 1000);
  elapsed := 0;
  while elapsed < ms do
  begin
    Application.ProcessMessages;
    elapsed := Trunc(Now * 24 * 60 * 60 * 1000) - start;
  end;
end;

// Thanks Deltics
// https://stackoverflow.com/questions/39484344/an-alternative-to-sleep-function-in-delphi7
procedure CatDelayAlt(const ms: integer);
var
  start: Int64;
  elapsed: integer;
  wait: boolean;

  function ProcessMessage: boolean;
  var
    msg: TMsg;
    handled: boolean;
    app: TApplicationHelper;
  begin
    app := TApplicationHelper(Application);

    result := False;
    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
    begin
      result := True;
      if msg.Message <> WM_QUIT then
      begin
        handled := False;
        if Assigned(Application.OnMessage) then
          Application.OnMessage(msg, handled);

        if not app.IsHintMsg(msg) and not handled and not app.IsMDIMsg(msg) and
          not app.IsKeyMsg(msg) and not app.IsDlgMsg(msg) then
        begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;
      end
      else
        PostQuitMessage(msg.wParam);
    end;
  end;

begin
  wait := False; // We will not wait for messages initially
  start := Trunc(Now * 24 * 60 * 60 * 1000);

  SetTimer(0, 0, ms, NIL);
  // Makes sure we get a message (WM_TIMER) at the end of the timeout period
  repeat
    if wait then
      WaitMessage;

    wait := NOT ProcessMessage;
    // If there was no message then we will wait for one next time around

    elapsed := Trunc(Now * 24 * 60 * 60 * 1000) - start;

  until (elapsed >= ms);
end;

// Gets Windows install language code from the Registry
function GetInstallLanguage(const defaultLang: integer = 409): integer;
var
  Reg: TRegistry;
begin
  result := defaultLang; // 409 EN
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Nls\Language\', False)
    then
    begin
      result := StrToIntDef(Reg.ReadString('InstallLanguage'), defaultLang);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

// Gets some key uninstall info from the Program Uninstall key in Registry
// If a version number is passed as the second parameter, the version is compared with
// the installed version and if it is an older version, the IsUptodate key returns false
function GetProgramUninstallInfo(const key: string; const version: string = '')
  : TCatUninstallProgInfo;
var
  Reg: TRegistry;
begin
  result.Available := False;
  result.IsUptodate := True;
  result.InstallLocation := emptystr;
  result.DisplayVersion := emptystr;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + key
      + '_is1\', False) then
    begin
      result.Available := True;
      result.DisplayVersion := Reg.ReadString('DisplayVersion');
      result.InstallLocation := Reg.ReadString('InstallLocation');
      if version <> emptystr then
      begin
        // Compare the version. If installed version is older than passed version
        // IsUpdate is set to false
        if CompareVersionNumber(result.DisplayVersion, version) = -1 then
          result.IsUptodate := False;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure OutDebug(const s: string);
begin
{$IFDEF UNICODE}
  OutputDebugString(pWideChar(WideString(s)));
{$ELSE}
  OutputDebugString(pAnsiChar(AnsiString(s)));
{$ENDIF}
end;

procedure SetShortDateFormat(const s: string);
begin
{$IFDEF DXE2_OR_UP}
  FormatSettings.SHORTDATEFORMAT := s;
{$ELSE}
  SHORTDATEFORMAT := s;
{$ENDIF}
end;

function RegistryKeyExists(RootKey: HKEY; key: string): boolean;
var
  Reg: TRegistry;
begin
  result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKeyReadOnly(key) then
    begin
      result := True;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function IsWindowsSeven: boolean;
begin
  result := (Win32MajorVersion = 6) and (Win32MinorVersion = 1) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

function IsWindowsEight: boolean;
begin
  result := (Win32MajorVersion = 6) and (Win32MinorVersion = 2) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

// Examles:
// IsSoftwareInstalled('Python\PythonCore');
// IsSoftwareInstalled('Google\Chrome');
function IsSoftwareInstalled(const softwarekey:string): boolean;
begin
  result := false;
  if RegistryKeyExists(HKEY_LOCAL_MACHINE, '\SOFTWARE\'+softwarekey) then
    result := true else
  if RegistryKeyExists(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\AppModel\Lookaside\user\Software\'+softwarekey) then
    result := true else
  if RegistryKeyExists(HKEY_CURRENT_USER, '\SOFTWARE\'+softwarekey) then
    result := true else
  if RegistryKeyExists(HKEY_CURRENT_USER, '\Wow6432Node\SOFTWARE\'+softwarekey) then
    result := true;
  // on windows 10, when an app is installed from the windows store, the key used is:
  //HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\AppModel\Lookaside\user\Software\
end;

function IsSoftwareInstalled_WindowApp(const softwarekey:string): boolean;
begin
  result := RegistryKeyExists(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\AppModel\Lookaside\user\Software\'+softwarekey);
end;

function IsPythonCoreInstalled: boolean;
begin
  result := false;
  if RegistryKeyExists(HKEY_LOCAL_MACHINE, '\SOFTWARE\Python\PythonCore') then
    result := true else
  if RegistryKeyExists(HKEY_LOCAL_MACHINE, '\SOFTWARE\Microsoft\AppModel\Lookaside\user\Software\Python\PythonCore') then
    result := true else
  if RegistryKeyExists(HKEY_CURRENT_USER, '\SOFTWARE\Python\PythonCore') then
    result := true else
  if RegistryKeyExists(HKEY_CURRENT_USER, '\Wow6432Node\SOFTWARE\Python\PythonCore') then
    result := true;
  // on windows 10, when python is installed from the windows store, the key used is:
  //HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\AppModel\Lookaside\user\Software\Python\PythonCore\X.X\InstallPath
end;

// ------------------------------------------------------------------------//
end.
