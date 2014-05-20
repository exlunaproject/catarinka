unit CatTasks;
{
  Catarinka - Task Management library

  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Portions based on code from Guido Geurt's ggProcessViewer.pas
}

interface

uses
{$IF CompilerVersion >= 23} // XE2 or higher
  Winapi.Windows, Vcl.Forms, System.SysUtils, System.Classes, Winapi.TlHelp32;
{$ELSE}
  Windows, Forms, SysUtils, Classes, TlHelp32;
{$IFEND}
function KillTask(const ExeFileName: string): Integer;
function RunTask(const ExeFileName: string; const Wait: boolean = false;
  const WindowState: Integer = SW_SHOW): Cardinal;
function TaskRunning(const ExeFileName: WideString): boolean;
procedure GetProcesses(ProcList: TStringList);
procedure GetProcessesOnNT(ProcList: TStringList);
procedure KillEXE(const ExeFileName: string);
procedure KillMultipleTask(ProcList: TStringList; const TaskName: string);
procedure KillProcessbyPID(const PID: Cardinal);
procedure ResumeProcess(const ProcessID: DWORD);
procedure SuspendProcess(const ProcessID: DWORD);

implementation

uses CatStrings;

type
  // NT Functions for getting the process information
  TEnumProcesses = function(lpidProcess: LPDWORD; cb: DWORD;
    var cbNeeded: DWORD): BOOL; StdCall;
  TGetModuleBaseNameA = function(hProcess: THandle; hModule: hModule;
    lpBaseName: PAnsiChar; nSize: DWORD): DWORD; StdCall;
  TGetModuleFileNameExA = function(hProcess: THandle; hModule: hModule;
    lpFilename: PAnsiChar; nSize: DWORD): DWORD; StdCall;
  TEnumProcessModules = function(hProcess: THandle; lphModule: LPDWORD;
    cb: DWORD; var lpcbNeeded: DWORD): BOOL; StdCall;
  TByte = array [0 .. 0] of byte;

  // Address holders of the procedures for NT
var
  EnumProcesses: TEnumProcesses;
  GetModuleBaseNameA: TGetModuleBaseNameA;
  GetModuleFileNameExA: TGetModuleFileNameExA;
  EnumProcessModules: TEnumProcessModules;

const
  THREAD_SUSPEND_RESUME = $00000002;
  cPSAPIDLL = 'PSAPI.dll';
  cProcSep = '_pid=';

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  dwThreadId: DWORD): DWORD; stdcall; external 'kernel32.dll';

procedure SuspendProcess(const ProcessID: DWORD);
var
  ThreadsSnapshot, ThreadHandle: THandle;
  ThreadRecord: TThreadEntry32;
begin
  ThreadsSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  ThreadRecord.dwSize := sizeof(ThreadRecord);
  if Thread32First(ThreadsSnapshot, ThreadRecord) then
  begin
    repeat
      if ThreadRecord.th32OwnerProcessID = ProcessID then
      begin
        ThreadHandle := OpenThread(THREAD_SUSPEND_RESUME, false,
          ThreadRecord.th32ThreadID);
        if ThreadHandle = 0 then
          exit;
        SuspendThread(ThreadHandle);
        CloseHandle(ThreadHandle);
      end;
    until not Thread32Next(ThreadsSnapshot, ThreadRecord);
  end;
  CloseHandle(ThreadsSnapshot);
end;

procedure ResumeProcess(const ProcessID: DWORD);
var
  ThreadsSnapshot: THandle;
  ThreadRecord: TThreadEntry32;
  ThreadHandle: THandle;
begin
  ThreadsSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  ThreadRecord.dwSize := sizeof(ThreadRecord);
  if Thread32First(ThreadsSnapshot, ThreadRecord) then
  begin
    repeat
      if ThreadRecord.th32OwnerProcessID = ProcessID then
      begin
        ThreadHandle := OpenThread(THREAD_SUSPEND_RESUME, false,
          ThreadRecord.th32ThreadID);
        if ThreadHandle = 0 then
          exit;
        ResumeThread(ThreadHandle);
        CloseHandle(ThreadHandle);
      end;
    until not Thread32Next(ThreadsSnapshot, ThreadRecord);
  end;
  CloseHandle(ThreadsSnapshot);
end;

procedure KillProcessbyPID(const PID: Cardinal);
var
  h: THandle;
  lpExitCode: {$IFDEF UNICODE}Cardinal{$ELSE}DWORD{$ENDIF};
begin
  h := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION, false, PID);
  if h = 0 then
    exit;
  if GetExitCodeProcess(h, lpExitCode) then
    TerminateProcess(h, lpExitCode)
  else
    CloseHandle(h);
end;

function RunTask(const ExeFileName: string; const Wait: boolean = false;
  const WindowState: Integer = SW_SHOW): Cardinal;
var
  Prog: array [0 .. 512] of char;
  CurDir: array [0 .. 255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: Cardinal;
begin
  StrPCopy(Prog, ExeFileName);
  GetDir(0, WorkDir);
  StrPCopy(CurDir, WorkDir);
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := WindowState;
  if CreateProcess(nil, Prog, nil, nil, false, CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    Result := ProcessInfo.dwProcessId;
    if Wait = true then
    begin
      repeat
        application.ProcessMessages;
        GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
        WaitForSingleObject(ProcessInfo.hProcess, 10);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
  end
  else
    Result := $FFFFFFFF; // -1
end;

function TaskRunning(const ExeFileName: WideString): boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := false;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if (UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(ExeFileName)) then
      Result := true;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure GetProcessesOnNT(ProcList: TStringList);
var
  i: Integer;
  PIDNeeded, dwsz: DWORD;
  PIDList: array [0 .. 1000] of Integer;
  PIDName: array [0 .. MAX_PATH - 1] of
{$IFDEF UNICODE}AnsiChar{$ELSE}char{$ENDIF};
  PH: THandle;
  hMod: hModule;
begin
  ProcList.clear;
  try
    if not EnumProcesses(@PIDList, 1000, PIDNeeded) then
      raise Exception.Create('Error: ' + cPSAPIDLL + ' not found.');
    for i := 0 to (PIDNeeded div sizeof(Integer) - 1) do
    begin
      PH := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false,
        PIDList[i]);
      if PH <> 0 then
        if GetModuleFileNameExA(PH, 0, PIDName, sizeof(PIDName)) > 0 then
          if EnumProcessModules(PH, @hMod, sizeof(hMod), dwsz) then
          begin
            GetModuleFileNameExA(PH, hMod, PIDName, sizeof(PIDName));
            ProcList.Add(ExtractFileName(PIDName) + cProcSep +
              IntToStr(PIDList[i]));
            CloseHandle(PH);
          end;
    end;
  except
  end;
end;

function KillTask(const ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(ExeFileName))) then
    begin
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0),
        FProcessEntry32.th32ProcessID), 0));
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure KillEXE(const ExeFileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  GetProcesses(sl);
  if sl.Count = 0 then
    KillTask(ExeFileName)
  else
    KillMultipleTask(sl, ExeFileName);
  sl.free;
end;

procedure KillMultipleTask(ProcList: TStringList; const TaskName: string);
var
  i, c: Integer;
begin
  c := ProcList.Count;
  for i := 0 to c do
  begin
    If i < c then
    begin
      if (lowercase(before(ProcList.strings[i], cProcSep)) = lowercase(TaskName))
      then
        KillProcessbyPID(strtoint(after(ProcList.strings[i], cProcSep)));
    end;
  end;
end;

procedure GetProcesses(ProcList: TStringList);
var
  h: THandle;
begin
  h := LoadLibrary(cPSAPIDLL);
  if (h <> 0) then
  begin
    @EnumProcesses := GetProcAddress(h, 'EnumProcesses');
    @GetModuleBaseNameA := GetProcAddress(h, 'GetModuleBaseNameA');
    @GetModuleFileNameExA := GetProcAddress(h, 'GetModuleFileNameExA');
    @EnumProcessModules := GetProcAddress(h, 'EnumProcessModules');
    GetProcessesOnNT(ProcList);
    FreeLibrary(h);
  end
end;

// ------------------------------------------------------------------------//
end.
