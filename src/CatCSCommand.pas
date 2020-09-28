unit CatCSCommand;

{
  Console Output Capturer
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
  Original code of callback function by David Heffernan (@davidheff)

  Changes:
  * 28.09.2020, FD - Rewrite based on example by DH.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Vcl.Forms, System.Classes, System.SysUtils;
{$ELSE}
  Windows, Forms, Classes, SysUtils;
{$ENDIF}

type
  TCatCSCommandOutput = procedure(const s: String) of object;

{$IFDEF DXE2_OR_UP}
type
    TCmdOutputArg<T> = reference to procedure(const Arg: T);
{$ENDIF}

type
  TCatCSCommand = class
  private
    fOnOutput: TCatCSCommandOutput;
    fPID: Cardinal;
    fTimeout: Cardinal;
    procedure SetTimeout(const ms:Cardinal);
  public
    procedure Run(const Command, Parameters: String);
    constructor Create;
    destructor Destroy; override;
    property OnOutput:TCatCSCommandOutput read fOnOutput write fOnOutput;
    property PID: Cardinal read fPID;
    property Timeout: Cardinal read fTimeout write SetTimeout;
  end;

    {$IFDEF DXE2_OR_UP}
    procedure RunCmdWithCallBack(const Command: string; const Parameters: string;
  CallBack: TCmdOutputArg<string>;const Timeout:dword=100);
    {$ENDIF}

implementation

const
  InheritHandleSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);

procedure TCatCSCommand.Run(const Command, Parameters: String);
var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  FileSize: Int64;
  AnsiBuffer: array [0 .. 819200 - 1] of AnsiChar;
begin
  Win32Check(CreatePipe(hReadStdout, hWriteStdout,
    @InheritHandleSecurityAttributes, 0));
  try
    si := Default (TStartupInfo);
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;
    Win32Check(CreateProcess(nil, PChar(Command + ' ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi));
    fPID := pi.dwProcessId;
    try
      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, fTimeout);
        Win32Check(WaitRes <> WAIT_FAILED);
        while True do
        begin
          Win32Check(GetFileSizeEx(hReadStdout, FileSize));
          if FileSize = 0 then
          begin
            break;
          end;
          Win32Check(ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1,
            BytesRead, nil));
          if BytesRead = 0 then
          begin
            break;
          end;
          AnsiBuffer[BytesRead] := #0;
          OemToAnsi(AnsiBuffer, AnsiBuffer);
          if Assigned(fOnOutput) then
            fOnOutput(string(AnsiBuffer));
        end;
        if WaitRes = WAIT_OBJECT_0 then
        begin
          break;
        end;
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hReadStdout);
    CloseHandle(hWriteStdout);
  end;
end;

{$IFDEF DXE2_OR_UP}
procedure RunCmdWithCallBack(const Command: string; const Parameters: string;
  CallBack: TCmdOutputArg<string>;const Timeout:dword=100);
var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  FileSize: Int64;
  AnsiBuffer: array [0 .. 819200 -1] of AnsiChar; // original was 1024 -1
  PID: Cardinal;
begin
  Win32Check(CreatePipe(hReadStdout, hWriteStdout,
    @InheritHandleSecurityAttributes, 0));
  try
    si := Default (TStartupInfo);
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;
    Win32Check(CreateProcess(nil, PChar(Command + ' ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi));
    PID := pi.dwProcessId;
    try
      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, Timeout);
        Win32Check(WaitRes <> WAIT_FAILED);
        while True do
        begin
          Win32Check(GetFileSizeEx(hReadStdout, FileSize));
          if FileSize = 0 then
          begin
            break;
          end;
          Win32Check(ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1,
            BytesRead, nil));
          if BytesRead = 0 then
          begin
            break;
          end;
          AnsiBuffer[BytesRead] := #0;
          OemToAnsi(AnsiBuffer, AnsiBuffer);
          if Assigned(CallBack) then
          begin
            CallBack(string(AnsiBuffer));
          end;
        end;
        if WaitRes = WAIT_OBJECT_0 then
        begin
          break;
        end;
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hReadStdout);
    CloseHandle(hWriteStdout);
  end;
end;
{$ENDIF}

procedure TCatCSCommand.SetTimeout(const ms:Cardinal);
begin
  fTimeout := ms;
end;

constructor TCatCSCommand.Create;
begin
  inherited Create;
  SetTimeout(100);
end;

destructor TCatCSCommand.Destroy;
begin
  inherited;
end;

end.
