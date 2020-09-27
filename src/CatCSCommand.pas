unit CatCSCommand;

{
  Console Output Capturer
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
  Original code of callback function by Jordi Corbilla with small contributions
  by Lars Fosdal

  Changes:
  * 27.09.2020, FD - Added CloseHandle(hWrite) to prevent ReadFile crash in console.
  * 22.09.2020, FD - Added Timeout with process kill.
  * 21.09.2020, FD - Call WaitForSingleObject with INFINITE (fixes loop).
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Vcl.Forms, System.Classes, System.SysUtils,
{$ELSE}
  Windows, Forms, Classes, SysUtils,
{$ENDIF}
  CatTasks, CatUtils, CatCSTimer;

type
  TCatCSCommandOutput = procedure(const s: String) of object;

{$IFDEF DXE2_OR_UP}
type
    TCmdOutputArg<T> = reference to procedure(const Arg: T);
{$ENDIF}

type
  TCatCSCommand = class
  private
    fAnsiMode: boolean;
    fOnOutput: TCatCSCommandOutput;
    fPID: Cardinal;
    fTimer: TConsoleTimer;
    fTimeout: integer;
    procedure SetTimeout(const ms:integer);
    procedure Timer1Timer(Sender: TObject);
  public
    procedure Run(const ACommand, AParameters: String);
    constructor Create;
    destructor Destroy; override;
    property AnsiMode:boolean read fAnsiMode write fAnsiMode;
    property OnOutput:TCatCSCommandOutput read fOnOutput write fOnOutput;
    property Timeout: integer read fTimeout write SetTimeout;
  end;

    {$IFDEF DXE2_OR_UP}
    procedure RunCmdWithCallBack(const ACommand, AParameters: String; CallBack: TCmdOutputArg<string>;const TimeoutMS:integer=0);
    {$ENDIF}

implementation

procedure TCatCSCommand.Run(const ACommand, AParameters: String);
const
    CReadBuffer = 2400;
var
    saSecurity: TSecurityAttributes;
    hRead: THandle;
    hWrite: THandle;
    suiStartup: TStartupInfo;
    piProcess: TProcessInformation;
    pBuffer: array [0 .. CReadBuffer] of AnsiChar;
    dBuffer: array [0 .. CReadBuffer] of AnsiChar;
    dRead: DWord;
    dRunning: DWord;
    ExitCode: Cardinal;
begin
    saSecurity.nLength := SizeOf(TSecurityAttributes);
    saSecurity.bInheritHandle := True;
    saSecurity.lpSecurityDescriptor := nil;

    if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    begin
        FillChar(suiStartup, SizeOf(TStartupInfo), #0);
        suiStartup.cb := SizeOf(TStartupInfo);
        suiStartup.hStdInput := hRead;
        suiStartup.hStdOutput := hWrite;
        suiStartup.hStdError := hWrite;
        suiStartup.dwFlags := suiStartup.dwFlags or STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        suiStartup.wShowWindow := SW_HIDE;

        if CreateProcess(nil, pChar(ACommand + ' ' + AParameters),
        @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
        begin
            // FD: Close hWrite handle or the function may hang in console mode
            CloseHandle(hWrite);
            fPID := piProcess.dwProcessId;
            if fTimeout > 0 then
              fTimer.Enabled := true;
            repeat
                Application.ProcessMessages;
                GetExitCodeProcess(piProcess.hProcess, ExitCode);
                // FD: Use WaitForSingleObject with INFINITE or the function will hang
                dRunning := WaitForSingleObject(piProcess.hProcess, INFINITE);
                repeat
                    if fTimeout > 0 then
                      fTimer.Reset;
                    dRead := 0;
                    ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                    pBuffer[dRead] := #0;

                    if fAnsiMode then
                      OemToAnsi(pBuffer, pBuffer) else
                      OemToCharA(pBuffer, dBuffer);
                    if Assigned(OnOutput) then
                    OnOutput(string(dBuffer));
                until (dRead < CReadBuffer);
            until (dRunning <> WAIT_TIMEOUT);
            CloseHandle(piProcess.hProcess);
            CloseHandle(piProcess.hThread);
        end;
        CloseHandle(hRead);
        CloseHandle(hWrite);
    end;
end;

{$IFDEF DXE2_OR_UP}
procedure RunCmdWithCallBack(const ACommand, AParameters: String; CallBack: TCmdOutputArg<string>;const TimeoutMS:integer=0);
const
    CReadBuffer = 2400;
var
    saSecurity: TSecurityAttributes;
    hRead: THandle;
    hWrite: THandle;
    suiStartup: TStartupInfo;
    piProcess: TProcessInformation;
    pBuffer: array [0 .. CReadBuffer] of AnsiChar;
    dBuffer: array [0 .. CReadBuffer] of AnsiChar;
    dRead: DWord;
    dRunning: DWord;
    tmTimer: TConsoleTimer;
    PID: Cardinal;
    ExitCode: Cardinal;
begin
    tmTimer := TConsoleTimer.Create;
    tmTimer.Interval := TimeoutMS;
    tmTimer.OnTimerCallBack := procedure()
      begin
        tmTimer.Enabled := false;
        KillTaskbyPID(PID);
      end;
    saSecurity.nLength := SizeOf(TSecurityAttributes);
    saSecurity.bInheritHandle := True;
    saSecurity.lpSecurityDescriptor := nil;

    if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    begin
        FillChar(suiStartup, SizeOf(TStartupInfo), #0);
        suiStartup.cb := SizeOf(TStartupInfo);
        suiStartup.hStdInput := hRead;
        suiStartup.hStdOutput := hWrite;
        suiStartup.hStdError := hWrite;
        suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        suiStartup.wShowWindow := SW_HIDE;

        if CreateProcess(nil, pChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
        begin
            // FD: Close hWrite handle or the function may hang in console mode
            CloseHandle(hWrite);
            PID := piProcess.dwProcessId;
            tmTimer.Enabled := (TimeoutMS > 0);
            repeat
                // FD: Use WaitForSingleObject with INFINITE or the function will hang
                dRunning := WaitForSingleObject(piProcess.hProcess, INFINITE);
                Application.ProcessMessages();
                GetExitCodeProcess(piProcess.hProcess, ExitCode);
                repeat
                    if TimeoutMS > 0 then
                      tmTimer.Reset;
                    dRead := 0;
                    ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                    pBuffer[dRead] := #0;
                    OemToCharA(pBuffer, dBuffer);
                    CallBack(string(dBuffer));
                until (dRead < CReadBuffer);
            until (dRunning <> WAIT_TIMEOUT);
            CloseHandle(piProcess.hProcess);
            CloseHandle(piProcess.hThread);
        end;
        CloseHandle(hRead);
        CloseHandle(hWrite);
    end;
    tmTimer.Free;
end;
{$ENDIF}

procedure TCatCSCommand.Timer1Timer(Sender: TObject);
begin
  fTimer.Enabled := false;
  KillTaskbyPID(fPID);
end;

procedure TCatCSCommand.SetTimeout(const ms:integer);
begin
  fTimer.Interval := ms;
  fTimeout := ms;
end;

constructor TCatCSCommand.Create;
begin
  inherited Create;
  fAnsiMode := false;
  fTimer := TConsoleTimer.Create;
  fTimer.OnTimerEvent := Timer1Timer;
  SetTimeout(0);
  fTimer.Enabled := false;
end;

destructor TCatCSCommand.Destroy;
begin
  fTimer.Free;
  inherited;
end;

end.
