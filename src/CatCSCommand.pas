unit CatCSCommand;

{
  Console Output Capturer
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
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
    TArg<T> = reference to procedure(const Arg: T);
{$ENDIF}

type
  TCatCSCommand = class
  private
    fOnOutput:TCatCSCommandOutput;
  public
    procedure Run(const ACommand, AParameters: String);
    constructor Create;
    destructor Destroy; override;
    property OnOutput:TCatCSCommandOutput read fOnOutput write fOnOutput;
  end;

    {$IFDEF DXE2_OR_UP}
    procedure RunCmdWithCallBack(const ACommand, AParameters: String; CallBack: TArg<string>);
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
            repeat
                Application.ProcessMessages;
                GetExitCodeProcess(piProcess.hProcess, ExitCode);
                // FD: Use WaitForSingleObject with INFINITE or the function will hang
                dRunning := WaitForSingleObject(piProcess.hProcess, INFINITE);
                repeat
                    dRead := 0;
                    ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                    pBuffer[dRead] := #0;

                    //OemToAnsi(pBuffer, pBuffer);
                    //Unicode support by Lars Fosdal
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
// Thanks Jordi Corbilla and Lars Fosdal by the anonymous procedure approach
procedure RunCmdWithCallBack(const ACommand, AParameters: String; CallBack: TArg<string>);
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
        suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        suiStartup.wShowWindow := SW_HIDE;

        if CreateProcess(nil, pChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
        begin
            repeat
                // FD: Use WaitForSingleObject with INFINITE or the function will hang
                dRunning := WaitForSingleObject(piProcess.hProcess, INFINITE);
                Application.ProcessMessages();
                repeat
                    dRead := 0;
                    ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                    pBuffer[dRead] := #0;

                    //OemToAnsi(pBuffer, pBuffer);
                    //Unicode support by Lars Fosdal
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
end;
{$ENDIF}

constructor TCatCSCommand.Create;
begin
  inherited Create;
end;

destructor TCatCSCommand.Destroy;
begin
  inherited;
end;

end.
