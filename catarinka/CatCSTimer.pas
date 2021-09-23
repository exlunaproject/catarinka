unit CatCSTimer;

{
  Catarinka - Console Timer

  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
  Based on TConsoleTimer by LU RD

  Changes:
  * 22.09.2020, FD - Added OnTimerCallBack that allows to work through callback
                   - Added Reset method
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, SyncObjs, Diagnostics;
{$ELSE}
  Windows, Classes, SyncObjs;
{$ENDIF}


type
    TTimerArg = {$IFDEF DXE2_OR_UP}reference to procedure{$ELSE}procedure of object{$ENDIF};

type
  TConsoleTimer = Class(TThread)
  private
    fCancelFlag: TSimpleEvent;
    fTimerEnabledFlag: TSimpleEvent;
    fTimerProc: TNotifyEvent; // method to call
    fTimerCallBack: TTimerArg;
    fInterval: integer;
    procedure SetEnabled(doEnable: boolean);
    function GetEnabled: boolean;
    procedure SetInterval(interval: integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property Interval: integer read FInterval write SetInterval;
    // Note: OnTimerEvent is executed in TConsoleTimer thread
    property OnTimerEvent: TNotifyEvent read fTimerProc write fTimerProc;
    property OnTimerCallBack: TTimerArg read fTimerCallback write fTimerCallBack;
  end;

implementation

constructor TConsoleTimer.Create;
begin
  inherited Create(false);
  FTimerEnabledFlag := TSimpleEvent.Create;
  FCancelFlag := TSimpleEvent.Create;
  FTimerProc := nil;
  FInterval := 1000;
  Self.FreeOnTerminate := false; // Main thread controls for thread destruction
end;

destructor TConsoleTimer.Destroy; // Call TConsoleTimer.Free to cancel the thread
begin
  Terminate; 
  FTimerEnabledFlag.ResetEvent; // Stop timer event
  FCancelFlag.SetEvent; // Set cancel flag
  Waitfor; // Synchronize
  FCancelFlag.Free;
  FTimerEnabledFlag.Free;
  inherited;
end;

procedure TConsoleTimer.SetEnabled(doEnable: boolean);
begin
  if doEnable then
    FTimerEnabledFlag.SetEvent
  else
    FTimerEnabledFlag.ResetEvent;
end;

procedure TConsoleTimer.SetInterval(interval: integer);
begin
  FInterval := interval;
end;

procedure TConsoleTimer.Execute;
var
  waitList: array [0 .. 1] of THandle;
  waitInterval,lastProcTime: Int64;
  sw: TStopWatch;
begin
  sw.Create;
  waitList[0] := FTimerEnabledFlag.Handle;
  waitList[1] := FCancelFlag.Handle;
  lastProcTime := 0;
  while not Terminated do
  begin
    if (WaitForMultipleObjects(2, @waitList[0], false, INFINITE) <>
      WAIT_OBJECT_0) then
      break; // Terminate thread when FCancelFlag is signaled
    if Assigned(fTimerProc)
    {$IFDEF DXE2_OR_UP}
    or Assigned(fTimerCallBack)
    {$ENDIF}
    then
    begin
      waitInterval := FInterval - lastProcTime;
      if (waitInterval < 0) then
        waitInterval := 0;
      if WaitForSingleObject(FCancelFlag.Handle,waitInterval) <> WAIT_TIMEOUT then
        break;

      if WaitForSingleObject(FTimerEnabledFlag.Handle, 0) = WAIT_OBJECT_0 then
      begin
        sw.Start;
        if Assigned(fTimerProc) then
          FTimerProc(Self);
       {$IFDEF DXE2_OR_UP}
        if Assigned(fTimerCallback) then
          FTimerCallback;
       {$ENDIF}
        sw.Stop;
        // Interval adjusted for FTimerProc execution time
        lastProcTime := sw.ElapsedMilliSeconds;
      end;
    end;
  end;
end;

function TConsoleTimer.GetEnabled: boolean;
begin
  Result := (FTimerEnabledFlag.Waitfor(0) = wrSignaled);
end;

procedure TConsoleTimer.Reset;
begin
 SetEnabled(false);
 SetEnabled(true);
end;

{

Additional Notes:

How to make the event execute in the main thread:

Reading: Handling events in console application (TidIRC) gives the answer.
https://stackoverflow.com/a/11366186/576719

Add a method in TConsoleTimer:

procedure TConsoleTimer.SwapToMainThread;
begin
  FTimerProc(Self);
end;
and change the call in the Execute method to:

Synchronize(SwapToMainThread);
To pump the synchronized calls, use CheckSynchronize() function in Classes unit:

while not KeyPressed do CheckSynchronize(); // Pump the synchronize queue
}

end.