unit CatLogger;
{
  Catarinka Debug Logger class with elapsed time calculation functions

  Copyright (c) 2003-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
{$DEFINE USEDIAGNOSTICS}
  System.Classes, System.SysUtils, Vcl.Controls, System.Diagnostics,
  System.TimeSpan;
{$ELSE}
  Classes, SysUtils, Controls, Windows;
{$ENDIF}

type
{$IFDEF USEDIAGNOSTICS}
  // uses high precision stop watch
  TCatStopWatch = TStopWatch;
{$ELSE}

  TCatStopWatch = record
    StartTime: Cardinal;
  end;
{$ENDIF}

type
  TCatElapsedTime = record
    AsString: string;
    AsStringFriendly: string;
    MsAsString: string;
    Ms: Double;
  end;

type
  TCatLoggerOnLog = procedure(const msg: string) of object;
  TCatLoggerOnDebug = procedure(const msg: string) of object;

type
  TCatLogger = class
  private
    fLastWatchedDebugMsg: string;
    fMessages: TStringList;
    fDebugMessages: TStringList;
    fDurationLog: TStringList;
    fOnLog: TCatLoggerOnLog;
    fOnDebug: TCatLoggerOnDebug;
    fStarted: boolean;
    fWatch: TCatStopWatch;
    procedure HandleDebugMessage(const msg: string; const IsStop:boolean);
    procedure WatchReset;
    function GetDurationLogText:string;
  public
    procedure Clear;
    procedure Debug(const msg: string);
    procedure DebugW(const msg: string);
    procedure Log(const msg: string);
    constructor Create;
    destructor Destroy; override;
    // properties
    property Messages: TStringList read fMessages;
    property DebugMessages: TStringList read fDebugMessages;
    property Durations: string read GetDurationLogText;
    property DurationLog: TStringList read fDurationLog;
    // events
    property OnDebug: TCatLoggerOnDebug read fOnDebug write fOnDebug;
    property OnLog: TCatLoggerOnLog read fOnLog write fOnLog;
  end;

function CatStopWatchNew: TCatStopWatch;
function ElapsedTimeToFriendlyTime(el:TTimeSpan):string;
function GetStopWatchElapsedTime(sw: TCatStopWatch): TCatElapsedTime;

implementation

uses CatStrings;

{$IFDEF USEDIAGNOSTICS}

function ElapsedTimeToFriendlyTime(el:TTimeSpan):string;
begin
  result := emptystr;
  if el.Hours <> 0 then
    result := result + IntToStr(el.Hours)+'h';
  if el.Minutes<>0 then
    result := result + IntToStr(el.Minutes)+'min';
  if el.Seconds <> 0 then
    result := result + IntToStr(el.Seconds)+'sec';
  result := result + IntToStr(el.Milliseconds)+'ms';
end;

function GetStopWatchElapsedTime(sw: TCatStopWatch): TCatElapsedTime;
var
  el: TTimeSpan;
begin
  el := sw.Elapsed;
  result.Ms := el.TotalMilliseconds;
  result.MsAsString := FloatToStr(el.TotalMilliseconds) + ' ms';
  result.AsString := el.ToString;
  result.AsStringFriendly := ElapsedTimeToFriendlyTime(el);
end;

function CatStopWatchNew: TCatStopWatch;
begin
  result := TStopWatch.StartNew;
end;
{$ELSE}

function GetStopWatchElapsedTime(sw: TCatStopWatch): TCatElapsedTime;
var
  t: Cardinal;
  Ms: Double;
begin
  t := GetTickCount;
  Ms := (t - sw.StartTime) / 1000;
  result.Ms := t;
  result.MsAsString := FloatToStr(Ms) + ' ms';
  result.AsString := result.MsAsString;
end;

function CatStopWatchNew: TCatStopWatch;
begin
  result.StartTime := GetTickCount;
end;
{$ENDIF}

procedure TCatLogger.HandleDebugMessage(const msg: string; const IsStop:boolean);
begin
  if IsStop = true then
    fDurationLog.Add(msg);
  fDebugMessages.Add(msg);
  if Assigned(fOnDebug) then
    fOnDebug(msg);
end;

function TCatLogger.GetDurationLogText:string;
begin
  result := '[['+crlf+fDurationLog.Text+']]';
end;

// Standard logging
procedure TCatLogger.Log(const msg: string);
begin
  fMessages.add(msg);
  if Assigned(fOnLog) then
    fOnLog(msg);
end;

// Debug logging
procedure TCatLogger.Debug(const msg: string);
begin
  HandleDebugMessage(msg, false);
end;

// Debug logging with StopWatch
procedure TCatLogger.DebugW(const msg:string);
begin
// Logs and starts (if not started yet) or resets the watcher
  if fStarted = false then begin
    fStarted := true;
    fLastWatchedDebugMsg := msg;
    fWatch := CatStopWatchNew;
    HandleDebugMessage('started: '+msg, true);
  end else begin
    WatchReset;
    fLastWatchedDebugMsg := msg;
  end;
end;

procedure TCatLogger.WatchReset;
var
  et: TCatElapsedTime;
begin
  et := GetStopWatchElapsedTime(fWatch);
  HandleDebugMessage(et.AsStringFriendly+': '+fLastWatchedDebugMsg, true);
  {$IFDEF USEDIAGNOSTICS}
  fWatch.Start;
  {$ELSE}
  fWatch := CatStopWatchNew;
  {$ENDIF}
end;

procedure TCatLogger.Clear;
begin
  fStarted := false;
  fMessages.Clear;
  fDebugMessages.Clear;
  fDurationLog.Clear;
  fLastWatchedDebugMsg := emptystr;
end;

constructor TCatLogger.Create;
begin
  inherited Create;
  fMessages := TStringList.Create;
  fDebugMessages := TStringList.Create;
  fDurationLog := TStringList.Create;
  Clear;
end;

destructor TCatLogger.Destroy;
begin
  fDebugMessages.Free;
  fDurationLog.Free;
  fMessages.Free;
  inherited;
end;

// ------------------------------------------------------------------------//
end.
