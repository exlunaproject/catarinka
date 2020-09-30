unit CatUtils;

{
  Catarinka - Utils

  Copyright (c) 2003-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Messages, System.SysUtils, Vcl.Forms;
{$ELSE}
  Windows, SysUtils, Forms;
{$ENDIF}
procedure CatDelay(const ms: Integer);
procedure CatDelayAlt(const ms: Integer);
procedure OutDebug(const s: string);
procedure SetShortDateFormat(const s: string);

implementation

type
  // Creates a sub-class in scope which we can use in a typecast
  //  to gain access to protected members of the target superclass

  TApplicationHelper = class(TApplication);


procedure CatDelay(const ms: Integer);
var
  start: Int64;
  elapsed: Integer;
begin
  start   := Trunc(Now * 24 * 60 * 60 * 1000);
  elapsed := 0;
  while elapsed < ms do
  begin
    Application.ProcessMessages;
    elapsed := Trunc(Now * 24 * 60 * 60 * 1000) - start;
  end;
end;

// Thanks Deltics
// https://stackoverflow.com/questions/39484344/an-alternative-to-sleep-function-in-delphi7
procedure CatDelayAlt(const ms: Integer);
var
  start: Int64;
  elapsed: Integer;
  wait: Boolean;

  function ProcessMessage: Boolean;
  var
    msg: TMsg;
    handled: Boolean;
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

        if   not app.IsHintMsg(msg)
         and not handled
         and not app.IsMDIMsg(msg)
         and not app.IsKeyMsg(msg)
         and not app.IsDlgMsg(msg) then
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
  wait  := FALSE; // We will not wait for messages initially
  start := Trunc(Now * 24 * 60 * 60 * 1000);

  SetTimer(0, 0, ms, NIL); // Makes sure we get a message (WM_TIMER) at the end of the timeout period
  repeat
    if wait then
      WaitMessage;

    wait := NOT ProcessMessage; // If there was no message then we will wait for one next time around

    elapsed := Trunc(Now * 24 * 60 * 60 * 1000) - start;

  until (elapsed >= ms);
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

// ------------------------------------------------------------------------//
end.
