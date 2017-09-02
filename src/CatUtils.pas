unit CatUtils;

{
  Catarinka - Utils

  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.SysUtils, Vcl.Forms;
{$ELSE}
  Windows, SysUtils, Forms;
{$ENDIF}
procedure CatDelay(const ms: Integer);
procedure OutDebug(const s: string);
procedure SetShortDateFormat(const s: string);

implementation

procedure CatDelay(const ms: Integer);
var
  c, te: Integer;
begin
  c := Integer(GetTickCount);
  repeat
    te := Integer(GetTickCount) - c;
    if te < 0 then
      te := te + MaxInt;

    if (ms > te) and (MsgWaitForMultipleObjects(0, PHandle(0)^, True, ms - te,
      QS_ALLEVENTS) <> WAIT_TIMEOUT) then
      Application.ProcessMessages
    else
      break;
  until False;
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
