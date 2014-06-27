unit CatUtils;

{
  Catarinka - Utils

  Copyright (c) 2003-2014 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Vcl.Forms;
{$ELSE}
  Windows, Forms;
{$ENDIF}
procedure CatDelay(const ms: Integer);

implementation

procedure CatDelay(const ms: Integer);
var
  c, te: Integer;
begin
  c := integer(GetTickCount);
  repeat
    te := integer(GetTickCount) - c;
    if te < 0 then
      te := te + MaxInt;

    if (ms > te) and (MsgWaitForMultipleObjects(0, PHandle(0)^, True, ms - te,
      QS_ALLEVENTS) <> WAIT_TIMEOUT) then
      Application.ProcessMessages
    else
      break;
  until False;
end;

// ------------------------------------------------------------------------//
end.
