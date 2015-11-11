unit CatMsg;

{
  Catarinka TCatMsg - For handling WM_COPYDATA messages
  Copyright (c) 2015 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, System.SysUtils;
{$ELSE}
  Classes, Windows, Messages, SysUtils;
{$ENDIF}

type
  TCatMsgOnCopyDataMessage = procedure(const msgid: integer; const str: string)
    of object;

type
  TCatMsg = class
  private
    fMsgHandle: HWND;
    fOnCopyDataMessage: TCatMsgOnCopyDataMessage;
    procedure crmMessage(var AMsg: TMessage);
    procedure WMCopyData(var message: TMessage); message WM_COPYDATA;
  public
    constructor Create;
    destructor Destroy; override;
    property OnCopyDataMessage: TCatMsgOnCopyDataMessage read fOnCopyDataMessage
      write fOnCopyDataMessage;
    property MsgHandle: HWND read fMsgHandle;
  end;

procedure SendCDMessage(desthandle, msgid: integer; l: string);

implementation

procedure SendCDMessage(desthandle, msgid: integer; l: string);
var
  pData: PCopyDataStruct;
begin
  pData := nil;
  try
    New(pData);
    pData^.dwData := msgid;
    pData^.cbData := Length(l) + 1;
    pData^.lpData := PAnsiChar(AnsiString(l));
    // SendMessage(desthandle, WM_COPYDATA, application.handle, integer(pData));
    SendMessage(desthandle, WM_COPYDATA, 0, integer(pData));
  finally
    Dispose(pData);
  end;
end;

procedure TCatMsg.WMCopyData(var message: TMessage);
var
  pData: PCopyDataStruct;
  str: string;
begin
  message.Result := 0;
  pData := PCopyDataStruct(message.LParam);
  if (pData = nil) then
    exit;

{$IF CompilerVersion >= 20.0} // >= Delphi 2009
  str := string(PAnsiChar(pData^.lpData));
{$ELSE} // < Delphi 2009
  str := string(StrPas(PAnsiChar(pData^.lpData)));
{$IFEND}
  if assigned(OnCopyDataMessage) then
    OnCopyDataMessage(pData^.dwData, str);
  message.Result := 1;
end;

procedure TCatMsg.crmMessage(var AMsg: TMessage);
begin
  try
    case AMsg.msg of
      WM_COPYDATA:
        WMCopyData(AMsg);
    end;
  except
    // Workaround for CEF windows focus related issue, when closing tab while loading
  end;
end;

constructor TCatMsg.Create;
begin
  fMsgHandle :=
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.AllocateHWnd(crmMessage);
end;

destructor TCatMsg.Destroy;
begin
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.DeallocateHWnd(fMsgHandle);
  inherited;
end;

end.
