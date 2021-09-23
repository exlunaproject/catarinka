unit CatClipboardListener;
{
  Catarinka Clipboard Listener
  Copyright (c) 2012-2020 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

// Works with Vista and later
// Before Vista, you should be using SetClipboardViewer() instead of AddClipboardFormatListener()

interface

uses
  Windows, Messages, Classes, SysUtils;

type
  { TClipboardListener }

  TClipboardListener = class(TObject)
  strict private
    FOnClipboardChange: TNotifyEvent;
    FWnd: HWND;
    procedure WindowProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property OnClipboardChange: TNotifyEvent read FOnClipboardChange
      write FOnClipboardChange;
  end;

implementation

constructor TClipboardListener.Create;
begin
  inherited;
    FWnd := AllocateHWnd(WindowProc);
    if not AddClipboardFormatListener(FWnd) then
      RaiseLastOSError;
end;

destructor TClipboardListener.Destroy;
begin
  if FWnd <> 0 then
  begin
    RemoveClipboardFormatListener(FWnd);
    DeallocateHWnd(FWnd);
  end;
  inherited;
end;

procedure TClipboardListener.WindowProc(var Msg: TMessage);
begin
  if (Msg.msg = WM_CLIPBOARDUPDATE) and Assigned(FOnClipboardChange) then
  begin
    Msg.Result := 0;
    FOnClipboardChange(Self);
  end;
end;

end.