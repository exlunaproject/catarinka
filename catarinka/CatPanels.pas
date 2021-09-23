unit CatPanels;

{
  Catarinka Panels
  Copyright (c) 2015-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Controls, CatUI;
{$ELSE}
  Classes, StdCtrls, ExtCtrls, Graphics, Controls;
{$ENDIF}

type
  TCanvasPanel = class(TPanel)
  public
    property Canvas;
  end;

type
  TBarTitlePanel = class(TPanel)
  private
    fCloseBtn: TButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CloseButton:TButton read fCloseBtn;
  end;

implementation

constructor TBarTitlePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 24;
  Color := clBtnShadow;
  ParentBackground := false;
  Font.Color := clWhite;
  fCloseBtn := TButton.Create(self);
  fCloseBtn.Parent := self;
  fCloseBtn.Align := alright;
  fCloseBtn.Font.Style := fCloseBtn.Font.Style + [fsBold];
  fCloseBtn.Width := 22;
  fCloseBtn.Caption := 'x';
end;

destructor TBarTitlePanel.Destroy;
begin
  fCloseBtn.Free;
  inherited Destroy;
end;

end.
