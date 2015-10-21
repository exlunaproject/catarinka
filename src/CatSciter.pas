unit CatSciter;

{
  Catarinka Sciter Component
  This works as a replacement for AxSciter.pas without having to change much of the code
  Copyright (c) 2012-2015 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Vcl.Controls, System.Classes,
{$ELSE}
  Controls, Classes,
{$ENDIF}
  Sciter, SciterApi;

type
  TCatSciterOnStdOut = procedure(ASender: TObject; const msg: WideString)
    of object;
  TCatSciterOnStdErr = procedure(ASender: TObject; const msg: WideString)
    of object;

type
  TCatSciter = class(TCustomControl)
  private
    fEngine: TSciter;
    fOnStdOut: TCatSciterOnStdOut;
    fOnStdErr: TCatSciterOnStdErr;
    function GetRoot: iElement;
    procedure SciterMessage(ASender: TObject;
      const Args: TSciterOnMessageEventArgs);
  protected
  public
    function Eval(const script: WideString): OleVariant;
    function LoadURL(const URL: WideString; Async: boolean = true): boolean;
    procedure LoadHTML(const html: WideString; const baseURL: WideString);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property Engine: TSciter read fEngine;
    property OnOnStdOut: TCatSciterOnStdOut read fOnStdOut write fOnStdOut;
    property OnOnStdErr: TCatSciterOnStdErr read fOnStdErr write fOnStdErr;
    property Root: iElement read GetRoot;
  end;

implementation

function TCatSciter.GetRoot: iElement;
begin
  result := fEngine.Root;
end;

procedure TCatSciter.SciterMessage(ASender: TObject;
  const Args: TSciterOnMessageEventArgs);
begin
  case Args.Severity of
    OS_INFO:
      if Assigned(fOnStdOut) then
        fOnStdOut(ASender, Args.Message);
    OS_ERROR:
      if Assigned(fOnStdErr) then
        fOnStdErr(ASender, Args.Message);
  end;
end;

procedure TCatSciter.LoadHTML(const html: WideString;
  const baseURL: WideString);
begin
  fEngine.LoadHTML(html, baseURL);
end;

function TCatSciter.LoadURL(const URL: WideString;
  Async: boolean = true): boolean;
begin
  result := fEngine.LoadURL(URL, Async);
end;

function TCatSciter.Eval(const script: WideString): OleVariant;
begin
// result := fEngine.Eval(script);
end;

constructor TCatSciter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];

  fEngine := TSciter.Create(self);
  fEngine.Parent := self;
  fEngine.Align := alClient;
  fEngine.OnMessage := SciterMessage;
end;

destructor TCatSciter.Destroy;
begin
  fEngine.free;
  inherited Destroy;
end;

end.
