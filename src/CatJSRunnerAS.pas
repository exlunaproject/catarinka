unit CatJSRunnerAS;
{
  Catarinka ActiveScript Executor (from Syhunt Scarlett project)
  Copyright (c) 2013-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, CatActiveScript, CatActiveScript32, SysUtils;

type
  TScarlettActiveScriptError = procedure(Line, Pos: Integer;
    ASrc, ADescription: String) of object;

type
  TScarlettActiveScript = class
  private
    asw: TSyActiveScriptWindow;
    aswold: TXSyActiveScriptWindow;
    fErrors: TStringList;
    fUseOldParser: boolean;
    fScript: string;
    fScriptHeader: TStringList;
    fScriptSuccess: boolean;
    fScriptLanguage: string;
    fOnScriptError: TScarlettActiveScriptError;
    fUseSafeSubSet: boolean;
    procedure aswError(Sender: TObject; Line, Pos: Integer;
      ASrc, ADescription: String);
    procedure SetLanguage(lang:string);
    procedure SetSafeSubSet(b:boolean);
  public
    engine: TObject;
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function RunScript(s: string): boolean;
    function RunExpression(s: string): string;
    function RunExpressionDirect(s: string): string;
    procedure AddNamedItem(AName : string; AIntf : IUnknown);
    procedure AddGlobalMember(AName:string);
    // properties
    property Errors: TStringList read fErrors;
    property Script: string read fScript;
    property ScriptHeader: TStringList read fScriptHeader;
    property ScriptLanguage: string read fScriptLanguage write SetLanguage;
    property ScriptSuccess: boolean read fScriptSuccess;
    property OnScriptError: TScarlettActiveScriptError read fOnScriptError
      write fOnScriptError;
    property UseOldEngine: boolean read fUseOldParser write fUseOldParser;
    property UseSafeSubset: boolean read fUseSafeSubSet write SetSafeSubSet;
  end;

var
  FActiveScript: TScarlettActiveScript;

const
  crlf = #13 + #10;

implementation

const
  cDefaultLanguage = 'JScript';

procedure TScarlettActiveScript.SetLanguage(lang:string);
begin
 fscriptlanguage := lang;
 asw.ScriptLanguage := lang;
 aswold.ScriptLanguage := lang;
end;

procedure TScarlettActiveScript.SetSafeSubSet(b:boolean);
begin
  fUseSafeSubSet := b;
  asw.usesafesubset := b;
  aswold.usesafesubset := b;
end;

procedure TScarlettActiveScript.AddGlobalMember(AName:string);
begin
 if fUseOldParser = false then
  asw.AddGlobalMember(Aname) else
  aswold.AddGlobalMember(AName);
end;

procedure TScarlettActiveScript.AddNamedItem(AName : string; AIntf : IUnknown);
begin
 if fUseOldParser = false then
  asw.AddNamedItem(Aname, AIntf) else
  aswold.AddNamedItem(AName, AIntf);
end;

function TScarlettActiveScript.RunExpression(s: string): string;
begin
  fScriptSuccess := true;
  errors.clear;
  if fUseOldParser = false then
  result := asw.RunExpression(s) else
  result := aswold.RunExpression(s);
end;

function TScarlettActiveScript.RunExpressionDirect(s: string): string;
begin
  if fUseOldParser = false then
  result := asw.RunExpression(s) else
  result := aswold.RunExpression(s);
end;

function TScarlettActiveScript.RunScript(s: string): boolean;
begin
  fScriptSuccess := true;
  errors.clear;
  fscript := scriptheader.text + s;
  if fUseOldParser = false then
  asw.execute(script) else
  aswold.Execute(script);
  result := ScriptSuccess;
end;

procedure TScarlettActiveScript.aswError(Sender: TObject; Line, Pos: Integer;
  ASrc, ADescription: String);
begin
  fScriptSuccess := false;
  errors.add(inttostr(Line) + ': ' + ADescription + ' [' + ASrc + ']');
  if Assigned(fOnScriptError) then
    fOnScriptError(Line, Pos, ASrc, ADescription);
end;

constructor TScarlettActiveScript.Create(AOwner: TObject);
begin
  inherited Create;
  fActiveScript := Self; // important
  if AOwner <> nil then
    engine := AOwner;

  fUseSafeSubSet := true;  // more SECURITY
  fUseOldParser := false;
  ferrors := TStringList.Create;
  fscriptheader := TStringList.Create;
  asw := TSyActiveScriptWindow.Create(nil);
  asw.OnError := aswError;

  aswold := TXSyActiveScriptWindow.Create(nil);
  aswold.OnError := aswError;

  SetLanguage(cDefaultLanguage);
  SetSafeSubSet(fUseSafeSubSet);
end;

destructor TScarlettActiveScript.Destroy;
begin
  ferrors.Free;
  scriptheader.free;
  aswold.Free;
  asw.free;
  inherited;
end;

end.
