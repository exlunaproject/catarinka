unit CatJSRunnerAS;
{
  Catarinka ActiveScript Executor (from Syhunt Scarlett project)
  Copyright (c) 2013-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, CatActiveScript, SysUtils;

type
  TScarlettActiveScriptError = procedure(Line, Pos: Integer;
    ASrc, ADescription: String) of object;

type
  TScarlettActiveScript = class
  private
    fasw_success: boolean;
    fOnScriptError: TScarlettActiveScriptError;
    procedure aswError(Sender: TObject; Line, Pos: Integer;
      ASrc, ADescription: String);
  public
    engine: TObject;
    asw: TSyActiveScriptWindow;
    scriptheader: tstringlist;
    errors: tstringlist;
    script: string;
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function RunScript(s: string): boolean;
    function RunExpression(s: string): string;
    // properties
    property asw_success: boolean read fasw_success;
    property OnScriptError: TScarlettActiveScriptError read fOnScriptError
      write fOnScriptError;
  end;

var
  FActiveScript: TScarlettActiveScript;

const
  crlf = #13 + #10;

implementation

function TScarlettActiveScript.RunExpression(s: string): string;
begin
  fasw_success := true;
  errors.clear;
  result := asw.RunExpression(s);
end;

function TScarlettActiveScript.RunScript(s: string): boolean;
begin
  fasw_success := true;
  errors.clear;
  script := scriptheader.text + s;
  asw.execute(script);
  result := fasw_success;
end;

procedure TScarlettActiveScript.aswError(Sender: TObject; Line, Pos: Integer;
  ASrc, ADescription: String);
begin
  fasw_success := false;
  errors.add(inttostr(Line) + ': ' + ADescription + ' [' + ASrc + ']');
  if Assigned(fOnScriptError) then
    fOnScriptError(Line, Pos, ASrc, ADescription);
end;

constructor TScarlettActiveScript.Create(AOwner: TObject);
begin
  inherited Create;
  FActiveScript := Self; // important
  if AOwner <> nil then
    engine := AOwner;
  errors := tstringlist.Create;
  scriptheader := tstringlist.Create;
  asw := TSyActiveScriptWindow.Create(nil);
  asw.ScriptLanguage := 'JScript';
  asw.UseSafeSubset := true; // more SECURITY
  asw.OnError := aswError;
end;

destructor TScarlettActiveScript.Destroy;
begin
  scriptheader.free;
  asw.free;
  inherited;
end;

end.
