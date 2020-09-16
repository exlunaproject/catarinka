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
  TScarlettActiveScript = class
  private
  public
    engine: TObject;
    asw: TSyActiveScriptWindow;
    asw_success: boolean;
    scriptheader: tstringlist;
    errors: tstringlist;
    script: string;
    constructor Create(AOwner: TObject); // overload;
    destructor Destroy; override;
    procedure aswError(Sender: TObject; Line, Pos: Integer;
      ASrc, ADescription: String);
    function RunScript(s: string): boolean;
    function RunExpression(s: string): string;
  end;

var
  FActiveScript: TScarlettActiveScript;

const
  crlf = #13 + #10;

implementation

function TScarlettActiveScript.RunExpression(s: string): string;
begin
  asw_success := true;
  errors.clear;
  result := asw.RunExpression(s);
end;

function TScarlettActiveScript.RunScript(s: string): boolean;
begin
  asw_success := true;
  errors.clear;
  script := scriptheader.text + s;
  asw.execute(script);
  result := asw_success;
end;

procedure TScarlettActiveScript.aswError(Sender: TObject; Line, Pos: Integer;
  ASrc, ADescription: String);
begin
  asw_success := false;
  errors.add(inttostr(Line) + ': ' + ADescription + ' [' + ASrc + ']');
  // Showmessage(ADescription + ': ' + ASrc);
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
