{
  Catarinka TCatProgress
  Copyright (c) 2010-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
 
 A custom progress tracker for when the flow is not so linear and you may skip
 steps

  Usage example:
  p := TCatProgress.Create;
  // register your stage names
  p.Add('stage.one');
  p.Add('stage.two');
  p.Add('stage.three');
  p.Add('stage.four');

  // sets the current stage
  p.SetStage('stage.two');

  print(IntToStr(p.Pos)); will print 2
  print(IntToStr(p.Max)); will print 4
  p.free;
}

unit CatProgress;

interface

uses
  Windows, Classes, SysUtils, CatJSON, CatLogger;

type
  TCatProgressOnStageChanged = procedure(const NewStage, LastStage, Duration: string)
    of object;

type
  TCatProgress = class
  private
    fList: TCatJSON;
    fLog: TStringList;
    fMax: integer;
    fPos: integer;
    fStage: string;
    fStageStartTime: TDateTime;
    fStageWatch: TCatStopWatch;
    fOnStageChange: TCatProgressOnStageChanged;
    function GetLog:string;
  public
    procedure Add(const s: string);
    procedure Clear;
    procedure Reset;
    procedure SetStage(const s: string);
    constructor Create;
    destructor Destroy; override;
    // properties
    property Current: string read fStage write SetStage;
    property Log: string read GetLog;
    property Max: integer read fMax;
    property Pos: integer read fPos;
    property OnStageChanged: TCatProgressOnStageChanged read fOnStageChange
      write fOnStageChange;
  end;

implementation

function TCatProgress.GetLog:string;
begin
  result := fLog.text;
end;

procedure TCatProgress.SetStage(const s: string);
var
  i: integer;
  desc:string;
begin
  // check if a stage with this name has been added
  i := fList.GetValue(s, -1);
  if i <> -1 then
  begin
    if (fStage <> EmptyStr) then begin
      desc := ElapsedTimeToFriendlyTime(fStageWatch.Elapsed);
      fLog.Add(fStage+': '+desc);
      if Assigned(fOnStageChange) then
      fOnStageChange(s, fStage, desc);
    end;
    fStageWatch := CatStopWatchNew;
    fStageStartTime := Now;
    fStage := s;
    fPos := i;
  end;
end;

procedure TCatProgress.Add(const s: string);
begin
  Inc(fMax);
  fList.sObject.i[s] := fMax;
end;

procedure TCatProgress.Reset;
begin
  fPos := 0;
  fStage := EmptyStr;
  fLog.Clear;
  fStageWatch.Stop;
end;

procedure TCatProgress.Clear;
begin
  fMax := 0;
  fList.Clear;
  fLog.Clear;
  fStageWatch.Stop;
end;

constructor TCatProgress.Create;
begin
  fMax := 0;
  fList := TCatJSON.Create;
  fLog := TStringList.Create;
  Reset;
end;

destructor TCatProgress.Destroy;
begin
  fList.Free;
  FLog.Free;
  inherited;
end;

end.
