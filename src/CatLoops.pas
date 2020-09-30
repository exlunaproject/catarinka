{
  Catarinka Quick Loopers
  Copyright (c) 2003-2019 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Usage examples:
  LoopStringList(memo1.Lines,
  procedure(i:integer;s:string) begin
  memo2.Lines.Add(s);
  end
  );
  LoopStringListEx(memo1.Lines,
  procedure(const line:TCatStringLoopLine) begin
  memo2.Lines.Add(line.value);
  end
  );

  For more advanced loopers, check the CatStringLoop unit.
}

unit CatLoops;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  CatStrings;

type
  TCatStringLoopLine = record
    index: integer;
    count: integer;
    value: string;
  end;

  TCatStringLoopProc<T> = reference to procedure
    (const line: TCatStringLoopLine);

procedure LoopStringList(sl: TStrings; const proc: TProc<integer, string>);
procedure LoopStringListEx(sl: TStrings;
  const proc: TCatStringLoopProc<TCatStringLoopLine>);

implementation

procedure LoopStringList(sl: TStrings; const proc: TProc<integer, string>);
var
  i, c: integer;
begin
  c := sl.count;
  for i := 0 to c do
  begin
    if i < c then
    begin
      if Assigned(proc) then
        proc(i, sl[i]);
    end;
  end;
end;

procedure LoopStringListEx(sl: TStrings;
  const proc: TCatStringLoopProc<TCatStringLoopLine>);
var
  i, c: integer;
  l: TCatStringLoopLine;
begin
  c := sl.count;
  for i := 0 to c do
  begin
    if i < c then
    begin
      l.index := i;
      l.value := sl[i];
      l.count := c;
      if Assigned(proc) then
        proc(l);
    end;
  end;
end;

end.
