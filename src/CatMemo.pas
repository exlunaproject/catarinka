unit CatMemo;

{
  Catarinka TLogMemo
  Copyright (c) 2015-2016 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Vcl.StdCtrls;
{$ELSE}
  StdCtrls;
{$ENDIF}

type
  TCatLogMemo = class(TMemo)
  private
  public
    procedure WriteLn(const s: string);
    procedure Write(const s: string);
  end;

implementation

uses
  CatStrings;

procedure TCatLogMemo.WriteLn(const s: string);
begin
  lines.Text := lines.Text + s + crlf;
end;

procedure TCatLogMemo.Write(const s: string);
begin
  lines.Text := lines.Text + s;
end;

end.
