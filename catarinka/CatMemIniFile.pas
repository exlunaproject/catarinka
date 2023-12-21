{
  Catarinka MemoryIni File extension
  Copyright (c) 2003-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This TMemIniFile alternative includes support for:
  * reading SectionValues to a string.
}

unit CatMemInifile;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.IniFiles,
{$ELSE}
  Classes, IniFiles,
{$ENDIF}
  CatStrings, CatStringList;

type
  TCatMemIniFile = class(TMemIniFile)
  protected
  private
    function GetText:string;
  public
    procedure LoadFromFile(const filename:string);
    procedure LoadFromString(const str:string);
    procedure SaveToFile(const filename:string);
    function ReadSectionValuesAsString(const Section:string):string;
    constructor Create;
    property Text: string read GetText write LoadFromString;
  end;

implementation

{ TCatMemIniFile }

function TCatMemIniFile.GetText:string;
var
  sl:TCatStringList;
begin
  sl := TCatStringList.Create;
  GetStrings(sl);
  result:= sl.Text;
  sl.Free;
end;

procedure TCatMemIniFile.LoadFromString(const str:string);
var sl:TCatStringList;
begin
  sl := TCatStringList.Create;
  sl.text := str;
  SetStrings(sl);
  sl.Free;
end;

procedure TCatMemIniFile.LoadFromFile(const filename:string);
var sl:TCatStringList;
begin
  sl := TCatStringList.Create;
  sl.LoadFromFile(filename);
  SetStrings(sl);
  sl.Free;
end;

procedure TCatMemIniFile.SaveToFile(const filename:string);
var
  sl:TCatStringList;
begin
  sl := TCatStringList.Create;
  GetStrings(sl);
  sl.SaveToFile(filename);
  sl.Free;
end;

function TCatMemIniFile.ReadSectionValuesAsString(const Section:string):string;
var sl:TStringList;
begin
  sl := TCatStringList.create;
  ReadSectionValues(Section, sl);
  result := sl.text;
  sl.Free;
end;

constructor TCatMemIniFile.Create;
begin
  inherited Create('');
end;

end.
