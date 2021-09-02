unit CatDDV;
{
  DDV (Double Dashed Values) Format
  Copyright (c) 2013-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Includes TDashedRecord - for creating, writing and reading to a DDV record line

  I created the DDV format as a more compact, easier to visualize and flexible
  alternative to the widely adopted CSV format. I said good-bye to the
  CSV format and started using just DDV.

  A COMPARISON BETWEEN DDV AND CSV

  DDV:
  - Uses CSV for storing name-value pairs after a special separator, but is NOT
    defined by columns. Example: 
    masterstring --a=123,b=456
  - Allows records to be defined fully, partially or to be entirely omitted
    Values are appended after the spaced-two-dash " --" special separator, which
    can be omitted if there are no values defined
  - Alphabetic sorting is flawless
  - Better data visualization and uses less disk/memory space if you have a
    large quantity of records with undefined values
  - Faster processing if you need to handle the master string before reading the
    values

  CSV:
  - Organized by fixed columns. Example:
    masterstring,123,456,etc
  - Columns must be separated by comma even if the column value is undefined
  - Sometimes breaks alphabetic sorting in CSV file (see example below)
  - Difficult visualization when you have multiple undefined values (",,,,"...
    and you do not know anymore which column is which unless you open it with a
    visualization tool like Excel)
  - Easier to remove a column or merge multiple files

  Both DDV and CSV do not support CRLF (line breaks).

  EXAMPLES

  With DDV a CSV is represented like:

  DDV:
  # --y=Year,m=Make,md=Model,d=Description
  Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"
  Thomas Anderson --y=2000,m=Mercury,md=Cougar
  John

  CSV:
  Owner,Year,Make,Model,Description
  "Henry Ford",1997,Ford,E350,"Super luxurious truck"
  "Thomas Anderson",2000,Mercury,Cougar,
  John,,,,
  
  With DDV:
    masterstring --a=123,b=456
      is the same as:
    masterstring --b=456,a=123

    masterstring --a=,b=
      is the same as:
    masterstring 

  DDV also makes alphabetic sorting easier.

  DDV when alphabetized is (perfect!):
  # --y=Year,m=Make,md=Model,d=Description
  Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"
  John
  Thomas Anderson --y=2000,m=Mercury,md=Cougar

  CSV when alphabetized is (fail!):
  "Henry Ford",1997,Ford,E350,"Super luxurious truck"
  "Thomas Anderson",2000,Mercury,Cougar,
  John,,,,

  Use TDashedRecord to create a DDV record:

  d := TDashedRecord.Create;
  d.Master := 'Henry Ford';
  d.WriteInteger('y',1997);
  d.WriteString('m','Ford');
  d.WriteString('md','E350');
  d.WriteString('d','Super luxurious truck');
  WriteLn(d.Text);

  This will print:
  Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"
  
  Use the Text property to load a DDV record:
  d := TDashedRecord.Create;
  d.text := 'Thomas Anderson --y=2000,m=Mercury,md=Cougar';
  WriteLn(d.ReadString('m',emptystr));
  d.Free;
  This will print Mercury
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

const
  cDDV_DefaultSeparator = ' --';

type
  TDashedRecord = class
  private
    fMasterValue:string;
    fPreserveZero:boolean;
    fRecord:TStringList;
    fSeparator:string;
    function GetDashedText:string;
    function ReadString_(const name:string):string;
    function ReadInteger_(const name:string):int64;
    function ReadBool_(const name:string):boolean;
    function GetValuesCount:int64;
    procedure SetDashedText(const DDV: string);
  public
    // Read* and Write* methods will get or set a value that you can also get or
    // set through the Values* properties
    // However, be aware that ValuesI returns a defaultvalue of 0 if the named value is
    // empty and ValuesB returns false in such cases
    function ReadString(const name,DefaultValue:string):string;
    function ReadInteger(const name:string;DefaultValue:int64):int64;
    function ReadBool(const name:string;DefaultValue:boolean):boolean;
    procedure WriteString(const name: string; const value: string);
    procedure WriteInteger(const name:string;const Value:int64);
    procedure WriteBool(const name:string;const Value:boolean);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    // properties
    property Master: string read fMasterValue write fMasterValue;
    property Separator: string read fSeparator write fSeparator;
    property Text: string read GetDashedText write SetDashedText;
    property ValuesS[const name: string]:string read ReadString_ write WriteString; default;
    property ValuesI[const name: string]:int64 read ReadInteger_ write WriteInteger;
    property ValuesB[const name: string]:boolean read ReadBool_ write WriteBool;
    property ValuesCount:int64 read GetValuesCount;
  end;

implementation

uses CatStrings;

function TDashedRecord.GetValuesCount:int64;
begin
  result := fRecord.Count;
end;

procedure TDashedRecord.WriteInteger(const name:string;const Value:int64);
var s:string;
begin
  s := IntToStr(value);
  if (value = 0) and (fPreserveZero = false) then
  s := emptystr;
  WriteString(name,s);
end;

function TDashedRecord.ReadInteger(const name:string;DefaultValue:int64):int64;
begin
  result := StrToIntDef(ReadString_(name),DefaultValue);
end;

procedure TDashedRecord.WriteBool(const name:string;const Value:boolean);
begin
  if value = true then
   WriteInteger(name,1) else
   WriteInteger(name,0);
end;

function TDashedRecord.ReadBool(const name:string;DefaultValue:boolean):boolean;
var
  s : string;
begin
  s := ReadString_(name);
  if s = emptystr then
    result := DefaultValue else
    result := StrToBool(s);
end;

function TDashedRecord.ReadInteger_(const name:string):int64;
begin
  result := ReadInteger(name, 0);
end;

function TDashedRecord.ReadString(const name,defaultvalue:string):string;
begin
  result := ReadString_(name);
  if result = emptystr then
    result := defaultvalue;
end;

function TDashedRecord.ReadString_(const name:string):string;
begin
  result := fRecord.Values[name];
end;

function TDashedRecord.ReadBool_(const name:string):boolean;
begin
  result := ReadBool(name,false);
end;

procedure TDashedRecord.WriteString(const name: string; const value: string);
var
  emptyidx:int64;
begin
  fRecord.Values[name] := value;
  if value = emptystr then begin
    // deletes empty value from list
    emptyidx := fRecord.IndexOf(name+'=');
    if fRecord.IndexOf(name+'=') <> -1 then
    fRecord.Delete(emptyidx);
  end;
end;

procedure TDashedRecord.SetDashedText(const ddv: string);
begin
  fRecord.Clear;
  fMasterValue := emptystr;
  if pos(fSeparator,ddv) <> 0 then begin
    fMasterValue := Before(ddv,fSeparator);
    fRecord.CommaText := After(ddv,fSeparator);
  end;
end;

function TDashedRecord.GetDashedText:string;
begin
  result := fMasterValue;
  if trim(fRecord.CommaText) <> emptystr then
   result := fMasterValue+fSeparator+fRecord.CommaText;
end;

// Clears any values
procedure TDashedRecord.Clear;
begin
  fMasterValue := emptystr;
  fRecord.Clear;
end;

constructor TDashedRecord.Create;
begin
  inherited Create;
  fSeparator := cDDV_DefaultSeparator;
  fRecord := TStringList.Create;
  fPreserveZero := false;
end;

destructor TDashedRecord.Destroy;
begin
  fRecord.Free;
  inherited;
end;

// ------------------------------------------------------------------------//
end.
