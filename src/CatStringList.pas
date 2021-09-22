{
  Catarinka String list
  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This TStringList alternative includes support for:
  * loading UTF8 encoded files that contain invalid characters
  * flushing the list to a file if it reaches a desired count when adding a
  string (FlushAdd method)
}

unit CatStringList;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  CatEncoding, CatStrings;

type
  TCatStringList = class(TStringList)
  protected
   {$IFDEF UNICODE}
    procedure LoadFromFile(const filename: string); override;
    procedure LoadFromFile(const filename: string;
      encoding: TEncoding); override;
    {$ENDIF}
  private
    fFlushFilename: string;
    fFlushEmptyLine: boolean;
    fFlushLineByLine: boolean;
    {$IFDEF UNICODE}
    procedure LoadFromFileUTF8(const filename: string);
    {$ENDIF}
    function CanFlushString(const s:string):boolean;
  public
    constructor Create;
    procedure FlushAdd(const s:string;const maxcount:int64);
    procedure FlushToFile;
    property FlushFilename:string read fFlushFilename write fFlushFilename;
    property FlushEmptyLine:boolean read fFlushEmptyLine write fFlushEmptyLine;
    property FlushLineByLine:boolean read fFlushLineByLine write fFlushLineByLine;
  end;

implementation

{ TCatStringList }

function FlushLines(const filename: TFilename; const s: String): boolean;
var
  f: Textfile;
begin
  try
    AssignFile(f, filename);
    if FileExists(filename) = false then
      ReWrite(f)
    else
    begin
      Reset(f);
      Append(f);
    end;
    WriteLn(f, s);
    Closefile(f);
    Result := true;
  except
    Result := false;
  end;
end;

constructor TCatStringList.Create;
begin
  inherited Create;
  fFlushFilename := emptystr;
  fFlushLineByLine := true;
  fFlushEmptyLine := false;
end;

function TCatStringList.CanFlushString(const s:string):boolean;
begin
  result := true;
  if fFlushEmptyLine = false then begin
    if trim(s) = emptystr then
    result := false;
    if trim(s) = crlf then
    result := false;
  end;
end;

// Saves all lines to a file indicated in the FlushFilename property
procedure TCatStringList.FlushToFile;
var
  i:integer;
  outtext:string;
begin
  if fFlushFilename = emptystr then
    Exit;
  if fFlushLineByLine = true then begin
    for i := 0 to Count - 1 do
      if CanFlushString(self[i]) = true then
        FlushLines(fFlushFilename, self[i]);
  end else begin
    outtext := Text;
    if endswith(outtext, crlf) = true then
      outtext := RemoveLastChar(RemoveLastChar(outtext));
    if CanFlushString(outtext) = true then
      FlushLines(fFlushFilename, outText);
  end;
  Clear;
end;

// Add the string to the list, flushing the contents to a file if reaches 
// the indicated max lines count
procedure TCatStringList.FlushAdd(const s:string; const maxcount:int64);
begin
  Add(s);
  if count > maxcount then
    FlushToFile; 
end;

{$IFDEF UNICODE}
// Workaround for rare invalid character error with bad UTF8 Signature encoded 
// files
procedure TCatStringList.LoadFromFileUTF8(const filename: string);
var
  enc: TUTF8Encoding;
begin
  enc := TUTF8Encoding.Create(CP_UTF8, 0, 0);
  inherited LoadFromFile(filename, enc);
  enc.Free;
end;

procedure TCatStringList.LoadFromFile(const filename: string);
begin
  // Use standard LoadFromFile only if not UTF8 file
  if IsFileUTF8(filename) = true then
    LoadFromFileUTF8(filename)
  else
    inherited LoadFromFile(filename);
end;

procedure TCatStringList.LoadFromFile(const filename: string;
  encoding: TEncoding);
begin
  // Call standard LoadFromFile only if not UTF8 encoding specified
  if encoding = TEncoding.UTF8 then
    LoadFromFileUTF8(filename)
  else
    inherited LoadFromFile(filename, encoding)
end;
{$ENDIF}

end.
