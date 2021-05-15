{
  Catarinka String list
  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This TStringList alternative includes support for loading UTF8 Signature
  encoded files
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
  CatEncoding;

type
  TCatStringList = class(TStringList)
  protected
   {$IFDEF UNICODE}
    procedure LoadFromFile(const filename: string); override;
    procedure LoadFromFile(const filename: string;
      encoding: TEncoding); override;
    {$ENDIF}
  private
    {$IFDEF UNICODE}
    procedure LoadFromFileUTF8(const filename: string);
    {$ENDIF}
  public
  end;

implementation

{ TCatStringList }

{$IFDEF UNICODE}
// Workaround for rare UTF8 Signature related error
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
  // Let LoadFromFile use the default encoding only if not UTF8 file
  if IsFileUTF8(filename) = true then
    LoadFromFileUTF8(filename)
  else
    inherited LoadFromFile(filename);
end;

procedure TCatStringList.LoadFromFile(const filename: string;
  encoding: TEncoding);
begin
  // Call standard LoadFromFile only if not UTF8 encoding
  if encoding = TEncoding.UTF8 then
    LoadFromFileUTF8(filename)
  else
    inherited LoadFromFile(filename, encoding)
end;
{$ENDIF}

end.
