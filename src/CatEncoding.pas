{
  Catarinka Encoding utils
  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

unit CatEncoding;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

function IsFileUTF8(const filename: String): Boolean;
function IsFileUTF8_BOM(const filename: string): Boolean;

implementation

{ TCatStringList }

// Returns true if the file is UTF8, false otherwise
// Thanks to Martin Holmes who crafted this FileMayBeUTF8() function based on
// examples from an old post at http://mail.nl.linux.org/linux-utf8/1999-09/msg00110.html
function IsFileUTF8(const filename: String): Boolean;
var
  Stream: TMemoryStream;
  BytesRead: integer;
  ArrayBuff: array [0 .. 127] of byte;
  PreviousByte: byte;
  i: integer;
  YesSequences, NoSequences: integer;

begin
  YesSequences := 0;
  NoSequences := 0;
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(filename);
    repeat

      { read from the TMemoryStream }

      BytesRead := Stream.Read(ArrayBuff, High(ArrayBuff) + 1);
      { Do the work on the bytes in the buffer }
      if BytesRead > 1 then
      begin
        for i := 1 to BytesRead - 1 do
        begin
          PreviousByte := ArrayBuff[i - 1];
          if ((ArrayBuff[i] and $C0) = $80) then
          begin
            if ((PreviousByte and $C0) = $C0) then
            begin
              inc(YesSequences)
            end
            else
            begin
              if ((PreviousByte and $80) = $0) then
                inc(NoSequences);
            end;
          end;
        end;
      end;
    until (BytesRead < (High(ArrayBuff) + 1));
    // Below, >= makes ASCII files = UTF-8, which is no problem.
    // Simple > would catch only UTF-8;
    Result := (YesSequences >= NoSequences);

  finally
    Stream.Free;
  end;
end;

// Detects UTF8 BOM (byte order mark) in file
// Many thanks to dummzeuch (https://blub.dummzeuch.de/) for the recipe
function IsFileUTF8_BOM(const filename: string): Boolean;
const
  BOM_LENGTH = 3;
var
  st: TMemoryStream;
  Buffer: array [0 .. BOM_LENGTH - 1] of byte;
begin
  Result := false;
  st := TMemoryStream.Create;
  try
    st.LoadFromFile(filename);
    st.Position := 0;
    if BOM_LENGTH = st.Read(Buffer, BOM_LENGTH) then
    begin
      // the file contains at least BOM_LENGTH bytes
      if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
      begin
        // we have a BOM
        Result := true;
      end
      else
      begin
        // no BOM
        Result := false;
      end;
    end;
  finally
    st.Free;
  end;
  if IsFileUTF8(filename) = false then
    Result := false;
end;

end.
