(* ************************************************ *)
(* *)
(* Advanced Encryption Standard (AES) *)
(* Interface Unit v1.0 *)
(* *)
(* *)
(* Copyright (c) 2002 Jorlen Young *)
(* *)
(* *)
(* *)
(* Description: *)
(* *)
(* Based on ElASE.pas unit package *)
(* *)
(* This is the standard interface for an AES encryption algorithm. *)
(* Pass to functions EncryptString and DecryptString *)
(* It is easy to encrypt strings. *)
(* *)
(* Author: Yang Zehui 2004.12.03 *)
(* *)
(* ************************************************ *)

{ 2012.02.02 fixed by catgirlfighter for UNICODE strings }

//2019.08.14 Felipe: added modified encrypt/decrypt stream functions

unit AES;

{$I CatarinkaX.inc}

interface

uses
  SysUtils, Classes, Math, ElAES;

type
  TKeyBit = (kb128, kb192, kb256);

{ function StrToHex(Value: AnsiString): AnsiString;
  function HexToStr(Value: AnsiString): AnsiString; }

function StreamToHex(f: TStream): String;
procedure HexToStream(s: string; f: TStream);

function EncryptString(Value: String; Key: String): String;
function DecryptString(Value: String; Key: String): String;

procedure EncryptStream(Stream, OutStrm: TStream; Key: string;
  KeyBit: TKeyBit = kb256);
procedure DecryptStream(Stream, OutStrm: TStream; Key: string;
  KeyBit: TKeyBit = kb256);

implementation

{ function StrToHex(Value: AnsiString): AnsiString;
  var
  I: Integer;
  begin
  Result := '';
  for I := 1 to Length(Value) do
  Result := Result + IntToHex(Ord(Value[I]), 2);
  end;

  function HexToStr(Value: AnsiString): AnsiString;
  var
  I: Integer;
  begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
  if ((I mod 2) = 1) then
  Result := Result + Chr(StrToInt('0x'+ Copy(Value, I, 2)));
  end;
  end; }

function HexN(s: Char): Byte;
begin
  Result := 0;
  case s of
    // '0': result := 0;
    '1':
      Result := 1;
    '2':
      Result := 2;
    '3':
      Result := 3;
    '4':
      Result := 4;
    '5':
      Result := 5;
    '6':
      Result := 6;
    '7':
      Result := 7;
    '8':
      Result := 8;
    '9':
      Result := 9;
    'A':
      Result := 10;
    'B':
      Result := 11;
    'C':
      Result := 12;
    'D':
      Result := 13;
    'E':
      Result := 14;
    'F':
      Result := 15;
  end;
end;

function StreamToHex(f: TStream): String;
var
  i: integer;
  b: Byte;
begin
  f.Position := 0;
  Result := '';
  for i := 0 to f.Size - 1 do
  begin
    f.ReadBuffer(b, 1);
    Result := Result + IntToHex(b, 2);
  end;
end;

procedure HexToStream(s: string; f: TStream);

var
  i: integer;
  n: Byte;

begin
  s := UPPERCASE(s);
  for i := 1 to length(s) do
    if i mod 2 = 1 then
      n := HexN(s[i]) * 16
    else
    begin
      n := n + HexN(s[i]);
      f.WriteBuffer(n, 1);
    end;
end;

{ Value and Key length must be > 0 }
function EncryptString(Value: String; Key: String): String;
var
  SS, DS: TStringStream;
  AESKey: TAESKey256;
begin
  Result := '';
  SS := TStringStream.Create(Value);
  DS := TStringStream.Create('');
  try
    // DS.WriteBuffer(Size, SizeOf(Size)); WTF???
    FillChar(AESKey, SizeOf(AESKey), 0);
    Move(PChar(Key)^, AESKey, Min(SizeOf(AESKey),
      SizeOf(Key[1]) * length(Key)));
    EncryptAESStreamECB(SS, 0, AESKey, DS);
    Result := StreamToHex(DS);
  finally
    SS.Free;
    DS.Free;
  end;
end;

{ Value and Key length must be > 0 }
function DecryptString(Value: String; Key: String): String;
var
  SS: TStringStream;
  DS: TStringStream;
  AESKey: TAESKey256;
begin
  Result := '';
  SS := TStringStream.Create('');
  HexToStream(Value, SS);
  DS := TStringStream.Create('');
  try
    // SS.ReadBuffer(Size, SizeOf(Size)); WTF???
    FillChar(AESKey, SizeOf(AESKey), 0);
    Move(PChar(Key)^, AESKey, Min(SizeOf(AESKey),
      SizeOf(Key[1]) * length(Key)));
    DecryptAESStreamECB(SS, 0 { SS.Size - SS.Position } , AESKey, DS);
    Result := DS.DataString;
  finally
    SS.Free;
    DS.Free;
  end;
end;

{  -- Stream encryption function defaults to 256-bit key decryption -- }
procedure EncryptStream(Stream, OutStrm: TStream; Key: string;
  KeyBit: TKeyBit = kb256);
var
  Count: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  OutStrm.Write(Count, SizeOf(Count));
  try
    {  -- 128-bit key with a maximum length of 16 characters --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(PChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
      EncryptAESStreamECB(Stream, 0, AESKey128, OutStrm);
    end;
    {  -- 192-bit key with a maximum length of 24 characters --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(PChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
      EncryptAESStreamECB(Stream, 0, AESKey192, OutStrm);
    end;
    {  -- 256-bit key with a maximum length of 32 characters --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(PChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
      EncryptAESStreamECB(Stream, 0, AESKey256, OutStrm);
    end;
  finally
  end;
end;

{  -- Stream decryption function is decrypted by default with 256-bit key --  }
procedure DecryptStream(Stream, OutStrm: TStream; Key: string;
  KeyBit: TKeyBit = kb256);
var
  Count, OutPos: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  Stream.Position := 0;
  OutPos :=OutStrm.Position;
  Stream.ReadBuffer(Count, SizeOf(Count));
  try
    {  -- 128-bit key with a maximum length of 16 characters --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(PChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
      DecryptAESStreamECB(Stream, Stream.Size - Stream.Position,
        AESKey128, OutStrm);
    end;
    {  -- 192-bit key with a maximum length of 24 characters --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(PChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
      DecryptAESStreamECB(Stream, Stream.Size - Stream.Position,
        AESKey192, OutStrm);
    end;
    {  -- 256-bit key with a maximum length of 32 characters --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(PChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
      DecryptAESStreamECB(Stream, Stream.Size - Stream.Position,
        AESKey256, OutStrm);
    end;
    OutStrm.Size := OutPos + Count;
    OutStrm.Position := OutPos;
  finally
  end;
end;

end.
