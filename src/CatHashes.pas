unit CatHashes;
{
  Catarinka - Hash functions

  Copyright (c) 2003-2021 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  If you have D10 Seattle or up, use System.Hash's MD5, otherwise use the
  MD5 function by Stijn Sanders (MIT license, included at the end of this file)
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  {$IFDEF USECROSSVCL}
  WinAPI.Windows,
  {$ENDIF}
  {$IFDEF D10SEATTLE_OR_UP}
  System.Hash,
  {$ENDIF}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

function MD5Hash(const s: string): string;
function MD5Hash_Sanders(s: UTF8String): UTF8String;

function MD5File(const filename:string): string;

function StreamToMD5(m: TStream): String;
function StreamToSHA1(m: TStream): String;

implementation

function MD5File(const filename:string): string;
var
  f: TFileStream;
begin
  f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  with f do
  begin
    try
      Result := StreamToMD5(f);
    except
      Result := emptystr;
    end;
    Free;
  end;
end;

function StreamToMD5(m: TStream): String;
begin
  {$IFDEF D10SEATTLE_OR_UP}
  result := THashMD5.GetHashString(m);
  {$ELSE}
  // TODO...
  result := emptystr;
  {$ENDIF}
end;

function StreamToSHA1(m: TStream): String;
begin
  {$IFDEF D10SEATTLE_OR_UP}
  result := THashSHA1.GetHashString(m);
  {$ELSE}
  // TODO...
  result := emptystr;
  {$ENDIF}
end;

function MD5Hash(const s: string): string;
begin
  {$IFDEF D10SEATTLE_OR_UP}
  result := THashMD5.GetHashString(s);
  {$ELSE}
  result := string(UTF8string(MD5Hash_Sanders(s)));
  {$ENDIF}
end;

{

  Taken from md5.pas v1.0.3
  Copyright 2012-2013 Stijn Sanders
  License: MIT (http://opensource.org/licenses/mit-license.php)

  https://github.com/stijnsanders/TMongoWire/blob/master/mongoAuth.pas

  Based on http://www.ietf.org/rfc/rfc1321.txt

}

function MD5Hash_Sanders(s: UTF8String): UTF8String;
const
  roll1: array [0 .. 3] of cardinal = (7, 12, 17, 22);
  roll2: array [0 .. 3] of cardinal = (5, 9, 14, 20);
  roll3: array [0 .. 3] of cardinal = (4, 11, 16, 23);
  roll4: array [0 .. 3] of cardinal = (6, 10, 15, 21);
  base1: array [0 .. 15] of cardinal = ($D76AA478, $E8C7B756, $242070DB,
    $C1BDCEEE, $F57C0FAF, $4787C62A, $A8304613, $FD469501, $698098D8, $8B44F7AF,
    $FFFF5BB1, $895CD7BE, $6B901122, $FD987193, $A679438E, $49B40821);
  base2: array [0 .. 15] of cardinal = ($F61E2562, $C040B340, $265E5A51,
    $E9B6C7AA, $D62F105D, $02441453, $D8A1E681, $E7D3FBC8, $21E1CDE6, $C33707D6,
    $F4D50D87, $455A14ED, $A9E3E905, $FCEFA3F8, $676F02D9, $8D2A4C8A);
  base3: array [0 .. 15] of cardinal = ($FFFA3942, $8771F681, $6D9D6122,
    $FDE5380C, $A4BEEA44, $4BDECFA9, $F6BB4B60, $BEBFBC70, $289B7EC6, $EAA127FA,
    $D4EF3085, $04881D05, $D9D4D039, $E6DB99E5, $1FA27CF8, $C4AC5665);
  base4: array [0 .. 15] of cardinal = ($F4292244, $432AFF97, $AB9423A7,
    $FC93A039, $655B59C3, $8F0CCC92, $FFEFF47D, $85845DD1, $6FA87E4F, $FE2CE6E0,
    $A3014314, $4E0811A1, $F7537E82, $BD3AF235, $2AD7D2BB, $EB86D391);
  Hex: array [0 .. 15] of AnsiChar = '0123456789abcdef';
var
  a: cardinal;
  dl, i, J, k, l: integer;
  d: array of cardinal;
  g, h: array [0 .. 3] of cardinal;
begin
  a := length(s);
  dl := a + 9;
  if (dl and $3F) <> 0 then
    dl := (dl and $FFC0) + $40;
  i := dl;
  dl := dl shr 2;
  SetLength(d, dl);
  SetLength(s, i);
  J := a + 1;
  s[J] := #$80;
  while J < i do
  begin
    inc(J);
    s[J] := #0;
  end;
  Move(s[1], d[0], i);
  d[dl - 2] := a shl 3;
  h[0] := $67452301;
  h[1] := $EFCDAB89;
  h[2] := $98BADCFE;
  h[3] := $10325476;
  i := 0;
  while i < dl do
  begin
    g := h;
    J := i;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + ((h[(l + 1) and 3] and h[(l + 2) and 3]) or
        (not(h[(l + 1) and 3]) and h[(l + 3) and 3])) + d[J] + base1[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll1[k and 3]) or (a shr (32 - roll1[k and 3])));
      inc(J);
    end;
    J := 1;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + ((h[(l + 3) and 3] and h[(l + 1) and 3]) or
        (not(h[(l + 3) and 3]) and h[(l + 2) and 3])) + d[i or (J and $F)]
        + base2[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll2[k and 3]) or (a shr (32 - roll2[k and 3])));
      inc(J, 5);
    end;
    J := 5;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + (h[(l + 1) and 3] xor h[(l + 2) and 3] xor h[(l + 3) and
        3]) + d[i or (J and $F)] + base3[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll3[k and 3]) or (a shr (32 - roll3[k and 3])));
      inc(J, 3);
    end;
    J := 0;
    for k := 0 to 15 do
    begin
      l := k * 3;
      a := h[l and 3] + (h[(l + 2) and 3] xor (h[(l + 1) and 3] or
        not h[(l + 3) and 3])) + d[i or (J and $F)] + base4[k];
      h[l and 3] := h[(l + 1) and 3] +
        ((a shl roll4[k and 3]) or (a shr (32 - roll4[k and 3])));
      inc(J, 7);
    end;
    for k := 0 to 3 do
      inc(h[k], g[k]);
    inc(i, 16);
  end;
  SetLength(result, 32);
  for k := 0 to 31 do
    result[k + 1] := Hex[h[k shr 3] shr ((k xor 1) shl 2) and $F];
end;

// ------------------------------------------------------------------------//
end.
