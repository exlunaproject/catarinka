unit CatTime;
{
  Catarinka - Useful time-related functions

  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, Vcl.Controls;
{$ELSE}
  SysUtils, Controls;
{$ENDIF}
function CalcAge(const StartDate, Date: TDate): integer;
function DateTimeToUnix(const Date: TDateTime): Longint;
function DescribeDateDiff(const t, d: string): string;
function DescribePassedTime(const starttime: TDateTime): string;
function DescribeTimeDiff(const t: string): string;
function DiffDate(const day1, day2: TDateTime): integer;
function GetDayOfWeekAsNumber: integer;
function GetDayOfWeekAsText: string;
function IsValidDate(const S: string;const format:string='mm/dd/yyyy';const sep:Char='/'): boolean;
function UnixToDateTime(const sec: Longint): TDateTime;

implementation

const
  UnixStartDate: TDateTime = 25569.0; // 01/01/1970

function CalcAge(const StartDate, Date: TDate): integer;
var
  d, m, y: Word;
  ds, ms, ys: Word;
  age: integer;
begin
  Result := 0;
  if not(Date > StartDate) then
    Exit;
  DecodeDate(Date, y, m, d);
  DecodeDate(StartDate, ys, ms, ds);
  age := y - ys;
  if m > ms then
    Result := age
  else
  begin
    if m < ms then
      Result := age - 1
    else
    begin
      if d >= ds then
        Result := age
      else
        Result := age - 1
    end
  end;
end;

function DateTimeToUnix(const Date: TDateTime): Longint;
begin
  Result := Round((Date - UnixStartDate) * 86400);
end;

function DescribeDateDiff(const t, d: string): string;
var
  dif: integer;
  d1, d2: TDate;
begin
  d1 := Date;
  d2 := strtodate(d);
  dif := trunc(d1) - trunc(d2);
  if dif = 1 then
    Result := t + ' Yesterday'
  else
    Result := t + ' ' + d;
end;

function DescribePassedTime(const starttime: TDateTime): string;
const
  timeformat = 'hh:nn:ss'; // 24h
  dateformat = 'ddd, dd mmm yyyy';
var
  Date, time: string;
begin
  Date := FormatDateTime(dateformat, starttime);
  time := FormatDateTime(timeformat, starttime);
  if FormatDateTime(dateformat, now) = Date then
    Result := DescribeTimeDiff(time)
  else
    Result := DescribeDateDiff(time, datetostr(starttime));
end;

function DescribeTimeDiff(const t: string): string;
  function TimeExt(n: string; s: string; p: string): string;
  begin
    if n = '1' then
      Result := n + ' ' + s
    else
      Result := n + ' ' + p;
  end;

var
  h, m, s, ms: string;
  t1, t2, ft: ttime;
const
  zero = '0';
begin
  t2 := now;
  t1 := strtotime(t);
  ft := t2 - t1;
  h := FormatDateTime('h', ft);
  m := FormatDateTime('n', ft);
  s := FormatDateTime('s', ft);
  ms := FormatDateTime('z', ft);
  if h <> zero then
  begin
    Result := 'about ' + TimeExt(h, 'hour ago', 'hours ago');
  end
  else
  begin
    if m <> zero then
    begin
      Result := TimeExt(m, 'minute ago', 'minutes ago');
    end
    else
    begin
      if s <> zero then
        Result := TimeExt(s, 'second ago', 'seconds ago') else
        Result := TimeExt(ms, 'millisecond ago', 'milliseconds ago');
    end;
  end;
end;

function DiffDate(const day1, day2: TDateTime): integer;
var
  diff: double;
begin
  diff := day2 - day1;
  Result := Round(diff);
end;

function GetDayOfWeekAsNumber: integer;
var
  d: TDateTime;
begin
  d := now;
  Result := DayOfWeek(d);
end;

function GetDayOfWeekAsText: string;
var
  d: TDateTime;
begin
  d := now;
  case DayOfWeek(d) of
    1:
      Result := 'Sunday';
    2:
      Result := 'Monday';
    3:
      Result := 'Tuesday';
    4:
      Result := 'Wednesday';
    5:
      Result := 'Thursday';
    6:
      Result := 'Friday';
    7:
      Result := 'Saturday';
  end;
end;

function IsValidDate(const S: string;const format:string='mm/dd/yyyy';const sep:Char='/'): boolean;
var
  dt: TDateTime;
  fs: TFormatSettings;
begin
  fs.ShortDateFormat := format;
  fs.DateSeparator := sep;
  if TryStrToDate(s, dt, fs) then
    result:=true
  else
    result:=false;
end;

function UnixToDateTime(const sec: Longint): TDateTime;
begin
  Result := (sec / 86400) + UnixStartDate;
end;

// ------------------------------------------------------------------------//
end.
