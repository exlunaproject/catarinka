unit CatTime;
{
  Catarinka - Useful time-related functions

  Copyright (c) 2003-2012 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, Vcl.Controls;
{$ELSE}
  SysUtils, Controls, Windows;
{$ENDIF}

const
 cMonthNamesLongEN: array[1..12] of string = ('January', 'February', 'March', 'April',
   'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
 cMonthNamesShortEN: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');


type
  TCatDecodedDate = record
    AsString: string;
    AsUnixtime: Int64;
    AsDateTime: TDateTime;
    Year: word;
    Month: word;
    Day: word;
  end;

type
  TCatDateRec = record
    Day:integer;
    Month:integer;
    Year:integer;
    AsString:string;
  end;

function CalcAge(const StartDate, Date: TDate): integer;
function DateTimeToUnix(const Date: TDateTime): Int64;
function DescribeDuration(const StartTime: TDateTime): string;
function DescribePassedDays(const aNow, aThen: TDateTime): string;
function DescribePassedDateTime(const starttime: TDateTime): string;
function DescribePassedTime(const t: string): string;
function DiffDate(const day1, day2: TDateTime): integer;
function ExtractDateFromTokenStr(const d,m,y:integer;s:string;sep:string=' '):TCatDateRec;
function GetDayOfWeekAsNumber: integer;
function GetDayOfWeekAsText: string;
function GetDecodedDate(const d: TDateTime):TCatDecodedDate;
function IsValidDate(const S: string; const format: string = 'mm/dd/yyyy';
  const sep: Char = '/'): boolean;
function UnixToDateTime(const sec: Int64): TDateTime;

implementation

uses CatStrings;

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

function DateTimeToUnix(const Date: TDateTime): Int64;
begin
  Result := Round((Date - UnixStartDate) * 86400);
end;

function DescribeDuration(const StartTime: TDateTime): string;
begin
  if trunc(StartTime) <> 0 then
    result := replacestr(DescribePassedDateTime(StartTime), ' ago', emptystr)
  else
    result := 'Not started.';
end;

function DescribePassedDays(const aNow, aThen: TDateTime): string;
var
  dif: integer;
begin
  dif := DiffDate(aThen, aNow);
  case dif of
    0:
      Result := 'Today';
    1:
      Result := 'Yesterday';
  else
    Result := IntToStr(dif) + ' days ago'
  end;
end;

function DescribePassedDateTime(const starttime: TDateTime): string;
const
  timeformat = 'hh:nn:ss'; // 24h
  dateformat = 'ddd, dd mmm yyyy';
var
  Date, time: string;
begin
  Date := FormatDateTime(dateformat, starttime);
  time := FormatDateTime(timeformat, starttime);
  if FormatDateTime(dateformat, now) = Date then
    Result := DescribePassedTime(time)
  else
    Result := DescribePassedDays(now, starttime);
end;

function DescribePassedTime(const t: string): string;
  function TimeExt(n: string; S: string; p: string): string;
  begin
    if n = '1' then
      Result := n + ' ' + S
    else
      Result := n + ' ' + p;
  end;

var
  h, m, S, ms: string;
  t1, t2, ft: ttime;
const
  zero = '0';
begin
  t2 := now;
  t1 := strtotime(t);
  ft := t2 - t1;
  h := FormatDateTime('h', ft);
  m := FormatDateTime('n', ft);
  S := FormatDateTime('s', ft);
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
      if S <> zero then
        Result := TimeExt(S, 'second ago', 'seconds ago')
      else
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

// Extracts a date from a tokenized string on which you know the position of day, month
// and year.
// Example:
//   ExtractDateFromTokenStr(1,2,3, '21, June 2021 ...')
//   returns a record with the day, month and year
function ExtractDateFromTokenStr(const d,m,y:integer;s:string;sep:string=' '):TCatDateRec;
  function Extract(const tknum:integer;ismonth:boolean=false):integer;
  var
    tk, sn : string;
  begin
    tk := GetToken(s,sep,tknum);
    sn := ExtractNumbers(tk);
    result := StrToIntDef(sn,0);
    if ismonth then begin
      if MatchStrInArrayIdx(tk,cMonthNamesLongEN) <> 0 then
        result := MatchStrInArrayIdx(tk,cMonthNamesLongEN);
      if MatchStrInArrayIdx(tk,cMonthNamesShortEN) <> 0 then
        result := MatchStrInArrayIdx(tk,cMonthNamesShortEN);
    end;
  end;
begin
  result.day := Extract(d);
  result.month := Extract(m, true);
  result.year := Extract(y);
  result.asstring := CatPadLeft(IntToStr(result.year),'0',4);
  result.asstring := result.asstring+'-'+CatPadLeft(IntToStr(result.month),'0',2);
  result.asstring := result.asstring+'-'+CatPadLeft(IntToStr(result.day),'0',2);
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

function GetDecodedDate(const d: TDateTime):TCatDecodedDate;
begin
  result.AsDateTime := d;
  DecodeDate(d, result.Year, result.Month, result.Day);
  result.AsString := DateTimeToStr(d);
  result.AsUnixtime := DateTimeToUnix(d);
end;

function IsValidDate(const S: string; const format: string = 'mm/dd/yyyy';
  const sep: Char = '/'): boolean;
var
  dt: TDateTime;
  fs: TFormatSettings;
begin
  fs.ShortDateFormat := format;
  fs.DateSeparator := sep;
  if TryStrToDate(S, dt, fs) then
    Result := true
  else
    Result := false;
end;

function UnixToDateTime(const sec: Int64): TDateTime;
begin
  Result := (sec / 86400) + UnixStartDate;
end;

// ------------------------------------------------------------------------//
end.
