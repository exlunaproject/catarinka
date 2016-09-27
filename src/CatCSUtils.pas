unit CatCSUtils;

{
  Console Utils
  Useful routines for console applications
  Copyright (c) 2013 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Portions extracted from CRT32 written by Attila Szomor, based on
  Frank Zimmer's freeware CRT32 library
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils;
{$ELSE}
  Windows, Classes, SysUtils;
{$ENDIF}

type
  TCatCSHelper = class
  private
    fLastLnLength: integer;
    fScreenHandle: THandle;
  public
    procedure ResetColor;
    procedure UpdateLn(s: string);
    procedure WriteDot;
    procedure WriteLn_Cyan(s: string);
    procedure WriteLn_Green(s: string);
    procedure WriteLn_Red(s: string);
    procedure WriteLn_White(s: string);
    constructor Create;
    destructor Destroy; override;
  end;

procedure TextColor(Color: Byte);
procedure WriteLnC(s: string; Color: Byte);
procedure WriteC(s: string; Color: Byte);

var
  CS: TCatCSHelper;

implementation

uses
  CatStrings;

{ const
  //Foreground and background color constants of original CRT unit
  bgBlack = 0;
  bgBlue = 1;
  bgGreen = 2;
  bgCyan = 3;
  bgRed = 4;
  bgMagenta = 5;
  bgBrown  = 6;
  bgLightGray = 7;
  //Foreground color constants of original CRT unit
  fgDarkGray = 8;
  fgLightBlue = 9;
  fgLightGreen = 10;
  fgLightCyan = 11;
  fgLightRed = 12;
  fgLightMagenta = 13;
  fgYellow = 14;
  fgWhite = 15;
  //Add-in for blinking of original CRT unit
  Blink = 128; }

const
  csclBLACK = 0;
  csclBLUE = FOREGROUND_BLUE;
  csclGREEN = FOREGROUND_GREEN;
  csclCYAN = FOREGROUND_GREEN + FOREGROUND_BLUE;
  csclRED = FOREGROUND_RED;
  csclMAGENTA = FOREGROUND_RED + FOREGROUND_BLUE;
  csclBROWN = FOREGROUND_RED + FOREGROUND_GREEN;
  csclLIGHTGRAY = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_BLUE;
  csclDARKGRAY = FOREGROUND_INTENSITY;
  csclLIGHTBLUE = FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclLIGHTGREEN = FOREGROUND_GREEN + FOREGROUND_INTENSITY;
  csclLIGHTCYAN = FOREGROUND_GREEN + FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclLIGHTRED = FOREGROUND_RED + FOREGROUND_INTENSITY;
  csclLIGHTMAGENTA = FOREGROUND_RED + FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclYELLOW = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_INTENSITY;
  csclWHITE = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_BLUE +
    FOREGROUND_INTENSITY;

procedure WriteC(s: string; Color: Byte);
begin
  TextColor(Color);
  write(s);
  TextColor(csclLIGHTGRAY); // default console color
end;

procedure WriteLnC(s: string; Color: Byte);
begin
  TextColor(Color);
  writeln(s);
  TextColor(csclLIGHTGRAY); // default console color
end;

procedure TextColor(Color: Byte);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), Color);
end;

procedure TCatCSHelper.UpdateLn(s: string);
var
  CSBufInf: TConsoleScreenBufferInfo;
  garbageIntegerVar: cardinal;
  blankstr: string;
begin
  if fScreenHandle = 0 then
    fScreenHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(fScreenHandle, CSBufInf);
  if length(s) > fLastLnLength then
    fLastLnLength := length(s);
  blankstr := StringofChar(' ', fLastLnLength);
  WriteConsoleOutputCharacter(fScreenHandle,
    {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(s + blankstr),
    length(s + blankstr), CSBufInf.dwCursorPosition, garbageIntegerVar);
end;

procedure TCatCSHelper.ResetColor;
begin
  TextColor(csclLIGHTGRAY); // default console color
end;

procedure TCatCSHelper.WriteLn_Cyan(s: string);
begin
  WriteLnC(s, csclLIGHTCYAN);
end;

procedure TCatCSHelper.WriteLn_Green(s: string);
begin
  WriteLnC(s, csclLIGHTGREEN);
end;

procedure TCatCSHelper.WriteLn_Red(s: string);
begin
  WriteLnC(s, csclLIGHTRED);
end;

procedure TCatCSHelper.WriteLn_White(s: string);
begin
  WriteLnC(s, csclWHITE);
end;

procedure TCatCSHelper.WriteDot;
begin
  System.Write('.');
end;

constructor TCatCSHelper.Create;
begin
  //
end;

destructor TCatCSHelper.Destroy;
begin
  inherited;
end;

initialization

CS := TCatCSHelper.Create;

finalization

CS.Free;

end.
