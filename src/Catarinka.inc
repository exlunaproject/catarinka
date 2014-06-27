{$IFDEF FPC}
 {$mode DELPHI}{$H+}
{$ELSE}
 {$IF CompilerVersion >= 23} // Delphi XE2 or higher
  {$DEFINE DXE2_OR_UP}
 {$IFEND}
 {$IF CompilerVersion < 20} // Before D2009
  {$DEFINE CHARINSET_UNAVAILABLE}
 {$IFEND}
{$ENDIF}