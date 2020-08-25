{$IFDEF FPC}
 {$mode DELPHI}{$H+}
{$ELSE}
 {$IF CompilerVersion >= 30} // Delphi 10 Seattle or higher
  {$DEFINE D10SEATTLE_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 29} // Delphi XE8 or higher
  {$DEFINE DXE8_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 28} // Delphi XE7 or higher
  {$DEFINE DXE7_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 23} // Delphi XE2 or higher
  {$DEFINE DXE2_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 24} // Delphi XE3 or higher
  {$DEFINE DXE3_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 15} // Delphi 7 or higher
  {$DEFINE D7_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 14} // Delphi 6 or higher
  {$DEFINE D6_OR_UP}
 {$IFEND}
 {$IF CompilerVersion < 20} // Before D2009
  {$DEFINE CHARINSET_UNAVAILABLE}
 {$IFEND}
{$ENDIF}

 ///{$DEFINE USEWACEF}

{$IFDEF CPUX32}
  {$DEFINE CANUSECEFLOADERROR}
{$ENDIF}

 {$IFDEF CPUX64}
  // OnLoadError was causing a crash when using cef_binary_3.2454.1344.g2782fb8_windows64
  // https://github.com/hgourvest/dcef3/issues/12#issuecomment-158649005
  {$DEFINE CANUSECEFLOADERROR}
{$ENDIF}

{$I CatarinkaX.inc}
