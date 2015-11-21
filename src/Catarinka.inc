{$IFDEF FPC}
 {$mode DELPHI}{$H+}
{$ELSE}
 {$IF CompilerVersion >= 23} // Delphi XE2 or higher
  {$DEFINE DXE2_OR_UP}
 {$IFEND}
 {$IF CompilerVersion >= 24} // Delphi XE3 or higher
  {$DEFINE DXE3_OR_UP}
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
