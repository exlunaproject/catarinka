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

{$DEFINE USEWACEF}
// If USECEFBETA is defined, WACEF for CEF v3.2357 will be required
// {$DEFINE USECEFBETA}