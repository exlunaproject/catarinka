# Catarinka

Catarinka is a set of useful libraries for Pascal/Delphi, developed as part of the [Sandcat Browser](https://github.com/syhunt/sandcat) project.

## Directories

* `/docs` - Documentation about the library's functions and classes for Pascal/Delphi usage
 
## Pascal Components

Catarinka includes a set of visual and non-visual components, and methods for Pascal/Delphi. The kit includes the following components:

* `TCatActiveScript` - Improved ActiveScript library based on code by Serhiy Perevoznyk.
* `TCatChromium` - A web browser component built on top of WACEF3/DCEF3.
* `TCatConsole` - Console component built on top of a modified version of the Console component by Michael Elsd�rfer.
* `TCatCSCommand` - Allows to call a console application and capture its output.
* `TDashedRecord` - DDV: an alternative to the CSV format and simple library for writing and reading the format.
* `TConsoleTimer` - A timer that works from within console applications. Based on component by LU RD.
* `TCatHighlighters` - Provides quick access to multiple SynEdit highlighters with a color scheme adapted from the CodeRay project.
* `TClipboardListener` - Monitors changes to the Windows clipboard.
* `THTMLEntities` - HTML Entity Encoder and Decoder.
* `TCatHTMLParser` - HTML Parser based on a component by Przemyslaw Jankowski.
* `TCatJSON` - JSON Manipulation component built on top of the XSuperObject or SuperObject.
* `TCatListEditor` - A list editor based on SuperList by David Koretzky.
* `TCatMsg`, `TCatMsgCromis` - Easy to deploy IPC components.
* `TStringPattern`, `TIntegerPattern` - Flexible pattern validation engine using method chaining.
* `TCatPreferences` - JSON-Based settings management component
* `TCatStorage` - VFS/Cache component that uses the Structured Storage library by Primoz Gabrijelcic.
* `TCatSynEdit` - Enhanced SynEdit with popup menu and improved scrolling.
* `TJIniList` - INI-Like component using JSON
* `TStringListCache` - Caches multiple string lists, allowing them to be loaded or saved together to a file.
* `TStringLoop`, `TSepStringLoop` - Simple components for looping through a string list or a separated string.
* Several libraries with string manipulation functions, file system functions and more.

## Compatibility

All components here work with the latest Delphi releases (for both 32-bit and 64-bit compilation) and the older D7. Most of them may work with FPC and Lazarus with minor modifications.

Most components can be cross-compiled with Delphi XE10 Tokyo and CrossVCL.

### Before Compiling

CatPrefs: Rename the `src\CatCryptKey.pas` file, edit it and add your own encryption keys or key generators.

## Dependencies

* All included here, except the following which you need to download separately:
* [DCEF](https://github.com/hgourvest/dcef3) or [WACEF](https://bitbucket.org/WaspAce/wacef), needed by CatChromium. See [dcef-mod](https://github.com/felipedaragon/dcef-mod) for a copy of the latest CEF components, with or without minor modifications
* [DCPcrypt 2](https://bitbucket.org/wpostma/dcpcrypt2010) - needed by CatDCP.
* [SynWeb 1.5](https://code.google.com/p/synweb/) and [SynEdit](http://sourceforge.net/projects/synedit/) - needed by CatSynEdit.
* [Synopse](https://github.com/synopse/SynPDF) - needed by CatCryptoSyno.
* [Structured Storage](https://github.com/gabr42/GpDelphiUnits) - need by CatStorage.
* [Abbrevia 5.0](http://sourceforge.net/projects/tpabbrevia/) - needed by CatZIP.
* [Cromis](http://www.cromis.net/blog/downloads/cromis-ipc/) - needed by CatMsgCromis.