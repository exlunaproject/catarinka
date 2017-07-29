# Catarinka

Catarinka is a set of libraries for Lua and Pascal/Delphi, developed as part of the [Sandcat Browser](https://github.com/felipedaragon/sandcat) project.

## Directories

* `/lualib` - Catarinka Lua Library source
 * `/docs` - Documentation about the library's functions and classes
* `/src` - a set of visual and non-visual components, and methods for Pascal/Delphi
 
## Pascal Components

 This kit includes the following components:

* `TCatChromium` - A web browser component built on top of WACEF3/DCEF3.
* `TCatConsole` - Console component built on top of a modified version of the Console component by Michael Elsd�rfer.
* `TCatHighlighters` - Provides quick access to multiple SynEdit highlighters with a color scheme adapted from the CodeRay project.
* `TCatHTMLParser` - HTML Parser based on a component by Przemyslaw Jankowski
* `TCatJSON` - JSON Manipulation component built on top of the SuperObject.
* `TCatListEditor` - A list editor based on SuperList by David Koretzky.
* `TCatMsg`, `TCatMsgCromis` - Easy to deploy IPC components.
* `TStringPattern`, `TIntegerPattern` - Flexible pattern validation engine using method chaining.
* `TCatPreferences` - JSON-Based settings management component
* `TCatStorage` - VFS/Cache component that uses the Structured Storage library by Primoz Gabrijelcic.
* `TCatSynEdit` - Enhanced SynEdit with popup menu and improved scrolling.
* `TJIniList` - INIList-Like component using JSON
* `TStringListCache` - Caches multiple string lists, allowing them to be loaded or saved together to a file.
* `TStringLoop`, `TSepStringLoop` - Simple components for looping through a string list or a separated string.
* Several libraries with string manipulation functions, file system functions and more.

## Compatibility

All components here work with the latest Delphi releases (for both 32-bit and 64-bit compilation) and the older D7. Most of them may work with FPC and Lazarus.

### Before Compiling

CatPrefs: Rename the `src\CatDCPKey.pas` file, edit it and add your own encryption keys or key generators.

## Dependencies

* All included in the `src` directory, except the following which you need to download separately:
* [DCEF](https://github.com/hgourvest/dcef3) or [WACEF](https://bitbucket.org/WaspAce/wacef), needed by CatChromium. See [dcef-mod](https://github.com/felipedaragon/dcef-mod) for a copy of the latest CEF components, with or without minor modifications
* [DCPcrypt 2](https://bitbucket.org/wpostma/dcpcrypt2010) - needed by CatDCP.
* [SynWeb 1.5](https://code.google.com/p/synweb/) and [SynEdit](http://sourceforge.net/projects/synedit/) - needed by CatSynEdit.
* [Structured Storage](https://github.com/gabr42/GpDelphiUnits) - need by CatStorage.
* [Abbrevia 5.0](http://sourceforge.net/projects/tpabbrevia/) - needed by CatZIP.
* [Cromis](http://www.cromis.net/blog/downloads/cromis-ipc/) - needed by CatMsgCromis.

## License & Credits

Catarinka was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This project is licensed under a 3-clause BSD license - see the LICENSE file for details.

Some libraries and third-party code included with Catarinka use different licenses, such as MIT and MPL. You can find them in the comments of the source code files.

## Contact

Twitter: [@felipedaragon](https://twitter.com/felipedaragon), [@syhunt](https://twitter.com/syhunt)

Email: felipe _at_ syhunt.com

If you want to report a security bug, please see the `SECURITY.md` file.