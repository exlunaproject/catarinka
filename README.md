# Catarinka

Catarinka is a set of visual and non-visual components, and methods for Pascal/Delphi, developed as part of the [Sandcat Browser](https://github.com/felipedaragon/sandcat) project. This kit includes the following components:

* `TCatChromium` - A web browser component built on top of WACEF3/DCEF3.
* `TCatConsole` - Console component built on top of a modified version of the Console component by Michael Elsdörfer.
* `TCatHighlighters` - Provides quick access to multiple SynEdit highlighters with a color scheme adapted from the CodeRay project.
* `TCatHTMLParser` - HTML Parser based on a component by Przemyslaw Jankowski
* `TCatJSON` - JSON Manipulation component built on top of the SuperObject.
* `TCatListEditor` - A list editor based on SuperList by David Koretzky.
* `TCatPreferences` - JSON-Based settings management component
* `TCatStorage` - VFS/Cache component that uses the Structured Storage library by Primoz Gabrijelcic.
* `TCatSynEdit` - Enhanced SynEdit with popup menu and improved scrolling.
* `TJIniList` - INIList-Like component using JSON
* `TStringLoop` - A simple component for looping through a string list
* Several libraries with string manipulation functions, file system functions and more.

## Compatibility

All components here work with the latest Delphi releases (for both 32-bit and 64-bit compilation) and the older D7. Most of them may work with FPC and Lazarus.

### Before Compiling

CatPrefs: Rename the `src\CatDCPKey.pas` file, edit it and add your own encryption keys or key generators.

## Dependencies

* All included in the `src` directory, except the following which you need to download separately:
* [DCPcrypt 2](https://bitbucket.org/wpostma/dcpcrypt2010) - needed by CatDCP.
* [SynWeb 1.5](https://code.google.com/p/synweb/) and [SynEdit](http://sourceforge.net/projects/synedit/) - needed by CatSynEdit.
* [Structured Storage](https://code.google.com/p/gpdelphiunits/) - need by CatStorage.
* [Abbrevia 5.0](http://sourceforge.net/projects/tpabbrevia/) - needed by CatZIP.

## License & Credits

Catarinka was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This project is licensed under a 3-clause BSD license - see the LICENSE file for details.

Some libraries and third-party code included with Catarinka use different licenses, such as MIT and MPL. You can find them in the comments of the source code files.

## Contact

Twitter: [@felipedaragon](https://twitter.com/felipedaragon), [@syhunt](https://twitter.com/syhunt)

Email: felipe _at_ syhunt.com

If you want to report a security bug, please see the `docs\SECURITY.md` file.