# Catarinka Lua Library

This is a multi-purpose set of Lua extensions developed to be used in the [Sandcat Browser](https://github.com/felipedaragon/sandcat) but that can also be used separately with any Lua-based application. Currently, this library extends Lua with [over 60 functions](https://github.com/exlunaproject/catarinka/blob/master/lualib/docs/functions.md) and some useful classes. The project's goal is to make the development of Lua applications easier and to push the boundaries of the Lua language to do innovative things. This will always be a work in progress with new additions and regular updates.

## Directories

* `/docs_lua` - Documentation about the library's functions and classes for Lua usage
* `/src` - Catarinka Lua Library source. `CtkCore.pas` is the main source code file that performs the Lua library registration during runtime

## Usage

To use Catarinka, you just need to load the library using `require "Catarinka"`. After this you can use any of the library's functions. For a list of functions, see [here](https://github.com/exlunaproject/catarinka/blob/master/lualib/docs/functions.md).

### Classes

All Catarinka classes (described in `docs\classes.*`) have a "new" method that must be used for creating the object and a "release" method for freeing it.

## Download

Compiled binaries for Windows can be downloaded from the links below.

* http://www.exluna.org/

## Dependencies

For compiling Catarinka you will need [pLua](https://github.com/exlunaproject/pLua-XE) and  [LibTar](http://www.destructor.de/libtar/).