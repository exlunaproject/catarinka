## File System Functions

### File Functions (file.*)

* **canopen** ( filename ): Returns true if a file can be opened. If the file is locked, returns false.
* **cleanname** (filename): Returns a filename with invalid characters stripped out.
* **copy** ( source, dest ): Copies a file to a new file.
* **delete** ( filename ): Deletes a file.
* **exec** ( filename ): Executes a file.
* **exechide** ( filename ): Executes a file in hidden state.
* **exists** ( filename ): Returns true if a file exists, and false otherwise.
* **getcontents** ( filename ): Returns the contents of a local file.
* **getdir** ( filename ): Gets the directory part of a filename.
* **getext** ( filename ): Gets the extension part of a filename.
* **getname** ( filename ): Gets the name and extension part of a filename.
* **getsize** ( filename ): Gets the size in bytes of a filename.
* **getver** ( filename ): Gets the version of a binary file.

### Directory Functions (dir.*)

* **create** ( dirname ): Recursively creates a directory.
* **delete** ( dirname ): Deletes a directory and its subdirectories.
* **exists** ( dirname ): Returns true if a directory exists, and false otherwise.
* **getdirlist** ( dirname ): Returns the list of sub directories of a directory.
* **getfilelist** ( dirname ): Returns the list of files of a directory.
* **packtotar** ( dirname, outfilename [, filemask]): Packs a directory to a TAR file.
* **unpackfromtar** ( tarfilename, outdirname ): Unpacks a TAR file to a directory.

## String Operations (string.*)

* **after** ( s, sub ): Returns the portion of the string after a specific sub-string.
* **before** ( s, sub ): Returns the portion of the string before a specific sub-string.
* **between** ( s, start, stop ): Returns a string between 2 strings.
* **decrease** ( s [,step] ): Decreases the string characters.
* **gettoken** ( s, delim, int ): Returns what comes after a delimiter.
* **increase** ( s [,step] ): Increases the string characters.
* **lastchar** ( s ): Returns the last character of a string.
* **maxlen** ( s, max [,addellipsis] ): Cuts a string if it exceeds the max number of characters.
* **occur** ( s, sub ): Returns the count of the occurrence of a particular string or character.
* **random** ( int ): Returns a random string that is the length of your choosing.
* **replace** ( s, find, rep ): Replaces a string.
* **replacefirst** ( s, find, rep ): Replaces just the first occurrence of a sub string in a string.
* **stripquotes** ( s ): Returns a string with removed quotes.
* **stripblanklines** ( s ): Returns a multi-line string without any blank lines.
* **trim** ( s ): Returns a string without redundant whitespace.

### String Matching Functions

* **beginswith** ( s, prefix ): Checks if a string begins with a specific string.
* **endswith** ( s, termination ): Checks if a string ends with a string.
* **ishex** ( s ): Checks if a string is a hexadecimal representation.
* **isint** ( s ): Checks if a string is integer.
* **match** ( s, pattern ): Wildcard matching (* and ?).

These will return a boolean value.

### String Classes

* **list**: Returns a stringlist object (see `classes.stringlist.md`).
* **loop**: Returns a stringloop object (see `classes.stringloop.md`).

## Regular Expression Functions (re.*)

* **find** ( s, regex): Regular expression finder. Returns a string.
* **match** ( s, regex): Returns true if it matches a regular expression, false otherwise.
* **replace** ( s, regex, rep ): Finds a string using a regular expression and replaces it. Returns a new string.

## Web Functions

### HTML Functions (html.*)

* **beautifycss** ( css ): Formats a CSS code.
* **beautifyjs** ( js ): Formats a JavaScript code.
* **escape** ( s ): Escapes HTML tag characters.
* **gettagcontents** ( html, tag ): Extracts the content of HTML tags.
* **parser**: Returns a HTML parser object (see `classes.htmlparser.md`).
* **striptags** ( s ): Removes tags from a string.
* **unescape** ( s ): Unescapes HTML tag characters.

### URL Functions (url.*)

* **changepath** ( url, newpath ): Changes the path of an URL.
* **combine** ( url, path ): Combines a path to a URL.
* **crack** ( url ) : Returns the main components of an URL as a table.
 * fileext - filename extension (eg: .lp)
 * filename - filename (eg: index.lp)
 * host - host name (eg: www.lua.org)
 * path - location (eg: demo/index.lp)
 * port - port number (eg: 80)
 * proto - protocol (eg: https)
* **decode** ( s ): Decodes an URL.
* **encode** ( s ): Encodes an URL.
* **encodefull** ( s ): Full URL Encode.
* **fileurltofilename** ( fileurl ): Converts a file URL to a proper filename.
* **genfromhost** ( hostname , port ): Generates an URL from a hostname and a port.
* **getfileext** ( url ): Returns the extension from an URL filename.
* **getfilename** ( url ): Returns the URL filename.
* **gettiny** ( url ): Returns a tiny version of an URL (uses tinyurl.com).

### JSON Functions (json.*)

* **object**: Returns a JSON object (see `classes.jsonobject.md`).

### HTTP Functions (http.*)

* **crackrequest** ( headers ): Returns the main components of the headers of a HTTP request as a table.
 * data - Request/POST data (if any)
 * method' - Request method (GET, POST, HEAD, etc...)
 * path - URL path
* **getheader** ( headers, fieldname ): Returns the value of a header field.

## Miscellaneous

### Net Functions (net.*)

* **nametoip** ( name ): Converts host name to IP address.
* **iptoname** ( ip ): Converts IP address to host name.

### Base64 Functions (base64.*)

* **encode** ( s ): Returns a string converted to a base64 string.
* **decode** ( s ): Converts a base64 string to a string.

### Conversion Functions (convert.*)

* **commastrtostr** ( s ): Converts a comma string to a string.
* **strtoalphanum** ( s ): Converts a string to alphanumeric string.
* **strtocommastr** ( s ): Converts a string to a comma string.
* **strtohex** ( s ): Converts a string to a hexadecimal string.
* **hextoint** ( s ): Converts a hex string to integer.
* **hextostr** ( s ): Converts a hexadecimal string to string.

### Crypto Functions (crypto.*)

* **md5** ( s ): Returns the MD5 hash of a given string.
* **sha1** ( s ): Returns the SHA-1 hash of a given string.

### Task Functions (task.*)

* **isrunning** ( exefilename  [,fullname] ): Returns true if a process is running, false otherwise.
* **kill** ( exefilename [,fullname] ): Closes a running process by its executable name. If the second parameter is true, closes the process only if the full filename matches.

### Utils (utils.*)

* **delay** ( ms ): Waits a specific number of milliseconds before proceeding.
* **getarg** ( s or int , defaultvalue): Returns an argument passed to the executable as a string. Example: getarg('-pid') for returning the value of the argument provided as ``-pid:somevalue``, getarg(1) for returning the first argument, getarg() for returning all arguments. If the argument value is empty, returns the default value.
* **hasarg** ( s ):  Returns true if the argument has been provided, false otherwise.
* **clipboard_gettext** ( ): Returns the current Clipboard text (if any).
* **clipboard_settext** ( s ): Copies a text to the Clipboard. 