## html.parser

Parses a HTML document.

### Methods

* **clear** ( ): Clears the document text (deletes all lines).
* **getattrib** ( attr ): Returns the value of a tag attribute.
* **load** ( html ): Loads a document from a string.
* **parsing** ( ): Returns true while still parsing the document.
* **reset** ( ): Stops parsing the document, goes back to the beginning.
* **setattrib** ( attr, s ): Sets the value of a tag attribute.
* **stop** ( ): Stops parsing the document.

### Properties

name | return type | description
--- | --- | ---
**pos** | integer | Returns the current position.
**tagcontent** | string | Returns the content of the current tag.
**tagline** | integer | Returns the line of the current tag.
**tagname** | string | Returns the name of the current tag.
**tagpos** | integer | Returns the position of the current tag.

### Example - Using html.parser

```lua
local s = require "Catarinka"
local html = [[
<html>
<a href="http://www.lua.org">Lua</a>
</html>
]]
local p = s.html.parser:new()
p:load(html)
while p:parsing() do
 print(p.tagname)
 print(p:getattrib('href'))
end
p:release()
```
