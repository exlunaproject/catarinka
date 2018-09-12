## string.loop

Loops through a string list.

### Methods

* **add** ( s ): Adds a string.
* **clear** ( ): Clears the list (deletes all lines).
* **curdelete** ( ): Deletes the current line.
* **get** ( number ): Gets a line by its number.
* **indexof** ( s ): Returns the line of a string. If not found, returns -1.
* **load** ( s ): Loads a string list from a string.
* **loadfromfile** ( filename ): Loads the list from a file.
* **parsing** ( ): Returns true while still parsing the list.
* **reset** ( ): Stops parsing the list, goes back to the beginning.
* **savetofile** ( filename ): Saves the list to a file.
* **stop** ( ): Stops parsing the list.

### Properties

name | return type | description
--- | --- | ---
**commatext** | string | Gets (or sets) the list from a comma string.
**count** | integer | Returns the number of strings in the list.
**curindex** | integer | Returns the index of the current string.
**current** | string | Gets (or sets) the current string.
**text** | string | Gets or sets the list of strings.

### Usage Example

```lua
local s = require "Catarinka"
local list = [[
Turkey
Russia
Azerbaijan
United Kingdom
Montenegro
]]
local p = s.string.loop:new()
p:load(list)
while p:parsing() do
 print(p.current)
end
p:release()
```