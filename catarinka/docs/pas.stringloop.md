# Catarinka Loops

Tired of limited for-to loops? TStringLoop makes it easy to iterate over each string in a string list while, optionally, looking for defined patterns.

## Usage Examples

Use **TStringLoop** to iterate over each string in a string list:

```pascal
s := TStringLoop.Create(listbox1.items);
  while s.found do
    showmessage(s.current);
s.free;
```

Use **TSepStringLoop** instead to do the same with a separated string:
  
```pascal
s := TSepStringLoop.Create('str1;str2;str3',';');
  while s.found do
    showmessage(s.current);
s.free;
```

Call Stop, Stop(aVariant) or Break to end the loop:

```pascal
while s.found do
  if s.currentlower = 'somestring' then
    result := s.stop(true); // stops the loop, returning true
```

Use the Pattern property to call a variety of chainable pattern matching methods:

```pascal
while s.found do
  if s.pattern.begins(['http://','https://']).ends('.com').match then
    showmessage(s.current);
```

Call Reset if you want to repeat from the beginning:

```pascal
  while s.found do
    showmessage(s.current);
  s.reset;
  while s.found do
    showmessage(s.current);
```

## License

Catarinka Loops was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This project is licensed under a 3-clause BSD license - see the LICENSE file for details.