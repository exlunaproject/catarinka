# Catarinka Patterns
## Pattern Validation for Pascal/Delphi

This is a flexible string and integer validation engine for Pascal/Delphi with support for case sensitive or insensitive reusable chained validations. Currently over 30 chainable checks and custom checks are supported. It compiles under Delphi XE down to Delphi 7, both 32-bit and 64-bit.

### How It Works

The Match() function ignites the validation and returns true if all checks passed, but if a check fails, it breaks the chain (not really executing the rest) and returns false.

```pascal
  v.begins(['http:','https:']).ends('.com').match('http://someurl.com'); // true
```

It is possible to determine the check that failed using the Index property - if you have, for example, three chained checks and the second one failed, the Index value will be 2.

### Inspiration

This project is inspired by Sailor's Valua (Validation for Lua) and Respect Validation for PHP. Be sure to check them later at: [valua](https://github.com/sailorproject/valua) and [Validation](https://github.com/Respect/Validation).

### Usage Examples:

```pascal
  // chain validation way:
  v := TStringPattern.Create;
  v.begins(['http:','https:']).ends('.com').match('http://someurl.com'); // true
  memo1.lines.add(v.value + '=' +v.resultasstr);

  // match using [] as shorthand to Match()
  if v.wild('Ca*ka').len(9)['Catarinka'] then // true
  if v.contains('some').len(10)['somestring'] then // true

  // determine where it has failed with the Index property
  if v.int.len(4)['301'] = false then begin
  case v.index of
  1: showmessage('must be a number!');
  2: showmessage('must have four digits!');
  end;
  end;

  // create with a single line and use Lock to make it reusable
  v := TStringPattern.Create.Begins('http://').Ends('.com').Lock;
  if v['http://someurl.com'] then
  showmessage('valid!'); // true
  if v['http://someurl.org'] then // false
  (...)
  if v['https://someurl.com'] then // false
  (...)
  v.free;

  // make it case insensitive just by calling MatchI(), instead of Match():
  v.Contains('some').Len(10).MatchI('SOMESTRING') // true
  v.Contains('some').Len(10).Match('SOMESTRING') // false

  // match against string lists
  if v.InList(listbox1.Items)['somestring'] then
  showmessage('true!');

  // match against an integer
  vi := TIntegerPattern.Create;
  if vi.positive.between(1, 255)[input] then
  (...)

  // use your own custom methods
  const
  VALCUST_ISCREDITCARD = 1;
  VALCUST_SOMETHINGELSE = 2;
  v := TStringPattern.Create;
  v.oncustommethod := yourcustommethod;
  if v.len(50).contains('str').custom(VALCUST_ISCREDITCARD)['somestring'] then
  (...)

  // and you can still use it the standard Pascal way if you prefer
  s := 'SOMESTRING';
  v := TStringPattern.Create;
  v.Contains('some');
  v.Len(10);
  if v.MatchI(s) then
  showmessage('matched!');
  v.free;
```
  
### License

Catarinka Patterns was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This project is licensed under a 3-clause BSD license - see the LICENSE file for details.

### See Also

* [FluentQuery](https://github.com/malcolmgroves/FluentQuery)