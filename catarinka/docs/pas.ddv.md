# DDV

Feel that you could use something simpler than CSV for data?
Perhaps the DDV (Double Dashed Values) format is better for your project.

I created the DDV format as a more compact, easier to visualize and flexible alternative to the widely adopted CSV (Comma Separated Values) data format. 
I said good-bye to the CSV format and started using just DDV.

## A Comparison between DDV and CSV

DDV:
* Uses CSV for storing name-value pairs after a special separator, but is **NOT** defined by columns. Example: `masterstring --a=123,b=456`
* Allows records to be defined fully, partially or to be entirely omitted. Values are appended after the spaced-two-dash " --" special separator, which can be omitted if there are no values defined
* Alphabetic sorting becomes possible
* Better data visualization and uses less disk/memory space if you have a large quantity of records with undefined values
* Faster processing if you need to handle the master string before reading the values
* Quotes in values gets escaped just like CSV: --v1=myvalue1,"v2=""mvalue2"""

CSV:
* Organized by fixed columns. Example: `masterstring,123,456,etc`
* Columns must be separated by comma even if the column value is undefined
* Sometimes breaks alphabetic sorting in CSV file (see example below)
* Difficult visualization when you have multiple undefined values (",,,,"... and you do not know anymore which column is which unless you open it with a visualization tool like Excel)
* Easier to remove a column or merge multiple files

Both DDV and CSV do not support CRLF (line breaks).

## Examples

`masterstring --a=123,b=456` is the same as `masterstring --b=456,a=123`
`masterstring --a=,b=` is the same as `masterstring`

With DDV the CSV (below) is represented like:

```
# --y=Year,m=Make,md=Model,d=Description
Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"
Thomas Anderson --y=2000,m=Mercury,md=Cougar
John
```

CSV equivalent:
```
Owner,Year,Make,Model,Description
"Henry Ford",1997,Ford,E350,"Super luxurious truck"
"Thomas Anderson",2000,Mercury,Cougar,
John,,,,
```

DDV also makes alphabetic sorting easier.

DDV when alphabetized is perfect:
```
# --y=Year,m=Make,md=Model,d=Description
Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"
John
Thomas Anderson --y=2000,m=Mercury,md=Cougar
```

CSV when alphabetized can be problematic:
```
"Henry Ford",1997,Ford,E350,"Super luxurious truck"
"Thomas Anderson",2000,Mercury,Cougar,
John,,,,
```

## Usage Example (Pascal)

Use **TDashedRecord** to create a DDV record:

```pascal
d := TDashedRecord.Create;
d.Master := 'Henry Ford';
d.WriteInteger('y',1997);
d.WriteString('m','Ford');
d.WriteString('md','E350');
d.WriteString('d','Super luxurious truck');

WriteLn(d.Text);

// This will print:
// Henry Ford --y=1997,m=Ford,md=E350,"d=Super luxurious truck"

d.Free;
```

Use the Text property to load a DDV record:
```pascal
  d := TDashedRecord.Create;
  d.text := 'Thomas Anderson --y=2000,m=Mercury,md=Cougar';
  WriteLn(d.ReadString('m',emptystr));
  d.Free;
```

This will print Mercury

## Source

The source code is available in file  `src\CatDDV.pas`

## License

DDV was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This project is licensed under a 3-clause BSD license - see the LICENSE file for details.