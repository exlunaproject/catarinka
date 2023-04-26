unit CatAxUtils;

{
  Catarinka ActiveX Utils
  Copyright (c) 2013-2023 Syhunt Informatica
  Based on an example by cjc - https://delphi.cjcsoft.net/viewthread.php?tid=42770
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

Uses Classes, ActiveX, Axctrls;

Type
  TInterfaceStream = Class(TMemoryStream)
  Public
    Procedure LoadFromIStream(Source: IStream);
    Function GetIStream: IStream;
  end;

implementation

Procedure TInterfaceStream.LoadFromIStream(Source: IStream);
var
  Adapt: TOLEStream;
  Buff: Byte;
  I: Integer;
begin
  if Source = nil then
    exit;

  Adapt := TOLEStream.Create(Source);
  Adapt.Position := 0;
  Self.Clear;
  Self.Position := 0;
  For I := 0 to Adapt.Size - 1 do
  begin
    Adapt.Read(Buff, 1);
    Self.Write(Buff, 1);
  end;
  Self.Position := 0;
end;

Function TInterfaceStream.GetIStream: IStream;
var
  Adapt: TStreamAdapter;
  tPos: UInt64;
begin
  Adapt := TStreamAdapter.Create(Self, soReference);
  Adapt.Seek(0, 0, tPos);
  Result := Adapt as IStream;
end;

{
  Now it's simple to use IStream.  For instance, if you have a method of a COM object that your building that needs to return a IStream, simply declare a TInterfaceStream as a private member of the object( we'll call it FStream here), Create it on initialize, and write your method like this

  Function TSampleCOMObj.Mehtod1 : IStream
  begin
  // Here's where you load whatever actually goes into the stream
  result := FStream.GetIStream;
  end;

  Making it a local variable to the method could be a little tricky.  There may be a potentiality that the memory could be deallocated prior to the application using this object gets around to reading the contents.  Making it a private member is safer.

  On the application side, simply do the reverse:

  Procedure Form1.Button1OnClick(Sender : TObject);
  var
  Server : ISampleCOMObj;
  temp : IStream;
  ResultStream : TInterfaceStream;
  begin
  Server := CreateCOMObject(Class_TSampleCOMObj) as ISampleComObj;
  temp := Server.method1;
  ResultStream := TinterfaceStream.Create;
  ResultStream.Clear;
  resultStream.Position := 0;
  resultstream.LoadFromIStream(Temp);
  // do whatever it is you want with the data in the stream;
  end;

  This is also a great way to move TStrings around.  IStrings is Delphi specific, but a TStrings saved to a IStream isn't.
}

end.
