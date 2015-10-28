{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WACEF;

interface

uses
  WACefCApi, WACefCExports, WACefGUI, WACefInterfaces, WACefLib, WACefOwns, 
  WACefRefs, WACefTypes, WACefComponent, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WACefComponent', @WACefComponent.Register);
end;

initialization
  RegisterPackage('WACEF', @Register);
end.
