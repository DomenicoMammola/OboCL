{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OMultiPanel_Design;

interface

uses
  OMultiPanel_reg, OMultiPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('OMultiPanel_reg', @OMultiPanel_reg.Register);
end;

initialization
  RegisterPackage('OMultiPanel_Design', @Register);
end.
