// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFloatsManagement;

interface

function DoublesAreEqual(aValue1, aValue2 : double; aDecimalNumbers : integer): boolean; overload;
function DoublesAreEqual(aValue1, aValue2 : double): boolean; overload;

function SafeDiv (numer, denom: double): double;

procedure SetDefaultDecimalNumbers (aDecimalNumbers : integer);

implementation

uses
  Math;

var
  DefaultDecimalNumbers : integer;
  DefaultCompareValue : double;



function DoublesAreEqual(aValue1, aValue2 : double; aDecimalNumbers : integer): boolean; overload;
var
  CompareValue : double;
begin
  CompareValue := Power(10, -1 * aDecimalNumbers) - Power(10, -1 * (aDecimalNumbers + 1)) - Power(10, -1 * (aDecimalNumbers + 2));
  Result := (Abs(aValue1 - aValue2) <= CompareValue);
end;

function DoublesAreEqual(aValue1, aValue2 : double): boolean; overload;
begin
  Result := (Abs(aValue1 - aValue2) <= DefaultCompareValue);
end;

procedure SetDefaultDecimalNumbers (aDecimalNumbers : integer);
begin
  if (DefaultDecimalNumbers <> aDecimalNumbers) then
  begin
    DefaultCompareValue := Power(10, -1 * aDecimalNumbers) - Power(10, -1 * (aDecimalNumbers + 1)) - Power(10, -1 * (aDecimalNumbers + 2));
    DefaultDecimalNumbers := aDecimalNumbers;
  end;
end;

function SafeDiv (numer, denom: double): double;
begin
  if (denom = 0) then
    result := 0
  else
    result:= numer/denom;
end;


initialization
  SetDefaultDecimalNumbers(5);

end.
