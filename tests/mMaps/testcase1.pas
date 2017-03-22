unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, mMaps

  {$IFNDEF FPC}, IOUtils, TestFramework
  {$ELSE}
  ,fpcunit, testutils, testregistry
  {$ENDIF};

type

  TTestCase1= class(TTestCase)
  published
    procedure TestStringDictionary;
  end;

implementation

type

  { TMyFakeObj }

  TMyFakeObj = class
  public
    value : String;
    constructor Create(aValue : String);
  end;

{ TMyFakeObj }

constructor TMyFakeObj.Create(aValue: String);
begin
  value := aValue;
end;

procedure TTestCase1.TestStringDictionary;
var
  dic : TmStringDictionary;
  obj : TMyFakeObj;
  objList : TObjectList;
begin
  objList := TObjectList.Create(true);
  try
    dic := TmStringDictionary.Create();
    try
      obj := TMyFakeObj.Create('a');
      objList.Add(obj);
      dic.Add('a', obj);
      obj := TMyFakeObj.Create('b');
      objList.Add(obj);
      dic.Add('b', obj);
      obj := TMyFakeObj.Create('c');
      objList.Add(obj);
      dic.Add('c', obj);
      AssertEquals((dic.Find('b') as TMyFakeObj).value, 'b');
      AssertEquals((dic.Find('a') as TMyFakeObj).value, 'a');
      AssertEquals((dic.Find('c') as TMyFakeObj).value, 'c');
      AssertTrue(dic.Find('z') = nil);
    finally
      dic.Free;
    end;
  finally
    objList.Free;
  end;
end;



initialization

  RegisterTest(TTestCase1);
end.

