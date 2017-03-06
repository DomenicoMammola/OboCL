// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXML;

interface

uses
  Classes, SysUtils;

type
  TmXMLString = string;

  EmXmlError = class(Exception);

  TImpl_mXmlElement = class;
  TImpl_mXmlDocument = class;
  TImpl_mXmlElementCursor = class;

  TmXmlElement = class;
  TmXmlDocument = class
  private
    FImpl : TImpl_mXmlDocument;
  public
    constructor Create;
    destructor Destroy; override;

    function RootElement: TmXmlElement;
    function CreateRootElement(Name: string): TmXmlElement;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
  end;

  TmXmlElement = class
  private
    FImpl : TImpl_mXmlElement;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;

    function AddElement(Name: TmXMLString): TmXmlElement;
    function HasAttribute(const Name: TmXMLString): boolean;
    procedure SetAttribute(Name, Value: TmXmlString);
    function GetAttribute(Name: TmXmlString): TmXmlString; overload;
    function GetAttribute(Name: TmXmlString; Default: TmXmlString): TmXmlString; overload;
    procedure SetDateTimeAttribute(Name : TmXmlString; Value: TDateTime);
    function GetDateTimeAttribute(Name: TmXmlString): TDateTime; overload;
    function GetDateTimeAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload;
    procedure SetIntegerAttribute(Name : TmXmlString; Value: integer);
    function GetIntegerAttribute(Name: TmXmlString): integer; overload;
    function GetIntegerAttribute(Name: TmXmlString; Default : integer): integer; overload;
  end;

  TmXmlElementCursor = class
  private
    FImpl : TImpl_mXmlElementCursor;
    function GetElement(I: integer): TmXmlElement;
  public
    constructor Create(Element: TmXmlElement; Name: TmXmlString); reintroduce; overload;
    constructor Create(Element: TmXmlElement); reintroduce; overload;
    destructor Destroy; override;
    function Count : integer;
  public
    property Elements[I: integer]: TmXmlElement read GetElement; default;
  end;

  TImpl_mXmlElement = class
  public
    constructor Create; virtual; abstract;
  public
    function _AddElement(Name: TmXMLString): TmXmlElement; virtual; abstract;
    function _HasAttribute(const Name: TmXMLString): boolean; virtual; abstract;
    procedure _SetAttribute(Name, Value: TmXmlString); virtual; abstract;
    function _GetAttribute(Name: TmXmlString): TmXmlString; overload; virtual; abstract;
    function _GetAttribute(Name: TmXmlString; Default: TmXmlString): TmXmlString; overload; virtual; abstract;
    procedure _SetDateTimeAttribute(Name : TmXmlString; Value : TDateTime); virtual; abstract;
    function _GetDateTimeAttribute(Name: TmXmlString): TDateTime; overload; virtual; abstract;
    function _GetDateTimeAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; virtual; abstract;
    procedure _SetIntegerAttribute(Name : TmXmlString; Value: integer); virtual; abstract;
    function _GetIntegerAttribute(Name: TmXmlString): integer; overload; virtual; abstract;
    function _GetIntegerAttribute(Name: TmXmlString; Default : integer): integer; overload; virtual; abstract;

    procedure _SetOwner(aOwner : TObject); virtual; abstract;
  end;

  TImpl_mXmlDocument = class
  public
    constructor Create; virtual; abstract;
    function _RootElement: TmXmlElement; virtual; abstract;
    function _CreateRootElement(Name: string): TmXmlElement; virtual; abstract;
    procedure _Clear; virtual; abstract;
    procedure _SaveToStream(Stream: TStream); virtual; abstract;
    procedure _LoadFromStream(Stream: TStream); virtual; abstract;
    procedure _SaveToFile(FileName: string); virtual; abstract;
    procedure _LoadFromFile(FileName: string); virtual; abstract;
  end;

  TImpl_mXmlElementCursor = class
  public
    constructor Create; virtual; abstract;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); virtual; abstract;
    function _GetElement(I : integer) : TmXmlElement; virtual; abstract;
    function _Count : integer; virtual; abstract;
  end;

  TImpl_Factory = class
  public
    function GetTImpl_mXmlElement : TImpl_mXmlElement;
    function GetTImpl_mXmlDocument : TImpl_mXmlDocument;
    function GetTImpl_mXmlElementCursor : TImpl_mXmlElementCursor;
  end;

implementation

uses
  mXML_oxml;

var
  InternalFactory : TImpl_Factory;



{ TmXmlElement }

function TmXmlElement.AddElement(Name: TmXMLString): TmXmlElement;
begin
  Result := FImpl._AddElement(Name);
end;

constructor TmXmlElement.Create(Owner: TObject);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElement;
  FImpl._SetOwner(Owner);
end;

destructor TmXmlElement.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElement.GetAttribute(Name, Default: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(Name, Default);
end;

function TmXmlElement.GetDateTimeAttribute(Name: TmXmlString): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(Name);
end;

function TmXmlElement.GetDateTimeAttribute(Name: TmXmlString; Default: TDateTime): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(Name, Default);
end;

function TmXmlElement.GetIntegerAttribute(Name: TmXmlString): integer;
begin
  Result := FImpl._GetIntegerAttribute(Name);
end;

function TmXmlElement.GetIntegerAttribute(Name: TmXmlString; Default: integer): integer;
begin
  Result := FImpl._GetIntegerAttribute(Name, Default)
end;

function TmXmlElement.GetAttribute(Name: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(Name);
end;

function TmXmlElement.HasAttribute(const Name: TmXMLString): boolean;
begin
  Result := FImpl._HasAttribute(Name);
end;


procedure TmXmlElement.SetAttribute(Name, Value: TmXmlString);
begin
  FImpl._SetAttribute(Name, Value)
end;

procedure TmXmlElement.SetDateTimeAttribute(Name: TmXmlString; Value: TDateTime);
begin
  FImpl._SetDateTimeAttribute(Name, Value);
end;

procedure TmXmlElement.SetIntegerAttribute(Name: TmXmlString; Value: integer);
begin
  FImpl._SetIntegerAttribute(Name, Value);
end;

{ TmXmlDocument }

procedure TmXmlDocument.Clear;
begin
  FImpl._Clear;
end;

constructor TmXmlDocument.Create;
begin
  FImpl := InternalFactory.GetTImpl_mXmlDocument;
end;

function TmXmlDocument.CreateRootElement(Name: string): TmXmlElement;
begin
  Result := FImpl._CreateRootElement(Name);
end;

destructor TmXmlDocument.Destroy;
begin
  FImpl.Free;
  inherited;
end;

procedure TmXmlDocument.LoadFromFile(FileName: string);
begin
  FImpl._LoadFromFile(FileName);
end;

procedure TmXmlDocument.LoadFromStream(Stream: TStream);
begin
  FImpl._LoadFromStream(Stream);
end;

function TmXmlDocument.RootElement: TmXmlElement;
begin
  Result := FImpl._RootElement;
end;

procedure TmXmlDocument.SaveToFile(FileName: string);
begin
  FImpl._SaveToFile(FileName);
end;

procedure TmXmlDocument.SaveToStream(Stream: TStream);
begin
  FImpl._SaveToStream(Stream);
end;

{ TImpl_Factory }

function TImpl_Factory.GetTImpl_mXmlDocument: TImpl_mXmlDocument;
begin
  Result := TImpl_oxml_mXmlDocument.Create;
end;

function TImpl_Factory.GetTImpl_mXmlElement: TImpl_mXmlElement;
begin
  Result := TImpl_oxml_mXmlElement.Create;
end;

function TImpl_Factory.GetTImpl_mXmlElementCursor: TImpl_mXmlElementCursor;
begin
  Result := TImpl_oxml_mXmlElementCursor.Create;
end;

{ TmXmlElementCursor }

constructor TmXmlElementCursor.Create(Element: TmXmlElement; Name: TmXmlString);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, Name);
end;

function TmXmlElementCursor.Count: integer;
begin
  Result := FImpl._Count;
end;

constructor TmXmlElementCursor.Create(Element: TmXmlElement);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, '');
end;

destructor TmXmlElementCursor.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElementCursor.GetElement(I: integer): TmXmlElement;
begin
  Result := FImpl._GetElement(I);
end;

initialization
  InternalFactory := TImpl_Factory.Create;

finalization
  InternalFactory.Free;
end.