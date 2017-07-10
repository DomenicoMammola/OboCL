// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiFramework;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mMaps, mUtility,
  UramakiBase, UramakiFrameworkConnector, UramakiFrameworkClasses;

type

  { TUramakiFramework }

  TUramakiFramework = class
  strict private
    FTransformersDictionary : TmStringDictionary;
    FTransformers: TUramakiTransformers;
    FPublishersDictionary : TmStringDictionary;
    FPublishers : TUramakiPublishers;
    FLivingPlatesDictionary : TmStringDictionary;
    FLivingPlates : TObjectList;
    FCurrentTransactionId : TGuid;
    procedure StartTransaction;
    procedure EndTransaction;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPublisher (aPublisher : TUramakiPublisher);
    procedure AddTransformer (aTransformer : TUramakiTransformer);

    procedure GetAvailableTransformers (const aInputUramakiId : string; aList : TUramakiTransformers);
  end;

implementation

uses
  SysUtils;

{ TUramakiFramework }

procedure TUramakiFramework.StartTransaction;
var
  i : integer;
begin
  if not IsEqualGUID(FCurrentTransactionId,GUID_NULL) then
    raise TUramakiException.Create('UramakiFramework: Transaction already in progress.');

  FCurrentTransactionId := TGuid.NewGuid;

  for i := 0 to FTransformers.Count - 1 do
    FTransformers.Get(i).StartTransaction(FCurrentTransactionId);

  for i := 0 to FPublishers.Count -1 do
    FPublishers.Get(i).StartTransaction(FCurrentTransactionId);

  for i := 0 to FLivingPlates.Count - 1 do
    (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate.StartTransaction(FCurrentTransactionId);
end;

procedure TUramakiFramework.EndTransaction;
var
  i : integer;
begin
  if IsEqualGUID(FCurrentTransactionId, GUID_NULL) then
    raise TUramakiException.Create('UramakiFramework: No transaction is in progress.');

  for i := 0 to FTransformers.Count - 1 do
    FTransformers.Get(i).EndTransaction(FCurrentTransactionId);

  for i := 0 to FPublishers.Count -1 do
    FPublishers.Get(i).EndTransaction(FCurrentTransactionId);

  for i := 0 to FLivingPlates.Count - 1 do
    (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate.EndTransaction(FCurrentTransactionId);

  FCurrentTransactionId := GUID_NULL;
end;

constructor TUramakiFramework.Create;
begin
  FTransformersDictionary := TmStringDictionary.Create();
  FTransformers := TUramakiTransformers.Create;
  FPublishersDictionary := TmStringDictionary.Create();
  FPublishers := TUramakiPublishers.Create;
  FLivingPlatesDictionary := TmStringDictionary.Create();
  FLivingPlates := TObjectList.Create(true);
  FCurrentTransactionId := GUID_NULL;
end;

destructor TUramakiFramework.Destroy;
begin
  FTransformersDictionary.Free;
  FTransformers.Free;
  FPublishersDictionary.Free;
  FPublishers.Free;
  FLivingPlatesDictionary.Free;
  FLivingPlates.Free;
  inherited Destroy;
end;

procedure TUramakiFramework.AddPublisher(aPublisher: TUramakiPublisher);
begin
  if not Assigned(FPublishersDictionary.Find(aPublisher.GetMyId)) then
  begin
    FPublishersDictionary.Add(aPublisher.GetMyId, aPublisher);
    FPublishers.Add(aPublisher);
  end;
end;

procedure TUramakiFramework.AddTransformer(aTransformer: TUramakiTransformer);
begin
  if not Assigned(FTransformersDictionary.Find(aTransformer.GetMyId)) then
  begin
    FTransformersDictionary.Add(aTransformer.GetMyId, aTransformer);
    FTransformers.Add(aTransformer);
  end;
end;

procedure TUramakiFramework.GetAvailableTransformers(const aInputUramakiId : string; aList: TUramakiTransformers);
var
  i : integer;
begin
  aList.Clear;

  for i := 0 to aList.Count - 1 do
  begin
    if CompareText(aList.Get(i).GetInputUramakiId, aInputUramakiId) = 0 then
      aList.Add(aList.Get(i));
  end;
end;

end.
