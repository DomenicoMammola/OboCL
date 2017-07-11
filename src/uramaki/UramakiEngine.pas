// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiEngine;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mMaps, mUtility,
  UramakiBase, UramakiEngineConnector, UramakiEngineClasses;

type

  { TUramakiEngine }

  TUramakiEngine = class
  strict private
    FTransformers: TUramakiTransformers;
    FPublishers : TUramakiPublishers;

    FLivingPlates : TObjectList;

    FCurrentTransactionId : TGuid;
    procedure StartTransaction;
    procedure EndTransaction;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPublisher (aPublisher : TUramakiPublisher);
    procedure AddTransformer (aTransformer : TUramakiTransformer);

    function CreateLivingPlate(aParentPlateId : TGuid) : TUramakiLivingPlate;
    function FindLivingPlate (aPlateId : TGuid) : TUramakiLivingPlate;

//    function BuildRoll(aParentPlateId : TGuid; aTransformations : TStringList) : TUramakiRoll;


//    function BuildLivingPlate(aParentPlateId : TGuid; aLivingPlate : TUramaki

    procedure GetAvailableTransformers (const aInputUramakiId : string; aList : TUramakiTransformers);
    procedure GetAvailablePublishers (const aInputUramakiId : string; aList : TUramakiPublishers);
  end;

implementation

uses
  SysUtils;

{ TUramakiEngine }

procedure TUramakiEngine.StartTransaction;
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

procedure TUramakiEngine.EndTransaction;
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

(*
function TUramakiEngine.BuildRoll(aParentPlateId : TGuid; aTransformations : TStringList) : TUramakiRoll;
var
  parentPlate : TUramakiLivingPlate;
  i : integer;
  currentTransformer : TUramakiTransformer;
  sourceRoll : TUramakiRoll;
  currentTransformation : TUramakiActualTransformation;
  garbage : TObjectList;
begin
  Result := nil;
  if not IsEqualGUID(aParentPlateId, GUID_NULL) then
  begin
    parentPlate := FLivingPlatesDictionary.Find(GUIDToString(aParentPlateId));
    if not Assigned(parentPlate) then
      exit;
  end
  else
    parentPlate := nil;

  garbage := TObjectList.Create(true);
  try
    for i := 0 to aTransformers.Count -1 do
    begin
      currentTransformer := FTransformersDictionary.Find(aTransformers.Strings[i]);
      if i = 0 then
      begin
        if (currentTransformer.GetInputUramakiId = NULL_URAMAKI_ID) then
          sourceRoll := nil
        else
          sourceRoll := parentPlate.Plate.GetUramaki(currentTransformer.GetInputUramakiId);
        currentTransformation := Result.Transformations.Add;
        currentTransformation.Transformer := currentTransformer;
        currentTransformer.Transform(sourceRoll, currentTransformation.TransformationContext);

      end;
    end;
  finally
    garbage.Free;
  end;
end;  *)

constructor TUramakiEngine.Create;
begin
  FTransformers := TUramakiTransformers.Create;
  FPublishers := TUramakiPublishers.Create;
  FLivingPlates := TObjectList.Create(true);
  FCurrentTransactionId := GUID_NULL;
end;

destructor TUramakiEngine.Destroy;
begin
  FTransformers.Free;
  FPublishers.Free;
  FLivingPlates.Free;
  inherited Destroy;
end;

procedure TUramakiEngine.AddPublisher(aPublisher: TUramakiPublisher);
begin
  if not Assigned(FPublishers.FindById(aPublisher.GetMyId)) then
  begin
    FPublishers.Add(aPublisher);
  end;
end;

procedure TUramakiEngine.AddTransformer(aTransformer: TUramakiTransformer);
begin
  if not Assigned(FTransformers.FindById(aTransformer.GetMyId)) then
  begin
    FTransformers.Add(aTransformer);
  end;
end;

function TUramakiEngine.CreateLivingPlate(aParentPlateId: TGuid): TUramakiLivingPlate;
begin
  Result := TUramakiLivingPlate.Create;
  FLivingPlates.Add(Result);
  Result.ParentIdentifier := aParentPlateId;
  if
  Result.Parent;
end;

function TUramakiEngine.FindLivingPlate(aPlateId: TGuid): TUramakiLivingPlate;
var
  i : integer;
begin
  Result := nil;
  if IsEqualGUID(aPlateId, GUID_NULL) then
    exit;
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if IsEqualGUID((FLivingPlates.Items[i] as T );
  end;
end;

procedure TUramakiEngine.GetAvailableTransformers(const aInputUramakiId : string; aList: TUramakiTransformers);
var
  i : integer;
begin
  aList.Clear;

  for i := 0 to FTransformers.Count - 1 do
  begin
    if CompareText(FTransformers.Get(i).GetInputUramakiId, aInputUramakiId) = 0 then
      aList.Add(FTransformers.Get(i));
  end;
end;

procedure TUramakiEngine.GetAvailablePublishers(const aInputUramakiId: string; aList: TUramakiPublishers);
var
  i : integer;
begin
  aList.Clear;

  for i := 0 to FPublishers.Count - 1 do
  begin
    if CompareText(FPublishers.Get(i).GetInputUramakiId, aInputUramakiId) = 0 then
      aList.Add(FPublishers.Get(i));
  end;
end;

end.
