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
  Classes, contnrs, syncobjs, Forms,
  mMaps, mUtility, mXML,
  UramakiBase, UramakiEngineClasses;

type

  TUramakiEngine = class;

  TUramakiEngineMessage = class
  end;

  { TUramakiEngineMessageQueue }

  TUramakiEngineMessageQueue = class
  private
    FMessages: TObjectList;
    FCriticalSection: TCriticalSection;
    FEvent : TSimpleEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Put(aMessage : TUramakiEngineMessage);
    function Pick : TUramakiEngineMessage;
    property Event : TSimpleEvent read FEvent write FEvent;
  end;

  TDoProcessMessage = procedure (aMessage : TUramakiEngineMessage) of object;

  { TUramakiEngineMessagesThread }

  TUramakiEngineMessagesThread = class(TThread)
  strict private
    FQueue : TUramakiEngineMessageQueue;
    FLetsGoEvent : TSimpleEvent;
    FLetsDieEvent : TSimpleEvent;
    FCurrentMessage: TUramakiEngineMessage;
    FDoProcessMessage : TDoProcessMessage;

    procedure FProcessMessage;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property LetsGoEvent : TSimpleEvent read FLetsGoEvent;
    property LetsDieEvent : TSimpleEvent read FLetsDieEvent write FLetsDieEvent;
    property MessagesQueue : TUramakiEngineMessageQueue read FQueue write FQueue;
    property DoProcessMessage : TDoProcessMessage read FDoProcessMessage write FDoProcessMessage;
  end;

  { TUramakiEngineMediator }

  TUramakiEngineMediator = class (TUramakiAbstractEngineMediator)
  strict private
    FEngine : TUramakiEngine;
  public
    constructor Create (aEngine : TUramakiEngine);
    procedure PleaseAskMyFatherToRefreshItsChilds(aPlate : TUramakiPlate); override;
    procedure PleaseRefreshMyChilds (aPlate : TUramakiPlate); override;
    procedure PleaseClearMyChilds (aPlate : TUramakiPlate); override;
    function GetInstanceIdentifier (aPlate : TUramakiPlate) : TGuid; override;
  end;


  { TUramakiEngine }

  TUramakiEngine = class
  strict private
    FTransformers: TUramakiTransformers;
    FPublishers : TUramakiPublishers;

    FLivingPlates : TObjectList;
    FMediator : TUramakiEngineMediator;

    FMessagesQueue : TUramakiEngineMessageQueue;
    FMessagesThread : TUramakiEngineMessagesThread;
    FWaitForCloseThreadEvent : TSimpleEvent;

    FCurrentTransactionId : TGuid;
    procedure StartTransaction;
    procedure EndTransaction;
    procedure ProcessMessage(aMessage : TUramakiEngineMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPublisher (aPublisher : TUramakiPublisher);
    procedure AddTransformer (aTransformer : TUramakiTransformer);

    function CreateLivingPlate(aParentPlateId : TGuid) : TUramakiLivingPlate;
    function FindLivingPlateByPlateId (aPlateId : TGuid) : TUramakiLivingPlate;
    function FindLivingPlateByPlate (aPlate : TUramakiPlate) : TUramakiLivingPlate;
    procedure FindLivingPlatesByParent(aParentPlate : TUramakiPlate; aList : TObjectList);
    function FindTransformer (aTransformerId : string) : TUramakiTransformer;
    function FindPublisher (aPublisherId : string) : TUramakiPublisher;
    procedure FeedLivingPlate (aLivingPlate : TUramakiLivingPlate);
    procedure FreePlatesOfLivingPlates;

    procedure LoadLivingPlatesFromXMLElement (aElement : TmXmlElement);
    procedure SaveLivingPlatesToXMLElement (aElement : TmXmlElement);

    procedure LoadPlatesFromXMLElement (aElement : TmXmlElement);
    procedure SavePlatesToXMLElement (aElement : TmXmlElement);

    procedure GetAvailableTransformers (const aInputUramakiId : string; aList : TUramakiTransformers);
    procedure GetAvailablePublishers (const aInputUramakiId : string; aList : TUramakiPublishers);

    property Mediator : TUramakiEngineMediator read FMediator;
  end;

implementation

uses
  SysUtils;

{ TUramakiEngineMessagesThread }

procedure TUramakiEngineMessagesThread.FProcessMessage;
begin
  FDoProcessMessage(FCurrentMessage);
end;

constructor TUramakiEngineMessagesThread.Create;
begin
  inherited Create(false);
  FLetsGoEvent := TSimpleEvent.Create;
  Self.Priority:= tpHigher;
end;

destructor TUramakiEngineMessagesThread.Destroy;
begin
  FLetsGoEvent.Free;
  inherited Destroy;
end;

procedure TUramakiEngineMessagesThread.Execute;
var
  tmpMessage: TUramakiEngineMessage;
begin
  while not Terminated do
  begin
    if Assigned(FQueue) then
    begin
      tmpMessage := FQueue.Pick;
      while (tmpMessage <> nil) and (not Self.Terminated) do
      begin
        FCurrentMessage := tmpMessage;
        try
          Synchronize(FProcessMessage);
        except
          on e:Exception do
          begin
            Application.ShowException(e);
          end;
        end;
        tmpMessage := FQueue.Pick;
      end;
    end;

    if not Terminated then
    begin
      FLetsGoEvent.WaitFor(INFINITE);
      FLetsGoEvent.ResetEvent;
    end;
  end;
  Sleep (10);
  FLetsDieEvent.SetEvent;
end;

{ TUramakiEngineMessageQueue }

constructor TUramakiEngineMessageQueue.Create;
begin
  FMessages := TObjectList.Create(false);
  FCriticalSection:= TCriticalSection.Create;
end;

destructor TUramakiEngineMessageQueue.Destroy;
begin
  FreeAndNil(FCriticalSection);
  FreeAndNil(FMessages);
  inherited Destroy;
end;

procedure TUramakiEngineMessageQueue.Put(aMessage: TUramakiEngineMessage);
begin
  FCriticalSection.Acquire;
  try
    FMessages.Add(aMessage);
  finally
    FCriticalSection.Leave;
  end;
  if Assigned(FEvent) then
    FEvent.SetEvent;
end;

function TUramakiEngineMessageQueue.Pick: TUramakiEngineMessage;
begin
  FCriticalSection.Acquire;
  try
    if FMessages.Count > 0 then
    begin
      Result := FMessages.Items[0] as TUramakiEngineMessage;
      FMessages.Delete(0);
    end
    else
      Result := nil;
  finally
    FCriticalSection.Leave;
  end;

end;

{ TUramakiEngineMediator }

constructor TUramakiEngineMediator.Create(aEngine: TUramakiEngine);
begin
  FEngine := aEngine;
end;

procedure TUramakiEngineMediator.PleaseAskMyFatherToRefreshItsChilds(aPlate: TUramakiPlate);
var
  livingPlate : TUramakiLivingPlate;
begin
  livingPlate:= FEngine.FindLivingPlateByPlate(aPlate);
  if Assigned(livingPlate) then
  begin
    if not IsEqualGUID(livingPlate.ParentIdentifier, GUID_NULL) then
    begin
      livingPlate := FEngine.FindLivingPlateByPlateId(livingPlate.ParentIdentifier);
      Self.PleaseRefreshMyChilds(livingPlate.Plate);
    end;
  end;
end;

procedure TUramakiEngineMediator.PleaseRefreshMyChilds(aPlate: TUramakiPlate);
var
  childs : TObjectList;
  i: integer;
begin
  childs := TObjectList.Create(false);
  try
    FEngine.FindLivingPlatesByParent(aPlate, childs);
    for i := 0 to childs.Count - 1 do
    begin
      FEngine.FeedLivingPlate(childs.Items[i] as TUramakiLivingPlate);
    end;
  finally
    childs.Free;
  end;
end;

procedure TUramakiEngineMediator.PleaseClearMyChilds(aPlate: TUramakiPlate);
var
  childs : TObjectList;
  i: integer;
begin
  childs := TObjectList.Create(false);
  try
    FEngine.FindLivingPlatesByParent(aPlate, childs);
    for i := 0 to childs.Count - 1 do
    begin
      (childs.Items[i] as TUramakiLivingPlate).Plate.Clear;
    end;
  finally
    childs.Free;
  end;
end;

function TUramakiEngineMediator.GetInstanceIdentifier(aPlate: TUramakiPlate): TGuid;
var
  tmpLivingPlate : TUramakiLivingPlate;
begin
  Result := GUID_NULL;;
  tmpLivingPlate := FEngine.FindLivingPlateByPlate(aPlate);
  if Assigned(tmpLivingPlate) then
    Result := tmpLivingPlate.InstanceIdentifier;
end;

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

procedure TUramakiEngine.ProcessMessage(aMessage: TUramakiEngineMessage);
//var
//  childs : TObjectList;
//  i: integer;
begin
(*  childs := TObjectList.Create(false);
  try
    FindLivingPlatesByParent(aPlate, childs);
    for i := 0 to childs.Count - 1 do
    begin
      Self.FeedLivingPlate(childs.Items[i] as TUramakiLivingPlate);
    end;
  finally
    childs.Free;
  end;*)
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
  FMediator := TUramakiEngineMediator.Create(Self);

  FWaitForCloseThreadEvent := TSimpleEvent.Create;
  FWaitForCloseThreadEvent.ResetEvent;
  FMessagesQueue := TUramakiEngineMessageQueue.Create;

  FMessagesThread := TUramakiEngineMessagesThread.Create;
  FMessagesThread.LetsDieEvent := FWaitForCloseThreadEvent;
  FMessagesThread.MessagesQueue := FMessagesQueue;
  FMessagesThread.DoProcessMessage:= Self.ProcessMessage;
  FMessagesQueue.Event := FMessagesThread.LetsGoEvent;
end;

destructor TUramakiEngine.Destroy;
begin
  FMessagesThread.Terminate;
  FMessagesThread.MessagesQueue := nil;
  FMessagesThread.LetsGoEvent.SetEvent;
  FWaitForCloseThreadEvent.WaitFor(3000);
  FreeAndNil(FWaitForCloseThreadEvent);
  FreeAndNil(FMessagesThread);
  FreeAndNil(FMessagesQueue);

  FTransformers.Free;
  FPublishers.Free;
  FLivingPlates.Free;
  FMediator.Free;
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
end;

function TUramakiEngine.FindLivingPlateByPlateId(aPlateId: TGuid): TUramakiLivingPlate;
var
  i : integer;
begin
  Result := nil;
  if IsEqualGUID(aPlateId, GUID_NULL) then
    exit;
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if IsEqualGUID((FLivingPlates.Items[i] as TUramakiLivingPlate).InstanceIdentifier, aPlateId ) then
    begin
      Result := FLivingPlates.Items[i] as TUramakiLivingPlate;
      exit;
    end;
  end;
end;

function TUramakiEngine.FindLivingPlateByPlate(aPlate: TUramakiPlate): TUramakiLivingPlate;
var
  i : integer;
begin
  Result := nil;
  if not Assigned(aPlate) then
    exit;
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate = aPlate then
    begin
      Result := FLivingPlates.Items[i] as TUramakiLivingPlate;
      exit;
    end;
  end;
end;

procedure TUramakiEngine.FindLivingPlatesByParent(aParentPlate: TUramakiPlate; aList: TObjectList);
var
  parentGuid : TGuid;
  i : integer;
begin
  assert(aList.OwnsObjects = false);

  aList.Clear;
  if not Assigned(aParentPlate) then
    exit;
  parentGuid := FindLivingPlateByPlate(aParentPlate).InstanceIdentifier;
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if IsEqualGUID((FLivingPlates.Items[i] as TUramakiLivingPlate).ParentIdentifier, parentGuid) then
    begin
      aList.Add(FLivingPlates.Items[i]);
    end;
  end;

end;

function TUramakiEngine.FindTransformer(aTransformerId: string): TUramakiTransformer;
begin
  Result := FTransformers.FindById(aTransformerId);
end;

function TUramakiEngine.FindPublisher(aPublisherId: string): TUramakiPublisher;
begin
  Result := FPublishers.FindById(aPublisherId);
end;

procedure TUramakiEngine.FeedLivingPlate(aLivingPlate: TUramakiLivingPlate);
var
  i : integer;
  startUramakiId : string;
  inputUramakiRoll : TUramakiRoll;
  Garbage : TObjectList;
  tmpParent : TUramakiLivingPlate;
begin
  if not Assigned(aLivingPlate.Publication.Publisher) then
    exit;
  if aLivingPlate.Publication.Publisher.GetInputUramakiId = NULL_URAMAKI_ID then
  begin
    aLivingPlate.Publication.Publisher.Publish(nil, aLivingPlate.Plate, aLivingPlate.Publication.PublicationContext);
  end
  else
  begin
    if aLivingPlate.Transformations.Count > 0 then
      startUramakiId := aLivingPlate.Transformations.Items[0].Transformer.GetInputUramakiId
    else
      startUramakiId:=aLivingPlate.Publication.Publisher.GetInputUramakiId;

    Garbage := TObjectList.Create(true);
    try
      aLivingPlate.StartShining;
      try
        tmpParent := Self.FindLivingPlateByPlateId(aLivingPlate.ParentIdentifier);
        if Assigned(tmpParent) and Assigned(tmpParent.Plate) then
        begin
          inputUramakiRoll := tmpParent.Plate.GetUramakiRoll(startUramakiId);
          Garbage.Add(inputUramakiRoll);
        end
        else
          inputUramakiRoll := nil;
        for i := 0 to aLivingPlate.Transformations.Count -1 do
        begin
          inputUramakiRoll := aLivingPlate.Transformations.Items[i].Transformer.Transform(inputUramakiRoll, aLivingPlate.Transformations.Items[0].TransformationContext);
          Garbage.Add(inputUramakiRoll);
        end;

        aLivingPlate.Publication.Publisher.Publish(inputUramakiRoll, aLivingPlate.Plate, aLivingPlate.Publication.PublicationContext);
      finally
        aLivingPlate.StopShining;
      end;
    finally
      Garbage.Free;
    end;
  end;
end;

procedure TUramakiEngine.FreePlatesOfLivingPlates;
var
  i : integer;
begin
  for i := 0 to FLivingPlates.Count -1 do
  begin
    (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate.Free;
    (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate := nil;
  end;
end;

procedure TUramakiEngine.LoadLivingPlatesFromXMLElement(aElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
  tmpPlate : TUramakiLivingPlate;
begin
  FreePlatesOfLivingPlates;
  FLivingPlates.Clear;
  cursor := TmXmlElementCursor.Create(aElement, 'livingPlate');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      tmpPlate := TUramakiLivingPlate.Create;
      FLivingPlates.Add(tmpPlate);
      tmpPlate.LoadFromXml(cursor.Elements[i], FPublishers, FTransformers);
    end;
  finally
    cursor.Free;
  end;
end;

procedure TUramakiEngine.SaveLivingPlatesToXMLElement(aElement: TmXmlElement);
var
  i : integer;
begin
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if not (FLivingPlates.Items[i] as TUramakiLivingPlate).Deleted then
      (FLivingPlates.Items[i] as TUramakiLivingPlate).SaveToXml(aElement.AddElement('livingPlate'));
  end;
end;

procedure TUramakiEngine.LoadPlatesFromXMLElement(aElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
  tmpPlate : TUramakiLivingPlate;
  tmpId : TGUID;
begin
  cursor := TmXmlElementCursor.Create(aElement, 'plateConfiguration');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      tmpId := StringToGUID(cursor.Elements[i].GetAttribute('identifier'));
      tmpPlate := FindLivingPlateByPlateId(tmpId);
      if Assigned(tmpPlate) and Assigned(tmpPlate.Plate) then
        tmpPlate.Plate.LoadConfigurationFromXML(cursor.Elements[i]);
    end;
  finally
    cursor.Free;
  end;
end;

procedure TUramakiEngine.SavePlatesToXMLElement(aElement: TmXmlElement);
var
  i : integer;
  tmpElement : TmXmlElement;
begin
  for i := 0 to FLivingPlates.Count - 1 do
  begin
    if Assigned((FLivingPlates.Items[i] as TUramakiLivingPlate).Plate) then
    begin
      tmpElement := aElement.AddElement('plateConfiguration');
      tmpElement.SetAttribute('identifier', GUIDToString((FLivingPlates.Items[i] as TUramakiLivingPlate).InstanceIdentifier));
      (FLivingPlates.Items[i] as TUramakiLivingPlate).Plate.SaveConfigurationToXml(tmpElement);
    end;
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
