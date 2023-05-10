// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiDesktop;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ComCtrls, Graphics, Menus, contnrs, ExtCtrls, Forms,

  OMultiPanelSetup,
  mXML, mMaps,

  UramakiBase, UramakiEngine, UramakiEngineClasses,
  UramakiDesktopGUI, UramakiDesktopLayout,
  UramakiDesktopLayoutLCLConfigForm, UramakiDesktopLCLIcons;

type

  { TUramakiDesktopManager }

  TUramakiDesktopManager = class
  strict private
    FEngine : TUramakiEngine;
    FParentControl : TWinControl;
    FContainer : TUramakiDesktopContainerPanel;
    FDesktopDataModule: TUramakiDesktopDataModule;
    FMenuGarbageCollector : TObjectList;


//    procedure CreateToolbar;
    procedure BuildAndFeedPlate(aLivingPlate: TUramakiLivingPlate; aItem: TPanel);
    procedure OnAddPlate (Sender : TObject);
    procedure DoLinkLayoutItemToPlate(aItem : TPanel; aLivingPlateInstanceIdentificator : TGuid);
    procedure DoStartShiningPanel(const aPlate : TUramakiLivingPlate);
    procedure DoStopShiningPanel(const aPlate : TUramakiLivingPlate);
    procedure DoRemovePanel (const aLivingPlateInstanceIdentifier : TGuid);
    procedure MarkChildsAsDeleted (const aPlate : TUramakiLivingPlate);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init (aParent : TWinControl);

    procedure AddPublisher (aPublisher : TUramakiPublisher);
    procedure AddTransformer (aTransformer : TUramakiTransformer);

    procedure LoadFromStream (aStream : TStream);
    procedure SaveToStream (aStream : TStream);
    procedure ShowConfigurationForm;
    procedure FillAddWidgetMenu (aMenuItem : TMenuItem; const aInputUramakiId : string; aLivingPlateIdentifier : TGuid);
  end;

implementation

uses
  SysUtils, Dialogs,
  mWaitCursor;

type
  TMenuInfo = class
  public
    LivingPlateIdenfier : TGuid;
    TransformerId : string;
    PublisherId : string;
  end;

{ TUramakiDesktopManager }

{
procedure TUramakiDesktopManager.CreateToolbar;
begin
//  FToolbar.AddButton(ICON_ADD, nil, '', '', '', false);
  FToolbar.AddDropdown(FRootPopupMenu, nil, 'Add...');
  FToolbar.AddButton(ICON_OPEN, OnLoadFromFile, '', 'Open a report file..', '', false);
  FToolbar.AddButton(ICON_SAVE, OnSaveToFile, '', 'Save to a report file...', '', false);
  FToolbar.AddButton(ICON_CONFIGURE, OnConfigureLayout, '', 'Configure layout of report', '', false);
  FToolbar.UpdateControls;

*)
end;}


procedure TUramakiDesktopManager.OnAddPlate(Sender: TObject);
var
  tmpMenuInfo : TMenuInfo;
  item : TUramakiDesktopSimplePanel;
  tmpLivingPlate : TUramakiLivingPlate;
begin
  tmpMenuInfo := TMenuInfo((Sender as TMenuItem).Tag);
  item := FContainer.AddItem;
  tmpLivingPlate := FEngine.CreateLivingPlate(tmpMenuInfo.LivingPlateIdenfier);
  item.LivingPlateInstanceIdentifier := tmpLivingPlate.InstanceIdentifier;
  if tmpMenuInfo.TransformerId <> '' then
    tmpLivingPlate.Transformations.Add.Transformer := FEngine.FindTransformer(tmpMenuInfo.TransformerId);
  tmpLivingPlate.Publication.Publisher := FEngine.FindPublisher(tmpMenuInfo.PublisherId);
  item.TabData.TabCaption:= tmpLivingPlate.Publication.Publisher.GetDescription;
  item.TabData.TabColor:= DEFAULT_TAB_COLOR;

  BuildAndFeedPlate(tmpLivingPlate, item);
end;

procedure TUramakiDesktopManager.BuildAndFeedPlate(aLivingPlate : TUramakiLivingPlate; aItem: TPanel);
var
  parentRolls : TStringList;
  i : integer;
begin
  try
    TWaitCursor.ShowWaitCursor('TUramakiDesktopManager.BuildAndFeedPlate');
    if Assigned(aLivingPlate.Publication) and Assigned(aLivingPlate.Publication.Publisher) then
    begin
      aLivingPlate.Plate := aLivingPlate.Publication.Publisher.CreatePlate(aItem);
      aLivingPlate.Plate.Parent := aItem;
      aLivingPlate.Plate.Align := alClient;
      aLivingPlate.Plate.EngineMediator := FEngine.Mediator;
      aLivingPlate.DoPlateStartShining := Self.DoStartShiningPanel;
      aLivingPlate.DoPlateStopShining:= Self.DoStopShiningPanel;

      if aItem is TUramakiDesktopSimplePanel then
      begin
        (aItem as TUramakiDesktopSimplePanel).AddMenuItem.Clear;
        parentRolls := TStringList.Create;
        try
          aLivingPlate.Plate.GetAvailableUramakiRolls(parentRolls);
          if parentRolls.Count > 0 then
          begin
            for i := 0 to parentRolls.Count - 1 do
            begin
              Self.FillAddWidgetMenu((aItem as TUramakiDesktopSimplePanel).AddMenuItem, parentRolls.Strings[i], aLivingPlate.InstanceIdentifier);
            end;
          end;
        finally
          parentRolls.Free;
        end;

        (aItem as TUramakiDesktopSimplePanel).DoRemovePanel := Self.DoRemovePanel;
      end;
      FEngine.FeedLivingPlate(aLivingPlate);
    end;
  finally
    TWaitCursor.UndoWaitCursor('TUramakiDesktopManager.BuildAndFeedPlate');
  end;
end;

procedure TUramakiDesktopManager.DoLinkLayoutItemToPlate(aItem: TPanel; aLivingPlateInstanceIdentificator: TGuid);
var
  tmpLivingPlate : TUramakiLivingPlate;
begin
  tmpLivingPlate := FEngine.FindLivingPlateByPlateId(aLivingPlateInstanceIdentificator);
  if Assigned(tmpLivingPlate) then
    BuildAndFeedPlate(tmpLivingPlate, aItem);
end;

procedure TUramakiDesktopManager.DoStartShiningPanel(const aPlate : TUramakiLivingPlate);
begin
  if (aPlate.Plate.Parent is TUramakiDesktopSimplePanel) then
  begin
    (aPlate.Plate.Parent as TUramakiDesktopSimplePanel).StartShining;
  end;
end;

procedure TUramakiDesktopManager.DoStopShiningPanel(const aPlate: TUramakiLivingPlate);
begin
  if (aPlate.Plate.Parent is TUramakiDesktopSimplePanel) then
  begin
    (aPlate.Plate.Parent as TUramakiDesktopSimplePanel).StopShining;
  end;
end;

procedure TUramakiDesktopManager.DoRemovePanel(const aLivingPlateInstanceIdentifier: TGuid);
var
  tmpLivingPlate : TUramakiLivingPlate;
  memStream : TMemoryStream;
  tmpPanel : TUramakiDesktopSimplePanel;
begin
  tmpLivingPlate := FEngine.FindLivingPlateByPlateId(aLivingPlateInstanceIdentifier);
  if Assigned (tmpLivingPlate) then
  begin
    tmpLivingPlate.Deleted := true;
    tmpPanel := FContainer.FindPanelByPlateId(aLivingPlateInstanceIdentifier);
    if Assigned(tmpPanel) then
      tmpPanel.Deleted:= true;

    MarkChildsAsDeleted(tmpLivingPlate);
  end;
  memStream := TMemoryStream.Create;
  try
    Self.SaveToStream(memStream);
    memStream.Position:= 0;
    Self.LoadFromStream(memStream);
  finally
    memStream.Free;
  end;
end;

procedure TUramakiDesktopManager.MarkChildsAsDeleted(const aPlate: TUramakiLivingPlate);
var
  childs : TObjectList;
  i : integer;
  tmpPanel : TUramakiDesktopSimplePanel;
begin
  childs := TObjectList.Create(false);
  try
    FEngine.FindLivingPlatesByParent(aPlate.Plate, childs);
    for i := 0 to childs.Count - 1 do
    begin
      (childs.Items[i] as TUramakiLivingPlate).Deleted:= true;
      tmpPanel := FContainer.FindPanelByPlateId((childs.Items[i] as TUramakiLivingPlate).InstanceIdentifier);
      if Assigned(tmpPanel) then
        tmpPanel.Deleted:= true;

      MarkChildsAsDeleted(childs.Items[i] as TUramakiLivingPlate);
    end;
  finally
    childs.Free;
  end;
end;

constructor TUramakiDesktopManager.Create;
begin
  FEngine := TUramakiEngine.Create;
  FDesktopDataModule:= TUramakiDesktopDataModule.Create(nil);
  FMenuGarbageCollector := TObjectList.Create(true);
end;

destructor TUramakiDesktopManager.Destroy;
begin
  FreeAndNil(FEngine);
  FreeAndNil(FDesktopDataModule);
  FreeAndNil(FMenuGarbageCollector);
  inherited Destroy;
end;

procedure TUramakiDesktopManager.Init(aParent : TWinControl);
begin
  FParentControl := aParent;

  FContainer := TUramakiDesktopContainerPanel.Create(FParentControl);
  FContainer.Parent := FParentControl;
  FContainer.Init(ctHorizontal);
  FContainer.Align:= alClient;
end;

procedure TUramakiDesktopManager.AddPublisher(aPublisher: TUramakiPublisher);
begin
  FEngine.AddPublisher(aPublisher);
end;

procedure TUramakiDesktopManager.AddTransformer(aTransformer: TUramakiTransformer);
begin
  FEngine.AddTransformer(aTransformer);
end;

procedure TUramakiDesktopManager.LoadFromStream(aStream : TStream);
var
  doc : TmXmlDocument;
  cursor : TmXmlElementCursor;
  tmp : TUramakiDesktopLayoutConfContainerItem;
begin
  try
    TWaitCursor.ShowWaitCursor('TUramakiDesktopManager.LoadFromStream');

    doc := TmXmlDocument.Create;
    try
      doc.LoadFromStream(aStream);

      cursor := TmXmlElementCursor.Create(doc.RootElement, 'livingPlates');
      try
        FEngine.LoadLivingPlatesFromXMLElement(cursor.Elements[0]);
      finally
        cursor.Free;
      end;

      cursor := TmXmlElementCursor.Create(doc.RootElement, 'layout');
      try
        tmp := TUramakiDesktopLayoutConfContainerItem.Create;
        try
          tmp.LoadFromXMLElement(cursor.Elements[0]);
          FContainer.ImportFromConfItem(tmp, Self.DoLinkLayoutItemToPlate);
        finally
          tmp.Free;
        end;
      finally
        cursor.Free;
      end;

      cursor := TmXmlElementCursor.Create(doc.RootElement, 'plates');
      try
        if Cursor.Count > 0 then
          FEngine.LoadPlatesFromXMLElement(Cursor.Elements[0]);
      finally
        cursor.Free;
      end;

    finally
      doc.Free;
    end;

    // Weird! This only to avoid strange random lock under Windows. Still don't know why.
    // Resolution: avoid use of Application.TaskBarBehavior:= tbMultiButton under Windows 10
    // Self.FParentControl.SetFocus;
  finally
    TWaitCursor.UndoWaitCursor('TUramakiDesktopManager.LoadFromStream');
  end;
end;

procedure TUramakiDesktopManager.SaveToStream(aStream : TStream);
var
  doc : TmXmlDocument;
  root : TmXmlElement;
  tmp : TUramakiDesktopLayoutConfItem;
begin
  doc := TmXmlDocument.Create;
  try
    root := doc.CreateRootElement('uramakiReport');
    root.SetIntegerAttribute('version', 1);
    tmp := FContainer.ExportAsConfItem;
    try
      tmp.SaveToXMLElement(root.AddElement('layout'));
      FEngine.SaveLivingPlatesToXMLElement(root.AddElement('livingPlates'));
      FEngine.SavePlatesToXMLElement(root.AddElement('plates'));

      doc.SaveToStream(aStream);
    finally
      tmp.Free;
    end;
  finally
    doc.Free;
  end;
end;

procedure TUramakiDesktopManager.ShowConfigurationForm;
var
  Dlg : TDesktopLayoutConfigForm;
  tmpConfItem, tmpConfItemOut : TUramakiDesktopLayoutConfItem;
  fakeDocument : TmXmlDocument;
begin
  Dlg := TDesktopLayoutConfigForm.Create(nil);
  try
    tmpConfItem := FContainer.ExportAsConfItem;
    try
      Dlg.Init(tmpConfItem);
      if Dlg.ShowModal = mrOk then
      begin
        try
          TWaitCursor.ShowWaitCursor('TUramakiDesktopManager.ShowConfigurationForm');
          tmpConfItemOut := Dlg.ExtractModifiedLayout;
          try
            assert (tmpConfItemOut is TUramakiDesktopLayoutConfContainerItem);
            fakeDocument := TmXmlDocument.Create;
            try
              FEngine.SavePlatesToXMLElement(fakeDocument.CreateRootElement('root'));
              FContainer.ImportFromConfItem(tmpConfItemOut, Self.DoLinkLayoutItemToPlate);
              FEngine.LoadPlatesFromXMLElement(fakeDocument.RootElement);
            finally
              fakeDocument.Free;
            end;
          finally
            tmpConfItemOut.Free;
          end;
        finally
          TWaitCursor.UndoWaitCursor('TUramakiDesktopManager.ShowConfigurationForm');
        end;
      end;
    finally
      tmpConfItem.Free;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TUramakiDesktopManager.FillAddWidgetMenu(aMenuItem: TMenuItem; const aInputUramakiId : string; aLivingPlateIdentifier : TGuid);

  function ExplodeMenuItem (aCategories : TStringList; var aCurIndex : integer; aMenuItem : TMenuItem) : TMenuItem;
  var
    i : integer;
    curCategory : String;
    newMenu : TMenuItem;
  begin
    Result := nil;
    curCategory := aCategories.Strings[aCurIndex];
    for i := 0 to aMenuItem.Count - 1 do
    begin
      if aMenuItem.Items[i].Caption = curCategory then
      begin
        if aCurIndex = aCategories.Count - 1 then
          Result := aMenuItem.Items[i]
        else
        begin
          inc (aCurIndex);
          Result := ExplodeMenuItem(aCategories, aCurIndex, aMenuItem.Items[i]);
        end;
        break;
      end;
    end;

    if not Assigned(Result) then
    begin
      newMenu := TMenuItem.Create(aMenuItem);
      newMenu.Caption := curCategory;
      aMenuItem.Add(newMenu);
      if aCurIndex = aCategories.Count - 1 then
        Result := newMenu
      else
      begin
        inc(aCurIndex);
        Result := ExplodeMenuItem(aCategories, aCurIndex, newMenu);
      end;
    end;
  end;
var
  i, j, k, w : integer;
  tempListOfTransformers : TUramakiTransformers;
  tempListOfPublishers : TUramakiPublishers;
  mt, mt2, mtCategory : TMenuItem;
  tmpMenuInfo : TMenuInfo;
  categories : TStringList;
begin
  if not Assigned(aMenuItem) then
    exit;

  //aMenuItem.Clear;

  tempListOfTransformers := TUramakiTransformers.Create;
  tempListOfPublishers := TUramakiPublishers.Create;
  try

    // publishers without transformers
    FEngine.GetAvailablePublishers(aInputUramakiId, tempListOfPublishers);
    if tempListOfPublishers.Count > 0 then
    begin
      for j := 0 to tempListOfPublishers.Count - 1 do
      begin
        mt2 := TMenuItem.Create(aMenuItem);
        mt2.Caption := tempListOfPublishers.Get(j).GetDescription;
        mt2.OnClick:= Self.OnAddPlate;
        tmpMenuInfo := TMenuInfo.Create;
        tmpMenuInfo.PublisherId:= tempListOfPublishers.Get(j).GetMyId;
        tmpMenuInfo.TransformerId:= '';
        tmpMenuInfo.LivingPlateIdenfier := aLivingPlateIdentifier;
        mt2.Tag:= PtrInt(tmpMenuInfo);
        FMenuGarbageCollector.Add(tmpMenuInfo);

        if tempListOfPublishers.Get(j).GetCategory <> '' then
        begin
          categories := TStringList.Create;
          try
            categories.StrictDelimiter:= true;
            categories.Delimiter:= TUramakiPublisher.CATEGORY_LEVELS_SEPARATOR;
            categories.DelimitedText:= tempListOfPublishers.Get(j).GetCategory;
            w := 0;
            mtCategory := ExplodeMenuItem(categories, w, aMenuItem);
            mtCategory.Add(mt2);
          finally
            categories.Free;
          end;
        end
        else
          aMenuItem.Add(mt2);
      end;
    end;

    // root transformers
    FEngine.GetAvailableTransformers(aInputUramakiId, tempListOfTransformers);
    for i := 0 to tempListOfTransformers.Count -1 do
    begin
      FEngine.GetAvailablePublishers(tempListOfTransformers.Get(i).GetOutputUramakiId, tempListOfPublishers);
      if tempListOfPublishers.Count > 0 then
      begin
        mt := TMenuItem.Create(aMenuItem);
        mt.Caption:= tempListOfTransformers.Get(i).GetDescription;
        aMenuItem.Add(mt);
        for j := 0 to tempListOfPublishers.Count - 1 do
        begin
          mt2 := TMenuItem.Create(aMenuItem);
          mt2.Caption := tempListOfPublishers.Get(j).GetDescription;
          mt2.OnClick:= Self.OnAddPlate;
          tmpMenuInfo := TMenuInfo.Create;
          tmpMenuInfo.PublisherId:= tempListOfPublishers.Get(j).GetMyId;
          tmpMenuInfo.TransformerId:= tempListOfTransformers.Get(i).GetMyId;
          tmpMenuInfo.LivingPlateIdenfier := aLivingPlateIdentifier;
          mt2.Tag:= PtrInt(tmpMenuInfo);
          FMenuGarbageCollector.Add(tmpMenuInfo);
          mt.Add(mt2);
        end;
      end;
    end;
  finally
    tempListOfPublishers.Free;
    tempListOfTransformers.Free;
  end;
end;


end.
