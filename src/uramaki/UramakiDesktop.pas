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
  Controls, ComCtrls, Graphics, Menus, contnrs, ExtCtrls,

  oMultiPanelSetup,
  ATButtons, ATButtonsToolbar,
  mXML,

  UramakiBase, UramakiEngine, UramakiEngineClasses,
  UramakiDesktopBase, UramakiDesktopGUI, UramakiDesktopLayout,
  UramakiDesktopLayoutLCLConfigForm, UramakiDesktopLCLIcons;

type

  { TUramakiDesktopManager }

  TUramakiDesktopManager = class
  strict private
    FEngine : TUramakiEngine;
    FToolbar : TATButtonsToolbar;
    FParentControl : TWinControl;
    FContainer : TUramakiDesktopContainerPanel;
    FRootPopupMenu : TPopupMenu;
    FDesktopDataModule: TUramakiDesktopDataModule;

    procedure CreateToolbar;
    procedure BuildRootPopupMenu (Sender: TObject);
    procedure OnAddPlate (Sender : TObject);
    procedure OnConfigureLayout (Sender : TObject);
    procedure OnSaveToFile(Sender : TObject);
    procedure OnLoadFromFile (Sender : TObject);
    procedure DoLinkLayoutItemToPlate(aItem : TPanel; aLivingPlateInstanceIdentificator : TGuid);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init (aParent : TWinControl);

    procedure AddPublisher (aPublisher : TUramakiPublisher);
    procedure AddTransformer (aTransformer : TUramakiTransformer);

    //property Toolbar : TToolBar read FToolbar;
  end;

implementation

uses
  SysUtils, Dialogs;

type
  TMenuInfo = class
  public
    LivingPlateIdenfier : TGuid;
    TransformerId : string;
    PublisherId : string;
  end;

var
  MenuGarbageCollector : TObjectList;

{ TUramakiDesktopManager }

procedure TUramakiDesktopManager.CreateToolbar;
begin
//  FToolbar.AddButton(ICON_ADD, nil, '', '', '', false);
  FToolbar.AddDropdown(FRootPopupMenu, nil, 'Add..');
  FToolbar.AddButton(ICON_OPEN, OnLoadFromFile, '', 'Open a report file..', '', false);
  FToolbar.AddButton(ICON_SAVE, OnSaveToFile, '', 'Save to a report file..', '', false);
  FToolbar.AddButton(ICON_CONFIGURE, OnConfigureLayout, '', 'Configure layout of report', '', false);
  FToolbar.UpdateControls;

(*
var
  tmpBtn : TToolButton;
begin
  tmpBtn := TToolButton.Create(FToolbar);
  tmpBtn.Style:= tbsDropDown;
  tmpBtn.Caption:= 'Nuovo';
  tmpBtn.Parent := FToolbar;
  tmpBtn.DropdownMenu := FRootPopupMenu;

  tmpBtn := TToolButton.Create(FToolbar);
  tmpBtn.Style := tbsButton;
  tmpBtn.Caption:= 'Save';
  tmpBtn.Parent := FToolbar;
  tmpBtn.OnClick:= OnSaveToFile;

  tmpBtn := TToolButton.Create(FToolbar);
  tmpBtn.Style := tbsButton;
  tmpBtn.Caption:= 'Load';
  tmpBtn.Parent := FToolbar;
  tmpBtn.OnClick:= OnLoadFromFile;

  tmpBtn := TToolButton.Create(FToolbar);
  tmpBtn.Style := tbsButton;
  tmpBtn.Caption:= 'Conf';
  tmpBtn.Parent := FToolbar;
  tmpBtn.OnClick:= OnConfigureLayout;
*)
end;

procedure TUramakiDesktopManager.BuildRootPopupMenu(Sender: TObject);
var
  i, j : integer;
  tempListOfTransformers : TUramakiTransformers;
  tempListOfPublishers : TUramakiPublishers;
  mt, mt2 : TMenuItem;
  tmpMenu : TPopupMenu;
  tmpMenuInfo : TMenuInfo;
begin
  tmpMenu := FRootPopupMenu;// (Sender as TPopupMenu);
  tmpMenu.Items.Clear;
  MenuGarbageCollector.Clear;

  tempListOfTransformers := TUramakiTransformers.Create;
  tempListOfPublishers := TUramakiPublishers.Create;
  try
    FEngine.GetAvailableTransformers(NULL_URAMAKI_ID, tempListOfTransformers);
    for i := 0 to tempListOfTransformers.Count -1 do
    begin
      FEngine.GetAvailablePublishers(tempListOfTransformers.Get(i).GetOutputUramakiId, tempListOfPublishers);
      if tempListOfPublishers.Count > 0 then
      begin
        mt := TMenuItem.Create(tmpMenu);
        mt.Caption:= tempListOfTransformers.Get(i).GetDescription;
        tmpMenu.Items.Add(mt);
        for j := 0 to tempListOfPublishers.Count - 1 do
        begin
          mt2 := TMenuItem.Create(tmpMenu);
          mt2.Caption := tempListOfPublishers.Get(j).GetDescription;
          mt.Add(mt2);
          mt2.OnClick:= Self.OnAddPlate;
          tmpMenuInfo := TMenuInfo.Create;
          tmpMenuInfo.PublisherId:= tempListOfPublishers.Get(j).GetMyId;
          tmpMenuInfo.TransformerId:= tempListOfTransformers.Get(i).GetMyId;
          tmpMenuInfo.LivingPlateIdenfier := GUID_NULL;
          mt2.Tag:= PtrInt(tmpMenuInfo);
          MenuGarbageCollector.Add(tmpMenuInfo);
        end;
      end;
    end;
  finally
    tempListOfPublishers.Free;
    tempListOfTransformers.Free;
  end;
end;

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
  tmpLivingPlate.Transformations.Add.Transformer := FEngine.FindTransformer(tmpMenuInfo.TransformerId);
  tmpLivingPlate.Publication.Publisher := FEngine.FindPublisher(tmpMenuInfo.PublisherId);

  tmpLivingPlate.Plate := tmpLivingPlate.Publication.Publisher.CreatePlate(item);
  tmpLivingPlate.Plate.Parent := item;
  tmpLivingPlate.Plate.Align := alClient;

  FEngine.FeedLivingPlate(tmpLivingPlate);
end;

procedure TUramakiDesktopManager.OnConfigureLayout(Sender: TObject);
var
  Dlg : TDesktopLayoutConfigForm;
  tmpConfItem, tmpConfItemOut : TUramakiDesktopLayoutConfItem;
begin
  Dlg := TDesktopLayoutConfigForm.Create(nil);
  try
    tmpConfItem := FContainer.ExportAsConfItem;
    try
      Dlg.Init(tmpConfItem);
      if Dlg.ShowModal = mrOk then
      begin
        tmpConfItemOut := Dlg.ExtractModifiedLayout;
        try
          assert (tmpConfItemOut is TUramakiDesktopLayoutConfContainerItem);
          FContainer.ImportFromConfItem(tmpConfItemOut, Self.DoLinkLayoutItemToPlate);
        finally
          tmpConfItemOut.Free;
        end;
      end;
    finally
      tmpConfItem.Free;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TUramakiDesktopManager.OnSaveToFile(Sender: TObject);
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
      FEngine.SaveToXMLElement(root.AddElement('plates'));

      doc.SaveToFile('layout.xml');
    finally
      tmp.Free;
    end;
  finally
    doc.Free;
  end;
end;

procedure TUramakiDesktopManager.OnLoadFromFile(Sender: TObject);
var
  doc : TmXmlDocument;
  cursor : TmXmlElementCursor;
  tmp : TUramakiDesktopLayoutConfContainerItem;
begin
  doc := TmXmlDocument.Create;
  try
    doc.LoadFromFile('layout.xml');

    cursor := TmXmlElementCursor.Create(doc.RootElement, 'plates');
    try
      FEngine.LoadFromXMLElement(cursor.Elements[0]);
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
  finally
    doc.Free;
  end;
end;

procedure TUramakiDesktopManager.DoLinkLayoutItemToPlate(aItem: TPanel; aLivingPlateInstanceIdentificator: TGuid);
var
  tmpLivingPlate : TUramakiLivingPlate;
begin
  tmpLivingPlate := FEngine.FindLivingPlate(aLivingPlateInstanceIdentificator);
  assert (not Assigned(tmpLivingPlate.Plate));
  tmpLivingPlate.Plate := tmpLivingPlate.Publication.Publisher.CreatePlate(aItem);
  tmpLivingPlate.Plate.Parent := aItem;
  tmpLivingPlate.Plate.Align:= alClient;
  FEngine.FeedLivingPlate(tmpLivingPlate);
end;

constructor TUramakiDesktopManager.Create;
begin
  FEngine := TUramakiEngine.Create;
  FDesktopDataModule:= TUramakiDesktopDataModule.Create(nil);
end;

destructor TUramakiDesktopManager.Destroy;
begin
  FreeAndNil(FEngine);
  FreeAndNil(FDesktopDataModule);
  inherited Destroy;
end;

procedure TUramakiDesktopManager.Init(aParent : TWinControl);
begin
  FParentControl := aParent;

  FRootPopupMenu := TPopupMenu.Create(FParentControl);
  FRootPopupMenu.OnPopup:= Self.BuildRootPopupMenu;

(*  FToolbar := TToolBar.Create(FParentControl);
  FToolbar.Parent := FParentControl;
  FToolbar.Align:= alTop;*)

  FToolbar := TATButtonsToolbar.Create(FParentControl);
  FToolbar.Images := FDesktopDataModule.UramakiDesktopImageList;
  FToolbar.Parent := FParentControl;
  FToolbar.Align:= alTop;
  Self.CreateToolbar;

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

initialization
  MenuGarbageCollector := TObjectList.Create(true);

finalization
  MenuGarbageCollector.Free;

end.
