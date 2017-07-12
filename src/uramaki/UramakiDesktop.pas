unit UramakiDesktop;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, ComCtrls, Graphics, Menus, contnrs, ExtCtrls,
  oMultiPanelSetup,
  mXML,
  UramakiBase, UramakiEngine, UramakiEngineClasses,
  UramakiDesktopBase, UramakiDesktopGUI, UramakiDesktopLayout,
  UramakiDesktopLayoutLCLConfigForm;

type

  { TUramakiDesktopManager }

  TUramakiDesktopManager = class
  strict private
    FEngine : TUramakiEngine;
    FToolbar : TToolbar;
    FParentControl : TWinControl;
    FContainer : TUramakiDesktopLayoutContainerItem;
    FRootPopupMenu : TPopupMenu;

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

    property Toolbar : TToolBar read FToolbar;
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
  tmpBtn.Parent := FToolbar;
  tmpBtn.OnClick:= OnSaveToFile;

  tmpBtn := TToolButton.Create(FToolbar);
  tmpBtn.Style := tbsButton;
  tmpBtn.Parent := FToolbar;
  tmpBtn.OnClick:= OnLoadFromFile;

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
  tmpMenu := (Sender as TPopupMenu);
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
  item : TUramakiDesktopLayoutSimpleItem;
  tmpLivingPlate : TUramakiLivingPlate;
begin
  tmpMenuInfo := TMenuInfo((Sender as TMenuItem).Tag);
  item := FContainer.AddItem;
  tmpLivingPlate := FEngine.CreateLivingPlate(tmpMenuInfo.LivingPlateIdenfier);
  item.LivingPlateInstanceIdentifier := tmpLivingPlate.InstanceIdentifier;
  tmpLivingPlate.Transformations.Add.Transformer := FEngine.FindTransformer(tmpMenuInfo.TransformerId);
  tmpLivingPlate.Publication.Publisher := FEngine.FindPublisher(tmpMenuInfo.PublisherId);

  tmpLivingPlate.Plate := tmpLivingPlate.Publication.Publisher.CreatePlate;
  (tmpLivingPlate.Plate as TUramakiDesktopPlate).LinkToPanel(item);

  FEngine.FeedLivingPlate(tmpLivingPlate);
end;

procedure TUramakiDesktopManager.OnConfigureLayout(Sender: TObject);
begin
  //
end;

procedure TUramakiDesktopManager.OnSaveToFile(Sender: TObject);
var
  doc : TmXmlDocument;
  root : TmXmlElement;
begin
  doc := TmXmlDocument.Create;
  try
    root := doc.CreateRootElement('uramakiReport');
    root.SetIntegerAttribute('version', 1);
    FContainer.SaveToXMLElement(root.AddElement('layout'));
    FEngine.SaveToXMLElement(root.AddElement('plates'));

    doc.SaveToFile('c:\temp\layout.xml');
  finally
    doc.Free;
  end;
end;

procedure TUramakiDesktopManager.OnLoadFromFile(Sender: TObject);
var
  doc : TmXmlDocument;
  cursor : TmXmlElementCursor;
begin
  doc := TmXmlDocument.Create;
  try
    doc.LoadFromFile('c:\temp\layout.xml');

    cursor := TmXmlElementCursor.Create(doc.RootElement, 'plates');
    try
      FEngine.LoadFromXMLElement(cursor.Elements[0]);
    finally
      cursor.Free;
    end;

    cursor := TmXmlElementCursor.Create(doc.RootElement, 'layout');
    try
      FContainer.LoadFromXMLElement(cursor.Elements[0], Self.DoLinkLayoutItemToPlate);
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
  tmpLivingPlate.Plate := tmpLivingPlate.Publication.Publisher.CreatePlate;
  (tmpLivingPlate.Plate as TUramakiDesktopPlate).LinkToPanel(aItem);
  FEngine.FeedLivingPlate(tmpLivingPlate);
end;

constructor TUramakiDesktopManager.Create;
begin
  FEngine := TUramakiEngine.Create;
end;

destructor TUramakiDesktopManager.Destroy;
begin
  FreeAndNil(FEngine);
  inherited Destroy;
end;

procedure TUramakiDesktopManager.Init(aParent : TWinControl);
begin
  FParentControl := aParent;

  FRootPopupMenu := TPopupMenu.Create(FParentControl);
  FRootPopupMenu.OnPopup:= Self.BuildRootPopupMenu;

  FToolbar := TToolBar.Create(FParentControl);
  FToolbar.Parent := FParentControl;
  FToolbar.Align:= alTop;
  Self.CreateToolbar;

  FContainer := TUramakiDesktopLayoutContainerItem.Create(FParentControl);
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
