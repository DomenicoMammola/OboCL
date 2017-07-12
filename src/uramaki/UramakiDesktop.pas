unit UramakiDesktop;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, ComCtrls, Graphics, Menus, contnrs,
  oMultiPanelSetup,
  UramakiBase, UramakiEngine, UramakiEngineClasses,
  UramakiDesktopBase, UramakiDesktopGUI;

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
  tmpLivingPlate := FEngine.CreateLivingPlate(GUID_NULL);
  tmpLivingPlate.Transformations.Add.Transformer := FEngine.FindTransformer(tmpMenuInfo.TransformerId);
  tmpLivingPlate.Publication.Publisher := FEngine.FindPublisher(tmpMenuInfo.PublisherId);
  tmpLivingPlate.Plate := tmpLivingPlate.Publication.Publisher.CreatePlate;
  (tmpLivingPlate.Plate as TUramakiDesktopPlate).Init(nil, item);
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
  FContainer.Init(ctTabbed);
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
