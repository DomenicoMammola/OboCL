unit UramakiDesktopLayoutLCLConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, ComCtrls, StdCtrls, Menus, contnrs,
  UramakiDesktopLayout;

type

  { TDesktopLayoutConfigForm }

  TDesktopLayoutConfigForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    LayoutTreeView: TTreeView;
    MI_NewVertical: TMenuItem;
    MI_Tabbed: TMenuItem;
    MI_NewHorizontal: TMenuItem;
    PopupMenuAdd: TPopupMenu;
    ToolBar1: TToolBar;
    BtnAdd: TToolButton;
    procedure LayoutTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LayoutTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure AddNewContainer (aContainerType : TContainerType);
    procedure MI_NewHorizontalClick(Sender: TObject);
    procedure MI_NewVerticalClick(Sender: TObject);
    procedure MI_TabbedClick(Sender: TObject);
  private
    FWIPLayout : TUramakiDesktopLayoutConfItem;
    FGarbage : TObjectList;
    procedure AddNode (aParentNode : TTreeNode; aLayoutItem : TUramakiDesktopLayoutConfItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aSource : TUramakiDesktopLayoutConfItem);
    function ExtractModifiedLayout : TUramakiDesktopLayoutConfItem;
  end;


implementation

{$R *.lfm}

{ TDesktopLayoutConfigForm }

procedure TDesktopLayoutConfigForm.LayoutTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = LayoutTreeView);
end;

procedure TDesktopLayoutConfigForm.AddNewContainer(aContainerType : TContainerType);
var
  Item: TUramakiDesktopLayoutConfContainerItem;
begin
  Item:= TUramakiDesktopLayoutConfContainerItem.Create;
  FGarbage.Add(Item);
  Item.ContainerType:= aContainerType;
  if Assigned(LayoutTreeView.Selected) and (LayoutTreeView.Selected <> LayoutTreeView.Items.GetFirstNode) then
    LayoutTreeView.Items.AddObject(LayoutTreeView.Selected, Item.GetDescription, Item)
  else
    LayoutTreeView.Items.AddChildObject(LayoutTreeView.Items.GetFirstNode, Item.GetDescription, Item);
end;

procedure TDesktopLayoutConfigForm.MI_NewHorizontalClick(Sender: TObject);
begin
  AddNewContainer(ctHorizontal);
end;

procedure TDesktopLayoutConfigForm.MI_NewVerticalClick(Sender: TObject);
begin
  AddNewContainer(ctVertical);
end;

procedure TDesktopLayoutConfigForm.MI_TabbedClick(Sender: TObject);
begin
  AddNewContainer(ctTabbed);
end;

procedure TDesktopLayoutConfigForm.LayoutTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  tv     : TTreeView;
  iNode  : TTreeNode;
  confItem : TUramakiDesktopLayoutConfItem;
begin
  tv := TTreeView(Sender);      { Sender is TreeView where the data is being dropped  }
  iNode := tv.GetNodeAt(x,y);   { x,y are drop coordinates (relative to the Sender)   }
                                {   since Sender is TreeView we can evaluate          }
                                {   a tree at the X,Y coordinates                     }
  if Source = LayoutTreeView then begin         { drop is happening within a TreeView   }
    if Assigned(tv.Selected) and             {  check if any node has been selected  }
      (iNode <> tv.Selected) and (tv.Selected <> tv.Items.GetFirstNode) then            {   and we're dropping to another node  }
    begin
      if iNode <> nil then
      begin
        confItem := TUramakiDesktopLayoutConfItem(iNode.Data);
        if confItem is TUramakiDesktopLayoutConfContainerItem then
        begin
          if iNode.GetLastChild = tv.Selected then
            tv.Selected.MoveTo(iNode, naAddChildFirst)
          else
            tv.Selected.MoveTo(iNode, naAddChild); { complete the drop operation, by moving the selectede node }
        end
        else
        begin
          if tv.Selected.GetNextSibling = iNode then
            tv.Selected.MoveTo(iNode, naInsertBehind)
          else
            tv.Selected.MoveTo(iNode, naInsert);
        end;
      end
      else
        tv.Selected.MoveTo(iNode, naAdd); { complete the drop operation, by moving in root of a TreeView }
    end;
  end;
end;

procedure TDesktopLayoutConfigForm.AddNode(aParentNode: TTreeNode; aLayoutItem: TUramakiDesktopLayoutConfItem);
var
  tmpContainer : TUramakiDesktopLayoutConfContainerItem;
  tmpNode : TTreeNode;
  i : integer;
begin
  if not Assigned(aParentNode) then
    tmpNode := LayoutTreeView.Items.AddObject(aParentNode, aLayoutItem.GetDescription, aLayoutItem)
  else
    tmpNode := LayoutTreeView.Items.AddChildObject(aParentNode, aLayoutItem.GetDescription, aLayoutItem);
  if aLayoutItem is TUramakiDesktopLayoutConfContainerItem then
  begin
    tmpContainer := aLayoutItem as TUramakiDesktopLayoutConfContainerItem;
    for i := 0 to tmpContainer.Count - 1 do
    begin
      AddNode(tmpNode, tmpContainer.Get(i));
    end;
  end;
end;

constructor TDesktopLayoutConfigForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWIPLayout := nil;
  FGarbage:= TObjectList.Create(true);
end;

destructor TDesktopLayoutConfigForm.Destroy;
begin
  FreeAndNil(FWIPLayout);
  FGarbage.Free;
  inherited Destroy;
end;

procedure TDesktopLayoutConfigForm.Init(aSource: TUramakiDesktopLayoutConfItem);
begin
  FWIPLayout := TUramakiDesktopLayoutConfContainerItem.Create;
  (FWIPLayout as TUramakiDesktopLayoutConfContainerItem).Assign(aSource, true);
  LayoutTreeView.BeginUpdate;
  try
    LayoutTreeView.Items.Clear;
    AddNode(nil, FWIPLayout);
  finally
    LayoutTreeView.EndUpdate;
  end;
end;

function TDesktopLayoutConfigForm.ExtractModifiedLayout: TUramakiDesktopLayoutConfItem;

  function AlmenoUnDiscendenteNonContainer (aNode : TTreeNode) : boolean;
  var
    layoutItem : TUramakiDesktopLayoutConfItem;
    tmpNode : TTreeNode;
  begin
    Result := false;
    layoutItem := TUramakiDesktopLayoutConfItem(aNode.Data);
    if (layoutItem is TUramakiDesktopLayoutConfContainerItem) then
    begin
      tmpNode := aNode.GetFirstChild;
      while Assigned(tmpNode) and (not Result) do
      begin
        if TUramakiDesktopLayoutConfItem(tmpNode.Data) is TUramakiDesktopLayoutConfSimpleItem then
        begin
          Result := true;
          break;
        end
        else
        begin
          Result := AlmenoUnDiscendenteNonContainer(tmpNode);
          if Result then
            break;
        end;
        tmpNode := tmpNode.GetNextSibling;
      end;
    end;
  end;

  function ExportLayoutFromNode (aNode : TTreeNode) : TUramakiDesktopLayoutConfItem;
  var
    layoutItem : TUramakiDesktopLayoutConfItem;
    tmpNode : TTreeNode;
    newItem : TUramakiDesktopLayoutConfItem;
  begin
    Result := nil;
    layoutItem := TUramakiDesktopLayoutConfItem(aNode.Data);
    if (layoutItem is TUramakiDesktopLayoutConfContainerItem) and AlmenoUnDiscendenteNonContainer(aNode) then
    begin
      // it's a container with one or more childs, containers without childs are discarded
      Result := TUramakiDesktopLayoutConfContainerItem.Create;
      (Result as TUramakiDesktopLayoutConfContainerItem).Assign(layoutItem, false);
      tmpNode := aNode.GetFirstChild;
      while Assigned(tmpNode) do
      begin
        newItem := ExportLayoutFromNode(tmpNode);
        if Assigned(newItem) then
          (Result as TUramakiDesktopLayoutConfContainerItem).AddItem(newItem);
        tmpNode := tmpNode.GetNextSibling;
      end;
    end
    else
    if (layoutItem is TUramakiDesktopLayoutConfSimpleItem) then
    begin
      Result := TUramakiDesktopLayoutConfSimpleItem.Create;
      (Result as TUramakiDesktopLayoutConfSimpleItem).Assign(layoutItem);
    end;
  end;


begin
  Result := ExportLayoutFromNode(LayoutTreeView.Items.GetFirstNode);
end;

end.

