unit UramakiDesktopGUI;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, ExtCtrls, ComCtrls,
  Graphics,
  oMultiPanelSetup, OMultiPanel;

type

  TUramakiDesktopLayoutItem = class (TPanel)

  end;

  { TUramakiDesktopLayoutSimpleItem }

  TUramakiDesktopLayoutSimpleItem = class(TUramakiDesktopLayoutItem)
  strict private
    FTitleBar : TPanel;
    FContentPanel : TPanel;
  public
    constructor Create(TheOwner: TComponent); override;

    property TitleBar : TPanel read FTitleBar;
    property ContentPanel : TPanel read FContentPanel;
  end;

  TContainerType = (ctVertical, ctHorizontal, ctTabbed);

  { TUramakiDesktopLayoutContainerItem }

  TUramakiDesktopLayoutContainerItem = class (TUramakiDesktopLayoutItem)
  strict private
    FContainerType : TContainerType;
    FRootPanel : TOMultiPanel;
    FPageControl : TPageControl;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Init(aContainerType : TContainerType);

    function AddItem : TUramakiDesktopLayoutSimpleItem;
    function AddContainer : TUramakiDesktopLayoutContainerItem;
  end;

implementation

{ TUramakiDesktopLayoutContainerItem }

constructor TUramakiDesktopLayoutContainerItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TUramakiDesktopLayoutContainerItem.Init(aContainerType: TContainerType);
begin
  FContainerType:= aContainerType;
  if FContainerType = ctTabbed then
  begin
    FPageControl := TPageControl.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.Align:= alClient;
  end
  else
  begin
    FRootPanel := TOMultiPanel.Create(Self);
    FRootPanel.Parent := Self;
    FRootPanel.Align:= alClient;
    if FContainerType = ctHorizontal then
      FRootPanel.PanelType:= ptHorizontal
    else
      FRootPanel.PanelType:= ptVertical;
  end;
end;

function TUramakiDesktopLayoutContainerItem.AddItem : TUramakiDesktopLayoutSimpleItem;
var
  ts : TTabSheet;
begin
  if FContainerType = ctTabbed then
  begin
    ts := FPageControl.AddTabSheet;
    Result := TUramakiDesktopLayoutSimpleItem.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
  end
  else
  begin
    Result := TUramakiDesktopLayoutSimpleItem.Create(FRootPanel);
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
  end;
end;

function TUramakiDesktopLayoutContainerItem.AddContainer: TUramakiDesktopLayoutContainerItem;
var
  ts : TTabSheet;
begin
  if FContainerType = ctTabbed then
  begin
    ts := FPageControl.AddTabSheet;
    Result := TUramakiDesktopLayoutContainerItem.Create(ts);
    Result.Parent := ts;
    Result.Align := alClient;
  end
  else
  begin
    Result := TUramakiDesktopLayoutContainerItem.Create(FRootPanel);
    Result.Parent := FRootPanel;
    Result.Align:= alClient;
    FRootPanel.PanelCollection.AddControl(Result);
  end;
end;

{ TUramakiDesktopLayoutSimpleItem }

constructor TUramakiDesktopLayoutSimpleItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitleBar := TPanel.Create(Self);
  FTitleBar.Color:= clBlue;
  FTitleBar.Parent := Self;
  FTitleBar.Align:= alTop;
  FTitleBar.BorderStyle:= bsNone;
  FTitleBar.BevelInner:= bvNone;
  FTitleBar.BevelOuter:= bvNone;
  FTitleBar.Height:= 30;

  FContentPanel := TPanel.Create(Self);
  FContentPanel.Parent := Self;
  FContentPanel.BorderStyle:= bsNone;
  FContentPanel.BevelOuter:= bvNone;
  FContentPanel.BevelInner:= bvNone;
  FContentPanel.Align:= alClient;
end;

end.
