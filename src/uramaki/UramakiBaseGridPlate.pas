// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiBaseGridPlate;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, DB, ComCtrls, Windows, DBGrids,
  Forms,

  UramakiBase,
  mVirtualDataSet, mFilterPanel, mFilter, mGridHelper, mDBGrid,
  mQuickReadOnlyVirtualDataSet, mXML, mVirtualDataSetInterfaces;

const
  WM_USER_REFRESHCHILDS = WM_USER + 1;

type

  TDoFillRollFromDatasetRow = procedure (aUrakamiRoll : TUramakiRoll; const aDataset : TDataset) of object;

  { TBaseGridPlate }

  TBaseGridPlate = class abstract (TUramakiPlate)
  protected
    FGrid: TmDBGrid;
    FDataset: TVirtualDataset;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmDBGridHelper;
    FDatasource : TDatasource;
    FToolbar : TToolBar;
    FLastSelectedRow : integer;
    FFilterPanel : TmFilterPanel;

    // override these:
    function GetDataProvider : IVDListDataProvider; virtual; abstract;
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;

    procedure CreateToolbar(aImageList : TImageList; aConfigureImageIndex : integer);
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow);
    procedure ProcessRefreshChilds(var Message: TMessage); message WM_USER_REFRESHCHILDS;
    procedure OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure OnDblClick(Sender : TObject);
    procedure InvokeChildsRefresh;

    procedure OnClearFilter (Sender : TObject);
    procedure OnExecuteFilter (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls;
    procedure EnableControls;
    procedure RefreshDataset;

    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); override;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); override;
  end;

implementation


{ TBaseGridPlate }


procedure TBaseGridPlate.CreateToolbar(aImageList : TImageList; aConfigureImageIndex : integer);
begin
  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align:= alTop;
  FToolbar.Images := aImageList;
  FToolbar.ShowHint:= true;
  FGridHelper.CreateStandardConfigureMenu(FToolbar, aConfigureImageIndex);
end;

procedure TBaseGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow);
var
  tmpBookmark : TBookMark;
  i : integer;
begin
  if FDataset.ControlsDisabled then
    exit;

  if (FGrid.SelectedRows.Count > 1) then
  begin
    FGrid.BeginUpdate;
    try
      tmpBookmark := FDataset.Bookmark;
      try
        for i := 0 to FGrid.SelectedRows.Count - 1 do
        begin
          FDataset.GotoBookmark(FGrid.SelectedRows[i]);
          aDoFillRollFromDatasetRow(aUramakiRoll, FDataset);
        end;
      finally
        FDataset.GotoBookmark(tmpBookmark);
      end;
    finally
      FGrid.EndUpdate(true);
    end;
  end
  else if FGrid.SelectedRows.Count =  1 then
  begin
    aDoFillRollFromDatasetRow(aUramakiRoll, FDataset);
  end;
end;

procedure TBaseGridPlate.ProcessRefreshChilds(var Message: TMessage);
begin
  FGrid.OnSelectEditor:= nil;
//  FGrid.OnDblClick:= nil;
  try
    EngineMediator.PleaseRefreshMyChilds(Self);
  finally
//    FGrid.OnDblClick:= Self.OnDblClick;
    FGrid.OnSelectEditor:= Self.OnSelectEditor;
  end;
end;

procedure TBaseGridPlate.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
begin
  if FLastSelectedRow <> FGrid.Row then
  begin
    FLastSelectedRow := FGrid.Row;
    if Assigned(Self.Parent) then
      InvokeChildsRefresh;
  end;
end;

procedure TBaseGridPlate.OnDblClick(Sender: TObject);
begin
  if Assigned(Self.Parent) then
    InvokeChildsRefresh;
end;

procedure TBaseGridPlate.InvokeChildsRefresh;
begin
  PostMessage(Self.Handle, WM_USER_REFRESHCHILDS, 0, 0);
end;

procedure TBaseGridPlate.OnClearFilter(Sender: TObject);
begin
  if Assigned(FFilterPanel) then
  begin
    FFilterPanel.ClearAll();
    Self.DisableControls;
    try
      GetDataProvider.Clear();
      RefreshDataset;
    finally
      Self.EnableControls;
    end;
    InvokeChildsRefresh;
  end;
end;

procedure TBaseGridPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
  oldCursor : TCursor;
begin
  Self.DisableControls;
  try
    OldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      tmpFilters := TmFilters.Create;
      try
        FFilterPanel.GetFilters(tmpFilters);
        Self.ReloadData(tmpFilters);
        RefreshDataset;
      finally
        tmpFilters.Free;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  finally
    Self.EnableControls;
  end;
  InvokeChildsRefresh;
end;

procedure TBaseGridPlate.DisableControls;
begin
  FDataset.DisableControls;
end;

procedure TBaseGridPlate.EnableControls;
begin
  FDataset.EnableControls;
end;

procedure TBaseGridPlate.RefreshDataset;
begin
  FGrid.SelectedRows.Clear;
  FGrid.SelectedIndex:= -1;
  FLastSelectedRow := -1;
  FDataset.Refresh;
end;

constructor TBaseGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGrid := TmDBGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  FDatasource:= TDataSource.Create(Self);
  FGrid.DataSource := FDataSource;


  FDataset := TVirtualDataset.Create(nil);
  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  FDataset.DatasetDataProvider := FProvider;
  FDatasource.DataSet := FDataset;

  FGridHelper:= TmDBGridHelper.Create(FGrid, FProvider.FormulaFields);
  FGridHelper.SetupGrid;
  FGrid.SortManager := Self.FDataset.SortManager;
  FGrid.FilterManager := Self.FDataset.FilterManager;
  FGrid.ColumnsHeaderMenuVisible:= true;

  FLastSelectedRow:=-1;
  FGrid.OnSelectEditor:= Self.OnSelectEditor;
  //FGrid.OnDblClick:= Self.OnDblClick;
end;

destructor TBaseGridPlate.Destroy;
begin
  FGridHelper.Free;
  FProvider.Free;
  FDataset.Free;

  inherited Destroy;
end;

procedure TBaseGridPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
var
  Cursor : TmXmlElementCursor;
begin
  Cursor := TmXmlElementCursor.Create(aXMLElement, 'gridConfiguration');
  try
    if Cursor.Count > 0 then
      FGridHelper.LoadSettingsFromXML(Cursor.Elements[0]);
  finally
    Cursor.Free;
  end;

end;

procedure TBaseGridPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
begin
  FGridHelper.SaveSettingsToXML(aXMLElement.AddElement('gridConfiguration'));
end;

end.
