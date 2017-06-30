// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  DBGrids, Controls, Menus,
  mGridColumnSettings, mXML, mGridSettingsForm, mSortConditions, mGridIcons,
  mDatasetInterfaces;

type

  { TmDBGridHelper }

  TmDBGridHelper = class
  strict private
    FSettings : TmGridColumnsSettings;
    FDBGrid : TDBGrid;
    FSortManager : ISortableDatasetManager;
    FGridIcons: TmGridIconsDataModule;
    FHeaderPopupMenu : TPopupMenu;
    FCurrentGridCol : longint;
    FGridPopupMenu : TPopupMenu;
    // original events
    FOriginalOnTitleClick : TDBGridClickEvent;
    FOriginalOnMouseDown : TMouseEvent;
    procedure OnTitleClick (Column: TColumn);
    procedure OnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BuildHeaderPopupMenu;
  public
    constructor Create(aGrid : TDBGrid);
    destructor Destroy; override;
    function EditSettings : boolean;
    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);
    procedure EnableSort (aSortManager : ISortableDatasetManager);
    procedure EnableHeaderPopupMenu (aGridPopupMenu : TPopupMenu);

    property Grid : TDBGrid read FDBGrid;
  end;

implementation

uses
  SysUtils;

{ TmDBGridHelper }


// inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
procedure TmDBGridHelper.OnTitleClick(Column: TColumn);
var
  tmpSortType : TSortType;
  tmpSortConditions : TSortByConditions;
  i, idx : integer;
begin
  try
    tmpSortType := stAscending;

    // remove every arrow from column captions
    for i := 0 to FDBGrid.Columns.Count - 1 do
      FDBGrid.Columns[i].Title.ImageIndex := -1;

    // analize current filter
    if (FSortManager.GetSorted) and (FSortManager.GetSortByConditions.Count > 0) and (FSortManager.GetSortByConditions.Items[0].FieldName = Column.FieldName) then
    begin
      if FSortManager.GetSortByConditions.Items[0].SortType = stAscending then
        tmpSortType:= stDescending
      else
      begin
        FSortManager.ClearSort;
        exit;
      end
    end;

    // set new sort condition
    FSortManager.GetSortByConditions.Clear;
    with FSortManager.GetSortByConditions.Add do
    begin
      FieldName:= Column.FieldName;
      SortType:= tmpSortType;
    end;

    // do sort
    if FSortManager.Sort then
    begin
      if tmpSortType = stAscending then
        idx := 0
      else
        idx := 1;
      FDBGrid.Columns[Column.Index].Title.ImageIndex := idx;
    end;
  finally
    if Assigned(FOriginalOnTitleClick) then
      FOriginalOnTitleClick(Column);
  end;
end;

procedure TmDBGridHelper.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tmpCol, tmpRow : longint;
begin
  // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
  if Button = mbRight then
  begin
    if Y < FDBGrid.DefaultRowHeight then
    begin
       FDBGrid.PopupMenu := FHeaderPopupMenu;
       FDBGrid.MouseToCell(X, Y, tmpCol, tmpRow);
       FCurrentGridCol := tmpCol;
    end
    else
       FDBGrid.PopupMenu := FGridPopupMenu;
  end;

  if Assigned(FOriginalOnMouseDown) then
    FOriginalOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TmDBGridHelper.BuildHeaderPopupMenu;
var
  tmpMenuItem : TMenuItem;
begin
  if not Assigned(FHeaderPopupMenu) then
  begin
    FHeaderPopupMenu:= TPopupMenu.Create(FDBGrid);
    tmpMenuItem := TMenuItem.Create(FHeaderPopupMenu);
    tmpMenuItem.Caption:= 'Filter values..';
    FHeaderPopupMenu.Items.Add(tmpMenuItem);
    tmpMenuItem := TMenuItem.Create(FHeaderPopupMenu);
    tmpMenuItem.Caption:= 'Add summary..';
    FHeaderPopupMenu.Items.Add(tmpMenuItem);
  end;
end;

constructor TmDBGridHelper.Create(aGrid : TDBGrid);
begin
  FSettings := TmGridColumnsSettings.Create;
  FSortManager := nil;
  FDBGrid := aGrid;
end;

destructor TmDBGridHelper.Destroy;
begin
  FSortManager := nil;
  FDBGrid := nil;
  FSettings.Free;
  inherited Destroy;
end;

function TmDBGridHelper.EditSettings : boolean;
var
  frm : TGridSettingsForm;
begin
  Result := false;
  ReadSettingsFromGrid(FSettings, FDBGrid);
  frm := TGridSettingsForm.Create(nil);
  try
    frm.Init(FSettings);
    if frm.ShowModal = mrOk then
    begin
      ApplySettingsToGrid(FSettings, FDBGrid);
      Result := true;
    end;
  finally
    frm.Free;
  end;
end;

procedure TmDBGridHelper.LoadSettings(aStream: TStream);
var
  doc : TmXmlDocument;
  cursor : TmXmlElementCursor;
begin
  doc := TmXmlDocument.Create;
  try
    doc.LoadFromStream(aStream);
    cursor := TmXmlElementCursor.Create(doc.RootElement, 'columns');
    try
      FSettings.LoadFromXmlElement(cursor.Elements[0]);
    finally
      cursor.Free;
    end;
  finally
    doc.Free;
  end;
  ApplySettingsToGrid(FSettings, FDBGrid);
end;

procedure TmDBGridHelper.SaveSettings(aStream: TStream);
var
  doc : TmXmlDocument;
  root : TmXmlElement;
begin
  ReadSettingsFromGrid(FSettings, FDBGrid);
  doc := TmXmlDocument.Create;
  try
    root := doc.CreateRootElement('configuration');
    root.SetAttribute('version', '1');
    FSettings.SaveToXmlElement(root.AddElement('columns'));
    doc.SaveToStream(aStream);
  finally
    doc.Free;
  end;
end;

procedure TmDBGridHelper.EnableSort(aSortManager : ISortableDatasetManager);
begin
  if Assigned(FDBGrid) then
  begin
    FGridIcons:= TmGridIconsDataModule.Create(FDBGrid);
    FDBGrid.TitleImageList := FGridIcons.GridImageList;
    FOriginalOnTitleClick := FDBGrid.OnTitleClick;
    FDBGrid.OnTitleClick:= Self.OnTitleClick;
    FSortManager:= aSortManager;
  end;
end;

procedure TmDBGridHelper.EnableHeaderPopupMenu(aGridPopupMenu : TPopupMenu);
begin
  if not Assigned(FHeaderPopupMenu) then
    Self.BuildHeaderPopupMenu;
  FGridPopupMenu := aGridPopupMenu;
  FOriginalOnMouseDown := FDBGrid.OnMouseDown;
  FDBGrid.OnMouseDown:= Self.OnMouseDown;
end;

end.
