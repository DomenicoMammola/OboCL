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
  DBGrids, Controls,
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
    procedure OnTitleClick (Column: TColumn);
  public
    constructor Create;
    destructor Destroy; override;
    function EditSettings : boolean;
    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);
    procedure EnableSort (aSortManager : ISortableDatasetManager);

    property Grid : TDBGrid read FDBGrid write FDBGrid;
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
end;

constructor TmDBGridHelper.Create;
begin
  FSettings := TmGridColumnsSettings.Create;
  FSortManager := nil;
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
    FDBGrid.OnTitleClick:= Self.OnTitleClick;
    FSortManager:= aSortManager;
  end;
end;

end.
