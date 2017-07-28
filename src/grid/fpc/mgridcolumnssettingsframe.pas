// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridColumnsSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, memds, FileUtil, Forms, Controls, DBGrids,

  mGridColumnSettings, mDBGrid;

type

  { TGridColumnsSettingsFrame }

  TGridColumnsSettingsFrame = class(TFrame)
    DataSourceColSettings: TDataSource;
    ColSettingsDataset: TMemDataset;
  private
    FSettings : TmGridColumnsSettings;
    FColSettingsGrid: TmDBGrid;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aSettings : TmGridColumnsSettings);
    procedure UpdateSettings;
  end;

implementation

{$R *.lfm}

{ TGridColumnsSettingsFrame }

constructor TGridColumnsSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FColSettingsGrid:= TmDBGrid.Create(Self);
  FColSettingsGrid.Parent := Self;
  FColSettingsGrid.Align:= alClient;

  FColSettingsGrid.Options:= [dgEditing,dgTitles,dgIndicator,dgColumnResize,dgColumnMove,dgColLines,dgRowLines,dgTabs,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit];
  FColSettingsGrid.OptionsExtra:=[dgeAutoColumns,dgeCheckboxColumn];
//  FColSettingsGrid.Color:= clWindow;
  with FColSettingsGrid.Columns.Add do
  begin
    ReadOnly := True;
    Title.Caption := 'Name';
    Width := 250;
    FieldName := 'CSDFFieldName';
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := 'Visible';
    FieldName := 'CSDFVisible';
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := 'Label';
    Width := 250;
    FieldName := 'CSDFDisplayLabel';
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := 'Format';
    FieldName := 'CSDFDisplayFormat';
  end;
  FColSettingsGrid.DataSource := DataSourceColSettings;
end;

destructor TGridColumnsSettingsFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TGridColumnsSettingsFrame.Init(aSettings: TmGridColumnsSettings);
var
  i : integer;
  op : TmGridColumnSettings;
begin
  FSettings := aSettings;
  ColSettingsDataset.DisableControls;
  try
    for i := 0 to aSettings.Count -1 do
    begin
      op := aSettings.Get(i);
      ColSettingsDataset.Append;
      ColSettingsDataset.FieldByName('CSDFFieldName').AsString := op.FieldName;
      if op.DisplayLabel.NotNull then
        ColSettingsDataset.FieldByName('CSDFDisplayLabel').AsString := op.DisplayLabel.Value;
      if op.DisplayFormat.NotNull then
        ColSettingsDataset.FieldByName('CSDFDisplayFormat').AsString:= op.DisplayFormat.Value;
      if op.Visible.NotNull then
        ColSettingsDataset.FieldByName('CSDFVisible').AsBoolean:= op.Visible.Value;
      ColSettingsDataset.Post;
    end;
  finally
    ColSettingsDataset.EnableControls;
  end;
  ColSettingsDataset.First;
end;

procedure TGridColumnsSettingsFrame.UpdateSettings;
var
  op : TmGridColumnSettings;
begin
  if Assigned(FSettings) then
  begin
    ColSettingsDataset.DisableControls;
    try
      ColSettingsDataset.First;
      while not ColSettingsDataset.EOF do
      begin
        op := FSettings.GetSettingsForField(ColSettingsDataset.FieldByName('CSDFFieldName').AsString);
        if (ColSettingsDataset.FieldByName('CSDFDisplayLabel').IsNull) then
          op.DisplayLabel.IsNull:= true
        else
          op.DisplayLabel.Value:= ColSettingsDataset.FieldByName('CSDFDisplayLabel').AsString;

        if (ColSettingsDataset.FieldByName('CSDFDisplayFormat').IsNull) then
          op.DisplayFormat.IsNull:= true
        else
          op.DisplayFormat.Value := ColSettingsDataset.FieldByName('CSDFDisplayFormat').AsString;

        if (ColSettingsDataset.FieldByName('CSDFVisible').IsNull) then
          op.Visible.IsNull:= true
        else
          op.Visible.Value:= ColSettingsDataset.FieldByName('CSDFVisible').AsBoolean;

        ColSettingsDataset.Next;
      end;
    finally
      ColSettingsDataset.EnableControls;
    end;
  end;
end;

end.

