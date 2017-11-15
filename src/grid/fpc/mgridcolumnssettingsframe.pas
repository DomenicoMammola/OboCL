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

  mGridColumnSettings;

resourcestring
  SNameFieldLabel = 'Name';
  SVisibleFieldLabel = 'Visible';
  SLabelFieldLabel = 'Label';
  SFormatFieldLabel = 'Format';

type

  { TGridColumnsSettingsFrame }

  TGridColumnsSettingsFrame = class(TFrame)
    DataSourceColSettings: TDataSource;
    ColSettingsDataset: TMemDataset;
  private
    const FLD_FIELDNAME = 'CSDFFieldName';
    const FLD_VISIBLE = 'CSDFVisible';
    const FLD_LABEL = 'CSDFDisplayLabel';
    const FLD_FORMAT = 'CSDFDisplayFormat';
  private
    FSettings : TmGridColumnsSettings;
    FColSettingsGrid: TDBGrid;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aSettings : TmGridColumnsSettings);
    procedure UpdateSettings;
  end;

implementation

uses
  mDBGrid;

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
    Title.Caption := SNameFieldLabel;
    Width := 250;
    FieldName := FLD_FIELDNAME;
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := SVisibleFieldLabel;
    FieldName := FLD_VISIBLE;
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := SLabelFieldLabel;
    Width := 250;
    FieldName := FLD_LABEL;
  end;
  with FColSettingsGrid.Columns.Add do
  begin
    Title.Caption := SFormatFieldLabel;
    FieldName := FLD_FORMAT;
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
  tmp : string;
begin
  FSettings := aSettings;
  ColSettingsDataset.DisableControls;
  try
    for i := 0 to aSettings.Count -1 do
    begin
      op := aSettings.Get(i);
      ColSettingsDataset.Append;
      tmp := op.FieldName;
      ColSettingsDataset.FieldByName(FLD_FIELDNAME).AsString := tmp;
      if op.DisplayLabel.NotNull then
        ColSettingsDataset.FieldByName(FLD_LABEL).AsString := op.DisplayLabel.Value;
      if op.DisplayFormat.NotNull then
        ColSettingsDataset.FieldByName(FLD_FORMAT).AsString:= op.DisplayFormat.Value;
      if op.Visible.NotNull then
        ColSettingsDataset.FieldByName(FLD_VISIBLE).AsBoolean:= op.Visible.Value;
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
        op := FSettings.GetSettingsForField(ColSettingsDataset.FieldByName(FLD_FIELDNAME).AsString);

        if (ColSettingsDataset.FieldByName(FLD_LABEL).IsNull) then
          op.DisplayLabel.IsNull:= true
        else
          op.DisplayLabel.Value:= ColSettingsDataset.FieldByName(FLD_LABEL).AsString;

        if (ColSettingsDataset.FieldByName(FLD_FORMAT).IsNull) then
          op.DisplayFormat.IsNull:= true
        else
          op.DisplayFormat.Value := ColSettingsDataset.FieldByName(FLD_FORMAT).AsString;

        if (ColSettingsDataset.FieldByName(FLD_VISIBLE).IsNull) then
          op.Visible.IsNull:= true
        else
        begin
          if (op.Visible.IsNull or (not op.Visible.Value)) and ColSettingsDataset.FieldByName(FLD_VISIBLE).AsBoolean then
            op.Width.Value := 100;
          op.Visible.Value:= ColSettingsDataset.FieldByName(FLD_VISIBLE).AsBoolean;
        end;


        ColSettingsDataset.Next;
      end;
    finally
      ColSettingsDataset.EnableControls;
    end;
  end;
end;

end.

