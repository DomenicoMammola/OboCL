// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLookupPanelInstantQuery;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, DB,
  StdCtrls,
  mDataProviderInterfaces, mQuickReadOnlyVirtualDataSetProvider,
  mVirtualDataSet, mLookupWindowEvents,
  DBGrids;

resourcestring
  rsSearchButtonCaption = 'Search';

type

  { TmLookupPanelInstantQuery }

  TmLookupPanelInstantQuery = class (TCustomPanel)
  strict private
    FGrid : TDBGrid;
    FDatasource : TDatasource;
    FTopPanel : TPanel;
    FBottomPanel : TPanel;
    FSearchBtn : TButton;
    FEditText: TEdit;
    FDisplayFieldNames : TStringList;
    FKeyFieldName : String;
    FOnSelectAValue : TOnSelectAValueDatum;
    FClicked : boolean;

    FDatasetProvider: TReadOnlyVirtualDatasetProvider;
    FVirtualDataset : TmVirtualDataset;
    FInstantQueryManager : IVDInstantQueryManager;
    FInstantQueryManagerAdditionalOptions: TList;

    procedure OnClickSearch(aSender : TObject);
    procedure OnDoubleClickGrid(Sender: TObject);
    procedure OnKeyDownGrid(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnKeyDownEdit(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnChangeOption(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const aInstantQueryManager : IVDInstantQueryManager); overload;
    procedure Init(const aInstantQueryManager : IVDInstantQueryManager; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList); overload;
    procedure SetFocusOnFilter;
    procedure GetSelectedValues (out aKeyValue: variant; out aDisplayLabel: string; out aDatum : IVDDatum);

    property OnSelectAValue : TOnSelectAValueDatum read FOnSelectAValue write FOnSelectAValue;
  end;



implementation

uses
  Variants, Forms, LCLType,
  mMagnificationFactor, mWaitCursor;

{ TmLookupPanelInstantQuery }

procedure TmLookupPanelInstantQuery.OnClickSearch(aSender: TObject);
begin
  if Assigned(FInstantQueryManager) then
  begin
    TWaitCursor.ShowWaitCursor('TmLookupPanelInstantQuery.OnClickSearch');
    try
      FClicked := true;
      FInstantQueryManager.Clear;
      FInstantQueryManager.FilterDataProvider(FEditText.Text);
      FVirtualDataset.Refresh;
      FGrid.AutoAdjustColumns;
    finally
      TWaitCursor.UndoWaitCursor('TmLookupPanelInstantQuery.OnClickSearch');
    end;
  end;
end;

procedure TmLookupPanelInstantQuery.OnDoubleClickGrid(Sender: TObject);
var
  tmpDisplayLabel: string;
  tmpKeyValue: variant;
  tmpDatum: IVDDatum;
begin
  if (FGrid.SelectedRows.Count = 1) and (Assigned(FOnSelectAValue)) then
  begin
    tmpKeyValue := Null;
    tmpDisplayLabel:= '';
    Self.GetSelectedValues(tmpKeyValue, tmpDisplayLabel, tmpDatum);
    FOnSelectAValue(tmpKeyValue, tmpDisplayLabel, tmpDatum);
  end;
end;

procedure TmLookupPanelInstantQuery.OnKeyDownGrid(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tmpDisplayLabel: string;
  tmpKeyValue: variant;
  tmpDatum: IVDDatum;
begin
  if (Key = VK_RETURN) and (FGrid.SelectedRows.Count = 1) and (Assigned(FOnSelectAValue)) then
  begin
    tmpKeyValue := Null;
    tmpDisplayLabel:= '';
    Self.GetSelectedValues(tmpKeyValue, tmpDisplayLabel, tmpDatum);
    FOnSelectAValue(tmpKeyValue, tmpDisplayLabel, tmpDatum);
  end;
end;

procedure TmLookupPanelInstantQuery.OnKeyDownEdit(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (FGrid.DataSource.DataSet.RecordCount > 0) then
  begin
    FGrid.SetFocus;
    FGrid.SelectedRows.CurrentRowSelected:= true;
  end;
end;

procedure TmLookupPanelInstantQuery.OnChangeOption(Sender: TObject);
begin
  if Sender is TCheckBox then
  begin
    TInstantQueryManagerAdditionalOption(FInstantQueryManagerAdditionalOptions.Items[(Sender as TCheckBox).Tag]).Callback((Sender as TCheckBox).Checked);
    if FClicked then
    begin
      FInstantQueryManager.Clear;
      FVirtualDataset.Refresh;
      FGrid.AutoAdjustColumns;
    end;
  end;
end;

constructor TmLookupPanelInstantQuery.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FDatasource := TDataSource.Create(Self);
  FVirtualDataset := TmVirtualDataset.Create(Self);
  FInstantQueryManager := nil;
  FDisplayFieldNames := TStringList.Create;
  FKeyFieldName:= '';
  FClicked := false;

  FDatasetProvider:= TReadOnlyVirtualDatasetProvider.Create;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align:= alTop;
  FTopPanel.BevelInner:= bvNone;
  FTopPanel.BevelOuter:= bvNone;
  FTopPanel.Caption := '';
  FTopPanel.Height:= ScaleForMagnification(20, true);

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := Self;
  FBottomPanel.Align := alBottom;
  FBottomPanel.BevelInner:= bvNone;
  FBottomPanel.BevelOuter:= bvNone;
  FBottomPanel.Caption:= '';
  FBottomPanel.Height:= ScaleForMagnification(20, true);
  FBottomPanel.Visible:= false;

  FGrid := TDBGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  FGrid.DataSource := FDatasource;
  FGrid.Flat := True;
  FGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect];
  FGrid.OnDblClick:=OnDoubleClickGrid;
  FGrid.OnKeyDown:=OnKeyDownGrid;
  FDatasource.DataSet := FVirtualDataset;
  FVirtualDataset.DatasetDataProvider := FDatasetProvider;

  FSearchBtn := TButton.Create(FTopPanel);
  FSearchBtn.Parent := FTopPanel;
  FSearchBtn.Caption:= rsSearchButtonCaption;
  FSearchBtn.Align:= alRight;
  FSearchBtn.OnClick:= Self.OnClickSearch;
  FSearchBtn.Default:= true;

  FEditText:= TEdit.Create(FTopPanel);
  FEditText.Parent := FTopPanel;
  FEditText.Align:= alClient;
  FEditText.OnKeyDown:= OnKeyDownEdit;
end;

destructor TmLookupPanelInstantQuery.Destroy;
begin
  FVirtualDataset.Active:= false;
  FDatasetProvider.Free;
  FDisplayFieldNames.Free;
  inherited Destroy;
end;

procedure TmLookupPanelInstantQuery.Init(const aInstantQueryManager: IVDInstantQueryManager);
begin
  Self.Init(aInstantQueryManager, nil, '', nil);
end;

procedure TmLookupPanelInstantQuery.Init(const aInstantQueryManager: IVDInstantQueryManager; const aFieldNames: TStringList; const aKeyFieldName: string; const aDisplayFieldNames: TStringList);
var
  fields : TStringList;
  i, q : integer;
  tmpCheckBox: TCheckBox;
begin
  fields := TStringList.Create;
  try
    FInstantQueryManager := aInstantQueryManager;
    FInstantQueryManager.Clear;

    FInstantQueryManagerAdditionalOptions := FInstantQueryManager.GetAdditionalOptions;
    if Assigned(FInstantQueryManagerAdditionalOptions) and (FInstantQueryManagerAdditionalOptions.Count > 0) then
    begin
      FBottomPanel.Visible:= true;
      for i := FInstantQueryManagerAdditionalOptions.Count - 1 downto 0 do
      begin
        tmpCheckBox := TCheckBox.Create(FBottomPanel);
        tmpCheckBox.Parent := FBottomPanel;
        tmpCheckBox.Caption := TInstantQueryManagerAdditionalOption(FInstantQueryManagerAdditionalOptions.Items[i]).Caption + '  ';
        tmpCheckBox.Checked := TInstantQueryManagerAdditionalOption(FInstantQueryManagerAdditionalOptions.Items[i]).DefaultValue;
        tmpCheckBox.Tag:= i;
        tmpCheckBox.OnChange := OnChangeOption;
        tmpCheckBox.Align:= alLeft;
      end;
    end;

    FGrid.DataSource.DataSet.DisableControls;
    try
      FDatasetProvider.Init(FInstantQueryManager.GetDataProvider);
      FInstantQueryManager.GetDataProvider.FillVirtualFieldDefs(FDatasetProvider.VirtualFieldDefs, '');
      FVirtualDataset.Active:= true;
      FVirtualDataset.Refresh;
      if aFieldNames <> nil then
        fields.AddStrings(aFieldNames)
      else
        FInstantQueryManager.GetDataProvider.GetMinimumFields(fields);
      for i := 0 to FVirtualDataset.Fields.Count - 1 do
      begin
        if fields.IndexOf(FVirtualDataset.Fields[i].FieldName) < 0 then
          FVirtualDataset.Fields[i].Visible:= false;
      end;
      FDisplayFieldNames.Clear;
      if aDisplayFieldNames <> nil then
        FDisplayFieldNames.AddStrings(aDisplayFieldNames)
      else
        FInstantQueryManager.GetDataProvider.GetMinimumFields(FDisplayFieldNames);

      FKeyFieldName:= aKeyFieldName;
      if FKeyFieldName = '' then
        FKeyFieldName:= aInstantQueryManager.GetDataProvider.GetKeyFieldName;
    finally
      FGrid.DataSource.DataSet.EnableControls;
    end;

    FGrid.BeginUpdate;
    try
      for i := 0 to fields.Count - 1 do
      begin
        for q := 0 to FGrid.Columns.Count -1 do
        begin
          if FGrid.Columns.Items[q].FieldName = fields.Strings[i] then
          begin
            FGrid.Columns.Items[q].Index:= i;
            break;
          end;
        end;
      end;
    finally
      FGrid.EndUpdate(true);
    end;
  finally
    fields.Free;
  end;

end;

procedure TmLookupPanelInstantQuery.SetFocusOnFilter;
begin
  FEditText.SetFocus;
end;

procedure TmLookupPanelInstantQuery.GetSelectedValues(out aKeyValue: variant; out aDisplayLabel: string; out aDatum : IVDDatum);
var
  value : Variant;
  tmpDatum : IVDDatum;
begin
  aKeyValue := null;
  aDisplayLabel:= '';
  if FGrid.SelectedRows.Count = 1 then
  begin
    aKeyValue := FVirtualDataset.FieldByName(FKeyFieldName).Value;
    value := FVirtualDataset.FieldByName(FInstantQueryManager.GetDataProvider.GetKeyFieldName).Value;
    tmpDatum := FInstantQueryManager.GetDataProvider.FindDatumByStringKey(VarToStr(value));
    aDisplayLabel:= ConcatenateFieldValues(tmpDatum, FDisplayFieldNames);
    aDatum := tmpDatum.Clone;
  end;
end;

end.
