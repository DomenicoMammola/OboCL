// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDBGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mGridHelper, mDBGrid, mCellDecorations, mVirtualDatasetFormulas;

type
  { TmDBGridHelper }

  TmDBGridHelper = class(TmAbstractGridHelper)
  protected
    FDBGrid : TmDBGrid;
  public
    constructor Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations); virtual;
    destructor Destroy; override;

    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;
    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;

    property DBGrid : TmDBGrid read FDBGrid;
  end;

implementation

uses
  Controls, DBGrids,
  mMaps;

{ TmDBGridHelper }

constructor TmDBGridHelper.Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations);
begin
  InternalSetup(aGrid, aFormulaFields, aCellDecorations);
  FDBGrid := aGrid;
end;

destructor TmDBGridHelper.Destroy;
begin
  FDBGrid := nil;
  inherited Destroy;
end;

procedure TmDBGridHelper.SetupGrid (const aEnableAutoSizedColumns : boolean = true);
begin
  FDBGrid.Align:= alClient;
  FDBGrid.AlternateColor:= DefaultGridAlternateColor;
  FDBGrid.Flat := True;
  if aEnableAutoSizedColumns then
    FDBGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect]
  else
    FDBGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit,                    dgDisableDelete, dgDisableInsert, dgMultiselect];
end;

procedure TmDBGridHelper.SelectAllRows;
var
  curRecNo : longint;
begin
  curRecNo := -1;
  if dgMultiselect in FDBGrid.Options then
  begin
    FDBGrid.BeginUpdate;
    try
      FDBGrid.ClearSelections;
      curRecNo := FDBGrid.DataSource.DataSet.RecNo;
      FDBGrid.DataSource.DataSet.DisableControls;
      try
        FDBGrid.DataSource.DataSet.First;
        while not FDBGrid.DataSource.DataSet.EOF do
        begin
          FDBGrid.SelectedRows.CurrentRowSelected:= true;
          FDBGrid.DataSource.DataSet.Next;
        end;
        if curRecNo >= 0 then
          FDBGrid.DataSource.DataSet.RecNo:= curRecNo;
      finally
        FDBGrid.DataSource.DataSet.EnableControls;
      end;
    finally
      FDBGrid.EndUpdate(true);
    end;
  end;
end;

procedure TmDBGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
var
  i : integer;
  index : TmStringDictionary;
  firstSelectedRecNo, curRecNo : longint;
begin
  if dgMultiselect in FDBGrid.Options then
  begin
    curRecNo := -1;
    firstSelectedRecNo:= -1;
    index := TmStringDictionary.Create(false);
    try
      for i := 0 to aValues.Count - 1 do
      begin
        if not index.Contains(aValues.Strings[i]) then
          index.Add(aValues.Strings[i], index);
      end;
      FDBGrid.BeginUpdate;
      try
        FDBGrid.ClearSelections;
        curRecNo := FDBGrid.DataSource.DataSet.RecNo;
        FDBGrid.DataSource.DataSet.DisableControls;
        try
          FDBGrid.DataSource.DataSet.First;
          while not FDBGrid.DataSource.DataSet.EOF do
          begin
            if index.Contains(FDBGrid.DataSource.DataSet.FieldByName(aKeyField).AsString) then
            begin
              FDBGrid.SelectedRows.CurrentRowSelected:= true;
              if firstSelectedRecNo = -1 then
                FDBGrid.DataSource.DataSet.RecNo;
            end
            else
              FDBGrid.SelectedRows.CurrentRowSelected:= false;
            FDBGrid.DataSource.DataSet.Next;
          end;
          if firstSelectedRecNo >= 0 then
            FDBGrid.DataSource.DataSet.RecNo:= firstSelectedRecNo
          else if curRecNo >= 0 then
            FDBGrid.DataSource.DataSet.RecNo:= curRecNo;
        finally
          FDBGrid.DataSource.DataSet.EnableControls;
        end;
      finally
        FDBGrid.EndUpdate(true);
      end;
    finally
      index.Free;
    end;
  end;
end;

end.
