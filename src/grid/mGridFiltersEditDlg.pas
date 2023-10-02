// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridFiltersEditDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  DB, StdCtrls, ExtCtrls, contnrs,
  mFilter, mFields;

type

  { TFiltersEditDlg }

  TFiltersEditDlg = class(TForm)
    LBFilterConditions: TListBox;
    RemoveButton: TButton;
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBFilterConditionsClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
  private
    FGarbage: TObjectList;
    FRemovedFields: TStringList;
    procedure InternalInit (const aFilters: TmFilters; const aDisplayNames : TStringList);
  public
    procedure Init (const aFilters: TmFilters; const aDataset: TDataset); overload;
    procedure Init (const aFilters: TmFilters; const aFields: TmFields); overload;
    procedure GetRemovedFilterConditions (aFieldNames : TStringList);
  end;

implementation

uses
  mBaseClassesAsObjects,
  mWaitCursor;

{$R *.lfm}

{ TFiltersEditDlg }

procedure TFiltersEditDlg.FormCreate(Sender: TObject);
begin
  FGarbage:= TObjectList.Create(true);
  FRemovedFields:= TStringList.Create;
end;

procedure TFiltersEditDlg.FormDestroy(Sender: TObject);
begin
  FRemovedFields.Free;
  FGarbage.Free;
end;

procedure TFiltersEditDlg.LBFilterConditionsClick(Sender: TObject);
begin
  if (LBFilterConditions.Count > 0) and (LBFilterConditions.ItemIndex >= 0) then
    RemoveButton.Enabled:= true
  else
    RemoveButton.Enabled:= false;
end;

procedure TFiltersEditDlg.RemoveButtonClick(Sender: TObject);
begin
  if (LBFilterConditions.Count > 0) and (LBFilterConditions.ItemIndex >= 0) then
  begin
    FRemovedFields.Add((LBFilterConditions.Items.Objects[LBFilterConditions.ItemIndex] as TStringObject).Value);
    LBFilterConditions.Items.Delete(LBFilterConditions.ItemIndex);
    LBFilterConditionsClick(Sender);
  end;
end;

procedure TFiltersEditDlg.InternalInit(const aFilters: TmFilters; const aDisplayNames: TStringList);
var
  i : integer;
  tmp: String;
  tmpShell: TStringObject;
begin
  try
    TWaitCursor.ShowWaitCursor('TFiltersEditDlg.InternalInit');
    for i := 0 to aFilters.Count - 1 do
    begin
      tmp := aDisplayNames[i] + ' [' + aFilters.Get(i).FieldName + ']';
      tmpShell:= TStringObject.Create(aFilters.Get(i).FieldName);
      FGarbage.Add(tmpShell);
      LBFilterConditions.AddItem(tmp, tmpShell);
    end;
  finally
    TWaitCursor.UndoWaitCursor('TFiltersEditDlg.InternalInit');
  end;
end;

procedure TFiltersEditDlg.Init(const aFilters: TmFilters; const aDataset: TDataset);
var
  displayNames : TStringList;
  i : integer;
begin
  displayNames := TStringList.Create;
  try
    for i := 0 to aFilters.Count - 1 do
      displayNames.Add(aDataset.FieldByName(aFilters.Get(i).FieldName).DisplayLabel);
    InternalInit(aFilters, displayNames);
  finally
    displayNames.Free;
  end;
end;

procedure TFiltersEditDlg.Init(const aFilters: TmFilters; const aFields: TmFields);
var
  displayNames : TStringList;
  i : integer;
begin
  displayNames := TStringList.Create;
  try
    for i := 0 to aFilters.Count - 1 do
      displayNames.Add(aFields.FieldByName(aFilters.Get(i).FieldName).DisplayLabel);
    InternalInit(aFilters, displayNames);
  finally
    displayNames.Free;
  end;
end;

procedure TFiltersEditDlg.GetRemovedFilterConditions (aFieldNames : TStringList);
begin
  aFieldNames.Clear;
  aFieldNames.AddStrings(FRemovedFields);
end;

end.

