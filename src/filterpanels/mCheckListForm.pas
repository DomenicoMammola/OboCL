// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mCheckListForm;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, CheckLst, DB, contnrs,
  mBaseClassesAsObjects, mUtility;

type

  { TmCheckListWindow }

  TmCheckListWindow = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    CheckListBox: TCheckListBox;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  strict private
    FGarbage : TObjectList;
    function GetSelected: Variant;
    function CountCheckedItems : integer;
    function GetSelectedLabels: string;
  public
    procedure GetSelectedAsStrings (aList : TStringList);
    { public declarations }
    procedure AddValue (const aLabel: string; const aValue: variant);
    procedure SetCurrentValue (const aValue: variant);
    property Selected : Variant read GetSelected;
    property SelectedLabels: string read GetSelectedLabels;
  end;


implementation

uses
  variants;

{$R *.lfm}

{ TmCheckListWindow }

procedure TmCheckListWindow.FormShow(Sender: TObject);
begin
  CheckListBox.SetFocus;
end;

procedure TmCheckListWindow.FormCreate(Sender: TObject);
begin
  FGarbage := TObjectList.Create(true);
end;

procedure TmCheckListWindow.FormDestroy(Sender: TObject);
begin
  FGarbage.Free;
end;

procedure TmCheckListWindow.OkBtnClick(Sender: TObject);
begin
  if CountCheckedItems > 0 then
    ModalResult := mrOk
  else
    MessageDlg('Warning', 'No value selected.', mtInformation, [mbOk], 0);
end;

function TmCheckListWindow.GetSelected: Variant;
var
  i, k : integer;
  tmpVariant: variant;
begin
  Result := Null;
  k := CountCheckedItems;
  if k > 0 then
  begin
    tmpVariant := variants.VarArrayCreate([0, k-1], varvariant);
    k := 0;
    for i := 0 to CheckListBox.Count - 1 do
    begin
      if CheckListBox.Checked[i] then
      begin
        VarArrayPut(tmpVariant, (FGarbage.Items[i] as TVariantObject).Value, [k]);
        inc(k);
      end;
    end;
    Result := tmpVariant;
  end;
end;

function TmCheckListWindow.CountCheckedItems: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to CheckListBox.Count - 1 do
  begin
    if CheckListBox.Checked[i] then
      inc (Result);
  end;
end;

function TmCheckListWindow.GetSelectedLabels: string;
var
  i : integer;
  separator : string;
begin
  Result := '';
  separator := '';
  for i := 0 to CheckListBox.Count - 1 do
  begin
    if CheckListBox.Checked[i] then
    begin
      Result := Result + separator + CheckListBox.Items[i];
      separator := '+';
    end;
  end;
end;

procedure TmCheckListWindow.GetSelectedAsStrings(aList: TStringList);
var
  i : integer;
begin
  aList.Clear;
  if CountCheckedItems > 0 then
  begin
    for i := 0 to CheckListBox.Count - 1 do
    begin
      if CheckListBox.Checked[i] then
        aList.Add(VarToStr((FGarbage.Items[i] as TVariantObject).Value));
    end;
  end;
end;

procedure TmCheckListWindow.AddValue(const aLabel: string; const aValue: variant);
var
  tmp : TVariantObject;
begin
  tmp := TVariantObject.Create(aValue);
  FGarbage.Add(tmp);
  CheckListBox.AddItem(aLabel, tmp);
end;

procedure TmCheckListWindow.SetCurrentValue(const aValue: variant);
var
  tmpList : TStringList;
  i : integer;
begin
  if VarIsNull(aValue) then
    exit;

  tmpList := TStringList.Create;
  try
    mUtility.ConvertVariantToStringList(aValue, tmpList);
    for i := 0 to CheckListBox.Count - 1 do
    begin
      if tmpList.IndexOf(VarToStr((FGarbage.Items[i] as TVariantObject).Value)) >= 0 then
      begin
        CheckListBox.Checked[i] := true;
      end;
    end;
  finally
    tmpList.Free;
  end;
end;

end.

