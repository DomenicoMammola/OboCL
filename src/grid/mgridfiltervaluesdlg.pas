// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGridFilterValuesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  CheckLst, ExtCtrls, ComCtrls, StdCtrls, Menus, ListFilterEdit;

resourcestring
  SExportValuesMenuCaption = 'Export values...';
  SWarningFileExists = 'File exists';
  SAskOverwriteFile = 'File already exists. Overwrite it?';

type

  { TFilterValuesDlg }

  TFilterValuesDlg = class(TForm)
    BtnAddAll: TButton;
    BtnClear: TButton;
    ButtonPanel1: TButtonPanel;
    ListBoxToBeFiltered: TListBox;
    ListBoxFilter: TListBox;
    ListFilterEditAdvanced: TListFilterEdit;
    MI_ExportValues: TMenuItem;
    PageControlFilter: TPageControl;
    PanelTop: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PopupMenuValues: TPopupMenu;
    Splitter1: TSplitter;
    TSAdvanced: TTabSheet;
    TSSimple: TTabSheet;
    ValuesListBox: TCheckListBox;
    ListFilterEdit: TListFilterEdit;
    procedure BtnAddAllClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxFilterClick(Sender: TObject);
    procedure ListBoxToBeFilteredClick(Sender: TObject);
    procedure MI_ExportValuesClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure Init (const aList : TStringList);
    procedure GetCheckedValues (aList : TStringList);
  end;

implementation


{$R *.lfm}

{ TFilterValuesDlg }

procedure TFilterValuesDlg.BtnClearClick(Sender: TObject);
begin
  ListBoxFilter.Clear;
end;

procedure TFilterValuesDlg.FormCreate(Sender: TObject);
begin
  MI_ExportValues.Caption:= SExportValuesMenuCaption;
end;

procedure TFilterValuesDlg.FormShow(Sender: TObject);
begin
  ListFilterEdit.SetFocus;
end;

procedure TFilterValuesDlg.ListBoxFilterClick(Sender: TObject);
begin
  if ListBoxFilter.ItemIndex >= 0 then
    ListBoxFilter.Items.Delete(ListBoxFilter.ItemIndex);
end;

procedure TFilterValuesDlg.ListBoxToBeFilteredClick(Sender: TObject);
var
  tmp : string;
begin
  if ListBoxToBeFiltered.ItemIndex >= 0 then
  begin
    tmp := ListBoxToBeFiltered.Items[ListBoxToBeFiltered.ItemIndex];
    if ListBoxFilter.Items.IndexOf(tmp) < 0 then
      ListBoxFilter.Items.Append(tmp);
  end;
end;

procedure TFilterValuesDlg.MI_ExportValuesClick(Sender: TObject);
var
  dlg : TSaveDialog;
  tmpList : TStringList;
  i : integer;
begin
  dlg := TSaveDialog.Create(Self);
  try
    dlg.DefaultExt:= 'txt';
    dlg.Filter:='Text file|*.txt';

    if dlg.Execute then
    begin
      if (not FileExists(dlg.FileName)) or (MessageDlg(SWarningFileExists, SAskOverwriteFile, mtConfirmation, mbYesNo, 0) = mrYes) then
      begin
        tmpList := TStringList.Create;
        try
          if PageControlFilter.ActivePageIndex = 0 then
          begin
            for i := 0 to ValuesListBox.Count - 1 do
              tmpList.Append(ValuesListBox.Items[i]);
          end
          else
            tmpList.AddStrings(ListBoxToBeFiltered.Items);

          tmpList.SaveToFile(dlg.FileName);
        finally
          tmpList.Free;
        end;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TFilterValuesDlg.BtnAddAllClick(Sender: TObject);
var
  i : integer;
  OldCursor : TCursor;
begin

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor:= crHourGlass;
    ListBoxFilter.Items.BeginUpdate;
    try
      for i := 0 to ListBoxToBeFiltered.Count - 1 do
      begin
        if ListBoxFilter.Items.IndexOf(ListBoxToBeFiltered.Items[i]) < 0 then
          ListBoxFilter.Items.Add(ListBoxToBeFiltered.Items[i]);
      end;
    finally
      ListBoxFilter.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TFilterValuesDlg.Init(const aList: TStringList);
var
  OldCursor : TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor:= crHourGlass;
    ListFilterEdit.FilteredListbox := ValuesListBox;
    ValuesListBox.Items.BeginUpdate;
    try
      ListFilterEdit.Items.AddStrings(aList);
    finally
      ValuesListBox.Items.EndUpdate;
    end;
    ListFilterEdit.ForceFilter('#');
    ListFilterEdit.ResetFilter;

    ListFilterEditAdvanced.FilteredListbox := ListBoxToBeFiltered;
    ListFilterEditAdvanced.Items.AddStrings(aList);
    ListFilterEditAdvanced.ForceFilter('#');
    ListFilterEditAdvanced.ResetFilter;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TFilterValuesDlg.GetCheckedValues(aList: TStringList);
var
  i : integer;
begin
  if PageControlFilter.ActivePageIndex = 0 then
  begin
    for i := 0 to ValuesListBox.Count - 1 do
    begin
      if ValuesListBox.Checked[i] then
        aList.Append(ValuesListBox.Items[i]);
    end;
  end
  else
    aList.AddStrings(ListBoxFilter.Items);
end;

end.

