unit mGridFilterValuesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComboEx, CheckLst, ExtCtrls, ComCtrls, StdCtrls, ListFilterEdit;

type

  { TFilterValuesDlg }

  TFilterValuesDlg = class(TForm)
    BtnAddAll: TButton;
    BtnClear: TButton;
    ButtonPanel1: TButtonPanel;
    ListBoxToBeFiltered: TListBox;
    ListBoxFilter: TListBox;
    ListFilterEditAdvanced: TListFilterEdit;
    PageControlFilter: TPageControl;
    PanelTop: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    Splitter1: TSplitter;
    TSAdvanced: TTabSheet;
    TSSimple: TTabSheet;
    ValuesListBox: TCheckListBox;
    ListFilterEdit: TListFilterEdit;
    procedure BtnAddAllClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure ListBoxFilterClick(Sender: TObject);
    procedure ListBoxToBeFilteredClick(Sender: TObject);
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

procedure TFilterValuesDlg.BtnAddAllClick(Sender: TObject);
var
  i : integer;
begin
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

    //ListFilterEditAdvanced.FilteredListbox := ListBoxToBeFiltered;
    ListBoxToBeFiltered.Items.BeginUpdate;
    try
      ListBoxToBeFiltered.Items.AddStrings(aList);
    finally
      ListBoxToBeFiltered.Items.EndUpdate;
    end;
    //ListFilterEditAdvanced.ForceFilter('#');
    //ListFilterEditAdvanced.ResetFilter;
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

