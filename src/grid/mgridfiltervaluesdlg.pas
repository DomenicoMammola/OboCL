unit mGridFilterValuesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComboEx, CheckLst, ListFilterEdit;

type

  { TFilterValuesDlg }

  TFilterValuesDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    ValuesListBox: TCheckListBox;
    ListFilterEdit: TListFilterEdit;
  private
    { private declarations }
  public
    procedure Init (const aList : TStringList);
    procedure GetCheckedValues (aList : TStringList);
  end;

implementation

{$R *.lfm}

{ TFilterValuesDlg }

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
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TFilterValuesDlg.GetCheckedValues(aList: TStringList);
var
  i : integer;
begin
  for i := 0 to ValuesListBox.Count - 1 do
  begin
    if ValuesListBox.Checked[i] then
      aList.Append(ValuesListBox.Items[i]);
  end;
end;

end.

