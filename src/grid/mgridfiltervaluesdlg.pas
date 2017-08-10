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
    procedure Init (aList : TStringList);
  end;

implementation

{$R *.lfm}

{ TFilterValuesDlg }

procedure TFilterValuesDlg.Init(aList: TStringList);
begin
  ListFilterEdit.Items.AddStrings(aList);
end;

end.

