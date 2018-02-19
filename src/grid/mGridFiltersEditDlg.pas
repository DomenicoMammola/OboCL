unit mGridFiltersEditDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  DB, StdCtrls, ExtCtrls, contnrs,
  mFilter;

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
  public
    procedure Init (const aFilters: TmFilters; const aDataset: TDataset);
    procedure GetRemovedFilterConditions (aFieldNames : TStringList);
  end;

implementation

uses
  mBaseClassesAsObjects;

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

procedure TFiltersEditDlg.Init(const aFilters: TmFilters; const aDataset: TDataset);
var
  OldCursor : TCursor;
  i : integer;
  tmp: String;
  tmpShell: TStringObject;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor:= crHourGlass;
    for i := 0 to aFilters.Count - 1 do
    begin
      tmp := aDataset.FieldByName(aFilters.Get(i).FieldName).DisplayLabel + ' [' + aFilters.Get(i).FieldName + ']';
      tmpShell:= TStringObject.Create(aFilters.Get(i).FieldName);
      FGarbage.Add(tmpShell);
      LBFilterConditions.AddItem(tmp, tmpShell);
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TFiltersEditDlg.GetRemovedFilterConditions (aFieldNames : TStringList);
begin
  aFieldNames.Clear;
  aFieldNames.AddStrings(FRemovedFields);
end;

end.

