// This is part of the Obo Component Library

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// This software is distributed without any warranty.

// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridEditors;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Grids,
  LCLType
  //  ,ECEditBtns
  ;

type
  (*
  The user can digit free text in the editor but if a special key is pressed the editor will run the event.
  So, for instance, a lookup panel can be activated while keeping both the hands on the keyboard.
  The default special key is VK_RETURN.
  If the editor is read-only it is still possible to clear the value of the cell by pressing DELETE (key can be customized).
  The editor can be activated throw the OnSelectEditor event of the grid:

  [..]
  if .. then
  begin
    FEditor.Text := myGrid.Cells[myGrid.Col, myGrid.Row];
    Editor := FEditor;
  end;
  [..]

  before in the creation method of the parent component:
  [..]
  FEditor := TmExtDialogCellEditor.Create(Self);
  FEditor.Visible := false;
  FEditor.Event:= Self.myEvent;
  FEditor.ParentGrid := ..;
  [..]

  *)

  TmOnCellEditorShowDialogEvent = function(const aCol, aRow: integer; var aNewDisplayValue: string; var aNewActualValue: variant): boolean of object;
  TmOnCellEditorShowWizardEvent = function(const aCol, aRow: integer; var aNewDisplayValue: string; var aNewActualValue: variant): boolean of object;
  TmOnCellEditorClearEvent = function(const aCol, aRow: integer): boolean of object;

  { TmExtDialogCellEditor }

  TmExtDialogCellEditor = class(TStringCellEditor)
  strict private
    FParentGrid: TCustomStringGrid;
    FOnShowDialogEvent: TmOnCellEditorShowDialogEvent;
    FOnShowWizardEvent: TmOnCellEditorShowWizardEvent;
    FOnClearEvent: TmOnCellEditorClearEvent;
    FDefaultShowEditorKey: word;
    FDefaultShowWizardKey: word;
    FDefaultClearKey: word;
    FAllowDeleteWhenReadOnly: boolean;
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure DblClick; override;
  public
    constructor Create(Aowner: TComponent); override;

    property ParentGrid: TCustomStringGrid read FParentGrid write FParentGrid;

    property OnShowDialogEvent: TmOnCellEditorShowDialogEvent read FOnShowDialogEvent write FOnShowDialogEvent;
    property OnShowWizardEvent: TmOnCellEditorShowWizardEvent read FOnShowWizardEvent write FOnShowWizardEvent;
    property OnClearEvent: TmOnCellEditorClearEvent read FOnClearEvent write FOnClearEvent;

    property DefaultShowEditorKey: word read FDefaultShowEditorKey write FDefaultShowEditorKey;
    property DefaultClearKey: word read FDefaultClearKey write FDefaultClearKey;
    property DefaultShowWizardKey: word read FDefaultShowWizardKey write FDefaultShowWizardKey;
    property AllowDeleteWhenReadOnly: boolean read FAllowDeleteWhenReadOnly write FAllowDeleteWhenReadOnly;
  end;

  { TmExtButtonTextCellEditor }

  TmExtButtonTextCellEditor = class (TCompositeCellEditor)
  strict private
    FTextEditor : TmExtDialogCellEditor;
    FLookupButtonEditor : TButtonCellEditor;
    FParentGrid: TCustomStringGrid;
    FOnShowDialogEvent: TmOnCellEditorShowDialogEvent;
    FOnShowWizardEvent: TmOnCellEditorShowWizardEvent;
    FOnClearEvent: TmOnCellEditorClearEvent;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
    procedure SetOnShowDialogEvent(AValue: TmOnCellEditorShowDialogEvent);
    procedure SetOnShowWizardEvent(AValue: TmOnCellEditorShowWizardEvent);
    procedure SetParentGrid(AValue: TCustomStringGrid);
    procedure OnClickLookupButton (Sender: TObject);
  public
    constructor Create(Aowner: TComponent); override;

    property TextEditor : TmExtDialogCellEditor read FTextEditor;

    property ParentGrid: TCustomStringGrid read FParentGrid write SetParentGrid;
    property OnShowDialogEvent: TmOnCellEditorShowDialogEvent read FOnShowDialogEvent write SetOnShowDialogEvent;
    property OnShowWizardEvent: TmOnCellEditorShowWizardEvent read FOnShowWizardEvent write SetOnShowWizardEvent;
    property OnClearEvent: TmOnCellEditorClearEvent read FOnClearEvent write SetOnClearEvent;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;


implementation

uses
  Controls;

{ TmExtButtonTextCellEditor }

procedure TmExtButtonTextCellEditor.SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
begin
  FOnClearEvent:=AValue;
  FTextEditor.OnClearEvent:= AValue;
end;

function TmExtButtonTextCellEditor.GetReadOnly: Boolean;
begin
  Result := FTextEditor.ReadOnly;
end;

procedure TmExtButtonTextCellEditor.SetOnShowDialogEvent(AValue: TmOnCellEditorShowDialogEvent);
begin
  FOnShowDialogEvent:=AValue;
  FTextEditor.OnShowDialogEvent:= aValue;
end;

procedure TmExtButtonTextCellEditor.SetOnShowWizardEvent(AValue: TmOnCellEditorShowWizardEvent);
begin
  FOnShowWizardEvent:=AValue;
  FTextEditor.OnShowWizardEvent:=aValue;
end;

procedure TmExtButtonTextCellEditor.SetParentGrid(AValue: TCustomStringGrid);
begin
  if FParentGrid=AValue then Exit;
  FParentGrid:=AValue;
  FTextEditor.ParentGrid := aValue;
end;

procedure TmExtButtonTextCellEditor.OnClickLookupButton(Sender: TObject);
var
  newDisplayValue: string;
  newActualValue: variant;
begin
  if Assigned(FOnShowDialogEvent) then
  begin
    if FOnShowDialogEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
      FTextEditor.Text := newDisplayValue;
  end;
end;

procedure TmExtButtonTextCellEditor.SetReadOnly(Value: Boolean);
begin
  FTextEditor.ReadOnly:= Value;
end;

constructor TmExtButtonTextCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FOnShowDialogEvent := nil;
  FOnShowWizardEvent := nil;
  FOnClearEvent := nil;
  FTextEditor := TmExtDialogCellEditor.Create(AOwner);
  FLookupButtonEditor := TButtonCellEditor.Create(AOwner);
  FLookupButtonEditor.Caption := '...';
  FLookupButtonEditor.OnClick := Self.OnClickLookupButton;
  Self.AddEditor(FTextEditor, alClient, true);
  Self.AddEditor(FLookupButtonEditor, alRight, false);
end;

{ TmExtDialogCellEditor }

procedure TmExtDialogCellEditor.KeyDown(var Key: word; Shift: TShiftState);
var
  newDisplayValue: string;
  newActualValue: variant;
begin
  inherited KeyDown(Key, Shift);
  if Key = FDefaultShowEditorKey then
  begin
    if Assigned(FOnShowDialogEvent) then
    begin
      if FOnShowDialogEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
        Self.Text := newDisplayValue;
    end;
  end
  else
  if Key = FDefaultShowWizardKey then
  begin
    if Assigned(FOnShowWizardEvent) then
    begin
      if FOnShowWizardEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
        Self.Text := newDisplayValue;
    end;
  end
  else
  if FAllowDeleteWhenReadOnly and (Self.ReadOnly) and (Key = FDefaultClearKey) then
  begin
    if Assigned(FOnClearEvent) then
    begin
      if FOnClearEvent(FParentGrid.Col, FParentGrid.Row) then
      begin
        FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row] := '';
        Self.Text := '';
      end;
    end
    else
    begin
      FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row] := '';
      Self.Text := '';
    end;
  end;
end;

procedure TmExtDialogCellEditor.DblClick;
var
  newDisplayValue: string;
  newActualValue: variant;
begin
  inherited DblClick;
  if Assigned(FOnShowDialogEvent) then
  begin
    if FOnShowDialogEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue,
      newActualValue) then
      Self.Text := newDisplayValue;
    //FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row];
  end;
end;

constructor TmExtDialogCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FDefaultShowEditorKey := VK_RETURN;
  FDefaultShowWizardKey := VK_F1;
  FDefaultClearKey := VK_DELETE;
  FAllowDeleteWhenReadOnly := True;
  FOnShowDialogEvent := nil;
  FOnShowWizardEvent := nil;
  FOnClearEvent := nil;
end;

end.
