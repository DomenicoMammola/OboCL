// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridEditors;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Grids,
  LCLType;

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
  FEditor := TmExtStringCellEditor.Create(Self);
  FEditor.Visible := false;
  FEditor.Event:= Self.myEvent;
  FEditor.ParentGrid := ..;
  [..]

  *)

  { TmExtStringCellEditor }

  TmExtStringCellEditor = class (TStringCellEditor)
  strict private
    FParentGrid : TCustomStringGrid;
    FOnKeyPressEvent : TOnSelectEvent;
    FDefaultEditKey : Word;
    FDefaultClearKey : Word;
    FAllowDeleteWhenReadOnly : boolean;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
  public
    constructor Create(Aowner : TComponent); override;

    property ParentGrid : TCustomStringGrid read FParentGrid write FParentGrid;
    property OnKeyPressEvent : TOnSelectEvent read FOnKeyPressEvent write FOnKeyPressEvent;
    property DefaultEditKey : Word read FDefaultEditKey write FDefaultEditKey;
    property DefaultClearKey : Word read FDefaultClearKey write FDefaultClearKey;
    property AllowDeleteWhenReadOnly : boolean read FAllowDeleteWhenReadOnly write FAllowDeleteWhenReadOnly;
  end;

implementation

{ TmExtStringCellEditor }

procedure TmExtStringCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = FDefaultEditKey then
  begin
    if Assigned(FOnKeyPressEvent) then
      FOnKeyPressEvent(Self, FParentGrid.Col, FParentGrid.Row);
    Self.Text := FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row];
  end
  else
  if FAllowDeleteWhenReadOnly and (Self.ReadOnly) and (Key = FDefaultClearKey) then
  begin
    FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row] := '';
    Self.Text := FParentGrid.Cells[FParentGrid.Col, FParentGrid.Row];
  end;
end;

constructor TmExtStringCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FDefaultEditKey:= VK_RETURN;
  FDefaultClearKey:= VK_DELETE;
  FAllowDeleteWhenReadOnly:= true;
end;

end.
