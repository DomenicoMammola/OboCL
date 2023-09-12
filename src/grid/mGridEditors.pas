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
  Classes, Grids, contnrs, StdCtrls,
  LCLType,
  ATButtons,
  mMaps;

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

  TmOnCellEditorGetValueEvent = function(const aCol, aRow: integer; out aNewDisplayValue: string; out aNewActualValue: variant): boolean of object;
  TmOnCellEditorShowWizardEvent = function(const aCol, aRow: integer; out aNewDisplayValue: string; out aNewActualValue: variant): boolean of object;
  TmOnCellEditorClearEvent = function(const aCol, aRow: integer): boolean of object;

  { TmExtDialogCellEditor }

  TmExtDialogCellEditor = class(TStringCellEditor)
  strict private
    FParentGrid: TCustomStringGrid;
    FOnGetValueEvent: TmOnCellEditorGetValueEvent;
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

    property OnGetValueEvent: TmOnCellEditorGetValueEvent read FOnGetValueEvent write FOnGetValueEvent;
    property OnShowWizardEvent: TmOnCellEditorShowWizardEvent read FOnShowWizardEvent write FOnShowWizardEvent;
    property OnClearEvent: TmOnCellEditorClearEvent read FOnClearEvent write FOnClearEvent;

    property DefaultShowEditorKey: word read FDefaultShowEditorKey write FDefaultShowEditorKey;
    property DefaultClearKey: word read FDefaultClearKey write FDefaultClearKey;
    property DefaultShowWizardKey: word read FDefaultShowWizardKey write FDefaultShowWizardKey;
    property AllowDeleteWhenReadOnly: boolean read FAllowDeleteWhenReadOnly write FAllowDeleteWhenReadOnly;
  end;

  { TATButtonCellEditor }

  TATButtonCellEditor = class(TATButton)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
  protected
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetBounds(var Msg: TGridMessage); message GM_SETBOUNDS;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_Ready(var Msg: TGridMessage); message GM_READY;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    property Col: Integer read FCol;
    property Row: Integer read FRow;
  end;


  TmCellEditorButtonStyle = (cebsDots, cebsCalendar, cebsMagicWand, cebsSwitchBetweenValues);

  { TmExtButtonTextCellEditor }

  TmExtButtonTextCellEditor = class (TCompositeCellEditor)
  strict private
    FTextEditor: TmExtDialogCellEditor;
    FParentGrid: TCustomStringGrid;
    FButtonStyle: TmCellEditorButtonStyle;
    FOnGetValueEvent: TmOnCellEditorGetValueEvent;
    FOnClearEvent: TmOnCellEditorClearEvent;
    function GetAllowFreeTypedText: Boolean;
    procedure SetAllowFreeTypedText(Value: Boolean);
    procedure SetButtonStyle(AValue: TmCellEditorButtonStyle);
    procedure SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
    procedure SetOnGetValueEvent(AValue: TmOnCellEditorGetValueEvent);
    procedure SetParentGrid(AValue: TCustomStringGrid);
    procedure OnClickLookupButton (Sender: TObject);
  protected
    FLookupButtonEditor : TATButtonCellEditor; //TButtonCellEditor;
  public
    constructor Create(Aowner: TComponent); override;

    property TextEditor : TmExtDialogCellEditor read FTextEditor;

    property ParentGrid: TCustomStringGrid read FParentGrid write SetParentGrid;
    property ButtonStyle: TmCellEditorButtonStyle read FButtonStyle write SetButtonStyle;

    property OnGetValueEvent: TmOnCellEditorGetValueEvent read FOnGetValueEvent write SetOnGetValueEvent;
    property OnClearEvent: TmOnCellEditorClearEvent read FOnClearEvent write SetOnClearEvent;

    property AllowFreeTypedText: Boolean read GetAllowFreeTypedText write SetAllowFreeTypedText;
  end;

  { TmExtButtonsTextCellEditor }

  TmExtButtonsTextCellEditor = class (TCompositeCellEditor)
  strict private
    FTextEditor: TmExtDialogCellEditor;
    FParentGrid: TCustomStringGrid;
    FOnGetValueEvent: TmOnCellEditorGetValueEvent;
    FOnClearEvent: TmOnCellEditorClearEvent;
    function GetAllowFreeTypedText: Boolean;
    procedure SetAllowFreeTypedText(Value: Boolean);
    procedure SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
    procedure SetOnGetValueEvent(AValue: TmOnCellEditorGetValueEvent);
    procedure SetParentGrid(AValue: TCustomStringGrid);
    procedure OnClickButton (Sender: TObject);
  protected
    FButtonEditors : TObjectList;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;

    procedure SetButtons (const aValue : integer);
    procedure SetButtonStyle(const aIndex : integer; const aStyle: TmCellEditorButtonStyle);
    procedure SetOnClickButton(const aIndex : integer; const aOnClickButton: TmOnCellEditorShowWizardEvent);

    property TextEditor : TmExtDialogCellEditor read FTextEditor;

    property ParentGrid: TCustomStringGrid read FParentGrid write SetParentGrid;

    property AllowFreeTypedText: Boolean read GetAllowFreeTypedText write SetAllowFreeTypedText;

    property OnGetValueEvent: TmOnCellEditorGetValueEvent read FOnGetValueEvent write SetOnGetValueEvent;
    property OnClearEvent: TmOnCellEditorClearEvent read FOnClearEvent write SetOnClearEvent;
  end;


implementation

uses
  Controls, SysUtils,
  mBaseClassesAsObjects, mGridIcons;

type

  { TSingleButtonEditorData }

  TSingleButtonEditorData = class
  public
    constructor Create;
    destructor Destroy; override;
  public
    Editor : TATButtonCellEditor;
    ButtonStyle: TmCellEditorButtonStyle;
    OnClick: TmOnCellEditorShowWizardEvent;
  end;

{ TSingleButtonEditorData }

constructor TSingleButtonEditorData.Create;
begin
  Editor:= nil;
  OnClick:= nil;
end;

destructor TSingleButtonEditorData.Destroy;
begin
//  if Assigned(Editor) and (not Assigned(Editor.Owner)) then
//    FreeAndNil(Editor);
  inherited Destroy;
end;

{ TmExtButtonsTextCellEditor }

function TmExtButtonsTextCellEditor.GetAllowFreeTypedText: Boolean;
begin
  Result := not FTextEditor.ReadOnly;
end;

procedure TmExtButtonsTextCellEditor.SetAllowFreeTypedText(Value: Boolean);
begin
  FTextEditor.ReadOnly:= not Value;
end;


procedure TmExtButtonsTextCellEditor.SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
begin
  FOnClearEvent:=AValue;
  FTextEditor.OnClearEvent:= AValue;
end;

procedure TmExtButtonsTextCellEditor.SetOnGetValueEvent(AValue: TmOnCellEditorGetValueEvent);
begin
  FOnGetValueEvent:=AValue;
  FTextEditor.OnGetValueEvent:= aValue;
end;

procedure TmExtButtonsTextCellEditor.SetParentGrid(AValue: TCustomStringGrid);
begin
  if FParentGrid=AValue then Exit;
  FParentGrid:=AValue;
  FTextEditor.ParentGrid := aValue;
end;

procedure TmExtButtonsTextCellEditor.OnClickButton(Sender: TObject);
var
  newDisplayValue: string;
  newActualValue: variant;
  idx : integer;
begin
  for idx := 0 to FButtonEditors.Count- 1 do
  begin
    if (FButtonEditors.Items[idx] as TSingleButtonEditorData).Editor = Sender then
    begin
      if Assigned((FButtonEditors.Items[idx] as TSingleButtonEditorData).OnClick) then
      begin
        if (FButtonEditors.Items[idx] as TSingleButtonEditorData).OnClick(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
          FTextEditor.Text := newDisplayValue;
      end;
      exit;
    end;
  end;
end;

constructor TmExtButtonsTextCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FOnGetValueEvent := nil;
  FOnClearEvent := nil;
  FTextEditor := TmExtDialogCellEditor.Create(AOwner);
  Self.AddEditor(FTextEditor, alClient, true);
  FButtonEditors := TObjectList.Create(true);
end;

destructor TmExtButtonsTextCellEditor.Destroy;
begin
  FButtonEditors.Free;
  inherited Destroy;
end;

procedure TmExtButtonsTextCellEditor.SetButtons(const aValue: integer);
var
  i : integer;
  btnData : TSingleButtonEditorData;
begin
  if FButtonEditors.Count > 0 then
    raise Exception.Create('Buttons already created');

  for i := 0 to aValue - 1 do
  begin
    btnData := TSingleButtonEditorData.Create;
    FButtonEditors.Add(btnData);
    btnData.Editor := TATButtonCellEditor.Create(Self.Owner);
    btnData.Editor.Images := GridIconsDataModule.GridEditorsImageList;
    btnData.Editor.ImageIndex:= GRID_EDITORS_ICON_DOTS;
    btnData.Editor.Kind:= abuIconOnly;
    btnData.ButtonStyle:= cebsDots;
    btnData.Editor.OnClick := Self.OnClickButton();
    Self.AddEditor(btnData.Editor, alRight, false);
  end;
end;

procedure TmExtButtonsTextCellEditor.SetButtonStyle(const aIndex: integer; const aStyle: TmCellEditorButtonStyle);
begin
  if aStyle <> (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).ButtonStyle then
  begin
    (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).ButtonStyle:= aStyle;
    if aStyle = cebsCalendar then
      (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).Editor.ImageIndex:= GRID_EDITORS_ICON_CALENDAR
    else if aStyle = cebsMagicWand then
      (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).Editor.ImageIndex:= GRID_EDITORS_ICON_MAGICWAND
    else if aStyle = cebsSwitchBetweenValues then
      (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).Editor.ImageIndex:= GRID_EDITORS_ICON_SWITCH_BETWEEN_VALUES
    else
      (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).Editor.ImageIndex:= GRID_EDITORS_ICON_DOTS;
    (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).Editor.Invalidate;
  end;
end;

procedure TmExtButtonsTextCellEditor.SetOnClickButton(const aIndex: integer; const aOnClickButton: TmOnCellEditorShowWizardEvent);
begin
  (FButtonEditors.Items[aIndex] as TSingleButtonEditorData).OnClick:= aOnClickButton;
end;


{ TATButtonCellEditor }

procedure TATButtonCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_HOOKKEYDOWN or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TATButtonCellEditor.msg_SetBounds(var Msg: TGridMessage);
var
  r: TRect;
begin
  r := Msg.CellRect;
  FGrid.AdjustInnerCellRect(r);
  if r.Right-r.Left>DEFBUTTONWIDTH then
    r.Left:=r.Right-DEFBUTTONWIDTH;
  SetBounds(r.Left, r.Top, r.Right-r.Left, r.Bottom-r.Top);
end;

procedure TATButtonCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TATButtonCellEditor.msg_Ready(var Msg: TGridMessage);
begin
  Width := DEFBUTTONWIDTH;
end;

procedure TATButtonCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

{ TmExtButtonTextCellEditor }

procedure TmExtButtonTextCellEditor.SetOnClearEvent(AValue: TmOnCellEditorClearEvent);
begin
  FOnClearEvent:=AValue;
  FTextEditor.OnClearEvent:= AValue;
end;

function TmExtButtonTextCellEditor.GetAllowFreeTypedText: Boolean;
begin
  Result := not FTextEditor.ReadOnly;
end;

procedure TmExtButtonTextCellEditor.SetOnGetValueEvent(AValue: TmOnCellEditorGetValueEvent);
begin
  FOnGetValueEvent:=AValue;
  FTextEditor.OnGetValueEvent:= aValue;
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
  if Assigned(FOnGetValueEvent) then
  begin
    if FOnGetValueEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
      FTextEditor.Text := newDisplayValue;
  end;
end;

procedure TmExtButtonTextCellEditor.SetAllowFreeTypedText(Value: Boolean);
begin
  FTextEditor.ReadOnly:= not Value;
end;

procedure TmExtButtonTextCellEditor.SetButtonStyle(AValue: TmCellEditorButtonStyle);
begin
  if FButtonStyle=AValue then Exit;
  FButtonStyle:=AValue;
  if aValue = cebsCalendar then
    FLookupButtonEditor.ImageIndex:= GRID_EDITORS_ICON_CALENDAR
  else if aValue = cebsMagicWand then
    FLookupButtonEditor.ImageIndex:= GRID_EDITORS_ICON_MAGICWAND
  else if aValue = cebsSwitchBetweenValues then
    FLookupButtonEditor.ImageIndex:= GRID_EDITORS_ICON_SWITCH_BETWEEN_VALUES
  else
    FLookupButtonEditor.ImageIndex:= GRID_EDITORS_ICON_DOTS;
  FLookupButtonEditor.Invalidate;
end;

constructor TmExtButtonTextCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FOnGetValueEvent := nil;
  FOnClearEvent := nil;
  FTextEditor := TmExtDialogCellEditor.Create(AOwner);
  FLookupButtonEditor := TATButtonCellEditor.Create(AOwner); //TButtonCellEditor.Create(AOwner);
  FLookupButtonEditor.Images := GridIconsDataModule.GridEditorsImageList;
  FLookupButtonEditor.ImageIndex:= GRID_EDITORS_ICON_DOTS;
  FLookupButtonEditor.Kind:= abuIconOnly;
  FButtonStyle:= cebsDots;
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
    if Assigned(FOnGetValueEvent) then
    begin
      if FOnGetValueEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
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
  if Assigned(FOnGetValueEvent) then
  begin
    if FOnGetValueEvent(FParentGrid.Col, FParentGrid.Row, newDisplayValue, newActualValue) then
      Self.Text := newDisplayValue;
  end;
end;

constructor TmExtDialogCellEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FDefaultShowEditorKey := VK_RETURN;
  FDefaultShowWizardKey := VK_F1;
  FDefaultClearKey := VK_DELETE;
  FAllowDeleteWhenReadOnly := True;
  FOnGetValueEvent := nil;
  FOnShowWizardEvent := nil;
  FOnClearEvent := nil;
end;

end.
