// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFilterPanel;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, StrUtils, Contnrs, Variants,
  ExtCtrls, EditBtn, Menus, Spin,
  mFilter, mBaseClassesAsObjects, mMathUtility;

type
  { TmFilterConditionPanel }

  TmFilterConditionPanel = class (TCustomPanel)
  private
    procedure SetFilterOperator(AValue: TmFilterOperator);
    procedure SetFlex(AValue: integer);
  protected
    FFlex : integer;
    FFieldName : String;
    FFilterOperator : TmFilterOperator;
    FOperatorsMenu : TPopupMenu;
    FAllowedOperators : TmFilterOperatorsSet;
    function CreateStandardLabel: TLabel;
    function CreateStandardOperatorMenu (aLabel: TLabel) : TPopupMenu;
    function FormatFilterCaption (aValue : String) : String;
    procedure UpdateCurrentOperatorCheck;
    procedure OperatorMenuItemClick (Sender : TObject);
    procedure OperatorMenuPopup (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); virtual; abstract;

    function GetFilterValue : Variant; virtual; abstract;

    function IsEmpty : boolean; virtual; abstract;
    procedure Clear; virtual; abstract;

    property Flex : integer read FFlex write SetFlex;
    property FieldName : String read FFieldName write FFieldName;
    property FilterOperator : TmFilterOperator read FFilterOperator write SetFilterOperator;
    property AllowedOperators : TmFilterOperatorsSet read FAllowedOperators write FAllowedOperators;
  end;

  { TmDateFilterConditionPanel }

  TmDateFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FDateEdit : TDateEdit;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    function IsEmpty : boolean; override;
    procedure Clear; override;
  end;

  TmEditFilterValueType = (efString, efUppercaseString, efInteger, efFloat);

  { TmEditFilterConditionPanel }

  TmEditFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FEdit : TEdit;
    FValueType : TmEditFilterValueType;
    procedure OnEditValueChanged (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure SetFilterValue (aValue : Variant);
    function IsEmpty : boolean; override;
    procedure Clear; override;

    property ValueType : TmEditFilterValueType read FValueType write FValueType;

  end;

  { TmComboFilterConditionPanel }

  TmComboFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FCombobox: TComboBox;
    FGarbage : TObjectList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure AddItem (aValue : String); overload;
    procedure AddItem (aLabel : String; aValue : Variant); overload;
    procedure ClearItems;
    procedure OptimalWidth;

    procedure Clear; override;
    function IsEmpty : boolean; override;
  end;

  { TmExecuteFilterPanel }

  TmExecuteFilterPanel = class (TmFilterConditionPanel)
  private
    FClearButton : TButton;
    FFilterButton : TButton;
    FOnClickClear : TNotifyEvent;
    FOnClickFilter : TNotifyEvent;

    procedure InternalOnClickClear (Sender: TObject);
    procedure InternalOnClickFilter (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure Clear; override;
    function IsEmpty : boolean; override;

    property OnClickClear : TNotifyEvent read FOnClickClear write FOnClickClear;
    property OnClickFilter : TNotifyEvent read FOnClickFilter write FOnClickFilter;
  end;

  { TmFilterPanel }

  TmFilterPanel = class (TCustomFlowPanel)
  strict private
    FFilterConditionPanels : TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddFilterCondition (aPanel : TmFilterConditionPanel);
    procedure ClearAll();
    procedure GetFilters (aFilters : TmFilters);
  end;


implementation

uses
  SysUtils
  {$IFNDEF LINUX},Windows{$ENDIF}
  ;

const
  DEFAULT_FLEX_WIDTH = 50;
  {$IFDEF LINUX}
  CB_SETDROPPEDWIDTH = 352;
  {$ENDIF}

{ TmExecuteFilterPanel }

procedure TmExecuteFilterPanel.InternalOnClickClear(Sender: TObject);
begin
  If Assigned(FOnClickClear) then
    FOnClickClear(Sender);
end;

procedure TmExecuteFilterPanel.InternalOnClickFilter(Sender: TObject);
begin
  if Assigned(FOnClickFilter) then
    FOnClickFilter(Sender);
end;

constructor TmExecuteFilterPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FClearButton := TButton.Create(Self);
  FClearButton.Parent := Self;
  FClearButton.Height:= Self.Height div 2;
  FClearButton.Align:= alTop;
  FClearButton.Caption := 'Ripulisci';
  FFilterButton := TButton.Create(Self);
  FFilterButton.Parent := Self;
  FFilterButton.Align := alClient;
  FFilterButton.Caption := 'Cerca';
  FOnClickClear:= nil;
  FOnClickFilter:= nil;
  FClearButton.OnClick:= Self.InternalOnClickClear;
  FFilterButton.OnClick:= Self.InternalOnClickFilter;
end;

procedure TmExecuteFilterPanel.SetFilterCaption(aValue: String);
begin
  // do nothing
end;

function TmExecuteFilterPanel.GetFilterValue: Variant;
begin
  Result := null;
end;

procedure TmExecuteFilterPanel.Clear;
begin
  // do nothing
end;

function TmExecuteFilterPanel.IsEmpty: boolean;
begin
  Result := true;
end;

{ TmFilterPanel }


constructor TmFilterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := alTop;
  Self.AutoSize := true;
  Self.BevelOuter:= bvNone;
  Self.BevelInner:= bvNone;
  FFilterConditionPanels := TList.Create;
end;

destructor TmFilterPanel.Destroy;
begin
  FFilterConditionPanels.Free;
  inherited Destroy;
end;

procedure TmFilterPanel.AddFilterCondition(aPanel: TmFilterConditionPanel);
begin
  FFilterConditionPanels.Add(aPanel);
  aPanel.Parent := Self;
end;

procedure TmFilterPanel.ClearAll;
var
  i : integer;
begin
  for i := 0 to FFilterConditionPanels.Count -1 do
  begin
    TmFilterConditionPanel(FFilterConditionPanels[i]).Clear;
  end;
end;

procedure TmFilterPanel.GetFilters(aFilters: TmFilters);
var
  i : integer;
  tmp : TmFilterConditionPanel;
begin
  for i := 0 to FFilterConditionPanels.Count - 1 do
  begin
    tmp := TmFilterConditionPanel(FFilterConditionPanels.Items[i]);
    if not tmp.IsEmpty then
    begin
      with aFilters.Add do
      begin
        FieldName:= tmp.FieldName;
        FilterOperator:= tmp.FilterOperator;
        Value:= tmp.GetFilterValue;
      end;
    end;
  end;
end;

{ TmEditFilterConditionPanel }

procedure TmEditFilterConditionPanel.OnEditValueChanged(Sender: TObject);
begin
  FEdit.OnEditingDone:= nil;
  try
    if FValueType = efInteger then
    begin
      if not IsNumeric(FEdit.Text, false) then
        FEdit.Text := '';
    end
    else
    if FValueType = efFloat then
    begin
      if not IsNumeric(FEdit.Text, true) then
        FEdit.Text := '';
    end
    else
    if FValueType = efUppercaseString then
    begin
      FEdit.Text:= UpperCase(FEdit.Text);
    end;
  finally
    FEdit.OnEditingDone:= Self.OnEditValueChanged;
  end;
end;

constructor TmEditFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alBottom;
  FEdit.Text := '';
  FEdit.OnEditingDone:= Self.OnEditValueChanged;
  FLabel := Self.CreateStandardLabel;
  FValueType:= efString;
  CreateStandardOperatorMenu(FLabel);
end;

procedure TmEditFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmEditFilterConditionPanel.GetFilterValue: Variant;
var
  tmp : Double;
begin
  if FEdit.Text = '' then
    Result := null
  else
  if FValueType = efUppercaseString then
  begin
    Result := Uppercase(FEdit.Text)
  end
  else
  if FValueType = efInteger then
  begin
    if IsNumeric(FEdit.Text, false) then
      Result := StrToInt(FEdit.Text)
    else
      Result := null;
  end
  else
  if FValueType = efFloat then
  begin
    if IsNumeric(FEdit.Text, true) then
    begin
      if TryToConvertToDouble(FEdit.Text, tmp) then
        Result := tmp
      else
        Result := null;
    end
    else
      Result := null;
  end
  else
    Result := FEdit.Text;
end;

procedure TmEditFilterConditionPanel.SetFilterValue(aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.Clear
  else
    FEdit.Text := VarToStr(aValue);
end;

function TmEditFilterConditionPanel.IsEmpty: boolean;
begin
  Result := (FEdit.Text = '');
end;

procedure TmEditFilterConditionPanel.Clear;
begin
  FEdit.Text:= '';
end;

{ TmComboFilterConditionPanel }

constructor TmComboFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCombobox := TComboBox.Create(Self);
  FComboBox.Parent := Self;
  FComboBox.Align:= alBottom;
  FComboBox.Style:= csDropDownList;
  FComboBox.DropDownCount:= 20;
  FLabel := Self.CreateStandardLabel;
  FGarbage := TObjectList.Create(true);
end;

destructor TmComboFilterConditionPanel.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

procedure TmComboFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmComboFilterConditionPanel.GetFilterValue: Variant;
begin
  if FComboBox.ItemIndex < 0 then
    Result := Null
  else
    Result := (FComboBox.Items.Objects[FComboBox.ItemIndex] as TVariantObject).Value;
end;

(*
function TmComboFilterConditionPanel.GetFilterObject: TObject;
begin
  if FComboBox.ItemIndex < 0 then
    Result := nil
  else
    Result := FComboBox.Items.Objects[FComboBox.ItemIndex];
end;

function TmComboFilterConditionPanel.GetFilterTag: NativeInt;
begin
  inherited;
  if FComboBox.ItemIndex >= 0 then
    Result := NativeInt(pointer(FComboBox.Items.Objects[FComboBox.ItemIndex]));
end;
*)

procedure TmComboFilterConditionPanel.AddItem(aValue: String);
begin
  Self.AddItem(aValue, aValue);
end;

procedure TmComboFilterConditionPanel.AddItem(aLabel: String; aValue: Variant);
var
  sh : TVariantObject;
begin
  sh := TVariantObject.Create(aValue);
  FGarbage.Add(sh);
  FComboBox.AddItem(aLabel, sh);
end;

procedure TmComboFilterConditionPanel.ClearItems;
begin
  FCombobox.Items.Clear;
  FGarbage.Clear;
end;

procedure TmComboFilterConditionPanel.OptimalWidth;
// Code from: https://www.thoughtco.com/sizing-the-combobox-drop-down-width-1058301
const
  HORIZONTAL_PADDING = 4;
var
  itemsFullWidth: integer;
  idx: integer;
  itemWidth: integer;
begin
  itemsFullWidth := 0;

  // get the max needed with of the items in dropdown state
  for idx := 0 to -1 + FCombobox.Items.Count do
  begin
    itemWidth := FCombobox.Canvas.TextWidth(FCombobox.Items[idx]);
    Inc(itemWidth, 2 * HORIZONTAL_PADDING);
    if (itemWidth > itemsFullWidth) then itemsFullWidth := itemWidth;
  end;

  // set the width of drop down if needed
  if (itemsFullWidth > FCombobox.Width) then
  begin
    //check if there would be a scroll bar
    if FCombobox.DropDownCount < FCombobox.Items.Count then
      itemsFullWidth := itemsFullWidth + {$IFDEF LINUX} 20 {$ELSE} GetSystemMetrics(SM_CXVSCROLL){$ENDIF};

    {$IFDEF LINUX}
    Perform(FCombobox.Handle, CB_SETDROPPEDWIDTH, itemsFullWidth);
    {$ELSE}
    SendMessage(FCombobox.Handle, CB_SETDROPPEDWIDTH, itemsFullWidth, 0);
    {$ENDIF}
  end;
end;

procedure TmComboFilterConditionPanel.Clear;
begin
  FCombobox.ItemIndex:= -1;
  FCombobox.Text:= '';
end;

function TmComboFilterConditionPanel.IsEmpty: boolean;
begin
  Result := (FCombobox.ItemIndex < 0);
end;

{ TmDateFilterConditionPanel }

constructor TmDateFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDateEdit := TDateEdit.Create(Self);
  FDateEdit.Parent := Self;
  FDateEdit.Align:= alBottom;
  FLabel := Self.CreateStandardLabel;
  CreateStandardOperatorMenu(FLabel);
end;

procedure TmDateFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmDateFilterConditionPanel.GetFilterValue: Variant;
begin
  if FDateEdit.Text = '' then
    Result := Null
  else
    Result := FDateEdit.Date;
end;

function TmDateFilterConditionPanel.IsEmpty: boolean;
begin
  Result := (trim(FDateEdit.Text) = '');
end;

procedure TmDateFilterConditionPanel.Clear;
begin
  FDateEdit.Text:= '';
end;

{ TmFilterConditionPanel }

procedure TmFilterConditionPanel.SetFilterOperator(AValue: TmFilterOperator);
begin
  if FFilterOperator=AValue then Exit;
  FFilterOperator:=AValue;
  UpdateCurrentOperatorCheck;
end;

procedure TmFilterConditionPanel.SetFlex(AValue: integer);
begin
  if FFlex=AValue then Exit;
  FFlex:=AValue;
  Self.Width := FFlex * DEFAULT_FLEX_WIDTH;
end;

function TmFilterConditionPanel.CreateStandardLabel: TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.Caption := '';
  Result.WordWrap:= true;
  Result.Alignment:= taCenter;
end;

function TmFilterConditionPanel.CreateStandardOperatorMenu (aLabel: TLabel) : TPopupMenu;
var
  mi : TMenuItem;
begin
  if not Assigned(FOperatorsMenu) then
  begin
    FOperatorsMenu := TPopupMenu.Create(Self);
    aLabel.PopupMenu := FOperatorsMenu;
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= '=';
    mi.Tag:= integer(foEq);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:='contains';
    mi.Tag := integer(foLike);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= '<>';
    mi.Tag:= integer(foNotEq);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= '>=';
    mi.Tag:= integer(foGtOrEq);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= '<=';
    mi.Tag:= integer(foLtOrEq);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= 'starts';
    mi.Tag:= integer(foStartWith);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);
    mi := TMenuItem.Create(FOperatorsMenu);
    mi.Caption:= 'ends';
    mi.Tag:= integer(foEndWith);
    mi.OnClick:= OperatorMenuItemClick;
    FOperatorsMenu.Items.Add(mi);

    UpdateCurrentOperatorCheck;
    FOperatorsMenu.OnPopup:= OperatorMenuPopup;
  end;

  Result := FOperatorsMenu;
end;

function TmFilterConditionPanel.FormatFilterCaption(aValue: String) : String;
begin
  if not AnsiEndsText(':', aValue) then
    Result := aValue + ':'
  else
    Result := aValue;
end;

procedure TmFilterConditionPanel.UpdateCurrentOperatorCheck;
var
  i : integer;
begin
  if Assigned(FOperatorsMenu) then
  begin
    for i := 0 to FOperatorsMenu.Items.Count - 1 do
      FOperatorsMenu.Items[i].Checked:= FOperatorsMenu.Items[i].Tag = integer(FFilterOperator);
  end;
end;

procedure TmFilterConditionPanel.OperatorMenuItemClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
    Self.FilterOperator := TmFilterOperator((Sender as TMenuItem).Tag);
end;

procedure TmFilterConditionPanel.OperatorMenuPopup(Sender: TObject);
var
  i : integer;
begin
  if Assigned(FOperatorsMenu) then
  begin
    for i := 0 to FOperatorsMenu.Items.Count - 1 do
      FOperatorsMenu.Items[i].Enabled:= (TmFilterOperator(FOperatorsMenu.Items[i].Tag) in FAllowedOperators);
  end;
end;

constructor TmFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.FFlex := 2;
  Self.Width := Self.FFlex * DEFAULT_FLEX_WIDTH;
  Self.Caption := '';
  Self.Height := 40;
  Self.FFilterOperator:= foUnknown;
  Self.FAllowedOperators:= [];
end;


end.
