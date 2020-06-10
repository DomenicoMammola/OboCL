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
  Classes, Controls, Graphics, StdCtrls, StrUtils, Contnrs, Variants, Types,
  ExtCtrls, EditBtn, Menus, {$IFNDEF LINUX}Windows, {$ENDIF}
  ATButtons,
  mFilter, mFilterOperators, mBaseClassesAsObjects, mMathUtility,
  mUtility, mDateEdit, mDataProviderFieldDefs, mDataProviderInterfaces,
  mFilterPanelDataModule, mDataProvider;

resourcestring
  SClearFilterCommand = 'Clear';

type
  { TmFilterConditionPanel }

  TmFilterConditionPanel = class (TCustomPanel)
  protected
    FFlex : integer;
    FFieldName : String;
    FCaption: string;
    FFilterOperator : TmFilterOperator;
    FOperatorsMenuItemSeparator : TMenuItem;
    FFilterMenu : TPopupMenu;
    FOperatorsMenuItems : TList;
    FAllowedOperators : TmFilterOperatorsSet;
    FStandardLabelFontSize : integer;
    function CreateStandardLabel: TLabel;
    function CreateStandardFilterMenu (aLabel: TLabel; const aAddFilterOperators : boolean) : TPopupMenu;
    procedure ApplyFilterCaption (aLabel : TLabel; const aValue : String; const aShowOperator: boolean= true);
    procedure UpdateCurrentOperatorCheck;
    procedure OperatorMenuItemClick (Sender : TObject);
    procedure ClearMenuItemClick (Sender : TObject);
    procedure FilterMenuPopup (Sender : TObject);
    procedure SetFilterOperator(AValue: TmFilterOperator); virtual;
  protected
    procedure SetFlex(AValue: integer); virtual;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFilterCaption (aValue : String); virtual;

    procedure ExportToFilter (aFilter : TmFilter); virtual;
    procedure ImportFromFilter (const aFilter: TmFilter); virtual;

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
    FBottomPanel : TPanel;
    FDateEditMin : TmDateEdit;
    FDateEditMax : TmDateEdit;
  protected
    procedure SetFilterOperator(AValue: TmFilterOperator); override;
    procedure SetFlex(AValue: integer); override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    procedure SetFilterValue (aValue : Variant);
    procedure ExportToFilter (aFilter : TmFilter); override;
    procedure ImportFromFilter (const aFilter: TmFilter); override;
    function IsEmpty : boolean; override;
    procedure Clear; override;
  end;

  TmEditFilterValueType = (efString, efUppercaseString, efInteger, efFloat, efUniqueIdentifier, efDate, efDateTime);
  TmComboCheckLookupFilterValueType = (cfString, cfInteger, cfFloat, cfDate, cfDateTime);

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
    procedure ExportToFilter (aFilter : TmFilter); override;
    procedure ImportFromFilter (const aFilter: TmFilter); override;
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
    FDefaultItemIndex : integer;
    FValueType : TmComboCheckLookupFilterValueType;
    FWidthOptimized : boolean;
    procedure SetDefaultItemIndex(AValue: integer);
    procedure OnDropDown(Sender : TObject);
    procedure CustomDrawItem (Control: TWinControl; Index: Integer;ARect: TRect; State: StdCtrls.TOwnerDrawState);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFilterCaption (aValue : String); override;
    procedure ExportToFilter (aFilter : TmFilter); override;
    procedure ImportFromFilter (const aFilter: TmFilter); override;
    procedure AddItem (aValue : String); overload;
    procedure AddItem (aLabel : String; aValue : Variant); overload;
    procedure ClearItems;
    procedure OptimalWidth;

    procedure Clear; override;
    function IsEmpty : boolean; override;

    property DefaultItemIndex : integer read FDefaultItemIndex write SetDefaultItemIndex;
    property ValueType : TmComboCheckLookupFilterValueType read FValueType write FValueType;
  end;

  { TmCheckListFilterConditionPanel }

  TmCheckListFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FCurrentValue : variant;
    FLabel : TLabel;
    FEdit : TEditButton;
    FGarbage : TObjectList;
    FValues : TStringList;
    FValueType : TmComboCheckLookupFilterValueType;
    procedure OnShowValuesList (Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFilterCaption (aValue : String); override;
    procedure ExportToFilter (aFilter : TmFilter); override;
    procedure ImportFromFilter (const aFilter: TmFilter); override;
    procedure AddItem (aValue : String); overload;
    procedure AddItem (aLabel : String; aValue : Variant); overload;
    procedure ClearItems;

    procedure Clear; override;
    function IsEmpty : boolean; override;
    property ValueType : TmComboCheckLookupFilterValueType read FValueType write FValueType;
  end;


  TmLookupFilterCondizionOnFillVirtualFields = procedure (aFieldDefs : TmVirtualFieldDefs) of object;



  TmBaseLookupFilterConditionPanel = class(TmFilterConditionPanel)
  protected
    FCurrentValue : variant;
    FLabel : TLabel;
    FEdit : TEditButton;
    FValueType : TmEditFilterValueType;
    FKeyFieldName : string;
    FLookupFieldNames : TStringList;
    FDisplayLabelFieldNames : TStringList;
    FOnFillVirtualFieldDefs : TmLookupFilterCondizionOnFillVirtualFields;

    procedure OnEditValueChanged (Sender : TObject);
    procedure OnShowLookup (Sender: TObject); virtual; abstract;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFilterCaption (aValue : String); override;
    procedure ExportToFilter (aFilter : TmFilter); override;
    procedure ImportFromFilter (const aFilter: TmFilter); override;
    procedure SetFilterValue (const aValue : Variant; const aDescription: String);
    function IsEmpty : boolean; override;
    procedure Clear; override;

    property ValueType : TmEditFilterValueType read FValueType write FValueType;
    property KeyFieldName : String read FKeyFieldName write FKeyFieldName;
    property OnFillVirtualFieldDefs : TmLookupFilterCondizionOnFillVirtualFields read FOnFillVirtualFieldDefs write FOnFillVirtualFieldDefs;
    property LookupFieldNames : TStringList read FLookupFieldNames;
    property DisplayLabelFieldNames: TStringList read FDisplayLabelFieldNames;
  end;

  { TmLookupFilterConditionPanel }
  TmLookupFilterConditionPanel = class(TmBaseLookupFilterConditionPanel)
  strict private
    FDataProvider: IVDDataProvider;
  protected
    procedure OnShowLookup (Sender: TObject); override;
  public
    property DataProvider : IVDDataProvider read FDataProvider write FDataProvider;
  end;

  { TmInstantQueryLookupFilterConditionPanel }

  TmInstantQueryLookupFilterConditionPanel = class(TmBaseLookupFilterConditionPanel)
  strict private
    FInstantQueryManager: IVDInstantQueryManager;
  protected
    procedure OnShowLookup (Sender: TObject); override;
  public
    property InstantQueryManager : IVDInstantQueryManager read FInstantQueryManager write FInstantQueryManager;
  end;


  { TmExecuteFilterPanel }

  TmExecuteFilterPanel = class (TmFilterConditionPanel)
  private
//    FClearButton : TButton;
//    FFilterButton : TButton;
    FClearButton : TATButton;
    FFilterButton : TATButton;
    FOnClickClear : TNotifyEvent;
    FOnClickFilter : TNotifyEvent;
    FImagesDataModule: TmFilterPnlDataModule;

    procedure InternalOnClickClear (Sender: TObject);
    procedure InternalOnClickFilter (Sender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ExportToFilter (aFilter : TmFilter); override;
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
  SysUtils, LResources, Forms,
  mQuickReadOnlyVirtualDataSet, mLookupForm, mCheckListForm, mlookupformInstantQuery,
  mDoubleList, mGraphicsUtility, mMagnificationFactor;

const
  DEFAULT_FLEX_WIDTH = 50;
  {$IFDEF LINUX}
  DEFAULT_HEIGHT = 45;
  {$ELSE}
  DEFAULT_HEIGHT = 40;
  {$ENDIF}

{ TmInstantQueryLookupFilterConditionPanel }

procedure TmInstantQueryLookupFilterConditionPanel.OnShowLookup(Sender: TObject);
var
  lookupFrm: TmLookupInstantQueryFrm;
  keyFieldName : string;
  tmpVirtualFieldDefs : TmVirtualFieldDefs;
begin
  if not Assigned(FInstantQueryManager) then
    raise Exception.Create('[TmInstantQueryLookupFilterConditionPanel] Missing InstantQueryManager.');

  lookupFrm := TmLookupInstantQueryFrm.Create(Self);
  try
    if FLookupFieldNames.Count = 0 then
      FInstantQueryManager.GetDataProvider.GetMinimumFields(FLookupFieldNames);
    if FLookupFieldNames.Count = 0 then
    begin
      tmpVirtualFieldDefs := TmVirtualFieldDefs.Create;
      try
        FInstantQueryManager.GetDataProvider.FillVirtualFieldDefs(tmpVirtualFieldDefs, '');
        tmpVirtualFieldDefs.ExtractFieldNames(FLookupFieldNames);
      finally
        tmpVirtualFieldDefs.Free;;
      end;
    end;

    if FDisplayLabelFieldNames.Count = 0 then
      FInstantQueryManager.GetDataProvider.GetMinimumFields(FDisplayLabelFieldNames);
    if FDisplayLabelFieldNames.Count = 0 then
      raise Exception.Create('[TmInstantQueryLookupFilterConditionPanel] Missing DisplayLabelFieldNames.');

    if FKeyFieldName <> '' then
      keyFieldName := FKeyFieldName
    else
      keyFieldName := FInstantQueryManager.GetDataProvider.GetKeyFieldName;

    lookupFrm.Init(FInstantQueryManager, FLookupFieldNames, keyFieldName, FDisplayLabelFieldNames);
    if lookupFrm.ShowModal = mrOk then
    begin
      FEdit.Text:= lookupFrm.SelectedDisplayLabel;
      FCurrentValue:= lookupFrm.SelectedValue;
    end;
  finally
    lookupFrm.Free;
  end;
end;

{ TmCheckListFilterConditionPanel }

procedure TmCheckListFilterConditionPanel.OnShowValuesList(Sender: TObject);
var
  Dlg : TmCheckListWindow;
  i : integer;
begin
  Dlg := TmCheckListWindow.Create(Self);
  try
    for i := 0 to FValues.Count - 1 do
      Dlg.AddValue(FValues.Strings[i], (FGarbage.Items[i] as TVariantObject).Value);
    Dlg.SetCurrentValue(FCurrentValue);
    if Dlg.ShowModal = mrOK then
    begin
      FCurrentValue:= Dlg.Selected;
      if VarIsNull(FCurrentValue) then
        FEdit.Clear
      else
        FEdit.Text:= Dlg.SelectedLabels;
    end;
  finally
    Dlg.Free;
  end;
end;

constructor TmCheckListFilterConditionPanel.Create(TheOwner: TComponent);
begin
   inherited Create(TheOwner);
  FCurrentValue:= null;
  FEdit := TEditButton.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alBottom;
  FEdit.Text := '';
  FEdit.DirectInput:= false;
  FEdit.Flat:= true;
//  FEdit.OnEditingDone:= Self.OnEditValueChanged;
  FEdit.ButtonCaption:='...';
  FEdit.OnButtonClick:= Self.OnShowValuesList;
  FLabel := Self.CreateStandardLabel;
  CreateStandardFilterMenu(FLabel, true);
  FGarbage := TObjectList.Create(true);
  FValues := TStringList.Create;
  FValueType:= cfString;
  CreateStandardFilterMenu(FLabel, false);
end;

destructor TmCheckListFilterConditionPanel.Destroy;
begin
  FGarbage.Free;
  FValues.Free;
  inherited Destroy;
end;

procedure TmCheckListFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  inherited;
  Self.ApplyFilterCaption(FLabel, aValue, False);
end;

procedure TmCheckListFilterConditionPanel.ExportToFilter(aFilter: TmFilter);
begin
  inherited ExportToFilter(aFilter);
  aFilter.Value:= FCurrentValue;
  aFilter.DisplayValue:= FEdit.Text;
  case FValueType of
    cfInteger: aFilter.DataType:= fdtInteger;
    cfFloat: aFilter.DataType:= fdtFloat;
    cfDate: aFilter.DataType:= fdtDate;
    cfDateTime: aFilter.DataType:= fdtDateTime
  else
    aFilter.DataType:= fdtString;
  end;
end;

procedure TmCheckListFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
begin
  inherited ImportFromFilter(aFilter);
  FCurrentValue := aFilter.Value;
  FEdit.Text:= aFilter.DisplayValue;
end;

procedure TmCheckListFilterConditionPanel.AddItem(aValue: String);
begin
  Self.AddItem(aValue, aValue);
end;

procedure TmCheckListFilterConditionPanel.AddItem(aLabel: String; aValue: Variant);
var
  sh : TVariantObject;
begin
  sh := TVariantObject.Create(aValue);
  FGarbage.Add(sh);
  FValues.Add(aLabel);
end;

procedure TmCheckListFilterConditionPanel.ClearItems;
begin
  FValues.Clear;
  FGarbage.Clear;
end;


procedure TmCheckListFilterConditionPanel.Clear;
begin
  ClearItems;
  FCurrentValue:= null;
  FEdit.Text:= '';
end;

function TmCheckListFilterConditionPanel.IsEmpty: boolean;
begin
  Result := VarIsNull(FCurrentValue);
end;

{ TmBaseLookupFilterConditionPanel }

procedure TmBaseLookupFilterConditionPanel.OnEditValueChanged(Sender: TObject);
begin

end;

procedure TmLookupFilterConditionPanel.OnShowLookup(Sender: TObject);
var
  lookupFrm: TmLookupFrm;
  keyFieldName : string;
  tmpVirtualFieldDefs : TmVirtualFieldDefs;
begin
  if not Assigned(FDataProvider) then
    raise Exception.Create('[TmLookupFilterConditionPanel] Missing DataProvider.');

  lookupFrm := TmLookupFrm.Create(Self);
  try
    if FLookupFieldNames.Count = 0 then
      FDataProvider.GetMinimumFields(FLookupFieldNames);
    if FLookupFieldNames.Count = 0 then
    begin
      tmpVirtualFieldDefs := TmVirtualFieldDefs.Create;
      try
        FDataProvider.FillVirtualFieldDefs(tmpVirtualFieldDefs, '');
        tmpVirtualFieldDefs.ExtractFieldNames(FLookupFieldNames);
      finally
        tmpVirtualFieldDefs.Free;;
      end;
    end;

    if FDisplayLabelFieldNames.Count = 0 then
      FDataProvider.GetMinimumFields(FDisplayLabelFieldNames);
    if FDisplayLabelFieldNames.Count = 0 then
      raise Exception.Create('[TmLookupFilterConditionPanel] Missing DisplayLabelFieldNames.');

    if FKeyFieldName <> '' then
      keyFieldName := FKeyFieldName
    else
      keyFieldName := FDataProvider.GetKeyFieldName;

    lookupFrm.Init(FDataProvider, FLookupFieldNames, keyFieldName, FDisplayLabelFieldNames);
    if lookupFrm.ShowModal = mrOk then
    begin
      FEdit.Text:= lookupFrm.SelectedDisplayLabel;
      FCurrentValue:= lookupFrm.SelectedValue;
    end;
  finally
    lookupFrm.Free;
  end;
end;


constructor TmBaseLookupFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCurrentValue:= null;
  FEdit := TEditButton.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alBottom;
  FEdit.Text := '';
  FEdit.DirectInput:= false;
  FEdit.OnEditingDone:= Self.OnEditValueChanged;
  FEdit.ButtonCaption:='...';
  FEdit.OnButtonClick:= Self.OnShowLookup;
  FEdit.Flat:= true;
  FLabel := Self.CreateStandardLabel;
  FValueType:= efString;
  CreateStandardFilterMenu(FLabel, true);
  FKeyFieldName:= '';
//  FValueFieldName:= '';
  FOnFillVirtualFieldDefs:= nil;
  FLookupFieldNames := TStringList.Create;
  FDisplayLabelFieldNames := TStringList.Create;
end;

destructor TmBaseLookupFilterConditionPanel.Destroy;
begin
  FLookupFieldNames.Free;
  FDisplayLabelFieldNames.Free;
  inherited Destroy;
end;

procedure TmBaseLookupFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  inherited SetFilterCaption(aValue);
  Self.ApplyFilterCaption(FLabel, aValue, false);
end;

procedure TmBaseLookupFilterConditionPanel.ExportToFilter(aFilter: TmFilter);
begin
  inherited ExportToFilter(aFilter);
  aFilter.DisplayValue:= FEdit.Text;
  aFilter.DataType:= fdtString;
  if VarIsNull(FCurrentValue) then
    aFilter.Value := null
  else if FValueType = efUppercaseString then
    aFilter.Value := Uppercase(VarToStr(FCurrentValue))
  else
  if FValueType = efUniqueIdentifier then
  begin
    if IsUniqueIdentifier(trim(VarToStr(FCurrentValue))) then
      aFilter.Value := trim(FCurrentValue)
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtString;
  end;
  if FValueType = efInteger then
  begin
    if IsNumeric(VarToStr(FCurrentValue), false, true) then
      aFilter.Value := FCurrentValue
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtInteger;
  end
  else
  if (FValueType = efFloat) then
  begin
    if IsNumeric(VarToStr(FCurrentValue), true, true) then
      aFilter.Value := FCurrentValue
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtFloat;
  end
  else if (FValueType = efDateTime) then
  begin
    if VarIsFloat(FCurrentValue) then
      aFilter.Value := FCurrentValue
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtDateTime;
  end
  else if (FValueType = efDate) then
  begin
    if VarIsOrdinal(FCurrentValue) then
      aFilter.Value := FCurrentValue
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtDate;
  end
  else
    aFilter.Value := FCurrentValue;
end;

procedure TmBaseLookupFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
begin
  Self.Clear;
  inherited ImportFromFilter(aFilter);
  FCurrentValue := aFilter.Value;
  FEdit.Text := aFilter.DisplayValue;
end;

procedure TmBaseLookupFilterConditionPanel.SetFilterValue(const aValue : Variant; const aDescription: String);
begin
  if VarIsNull(aValue) then
    Self.Clear
  else
  begin
    FCurrentValue:= aValue;
    FEdit.Text := aDescription;
    Self.FilterOperator := foEq;
  end;
end;

function TmBaseLookupFilterConditionPanel.IsEmpty: boolean;
begin
  Result := VarIsNull(FCurrentValue);
end;

procedure TmBaseLookupFilterConditionPanel.Clear;
begin
  FEdit.Text:= '';
  FCurrentValue:= null;
end;

  {$IFDEF LINUX}
const
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
  FImagesDataModule:= TmFilterPnlDataModule.Create(Self);

  FClearButton := TATButton.Create(Self);
  FClearButton.Parent := Self;
  FClearButton.Width:= Self.Width div 2;
  FClearButton.Align:= alLeft;
  FClearButton.Images := FImagesDataModule.FilterPanelExecuteIcons;
  FClearButton.ImageIndex:= 0;
  FClearButton.Kind:=abuIconOnly;
  FClearButton.Flat := true;
  FFilterButton := TATButton.Create(Self);
  FFilterButton.Parent := Self;
  FFilterButton.Align := alClient;
  FFilterButton.Images := FImagesDataModule.FilterPanelExecuteIcons;
  FFilterButton.ImageIndex:= 1;
  FFilterButton.Kind:= abuIconOnly;
  FFilterButton.Flat:= true;
  FOnClickClear:= nil;
  FOnClickFilter:= nil;
  FClearButton.OnClick:= Self.InternalOnClickClear;
  FFilterButton.OnClick:= Self.InternalOnClickFilter;
end;


procedure TmExecuteFilterPanel.ExportToFilter (aFilter : TmFilter);
begin
  inherited ExportToFilter(aFilter);
  // do nothing
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
      tmp.ExportToFilter(aFilters.Add);
    end;
  end;
end;

{ TmEditFilterConditionPanel }

procedure TmEditFilterConditionPanel.OnEditValueChanged(Sender: TObject);
begin
  FEdit.OnEditingDone:= nil;
  try
    if FValueType = efUniqueIdentifier then
    begin
      if not IsUniqueIdentifier(FEdit.Text) then
        FEdit.Text := '';
    end
    else
    if FValueType = efInteger then
    begin
      if not IsNumeric(FEdit.Text, false, true) then
        FEdit.Text := '';
    end
    else
    if FValueType = efFloat then
    begin
      if not IsNumeric(FEdit.Text, true, true) then
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
  CreateStandardFilterMenu(FLabel, true);
end;

procedure TmEditFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  inherited;
  Self.ApplyFilterCaption(FLabel, aValue);
end;

procedure TmEditFilterConditionPanel.ExportToFilter (aFilter : TmFilter);
var
  tmp : Double;
  tmpDateTime : TDateTime;
begin
  inherited ExportToFilter(aFilter);
  aFilter.DataType:= fdtString;
  if FEdit.Text = '' then
    aFilter.Value := null
  else
  if FValueType = efUppercaseString then
    aFilter.Value := Uppercase(FEdit.Text)
  else
  if FValueType = efUniqueIdentifier then
  begin
    if IsUniqueIdentifier(trim(FEdit.Text)) then
      aFilter.Value := trim(FEdit.Text)
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtString;
  end
  else
  if FValueType = efInteger then
  begin
    if IsNumeric(FEdit.Text, false, true) then
      aFilter.Value := StrToInt(FEdit.Text)
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtInteger;
  end
  else
  if FValueType = efFloat then
  begin
    if IsNumeric(FEdit.Text, true, true) then
    begin
      if TryToConvertToDouble(FEdit.Text, tmp) then
        aFilter.Value := tmp
      else
        aFilter.Value := null;
    end
    else
      aFilter.Value := null;
    aFilter.DataType:= fdtFloat;
  end
  else if (FValueType = efDateTime) then
  begin
    if TryToUnderstandDateString(FEdit.Text, tmpDateTime) then
      aFilter.Value:= tmpDateTime
    else
      aFilter.Value:= null;
    aFilter.DataType:= fdtDateTime;
  end
  else if (FValueType = efDate) then
  begin
    if TryToUnderstandDateString(FEdit.Text, tmpDateTime) then
      aFilter.Value:= trunc(tmpDateTime)
    else
      aFilter.Value:= null;
    aFilter.DataType:= fdtDate;
  end
  else
    aFilter.Value := FEdit.Text;
end;

procedure TmEditFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
begin
  Self.Clear;
  inherited ImportFromFilter(aFilter);
  if not VarIsNull(aFilter.Value) then
  begin
    case FValueType of
      efUppercaseString: FEdit.Text := UpperCase(VarToStr(aFilter.Value));
      efInteger: FEdit.Text:= IntToStr(aFilter.Value);
      efFloat: FEdit.Text := FloatToStr(aFilter.Value);
      efUniqueIdentifier: FEdit.Text:= VarToStr(aFilter.Value)
      else
        FEdit.Text:= VarToStr(aFilter.Value);
    end;
  end;
end;

procedure TmEditFilterConditionPanel.SetFilterValue(aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.Clear
  else
  begin
    FEdit.Text := VarToStr(aValue);
    Self.FilterOperator := foEq;
  end;
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

procedure TmComboFilterConditionPanel.SetDefaultItemIndex(AValue: integer);
begin
  if FDefaultItemIndex=AValue then Exit;
  FDefaultItemIndex:=AValue;
  if (FDefaultItemIndex >= 0) and (FCombobox.Items.Count > FDefaultItemIndex) then
    FCombobox.ItemIndex:= FDefaultItemIndex;
end;

procedure TmComboFilterConditionPanel.OnDropDown(Sender: TObject);
begin
  if not FWidthOptimized then
  begin
    OptimalWidth;
    FWidthOptimized := true;
  end;
end;

procedure TmComboFilterConditionPanel.CustomDrawItem(Control: TWinControl; Index: Integer; ARect: Types.TRect; State: StdCtrls.TOwnerDrawState);
var
  tmpRect : TRect;
begin
  (Control as TComboBox).Canvas.FillRect(ARect);                                         //first paint normal background
  tmpRect := ARect;
  InflateRect(tmpRect, -2, 0);
  mGraphicsUtility.WriteText((Control as TComboBox).Canvas, tmpRect, (Control as TComboBox).Items[Index], taLeftJustify, true);
end;

constructor TmComboFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCombobox := TComboBox.Create(Self);
  FCombobox.Parent := Self;
  FCombobox.Align:= alBottom;
  FCombobox.Style:= csOwnerDrawFixed;
  FCombobox.DropDownCount:= 20;
  FCombobox.OnDropDown:= Self.OnDropDown;
  FCombobox.OnDrawItem:= CustomDrawItem();
  FLabel := Self.CreateStandardLabel;
  FGarbage := TObjectList.Create(true);
  FDefaultItemIndex:= -1;
  CreateStandardFilterMenu(FLabel, false);
  FValueType:= cfString;
  FWidthOptimized := false;
end;

destructor TmComboFilterConditionPanel.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

procedure TmComboFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  inherited;
  Self.ApplyFilterCaption(FLabel, aValue, False);
end;

procedure TmComboFilterConditionPanel.ExportToFilter (aFilter : TmFilter);
begin
  inherited ExportToFilter(aFilter);
  if FComboBox.ItemIndex < 0 then
    aFilter.Value := Null
  else
    aFilter.Value := (FComboBox.Items.Objects[FComboBox.ItemIndex] as TVariantObject).Value;

  case FValueType of
    cfInteger: aFilter.DataType:= fdtInteger;
    cfFloat: aFilter.DataType:= fdtFloat;
    cfDate: aFilter.DataType:= fdtDate;
    cfDateTime: aFilter.DataType:= fdtDateTime
  else
    aFilter.DataType:= fdtString;
  end;
end;

procedure TmComboFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
var
  i : integer;
begin
  Self.Clear;
  inherited ImportFromFilter(aFilter);
  if not VarIsNull(aFilter.Value) then
  begin
    for i := 0 to FCombobox.Items.Count - 1 do
    begin
      if CompareVariants((FComboBox.Items.Objects[i] as TVariantObject).Value, aFilter.Value) = 0 then
      begin
        FComboBox.ItemIndex := i;
        break;
      end;
    end;
  end;
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
  OptimalWidth;
end;

procedure TmComboFilterConditionPanel.AddItem(aLabel: String; aValue: Variant);
var
  sh : TVariantObject;
begin
  sh := TVariantObject.Create(aValue);
  FGarbage.Add(sh);
  FComboBox.AddItem(aLabel, sh);
  if (FDefaultItemIndex >= 0) and (FCombobox.Items.Count > FDefaultItemIndex) then
    FCombobox.ItemIndex:= FDefaultItemIndex;
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
    itemWidth := GetTextWidth(FCombobox.Items[idx], FCombobox.Font); // FCombobox.Canvas.TextWidth(FCombobox.Items[idx]);
    Inc(itemWidth, 2 * HORIZONTAL_PADDING);
    itemWidth := ScaleForMagnification(itemWidth, true);
    if (itemWidth > itemsFullWidth) then itemsFullWidth := itemWidth;
  end;

  // set the width of drop down if needed
  if (itemsFullWidth > FCombobox.Width) then
  begin
    //check if there would be a scroll bar
    if FCombobox.DropDownCount < FCombobox.Items.Count then
      itemsFullWidth := itemsFullWidth + {$IFDEF LINUX} 20 {$ELSE} GetSystemMetrics(SM_CXVSCROLL){$ENDIF};

    if FCombobox.Handle <> 0 then
    begin
      {$IFDEF LINUX}
      Perform(FCombobox.Handle, CB_SETDROPPEDWIDTH, itemsFullWidth);
      {$ELSE}
      SendMessage(FCombobox.Handle, CB_SETDROPPEDWIDTH, itemsFullWidth, 0);
      {$ENDIF}
    end;
  end;
end;

procedure TmComboFilterConditionPanel.Clear;
begin
  if (FDefaultItemIndex >= 0) and (FCombobox.Items.Count > FDefaultItemIndex) then
    FCombobox.ItemIndex:= FDefaultItemIndex
  else
  begin
    FCombobox.ItemIndex:= -1;
    FCombobox.Text:= '';
  end;
end;

function TmComboFilterConditionPanel.IsEmpty: boolean;
begin
  Result := (FCombobox.ItemIndex < 0);
end;

{ TmDateFilterConditionPanel }


procedure TmDateFilterConditionPanel.SetFilterOperator(AValue: TmFilterOperator);
begin
  inherited SetFilterOperator(AValue);

  if aValue = foBetween then
  begin
    FDateEditMax.Text:= FDateEditMin.Text;
    FDateEditMax.Visible:= true;
    FDateEditMax.Width:= FBottomPanel.Width div 2;
  end
  else
  begin
    FDateEditMax.Text:= '';
    FDateEditMax.Visible:= false;
  end;
end;

procedure TmDateFilterConditionPanel.SetFlex(AValue: integer);
begin
  inherited SetFlex(AValue);
  if FFilterOperator = foBetween then
    FDateEditMax.Width:= FBottomPanel.Width div 2;
end;

procedure TmDateFilterConditionPanel.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if FFilterOperator = foBetween then
    FDateEditMax.Width:= FBottomPanel.Width div 2;
end;

procedure TmDateFilterConditionPanel.DoOnResize;
begin
  if FFilterOperator = foBetween then
    FDateEditMax.Width:= FBottomPanel.Width div 2;
  inherited DoOnResize;
end;

constructor TmDateFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.BorderStyle:= bsNone;
  FBottomPanel.BevelInner:= bvNone;
  FBottomPanel.BevelOuter:= bvNone;
  FBottomPanel.Parent := Self;
  FBottomPanel.Align := alBottom;

  FDateEditMin := TmDateEdit.Create(Self);
  FDateEditMin.Parent := FBottomPanel;
  FBottomPanel.Height:= FDateEditMin.Height;
  FDateEditMin.Align:= alClient;
  FDateEditMin.Flat:= true;
  FDateEditMin.Height:= ScaleForMagnification(FDateEditMin.Height, true);
  FDateEditMax := TmDateEdit.Create(Self);
  FDateEditMax.Parent := FBottomPanel;
  FDateEditMax.Align:= alRight;
  FDateEditMax.Width:= FBottomPanel.Width div 2;
  FDateEditMax.Visible:= false;
  FDateEditMax.Flat:= false;
  FDateEditMax.Height:= ScaleForMagnification(FDateEditMax.Height, true);
  FLabel := Self.CreateStandardLabel;
  CreateStandardFilterMenu(FLabel, true);
end;

procedure TmDateFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  inherited;
  Self.ApplyFilterCaption(FLabel, aValue);
end;

procedure TmDateFilterConditionPanel.SetFilterValue(aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.Clear
  else
  begin
    Self.FilterOperator := foEq;
    FDateEditMin.Text := VarToStr(aValue);
  end;
end;

procedure TmDateFilterConditionPanel.ExportToFilter (aFilter : TmFilter);
var
  tmpVariant : variant;
begin
  inherited ExportToFilter(aFilter);
  if FDateEditMin.Text = '' then
    aFilter.Value := Null
  else
  begin
    if FilterOperator = foBetween then
    begin
      tmpVariant := variants.VarArrayCreate([0, 1], vardate);
      VarArrayPut(tmpVariant, FDateEditMin.Date, [0]);
      VarArrayPut(tmpVariant, FDateEditMax.Date, [1]);
      aFilter.Value := tmpVariant;
    end
    else
      aFilter.Value := FDateEditMin.Date;
  end;
  aFilter.DataType:= fdtDate;
end;

procedure TmDateFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
var
  tmpList : TDoubleList;
begin
  Self.Clear;

  inherited ImportFromFilter(aFilter);
  if VarIsNull(aFilter.Value) then
  begin
    FDateEditMin.Text:= '';
    FDateEditMax.Text:= '';
  end
  else
  begin
    if aFilter.FilterOperator = foBetween then
    begin
      tmpList := TDoubleList.Create;
      try
        mUtility.ConvertVariantToDateTimeList(aFilter.Value, tmpList);
        if tmpList.Count = 2 then
        begin
          FDateEditMin.Date:= tmpList.Items[0];
          FDateEditMax.Date:= tmpList.Items[1];
        end;
      finally
        tmpList.Free;
      end;
    end
    else
      FDateEditMin.Date:= aFilter.Value;
  end;
end;

function TmDateFilterConditionPanel.IsEmpty: boolean;
begin
  Result := (trim(FDateEditMin.Text) = '');
  if FilterOperator = foBetween then
    Result := Result or (trim(FDateEditMax.Text) = '');
end;

procedure TmDateFilterConditionPanel.Clear;
begin
  FDateEditMin.Text:= '';
  FDateEditMax.Text:= '';
end;

{ TmFilterConditionPanel }

procedure TmFilterConditionPanel.SetFilterOperator(AValue: TmFilterOperator);
begin
  if FFilterOperator=AValue then Exit;
  FFilterOperator:=AValue;
  UpdateCurrentOperatorCheck;
  Self.SetFilterCaption(FCaption);
end;

procedure TmFilterConditionPanel.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  inherited;
  Self.Width := Round(Self.Width * AXProportion);
  Self.Height:= Round(Self.Height * AYProportion);
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
  Result.Font.Size := Round((- Graphics.GetFontData(Result.Font.Handle).Height * 72 / Result.Font.PixelsPerInch));
  ScaleFontForMagnification(Result.Font);
  FStandardLabelFontSize:= Result.Font.Size;
end;

function TmFilterConditionPanel.CreateStandardFilterMenu (aLabel: TLabel; const aAddFilterOperators : boolean) : TPopupMenu;
var
  mi : TMenuItem;
begin
  if not Assigned(FFilterMenu) then
  begin
    FFilterMenu := TPopupMenu.Create(Self);
    aLabel.PopupMenu := FFilterMenu;

    mi := TMenuItem.Create(FFilterMenu);
    mi.Caption := SClearFilterCommand;
    mi.OnClick:= Self.ClearMenuItemClick;
    FFilterMenu.Items.Add(mi);

    if aAddFilterOperators then
    begin
      FOperatorsMenuItemSeparator := TMenuItem.Create(FFilterMenu);
      FOperatorsMenuItemSeparator.Caption:= '-';
      FFilterMenu.Items.Add(FOperatorsMenuItemSeparator);

      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foEq);
      mi.Tag:= PtrInt(foEq);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:=TmFilterOperatorToString(foLike);
      mi.Tag := PtrInt(foLike);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foNotEq);
      mi.Tag:= PtrInt(foNotEq);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foGtOrEq);
      mi.Tag:= PtrInt(foGtOrEq);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foLtOrEq);
      mi.Tag:= PtrInt(foLtOrEq);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foStartWith);
      mi.Tag:= PtrInt(foStartWith);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foEndWith);
      mi.Tag:= PtrInt(foEndWith);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);
      mi := TMenuItem.Create(FFilterMenu);
      mi.Caption:= TmFilterOperatorToString(foBetween);
      mi.Tag:= PtrInt(foBetween);
      mi.OnClick:= OperatorMenuItemClick;
      FFilterMenu.Items.Add(mi);
      FOperatorsMenuItems.Add(mi);

      UpdateCurrentOperatorCheck;
    end;
    FFilterMenu.OnPopup:= FilterMenuPopup;
  end;

  Result := FFilterMenu;
end;

procedure TmFilterConditionPanel.ApplyFilterCaption (aLabel : TLabel; const aValue : String; const aShowOperator: boolean= true);
var
  s : String;
begin
  if not AnsiEndsText(':', aValue) then
    s := aValue + ':'
  else
    s := aValue;
  if aShowOperator and (FFilterOperator <> foUnknown) and (FFilterOperator <> foEq) and (SizeOf(FAllowedOperators) > 1) then
    s := s + ' [' + TmFilterOperatorToString(Self.FFilterOperator) + ']';

  aLabel.Caption:= s;
  aLabel.Font.Size:= FStandardLabelFontSize;
  if GetTextWidth(s, aLabel.Font) > aLabel.Width then
    aLabel.Font.Size:= trunc(FStandardLabelFontSize * 0.8);
end;

procedure TmFilterConditionPanel.UpdateCurrentOperatorCheck;
var
  i : integer;
begin
  if Assigned(FFilterMenu) then
  begin
    for i := 0 to FOperatorsMenuItems.Count - 1 do
      TMenuItem(FOperatorsMenuItems.Items[i]).Checked:= TMenuItem(FOperatorsMenuItems.Items[i]).Tag = PtrInt(FFilterOperator);
  end;
end;

procedure TmFilterConditionPanel.OperatorMenuItemClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
    Self.FilterOperator := TmFilterOperator((Sender as TMenuItem).Tag);
end;

procedure TmFilterConditionPanel.ClearMenuItemClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
    Self.Clear;
end;

procedure TmFilterConditionPanel.FilterMenuPopup(Sender: TObject);
var
  i : integer;
  almostOne : boolean;
begin
  if Assigned(FFilterMenu) and (FFilterMenu.Items.Count > 2) then
  begin
    almostOne:= false;
    for i := 0 to FOperatorsMenuItems.Count - 1 do
    begin
      TMenuItem(FOperatorsMenuItems.Items[i]).Visible:= (TmFilterOperator(TMenuItem(FOperatorsMenuItems.Items[i]).Tag) in FAllowedOperators);
      almostOne:= almostOne or TMenuItem(FOperatorsMenuItems.Items[i]).Visible;
    end;
    // show or hide the separator:
    FOperatorsMenuItemSeparator.Visible := almostOne;
  end;
end;

constructor TmFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.FFlex := 2;
  Self.Width := Self.FFlex * ScaleForMagnification(DEFAULT_FLEX_WIDTH, true);
  Self.Caption := '';
  Self.Height := ScaleForMagnification(DEFAULT_HEIGHT, true);
  Self.FFilterOperator:= foUnknown;
  Self.FAllowedOperators:= [];
  FOperatorsMenuItems := TList.Create;
end;

destructor TmFilterConditionPanel.Destroy;
begin
  FOperatorsMenuItems.Free;
  inherited Destroy;
end;

procedure TmFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FCaption := aValue;
end;

procedure TmFilterConditionPanel.ExportToFilter(aFilter: TmFilter);
begin
  aFilter.FieldName := Self.FieldName;
  aFilter.FilterOperator := Self.FilterOperator;
end;

procedure TmFilterConditionPanel.ImportFromFilter(const aFilter: TmFilter);
begin
  Self.FilterOperator := aFilter.FilterOperator;
end;

end.
