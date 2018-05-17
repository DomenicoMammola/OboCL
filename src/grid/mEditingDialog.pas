// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// This can be uses as a workaround for this problem:
// https://forum.lazarus.freepascal.org/index.php?topic=16308.0
//
unit mEditingDialog;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Forms, ValEdit, Graphics, Grids, contnrs, ExtCtrls,
  SysUtils, variants, StdCtrls, Buttons,
  oMultiPanelSetup, OMultiPanel,
  mGridEditors, mMaps, mCalendarDialog, mUtility, mMathUtility, mLookupFormForDataset,
  mQuickReadOnlyVirtualDataSet, mVirtualDataSet, mVirtualFieldDefs, mNullables,
  mISO6346Utility, mVirtualDataSetInterfaces, mBooleanDataProvider;

resourcestring
  SPropertyColumnTitle = 'Property';
  SValueColumnTitle = 'Value';
  SMissingValuesTitle = 'Missing values';
  SMissingValuesWarning = 'Something is wrong, some mandatory values are missing:';
  SDefaultCaption = 'Edit values';
  SErrorNotADate = 'Not a date.';
  SErrorNotANumber = 'Not a number.';
  SErrorNotATime = 'Not a time.';

type

  TmEditingPanelEditorKind = (ekInteger, ekFloat, ekDate, ekTime, ekLookupText, ekLookupInteger, ekLookupFloat, ekText, ekUppercaseText, ekContainerNumber, ekMRNNumber);

  TmEditingPanel = class;

  TmOnEditValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aNewDisplayValue: string; const aNewActualValue : variant) of object;
  TmOnValidateValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant) of object;
  TmOnInitProviderForLookupEvent = procedure (aSender : TmEditingPanel; const aName : string; aDatasetProvider : TReadOnlyVirtualDatasetProvider; aFieldsList : TStringList; out aKeyFieldName, aDisplayLabelFieldName: string) of object;
  TmOnGetValueFromLookupKeyValueEvent = procedure (aSender : TmEditingPanel; const aName : string; var aLookupValue: variant; var aDisplayValue: string) of object;

  { TmEditorLineDataProvider }

  TmEditorLineDataProvider = class
  strict private
    FDataProvider: IVDListDataProvider;
    FVirtualFieldDefs: TmVirtualFieldDefs;
    FKeyFieldName: string;
    FDisplayLabelFieldName: string;
    FFields : TStringList;
    FBooleanProvider : TBooleanDataProvider;
    function GetUseBooleanProvider: boolean;
    procedure SetUseBooleanProvider(AValue: boolean);
  private
    class procedure ExtractFields(aVirtualFields: TmVirtualFieldDefs; aList: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    property DataProvider : IVDListDataProvider read FDataProvider write FDataProvider;
    property VirtualFieldDefs : TmVirtualFieldDefs read FVirtualFieldDefs;
    property KeyFieldName: string read FKeyFieldName write FKeyFieldName;
    property DisplayLabelFieldName: string read FDisplayLabelFieldName write FDisplayLabelFieldName;
    property Fields : TStringList read FFields;
    property UseBooleanProvider : boolean read GetUseBooleanProvider write SetUseBooleanProvider;
  end;

  { TmValueListEditor }

  TmValueListEditor = class(TValueListEditor)
  protected
    Function EditingAllowed(ACol : Integer = -1) : Boolean; override;
  end;

  { TmEditingPanel }

  TmEditingPanel = class(TCustomPanel)
  strict private
    FRootPanel : TOMultiPanel;
    FValueListEditor: TmValueListEditor;
    FCustomDateEditor : TmExtDialogCellEditor;
    FCustomEditor : TmExtDialogCellEditor;
    FCustomButtonEditor : TmExtButtonTextCellEditor;
    FLinesByName : TmStringDictionary;
    FLinesByRowIndex : TmIntegerDictionary;
    FMemosByName : TmStringDictionary;
    FLines : TObjectList;
    FMemos : TObjectList;
    FOnEditValueEvent: TmOnEditValueEvent;
    FOnValidateValueEvent: TmOnValidateValueEvent;
    FOnInitProviderForLookupEvent: TmOnInitProviderForLookupEvent;
    FOnGetValueFromLookupKeyValueEvent: TmOnGetValueFromLookupKeyValueEvent;
    FMultiEditMode : boolean;
    FSomethingChanged : boolean;

    function GetAlternateColor: TColor;
    procedure SetAlternateColor(AValue: TColor);

    procedure OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnValueListEditorSelectEditor(Sender: TObject; aCol,  aRow: Integer; var Editor: TWinControl);
    procedure OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    function OnShowDialog  (const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
    function OnValueListEditorClearValue (const aCol, aRow: integer): boolean;
    function ComposeCaption (const aCaption : string; const aMandatory : boolean): string;
    function GetValueFromMemo (const aName : string; const aTrimValue : boolean) : string;
  protected
    FCommitted : boolean;
  protected
    procedure ExtractFields (aVirtualFields : TmVirtualFieldDefs; aList : TStringList);
    procedure SetReadOnly (const aName : string; const aValue : boolean); overload;
    procedure SetReadOnly (const aValue : boolean); overload;
    // override these or use events (don't mix overrides and events!):
    procedure InternalOnEditValue(const aName : string; const aNewDisplayValue : variant; const aNewActualValue: variant); virtual;
    procedure InternalOnValidateValue(const aName : string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant); virtual;
    procedure InternalInitProviderForLookup (const aName : string; aDatasetProvider : TReadOnlyVirtualDatasetProvider; aFieldsList : TStringList;
      out aKeyFieldName, aDisplayLabelFieldName: string); virtual;
//    procedure InternalGetValueFromLookupKeyValue (const aName : string; const aLookupValue: variant; out aDisplayValue : string; out aActualValue: variant); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocusInEditor;
    function CheckMandatoryLines(var aMissingValues: string): boolean;
    procedure CommitChanges;

    procedure AddLine (const aName : string; const aCaption : string; const aDefaultDisplayValue : string; const aDefaultActualValue: variant; const aEditorKind : TmEditingPanelEditorKind; const aReadOnly : boolean = false; const aMandatory: boolean = false; const aChangedValueDestination : TAbstractNullable = nil);
    procedure AddLineForNullable (const aName: string; const aCaption: String; aValue: TAbstractNullable; const aEditorKind: TmEditingPanelEditorKind; const aReadOnly, aMandatory : boolean; const aDisplayValue : variant);

    procedure AddMemo (const aName : string; const aCaption : string; const aDefaultValue : string; const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil);
    function GetValue(const aName : string) : Variant;
    procedure SetValue(const aName : string; const aDisplayValue: String; const aActualValue: variant);
    function GetEditorKind(const aName : string): TmEditingPanelEditorKind;
    function GetDataProviderForLine (const aName : string): TmEditorLineDataProvider;

    property AlternateColor : TColor read GetAlternateColor write SetAlternateColor;
    property OnEditValue: TmOnEditValueEvent read FOnEditValueEvent write FOnEditValueEvent;
    property OnValidateValue: TmOnValidateValueEvent read FOnValidateValueEvent write FOnValidateValueEvent;
    property OnInitProviderForLookup: TmOnInitProviderForLookupEvent read FOnInitProviderForLookupEvent write FOnInitProviderForLookupEvent;
    property OnGetValueFromLookupKeyValue: TmOnGetValueFromLookupKeyValueEvent read FOnGetValueFromLookupKeyValueEvent write FOnGetValueFromLookupKeyValueEvent;

    property MultiEditMode : boolean read FMultiEditMode write FMultiEditMode;
    property SomethingChanged : boolean read FSomethingChanged;
  end;

  { TmEditingForm }

  TmEditingForm = class (TCustomForm)
  strict private
    FBottomPanel: TPanel;
    FCancelBtn: TBitBtn;
    FOkBtn: TBitBtn;
    FEditingPanel : TmEditingPanel;
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  public
    property EditingPanel: TmEditingPanel read FEditingPanel;
  end;



implementation

uses
  Dialogs,
  mToast;

type

  { TEditorLine }

  TEditorLine = class
  private
    Name: String;
    Caption: String;
    EditorKind: TmEditingPanelEditorKind;
    Index: integer;
    ReadOnly: boolean;
    Mandatory: boolean;
    ActualValue: variant;
    ChangedValueDestination: TAbstractNullable;
    Changed: boolean;
    LineDataProvider: TmEditorLineDataProvider;
  public
    constructor Create;
    destructor Destroy; override;

    function RowIndex : integer;
  end;

  TEditorMemo = class
  private
    Name : String;
    Memo : TMemo;
    ChangedValueDestination: TAbstractNullable;
  end;

{ TmEditorLineDataProvider }

function TmEditorLineDataProvider.GetUseBooleanProvider: boolean;
begin
  Result := Assigned(FBooleanProvider);
end;

procedure TmEditorLineDataProvider.SetUseBooleanProvider(AValue: boolean);
begin
  if aValue then
  begin
    if not Assigned(FBooleanProvider) then
      FBooleanProvider:= TBooleanDataProvider.Create;
    FDataProvider := FBooleanProvider;
    FKeyFieldName := TBooleanDatum.GetKeyField;
    FDisplayLabelFieldName:= FKeyFieldName;
    TBooleanDatum.FillVirtualFieldDefs(FVirtualFieldDefs, '');
    ExtractFields(FVirtualFieldDefs, FFields);
  end
  else
  begin
    if Assigned(FDataProvider) and Assigned(FBooleanProvider) then
      FDataProvider := nil;
    FreeAndNil(FBooleanProvider);
  end;
end;

class procedure TmEditorLineDataProvider.ExtractFields(aVirtualFields: TmVirtualFieldDefs; aList: TStringList);
var
  i : integer;
begin
  aList.Clear;
  for i := 0 to aVirtualFields.Count -1 do
    aList.Add(aVirtualFields.VirtualFieldDefs[i].Name);
end;

constructor TmEditorLineDataProvider.Create;
begin
  FVirtualFieldDefs := TmVirtualFieldDefs.Create;
  FDataProvider := nil;
  FKeyFieldName := '';
  FDisplayLabelFieldName := '';
  FFields := TStringList.Create;
  FBooleanProvider := nil;
end;

destructor TmEditorLineDataProvider.Destroy;
begin
  FFields.Free;
  FVirtualFieldDefs.Free;
  FreeAndNil(FBooleanProvider);
  inherited Destroy;
end;

constructor TEditorLine.Create;
begin
  LineDataProvider := TmEditorLineDataProvider.Create;
  Name:= '';
  Caption:= '';
  Index:= 0;
  ReadOnly:= false;
  Mandatory:= false;
  ActualValue:= Null;
  ChangedValueDestination:= nil;
  Changed:= false;
end;

destructor TEditorLine.Destroy;
begin
  LineDataProvider.Free;
  inherited Destroy;
end;

function TEditorLine.RowIndex: integer;
begin
  Result := Index + 1;
end;

{ TmValueListEditor }

function TmValueListEditor.EditingAllowed(ACol: Integer): Boolean;
begin
  if ACol = 0 then
    Result := false
  else
    Result:=inherited EditingAllowed(ACol);
end;

{ TmEditingForm }

procedure TmEditingForm.FormShow(Sender: TObject);
begin
  if FOkBtn.Visible then
  begin
    FOkBtn.SetFocus;
    FEditingPanel.SetFocusInEditor;
  end
  else
  begin
    FCancelBtn.SetFocus;
  end;
end;

procedure TmEditingForm.OkBtnClick(Sender: TObject);
var
  missingValues: string;
begin
  if FOkBtn.Focused then
  begin
    if not FEditingPanel.CheckMandatoryLines(missingValues) then
    begin
      MessageDlg(SMissingValuesTitle, SMissingValuesWarning + sLineBreak + missingValues , mtInformation, [mbOK],0);
      exit;
    end;

    FEditingPanel.CommitChanges;

    ModalResult := mrOk;
  end;
end;

constructor TmEditingForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);

  Self.Height:= 550;
  Self.Width:= 800;
  //Self.BorderStyle:= bsDialog;
  Self.OnShow:= FormShow;
  Self.Caption:= SDefaultCaption;
  Self.Position:= poMainFormCenter;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := Self;
  FBottomPanel.Align:= alBottom;
  FBottomPanel.Height:= 50;
  FBottomPanel.BevelInner:= bvNone;
  FBottomPanel.BevelOuter:= bvNone;

  FOkBtn:= TBitBtn.Create(FBottomPanel);
  FOkBtn.Kind:= bkOK;

  FOkBtn.Width := 75;
  FOkBtn.Height := 30;
  FOkBtn.Parent:= FBottomPanel;
  FOkBtn.Left := 0; // Self.Width - 150 - 30;
  FOkBtn.Top := 8;
  FOkBtn.Anchors:= [akTop, akRight];
  FOkBtn.DefaultCaption:= true;
  FOkBtn.OnClick:= OkBtnClick;
  FOkBtn.ModalResult:= mrNone;

  FCancelBtn:= TBitBtn.Create(FBottomPanel);
  FCancelBtn.Kind:= bkCancel;

  FCancelBtn.Width := 75;
  FOkBtn.Height := 30;
  FCancelBtn.Parent:= FBottomPanel;
  FCancelBtn.Left := 80; //Self.Width - 75 - 15;
  FCancelBtn.Top := 8;
  FCancelBtn.Anchors:= [akTop, akRight];
  FCancelBtn.DefaultCaption:= true;
  FCancelBtn.OnClick:= OkBtnClick;
  FCancelBtn.ModalResult:= mrCancel;

  FEditingPanel := TmEditingPanel.Create(Self);
  FEditingPanel.Parent := Self;
  FEditingPanel.Align:= alClient;

end;

{ TmEditingPanel }

function TmEditingPanel.GetAlternateColor: TColor;
begin
  Result := FValueListEditor.AlternateColor;
end;

procedure TmEditingPanel.SetAlternateColor(AValue: TColor);
begin
  FValueListEditor.AlternateColor:= aValue;
end;

procedure TmEditingPanel.OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if (aCol = 0) or (aRow = 0) then
  begin
    FValueListEditor.Canvas.Font.Style := FValueListEditor.Canvas.Font.Style + [fsBold];
    if MultiEditMode and (aRow > 0) then
    begin
      if not (FLinesByRowIndex.Find(aRow) as TEditorLine).Changed then
        FValueListEditor.Canvas.Font.Color:= clGray
      else
        FValueListEditor.Canvas.Font.Color:= clBlack;
    end;
  end;
end;

procedure TmEditingPanel.OnValueListEditorSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  curLine : TEditorLine;
begin
  if aCol <> 1 then
    exit;

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;
  if (not Assigned(curLine)) or curLine.ReadOnly then
    exit;

  if (curLine.EditorKind = ekDate) then
  begin
    FCustomDateEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomDateEditor;
  end
  else if (curLine.EditorKind = ekLookupText) or (curLine.EditorKind = ekLookupFloat) or (curLine.EditorKind = ekLookupInteger) then
  begin
//    FCustomEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
//    Editor := FCustomEditor;
    FCustomButtonEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomButtonEditor;
  end;
end;

procedure TmEditingPanel.OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  vDate : TDate;
  curLine : TEditorLine;
  tmpDouble : Double;
  errorMessage : String;
  oldActualValue : variant;
begin
  NewValue := trim(NewValue);

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  oldActualValue := curLine.ActualValue;

  if curLine.ReadOnly then
  begin
    NewValue := OldValue;
    exit;
  end;

  if curLine.EditorKind = ekDate then
  begin
    vDate := 0;
    if NewValue <> '' then
    begin
      if TryToUnderstandDateString(NewValue, vDate) then
      begin
        NewValue := DateToStr(vDate);
        curLine.ActualValue := vDate;
      end
      else
      begin
        NewValue := OldValue;
        TmToast.ShowText(SErrorNotADate);
      end;
    end
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekTime then
  begin
    vDate := 0;
    if NewValue <> '' then
    begin
      if TryToUnderstandTimeString(NewValue, vDate) then
      begin
        NewValue := TimeToStr(vDate);
        curLine.ActualValue := vDate;
      end
      else
      begin
        NewValue := OldValue;
        TmToast.ShowText(SErrorNotATime);
      end;
    end
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekInteger then
  begin
    if NewValue <> '' then
    begin
      if not IsNumeric(NewValue, false) then
      begin
        NewValue := OldValue;
        TmToast.ShowText(SErrorNotANumber);
      end
      else
      begin
        curLine.ActualValue:= StrToInt(NewValue);
      end;
    end
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekFloat then
  begin
    if NewValue <> '' then
    begin
      if TryToConvertToDouble(NewValue, tmpDouble) then
      begin
        NewValue := FloatToStr(tmpDouble);
        curLine.ActualValue:= tmpDouble;
      end
      else
      begin
        NewValue := OldValue;
        TmToast.ShowText(SErrorNotANumber);
      end;
    end
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekUppercaseText then
  begin
    NewValue := Uppercase(NewValue);
    if NewValue <> '' then
      curLine.ActualValue:= NewValue
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekText then
  begin
    if NewValue <> '' then
      curLine.ActualValue:= NewValue
    else
      curLine.ActualValue:= null;
  end else if curLine.EditorKind = ekContainerNumber then
  begin
    if NewValue <> '' then
    begin
      NewValue := Uppercase(NewValue);
      if not mISO6346Utility.IsContainerCodeValid(NewValue, errorMessage) then
      begin
        NewValue := OldValue;
        TmToast.ShowText(errorMessage);
      end
      else
      begin
        curLine.ActualValue:= NewValue;
      end;
    end
    else
    begin
      curLine.ActualValue:= null;
    end;
  end else if curLine.EditorKind = ekMRNNumber then
  begin
    if NewValue <> '' then
    begin
      NewValue := Uppercase(NewValue);
      if not mISO6346Utility.IsMRNCodeValid(NewValue, errorMessage) then
      begin
        NewValue := OldValue;
        TmToast.ShowText(errorMessage);
      end
      else
      begin
        curLine.ActualValue:= NewValue;
      end;
    end;
  end;

  Self.InternalOnValidateValue(curLine.Name, OldValue, NewValue, oldActualValue, curLine.ActualValue);

  if NewValue <> OldValue then
  begin
    Self.InternalOnEditValue(curLine.Name, NewValue, curLine.ActualValue);
    curLine.Changed := curLine.Changed or (NewValue <> OldValue);
  end;
end;

function TmEditingPanel.OnShowDialog(const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
var
  calendarFrm : TmCalendarDialog;
  str, tmpKeyFieldName, tmpDisplayLabelFieldName : String;
  curLine : TEditorLine;
  lookupFrm : TmLookupWindow;
  tmpDatasetProvider : TReadOnlyVirtualDatasetProvider;
  tmpDataset : TmVirtualDataset;
  tmpFieldsList : TStringList;
begin
  Result := false;

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if curLine.ReadOnly then
    exit;

  if curLine.EditorKind = ekDate then
  begin
    calendarFrm := TmCalendarDialog.Create;
    try
      if FValueListEditor.Cells[aCol, aRow] <> '' then
      begin
        try
          calendarFrm.Date := StrToDate(FValueListEditor.Cells[aCol, aRow]);
        except
          // ignored
        end;
      end;
      if calendarFrm.Execute then
      begin
        str := DateToStr(calendarFrm.Date);
        FValueListEditor.Cells[aCol, aRow] := str;
        aNewDisplayValue := str;
        aNewActualValue := calendarFrm.Date;
        curLine.ActualValue:= calendarFrm.Date;
//        Self.InternalOnEditValue(curLine.Name, str, calendarFrm.Date);
        Result := true;
      end;
    finally
      calendarFrm.Free;
    end;
  end
  else if (curLine.EditorKind = ekLookupText) or (curLine.EditorKind = ekLookupInteger) or (curLine.EditorKind = ekLookupFloat) then
  begin
    tmpFieldsList := TStringList.Create;
    try
      lookupFrm := TmLookupWindow.Create(Self);
      try
        tmpDatasetProvider := TReadOnlyVirtualDatasetProvider.Create;
        tmpDataset := TmVirtualDataset.Create(Self);
        try
          tmpDataset.DatasetDataProvider := tmpDatasetProvider;

          InternalInitProviderForLookup(curLine.Name, tmpDatasetProvider, tmpFieldsList, tmpKeyFieldName, tmpDisplayLabelFieldName);

          tmpDataset.Active:= true;
          tmpDataset.Refresh;
          lookupFrm.Init(tmpDataset, tmpFieldsList, tmpKeyFieldName, tmpDisplayLabelFieldName);
          if lookupFrm.ShowModal = mrOk then
          begin
            aNewDisplayValue:= lookupFrm.SelectedDisplayLabel;
            aNewActualValue:= lookupFrm.SelectedValue;
            if Assigned(FOnGetValueFromLookupKeyValueEvent) then
              FOnGetValueFromLookupKeyValueEvent(Self, curLine.Name, aNewActualValue, aNewDisplayValue);

            FValueListEditor.Cells[aCol, aRow] := aNewDisplayValue;
            curLine.ActualValue:= aNewActualValue;
            Result := true;
          end;
        finally
          tmpDataset.Free;
          tmpDatasetProvider.Free;
        end;

      finally
        lookupFrm.Free;
      end;
    finally
      tmpFieldsList.Free;
    end;
  end;
end;

function TmEditingPanel.OnValueListEditorClearValue(const aCol, aRow: integer): boolean;
var
  curLine: TEditorLine;
begin
  Result := false;
  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if curLine.ReadOnly then
    exit;

  if MultiEditMode then
    curLine.Changed := false
  else
    curLine.Changed:= curLine.Changed and (FValueListEditor.Rows[curLine.Index + 1].Strings[1] <> '');

  FValueListEditor.Rows[curLine.Index + 1].Strings[1] := '';
  curLine.ActualValue:= null;
  Result := true;
end;

function TmEditingPanel.ComposeCaption(const aCaption: string;
  const aMandatory: boolean): string;
begin
  if aMandatory then
    Result := aCaption + ' *'
  else
    Result := aCaption;
end;

procedure TmEditingPanel.AddLine(const aName: string; const aCaption: string; const aDefaultDisplayValue: string; const aDefaultActualValue: variant; const aEditorKind : TmEditingPanelEditorKind; const aReadOnly : boolean = false; const aMandatory: boolean = false; const aChangedValueDestination : TAbstractNullable=nil);
var
  tmp : TEditorLine;
begin
  tmp := TEditorLine.Create;
  FLines.Add(tmp);
  FLinesByName.Add(aName, tmp);
  tmp.EditorKind:= aEditorKind;
  tmp.Name := aName;
  tmp.Caption := aCaption;
  tmp.Index:= FValueListEditor.InsertRow(ComposeCaption(aCaption, aMandatory), aDefaultDisplayValue, true);
  tmp.ActualValue:= aDefaultActualValue;
  tmp.ReadOnly:= aReadOnly;
  tmp.Mandatory:= aMandatory;
  tmp.ChangedValueDestination := aChangedValueDestination;
  FLinesByRowIndex.Add(tmp.RowIndex, tmp);
  FValueListEditor.ItemProps[tmp.Index].ReadOnly:= tmp.ReadOnly;
  if (not tmp.ReadOnly) and ((aEditorKind = ekDate) or (aEditorKind = ekLookupFloat) or (aEditorKind = ekLookupInteger) or (aEditorKind = ekLookupText)) then
    FValueListEditor.ItemProps[tmp.Index].EditStyle:=esEllipsis;
end;

procedure TmEditingPanel.AddLineForNullable(const aName: string; const aCaption: String; aValue: TAbstractNullable;
  const aEditorKind: TmEditingPanelEditorKind; const aReadOnly, aMandatory: boolean; const aDisplayValue : variant);
var
  tmp : TEditorLine;
  str : string;
begin
  tmp := TEditorLine.Create;
  FLines.Add(tmp);
  FLinesByName.Add(aName, tmp);
  tmp.EditorKind:= aEditorKind;
  tmp.Name := aName;
  tmp.Caption := aCaption;
  if VarIsNull(aDisplayValue) then
  begin
    if (aEditorKind = ekDate) and (aValue is TNullableDateTime) then
      str := (aValue as TNullableDateTime).AsString(false)
    else if (aEditorKind = ekTime) and (aValue is TNullableTime) then
      str := (aValue as TNullableTime).AsString
    else
      str := aValue.AsString;
  end
  else
    str := VarToStr(aDisplayValue);
  tmp.Index:= FValueListEditor.InsertRow(ComposeCaption(aCaption, aMandatory), str, true);
  tmp.ActualValue:= aValue.AsVariant;
  tmp.ReadOnly:= aReadOnly;
  tmp.Mandatory:= aMandatory;
  tmp.ChangedValueDestination := aValue;
  FLinesByRowIndex.Add(tmp.RowIndex, tmp);
  FValueListEditor.ItemProps[tmp.Index].ReadOnly:= tmp.ReadOnly;
  if (not tmp.ReadOnly) and ((aEditorKind = ekDate) or (aEditorKind = ekLookupFloat) or (aEditorKind = ekLookupInteger) or (aEditorKind = ekLookupText)) then
    FValueListEditor.ItemProps[tmp.Index].EditStyle:=esEllipsis;
end;

procedure TmEditingPanel.AddMemo(const aName: string; const aCaption: string;const aDefaultValue: string; const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil);
var
  tmpPanel1, tmpPanel2 : TPanel;
  tmpMemo : TMemo;
  i : integer;
  position : double;
  tmpEditorMemo : TEditorMemo;
begin
  tmpPanel1 := TPanel.Create(FRootPanel);
  tmpPanel1.Parent := FRootPanel;
  tmpPanel1.BevelInner:= bvNone;
  tmpPanel1.BevelOuter:= bvNone;
  FRootPanel.PanelCollection.AddControl(tmpPanel1);

  tmpPanel2 := TPanel.Create(tmpPanel1);
  tmpPanel2.Parent := tmpPanel1;
  tmpPanel2.Align:= alLeft;
  tmpPanel2.BevelInner:= bvNone;
  tmpPanel2.BevelOuter:= bvNone;
  tmpPanel2.Width:= FValueListEditor.DefaultColWidth;
  tmpPanel2.Caption:= aCaption;
  tmpPanel2.Font.Style:=[fsBold];
  tmpPanel2.BorderWidth:= 1;
  tmpPanel2.BorderStyle:=bsSingle;
  tmpMemo:= TMemo.Create(tmpPanel1);
  tmpMemo.Parent := tmpPanel1;
  tmpMemo.Align:= alClient;
  tmpMemo.ScrollBars:= ssVertical;
  tmpMemo.WantReturns:= true;
  tmpMemo.Text:= aDefaultValue;

  tmpEditorMemo := TEditorMemo.Create;
  tmpEditorMemo.Name:= aName;
  tmpEditorMemo.Memo := tmpMemo;
  tmpEditorMemo.ChangedValueDestination := aChangedValueDestination;

  FMemos.Add(tmpEditorMemo);
  FMemosByName.Add(aName, tmpEditorMemo);

  FRootPanel.PanelCollection.Items[FRootPanel.PanelCollection.Count - 1].Position:= 1;
  position := 1 - aMemoHeightPercent;
  for i := FRootPanel.PanelCollection.Count -2 downto 0 do
  begin
    FRootPanel.PanelCollection.Items[i].Position := position;
    position := position - aMemoHeightPercent;
  end;
end;

procedure TmEditingPanel.ExtractFields(aVirtualFields: TmVirtualFieldDefs; aList: TStringList);
begin
  TmEditorLineDataProvider.ExtractFields(aVirtualFields, aList);
end;

function TmEditingPanel.GetValue(const aName: string): Variant;
var
  curLine : TEditorLine;
  tmp : string;
begin
  Result := Null;

  if FLinesByName.Contains(aName) then
  begin
    curLine := FLinesByName.Find(aName) as TEditorLine;
    Result := curLine.ActualValue;
  end
  else if FMemosByName.Contains(aName) then
  begin
    tmp := GetValueFromMemo(aName, true);
    if tmp <> '' then
      Result := tmp;
  end;
end;

function TmEditingPanel.GetEditorKind(const aName: string): TmEditingPanelEditorKind;
var
  curLine: TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  if Assigned(curLine) then
    Result := curLine.EditorKind
  else
    raise Exception.Create('Unknown editor line:' + aName);
end;

function TmEditingPanel.GetDataProviderForLine(const aName: string): TmEditorLineDataProvider;
var
  curLine: TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  if Assigned(curLine) then
    Result := curLine.LineDataProvider
  else
    Result := nil;
end;

procedure TmEditingPanel.SetValue(const aName: string; const aDisplayValue: String; const aActualValue: variant);
var
  curLine: TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  if Assigned(curLine) then
  begin
    curLine.Changed := curLine.Changed or (aDisplayValue <> FValueListEditor.Rows[curLine.Index + 1].Strings[1]);
    FValueListEditor.Rows[curLine.Index + 1].Strings[1] := aDisplayValue;
    curLine.ActualValue:= aActualValue;
  end;
end;


procedure TmEditingPanel.SetReadOnly(const aName: string; const aValue : boolean);
var
  tmpObj : TObject;
begin
  (FLinesByName.Find(aName) as TEditorLine).ReadOnly:= aValue;

  tmpObj := FMemosByName.Find(aName);
  if Assigned(tmpObj) then
    (tmpObj as TEditorMemo).Memo.ReadOnly:= aValue
  else
    FValueListEditor.ItemProps[aName].ReadOnly := aValue;
end;

procedure TmEditingPanel.SetReadOnly(const aValue: boolean);
var
  i : integer;
  tmp : TItemProp;
begin
  for i := 0 to FLines.Count - 1 do
    (FLines.Items[i] as TEditorLine).ReadOnly:= aValue;
  for i := 0 to FMemos.Count - 1 do
    (FMemos.Items[i] as TEditorMemo).Memo.ReadOnly:= aValue;
  for i := 0 to FValueListEditor.Strings.Count -1 do
  begin
    tmp := (FValueListEditor.ItemProps[i]);
    if Assigned(tmp) then
      tmp.ReadOnly := aValue;
  end;
end;

function TmEditingPanel.GetValueFromMemo(const aName: string; const aTrimValue : boolean): string;
var
  i : integer;
  sep, s : string;
  tmpMemo : TMemo;
begin
  tmpMemo := (FMemosByName.Find(aName) as TEditorMemo).Memo;
  Result := '';
  sep := '';
  for i := 0 to tmpMemo.Lines.Count -1 do
  begin
    Result := Result + sep + tmpMemo.Lines[i];
    sep := Chr(13);
  end;
  if aTrimValue then
    Result := Trim(Result);
  s := StringReplace(Result, Chr(13), '', [rfReplaceAll]);
  if Length(s) = 0 then
    Result := '';
end;

procedure TmEditingPanel.InternalOnEditValue(const aName: string; const aNewDisplayValue : variant; const aNewActualValue: variant);
begin
  if Assigned(FOnEditValueEvent) then
    FOnEditValueEvent(Self, aName, aNewDisplayValue, aNewActualValue);
end;

procedure TmEditingPanel.InternalOnValidateValue(const aName: string; const aOldDisplayValue: String; var aNewDisplayValue: String; const aOldActualValue: Variant; var aNewActualValue: variant);
begin
  if Assigned(FOnValidateValueEvent) then
    FOnValidateValueEvent(Self, aName, aOldDisplayValue, aNewDisplayValue, aOldActualValue, aNewActualValue);
end;

procedure TmEditingPanel.InternalInitProviderForLookup(const aName : string; aDatasetProvider: TReadOnlyVirtualDatasetProvider; aFieldsList: TStringList; out aKeyFieldName, aDisplayLabelFieldName: string);
var
  curLine: TEditorLine;
begin
  aKeyFieldName := '';
  aDisplayLabelFieldName := '';
  if Assigned(FOnInitProviderForLookupEvent) then
    FOnInitProviderForLookupEvent(Self, aName, aDatasetProvider, aFieldsList, aKeyFieldName, aDisplayLabelFieldName)
  else
  begin
    curLine := FLinesByName.Find(aName) as TEditorLine;
    if Assigned(curLine) and Assigned(curLine.LineDataProvider.DataProvider) then
    begin
      aFieldsList.AddStrings(curLine.LineDataProvider.Fields);
      aKeyFieldName:= curLine.LineDataProvider.KeyFieldName;
      aDisplayLabelFieldName:= curLine.LineDataProvider.DisplayLabelFieldName;
      aDatasetProvider.Init(curLine.LineDataProvider.DataProvider);
      aDatasetProvider.VirtualFieldDefs.Assign(curLine.LineDataProvider.VirtualFieldDefs);
    end;
  end;
end;

(*
procedure TmEditingPanel.InternalGetValueFromLookupKeyValue(const aName: string; const aLookupValue: variant; out aDisplayValue : string; out aActualValue: variant);
begin
  if Assigned(FOnGetValueFromLookupKeyValueEvent) then
    FOnGetValueFromLookupKeyValueEvent(aName, aLookupValue, aDisplayValue, aActualValue)
  else
  begin
    aDisplayValue:= aLookupValueAsString;
    aActualValue:= aLookupValue;
  end;
end;*)

constructor TmEditingPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.BorderStyle:= bsNone;
  Self.Caption:= '';
  FRootPanel := TOMultiPanel.Create(Self);
  FRootPanel.Parent := Self;
  FRootPanel.PanelType:= ptVertical;
  FRootPanel.Align:= alClient;

  FCommitted:= false;
  FMultiEditMode:= false;
  FSomethingChanged:= false;

  FValueListEditor:= TmValueListEditor.Create(FRootPanel);
  FValueListEditor.Parent := FRootPanel;
  FValueListEditor.Align:= alClient;
  FRootPanel.PanelCollection.AddControl(FValueListEditor);
  FValueListEditor.Height:= 200;
  FValueListEditor.AlternateColor := clMoneyGreen;
  FValueListEditor.AutoAdvance := aaDown;
  FValueListEditor.DefaultColWidth := 230;
  FValueListEditor.FixedCols := 0;
  FValueListEditor.Flat := True;
  FValueListEditor.RowCount := 2;
  FValueListEditor.TabOrder := 0;
  FValueListEditor.OnPrepareCanvas := Self.OnValueListEditorPrepareCanvas;
  FValueListEditor.OnSelectEditor := Self.OnValueListEditorSelectEditor;
  FValueListEditor.OnValidateEntry := Self.OnValueListEditorValidateEntry;
  FValueListEditor.TitleCaptions.Add(SPropertyColumnTitle);
  FValueListEditor.TitleCaptions.Add(SValueColumnTitle);
  FValueListEditor.ColWidths[0] := 230;
  FValueListEditor.ColWidths[1] := 370;

  FCustomEditor := TmExtDialogCellEditor.Create(Self);
  FCustomEditor.Visible := false;
  FCustomEditor.ReadOnly := true;
  FCustomEditor.OnShowDialogEvent:= Self.OnShowDialog;
  FCustomEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FCustomEditor.ParentGrid := FValueListEditor;

  FCustomDateEditor := TmExtDialogCellEditor.Create(Self);
  FCustomDateEditor.Visible := false;
  FCustomDateEditor.ReadOnly := false;
  FCustomDateEditor.OnShowDialogEvent:= Self.OnShowDialog;
  FCustomDateEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FCustomDateEditor.ParentGrid := FValueListEditor;

  FCustomButtonEditor := TmExtButtonTextCellEditor.Create(Self);
  FCustomButtonEditor.Visible:= false;
  FCustomButtonEditor.OnShowDialogEvent:= Self.OnShowDialog;
  FCustomButtonEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FCustomButtonEditor.ParentGrid := FValueListEditor;
  FCustomButtonEditor.ReadOnly:= true;

  FLinesByName := TmStringDictionary.Create();
  FLinesByRowIndex := TmIntegerDictionary.Create();
  FLines := TObjectList.Create(true);
  FMemos := TObjectList.Create(true);
  FMemosByName := TmStringDictionary.Create();

  FOnEditValueEvent:= nil;
  FOnValidateValueEvent:= nil;
  FOnInitProviderForLookupEvent:= nil;
  FOnGetValueFromLookupKeyValueEvent:= nil;
end;

destructor TmEditingPanel.Destroy;
begin
  FLinesByName.Free;
  FLinesByRowIndex.Free;
  FLines.Free;
  FMemos.Free;
  FMemosByName.Free;
  inherited Destroy;
end;

procedure TmEditingPanel.SetFocusInEditor;
begin
  FValueListEditor.SetFocus;
  FValueListEditor.Row:= 1;
  FValueListEditor.Col:= 1;
  FValueListEditor.EditorMode:= true;
end;

function TmEditingPanel.CheckMandatoryLines(var aMissingValues: string): boolean;
var
  i : integer;
  tmpLine : TEditorLine;
  comma : string;
begin
  Result := true;
  aMissingValues:= '';
  comma := '';
  for i:= 0 to FLines.Count - 1 do
  begin
    tmpLine:= FLines.Items[i] as TEditorLine;
    if tmpLine.Mandatory then
    begin
      if VarIsNull(GetValue(tmpLine.Name)) then
      begin
        aMissingValues:= aMissingValues + comma + tmpLine.Caption;
        comma := ',' + sLineBreak;
        Result := false;
      end;
    end;
  end;
end;

procedure TmEditingPanel.CommitChanges;
var
  i : integer;
  tmpLine : TEditorLine;
  tmpMemo : TEditorMemo;
begin
  if FCommitted then
    exit;

  for i:= 0 to FLines.Count - 1 do
  begin
    tmpLine:= FLines.Items[i] as TEditorLine;
    if Assigned(tmpLine.ChangedValueDestination) and (not tmpLine.ReadOnly) then
    begin
      if (not MultiEditMode) or  (MultiEditMode and tmpLine.Changed) then
      begin
        tmpLine.ChangedValueDestination.CheckIfDifferentAndAssign(tmpLine.ActualValue);
        FSomethingChanged := FSomethingChanged or tmpLine.ChangedValueDestination.TagChanged;
      end;
    end;
  end;
  for i := 0 to FMemos.Count - 1 do
  begin
    tmpMemo := FMemos.Items[i] as TEditorMemo;
    if Assigned(tmpMemo.ChangedValueDestination) and (not tmpMemo.Memo.ReadOnly) then
    begin
      tmpMemo.ChangedValueDestination.CheckIfDifferentAndAssign(Self.GetValue(tmpMemo.Name));
      FSomethingChanged := FSomethingChanged or tmpMemo.ChangedValueDestination.TagChanged;
    end;
  end;
  FCommitted := true;
end;

end.
