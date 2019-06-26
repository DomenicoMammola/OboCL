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
unit mEditingForm;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, Forms, ValEdit, Graphics, Grids, contnrs, ExtCtrls,
  SysUtils, variants, StdCtrls, Buttons, CheckLst,
  oMultiPanelSetup, OMultiPanel,
  mGridEditors, mMaps, mCalendarDialog, mUtility, mMathUtility,
  mLookupForm, mlookupformInstantQuery,
  mQuickReadOnlyVirtualDataSet, mDataProviderFieldDefs, mNullables,
  mISO6346Utility, mDataProviderInterfaces, mBooleanDataProvider;

resourcestring
  SPropertyColumnTitle = 'Property';
  SValueColumnTitle = 'Value';
  SMissingValuesTitle = 'Missing values';
  SMissingValuesWarning = 'Something is wrong, some mandatory values are missing:';
  SDefaultCaption = 'Edit values';
  SErrorNotADate = 'Not a date.';
  SErrorNotANumber = 'Not a number.';
  SErrorNotAColor = 'Not a color.';
  SErrorNotATime = 'Not a time.';
  SErrorNotAMonth = 'Not a month.';
  SErrorNotAYear = 'Not a year.';
  SErrorYearInThePast = 'Year in the past.';
  SErrorYearInTheFuture = 'Year in the future.';
  SErrorYearTooFaraway = 'Year too faraway.';
  SMultiEditClearValues = 'To be cleared';

type

  TmEditorLineKind = (ekSimple, ekLookup, ekDialog, ekCalendar, ekWizard, ekLookupPlusWizard, ekColorDialog, ekLookupInstantQuery);
  TmEditorLineDataType = (dtInteger, dtFloat, dtDate, dtTime, dtText, dtUppercaseText, dtContainerNumber, dtMRNNumber, dtCurrentYearOrInThePast, dtCurrentYearOrInTheFuture, dtYear, dtMonth, dtColor);
  TmEditorLineReadOnlyMode = (roAllowEditing, roReadOnly, roAllowOnlySetValue);

  TmEditingPanel = class;

  TmOnEditValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aNewDisplayValue: string; const aNewActualValue : variant) of object;
  TmOnValidateValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant; out aErrorMessage : string) of object;
  TmOnShowDialogEvent = function (aSender : TmEditingPanel; const aName: string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant): boolean of object;
  TmOnActivateWizardEvent = function (aSender : TmEditingPanel; const aName: string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant): boolean of object;

  { TmEditorLineConfiguration }

  TmEditorLineConfiguration = class
  strict private
    FDataProvider: IVDDataProvider;
    FInstantQueryManager: IVDInstantQueryManager;
    FLookupFieldNames : TStringList;
    FDisplayLabelFieldNames: TStringList;
    FAlternativeKeyFieldName : string;
    FBooleanProvider : TBooleanDatasetDataProvider;
    FCaption: String;
    FReadOnly: TmEditorLineReadOnlyMode;
    FMandatory: boolean;
    FAllowFreeTypedText : boolean;
    FChangedValueDestination: TAbstractNullable;
    FEditorKind: TmEditorLineKind;
    FDataType : TmEditorLineDataType;
    FTimeMaxDistanceInMonths : integer;

    function GetUseBooleanProvider: boolean;
    procedure SetUseBooleanProvider(AValue: boolean);
  private
    FFractionalPartDigits : byte;
    FDisplayFormat : String;
    FRoundingMethod : TRoundingMethod;
  public
    constructor Create;
    destructor Destroy; override;

    property EditorKind : TmEditorLineKind read FEditorKind write FEditorKind;
    property DataType : TmEditorLineDataType read FDataType write FDataType;
    property TimeMaxDistanceInMonths : integer read FTimeMaxDistanceInMonths write FTimeMaxDistanceInMonths;
    property DataProvider : IVDDataProvider read FDataProvider write FDataProvider;
    property InstantQueryManager: IVDInstantQueryManager read FInstantQueryManager write FInstantQueryManager;

    property Caption: String read FCaption write FCaption;
    property ReadOnly: TmEditorLineReadOnlyMode read FReadOnly write FReadOnly;
    property Mandatory: boolean read FMandatory write FMandatory;
    property AllowFreeTypedText : boolean read FAllowFreeTypedText write FAllowFreeTypedText;
    property ChangedValueDestination: TAbstractNullable read FChangedValueDestination write FChangedValueDestination;

    property DisplayLabelFieldNames: TStringList read FDisplayLabelFieldNames;
    property AlternativeKeyFieldName : string read FAlternativeKeyFieldName write FAlternativeKeyFieldName;
    property LookupFieldNames : TStringList read FLookupFieldNames;
    property UseBooleanProvider : boolean read GetUseBooleanProvider write SetUseBooleanProvider;
    property FractionalPartDigits : byte read FFractionalPartDigits;
    property DisplayFormat : String read FDisplayFormat;
    property RoundingMethod : TRoundingMethod read FRoundingMethod;
  end;

  { TmValueListEditor }

  TmValueListEditor = class(TValueListEditor)
  protected
    Function EditingAllowed(ACol : Integer = -1) : Boolean; override;
  end;

  TOnGetValueSource = (sMainButton, sWizardButton);

  { TmEditingPanel }

  TmEditingPanel = class(TCustomPanel)
  strict private
    const COLOR_CURRENT_ROW = $39EEFD;
  strict private
    FRootPanel : TOMultiPanel;
    FClearValuesPanel : TPanel;
    FValueListEditor: TmValueListEditor;
    FDateCellEditor : TmExtButtonTextCellEditor;
    FButtonCellEditor : TmExtButtonTextCellEditor;
    FWizardCellEditor : TmExtButtonTextCellEditor;
    FButtonWizardCellEditor : TmExtButtonsTextCellEditor;
    FLinesByName : TmStringDictionary;
    FLinesByRowIndex : TmIntegerDictionary;
    FMemosByName : TmStringDictionary;
    FLines : TObjectList;
    FMemos : TObjectList;
    FClearValuesList : TCheckListBox;
    FOnEditValueEvent: TmOnEditValueEvent;
    FOnValidateValueEvent: TmOnValidateValueEvent;
    FOnShowDialogEvent: TmOnShowDialogEvent;
    FOnActivateWizardEvent: TmOnActivateWizardEvent;
    FMultiEditMode : boolean;
    FSomethingChanged : boolean;
    FCommitted : boolean;
    FLastEditorUsed : string;

    function GetAlternateColor: TColor;
    procedure SetAlternateColor(AValue: TColor);

    procedure OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnValueListEditorSelectEditor(Sender: TObject; aCol,  aRow: Integer; var Editor: TWinControl);
    procedure OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    function OnGetValueFromMainSource(const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
    function OnGetValue (const aCol, aRow : integer; const aSource: TOnGetValueSource; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
    function OnGetValueFromWizard (const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
    function OnValueListEditorClearValue (const aCol, aRow: integer): boolean;
    procedure OnClickClearValue(Sender: TObject);
    function ComposeCaption (const aCaption : string; const aMandatory : boolean): string;
    function GetValueFromMemo (const aName : string; const aTrimValue : boolean) : string;
    procedure CheckDate (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckTime (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckInteger (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckYear(const aOldStringValue : string; const aDataType: TmEditorLineDataType; const aDistanceInMonths : integer; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckMonth(const aOldStringValue : string; const aDataType: TmEditorLineDataType; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckFloat (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant; const aLineConfiguration : TmEditorLineConfiguration);
    procedure CheckColor (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
  private
    procedure SetMultiEditMode(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocusInEditor;
    function CheckMandatoryLines(var aMissingValues: string): boolean;
    procedure CommitChanges;

    function AddLine(const aName : String): TmEditorLineConfiguration;

    procedure AddMemo (const aName : string; const aCaption : string; const aDefaultValue : string; const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil;
      const aReadOnly : TmEditorLineReadOnlyMode = roAllowEditing);

    function GetValue(const aName : string) : Variant;
    procedure SetValue(const aName : string; const aDisplayValue: String; const aActualValue: variant);

    function GetConfigurationForLine (const aName : string): TmEditorLineConfiguration;

    procedure Run;

    property AlternateColor : TColor read GetAlternateColor write SetAlternateColor;
    property MultiEditMode : boolean read FMultiEditMode write SetMultiEditMode;

    property OnEditValue: TmOnEditValueEvent read FOnEditValueEvent write FOnEditValueEvent;
    property OnValidateValue: TmOnValidateValueEvent read FOnValidateValueEvent write FOnValidateValueEvent;
    property OnShowDialog : TmOnShowDialogEvent read FOnShowDialogEvent write FOnShowDialogEvent;
    property OnActivateWizard : TmOnActivateWizardEvent read FOnActivateWizardEvent write FOnActivateWizardEvent;

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
    procedure SetReadOnly;
  public
    property EditingPanel: TmEditingPanel read FEditingPanel;
  end;



implementation

uses
  {$IFDEF DEBUG}
  LazLogger,
  {$ENDIF}
  LCLType,
  Dialogs, dateutils,
  mToast;

type

  { TEditorLine }

  TEditorLine = class
  private
    Name: String;
    Index: integer;

    Changed: boolean;
    ForceClear : boolean;
    Configuration: TmEditorLineConfiguration;
    ActualValue : variant;

    //OldActualValue : variant;
    //OldDisplayValue : TNullableStringRecord;
    //EditorExecuted : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function RowIndex : integer;
    procedure SetActualValue(AValue: variant);
  end;

  TEditorMemo = class
  private
    Name : String;
    Memo : TMemo;
    ChangedValueDestination: TAbstractNullable;
  end;

{ TmEditorLineConfiguration }

function TmEditorLineConfiguration.GetUseBooleanProvider: boolean;
begin
  Result := Assigned(FBooleanProvider);
end;

procedure TmEditorLineConfiguration.SetUseBooleanProvider(AValue: boolean);
begin
  if aValue then
  begin
    if not Assigned(FBooleanProvider) then
      FBooleanProvider:= TBooleanDatasetDataProvider.Create;
    FDataProvider := FBooleanProvider;
    FDisplayLabelFieldNames.Add(TBooleanDatum.FLD_VALUE);
    EditorKind:= ekLookup;
    AllowFreeTypedText:= false;
  end
  else
  begin
    if Assigned(FDataProvider) and Assigned(FBooleanProvider) then
      FDataProvider := nil;
    FreeAndNil(FBooleanProvider);
  end;
end;

constructor TmEditorLineConfiguration.Create;
begin
  FDataProvider := nil;
  FInstantQueryManager := nil;
  FDisplayLabelFieldNames := TStringList.Create;
  FAlternativeKeyFieldName:= '';
  FLookupFieldNames := TStringList.Create;
  FBooleanProvider := nil;
  FCaption:= '';
  FReadOnly:= roAllowEditing;
  FMandatory:= false;
  FChangedValueDestination:= nil;
  FEditorKind:= ekSimple;
  FDataType:= dtText;
  FAllowFreeTypedText := true;
  FFractionalPartDigits:= 0;
  FDisplayFormat:= '';
  FTimeMaxDistanceInMonths:= 0;
end;

destructor TmEditorLineConfiguration.Destroy;
begin
  FLookupFieldNames.Free;
  FDisplayLabelFieldNames.Free;
  FreeAndNil(FBooleanProvider);
  inherited Destroy;
end;

procedure TEditorLine.SetActualValue(AValue: variant);
begin
  ActualValue:=AValue;
end;

constructor TEditorLine.Create;
begin
  Configuration := TmEditorLineConfiguration.Create;
  Name:= '';
  Index:= 0;
  ActualValue:= Null;
  Changed:= false;
  ForceClear:= false;
end;

destructor TEditorLine.Destroy;
begin
  Configuration.Free;
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

  Self.Height:= trunc (Screen.Height * 0.80);
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

procedure TmEditingForm.SetReadOnly;
begin
  FOkBtn.Visible:= false;
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

    if (sender as TmValueListEditor).Row = aRow then
      FValueListEditor.Canvas.Brush.Color := TmEditingPanel.COLOR_CURRENT_ROW;

    if MultiEditMode and (aRow > 0) then
    begin
      if (FLinesByRowIndex.Find(aRow) as TEditorLine).ForceClear then
        FValueListEditor.Canvas.Font.Color:= clLtGray
      else
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
  (Sender as TmValueListEditor).Invalidate;

  if aCol <> 1 then
    exit;

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;
  if (not Assigned(curLine)) or (curLine.Configuration.ReadOnly in [roAllowOnlySetValue, roReadOnly]) or (curLine.ForceClear) then
    exit;

  if (curLine.Configuration.EditorKind = ekCalendar) then
  begin
    FDateCellEditor.TextEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FDateCellEditor;
  end
  else if (curLine.Configuration.EditorKind = ekColorDialog) then
  begin
    FButtonCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FButtonCellEditor;
    FButtonCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end
  else if (curLine.Configuration.EditorKind = ekLookup) or (curLine.Configuration.EditorKind = ekLookupInstantQuery) then
  begin
    FButtonCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FButtonCellEditor;
    FButtonCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end
  else if (curLine.Configuration.EditorKind = ekDialog) then
  begin
    FButtonCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FButtonCellEditor;
    FButtonCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end
  else if (curLine.Configuration.EditorKind = ekWizard) then
  begin
    FWizardCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FWizardCellEditor;
    FWizardCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end
  else if (curLine.Configuration.EditorKind = ekLookupPlusWizard) then
  begin
    FButtonWizardCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FButtonWizardCellEditor;
    FButtonWizardCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end;
end;

procedure TmEditingPanel.OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  curLine : TEditorLine;
  errorMessage : String;
  OldActualValue : variant;
begin
  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if (curLine.Configuration.ReadOnly = roAllowEditing) and (not curLine.ForceClear) then
    NewValue := Trim(NewValue)
  else
  begin
    NewValue := OldValue;
    exit;
  end;

  if NewValue = OldValue then
    exit;

  OldActualValue := curLine.ActualValue;

  if (curLine.Name = FLastEditorUsed) and (not curLine.Configuration.AllowFreeTypedText) then
    exit;

  if (curLine.Configuration.EditorKind = ekSimple)
    or (curLine.Configuration.EditorKind = ekCalendar)
    or (curLine.Configuration.AllowFreeTypedText) then
  begin
    if curLine.Configuration.DataType = dtDate then
      CheckDate (OldValue, NewValue, curLine.ActualValue)
    else if curLine.Configuration.DataType = dtTime then
      CheckTime (OldValue, NewValue, curLine.ActualValue)
    else if curLine.Configuration.DataType = dtInteger then
      CheckInteger (OldValue, NewValue, curLine.ActualValue)
    else if (curLine.Configuration.DataType = dtFloat) then
      CheckFloat (OldValue, NewValue, curLine.ActualValue, curLine.Configuration)
    else if (curLine.Configuration.DataType = dtColor) then
      CheckColor(OldValue, NewValue, curLine.ActualValue)
    else if curLine.Configuration.DataType = dtUppercaseText then
    begin
      NewValue := Uppercase(NewValue);
      if NewValue <> '' then
        curLine.ActualValue:= NewValue
      else
        curLine.ActualValue:= null;
    end else if curLine.Configuration.DataType = dtText then
    begin
      if NewValue <> '' then
        curLine.ActualValue:= NewValue
      else
        curLine.ActualValue:= null;
    end else if curLine.Configuration.DataType = dtContainerNumber then
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
    end else if curLine.Configuration.DataType = dtMRNNumber then
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
      end
      else
      begin
        curLine.ActualValue:= null;
      end;
    end
    else if (curLine.Configuration.DataType = dtYear) or (curLine.Configuration.DataType = dtCurrentYearOrInThePast) or
      (curLine.Configuration.DataType = dtCurrentYearOrInTheFuture) then
    begin
      CheckYear(OldValue, curLine.Configuration.DataType, curLine.Configuration.TimeMaxDistanceInMonths, NewValue, curLine.ActualValue);
    end
    else if (curLine.Configuration.DataType = dtMonth) then
    begin
      CheckMonth(OldValue, curLine.Configuration.DataType, NewValue, curLine.ActualValue);
    end;
  end
  else
  begin
    exit;
  end;


  if Assigned(FOnValidateValueEvent) then
  begin
    FOnValidateValueEvent(Self, curLine.Name, OldValue, NewValue, OldActualValue, curLine.ActualValue, errorMessage);
    if errorMessage <> '' then
      TmToast.ShowText(errorMessage);
  end;

  if NewValue <> OldValue then
  begin
    curLine.Changed := true;

    if Assigned(FOnEditValueEvent) then
      FOnEditValueEvent(Self, curLine.Name, NewValue, curLine.ActualValue);

    FValueListEditor.Invalidate;
  end;
end;

function TmEditingPanel.OnGetValueFromMainSource(const aCol, aRow: integer; var aNewDisplayValue: string; var aNewActualValue: variant): boolean;
begin
  Result := OnGetValue(aCol, aRow, sMainButton, aNewDisplayValue, aNewActualValue);
end;

function TmEditingPanel.OnGetValue(const aCol, aRow : integer; const aSource: TOnGetValueSource; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
var
  calendarFrm : TmCalendarDialog;
  colorDialog : TColorDialog;
  keyFieldName, errorMessage : String;
  curLine : TEditorLine;
  lookupFrm : TmLookupFrm;
  lookupFrmInstantQuery : TmLookupInstantQueryFrm;
  tmpVirtualFieldDefs : TmVirtualFieldDefs;
  OldStringValue : string;
  OldActualValue : Variant;
  tmpColor : TColor;
begin
  Result := false;

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if (curLine.Configuration.ReadOnly <> roAllowEditing) or (curLine.ForceClear) then
    exit;

  OldStringValue := FValueListEditor.Cells[aCol, aRow];
  OldActualValue := curLine.ActualValue;

  if curLine.Configuration.EditorKind = ekCalendar then
  begin
    calendarFrm := TmCalendarDialog.Create;
    try
      if OldStringValue <> '' then
      begin
        try
          calendarFrm.Date := StrToDate(OldStringValue);
        except
          // ignored
        end;
      end;
      if calendarFrm.Execute then
      begin
        aNewDisplayValue := DateToStr(calendarFrm.Date);
        curLine.ActualValue:= calendarFrm.Date;
        Result := true;
      end;
    finally
      calendarFrm.Free;
    end;
  end
  else if (curLine.Configuration.EditorKind = ekColorDialog) then
  begin
    colorDialog := TColorDialog.Create(Self);
    try
      if OldStringValue <> '' then
      begin
        if TryToUndestandColorString(OldStringValue, tmpColor) then
          colorDialog.Color:= tmpColor;
      end;
      if colorDialog.Execute then
      begin
        aNewDisplayValue:= ColorToString(colorDialog.Color);
        curLine.ActualValue:= aNewDisplayValue;
        Result := true;
      end;
    finally
      colorDialog.Free;
    end;
  end
  else if (curLine.Configuration.EditorKind = ekDialog) then
  begin
    if Assigned(FOnShowDialogEvent) then
    begin
      FLastEditorUsed:= curLine.Name;
      Result := FOnShowDialogEvent(Self, curLine.Name, OldStringValue, aNewDisplayValue, OldActualValue, curLine.ActualValue);
    end;
  end
  else if (curLine.Configuration.EditorKind = ekWizard) or ((curLine.Configuration.EditorKind = ekLookupPlusWizard) and (aSource = sWizardButton)) then
  begin
    if Assigned(FOnActivateWizardEvent) then
    begin
      FLastEditorUsed:= curLine.Name;
      Result := FOnActivateWizardEvent(Self, curLine.Name, OldStringValue, aNewDisplayValue, OldActualValue, curLine.ActualValue);
    end;
  end
  else if (curLine.Configuration.EditorKind = ekLookupInstantQuery) then
  begin
    lookupFrmInstantQuery := TmLookupInstantQueryFrm.Create(Self);
    try
      assert (Assigned(curLine.Configuration.InstantQueryManager));

      if curLine.Configuration.LookupFieldNames.Count = 0 then
        curLine.Configuration.InstantQueryManager.GetDataProvider.GetMinimumFields(curLine.Configuration.LookupFieldNames);
      if curLine.Configuration.LookupFieldNames.Count = 0 then
      begin
        tmpVirtualFieldDefs := TmVirtualFieldDefs.Create;
        try
          curLine.Configuration.InstantQueryManager.GetDataProvider.FillVirtualFieldDefs(tmpVirtualFieldDefs, '');
          tmpVirtualFieldDefs.ExtractFieldNames(curLine.Configuration.LookupFieldNames);
        finally
          tmpVirtualFieldDefs.Free;;
        end;
      end;

      if curLine.Configuration.DisplayLabelFieldNames.Count = 0 then
        curLine.Configuration.InstantQueryManager.GetDataProvider.GetMinimumFields(curLine.Configuration.DisplayLabelFieldNames);

      if curLine.Configuration.AlternativeKeyFieldName <> '' then
        keyFieldName := curLine.Configuration.AlternativeKeyFieldName
      else
        keyFieldName := curLine.Configuration.InstantQueryManager.GetDataProvider.GetKeyFieldName;

      lookupFrmInstantQuery.Init(curLine.Configuration.InstantQueryManager, curLine.Configuration.LookupFieldNames,
        keyFieldName, curLine.Configuration.DisplayLabelFieldNames);
      if lookupFrmInstantQuery.ShowModal = mrOk then
      begin
        aNewDisplayValue:= lookupFrmInstantQuery.SelectedDisplayLabel;
        curLine.ActualValue:= lookupFrmInstantQuery.SelectedValue;

        FLastEditorUsed:= curLine.Name;

        Result := true;
      end;
    finally
      lookupFrmInstantQuery.Free;
    end;
  end
  else if (curLine.Configuration.EditorKind = ekLookup) or ((curLine.Configuration.EditorKind = ekLookupPlusWizard) and (aSource = sMainButton)) then
  begin
    lookupFrm := TmLookupFrm.Create(Self);
    try
      assert (Assigned(curLine.Configuration.DataProvider));

      if curLine.Configuration.LookupFieldNames.Count = 0 then
        curLine.Configuration.DataProvider.GetMinimumFields(curLine.Configuration.LookupFieldNames);
      if curLine.Configuration.LookupFieldNames.Count = 0 then
      begin
        tmpVirtualFieldDefs := TmVirtualFieldDefs.Create;
        try
          curLine.Configuration.DataProvider.FillVirtualFieldDefs(tmpVirtualFieldDefs, '');
          tmpVirtualFieldDefs.ExtractFieldNames(curLine.Configuration.LookupFieldNames);
        finally
          tmpVirtualFieldDefs.Free;;
        end;
      end;

      if curLine.Configuration.DisplayLabelFieldNames.Count = 0 then
        curLine.Configuration.DataProvider.GetMinimumFields(curLine.Configuration.DisplayLabelFieldNames);

      if curLine.Configuration.AlternativeKeyFieldName <> '' then
        keyFieldName := curLine.Configuration.AlternativeKeyFieldName
      else
        keyFieldName := curLine.Configuration.DataProvider.GetKeyFieldName;

      lookupFrm.Init(curLine.Configuration.DataProvider, curLine.Configuration.LookupFieldNames,
        keyFieldName, curLine.Configuration.DisplayLabelFieldNames);
      if lookupFrm.ShowModal = mrOk then
      begin
        aNewDisplayValue:= lookupFrm.SelectedDisplayLabel;
        curLine.ActualValue:= lookupFrm.SelectedValue;

        FLastEditorUsed:= curLine.Name;

        Result := true;
      end;
    finally
      lookupFrm.Free;
    end;
  end;

  if Result then
  begin
    if Assigned(FOnValidateValueEvent) then
    begin
      FOnValidateValueEvent(Self, curLine.Name, OldStringValue, aNewDisplayValue, OldActualValue, curLine.ActualValue, errorMessage);
      if errorMessage <> '' then
        TmToast.ShowText(errorMessage);
    end;

    if aNewDisplayValue <> OldStringValue then
    begin
      curLine.Changed := true;

      FValueListEditor.OnValidateEntry := nil;
      try
        FValueListEditor.Cells[aCol, aRow] := aNewDisplayValue;
      finally
        FValueListEditor.OnValidateEntry := Self.OnValueListEditorValidateEntry;
      end;

      if Assigned(FOnEditValueEvent) then
        FOnEditValueEvent(Self, curLine.Name, aNewDisplayValue, curLine.ActualValue);

      FValueListEditor.Invalidate;
    end;
  end;
end;

function TmEditingPanel.OnGetValueFromWizard(const aCol, aRow: integer; var aNewDisplayValue: string; var aNewActualValue: variant): boolean;
begin
  Result := OnGetValue(aCol, aRow, sWizardButton, aNewDisplayValue, aNewActualValue);
end;

function TmEditingPanel.OnValueListEditorClearValue(const aCol, aRow: integer): boolean;
var
  curLine: TEditorLine;
  NewValue, OldValue, errorMessage : string;
  NewActualValue, OldActualValue: variant;
begin
  Result := false;
  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if (curLine.Configuration.ReadOnly <> roAllowEditing) or (curLine.ForceClear) then
    exit;

  NewValue := '';
  NewActualValue:= null;
  OldValue := FValueListEditor.Rows[curLine.Index + 1].Strings[1];
  OldActualValue := curLine.ActualValue;

  if MultiEditMode then
    curLine.Changed := false
  else
  begin
    if Assigned(FOnValidateValueEvent) then
    begin
      FOnValidateValueEvent(Self, curLine.Name, OldValue, NewValue, OldActualValue, NewActualValue, errorMessage);
      if errorMessage <> '' then
        TmToast.ShowText(errorMessage);
    end;

    if NewValue <> OldValue then
    begin
      curLine.Changed:= true; // curLine.Changed and (FValueListEditor.Rows[curLine.Index + 1].Strings[1] <> '');

      FValueListEditor.Rows[curLine.Index + 1].Strings[1] := NewValue;
      curLine.ActualValue:= NewActualValue;

      if Assigned(FOnEditValueEvent) then
        FOnEditValueEvent(Self, curLine.Name, NewValue, NewActualValue);

      FValueListEditor.Invalidate;
      Result := true;
    end;
  end;

(*
  FValueListEditor.Rows[curLine.Index + 1].Strings[1] := NewValue;
  curLine.ActualValue:= NewActualValue;
  Result := true;
  FValueListEditor.Invalidate;*)
end;

procedure TmEditingPanel.OnClickClearValue(Sender: TObject);
begin
  if Sender is TCheckListBox then
  begin
    ((Sender as TCheckListBox).Items.Objects[(Sender as TCheckListBox).ItemIndex] as TEditorLine).ForceClear:= (Sender as TCheckListBox).Checked[(Sender as TCheckListBox).ItemIndex];
    FValueListEditor.Invalidate;
  end;
end;

function TmEditingPanel.ComposeCaption(const aCaption: string;
  const aMandatory: boolean): string;
begin
  if aMandatory then
    Result := aCaption + ' *'
  else
    Result := aCaption;
end;

procedure TmEditingPanel.AddMemo(const aName: string; const aCaption: string;const aDefaultValue: string;
    const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil; const aReadOnly : TmEditorLineReadOnlyMode = roAllowEditing);
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
  tmpEditorMemo.Memo.ReadOnly := (aReadOnly <> roAllowEditing);

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

function TmEditingPanel.GetConfigurationForLine(const aName: string): TmEditorLineConfiguration;
var
  curLine: TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  if Assigned(curLine) then
    Result := curLine.Configuration
  else
    Result := nil;
end;

procedure TmEditingPanel.Run;
var
  i, k : integer;
  curLine : TEditorLine;
  str : String;
  curDatum : IVDDatum;
  curValue : Variant;
begin
  for i := 0 to FLines.Count - 1 do
  begin
    curLine := FLines.Items[i] as TEditorLine;

    if not Assigned(curLine.Configuration.ChangedValueDestination) then
      raise Exception.Create('Nullable destination not set for field ' + curLine.Name);

    curLine.ActualValue:= curLine.Configuration.ChangedValueDestination.AsVariant;

    if (curLine.Configuration.DataType = dtFloat) and (curLine.Configuration.ChangedValueDestination is TNullableDouble) then
    begin
      curLine.Configuration.FDisplayFormat:= (curLine.Configuration.ChangedValueDestination as TNullableDouble).DisplayFormat;
      curLine.Configuration.FFractionalPartDigits := (curLine.Configuration.ChangedValueDestination as TNullableDouble).FractionalPartDigits;
      curLine.Configuration.FRoundingMethod:= (curLine.Configuration.ChangedValueDestination as TNullableDouble).RoundingMethod;
      str := curLine.Configuration.ChangedValueDestination.AsString;
    end
    else if (curLine.Configuration.DataType = dtDate) and (curLine.Configuration.ChangedValueDestination is TNullableDateTime) then
      str := (curLine.Configuration.ChangedValueDestination as TNullableDateTime).AsString(false)
    else if (curLine.Configuration.DataType = dtColor) then
      str := (curLine.Configuration.ChangedValueDestination as TNullableColor).AsString
    else if (curLine.Configuration.DataType = dtTime) and (curLine.Configuration.ChangedValueDestination is TNullableTime) then
      str := (curLine.Configuration.ChangedValueDestination as TNullableTime).AsString
    else if ((curLine.Configuration.EditorKind = ekLookup) or ((curLine.Configuration.EditorKind = ekLookupPlusWizard))) and (curLine.Configuration.ChangedValueDestination.NotNull) then
    begin
      assert (Assigned(curLine.Configuration.DataProvider));

      if (curLine.Configuration.DisplayLabelFieldNames.Count = 0)  then
        curLine.Configuration.DataProvider.GetMinimumFields(curLine.Configuration.DisplayLabelFieldNames);

      if curLine.Configuration.AlternativeKeyFieldName = '' then
        curDatum := curLine.Configuration.DataProvider.FindDatumByStringKey(curLine.Configuration.ChangedValueDestination.AsString)
      else
      begin
        curDatum := nil;
        for k := 0 to curLine.Configuration.DataProvider.Count - 1 do
        begin
          curValue := curLine.Configuration.DataProvider.GetDatum(k).GetPropertyByFieldName(curLine.Configuration.AlternativeKeyFieldName);
          if CompareVariants(curValue, curLine.Configuration.ChangedValueDestination.AsVariant) = 0 then
          begin
            curDatum := curLine.Configuration.DataProvider.GetDatum(k);
            break;
          end;
        end;
      end;
      if Assigned(curDatum) then
        str := ConcatenateFieldValues(curDatum, curLine.Configuration.DisplayLabelFieldNames)
      else
        str := curLine.Configuration.ChangedValueDestination.AsString;
    end
    else
      str := curLine.Configuration.ChangedValueDestination.AsString;

    curLine.Index:= FValueListEditor.InsertRow(ComposeCaption(curLine.Configuration.Caption, curLine.Configuration.Mandatory), str, true);

    FLinesByRowIndex.Add(curLine.RowIndex, curLine);
    FValueListEditor.ItemProps[curLine.Index].ReadOnly:= (curLine.Configuration.ReadOnly <> roAllowEditing);
    if (curLine.Configuration.ReadOnly = roAllowEditing) and ((curLine.Configuration.EditorKind = ekCalendar) or (curLine.Configuration.EditorKind = ekLookup)
      or (curLine.Configuration.EditorKind = ekColorDialog) or (curLine.Configuration.EditorKind = ekLookupInstantQuery)
      or (curLine.Configuration.EditorKind = ekDialog) or (curLine.Configuration.EditorKind = ekWizard) or (curLine.Configuration.EditorKind = ekLookupPlusWizard)) then
        FValueListEditor.ItemProps[curLine.Index].EditStyle:=esEllipsis;

    if MultiEditMode and Assigned(FClearValuesList) then
      FClearValuesList.AddItem(curLine.Configuration.Caption, curLine);
  end;
end;

procedure TmEditingPanel.SetValue(const aName: string; const aDisplayValue: String; const aActualValue: variant);
var
  curLine: TEditorLine;
begin
  curLine := FLinesByName.Find(aName) as TEditorLine;
  if Assigned(curLine) and (curLine.Configuration.ReadOnly <> roReadOnly) then
  begin
    curLine.Changed := curLine.Changed or (aDisplayValue <> FValueListEditor.Rows[curLine.Index + 1].Strings[1]);
    FValueListEditor.Rows[curLine.Index + 1].Strings[1] := aDisplayValue;
    curLine.ActualValue:= aActualValue;
    FValueListEditor.Invalidate;
  end;
end;

(*
procedure TmEditingPanel.SetReadOnly(const aName: string; const aValue : boolean);
var
  tmpObj : TObject;
begin
  (FLinesByName.Find(aName) as TEditorLine).Configuration.ReadOnly:= aValue;

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
    (FLines.Items[i] as TEditorLine).Configuration.ReadOnly:= aValue;
  for i := 0 to FMemos.Count - 1 do
    (FMemos.Items[i] as TEditorMemo).Memo.ReadOnly:= aValue;
  for i := 0 to FValueListEditor.Strings.Count -1 do
  begin
    tmp := (FValueListEditor.ItemProps[i]);
    if Assigned(tmp) then
      tmp.ReadOnly := aValue;
  end;
end;
*)

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

procedure TmEditingPanel.CheckDate(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
var
  vDate : TDate;
begin
  vDate := 0;
  if aNewStringValue <> '' then
  begin
    if TryToUnderstandDateString(aNewStringValue, vDate) then
    begin
      aNewStringValue := DateToStr(vDate);
      aActualValue := vDate;
    end
    else
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotADate);
    end;
  end
  else
    aActualValue := null;
end;

procedure TmEditingPanel.CheckTime(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
var
  vDate : TDateTime;
begin
  vDate := 0;
  if aNewStringValue <> '' then
  begin
    if TryToUnderstandTimeString(aNewStringValue, vDate) then
    begin
      aNewStringValue := TimeToStr(vDate);
      aActualValue := vDate;
    end
    else
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotATime);
    end;
  end
  else
    aActualValue:= null;
end;

procedure TmEditingPanel.CheckInteger(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
begin
  if aNewStringValue <> '' then
  begin
    if not IsNumeric(aNewStringValue, false) then
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotANumber);
    end
    else
      aActualValue:= StrToInt(aNewStringValue);
  end
  else
    aActualValue:= null;
end;

procedure TmEditingPanel.CheckYear(const aOldStringValue: string; const aDataType: TmEditorLineDataType; const aDistanceInMonths : integer; var aNewStringValue: string; var aActualValue: variant);
var
  tmp : integer;
begin
  if aNewStringValue <> '' then
  begin
    if not IsNumeric(aNewStringValue, false) then
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotANumber);
    end
    else
    begin
      tmp := StrToInt(aNewStringValue);
      if (tmp <= 0) then
      begin
        aNewStringValue:= aOldStringValue;
        TmToast.ShowText(SErrorNotAYear);
      end
      else if (aDataType = dtCurrentYearOrInTheFuture) and (tmp < YearOf(Date)) then
      begin
        aNewStringValue:= aOldStringValue;
        TmToast.ShowText(SErrorYearInThePast);
      end
      else if (aDataType = dtCurrentYearOrInThePast) and (tmp > YearOf(Date)) then
      begin
        aNewStringValue:= aOldStringValue;
        TmToast.ShowText(SErrorYearInTheFuture);
      end
      else if (aDistanceInMonths > 0) and (abs(YearOf(Date) - tmp) > (aDistanceInMonths div 12)) then
      begin
        aNewStringValue:= aOldStringValue;
        TmToast.ShowText(SErrorYearTooFaraway);;
      end
      else
        aActualValue := tmp;
    end;
  end
  else
    aActualValue:= null;
end;

procedure TmEditingPanel.CheckMonth(const aOldStringValue: string; const aDataType: TmEditorLineDataType; var aNewStringValue: string; var aActualValue: variant);
var
  tmp : integer;
begin
  if aNewStringValue <> '' then
  begin
    if not IsNumeric(aNewStringValue, false) then
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotANumber);
    end
    else
    begin
      tmp := StrToInt(aNewStringValue);
      if (tmp < 1) or (tmp > 12) then
      begin
        aNewStringValue:= aOldStringValue;
        TmToast.ShowText(SErrorNotAMonth);
      end
      else
        aActualValue := tmp;
    end;
  end
  else
    aActualValue:= null;
end;

procedure TmEditingPanel.CheckFloat(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant; const aLineConfiguration : TmEditorLineConfiguration);
var
  tmpDouble : double;
begin
  if aNewStringValue <> '' then
  begin
    if TryToConvertToDouble(aNewStringValue, tmpDouble) then
    begin
      if aLineConfiguration.FractionalPartDigits > 0 then
        tmpDouble := RoundToExt(tmpDouble, aLineConfiguration.RoundingMethod, aLineConfiguration.FractionalPartDigits);
      if aLineConfiguration.DisplayFormat <> '' then
        aNewStringValue := FormatFloat(aLineConfiguration.DisplayFormat, tmpDouble)
      else
        aNewStringValue := FloatToStr(tmpDouble);
      aActualValue:= tmpDouble;
    end
    else
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotANumber);
    end;
  end
  else
    aActualValue := null;
end;

procedure TmEditingPanel.CheckColor(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
var
  tmpColor : TColor;
begin
  if aNewStringValue <> '' then
  begin
    if TryToUndestandColorString(aNewStringValue, tmpColor) then
    begin
      aNewStringValue := ColorToString(tmpColor);
      aActualValue:= aNewStringValue;
    end
    else
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotAColor);
    end;
  end
  else
    aActualValue := null;
end;

constructor TmEditingPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLastEditorUsed:= '';

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
  FValueListEditor.DefaultColWidth := 250;
  FValueListEditor.FixedCols := 0;
  FValueListEditor.Flat := True;
  FValueListEditor.RowCount := 2;
  FValueListEditor.TabOrder := 0;
  FValueListEditor.OnPrepareCanvas := Self.OnValueListEditorPrepareCanvas;
  FValueListEditor.OnSelectEditor := Self.OnValueListEditorSelectEditor;
  FValueListEditor.OnValidateEntry := Self.OnValueListEditorValidateEntry;
  FValueListEditor.TitleCaptions.Add(SPropertyColumnTitle);
  FValueListEditor.TitleCaptions.Add(SValueColumnTitle);
  FValueListEditor.ColWidths[0] := 250;
  FValueListEditor.ColWidths[1] := 350;

  FDateCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FDateCellEditor.Visible := false;
  FDateCellEditor.AllowFreeTypedText := true;
  FDateCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FDateCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FDateCellEditor.ParentGrid := FValueListEditor;
  FDateCellEditor.ButtonStyle:= cebsCalendar;

  FButtonCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FButtonCellEditor.Visible:= false;
  FButtonCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FButtonCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FButtonCellEditor.ParentGrid := FValueListEditor;
  FButtonCellEditor.AllowFreeTypedText:= false;

  FWizardCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FWizardCellEditor.Visible:= false;
  FWizardCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FWizardCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FWizardCellEditor.ParentGrid := FValueListEditor;
  FWizardCellEditor.AllowFreeTypedText:= false;
  FWizardCellEditor.ButtonStyle:= cebsMagicWand;

  FButtonWizardCellEditor := TmExtButtonsTextCellEditor.Create(Self);
  FButtonWizardCellEditor.Visible:= false;
  FButtonWizardCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FButtonWizardCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FButtonWizardCellEditor.ParentGrid := FValueListEditor;
  FButtonWizardCellEditor.AllowFreeTypedText:= false;
  FButtonWizardCellEditor.SetButtons(2);
  FButtonWizardCellEditor.SetButtonStyle(0, cebsMagicWand);
  FButtonWizardCellEditor.SetButtonStyle(1, cebsDots);
  FButtonWizardCellEditor.SetOnClickButton(0, Self.OnGetValueFromWizard);
  FButtonWizardCellEditor.TextEditor.OnShowWizardEvent:= Self.OnGetValueFromWizard;
  FButtonWizardCellEditor.SetOnClickButton(1, Self.OnGetValueFromMainSource);

  FLinesByName := TmStringDictionary.Create();
  FLinesByRowIndex := TmIntegerDictionary.Create();
  FLines := TObjectList.Create(true);
  FMemos := TObjectList.Create(true);
  FMemosByName := TmStringDictionary.Create();

  FOnEditValueEvent:= nil;
  FOnValidateValueEvent:= nil;
  FOnShowDialogEvent:= nil;
  FOnActivateWizardEvent:= nil;
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

function TmEditingPanel.AddLine(const aName: String): TmEditorLineConfiguration;
var
  tmp : TEditorLine;
begin
  tmp := TEditorLine.Create;
  tmp.Name := aName;
  FLines.Add(tmp);
  FLinesByName.Add(aName, tmp);
  Result := tmp.Configuration;
end;

procedure TmEditingPanel.SetFocusInEditor;
begin
  FValueListEditor.SetFocus;
  FValueListEditor.Row:= 1;
  FValueListEditor.Col:= 1;
  FValueListEditor.EditorMode:= true;
end;

procedure TmEditingPanel.SetMultiEditMode(AValue: boolean);
var
  tmpPanel : TPanel;
begin
  if FMultiEditMode=AValue then Exit;
  FMultiEditMode:=AValue;
  if FMultiEditMode then
  begin
    if not Assigned(FClearValuesPanel) then
    begin
      FClearValuesPanel := TPanel.Create(FRootPanel);
      FClearValuesPanel.Parent := FRootPanel;
      FClearValuesPanel.BevelInner:= bvNone;
      FClearValuesPanel.BevelOuter:= bvNone;
      FRootPanel.PanelCollection.AddControl(FClearValuesPanel);
      tmpPanel := TPanel.Create(FClearValuesPanel);
      tmpPanel.Parent := FClearValuesPanel;
      tmpPanel.Align:= alTop;
      tmpPanel.BevelInner:= bvNone;
      tmpPanel.BevelOuter:= bvNone;
      tmpPanel.Height:= FValueListEditor.DefaultRowHeight;
      tmpPanel.Caption:= SMultiEditClearValues;
      tmpPanel.Font.Style:=[fsBold];
      tmpPanel.BorderWidth:= 1;
      tmpPanel.BorderStyle:=bsSingle;
    end;
    if not Assigned(FClearValuesList) then
    begin
      FClearValuesList := TCheckListBox.Create(FClearValuesPanel);
      FClearValuesList.Parent := FClearValuesPanel;
      FClearValuesList.Align:= alClient;
      FClearValuesList.OnClickCheck:= Self.OnClickClearValue;
    end;
  end
  else
  begin
    if Assigned(FClearValuesPanel) then
      FClearValuesPanel.Visible:= false;
  end;
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
    if tmpLine.Configuration.Mandatory then
    begin
      if VarIsNull(GetValue(tmpLine.Name)) then
      begin
        aMissingValues:= aMissingValues + comma + tmpLine.Configuration.Caption;
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
    if Assigned(tmpLine.Configuration.ChangedValueDestination) and (tmpLine.Configuration.ReadOnly <> roReadOnly) then
    begin
      if (tmpLine.ForceClear) then
      begin
        tmpLine.Configuration.ChangedValueDestination.CheckIfDifferentAndAssign(null);
        FSomethingChanged := FSomethingChanged or tmpLine.Configuration.ChangedValueDestination.TagChanged;
      end
      else if (not MultiEditMode) or (MultiEditMode and tmpLine.Changed) then
      begin
        tmpLine.Configuration.ChangedValueDestination.CheckIfDifferentAndAssign(tmpLine.ActualValue);
        FSomethingChanged := FSomethingChanged or tmpLine.Configuration.ChangedValueDestination.TagChanged;
        {$IFDEF DEBUG}
        DebugLn(VarToStr(tmpLine.ActualValue));
        {$ENDIF}
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
