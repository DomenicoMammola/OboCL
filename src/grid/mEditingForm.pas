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
  SysUtils, variants, StdCtrls, Buttons, CheckLst, Menus,
  OMultiPanel,
  mGridEditors, mMaps, mCalendarDialog, mUtility, mMathUtility,
  mLookupForm, mlookupformInstantQuery,
  mDataProviderFieldDefs, mNullables,
  mISO6346Utility, mDataProviderInterfaces, mBooleanDataProvider;

resourcestring
  SOkToAllBtnCaption = 'OK to all';
  SPropertyColumnTitle = 'Property';
  SValueColumnTitle = 'Value';
  SMissingValuesTitle = 'Missing values';
  SMissingValuesWarning = 'Something is wrong, some mandatory values are missing:';
  SDefaultCaption = 'Edit values';
  SErrorNotADate = 'Not a date.';
  SErrorNotADateTime = 'Not a date + time.';
  SErrorNotANumber = 'Not a number.';
  SErrorNotAColor = 'Not a color.';
  SErrorNotATime = 'Not a time.';
  SErrorNotAMonth = 'Not a month.';
  SErrorNotAYear = 'Not a year.';
  SErrorYearInThePast = 'Year in the past.';
  SErrorYearInTheFuture = 'Year in the future.';
  SErrorYearTooFaraway = 'Year too faraway.';
  SMultiEditClearValues = 'To be cleared';
  SMenuCustomizeLinesOrder = 'Customize order of lines...';
  SCustomizeLinesOrderDoneTitle = 'Success';
  SCustomizeLinesOrderDoneMessage = 'Order of lines was changed. Close and re-open the window to view the changes. Save the report to a file to make them persistent.';

type

  TmEditorLineKind = (ekSimple, ekLookup, ekDialog, ekCalendar, ekWizard, ekLookupPlusWizard, ekColorDialog, ekLookupInstantQuery, ekLookupInstantQueryPlusWizard, ekSwitchBeetwenValues);
  TmEditorLineDataType = (dtInteger, dtFloat, dtDate, dtTime, dtText, dtUppercaseText, dtContainerNumber, dtMRNNumber, dtCurrentYearOrInThePast, dtCurrentYearOrInTheFuture, dtYear, dtMonth, dtColor, dtDateTime);
  TmEditorLineReadOnlyMode = (roAllowEditing, roReadOnly, roAllowOnlySetValue);

  TmEditingPanel = class;

  TmOnEditValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aNewDisplayValue: string; const aNewActualValue : variant) of object;
  TmOnValidateValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant; out aErrorMessage : string) of object;
  TmOnShowDialogEvent = function (aSender : TmEditingPanel; const aName: string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant): boolean of object;
  TmOnActivateWizardEvent = function (aSender : TmEditingPanel; const aName: string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant): boolean of object;
  TmOnCustomizeOrderOfLines = procedure (aSender : TmEditingPanel; const aLinesList : TStringList) of object;

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
    FStartDatum : IVDDatum;

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
    property StartDatum : IVDDatum read FStartDatum write FStartDatum;
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
    FValueListPopupMenu : TPopupMenu;
    FDateCellEditor : TmExtButtonTextCellEditor;
    FButtonCellEditor : TmExtButtonTextCellEditor;
    FWizardCellEditor : TmExtButtonTextCellEditor;
    FButtonWizardCellEditor : TmExtButtonsTextCellEditor;
    FSwitchCellEditor : TmExtButtonTextCellEditor;
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
    FOnCustomizeOrderOfLinesEvent : TmOnCustomizeOrderOfLines;
    FMultiEditMode : boolean;
    FSomethingChanged : boolean;
    FCommitted : boolean;
    FLastEditorUsed : string;
    FLinesSortOrderCustomizationMenuItem : TMenuItem;

    function GetAlternateColor: TColor;
    procedure SetAlternateColor(AValue: TColor);
    procedure SetOnCustomizeOrderOfLinesEvent(AValue: TmOnCustomizeOrderOfLines);

    procedure OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnValueListEditorSelectEditor(Sender: TObject; aCol,  aRow: Integer; var Editor: TWinControl);
    procedure OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure OnPaintMemoCaptionPanel(Sender: TObject);
    procedure OnChangeMemo(Sender: TObject);
    function OnGetValueFromMainSource(const aCol, aRow : integer; out aNewDisplayValue : string; out aNewActualValue: variant): boolean;
    function OnGetValue (const aCol, aRow : integer; const aSource: TOnGetValueSource; out aNewDisplayValue : string; out aNewActualValue: variant): boolean;
    function OnGetValueFromWizard (const aCol, aRow : integer; out aNewDisplayValue : string; out aNewActualValue: variant): boolean;
    function OnValueListEditorClearValue (const aCol, aRow: integer): boolean;
    procedure OnClickClearValue(Sender: TObject);
    function ComposeCaption (const aCaption : string; const aMandatory : boolean): string;
    function GetValueFromMemo (const aName : string; const aTrimValue : boolean) : string;
    procedure CheckDate (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckTime (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckDateTime (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckInteger (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckYear(const aOldStringValue : string; const aDataType: TmEditorLineDataType; const aDistanceInMonths : integer; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckMonth(const aOldStringValue : string; const aDataType: TmEditorLineDataType; var aNewStringValue : string; var aActualValue : variant);
    procedure CheckFloat (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant; const aLineConfiguration : TmEditorLineConfiguration);
    procedure CheckColor (const aOldStringValue : string; var aNewStringValue : string; var aActualValue : variant);
    procedure OnClickLinesSortOrderCustomizationMenuItem(aSender : TObject);
  private
    procedure SetMultiEditMode(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocusInEditor;
    function CheckMandatoryLines(out aMissingValues: string): boolean;
    procedure CommitChanges;

    function AddLine(const aName : String): TmEditorLineConfiguration;

    procedure AddMemo (const aName : string; const aCaption : string; const aDefaultValue : string; const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil;
      const aReadOnly : TmEditorLineReadOnlyMode = roAllowEditing);

    function GetValue(const aName : string) : Variant;
    procedure SetValue(const aName : string; const aDisplayValue: String; const aActualValue: variant);
    function GetSelectedDatum(const aName : string) : IVDDatum;

    function GetConfigurationForLine (const aName : string): TmEditorLineConfiguration;

    procedure Run(const aCustomizedSortOrder : TStringList = nil);
    procedure Renew;

    property AlternateColor : TColor read GetAlternateColor write SetAlternateColor;
    property MultiEditMode : boolean read FMultiEditMode write SetMultiEditMode;

    property OnEditValue: TmOnEditValueEvent read FOnEditValueEvent write FOnEditValueEvent;
    property OnValidateValue: TmOnValidateValueEvent read FOnValidateValueEvent write FOnValidateValueEvent;
    property OnShowDialog : TmOnShowDialogEvent read FOnShowDialogEvent write FOnShowDialogEvent;
    property OnActivateWizard : TmOnActivateWizardEvent read FOnActivateWizardEvent write FOnActivateWizardEvent;
    property OnCustomizeOrderOfLinesEvent : TmOnCustomizeOrderOfLines read FOnCustomizeOrderOfLinesEvent write SetOnCustomizeOrderOfLinesEvent;

    property SomethingChanged : boolean read FSomethingChanged;
  end;

  { TmEditingForm }

  TmEditingForm = class (TCustomForm)
  protected
    FEditingPanel : TmEditingPanel;
    FBottomPanel: TPanel;
    FCancelBtn: TBitBtn;
    FOkBtn: TBitBtn;
    FOkToAllBtn : TBitBtn;
    FOkToAllBtnOnClick : TNotifyEvent;
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkToAllBtnClick(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    procedure SetReadOnly;
    procedure ShowOkToAllButton; virtual;
  public
    property EditingPanel: TmEditingPanel read FEditingPanel;
    property OkToAllBtnOnClick : TNotifyEvent read FOkToAllBtnOnClick write FOkToAllBtnOnClick;
  end;


implementation

uses
  {$IFDEF DEBUG}
  LazLogger,
  {$ENDIF}
  LCLType,
  Dialogs, dateutils,
  mToast, mFormSetup, mMagnificationFactor, mIntList, mBaseClassesAsObjects, mEditingFormLinesConfigurationForm, mDarkMode, mGridHelper;

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
    SelectedDatum : IVDDatum;

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
    Caption : String;
    PanelCaption : TPanel;
    DefaultValue : String;
    Memo : TMemo;
    ForceClear : boolean;
    DefaultReadOnly : boolean;
    Changed : boolean;
    ChangedValueDestination: TAbstractNullable;
    MemoHeightPercent : double;
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
    EditorKind:= ekSwitchBeetwenValues;
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
  FStartDatum := nil;
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
  SelectedDatum:= nil;
  Changed:= false;
  ForceClear:= false;
end;

destructor TEditorLine.Destroy;
begin
  Configuration.Free;
  if Assigned(SelectedDatum) then
    SelectedDatum.AsObject.Free;
  SelectedDatum := nil;

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

procedure TmEditingForm.OkToAllBtnClick(Sender: TObject);
var
  missingValues: string;
begin
  if FOkToAllBtn.Focused then
  begin
    if not FEditingPanel.CheckMandatoryLines(missingValues) then
    begin
      MessageDlg(SMissingValuesTitle, SMissingValuesWarning + sLineBreak + missingValues , mtInformation, [mbOK],0);
      exit;
    end;

    FEditingPanel.CommitChanges;

    if Assigned(FOkToAllBtnOnClick) then
      FOkToAllBtnOnClick(Sender);

    ModalResult := mrOk;
  end;
end;

constructor TmEditingForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);

  Self.OnShow:= FormShow;
  Self.Caption:= SDefaultCaption;
  Self.Position:= poMainFormCenter;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := Self;
  FBottomPanel.Align:= alBottom;
  FBottomPanel.Height:= ScaleForMagnification(50, true);
  FBottomPanel.BevelInner:= bvNone;
  FBottomPanel.BevelOuter:= bvNone;

  FOkBtn:= TBitBtn.Create(FBottomPanel);
  FOkBtn.Kind:= bkOK;

  FOkBtn.Width := ScaleForMagnification(75, true);
  FOkBtn.Height := ScaleForMagnification(30, true);
  FOkBtn.Parent:= FBottomPanel;
  FOkBtn.Left := 0;
  FOkBtn.Top := ScaleForMagnification(8, true);
  FOkBtn.Anchors:= [akTop, akRight];
  FOkBtn.DefaultCaption:= true;
  FOkBtn.OnClick:= OkBtnClick;
  FOkBtn.ModalResult:= mrNone;

  FOkToAllBtn:= TBitBtn.Create(FBottomPanel);
  FOkToAllBtn.Kind:= bkOK;
  FOkToAllBtn.Caption:= SOkToAllBtnCaption;

  FOkToAllBtn.Width := ScaleForMagnification(75, true);
  FOkToAllBtn.Height := ScaleForMagnification(30, true);
  FOkToAllBtn.Parent:= FBottomPanel;
  FOkToAllBtn.Left := 0;
  FOkToAllBtn.Top := ScaleForMagnification(8, true);
  FOkToAllBtn.Anchors:= [akTop, akRight];
  // FOkToAllBtn.DefaultCaption:= true;
  FOkToAllBtn.OnClick:= OkToAllBtnClick;
  FOkToAllBtn.ModalResult:= mrNone;
  FOkToAllBtn.Visible := false;

  FOkToAllBtnOnClick := nil;

  FCancelBtn:= TBitBtn.Create(FBottomPanel);
  FCancelBtn.Kind:= bkCancel;

  FCancelBtn.Width := ScaleForMagnification(75, true);
  FCancelBtn.Height := ScaleForMagnification(30, true);
  FCancelBtn.Parent:= FBottomPanel;
  FCancelBtn.Left := FOkBtn.Left + ScaleForMagnification(10, true) + FOkBtn.Width;
  FCancelBtn.Top := FOkBtn.Top;
  FCancelBtn.Anchors:= [akTop, akRight];
  FCancelBtn.DefaultCaption:= true;
  FCancelBtn.OnClick:= OkBtnClick;
  FCancelBtn.ModalResult:= mrCancel;

  FEditingPanel := TmEditingPanel.Create(Self);
  FEditingPanel.Parent := Self;
  FEditingPanel.Align:= alClient;
  if IsDarkModeEnabled then
    FEditingPanel.AlternateColor:= GetActiveTheme.ColorAlternateCellBg
  else
    if mGridHelper.DefaultGridAlternateColor <> clNone then
      FEditingPanel.AlternateColor:= mGridHelper.DefaultGridAlternateColor;

  SetupFormAndCenter(Self, 0.8);
  FCancelBtn.Left:= FBottomPanel.Width - ScaleForMagnification(10, true) - FCancelBtn.Width;
  FOkBtn.Left:= FCancelBtn.Left - FOkBtn.Width - ScaleForMagnification(10, true);
end;

procedure TmEditingForm.SetReadOnly;
begin
  FOkBtn.Visible:= false;
end;

procedure TmEditingForm.ShowOkToAllButton;
begin
  FOkToAllBtn.Visible:= true;
  FOkToAllBtn.Left:= FCancelBtn.Left - FOkToAllBtn.Width - ScaleForMagnification(10, true);
  FOkBtn.Left:= FOkToAllBtn.Left - FOkBtn.Width - ScaleForMagnification(10, true);
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
    begin
      if IsDarkModeEnabled then
        FValueListEditor.Canvas.Brush.Color := GetActiveTheme.ColorSelectedCellBg
      else
        FValueListEditor.Canvas.Brush.Color := TmEditingPanel.COLOR_CURRENT_ROW;
    end;

    if MultiEditMode and (aRow > 0) then
    begin
      if (FLinesByRowIndex.Find(aRow) as TEditorLine).ForceClear then
      begin
        if IsDarkModeEnabled then
          FValueListEditor.Canvas.Font.Color:= GetActiveTheme.ColorLabelText
        else
          FValueListEditor.Canvas.Font.Color:= clLtGray;
      end
      else
      if not (FLinesByRowIndex.Find(aRow) as TEditorLine).Changed then
      begin
        if IsDarkModeEnabled then
          FValueListEditor.Canvas.Font.Color:= GetActiveTheme.ColorCellText
        else
          FValueListEditor.Canvas.Font.Color:= clGray;
      end
      else
      begin
        if IsDarkModeEnabled then
          FValueListEditor.Canvas.Font.Color:= GetActiveTheme.ColorDataModifiedTitleText
        else
          FValueListEditor.Canvas.Font.Color:= clBlack;
      end;
    end
    else
    begin
      if (aRow > 0) then
        if (FLinesByRowIndex.Find(aRow) as TEditorLine).Configuration.ReadOnly = roReadOnly then
        begin
          if IsDarkModeEnabled then
            FValueListEditor.Canvas.Font.Color := GetActiveTheme.ColorDisabledCellText
          else
            FValueListEditor.Canvas.Font.Color:= clGray;
        end;
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
  begin
    if curLine.ForceClear then
      Editor := nil;
    exit;
  end;

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
  else if curLine.Configuration.EditorKind = ekSwitchBeetwenValues then
  begin
    FSwitchCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FSwitchCellEditor;
    FSwitchCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
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
  else if (curLine.Configuration.EditorKind = ekLookupPlusWizard) or (curLine.Configuration.EditorKind = ekLookupInstantQueryPlusWizard) then
  begin
    FButtonWizardCellEditor.TextEditor.Text:= FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FButtonWizardCellEditor;
    FButtonWizardCellEditor.AllowFreeTypedText:= curLine.Configuration.AllowFreeTypedText;
  end
  else
  begin
    if IsDarkModeEnabled and Assigned(Editor) and (Editor is TStringCellEditor) then
    begin
      (Editor as TStringCellEditor).Font.Color:= GetActiveTheme.ColorSelectedCellText;
      (Editor as TStringCellEditor).Color:= GetActiveTheme.ColorSelectedCellBg;
    end;
  end;

end;

procedure TmEditingPanel.OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  curLine : TEditorLine;
  errorMessage : String;
  OldActualValue : variant;
begin
  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if not Assigned(curLine) then
    exit;

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
    else if curLine.Configuration.DataType = dtDateTime then
      CheckDateTime (OldValue, NewValue, curLine.ActualValue)
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

procedure TmEditingPanel.OnPaintMemoCaptionPanel(Sender: TObject);
var
  tmpIndex : integer;
  curEditorMemo : TEditorMemo;
begin
  if MultiEditMode and (Sender is TPanel) then
  begin
    tmpIndex := (Sender as TPanel).Tag;

    if (tmpIndex >= 0) and (tmpIndex < FMemos.Count) then
    begin
      curEditorMemo := FMemos.Items[tmpIndex] as TEditorMemo;
      if curEditorMemo.ForceClear then
        (Sender as TPanel).Font.Color := clLtGray
      else if not curEditorMemo.Changed then
        (Sender as TPanel).Font.Color := clGray
      else
        (Sender as TPanel).Font.Color := clBlack;
    end;
  end;
end;

procedure TmEditingPanel.OnChangeMemo(Sender: TObject);
var
  tmpIndex : integer;
  curEditorMemo : TEditorMemo;
begin
  if MultiEditMode and (Sender is TMemo) then
  begin
    tmpIndex := (Sender as TMemo).Tag;
    if (tmpIndex >= 0) and (tmpIndex < FMemos.Count) then
    begin
      curEditorMemo := FMemos.Items[tmpIndex] as TEditorMemo;
      if (Sender as TMemo).Text <> curEditorMemo.DefaultValue then
      begin
        if not curEditorMemo.Changed then
        begin
          curEditorMemo.Changed := true;
          curEditorMemo.PanelCaption.Invalidate;
        end;
      end
      else
      begin
        if curEditorMemo.Changed then
        begin
          curEditorMemo.Changed := false;
          curEditorMemo.PanelCaption.Invalidate;
        end;
      end;
    end;
  end;
end;

function TmEditingPanel.OnGetValueFromMainSource(const aCol, aRow: integer; out aNewDisplayValue: string; out aNewActualValue: variant): boolean;
begin
  Result := OnGetValue(aCol, aRow, sMainButton, aNewDisplayValue, aNewActualValue);
end;

function TmEditingPanel.OnGetValue(const aCol, aRow : integer; const aSource: TOnGetValueSource; out aNewDisplayValue : string; out aNewActualValue: variant): boolean;
var
  calendarFrm : TmCalendarDialog;
  colorDialog : TColorDialog;
  keyFieldName, errorMessage : String;
  curLine : TEditorLine;
  curDatum : IVDDatum;
  lookupFrm : TmLookupFrm;
  lookupFrmInstantQuery : TmLookupInstantQueryFrm;
  tmpVirtualFieldDefs : TmVirtualFieldDefs;
  OldStringValue : string;
  OldActualValue : Variant;
  tmpColor : TColor;
  minFields : TStringList;
  i, k : integer;
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
  else if (curLine.Configuration.EditorKind = ekWizard) or ((curLine.Configuration.EditorKind = ekLookupPlusWizard) and (aSource = sWizardButton))
    or ((curLine.Configuration.EditorKind = ekLookupInstantQueryPlusWizard) and (aSource = sWizardButton))then
  begin
    if Assigned(FOnActivateWizardEvent) then
    begin
      FLastEditorUsed:= curLine.Name;
      Result := FOnActivateWizardEvent(Self, curLine.Name, OldStringValue, aNewDisplayValue, OldActualValue, curLine.ActualValue);
    end;
  end
  else if (curLine.Configuration.EditorKind = ekLookupInstantQuery) or ((curLine.Configuration.EditorKind = ekLookupInstantQueryPlusWizard) and (aSource = sMainButton)) then
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
        if Assigned(curLine.SelectedDatum) then
          curLine.SelectedDatum.AsObject.Free;
        curLine.SelectedDatum := lookupFrmInstantQuery.SelectedDatum.Clone;

        FLastEditorUsed:= curLine.Name;

        Result := true;
      end;
    finally
      lookupFrmInstantQuery.Free;
    end;
  end
  else if curLine.Configuration.EditorKind = ekSwitchBeetwenValues then
  begin
    assert (Assigned(curLine.Configuration.DataProvider));
    if curLine.Configuration.DataProvider.Count > 0 then
    begin
      minFields := TStringList.Create;
      try
        curLine.Configuration.DataProvider.GetMinimumFields(minFields);
        if not VarIsNull(OldActualValue) then
        begin
          for i := 0 to curLine.Configuration.DataProvider.Count - 1 do
          begin
            if VarToStr(curLine.Configuration.DataProvider.GetDatum(i).GetPropertyByFieldName(curLine.Configuration.DataProvider.GetKeyFieldName)) = VarToStr(OldActualValue) then
            begin
              k := i + 1;
              if k >= curLine.Configuration.DataProvider.Count then
                k := 0;
              aNewDisplayValue:= ConcatenateFieldValues(curLine.Configuration.DataProvider.GetDatum(k), minFields);
              curLine.ActualValue:=curLine.Configuration.DataProvider.GetDatum(k).GetPropertyByFieldName(curLine.Configuration.DataProvider.GetKeyFieldName);
              FLastEditorUsed:= curLine.Name;
              Result := true;
              break;
            end
          end;
        end
        else
        begin
          aNewDisplayValue:= ConcatenateFieldValues(curLine.Configuration.DataProvider.GetDatum(0), minFields);
          curLine.ActualValue:=curLine.Configuration.DataProvider.GetDatum(0).GetPropertyByFieldName(curLine.Configuration.DataProvider.GetKeyFieldName);
          FLastEditorUsed:= curLine.Name;
          Result := true;
        end;
      finally
        minFields.Free;
      end;
    end;
  end
  else if (curLine.Configuration.EditorKind = ekLookup) or ((curLine.Configuration.EditorKind = ekLookupPlusWizard) and (aSource = sMainButton)) then
  begin
    assert (Assigned(curLine.Configuration.DataProvider));

    lookupFrm := TmLookupFrm.Create(Self);
    try
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

function TmEditingPanel.OnGetValueFromWizard(const aCol, aRow: integer; out aNewDisplayValue: string; out aNewActualValue: variant): boolean;
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
var
  obj : TObject;
begin
  if Sender is TCheckListBox then
  begin
    obj := (Sender as TCheckListBox).Items.Objects[(Sender as TCheckListBox).ItemIndex];
    if obj is TEditorLine then
    begin
      (obj as TEditorLine).ForceClear:= (Sender as TCheckListBox).Checked[(Sender as TCheckListBox).ItemIndex];
      FValueListEditor.Invalidate;
    end
    else if obj is TEditorMemo then
    begin
      (obj as TEditorMemo).ForceClear:= (Sender as TCheckListBox).Checked[(Sender as TCheckListBox).ItemIndex];
      (obj as TEditorMemo).Memo.ReadOnly := (obj as TEditorMemo).ForceClear or (obj as TEditorMemo).DefaultReadOnly;
      (obj as TEditorMemo).PanelCaption.Invalidate;
    end;
  end;
end;

function TmEditingPanel.ComposeCaption(const aCaption: string; const aMandatory: boolean): string;
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
  position, mainPanelPosition : double;
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
  tmpPanel2.OnPaint:= OnPaintMemoCaptionPanel;
  if IsDarkModeEnabled then
  begin
    tmpPanel2.Color:= GetActiveTheme.ColorBg;
    tmpPanel2.Font.Color:= GetActiveTheme.ColorCellText;
      tmpPanel2.BevelColor:= GetActiveTheme.ColorGridLines;
  end;
  tmpMemo:= TMemo.Create(tmpPanel1);
  tmpMemo.Parent := tmpPanel1;
  tmpMemo.Align:= alClient;
  tmpMemo.ScrollBars:= ssVertical;
  tmpMemo.WantReturns:= true;
  tmpMemo.Text:= aDefaultValue;
  tmpMemo.OnChange:= OnChangeMemo;
  if IsDarkModeEnabled then
  begin
    tmpMemo.Color:= GetActiveTheme.ColorCellBg;
    tmpMemo.Font.Color:= GetActiveTheme.ColorCellText;
  end;

  tmpEditorMemo := TEditorMemo.Create;
  tmpEditorMemo.ForceClear:= false;
  tmpEditorMemo.Name:= aName;
  tmpEditorMemo.Caption := aCaption;
  tmpEditorMemo.PanelCaption := tmpPanel2;
  tmpEditorMemo.Memo := tmpMemo;
  tmpEditorMemo.ChangedValueDestination := aChangedValueDestination;
  tmpEditorMemo.Memo.ReadOnly := (aReadOnly <> roAllowEditing);
  tmpEditorMemo.DefaultReadOnly := tmpEditorMemo.Memo.ReadOnly;
  tmpEditorMemo.MemoHeightPercent:= aMemoHeightPercent;
  tmpEditorMemo.DefaultValue:= aDefaultValue;
  tmpEditorMemo.Changed:= false;

  FMemos.Add(tmpEditorMemo);
  FMemosByName.Add(aName, tmpEditorMemo);
  tmpPanel2.Tag:= FMemos.Count - 1;
  tmpMemo.Tag:= FMemos.Count -1;

  mainPanelPosition := 1;
  for i := 0 to FMemos.Count - 1 do
    mainPanelPosition:= mainPanelPosition - (FMemos.Items[i] as TEditorMemo).MemoHeightPercent;

  FRootPanel.PanelCollection.Items[FRootPanel.PanelCollection.Count - 1].Position:= 1;
  position := 1;
  for i := FRootPanel.PanelCollection.Count - 2 downto 1  do
  begin
    position := position - (FMemos.Items[i] as TEditorMemo).MemoHeightPercent;
    FRootPanel.PanelCollection.Items[i].Position := position;
  end;
  FRootPanel.PanelCollection.Items[0].Position := mainPanelPosition;
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

procedure TmEditingPanel.Run (const aCustomizedSortOrder : TStringList = nil);
var
  j, k : integer;
  curLine : TEditorLine;
  str : String;
  curDatum : IVDDatum;
  curValue : Variant;
  sortedLines : TIntegerList;
  indexLines : TmStringDictionary;
  addedLines : TmStringDictionary;
  curIndex : TIntegerObject;
begin
  sortedLines := TIntegerList.Create;
  try
    if Assigned(aCustomizedSortOrder) then
    begin
      indexLines := TmStringDictionary.Create(true);
      addedLines := TmStringDictionary.Create(false);
      try
        for j := 0 to FLines.Count - 1 do
        begin
          curLine := FLines.Items[j] as TEditorLine;
          indexLines.Add(curLine.Name, TIntegerObject.Create(j));
        end;

        for j := 0 to aCustomizedSortOrder.Count - 1 do
        begin
          if not addedLines.Contains(aCustomizedSortOrder.Strings[j]) then
          begin
            curIndex := indexLines.Find(aCustomizedSortOrder.Strings[j]) as TIntegerObject;
            if Assigned(curIndex) then
            begin
              sortedLines.Add(curIndex.Value);
              addedLines.Add(aCustomizedSortOrder.Strings[j], addedLines);
            end;
          end;
        end;

        for j := 0 to FLines.Count -1 do
        begin
          curLine := FLines.Items[j] as TEditorLine;
          if not addedLines.Contains(curLine.Name) then
            sortedLines.Add(j);
        end;
      finally
        indexLines.Free;
        addedLines.Free;
      end;
    end
    else
    begin
      for j := 0 to FLines.Count - 1 do
        sortedLines.Add(j);
    end;

    for j := 0 to sortedLines.Count - 1 do
    begin
      curLine := FLines.Items[sortedLines.Items[j]] as TEditorLine;

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
      else if (curLine.Configuration.DataType = dtDateTime) and (curLine.Configuration.ChangedValueDestination is TNullableDateTime) then
        str := (curLine.Configuration.ChangedValueDestination as TNullableDateTime).AsString(true)
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
      else if (curLine.Configuration.EditorKind = ekSwitchBeetwenValues) then
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
      else if ((curLine.Configuration.EditorKind = ekLookupInstantQuery) or (curLine.Configuration.EditorKind = ekLookupInstantQueryPlusWizard)) and Assigned(curLine.Configuration.StartDatum) then
      begin
        if (curLine.Configuration.DisplayLabelFieldNames.Count = 0)  then
          curLine.Configuration.InstantQueryManager.GetDataProvider.GetMinimumFields(curLine.Configuration.DisplayLabelFieldNames);
        str := ConcatenateFieldValues(curLine.Configuration.StartDatum, curLine.Configuration.DisplayLabelFieldNames);
      end
      else
        str := curLine.Configuration.ChangedValueDestination.AsString;

      curLine.Index:= FValueListEditor.InsertRow(ComposeCaption(curLine.Configuration.Caption, curLine.Configuration.Mandatory), str, true);

      FLinesByRowIndex.Add(curLine.RowIndex, curLine);
      FValueListEditor.ItemProps[curLine.Index].ReadOnly:= (curLine.Configuration.ReadOnly <> roAllowEditing);
      if (curLine.Configuration.ReadOnly = roAllowEditing) and ((curLine.Configuration.EditorKind = ekCalendar) or (curLine.Configuration.EditorKind = ekLookup)
        or (curLine.Configuration.EditorKind = ekColorDialog) or (curLine.Configuration.EditorKind = ekLookupInstantQuery)
        or (curLine.Configuration.EditorKind = ekDialog) or (curLine.Configuration.EditorKind = ekWizard) or (curLine.Configuration.EditorKind = ekLookupPlusWizard) or (curLine.Configuration.EditorKind = ekLookupInstantQueryPlusWizard)) then
          FValueListEditor.ItemProps[curLine.Index].EditStyle:=esEllipsis;

      if MultiEditMode and Assigned(FClearValuesList) then
        FClearValuesList.AddItem(curLine.Configuration.Caption, curLine);
    end;
  finally
    sortedLines.Free;
  end;

  if MultiEditMode and Assigned(FClearValuesList) then
  begin
    for k := 0 to FMemos.Count - 1 do
      FClearValuesList.AddItem((FMemos.Items[k] as TEditorMemo).Caption, FMemos.Items[k]);
  end;
end;

procedure TmEditingPanel.Renew;
var
  i : integer;
  curLine : TEditorLine;
begin
  for i := 0 to FLines.Count -1 do
  begin
    curLine := FLines.Items[i] as TEditorLine;
    if Assigned(curLine) then
    begin
      FValueListEditor.ItemProps[curLine.Index].ReadOnly:= (curLine.Configuration.ReadOnly <> roAllowEditing);
      FValueListEditor.Keys[curLine.RowIndex] := ComposeCaption(curLine.Configuration.Caption, curLine.Configuration.Mandatory)
    end;
  end;
  FValueListEditor.Invalidate;
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

function TmEditingPanel.GetSelectedDatum(const aName: string): IVDDatum;
var
  curLine : TEditorLine;
begin
  Result := nil;

  if FLinesByName.Contains(aName) then
  begin
    curLine := FLinesByName.Find(aName) as TEditorLine;
    Result := curLine.SelectedDatum;
  end
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

procedure TmEditingPanel.CheckDateTime(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
var
  vDate, vTime : TDateTime;
  i : integer;
  dateString, timeString : String;
begin
  vDate := 0;
  if aNewStringValue <> '' then
  begin
    dateString := aNewStringValue;
    timeString := '';

    i := Pos(' ', aNewStringValue);
    if i > 0 then
    begin
      dateString:= Trim(Copy(aNewStringValue, 1, i));
      timeString:= Trim(Copy(aNewStringValue, i, 999));
    end;

    if TryToUnderstandDateString(dateString, vDate) then
    begin
      if timeString <> '' then
      begin
        if TryToUnderstandTimeString(timeString, vTime) then
        begin
          aNewStringValue:= DateToStr(vDate) + ' ' + TimeToStr(vTime);
          aActualValue := vDate + vTime;
        end
        else
        begin
          aNewStringValue := aOldStringValue;
          TmToast.ShowText(SErrorNotADateTime);
        end;
      end
      else
      begin
        aNewStringValue := DateToStr(vDate);
        aActualValue := vDate;
      end;
    end
    else
    begin
      aNewStringValue := aOldStringValue;
      TmToast.ShowText(SErrorNotADateTime);
    end;
  end
  else
    aActualValue := null;
end;

procedure TmEditingPanel.CheckInteger(const aOldStringValue: string; var aNewStringValue: string; var aActualValue: variant);
begin
  if aNewStringValue <> '' then
  begin
    if not IsNumeric(aNewStringValue, false, true) then
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
    if not IsNumeric(aNewStringValue, false, false) then
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
    if not IsNumeric(aNewStringValue, false, false) then
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

procedure TmEditingPanel.OnClickLinesSortOrderCustomizationMenuItem(aSender: TObject);
var
  frm : TEditingFormLinesSettingsForm;
  settings : TEditingFormLinesSettings;
  i : integer;
  el : TEditorLine;
  linesList : TStringList;
begin
  if not Assigned(FOnCustomizeOrderOfLinesEvent) then
    exit;

  frm := TEditingFormLinesSettingsForm.Create(Self);
  settings := TEditingFormLinesSettings.Create;
  try
    for i := 1 to FLines.Count do
    begin
      el := FLinesByRowIndex.Find(i) as TEditorLine;
      settings.Add(TEditingFormLineSettings.Create(el.Name, el.Configuration.Caption, el.Configuration.Mandatory));
    end;
    frm.Init(settings);
    if frm.ShowModal = mrOK then
    begin
      linesList := TStringList.Create;
      try
        frm.ExtractSettings(linesList);
        Self.FOnCustomizeOrderOfLinesEvent(Self, linesList);
        MessageDlg(SCustomizeLinesOrderDoneTitle, SCustomizeLinesOrderDoneMessage, mtInformation, [mbOk], 0);
      finally
        linesList.Free;
      end;
    end;
  finally
    frm.Free;
    settings.Free;
  end;
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

  if IsDarkModeEnabled then
  begin
    FRootPanel.SplitterColor:= GetActiveTheme.ColorBg;
    FRootPanel.SplitterHoverColor:= GetActiveTheme.ColorSelectedCellBg;
  end;

  FCommitted:= false;
  FMultiEditMode:= false;
  FSomethingChanged:= false;

  FValueListEditor:= TmValueListEditor.Create(FRootPanel);
  FValueListEditor.Parent := FRootPanel;
  FValueListEditor.Align:= alClient;
  FRootPanel.PanelCollection.AddControl(FValueListEditor);
  FValueListEditor.Height:= 200;
  if not IsDarkModeEnabled then
    FValueListEditor.AlternateColor := clMoneyGreen;
  FValueListEditor.AutoAdvance := aaDown;
  FValueListEditor.DefaultColWidth := 400;
  FValueListEditor.FixedCols := 0;
  FValueListEditor.Flat := True;
  FValueListEditor.RowCount := 2;
  FValueListEditor.TabOrder := 0;
  FValueListEditor.OnPrepareCanvas := Self.OnValueListEditorPrepareCanvas;
  FValueListEditor.OnSelectEditor := Self.OnValueListEditorSelectEditor;
  FValueListEditor.OnValidateEntry := Self.OnValueListEditorValidateEntry;
  FValueListEditor.TitleCaptions.Add(SPropertyColumnTitle);
  FValueListEditor.TitleCaptions.Add(SValueColumnTitle);
  FValueListEditor.ColWidths[0] := 400;
  FValueListEditor.ColWidths[1] := 500;

  if IsDarkModeEnabled then
  begin
    FValueListEditor.FixedColor:= GetActiveTheme.ColorBg;
    FValueListEditor.FixedGridLineColor:= GetActiveTheme.ColorGridLines;
    FValueListEditor.Color:= GetActiveTheme.ColorCellBg;
    FValueListEditor.Font.Color:= GetActiveTheme.ColorCellText;
  end;
  ScaleFontForMagnification(FValueListEditor.Font);

  FValueListPopupMenu := TPopupMenu.Create(FValueListEditor);
  FValueListEditor.PopupMenu := FValueListPopupMenu;

  FDateCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FDateCellEditor.Visible := false;
  FDateCellEditor.AllowFreeTypedText := true;
  FDateCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FDateCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FDateCellEditor.ParentGrid := FValueListEditor;
  FDateCellEditor.ButtonStyle:= cebsCalendar;
  if IsDarkModeEnabled then
  begin
    FDateCellEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FDateCellEditor.TextEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FDateCellEditor.Color:= GetActiveTheme.ColorSelectedCellBg;
    ScaleFontForMagnification(FDateCellEditor.Font);
    ScaleFontForMagnification(FDateCellEditor.TextEditor.Font);
  end;

  FButtonCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FButtonCellEditor.Visible:= false;
  FButtonCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FButtonCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FButtonCellEditor.ParentGrid := FValueListEditor;
  FButtonCellEditor.AllowFreeTypedText:= false;
  if IsDarkModeEnabled then
  begin
    FButtonCellEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FButtonCellEditor.TextEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FButtonCellEditor.Color:= GetActiveTheme.ColorSelectedCellBg;
  end;
  ScaleFontForMagnification(FButtonCellEditor.Font);
  ScaleFontForMagnification(FButtonCellEditor.TextEditor.Font);

  FWizardCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FWizardCellEditor.Visible:= false;
  FWizardCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FWizardCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FWizardCellEditor.ParentGrid := FValueListEditor;
  FWizardCellEditor.AllowFreeTypedText:= false;
  FWizardCellEditor.ButtonStyle:= cebsMagicWand;
  if IsDarkModeEnabled then
  begin
    FWizardCellEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FWizardCellEditor.TextEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FWizardCellEditor.Color:= GetActiveTheme.ColorSelectedCellBg;
  end;
  ScaleFontForMagnification(FWizardCellEditor.Font);
  ScaleFontForMagnification(FWizardCellEditor.TextEditor.Font);

  FSwitchCellEditor := TmExtButtonTextCellEditor.Create(Self);
  FSwitchCellEditor.Visible:= false;
  FSwitchCellEditor.OnGetValueEvent:= Self.OnGetValueFromMainSource;
  FSwitchCellEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FSwitchCellEditor.ParentGrid := FValueListEditor;
  FSwitchCellEditor.AllowFreeTypedText:= false;
  FSwitchCellEditor.ButtonStyle:= cebsSwitchBetweenValues;
  if IsDarkModeEnabled then
  begin
    FSwitchCellEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FSwitchCellEditor.TextEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FSwitchCellEditor.Color:= GetActiveTheme.ColorSelectedCellBg;
  end;
  ScaleFontForMagnification(FSwitchCellEditor.Font);
  ScaleFontForMagnification(FSwitchCellEditor.TextEditor.Font);

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
  if IsDarkModeEnabled then
  begin
    FButtonWizardCellEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FButtonWizardCellEditor.TextEditor.Font.Color:= GetActiveTheme.ColorSelectedCellText;
    FButtonWizardCellEditor.Color:= GetActiveTheme.ColorSelectedCellBg;
  end;
  ScaleFontForMagnification(FButtonWizardCellEditor.Font);
  ScaleFontForMagnification(FButtonWizardCellEditor.TextEditor.Font);

  FLinesByName := TmStringDictionary.Create();
  FLinesByRowIndex := TmIntegerDictionary.Create();
  FLines := TObjectList.Create(true);
  FMemos := TObjectList.Create(true);
  FMemosByName := TmStringDictionary.Create();

  FOnEditValueEvent:= nil;
  FOnValidateValueEvent:= nil;
  FOnShowDialogEvent:= nil;
  FOnActivateWizardEvent:= nil;
  FOnCustomizeOrderOfLinesEvent:= nil;
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
  FValueListEditor.ColWidths[0] := trunc(FValueListEditor.Width * 0.4);
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
      if IsDarkModeEnabled then
      begin
        tmpPanel.Color:= GetActiveTheme.ColorBg;
        tmpPanel.Font.Color:= GetActiveTheme.ColorTitleText;
      end;
    end;
    if not Assigned(FClearValuesList) then
    begin
      FClearValuesList := TCheckListBox.Create(FClearValuesPanel);
      FClearValuesList.Parent := FClearValuesPanel;
      FClearValuesList.Align:= alClient;
      FClearValuesList.OnClickCheck:= Self.OnClickClearValue;
      if IsDarkModeEnabled then
      begin
        FClearValuesList.Color:= GetActiveTheme.ColorCellBg;
        FClearValuesList.Font.Color:= GetActiveTheme.ColorCellText;
      end;
    end;
  end
  else
  begin
    if Assigned(FClearValuesPanel) then
      FClearValuesPanel.Visible:= false;
  end;
end;

procedure TmEditingPanel.SetOnCustomizeOrderOfLinesEvent(AValue: TmOnCustomizeOrderOfLines);
begin
  FOnCustomizeOrderOfLinesEvent:=AValue;
  if Assigned(FOnCustomizeOrderOfLinesEvent) then
  begin
    if not Assigned(FLinesSortOrderCustomizationMenuItem) then
    begin
      FLinesSortOrderCustomizationMenuItem := TMenuItem.Create(FValueListPopupMenu);
      FValueListPopupMenu.Items.Add(FLinesSortOrderCustomizationMenuItem);
      FLinesSortOrderCustomizationMenuItem.Caption:= SMenuCustomizeLinesOrder;
      FLinesSortOrderCustomizationMenuItem.OnClick:= OnClickLinesSortOrderCustomizationMenuItem;
    end;
  end
  else
  begin
    if Assigned(FLinesSortOrderCustomizationMenuItem) then
      FLinesSortOrderCustomizationMenuItem.Visible:= false;
  end;
end;

function TmEditingPanel.CheckMandatoryLines(out aMissingValues: string): boolean;
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
    if Assigned(tmpMemo.ChangedValueDestination) and (tmpMemo.ForceClear or (not tmpMemo.Memo.ReadOnly)) then
    begin
      if tmpMemo.ForceClear then
        tmpMemo.ChangedValueDestination.CheckIfDifferentAndAssign(null)
      else if (not MultiEditMode) or (MultiEditMode and tmpMemo.Changed) then
        tmpMemo.ChangedValueDestination.CheckIfDifferentAndAssign(Self.GetValue(tmpMemo.Name));
      FSomethingChanged := FSomethingChanged or tmpMemo.ChangedValueDestination.TagChanged;
    end;
  end;
  FCommitted := true;
end;


end.
