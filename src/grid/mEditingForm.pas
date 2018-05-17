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
  SysUtils, variants, StdCtrls, Buttons,
  oMultiPanelSetup, OMultiPanel,
  mGridEditors, mMaps, mCalendarDialog, mUtility, mMathUtility, mLookupForm,
  mQuickReadOnlyVirtualDataSet, mVirtualDataSet, mVirtualFieldDefs, mNullables,
  mISO6346Utility, mVirtualDataSetInterfaces, mBooleanDataProvider, mVirtualDatasetDataProvider;

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

  TmEditorLineKind = (ekInteger, ekFloat, ekDate, ekTime, ekLookupText, ekLookupInteger, ekLookupFloat, ekText, ekUppercaseText, ekContainerNumber, ekMRNNumber);

  TmEditingPanel = class;

  TmOnEditValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aNewDisplayValue: string; const aNewActualValue : variant) of object;
  TmOnValidateValueEvent = procedure (aSender : TmEditingPanel; const aName : string; const aOldDisplayValue : String; var aNewDisplayValue : String; const aOldActualValue: Variant; var aNewActualValue: variant) of object;

  { TmEditorLineConfiguration }

  TmEditorLineConfiguration = class
  strict private
    FDataProvider: TmDatasetDataProvider;
    FDisplayLabelFieldName: string;
    FAlternativeKeyFieldName : string;
    FFieldsForLookup : TStringList;
    FBooleanProvider : TBooleanDatasetDataProvider;
    FCaption: String;
    FReadOnly: boolean;
    FMandatory: boolean;
    FChangedValueDestination: TAbstractNullable;

    function GetUseBooleanProvider: boolean;
    procedure SetUseBooleanProvider(AValue: boolean);
  private
    FEditorKind: TmEditorLineKind;

    class procedure ExtractFields(aVirtualFields: TmVirtualFieldDefs; aList: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    property EditorKind : TmEditorLineKind read FEditorKind write FEditorKind;
    property DataProvider : TmDatasetDataProvider read FDataProvider write FDataProvider;

    property Caption: String read FCaption write FCaption;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property Mandatory: boolean read FMandatory write FMandatory;
    property ChangedValueDestination: TAbstractNullable read FChangedValueDestination write FChangedValueDestination;

    property DisplayLabelFieldName: string read FDisplayLabelFieldName write FDisplayLabelFieldName;
    property AlternativeKeyFieldName : string read FAlternativeKeyFieldName write FAlternativeKeyFieldName;
    property FieldsForLookup : TStringList read FFieldsForLookup;
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
    FCustomDateEditor : TmExtStringCellEditor;
    FCustomEditor : TmExtStringCellEditor;
    FLinesByName : TmStringDictionary;
    FLinesByRowIndex : TmIntegerDictionary;
    FMemosByName : TmStringDictionary;
    FLines : TObjectList;
    FMemos : TObjectList;
    FOnEditValueEvent: TmOnEditValueEvent;
    FOnValidateValueEvent: TmOnValidateValueEvent;
    FMultiEditMode : boolean;
    FSomethingChanged : boolean;
    FCommitted : boolean;

    function GetAlternateColor: TColor;
    procedure SetAlternateColor(AValue: TColor);

    procedure OnValueListEditorPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure OnValueListEditorSelectEditor(Sender: TObject; aCol,  aRow: Integer; var Editor: TWinControl);
    procedure OnValueListEditorValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    function OnValueListEditorEditValue  (const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
    function OnValueListEditorClearValue (const aCol, aRow: integer): boolean;
    function ComposeCaption (const aCaption : string; const aMandatory : boolean): string;
    function GetValueFromMemo (const aName : string; const aTrimValue : boolean) : string;
  private
    function CheckMandatoryLines(var aMissingValues: string): boolean;
    procedure CommitChanges;
    procedure SetFocusInEditor;
  protected
    procedure SetReadOnly (const aName : string; const aValue : boolean); overload;
    procedure SetReadOnly (const aValue : boolean); overload;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function AddLine(const aName : String): TmEditorLineConfiguration;

    procedure AddMemo (const aName : string; const aCaption : string; const aDefaultValue : string; const aMemoHeightPercent : double; const aChangedValueDestination : TAbstractNullable = nil);

    function GetValue(const aName : string) : Variant;
    procedure SetValue(const aName : string; const aDisplayValue: String; const aActualValue: variant);

    function GetConfigurationForLine (const aName : string): TmEditorLineConfiguration;

    procedure Run;

    property AlternateColor : TColor read GetAlternateColor write SetAlternateColor;
    property OnEditValue: TmOnEditValueEvent read FOnEditValueEvent write FOnEditValueEvent;
    property OnValidateValue: TmOnValidateValueEvent read FOnValidateValueEvent write FOnValidateValueEvent;

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
    Index: integer;

    ActualValue: variant;
    Changed: boolean;
    Configuration: TmEditorLineConfiguration;
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
  end
  else
  begin
    if Assigned(FDataProvider) and Assigned(FBooleanProvider) then
      FDataProvider := nil;
    FreeAndNil(FBooleanProvider);
  end;
end;

class procedure TmEditorLineConfiguration.ExtractFields(aVirtualFields: TmVirtualFieldDefs; aList: TStringList);
var
  i : integer;
begin
  aList.Clear;
  for i := 0 to aVirtualFields.Count -1 do
    aList.Add(aVirtualFields.VirtualFieldDefs[i].Name);
end;

constructor TmEditorLineConfiguration.Create;
begin
  FDataProvider := nil;
  FDisplayLabelFieldName := '';
  FAlternativeKeyFieldName:= '';
  FFieldsForLookup := TStringList.Create;
  FBooleanProvider := nil;
  FCaption:= '';
  FReadOnly:= false;
  FMandatory:= false;
  FChangedValueDestination:= nil;
//  FDefaultValue:= Null;
//  DefaultDisplayValue := Null;
end;

destructor TmEditorLineConfiguration.Destroy;
begin
  FFieldsForLookup.Free;
  FreeAndNil(FBooleanProvider);
  inherited Destroy;
end;

constructor TEditorLine.Create;
begin
  Configuration := TmEditorLineConfiguration.Create;
  Name:= '';
  Index:= 0;
  ActualValue:= Null;
  Changed:= false;
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
  if (not Assigned(curLine)) or curLine.Configuration.ReadOnly then
    exit;

  if (curLine.Configuration.EditorKind = ekDate) then
  begin
    FCustomDateEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomDateEditor;
  end
  else if (curLine.Configuration.EditorKind = ekLookupText) or (curLine.Configuration.EditorKind = ekLookupFloat) or (curLine.Configuration.EditorKind = ekLookupInteger) then
  begin
    FCustomEditor.Text := FValueListEditor.Cells[FValueListEditor.Col, FValueListEditor.Row];
    Editor := FCustomEditor;
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

  if curLine.Configuration.ReadOnly then
  begin
    NewValue := OldValue;
    exit;
  end;

  if curLine.Configuration.EditorKind = ekDate then
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
  end else if curLine.Configuration.EditorKind = ekTime then
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
  end else if curLine.Configuration.EditorKind = ekInteger then
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
  end else if curLine.Configuration.EditorKind = ekFloat then
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
  end else if curLine.Configuration.EditorKind = ekUppercaseText then
  begin
    NewValue := Uppercase(NewValue);
    if NewValue <> '' then
      curLine.ActualValue:= NewValue
    else
      curLine.ActualValue:= null;
  end else if curLine.Configuration.EditorKind = ekText then
  begin
    if NewValue <> '' then
      curLine.ActualValue:= NewValue
    else
      curLine.ActualValue:= null;
  end else if curLine.Configuration.EditorKind = ekContainerNumber then
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
  end else if curLine.Configuration.EditorKind = ekMRNNumber then
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

  if Assigned(FOnValidateValueEvent) then
    FOnValidateValueEvent(Self, curLine.Name, OldValue, NewValue, oldActualValue, curLine.ActualValue);


  if NewValue <> OldValue then
  begin
    if Assigned(FOnEditValueEvent) then
      FOnEditValueEvent(Self, curLine.Name, NewValue, curLine.ActualValue);

    curLine.Changed := curLine.Changed or (NewValue <> OldValue);
  end;
end;

function TmEditingPanel.OnValueListEditorEditValue(const aCol, aRow : integer; var aNewDisplayValue : string; var aNewActualValue: variant): boolean;
var
  calendarFrm : TmCalendarDialog;
  str, keyFieldName : String;
  curLine : TEditorLine;
  lookupFrm : TmLookupWindow;
  tmpDatasetProvider : TReadOnlyVirtualDatasetProvider;
  tmpDataset : TmVirtualDataset;
begin
  Result := false;

  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if curLine.Configuration.ReadOnly then
    exit;

  if curLine.Configuration.EditorKind = ekDate then
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
        Result := true;
      end;
    finally
      calendarFrm.Free;
    end;
  end
  else if (curLine.Configuration.EditorKind = ekLookupText) or (curLine.Configuration.EditorKind = ekLookupInteger) or (curLine.Configuration.EditorKind = ekLookupFloat) then
  begin
    lookupFrm := TmLookupWindow.Create(Self);
    try
      tmpDatasetProvider := TReadOnlyVirtualDatasetProvider.Create;
      tmpDataset := TmVirtualDataset.Create(Self);
      try
        tmpDataset.DatasetDataProvider := tmpDatasetProvider;


        assert (Assigned(curLine.Configuration.DataProvider));

        tmpDatasetProvider.Init(curLine.Configuration.DataProvider);
        curLine.Configuration.DataProvider.FillVirtualFieldDefs(tmpDatasetProvider.VirtualFieldDefs, '');

        if curLine.Configuration.FieldsForLookup.Count = 0 then
          curLine.Configuration.DataProvider.GetMinimumFields(curLine.Configuration.FieldsForLookup);
        if curLine.Configuration.FieldsForLookup.Count = 0 then
          TmEditorLineConfiguration.ExtractFields(tmpDatasetProvider.VirtualFieldDefs, curLine.Configuration.FieldsForLookup);

        tmpDataset.Active:= true;
        tmpDataset.Refresh;

        if curLine.Configuration.AlternativeKeyFieldName <> '' then
          keyFieldName := curLine.Configuration.AlternativeKeyFieldName
        else
          keyFieldName := curLine.Configuration.DataProvider.GetKeyFieldName;

        lookupFrm.Init(tmpDataset, curLine.Configuration.FieldsForLookup,
          keyFieldName, curLine.Configuration.DisplayLabelFieldName);
        if lookupFrm.ShowModal = mrOk then
        begin
          aNewDisplayValue:= lookupFrm.SelectedDisplayLabel;
          aNewActualValue:= lookupFrm.SelectedValue;

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
  end;
end;

function TmEditingPanel.OnValueListEditorClearValue(const aCol, aRow: integer): boolean;
var
  curLine: TEditorLine;
begin
  Result := false;
  curLine := FLinesByRowIndex.Find(aRow) as TEditorLine;

  if curLine.Configuration.ReadOnly then
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
    if Assigned(curLine.Configuration.ChangedValueDestination) then
      curLine.ActualValue:= curLine.Configuration.ChangedValueDestination.AsVariant;

    if (curLine.Configuration.EditorKind = ekDate) and (curLine.Configuration.ChangedValueDestination is TNullableDateTime) then
      str := (curLine.Configuration.ChangedValueDestination as TNullableDateTime).AsString(false)
    else if (curLine.Configuration.EditorKind = ekTime) and (curLine.Configuration.ChangedValueDestination is TNullableTime) then
      str := (curLine.Configuration.ChangedValueDestination as TNullableTime).AsString
    else if (curLine.Configuration.EditorKind = ekLookupText) and (curLine.Configuration.ChangedValueDestination.NotNull) then
    begin
      assert (Assigned(curLine.Configuration.DataProvider));
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
      begin
        curValue := curDatum.GetPropertyByFieldName(curLine.Configuration.DisplayLabelFieldName);
        if not VarIsNull(curValue) then
          str := VarToStr(curValue)
        else
          str := '';
      end
      else
        str := curLine.Configuration.ChangedValueDestination.AsString;
    end
    else
      str := curLine.Configuration.ChangedValueDestination.AsString;

    curLine.Index:= FValueListEditor.InsertRow(ComposeCaption(curLine.Configuration.Caption, curLine.Configuration.Mandatory), str, true);

    FLinesByRowIndex.Add(curLine.RowIndex, curLine);
    FValueListEditor.ItemProps[curLine.Index].ReadOnly:= curLine.Configuration.ReadOnly;
    if (not curLine.Configuration.ReadOnly) and ((curLine.Configuration.EditorKind = ekDate) or (curLine.Configuration.EditorKind = ekLookupFloat)
      or (curLine.Configuration.EditorKind = ekLookupInteger) or (curLine.Configuration.EditorKind = ekLookupText)) then
      FValueListEditor.ItemProps[curLine.Index].EditStyle:=esEllipsis;
  end;
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

  FCustomEditor := TmExtStringCellEditor.Create(Self);
  FCustomEditor.Visible := false;
  FCustomEditor.ReadOnly := true;
  FCustomEditor.OnShowEditorEvent:= Self.OnValueListEditorEditValue;
  FCustomEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FCustomEditor.ParentGrid := FValueListEditor;

  FCustomDateEditor := TmExtStringCellEditor.Create(Self);
  FCustomDateEditor.Visible := false;
  FCustomDateEditor.ReadOnly := false;
  FCustomDateEditor.OnShowEditorEvent:= Self.OnValueListEditorEditValue;
  FCustomDateEditor.OnClearEvent:= Self.OnValueListEditorClearValue;
  FCustomDateEditor.ParentGrid := FValueListEditor;

  FLinesByName := TmStringDictionary.Create();
  FLinesByRowIndex := TmIntegerDictionary.Create();
  FLines := TObjectList.Create(true);
  FMemos := TObjectList.Create(true);
  FMemosByName := TmStringDictionary.Create();

  FOnEditValueEvent:= nil;
  FOnValidateValueEvent:= nil;
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
    if Assigned(tmpLine.Configuration.ChangedValueDestination) and (not tmpLine.Configuration.ReadOnly) then
    begin
      if (not MultiEditMode) or  (MultiEditMode and tmpLine.Changed) then
      begin
        tmpLine.Configuration.ChangedValueDestination.CheckIfDifferentAndAssign(tmpLine.ActualValue);
        FSomethingChanged := FSomethingChanged or tmpLine.Configuration.ChangedValueDestination.TagChanged;
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
