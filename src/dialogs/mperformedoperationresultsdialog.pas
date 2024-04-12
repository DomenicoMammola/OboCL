// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mPerformedOperationResultsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, contnrs,
  attabs,
  mPerformedOperationResults;

resourcestring
  rsSummaryTabCaption = 'Summary';
  rsInfoTabCaption = 'Infos';
  rsWarningTabCaption = 'Warnings';
  rsErrorTabCaption = 'Errors';
  rsResultTabCaption = 'Results';
  rsCopyToClipboardMenuItem = 'Copy to clipboard';
  rsSaveToFileMenuItem = 'Save to file';
  rsRowsMustBeValidatedMsg = '%d rows still must be validated';
  rsTextCopiedMsg = 'Text copied to clipboard';
  rsFileExistsCaptionDlg = 'File exists';
  rsFileExistsMsgDlg = 'A file with the same name already exists. Do you want to overwrite it?';



type

  TPerformedOperationResultsDlgType = (porTabbed, porSingleList);

  { TPerformedOperationResultsDlg }

  TPerformedOperationResultsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    MainLabel: TLabel;
    BodyPanel: TPanel;
    TopPanel: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    const TAB_SUMMARY_INDEX = 0;
    const TAB_RESULT_INDEX = 1;
    const TAB_ERROR_INDEX = 2;
    const TAB_WARNING_INDEX = 3;
    const TAB_INFO_INDEX = 4;
  private
    FGarbage : TObjectList;
    FTabs : TATTabs;
    FClicks : integer;
    FNotebook : TNotebook;
    FResultsColor : TColor;
    FInfosColor : TColor;
    FWarningsColor : TColor;
    FErrorsColor : TColor;

    procedure OnTabClick (Sender: TObject);
    procedure OnDrawLBItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure OnDrawSingleListLBItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure OnDblClickLB (Sender : TObject);
    procedure OnCopyToClipboard (Sender : TObject);
    procedure OnSaveToFile (Sender : TObject);
  public
    { public declarations }
    procedure Init (const aMessage : string; const aLog : TPerformedOperationResultsAsLog; const aIfSuccessfulShowAlwaysSummaryFirst : boolean = false); overload;
    procedure Init (const aMessage : string; const aLog : TStrings); overload;
    procedure InitWithSingleList (const aMessage : string; const aLog : TPerformedOperationResultsAsLog);

    class procedure ShowResults(const aOwner: TComponent; const aMessage : string; const aLog: TPerformedOperationResultsAsLog; const aDialogType: TPerformedOperationResultsDlgType; const aIfSuccessfulShowAlwaysSummaryFirst : boolean = false); overload;
    class procedure ShowResults(const aOwner: TComponent; const aMessage : string; const aLog: TStrings); overload;
  end;


implementation

uses
  LCLType, Menus, Clipbrd, Math,
  ATListbox, ATFlatThemes,
  mFormSetup, mMagnificationFactor, mGraphicsUtility, mBaseClassesAsObjects, mToast;

{$R *.lfm}

{ TPerformedOperationResultsDlg }

procedure TPerformedOperationResultsDlg.FormCreate(Sender: TObject);
begin
  SetupFormAndCenter(Self, GetMagnificationFactor);
  FTabs := TATTabs.Create(BodyPanel);
  FTabs.Parent := BodyPanel;
  FTabs.Align:= alTop;
  FTabs.OptShowScrollMark:= false;
  FTabs.OnTabClick:= @OnTabClick;
  FTabs.OptMouseDoubleClickClose:= false;
  FTabs.OptShowPlusTab:= false;
  FTabs.OptShowXButtons:= atbxShowNone;
  FTabs.OptMouseDoubleClickClose:= false;
  FTabs.OptShowEntireColor:= true;
  FTabs.Height:= ScaleForMagnification(32, true);
  FTabs.OptTabHeight:= ScaleForMagnification(24, true);
  FTabs.OptTabWidthNormal:= ScaleForMagnification(200, true);
  FTabs.ColorBg:= clMenu;
  FTabs.ColorFont:= clInfoText;
  ScaleFontForMagnification(FTabs.Font);
  FTabs.OptMouseDragEnabled:= false; //enable drag-drop
  FTabs.OptMouseDragOutEnabled:= false; //also enable drag-drop to another controls
  FTabs.OptShowArrowsNear:= false;
  FTabs.OptShowDropMark:= false;
  FTabs.OptShowScrollMark:= false;
//  FTabs.OptButtonLayout:= '';
  FTabs.OptShowFlat:= false;
  // FTabs.OptShowAngled:= true;
  FTabs.OptSpaceSide := 0;
  FTabs.OptSpaceBetweenTabs:= 5;
  FTabs.OptActiveFontStyle:= [fsBold];
  FTabs.OptActiveFontStyleUsed:= true;

  FNotebook := TNotebook.Create(BodyPanel);
  FNotebook.Parent := BodyPanel;
  FNotebook.Align:= alClient;

  FGarbage := TObjectList.Create(true);

  ScaleFontForMagnification(MainLabel.Font);

  FResultsColor := RGBToColor(234,218,104);
  FInfosColor := RGBToColor(191,255,255);
  FWarningsColor := RGBToColor(255,156,108);
  FErrorsColor := clRed;
end;

procedure TPerformedOperationResultsDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := (FClicks = 0);
  if not CanClose then
    TmToast.ShowText(Format(rsRowsMustBeValidatedMsg, [FClicks]));
end;

procedure TPerformedOperationResultsDlg.FormDestroy(Sender: TObject);
begin
  FGarbage.Free;
end;

procedure TPerformedOperationResultsDlg.OnTabClick(Sender: TObject);
begin
  FNotebook.PageIndex := FTabs.TabIndex;
end;

procedure TPerformedOperationResultsDlg.OnDrawLBItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
var
  tmpColor: TColor;
begin
  if (Sender as TATListbox).ItemIndex = AIndex then
  begin
    tmpColor := ColorToRGB((Sender as TATListbox).Theme^.ColorBgListboxSel);
    C.Font.Color:= ColorToRGB((Sender as TATListbox).Theme^.ColorFontListboxSel);
  end
  else
  begin
    tmpColor:= ColorToRGB((Sender as TATListbox).Theme^.ColorBgListbox);
    C.Font.Color:= ColorToRGB((Sender as TATListbox).Theme^.ColorFontListbox);
  end;

  if Assigned((Sender as TATListbox).Items.Objects[AIndex]) and ((Sender as TATListbox).Items.Objects[AIndex] is TBooleanObject) then
    if ((Sender as TATListbox).Items.Objects[AIndex] as TBooleanObject).Value then
    begin
      C.Font.Color:= ColorToRGB(clBlack);
      if (Sender as TATListbox).ItemIndex = AIndex then
        tmpColor := DarkerColor(clYellow, 20)
      else
        tmpColor := clYellow;
    end;
  C.Brush.Color:=tmpColor;
  C.FillRect(ARect);
  C.TextOut(ARect.Left + (Sender as TATListbox).IndentLeft + 2 - (Sender as TATListbox).ScrollHorz, ARect.Top, (Sender as TATListbox).Items[AIndex]);
end;

procedure TPerformedOperationResultsDlg.OnDrawSingleListLBItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
var
  tmpColor: TColor;
  prefix : string;
begin
  if (Sender as TATListbox).ItemIndex = AIndex then
  begin
    tmpColor := ColorToRGB((Sender as TATListbox).Theme^.ColorBgListboxSel);
    C.Font.Color:= ColorToRGB((Sender as TATListbox).Theme^.ColorFontListboxSel);
  end
  else
  begin
    tmpColor:= ColorToRGB((Sender as TATListbox).Theme^.ColorBgListbox);
    C.Font.Color:= ColorToRGB((Sender as TATListbox).Theme^.ColorFontListbox);
  end;

  prefix := '';
  if Assigned((Sender as TATListbox).Items.Objects[AIndex]) and ((Sender as TATListbox).Items.Objects[AIndex] is TPerformedOperation) then
  begin
    if ((Sender as TATListbox).Items.Objects[AIndex] as TPerformedOperation).Level <> '' then
      prefix := '[' + ((Sender as TATListbox).Items.Objects[AIndex] as TPerformedOperation).Level + '] ';
    if ((Sender as TATListbox).Items.Objects[AIndex] as TPerformedOperation).Level = TPerformedOperation.ERROR then
      tmpColor := FErrorsColor
    else if ((Sender as TATListbox).Items.Objects[AIndex] as TPerformedOperation).Level = TPerformedOperation.WARNING then
      tmpColor := FWarningsColor
    else if ((Sender as TATListbox).Items.Objects[AIndex] as TPerformedOperation).Level = TPerformedOperation.RESULT then
      tmpColor := FResultsColor;
  end;

  C.Brush.Color:=tmpColor;
  C.FillRect(ARect);
  C.TextOut(ARect.Left + (Sender as TATListbox).IndentLeft + 2 - (Sender as TATListbox).ScrollHorz, ARect.Top, (Sender as TATListbox).Items[AIndex]);
end;

procedure TPerformedOperationResultsDlg.OnDblClickLB(Sender: TObject);
var
  i : integer;
  lb : TATListbox;
begin
  lb := (Sender as TATListbox);
  i := lb.ItemIndex;
  if i >= 0 then
  begin
    if Assigned(lb.Items.Objects[i]) and (lb.Items.Objects[i] is TBooleanObject) then
       if (lb.Items.Objects[i] as TBooleanObject).Value then
       begin
          (lb.Items.Objects[i] as TBooleanObject).Value := false;
          dec (FClicks);
          lb.Invalidate;
       end;
  end;
end;

procedure TPerformedOperationResultsDlg.OnCopyToClipboard(Sender: TObject);
var
  lb : TATListbox;
  i : integer;
  str, sep : String;
begin
  if (Sender is TMenuItem) and ((Sender as TMenuItem).Tag > 0) then
  begin
    str := '';
    sep := '';

    lb := TATListbox((Sender as TMenuItem).Tag);

    if lb.Items.Count > 0 then
    begin
      for i := 0 to lb.Items.Count - 1 do
      begin
        str := str + sep + lb.Items.Strings[i];
        sep := sLineBreak;
      end;
    end;

    if str <> '' then
    begin
      CopyTextToClipboard(str);
      TmToast.ShowText(rsTextCopiedMsg);
    end;
  end;
end;

procedure TPerformedOperationResultsDlg.OnSaveToFile(Sender: TObject);
var
  dlg : TSaveDialog;
  lb : TATListbox;
begin
  if (Sender is TMenuItem) and ((Sender as TMenuItem).Tag > 0) then
  begin
    lb := TATListbox((Sender as TMenuItem).Tag);

    if lb.Items.Count > 0 then
    begin
      dlg := TSaveDialog.Create(Self);
      try
        dlg.DefaultExt:='txt';
        dlg.Filter:='Text files|*.txt';
        if dlg.Execute then
        begin
          if (not FileExists(dlg.FileName)) or (MessageDlg(rsFileExistsCaptionDlg, rsFileExistsMsgDlg, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
            lb.Items.SaveToFile(dlg.FileName);
        end;
      finally
        dlg.Free;
      end;
    end;
  end;
end;

procedure TPerformedOperationResultsDlg.Init(const aMessage: string; const aLog: TPerformedOperationResultsAsLog; const aIfSuccessfulShowAlwaysSummaryFirst : boolean = false);
  procedure AddMemo (aIndex : integer; aLines : TStrings);
  var
    memo : TMemo;
  begin
    memo := TMemo.Create(FNotebook.Page[aIndex]);
    memo.Parent := FNotebook.Page[aIndex];
    memo.Align := alClient;
    memo.ScrollBars:= ssBoth;
    memo.Lines.AddStrings(aLines);
    memo.ReadOnly:= true;
    memo.Font.Size:= 12;
    ScaleFontForMagnification(memo.Font);
  end;

  procedure AddListBoxOperations (aIndex : integer; aOperations : TPerformedOperations);
  var
    lb : TATListbox;
    i : integer;
    shell : TBooleanObject;
    pm : TPopupMenu;
    mi : TMenuItem;
  begin
    lb := TATListbox.Create(FNotebook.Page[aIndex]);
    lb.Parent := FNotebook.Page[aIndex];
    lb.Align := alClient;
    lb.VirtualMode:= false;
    lb.OwnerDrawn:= false;
    lb.ThemedFont:= false;
    lb.OnDrawItem:= @OnDrawLBItem;
    lb.OnDblClick:= @OnDblClickLB;
    lb.Font.Size:= 12;
    for i := 0 to aOperations.Count - 1 do
    begin
      if aOperations.Get(i).MustBeValidated then
      begin
        inc(FClicks);
        shell := TBooleanObject.Create(true);
        FGarbage.Add(shell);
        lb.Items.AddObject(aOperations.Get(i).Message, shell);
        lb.OwnerDrawn:= true;
      end
      else
        lb.Items.Add(aOperations.Get(i).Message);
    end;
    ScaleFontForMagnification(lb.Font);

    pm := TPopupMenu.Create(lb);
    lb.PopupMenu := pm;
    mi := TMenuItem.Create(pm);
    pm.Items.Add(mi);
    mi.Caption:= rsCopyToClipboardMenuItem;
    mi.Tag:=PtrInt(lb);
    mi.OnClick:= @OnCopyToClipboard;

    mi := TMenuItem.Create(pm);
    pm.Items.Add(mi);
    mi.Caption:= rsSaveToFileMenuItem;
    mi.OnClick:= @OnSaveToFile;
    mi.Tag:=PtrInt(lb);
  end;
var
  clr : TColor;
  tabCaption : String;
begin
  MainLabel.Caption:= sLineBreak + aMessage;

  FTabs.AddTab(TAB_SUMMARY_INDEX, rsSummaryTabCaption, nil, false, clLtGray);
  FNotebook.Pages.Add(rsSummaryTabCaption);
  AddMemo(TAB_SUMMARY_INDEX, aLog.Messages);

  tabCaption := rsResultTabCaption;
  if aLog.Results.Count > 0 then
  begin
    clr := FResultsColor;
    tabCaption := tabCaption + ' (!)';
  end
  else
    clr := clDkGray;
  FTabs.AddTab(TAB_RESULT_INDEX, tabCaption, nil, false, clr);
  FNotebook.Pages.Add(rsResultTabCaption);
  AddListBoxOperations(TAB_RESULT_INDEX, aLog.Results);

  tabCaption := rsErrorTabCaption;
  if aLog.Errors.Count > 0 then
  begin
    clr := FErrorsColor;
    tabCaption := tabCaption + ' (!)';
  end
  else
    clr := clDkGray;
  FTabs.AddTab(TAB_ERROR_INDEX,  tabCaption, nil, false, clr);
  FNotebook.Pages.Add(rsErrorTabCaption);
  AddListBoxOperations(TAB_ERROR_INDEX, aLog.Errors);

  tabCaption := rsWarningTabCaption;
  if aLog.Warnings.Count > 0 then
  begin
    clr := FWarningsColor;
    tabCaption := tabCaption + ' (!)';
  end
  else
    clr := clDkGray;
  FTabs.AddTab(TAB_WARNING_INDEX, tabCaption, nil, false, clr);
  FNotebook.Pages.Add(rsWarningTabCaption);
  AddListBoxOperations(TAB_WARNING_INDEX, aLog.Warnings);

  tabCaption := rsInfoTabCaption;
  if aLog.Infos.Count > 0 then
  begin
    clr := FInfosColor;
    tabCaption := tabCaption + ' (!)';
  end
  else
    clr := clDkGray;
  FTabs.AddTab(TAB_INFO_INDEX,  tabCaption, nil, false, clr);
  FNotebook.Pages.Add(rsInfoTabCaption);
  AddListBoxOperations(TAB_INFO_INDEX, aLog.Infos);

  if aIfSuccessfulShowAlwaysSummaryFirst then
  begin
    if aLog.Errors.Count > 0 then
      FTabs.TabIndex:= TAB_ERROR_INDEX
    else
      FTabs.TabIndex:= TAB_SUMMARY_INDEX;
  end
  else
  begin
    if aLog.Results.Count > 0 then
      FTabs.TabIndex:= TAB_RESULT_INDEX
    else if aLog.Errors.Count > 0 then
      FTabs.TabIndex:= TAB_ERROR_INDEX
    else if aLog.Warnings.Count > 0 then
      FTabs.TabIndex:= TAB_WARNING_INDEX
    else
      FTabs.TabIndex:= TAB_SUMMARY_INDEX;
  end;
end;

procedure TPerformedOperationResultsDlg.Init(const aMessage: string; const aLog: TStrings);
var
  memo : TMemo;
begin
  MainLabel.Caption:= sLineBreak + aMessage;
  FNotebook.Visible:= false;
  FTabs.Visible:= false;
  memo := TMemo.Create(BodyPanel);
  memo.Parent := BodyPanel;
  memo.Align := alClient;
  memo.ScrollBars:= ssAutoBoth;
  memo.Lines.AddStrings(aLog);
  memo.ReadOnly:= true;
end;

procedure TPerformedOperationResultsDlg.InitWithSingleList(const aMessage: string; const aLog: TPerformedOperationResultsAsLog);
var
  lb : TATListbox;
  i : integer;
  pm : TPopupMenu;
  mi : TMenuItem;
begin
  MainLabel.Caption:= sLineBreak + aMessage;
  FNotebook.Visible:= false;
  FTabs.Visible:= false;
  lb := TATListbox.Create(BodyPanel);
  lb.Parent := BodyPanel;
  lb.Align := alClient;
  lb.VirtualMode:= false;
  lb.OwnerDrawn:= true;
  lb.ThemedFont:= false;
  lb.OnDrawItem:= @OnDrawSingleListLBItem;
  lb.Font.Size:= 12;
  ScaleFontForMagnification(lb.Font);
  for i := 0 to aLog.PerformedOperations.Count - 1 do
    lb.Items.AddObject(aLog.PerformedOperations.Get(i).Message, aLog.PerformedOperations.Get(i));

  pm := TPopupMenu.Create(lb);
  lb.PopupMenu := pm;
  mi := TMenuItem.Create(pm);
  pm.Items.Add(mi);
  mi.Caption:= rsCopyToClipboardMenuItem;
  mi.Tag:=PtrInt(lb);
  mi.OnClick:= @OnCopyToClipboard;

  mi := TMenuItem.Create(pm);
  pm.Items.Add(mi);
  mi.Caption:= rsSaveToFileMenuItem;
  mi.OnClick:= @OnSaveToFile;
  mi.Tag:=PtrInt(lb);
end;

class procedure TPerformedOperationResultsDlg.ShowResults(const aOwner: TComponent; const aMessage: string; const aLog: TPerformedOperationResultsAsLog; const aDialogType: TPerformedOperationResultsDlgType; const aIfSuccessfulShowAlwaysSummaryFirst : boolean = false);
var
  Dlg : TPerformedOperationResultsDlg;
begin
  Dlg := TPerformedOperationResultsDlg.Create(aOwner);
  try
    if aDialogType = porTabbed then
      Dlg.Init(aMessage, aLog, aIfSuccessfulShowAlwaysSummaryFirst)
    else if aDialogType = porSingleList then
      Dlg.InitWithSingleList(aMessage, aLog);
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

class procedure TPerformedOperationResultsDlg.ShowResults(const aOwner: TComponent; const aMessage: string; const aLog: TStrings);
var
  Dlg : TPerformedOperationResultsDlg;
begin
  Dlg := TPerformedOperationResultsDlg.Create(aOwner);
  try
    Dlg.Init(aMessage, aLog);
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;


end.

