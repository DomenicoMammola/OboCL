// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiKGridAsPivotPlate;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}


uses
  kgrids, Classes, Controls, Menus, Dialogs,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}
  mPivoter, mDataProviderInterfaces, mFilterPanel, mFilter, mXML, mIntList, mMaps,
  mKGridAsPivotHelper, mSpreadsheetAsPivotHelper,
  UramakiBase, UramakiToolbar;

const
  WM_USER_REFRESHCHILDS = WM_USER + 11;
  WM_USER_CLEARCHILDS = WM_USER + 12;

resourcestring
  SConfigureCommandHint = 'Configure...';
  SConfigurePivotCommandHint = 'Configure pivot...';
  SConfigurePivotCommandCaption = 'Configure pivot';
  SExportPivotAsXlsCommandHint = 'Export pivot data to Excel file (.xls)';
  SExportPivotAsXlsCommandCaption = 'Export to Excel file (.xls)...';
  SConfirmFileOverwriteCaption = 'Confirm';
  SConfirmFileOverwriteMessage = 'The selected file already exists. Overwrite it?';
  SUnableToWriteFileMessage = 'Unable to write file. Check if the file is open by another application. If so, close it and run this command again. Detail:';
  SWantToOpenFileMessage = 'Do you want to open the file?';
  SExcelFileDescription = 'Excel 97-2003 files';

type

  { TUramakiKGridAsPivotPlate }

  TUramakiKGridAsPivotPlate = class(TUramakiPlate)
  strict private
    procedure InvokeChildsClear;
  protected
    FPivoter : TmPivoter;
    FDataProvider : IVDDataProvider;
    FGrid : TKGrid;
    FGridHelper : TmKGridAsPivotHelper;
    FFilterPanel : TmFilterPanel;
    FToolbar : TUramakiToolbar;
    FConfigurePopupMenu : TPopupMenu;
    FSaveDialog : TSaveDialog;

    procedure OnClearFilter (Sender : TObject);
    procedure OnExecuteFilter (Sender : TObject);
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;
    procedure ProcessClearChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_CLEARCHILDS;
    procedure CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
    procedure OnEditSettings(Sender : TObject);
    procedure OnExportToXlsFile(Sender : TObject);
    function ConfirmFileOverwrite : boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(aDataProvider : IVDDataProvider);
    procedure ClearPivot;
    procedure DrawPivot;
    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); override;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); override;
    procedure Clear; override;

    property Pivoter : TmPivoter read FPivoter;
  end;

implementation

uses
  sysutils,
  mWaitCursor, mPivotSettings, mPivotSettingsForm, mNullables;

var
  _LastUsedFolderForExport : TNullableString;

function GetLastUsedFolderForExport: TNullableString;
begin
  if not Assigned(_LastUsedFolderForExport) then
    _LastUsedFolderForExport := TNullableString.Create();
  Result := _LastUsedFolderForExport;
end;

{ TUramakiKGridAsPivotPlate }

procedure TUramakiKGridAsPivotPlate.InvokeChildsClear;
begin
  PostMessage(Self.Handle, WM_USER_CLEARCHILDS, 0, 0);
end;

procedure TUramakiKGridAsPivotPlate.OnClearFilter(Sender: TObject);
begin
  if Assigned(FFilterPanel) then
  begin
    FFilterPanel.ClearAll();
    Self.Clear;
  end;
end;

procedure TUramakiKGridAsPivotPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
begin
  try
    TWaitCursor.ShowWaitCursor('TUramakiKGridAsPivotPlate.OnExecuteFilter');

    tmpFilters := TmFilters.Create;
    try
      FFilterPanel.GetFilters(tmpFilters);
      Self.ClearPivot;
      FPivoter.Clear(false);
      Self.ReloadData(tmpFilters);
      FPivoter.Calculate;
      Self.DrawPivot;
    finally
      tmpFilters.Free;
    end;
  finally
    TWaitCursor.UndoWaitCursor('TUramakiKGridAsPivotPlate.OnExecuteFilter');
  end;
end;

procedure TUramakiKGridAsPivotPlate.ProcessClearChilds(var Message: TLMessage);
begin
  EngineMediator.PleaseClearMyChilds(Self);
end;

procedure TUramakiKGridAsPivotPlate.CreateToolbar(aImageList: TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex: integer);
var
  mItm : TMenuItem;
begin
  FToolbar := TUramakiToolbar.Create(Self);
  FToolbar.Images := aImageList;
  FToolbar.Parent := Self;

  FConfigurePopupMenu := TPopupMenu.Create(FToolbar);

  with FToolbar.AddDropDownButton(FConfigurePopupMenu) do
  begin
    Hint := SConfigureCommandHint;
    ImageIndex:= aConfigureImageIndex;
    Kind := bkIcon;
  end;

  mItm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(mItm);
  mItm.OnClick:= Self.OnEditSettings;
  mItm.Hint:= SConfigurePivotCommandHint;
  mItm.Caption:= SConfigurePivotCommandCaption;

  mItm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(mItm);
  mItm.Caption:= '-';

  mItm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(mItm);
  mItm.OnClick:= Self.OnExportToXlsFile;
  mItm.Hint:= SExportPivotAsXlsCommandHint;
  mItm.Caption:= SExportPivotAsXlsCommandCaption;

  FToolbar.Update;
end;

procedure TUramakiKGridAsPivotPlate.OnEditSettings(Sender: TObject);
var
  frm : TPivotSettingsForm;
begin
  frm := TPivotSettingsForm.Create(nil);
  try
    frm.Init(FPivoter);
    if frm.ShowModal = mrOk then
    begin
      if frm.SomethingChanged then
      begin
        try
          TWaitCursor.ShowWaitCursor('TUramakiKGridAsPivotPlate.OnEditSettings');
          Self.ClearPivot;
          FPivoter.Clear(true);
          frm.UpdateSettingsInPivot(FPivoter);
          FPivoter.Calculate;
          Self.DrawPivot;
        finally
          TWaitCursor.UndoWaitCursor('TUramakiKGridAsPivotPlate.OnEditSettings');
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

procedure TUramakiKGridAsPivotPlate.OnExportToXlsFile(Sender: TObject);
var
  fs : TFileStream;
  hlp : TmSpreadsheetAsPivotHelper;
begin
  FSaveDialog.DefaultExt:= 'xls';
  FSaveDialog.Filter:=SExcelFileDescription + '|*.xls';

  if FSaveDialog.FileName <> '' then
    FSaveDialog.FileName := ChangeFileExt(FSaveDialog.FileName, FSaveDialog.DefaultExt);

  if GetLastUsedFolderForExport.NotNull and DirectoryExists(GetLastUsedFolderForExport.AsString) then
    FSaveDialog.InitialDir:= GetLastUsedFolderForExport.AsString;

  if FSaveDialog.Execute then
  begin
    if FileExists(FSaveDialog.FileName) then
    begin
      if not ConfirmFileOverwrite then
        exit;
    end;
    GetLastUsedFolderForExport.Value:= ExtractFilePath(FSaveDialog.FileName);
    try
      try
        TWaitCursor.ShowWaitCursor('OnExportToXlsFile');
        fs := TFileStream.Create(FSaveDialog.FileName, fmCreate);
        try
          hlp := TmSpreadsheetAsPivotHelper.Create;
          try
            hlp.ExportPivotAsXls(fs, FPivoter);
          finally
            hlp.Free;
          end;
        finally
          fs.Free;
        end;
      finally
        TWaitCursor.UndoWaitCursor('OnExportToXlsFile');
      end;

      if MessageDlg(SWantToOpenFileMessage, mtConfirmation, mbYesNo, 0) = mrYes then
        OpenDocument(FSaveDialog.FileName);
    except
      on E:Exception do
      begin
        MessageDlg(SUnableToWriteFileMessage + sLineBreak + e.Message, mtInformation, [mbOk], 0);
      end;
    end;
  end;
end;

function TUramakiKGridAsPivotPlate.ConfirmFileOverwrite: boolean;
begin
  Result := MessageDlg(SConfirmFileOverwriteCaption, SConfirmFileOverwriteMessage, mtConfirmation, mbYesNo, 0) = mrYes;
end;

constructor TUramakiKGridAsPivotPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPivoter := TmPivoter.Create;
  FGrid := TKGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.RowCount:= 0;
  FGrid.ColCount:= 0;
  FGrid.ClearGrid;
  FGrid.Flat:= true;
  FGridHelper := TmKGridAsPivotHelper.Create;
  FGridHelper.Init(FPivoter, FGrid);
  FSaveDialog := TSaveDialog.Create(nil);
end;

destructor TUramakiKGridAsPivotPlate.Destroy;
begin
  FPivoter.Free;
  FreeAndNil(FToolbar);
  FGridHelper.Free;
  FSaveDialog.Free;
  inherited Destroy;
end;

procedure TUramakiKGridAsPivotPlate.Init(aDataProvider: IVDDataProvider);
begin
  FDataProvider := aDataProvider;
  FPivoter.DataProvider := FDataProvider;
end;

procedure TUramakiKGridAsPivotPlate.ClearPivot;
begin
  FGrid.LockUpdate;
  try
    FGrid.ClearGrid;
    FGrid.FixedCols:= 0;
    FGrid.FixedRows:= 0;
    FGrid.RowCount:= 0;
    FGrid.ColCount:= 0;
    FGridHelper.Clear;
  finally
    FGrid.UnlockUpdate;
  end;
end;

procedure TUramakiKGridAsPivotPlate.DrawPivot;
begin
  FGridHelper.ApplyPivotToGrid;
end;

procedure TUramakiKGridAsPivotPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
begin
  cursor := TmXmlElementCursor.Create(aXMLElement, 'pivot');
  try
    if cursor.Count = 1 then
      LoadPivotConfigurationToXML(FPivoter, cursor.Elements[0]);
  finally
    cursor.Free;
  end;
end;

procedure TUramakiKGridAsPivotPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
begin
  SavePivotConfigurationToXML(FPivoter, aXMLElement.AddElement('pivot'));
end;

procedure TUramakiKGridAsPivotPlate.Clear;
begin
  FGrid.LockUpdate;
  try
    Self.ClearPivot;
    FPivoter.Clear(false);
    if Assigned(FPivoter.DataProvider) then
      FPivoter.DataProvider.Clear;
  finally
    FGrid.UnlockUpdate;
  end;
  InvokeChildsClear;
end;

finalization
  FreeAndNil(_LastUsedFolderForExport);


end.
