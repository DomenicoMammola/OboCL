// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB, Dialogs, Forms, Graphics, ComCtrls,
  DBGrids, Controls, Menus, LCLIntf, Grids,
  ATButtons,
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats, // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  mGridColumnSettings, mXML,
  mGridSettingsForm, mFormulaFieldsConfigurationForm,
  mDBGrid, mDrawGrid, mNullables, mGrids, mFields,
  mVirtualDatasetFormulas, mCellDecorations,
  mCellDecorationsConfigurationForm;

resourcestring
  SCSVFileDescription = 'Comma Separated Values files';
  SExcelFileDescription = 'Excel files';
  SHtmlFileDescription = 'Html files';
  SUnableToWriteFileMessage = 'Unable to write file. Check if the file is open by another application. If so, close it and run this command again. Detail:';
  SConfirmFileOverwriteCaption = 'Confirm';
  SConfirmFileOverwriteMessage = 'The selected file already exists. Overwrite it?';
  SConfirmDiscordedFileExtCaption = 'Confirm';
  SConfirmDiscordedFileExtMessage = 'The selected file is not a %s file. Continue anyway?';
  SWantToOpenFileMessage = 'Do you want to open the file?';
  SConfigureCommandHint = 'Configure...';
  SConfigureGridCommandHint = 'Configure grid...';
  SConfigureGridCommandCaption = 'Configure grid';
  SConfigureFormulaFieldsCommandHint = 'Configure formula fields...';
  SConfigureFormulaFieldsCommandCaption = 'Configure formula fields';
  SConfigureCellDecorationsCommandHint = 'Configure cell decorations...';
  SConfigureCellDecorationsCommandCaption = 'Configure cell decorations';
  SExportGridAsCsvCommandHint = 'Export grid data to csv file';
  SExportGridAsCsvCommandCaption = 'Export to csv file...';
  SExportGridAsXlsxCommandHint = 'Export grid data to Excel file (.xlsx)';
  SExportGridAsXlsxCommandCaption = 'Export to Excel file (.xlsx)...';
  SExportGridAsHtmlCommandHint = 'Export grid data to html file (.html)';
  SExportGridAsHtmlCommandCaption = 'Export to html file (.html)...';

type

  { TmAbstractGridHelper }

  TmAbstractGridHelper = class abstract
  strict private
    const DEFAULT_XLS_HEADER_COLOR : DWord = $00EED7BD;
  protected
    FGrid: TControl;

    FSettings : TmGridColumnsSettings;
    FFormulaFields : TmFormulaFields;
    FCellDecorations : TmCellDecorations;
    FSaveDialog: TSaveDialog;
    FConfigurePopupMenu : TPopupMenu;

    function ConfirmFileOverwrite : boolean;
    function ConfirmDiscordedFileExt (const aFileExt : String): boolean;
    procedure ExportGridToFile(aFileType : String);

    procedure InternalSetup(aGrid: TControl; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations);
  public
    destructor Destroy; override;

    procedure CreateStandardConfigureMenu(aToolbar : TToolbar; const aConfigureImageIndex : integer);

    function EditSettings : boolean;
    procedure OnEditSettings(Sender : TObject);
    procedure OnEditFormulaFields(Sender : TObject);
    procedure OnEditCellDecorations(Sender : TObject);

    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);
    procedure LoadSettingsFromXML (aXMLElement : TmXmlElement);
    procedure SaveSettingsToXML (aXMLElement : TmXMLElement);

    procedure ExportGridAsCsv (aStream : TStream);
    procedure OnExportGridAsCsv (Sender : TObject);
    procedure ExportGridAsXlsx (aStream : TStream);
    procedure OnExportGridAsXlsx (Sender : TObject);
    procedure ExportGridAsHtml (aStream: TStream);
    procedure OnExportGridAsHtml (Sender : TObject);

    procedure SelectAllRows; virtual; abstract;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); virtual; abstract;
    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); virtual; abstract;

    property FormulaFields : TmFormulaFields read FFormulaFields;
    property CellDecorations : TmCellDecorations read FCellDecorations;
  end;

  { TmDBGridHelper }

  TmDBGridHelper = class(TmAbstractGridHelper)
  protected
    FDBGrid : TmDBGrid;
  public
    constructor Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations); virtual;
    destructor Destroy; override;

    procedure SetupGrid (const aEnableAutoSizedColumns : boolean = true); override;
    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;

    property DBGrid : TmDBGrid read FDBGrid;
  end;

  { TmDrawGridHelper }

  TmDrawGridHelper = class(TmAbstractGridHelper)
  protected
    FDrawGrid : TmDrawGrid;
  public
    constructor Create(aGrid : TmDrawGrid; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations); virtual;
    destructor Destroy; override;

    procedure SetupGrid(const aEnableAutoSizedColumns : boolean = true); override;
    procedure SelectAllRows; override;
    procedure SelectRows (const aKeyField : String; const aValues : TStringList); override;

    property DrawGrid : TmDrawGrid read FDrawGrid;
  end;

function GetLastUsedFolderForExport : TNullableString;

var
  DefaultGridAlternateColor : TColor = clMoneyGreen;

implementation

uses
  SysUtils, variants,
  mCSV, mFloatsManagement, mSpreadsheetUtils,
  mVirtualDatasetFormulasToXml, mGridColumnSettingsToXml, mSummaryToXml,
  mCellDecorationsToXml, mWaitCursor, mMaps;

var
  _LastUsedFolderForExport : TNullableString;

function GetLastUsedFolderForExport: TNullableString;
begin
  if not Assigned(_LastUsedFolderForExport) then
    _LastUsedFolderForExport := TNullableString.Create();
  Result := _LastUsedFolderForExport;
end;

{ TmDrawGridHelper }

constructor TmDrawGridHelper.Create(aGrid: TmDrawGrid; aFormulaFields: TmFormulaFields; aCellDecorations: TmCellDecorations);
begin
  InternalSetup(aGrid, aFormulaFields, aCellDecorations);
  FDrawGrid:= aGrid;
end;

destructor TmDrawGridHelper.Destroy;
begin
  FDrawGrid:= nil;
  inherited Destroy;
end;

procedure TmDrawGridHelper.SetupGrid(const aEnableAutoSizedColumns : boolean = true);
begin
  FDrawGrid.Align:= alClient;
  FDrawGrid.AlternateColor:= DefaultGridAlternateColor;
  FDrawGrid.Flat := True;
  //(FDBGrid as TDrawGrid).Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect];
  FDrawGrid.Options := [goRowHighlight, goColSizing, goColMoving, goVertLine, goHorzLine, goTabs, goDrawFocusSelected, goDblClickAutoSize, goRelaxedRowSelect];
      (*
      goFixedVertLine,      // Ya
      goFixedHorzLine,      // Ya
      goVertLine,           // Ya
      goHorzLine,           // Ya
      goRangeSelect,        // Ya
      goDrawFocusSelected,  // Ya
      goRowSizing,          // Ya
      goColSizing,          // Ya
      goRowMoving,          // Ya
      goColMoving,          // Ya
      goEditing,            // Ya
      goAutoAddRows,        // JuMa
      goTabs,               // Ya
      goRowSelect,          // Ya
      goAlwaysShowEditor,   // Ya
      goThumbTracking,      // ya
      // Additional Options
      goColSpanning,        // Enable cellextent calcs
      ,   // User can see focused cell on goRowSelect
      goDblClickAutoSize,   // dblclicking columns borders (on hdrs) resize col.
      goSmoothScroll,       // Switch scrolling mode (pixel scroll is by default)
      goFixedRowNumbering,  // Ya
      goScrollKeepVisible,  // keeps focused cell visible while scrolling
      goHeaderHotTracking,  // Header cells change look when mouse is over them
      goHeaderPushedLook,   // Header cells looks pushed when clicked
      goSelectionActive,    // Setting grid.Selection moves also cell cursor
      goFixedColSizing,     // Allow to resize fixed columns
      goDontScrollPartCell, // clicking partially visible cells will not scroll
      goCellHints,          // show individual cell hints
      goTruncCellHints,     // show cell hints if cell text is too long
      goCellEllipsis,       // show "..." if cell text is too long
      goAutoAddRowsSkipContentCheck,//BB Also add a row (if AutoAddRows in Options) if last row is empty
      goRowHighlight        // Highlight the current Row
      *)

end;

procedure TmDrawGridHelper.SelectAllRows;
begin

end;

procedure TmDrawGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
begin

end;

{ TmDBGridHelper }


function TmAbstractGridHelper.ConfirmFileOverwrite: boolean;
begin
  Result := MessageDlg(SConfirmFileOverwriteCaption, SConfirmFileOverwriteMessage, mtConfirmation, mbYesNo, 0) = mrYes;
end;

function TmAbstractGridHelper.ConfirmDiscordedFileExt(const aFileExt: String): boolean;
begin
  Result := MessageDlg(SConfirmDiscordedFileExtCaption, Format(SConfirmDiscordedFileExtMessage, [aFileExt]), mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TmAbstractGridHelper.ExportGridToFile(aFileType: String);
var
  fs : TFileStream;
begin
  if aFileType = 'CSV' then
  begin
    FSaveDialog.DefaultExt:= 'csv';
    FSaveDialog.Filter:=SCSVFileDescription + '|*.csv';
  end
  else
  if aFileType = 'XLSX' then
  begin
    FSaveDialog.DefaultExt:= 'xlsx';
    FSaveDialog.Filter:=SExcelFileDescription + '|*.xlsx';
  end
  else
  if aFileType = 'HTML' then
  begin
    FSaveDialog.DefaultExt:= 'html';
    FSaveDialog.Filter:=SHtmlFileDescription + '|*.html';
  end;

  if FSaveDialog.FileName <> '' then
    FSaveDialog.FileName := ChangeFileExt(FSaveDialog.FileName, FSaveDialog.DefaultExt);

  if GetLastUsedFolderForExport.NotNull and DirectoryExists(GetLastUsedFolderForExport.AsString) then
    FSaveDialog.InitialDir:= GetLastUsedFolderForExport.AsString;

  if FSaveDialog.Execute then
  begin

    if Uppercase(ExtractFileExt(FSaveDialog.FileName)) <> Uppercase(FSaveDialog.DefaultExt) then
    begin
      if not ConfirmDiscordedFileExt (FSaveDialog.DefaultExt) then
        exit;
    end;

    if FileExists(FSaveDialog.FileName) then
    begin
      if not ConfirmFileOverwrite then
        exit;
    end;
    GetLastUsedFolderForExport.Value:= ExtractFilePath(FSaveDialog.FileName);
    try
      try
        TWaitCursor.ShowWaitCursor('TmAbstractGridHelper.ExportGridToFile');
        fs := TFileStream.Create(FSaveDialog.FileName, fmCreate);
        try
          if aFileType = 'CSV' then
            Self.ExportGridAsCsv(fs)
          else if aFileType = 'XLSX' then
            Self.ExportGridAsXlsx(fs)
          else if aFileType = 'HTML' then
            Self.ExportGridAsHtml(fs);
        finally
          fs.Free;
        end;
      finally
        TWaitCursor.UndoWaitCursor('TmAbstractGridHelper.ExportGridToFile');
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

procedure TmAbstractGridHelper.InternalSetup(aGrid: TControl; aFormulaFields: TmFormulaFields; aCellDecorations: TmCellDecorations);
begin
  FGrid := aGrid;
  if not Assigned(FSettings) then
    FSettings := TmGridColumnsSettings.Create;
  FSettings.Clear;
  FFormulaFields:= aFormulaFields;
  if not Assigned(FSaveDialog) then
    FSaveDialog := TSaveDialog.Create(nil);
  FCellDecorations := aCellDecorations;
end;

destructor TmAbstractGridHelper.Destroy;
begin
  FSettings.Free;
  FSaveDialog.Free;

  inherited Destroy;
end;

constructor TmDBGridHelper.Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields; aCellDecorations: TmCellDecorations);
begin
  InternalSetup(aGrid, aFormulaFields, aCellDecorations);
  FDBGrid := aGrid;
end;

destructor TmDBGridHelper.Destroy;
begin
  FDBGrid := nil;
  inherited Destroy;
end;

procedure TmDBGridHelper.SetupGrid (const aEnableAutoSizedColumns : boolean = true);
begin
  FDBGrid.Align:= alClient;
  FDBGrid.AlternateColor:= DefaultGridAlternateColor;
  FDBGrid.Flat := True;
  if aEnableAutoSizedColumns then
    FDBGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect]
  else
    FDBGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit,                    dgDisableDelete, dgDisableInsert, dgMultiselect];
end;


procedure TmAbstractGridHelper.CreateStandardConfigureMenu(aToolbar: TToolbar; const aConfigureImageIndex : integer);
var
  tmp : TToolButton;
  itm : TMenuItem;
begin
  FConfigurePopupMenu := TPopupMenu.Create(aToolbar);

  tmp := TToolButton.Create(aToolbar);
  tmp.Style:= tbsDropDown;
  tmp.DropdownMenu := FConfigurePopupMenu;
  tmp.ImageIndex := aConfigureImageIndex;
  tmp.Parent := aToolbar;
  tmp.Hint := SConfigureCommandHint;
  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnEditSettings;
  itm.Hint:= SConfigureGridCommandHint;
  itm.Caption:= SConfigureGridCommandCaption;
  if Assigned(FFormulaFields) then
  begin
    itm := TMenuItem.Create(FConfigurePopupMenu);
    FConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= Self.OnEditFormulaFields;
    itm.Hint:= SConfigureFormulaFieldsCommandHint;
    itm.Caption:= SConfigureFormulaFieldsCommandCaption;
  end;
  if Assigned(FCellDecorations) then
  begin
    itm := TMenuItem.Create(FConfigurePopupMenu);
    FConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= Self.OnEditCellDecorations();
    itm.Hint:= SConfigureCellDecorationsCommandHint;
    itm.Caption:= SConfigureCellDecorationsCommandCaption;
  end;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  itm.Caption:= '-';
  FConfigurePopupMenu.Items.Add(itm);

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsCsv;
  itm.Hint:= SExportGridAsCsvCommandHint;
  itm.Caption:= SExportGridAsCsvCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsXlsx;
  itm.Hint:= SExportGridAsXlsxCommandHint;
  itm.Caption:= SExportGridAsXlsxCommandCaption;

  itm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsHtml();
  itm.Hint:= SExportGridAsHtmlCommandHint;
  itm.Caption:= SExportGridAsHtmlCommandCaption;

  tmp := TToolButton.Create(aToolbar);
  tmp.Style:= tbsDivider;
  tmp.Parent := aToolbar;
end;

function TmAbstractGridHelper.EditSettings : boolean;
var
  frm : TGridSettingsForm;
  Intf : ImGrid;
begin
  Result := false;
  if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
  begin
    Intf.ReadSettings(FSettings);
    frm := TGridSettingsForm.Create(nil);
    try
      frm.Init(FSettings);
      if frm.ShowModal = mrOk then
      begin
        try
          TWaitCursor.ShowWaitCursor('TmAbstractGridHelper.EditSettings');
          Intf.ApplySettings(FSettings);
        finally
          TWaitCursor.UndoWaitCursor('TmAbstractGridHelper.EditSettings');
        end;
        Result := true;
      end;
    finally
      frm.Free;
    end;
  end;
end;

procedure TmAbstractGridHelper.OnEditSettings(Sender: TObject);
begin
  Self.EditSettings;
end;

procedure TmAbstractGridHelper.OnEditFormulaFields(Sender: TObject);
var
  frm : TFormulaFieldsConfigurationForm;
  Intf : ImGrid;
  fields : TmFields;
begin
  if not Assigned(FFormulaFields) then
    exit;

  fields := TmFields.Create;
  frm := TFormulaFieldsConfigurationForm.Create(nil);
  try
    if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
    begin
      Intf.GetFields(fields);
      frm.Init(FFormulaFields, fields);
      if frm.ShowModal = mrOk then
      begin
        Intf.ReadSettings(FSettings);
        Intf.RefreshDataProvider;
        Intf.ApplySettings(FSettings);
      end;
    end;
  finally
    frm.Free;
    fields.Free;
  end;
end;

procedure TmAbstractGridHelper.OnEditCellDecorations(Sender: TObject);
var
  frm : TCellDecorationsConfigurationForm;
  Intf : ImGrid;
  fields : TmFields;
begin
  if not Assigned(FCellDecorations) then
    exit;

  fields := TmFields.Create;
  frm := TCellDecorationsConfigurationForm.Create(nil);
  try
    if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
    begin
      Intf.GetFields(fields);
      frm.Init(FCellDecorations, fields);
      if frm.ShowModal = mrOk then
      begin
        FGrid.Invalidate;
        //Intf.ReadSettings(FSettings);
        //Intf.RefreshDataProvider;
        //ApplySettings(FSettings);
      end;
    end;
  finally
    frm.Free;
    fields.Free;
  end;
end;

procedure TmAbstractGridHelper.LoadSettings(aStream: TStream);
var
  doc : TmXmlDocument;
begin
  doc := TmXmlDocument.Create;
  try
    doc.LoadFromStream(aStream);
    LoadSettingsFromXML(doc.RootElement);
  finally
    doc.Free;
  end;
end;

procedure TmAbstractGridHelper.SaveSettings(aStream: TStream);
var
  doc : TmXmlDocument;
  root : TmXmlElement;
begin
  doc := TmXmlDocument.Create;
  try
    root := doc.CreateRootElement('configuration');
    SaveSettingsToXML(root);
    doc.SaveToStream(aStream);
  finally
    doc.Free;
  end;
end;

procedure TmAbstractGridHelper.LoadSettingsFromXML(aXMLElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  Intf : ImGrid;
begin
  if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
  begin
    cursor := TmXmlElementCursor.Create(aXMLElement, 'columns');
    try
      LoadGridColumnsSettingFromXmlElement(FSettings, cursor.Elements[0]);
    finally
      cursor.Free;
    end;
    if Assigned(FFormulaFields) then
    begin
      cursor := TmXmlElementCursor.Create(aXMLElement, 'formulaFields');
      try
        if cursor.Count > 0 then
        begin
          LoadFormulaFieldsFromXmlElement(FFormulaFields, cursor.Elements[0]);
          if FFormulaFields.Count > 0 then
            Intf.RefreshDataProvider;
        end;
      finally
        cursor.Free;
      end;
    end;
    if Assigned(Intf.GetSummaryManager) then
    begin
      cursor := TmXmlElementCursor.Create(aXMLElement, 'summaries');
      try
        if cursor.Count > 0 then
          LoadSummaryDefinitionsFromXmlElement(Intf.GetSummaryManager.GetSummaryDefinitions, cursor.Elements[0]);
      finally
        cursor.Free;
      end;
    end;
    if Assigned(FCellDecorations) then
    begin
      cursor := TmXmlElementCursor.Create(aXMLElement, 'cellDecorations');
      try
        if cursor.Count > 0 then
          LoadCellDecorationsFromXmlElement(FCellDecorations, cursor.Elements[0]);
      finally
        cursor.Free;
      end;
    end;
    Intf.ApplySettings(FSettings);
    if Assigned(Intf.GetSummaryManager) then
      Intf.GetSummaryManager.RefreshSummaries;
  end;
end;

procedure TmAbstractGridHelper.SaveSettingsToXML(aXMLElement: TmXMLElement);
var
  Intf : ImGrid;
begin
  if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
  begin
    Intf.ReadSettings(FSettings);
    aXMLElement.SetAttribute('version', '1');
    SaveGridColumnsSettingToXmlElement(FSettings, aXMLElement.AddElement('columns'));
    if Assigned(FFormulaFields) then
      SaveFormulaFieldsToXmlElement(FFormulaFields, aXMLElement.AddElement('formulaFields'));
    if Assigned(Intf.GetSummaryManager) then
      SaveSummaryDefinitionsToXmlElement(Intf.GetSummaryManager.GetSummaryDefinitions, aXMLElement.AddElement('summaries'));
    if Assigned(FCellDecorations) then
      SaveCellDecorationsToXmlElement(FCellDecorations, aXMLElement.AddElement('cellDecorations'));
  end;
end;

procedure TmAbstractGridHelper.ExportGridAsCsv(aStream: TStream);
var
  i : integer;
  Intf : ImGrid;
  CSVBuilder : TmCSVBuilder;
  columns : TmGridColumns;
  fields : TmFields;
  curField : TmField;
  curValue : Variant;
begin
  if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
  begin
    fields := TmFields.Create;
    columns := TmGridColumns.Create;
    CSVBuilder := TmCSVBuilder.Create;
    try
      CSVBuilder.Stream := aStream;
      CSVBuilder.QuoteChar:= '"';
      CSVBuilder.Delimiter:= ',';
      CSVBuilder.StartWrite;

      Intf.GetColumns(columns);
      Intf.GetFields(fields);

      Intf.GetDataCursor.StartBrowsing;
      try
        for i := 0 to columns.Count - 1 do
        begin
          if columns.Get(i).Visible then
            CSVBuilder.AppendCellRFC4180(columns.Get(i).Title);
        end;
        CSVBuilder.AppendRow;

        Intf.GetDataCursor.First;
        while not Intf.GetDataCursor.EOF do
        begin
          for i := 0 to columns.Count - 1 do
          begin
            if columns.Get(i).Visible then
            begin
              curField := fields.FieldByName(columns.Get(i).FieldName);
              curValue := Intf.GetDataCursor.GetValueByFieldName(columns.Get(i).FieldName);
              if (curField.DataType = ftSmallint) or (curField.DataType = ftInteger) or (curField.DataType = ftLargeint) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  CSVBuilder.AppendCellRFC4180(IntToStr(curValue));
              end
              else
              if (curField.DataType = ftFloat) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  if curField.DisplayFormat <> '' then
                    CSVBuilder.AppendCellRFC4180(FormatFloat(curField.DisplayFormat, curValue))
                  else
                    CSVBuilder.AppendCellRFC4180(FloatToStr(curValue));
              end
              else
              if (curField.DataType = ftDate) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  CSVBuilder.AppendCellRFC4180(DateToStr(curValue));
              end
              else
              if (curField.DataType = ftDateTime) or (curField.DataType = ftTime) or (curField.DataType = ftTimeStamp) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  CSVBuilder.AppendCellRFC4180(DateTimeToStr(curValue));
              end
              else
              if (curField.DataType = ftBoolean) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  CSVBuilder.AppendCellRFC4180(BoolToStr(curValue, true));
              end
              else
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCellRFC4180('')
                else
                  CSVBuilder.AppendCellRFC4180(VarToStr(curValue));
              end;
            end;
          end;
          CSVBuilder.AppendRow;

          Intf.GetDataCursor.Next;
        end;
      finally
        Intf.GetDataCursor.EndBrowsing;
      end;
      CSVBuilder.EndWrite;
    finally
      CSVBuilder.Free;
      columns.Free;
      fields.Free;
    end;
  end;
end;

procedure TmAbstractGridHelper.OnExportGridAsCsv(Sender : TObject);
begin
  ExportGridToFile('CSV');
end;

{$IFDEF FPC}

procedure TmAbstractGridHelper.ExportGridAsXlsx(aStream: TStream);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet : TsWorksheet;
  i, row, col : integer;
  Intf : ImGrid;
  curField : TmField;
  fields : TmFields;
  columns : TmGridColumns;
  curValue : Variant;
  d : double;
  helper : TmSpreadsheetHelper;
begin
  fields := TmFields.Create;
  columns := TmGridColumns.Create;
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');
    helper := TmSpreadsheetHelper.Create(MyWorksheet);
    try
      {$IFDEF WINDOWS}
      helper.DefaultFont.FontName:= 'Calibri';
      {$ELSE}
      helper.DefaultFont.FontName:= 'Arial';
      {$ENDIF}
      helper.DefaultFont.FontSize:= 10;
      helper.DefaultRowHeight:= 14;

      if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
      begin
        Intf.GetFields(fields);
        Intf.GetColumns(columns);

        Intf.GetDataCursor.StartBrowsing;
        try
          col := 0;
          for i := 0 to columns.Count - 1 do
          begin
            if columns.Get(i).Visible then
            begin
              helper.WriteText(0, col, columns.Get(i).Title, false, true, DEFAULT_XLS_HEADER_COLOR);
              helper.WriteSouthBorder(0, col);
              inc (col);
            end;
          end;

          row := 1;

          Intf.GetDataCursor.First;
          while not Intf.GetDataCursor.EOF do
          begin
            col := 0;

            for i := 0 to columns.Count - 1 do
            begin
              if columns.Get(i).Visible then
              begin
                curField := fields.FieldByName(columns.Get(i).FieldName);
                curValue:= Intf.GetDataCursor.GetValueByFieldName(curField.FieldName);
                if not VarIsNull(curValue) then
                begin
                  if (curField.DataType = ftSmallint) or (curField.DataType = ftInteger) then
                    helper.WriteInteger(row, col, curValue)
                  else
                  if (curField.DataType = ftLargeint) then
                    helper.WriteInt64(row, col, curValue)
                  else
                  if (curField.DataType = ftFloat) then
                  begin
                    if curField.DisplayFormat <> '' then
                      helper.WriteFloat(row, col, curValue, curField.DisplayFormat)
                    else
                      helper.WriteFloat(row, col, curValue);
                  end
                  else
                  if (curField.DataType = ftDate) then
                  begin
                    d := curValue;
                    helper.WriteDate(row, col, round(d));
                  end
                  else
                  if (curField.DataType = ftDateTime) or (curField.DataType = ftTime) or (curField.DataType = ftTimeStamp) then
                  begin
                    d := curValue;
                    if DoublesAreEqual(0, abs(d - round(d))) then
                      helper.WriteDate(row, col, curValue)
                    else
                      helper.WriteDateTime(row, col, curValue);
                  end
                  else
                  if (curField.DataType = ftBoolean) then
                    helper.WriteBoolean(row, col, curValue)
                  else
                    helper.WriteText(row, col, VarToStr(curValue));
                end;
                inc(col);
              end;
            end;

            inc(row);
            Intf.GetDataCursor.Next;
          end;
        finally
          Intf.GetDataCursor.EndBrowsing;
        end;
      end;
    finally
      helper.Free;
    end;
    MyWorkbook.WriteToStream(aStream, sfOOXML);
  finally
    MyWorkbook.Free;
    columns.Free;
    fields.Free;
  end;
end;

procedure TmAbstractGridHelper.OnExportGridAsXlsx(Sender : TObject);
begin
  ExportGridToFile('XLSX');
end;

procedure TmAbstractGridHelper.ExportGridAsHtml(aStream: TStream);
var
  htmlDoc : TStringList;
  i, row, col : integer;
  Intf : ImGrid;
  curField : TmField;
  fields : TmFields;
  columns : TmGridColumns;
  curValue : Variant;
  d : double;
begin
  fields := TmFields.Create;
  columns := TmGridColumns.Create;
  htmlDoc := TStringList.Create;
  try
    htmlDoc.Add('<!DOCTYPE html>');
    htmlDoc.Add('<html>');
    htmlDoc.Add('<head>');
    htmlDoc.Add('<meta charset="utf-8">');
    htmlDoc.Add('<!-- CREDITS ------------------------------ -->');
    htmlDoc.Add('<!-- derived from style by João Sardinha ( http://johnsardine.com/freebies/dl-html-css/simple-little-tab/ ) -->');
    htmlDoc.Add('<!-- -------------------------------------- -->');
    htmlDoc.Add('<style type=text/css>');
    htmlDoc.Add('p {');
    htmlDoc.Add('	font-family:Arial, Helvetica, sans-serif;');
    htmlDoc.Add('	background: #ededed;');
    htmlDoc.Add('	font-size:12px;');
    htmlDoc.Add('       text-shadow: 1px 1px 0px #fff;');
    htmlDoc.Add('       margin:20px;');
    htmlDoc.Add('       border:#ccc 1px solid;');
    htmlDoc.Add('}');
    htmlDoc.Add('table a:link {');
    htmlDoc.Add('	color: #666;');
    htmlDoc.Add('	font-weight: bold;');
    htmlDoc.Add('	text-decoration:none;');
    htmlDoc.Add('}');
    htmlDoc.Add('table a:visited {');
    htmlDoc.Add('	color: #999999;');
    htmlDoc.Add('	font-weight:bold;');
    htmlDoc.Add('	text-decoration:none;');
    htmlDoc.Add('}');
    htmlDoc.Add('table a:active,');
    htmlDoc.Add('table a:hover {');
    htmlDoc.Add('	color: #bd5a35;');
    htmlDoc.Add('	text-decoration:underline;');
    htmlDoc.Add('}');
    htmlDoc.Add('table {');
    htmlDoc.Add('	font-family:Arial, Helvetica, sans-serif;');
    htmlDoc.Add('	color:#666;');
    htmlDoc.Add('	font-size:12px;');
    htmlDoc.Add('	text-shadow: 1px 1px 0px #fff;');
    htmlDoc.Add('	background:#eaebec;');
    htmlDoc.Add('	margin:20px;');
    htmlDoc.Add('	border:#ccc 1px solid;');
    htmlDoc.Add('');
    htmlDoc.Add('	-moz-border-radius:3px;');
    htmlDoc.Add('	-webkit-border-radius:3px;');
    htmlDoc.Add('	border-radius:3px;');
    htmlDoc.Add('');
    htmlDoc.Add('	-moz-box-shadow: 0 1px 2px #d1d1d1;');
    htmlDoc.Add('	-webkit-box-shadow: 0 1px 2px #d1d1d1;');
    htmlDoc.Add('	box-shadow: 0 1px 2px #d1d1d1;');
    htmlDoc.Add('}');
    htmlDoc.Add('table th {');
    htmlDoc.Add('	padding:21px 25px 22px 25px;');
    htmlDoc.Add('	border-top:1px solid #fafafa;');
    htmlDoc.Add('	border-bottom:1px solid #e0e0e0;');
    htmlDoc.Add('');
    htmlDoc.Add('	background: #ededed;');
    htmlDoc.Add('	background: -webkit-gradient(linear, left top, left bottom, from(#ededed), to(#ebebeb));');
    htmlDoc.Add('	background: -moz-linear-gradient(top,  #ededed,  #ebebeb);');
    htmlDoc.Add('}');
    htmlDoc.Add('table th:first-child {');
    htmlDoc.Add('	text-align: left;');
    htmlDoc.Add('	padding-left:20px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:first-child th:first-child {');
    htmlDoc.Add('	-moz-border-radius-topleft:3px;');
    htmlDoc.Add('	-webkit-border-top-left-radius:3px;');
    htmlDoc.Add('	border-top-left-radius:3px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:first-child th:last-child {');
    htmlDoc.Add('	-moz-border-radius-topright:3px;');
    htmlDoc.Add('	-webkit-border-top-right-radius:3px;');
    htmlDoc.Add('	border-top-right-radius:3px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr {');
    htmlDoc.Add('	text-align: center;');
    htmlDoc.Add('	padding-left:20px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table td:first-child {');
    htmlDoc.Add('	text-align: left;');
    htmlDoc.Add('	padding-left:20px;');
    htmlDoc.Add('	border-left: 0;');
    htmlDoc.Add('}');
    htmlDoc.Add('table td {');
    htmlDoc.Add('	padding:18px;');
    htmlDoc.Add('	border-top: 1px solid #ffffff;');
    htmlDoc.Add('	border-bottom:1px solid #e0e0e0;');
    htmlDoc.Add('	border-left: 1px solid #e0e0e0;');
    htmlDoc.Add('');
    htmlDoc.Add('	background: #fafafa;');
    htmlDoc.Add('	background: -webkit-gradient(linear, left top, left bottom, from(#fbfbfb), to(#fafafa));');
    htmlDoc.Add('	background: -moz-linear-gradient(top,  #fbfbfb,  #fafafa);');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr.even td {');
    htmlDoc.Add('	background: #f6f6f6;');
    htmlDoc.Add('	background: -webkit-gradient(linear, left top, left bottom, from(#f8f8f8), to(#f6f6f6));');
    htmlDoc.Add('	background: -moz-linear-gradient(top,  #f8f8f8,  #f6f6f6);');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:last-child td {');
    htmlDoc.Add('	border-bottom:0;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:last-child td:first-child {');
    htmlDoc.Add('	-moz-border-radius-bottomleft:3px;');
    htmlDoc.Add('	-webkit-border-bottom-left-radius:3px;');
    htmlDoc.Add('	border-bottom-left-radius:3px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:last-child td:last-child {');
    htmlDoc.Add('	-moz-border-radius-bottomright:3px;');
    htmlDoc.Add('	-webkit-border-bottom-right-radius:3px;');
    htmlDoc.Add('	border-bottom-right-radius:3px;');
    htmlDoc.Add('}');
    htmlDoc.Add('table tr:hover td {');
    htmlDoc.Add('	background: #f2f2f2;');
    htmlDoc.Add('	background: -webkit-gradient(linear, left top, left bottom, from(#f2f2f2), to(#f0f0f0));');
    htmlDoc.Add('	background: -moz-linear-gradient(top,  #f2f2f2,  #f0f0f0);');
    htmlDoc.Add('}');

    htmlDoc.Add('</style>');
    htmlDoc.Add('</head>');
    htmlDoc.Add('<body>');


    htmlDoc.Add('<table>');

    if FGrid.GetInterface(SImGridInterface, Intf) or Self.GetInterface(SImGridInterface, Intf) then
    begin
      Intf.GetColumns(columns);
      Intf.GetFields(fields);

      Intf.GetDataCursor.StartBrowsing;
      try
        col := 0;
        htmlDoc.Add('<tr>');
        for i := 0 to columns.Count - 1 do
        begin
          if columns.Get(i).Visible then
          begin
            htmlDoc.Add('<th>' + columns.Get(i).Title + '</th>');
            inc (col);
          end;
        end;
        htmlDoc.Add('</tr>');

        row := 1;


        Intf.GetDataCursor.First;
        while not Intf.GetDataCursor.EOF do
        begin
          col := 0;

          htmlDoc.Add('<tr>');

          for i := 0 to columns.Count - 1 do
          begin
            if columns.Get(i).Visible then
            begin
              htmlDoc.Add('<td>');
              curField := fields.FieldByName(columns.Get(i).FieldName);
              curValue := Intf.GetDataCursor.GetValueByFieldName(columns.Get(i).FieldName);
              if not VarIsNull(curValue) then
              begin
                if (curField.DataType = ftSmallint) or (curField.DataType = ftInteger) or (curField.DataType = ftLargeint) then
                  htmlDoc.Add(IntToStr(curValue))
                else
                if (curField.DataType = ftFloat) then
                begin
                  d := curValue;
                  if curField.DisplayFormat <> '' then
                    htmlDoc.Add(FormatFloat(curField.DisplayFormat, d))
                  else
                    htmlDoc.Add(FloatToStr(d));
                end
                else
                if (curField.DataType = ftDate) then
                begin
                  d := curValue;
                  htmlDoc.Add(DateToStr(d));
                end
                else
                if (curField.DataType = ftDateTime) or (curField.DataType = ftTimeStamp) then
                begin
                  d := curValue;
                  htmlDoc.Add(DateTimeToStr(d));
                end
                else
                if (curField.DataType = ftTime) then
                begin
                  d := curValue;
                  htmlDoc.Add(TimeToStr(d));
                end
                else
                if (curField.DataType = ftBoolean) then
                begin
                  htmlDoc.Add(BoolToStr(curValue, true));
                end
                else
                  htmlDoc.Add(VarToStr(curValue));
              end;
              htmlDoc.Add('</td>');
              inc(col);
            end;
          end;
          htmlDoc.Add('</tr>');

          inc(row);
          Intf.GetDataCursor.Next;
        end;

      finally
        Intf.GetDataCursor.EndBrowsing;
      end;
      htmlDoc.Add('</table>');

      for i := 0 to Intf.GetSummaryManager.GetSummaryValues.Count - 1 do
      begin
        htmlDoc.Add('<p>' + Intf.GetSummaryManager.GetSummaryValues.Get(i).FormattedValue + '</p>');
      end;
    end;

    htmlDoc.Add('</body>');
    htmlDoc.Add('</html>');
    htmlDoc.SaveToStream(aStream);
  finally
    htmlDoc.Free;
    columns.Free;
    fields.Free;
  end;
end;

procedure TmAbstractGridHelper.OnExportGridAsHtml(Sender: TObject);
begin
  ExportGridToFile('HTML');
end;

procedure TmDBGridHelper.SelectAllRows;
var
  curRecNo : longint;
begin
  curRecNo := -1;
  if dgMultiselect in FDBGrid.Options then
  begin
    FDBGrid.BeginUpdate;
    try
      FDBGrid.ClearSelections;
      curRecNo := FDBGrid.DataSource.DataSet.RecNo;
      FDBGrid.DataSource.DataSet.DisableControls;
      try
        FDBGrid.DataSource.DataSet.First;
        while not FDBGrid.DataSource.DataSet.EOF do
        begin
          FDBGrid.SelectedRows.CurrentRowSelected:= true;
          FDBGrid.DataSource.DataSet.Next;
        end;
        if curRecNo >= 0 then
          FDBGrid.DataSource.DataSet.RecNo:= curRecNo;
      finally
        FDBGrid.DataSource.DataSet.EnableControls;
      end;
    finally
      FDBGrid.EndUpdate(true);
    end;
  end;
end;

procedure TmDBGridHelper.SelectRows(const aKeyField: String; const aValues: TStringList);
var
  i : integer;
  index : TmStringDictionary;
  firstSelectedRecNo, curRecNo : longint;
begin
  if dgMultiselect in FDBGrid.Options then
  begin
    curRecNo := -1;
    firstSelectedRecNo:= -1;
    index := TmStringDictionary.Create(false);
    try
      for i := 0 to aValues.Count - 1 do
      begin
        if not index.Contains(aValues.Strings[i]) then
          index.Add(aValues.Strings[i], index);
      end;
      FDBGrid.BeginUpdate;
      try
        FDBGrid.ClearSelections;
        curRecNo := FDBGrid.DataSource.DataSet.RecNo;
        FDBGrid.DataSource.DataSet.DisableControls;
        try
          FDBGrid.DataSource.DataSet.First;
          while not FDBGrid.DataSource.DataSet.EOF do
          begin
            if index.Contains(FDBGrid.DataSource.DataSet.FieldByName(aKeyField).AsString) then
            begin
              FDBGrid.SelectedRows.CurrentRowSelected:= true;
              if firstSelectedRecNo = -1 then
                FDBGrid.DataSource.DataSet.RecNo;
            end
            else
              FDBGrid.SelectedRows.CurrentRowSelected:= false;
            FDBGrid.DataSource.DataSet.Next;
          end;
          if firstSelectedRecNo >= 0 then
            FDBGrid.DataSource.DataSet.RecNo:= firstSelectedRecNo
          else if curRecNo >= 0 then
            FDBGrid.DataSource.DataSet.RecNo:= curRecNo;
        finally
          FDBGrid.DataSource.DataSet.EnableControls;
        end;
      finally
        FDBGrid.EndUpdate(true);
      end;
    finally
      index.Free;
    end;
  end;
end;

{$ELSE}

procedure TmDBGridHelper.ExportGridAsXls(aStream: TStream);
begin
  // TODO
end;
{$ENDIF}

finalization
  FreeAndNil(_LastUsedFolderForExport);

end.
