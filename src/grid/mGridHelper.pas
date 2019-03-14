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
  DBGrids, Grids, Controls, Menus, LCLIntf,
  ATButtons,
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats, // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  mGridColumnSettings, mXML,
  mGridSettingsForm, mFormulaFieldsConfigurationForm,
  mDBGrid, mNullables, mGrids, mFields,
  mVirtualDatasetFormulas;

resourcestring
  SCSVFileDescription = 'Comma Separated Values files';
  SExcelFileDescription = 'Excel 97-2003 files';
  SHtmlFileDescription = 'Html files';
  SUnableToWriteFileMessage = 'Unable to write file. Check if the file is open by another application. If so, close it and run this command again. Detail:';
  SConfirmFileOverwriteCaption = 'Confirm';
  SConfirmFileOverwriteMessage = 'The selected file already exists. Overwrite it?';
  SWantToOpenFileMessage = 'Do you want to open the file?';
  SConfigureCommandHint = 'Configure...';
  SConfigureGridCommandHint = 'Configure grid...';
  SConfigureGridCommandCaption = 'Configure grid';
  SConfigureFormulaFieldsCommandHint = 'Configure formula fields..';
  SConfigureFormulaFieldsCommandCaption = 'Configure formula fields';
  SExportGridAsCsvCommandHint = 'Export grid data to csv file';
  SExportGridAsCsvCommandCaption = 'Export to csv file...';
  SExportGridAsXlsCommandHint = 'Export grid data to Excel file (.xls)';
  SExportGridAsXlsCommandCaption = 'Export to Excel file (.xls)...';
  SExportGridAsHtmlCommandHint = 'Export grid data to html file (.html)';
  SExportGridAsHtmlCommandCaption = 'Export to html file (.html)...';

type

  { TmAbstractGridHelper }

  TmAbstractGridHelper = class abstract
  protected
    FGrid: TCustomGrid;

    FSettings : TmGridColumnsSettings;
    FFormulaFields : TmFormulaFields;
    FSaveDialog: TSaveDialog;
    FConfigurePopupMenu : TPopupMenu;

    function ConfirmFileOverwrite : boolean;
    procedure ExportGridToFile(aFileType : String);

    procedure InternalCreate(aGrid: TCustomGrid; aFormulaFields : TmFormulaFields);
  public
    destructor Destroy; override;

    procedure CreateStandardConfigureMenu(aToolbar : TToolbar; const aConfigureImageIndex : integer);

    function EditSettings : boolean;
    procedure OnEditSettings(Sender : TObject);
    procedure OnEditFormulaFields(Sender : TObject);

    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);
    procedure LoadSettingsFromXML (aXMLElement : TmXmlElement);
    procedure SaveSettingsToXML (aXMLElement : TmXMLElement);

    procedure ExportGridAsCsv (aStream : TStream);
    procedure OnExportGridAsCsv (Sender : TObject);
    procedure ExportGridAsXls (aStream : TStream);
    procedure OnExportGridAsXls (Sender : TObject);
    procedure ExportGridAsHtml (aStream: TStream);
    procedure OnExportGridAsHtml (Sender : TObject);

    procedure SelectAllRows; virtual; abstract;
  end;

  { TmDBGridHelper }

  TmDBGridHelper = class(TmAbstractGridHelper)
  protected
    FDBGrid : TmDBGrid;
  public
    constructor Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure SetupGrid;
    procedure SelectAllRows; override;

    property DBGrid : TmDBGrid read FDBGrid;
  end;


function GetLastUsedFolderForExport : TNullableString;

implementation

uses
  SysUtils, variants,
  mCSV, mFloatsManagement,
  mVirtualDatasetFormulasToXml, mGridColumnSettingsToXml, mSummaryToXml;

var
  _LastUsedFolderForExport : TNullableString;

function GetLastUsedFolderForExport: TNullableString;
begin
  if not Assigned(_LastUsedFolderForExport) then
    _LastUsedFolderForExport := TNullableString.Create();
  Result := _LastUsedFolderForExport;
end;

{ TmDBGridHelper }


function TmAbstractGridHelper.ConfirmFileOverwrite: boolean;
begin
  Result := MessageDlg(SConfirmFileOverwriteCaption, SConfirmFileOverwriteMessage, mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TmAbstractGridHelper.ExportGridToFile(aFileType: String);
var
  fs : TFileStream;
  oldCursor : TCursor;
begin
  if aFileType = 'CSV' then
  begin
    FSaveDialog.DefaultExt:= 'csv';
    FSaveDialog.Filter:=SCSVFileDescription + '|*.csv';
  end
  else
  if aFileType = 'XLS' then
  begin
    FSaveDialog.DefaultExt:= 'xls';
    FSaveDialog.Filter:=SExcelFileDescription + '|*.xls';
  end
  else
  if aFileType = 'HTML' then
  begin
    FSaveDialog.DefaultExt:= 'html';
    FSaveDialog.Filter:=SHtmlFileDescription + '|*.html';
  end;

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
      oldCursor := Screen.Cursor;
      try
        Screen.Cursor:= crHourGlass;
        fs := TFileStream.Create(FSaveDialog.FileName, fmCreate);
        try
          if aFileType = 'CSV' then
            Self.ExportGridAsCsv(fs)
          else if aFileType = 'XLS' then
            Self.ExportGridAsXls(fs)
          else if aFileType = 'HTML' then
            Self.ExportGridAsHtml(fs);
        finally
          fs.Free;
        end;
      finally
        Screen.Cursor:= oldCursor;
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

procedure TmAbstractGridHelper.InternalCreate(aGrid: TCustomGrid; aFormulaFields: TmFormulaFields);
begin
  FGrid := aGrid;
  FSettings := TmGridColumnsSettings.Create;
  FFormulaFields:= aFormulaFields;
  FSaveDialog := TSaveDialog.Create(nil);
end;

destructor TmAbstractGridHelper.Destroy;
begin
  FSettings.Free;
  FSaveDialog.Free;

  inherited Destroy;
end;

constructor TmDBGridHelper.Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields);
begin
  InternalCreate(aGrid, aFormulaFields);
  FDBGrid := aGrid;
end;

destructor TmDBGridHelper.Destroy;
begin
  FDBGrid := nil;
  inherited Destroy;
end;

procedure TmDBGridHelper.SetupGrid;
begin
  FDBGrid.Align:= alClient;
  if FDBGrid is TDBGrid then
  begin
    (FDBGrid as TDBGrid).AlternateColor:= clMoneyGreen;
    (FDBGrid as TDBGrid).Flat := True;
    (FDBGrid as TDBGrid).Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect];
  end;
{  else if FDBGrid is TDrawGrid then
  begin
    (FDBGrid as TDrawGrid).AlternateColor:= clMoneyGreen;
    (FDBGrid as TDrawGrid).Flat := True;
    //(FDBGrid as TDrawGrid).Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect];
    (FDBGrid as TDrawGrid).Options := [goRowHighlight, goColSizing, goColMoving, goVertLine, goHorzLine, goTabs, goDrawFocusSelected, goDblClickAutoSize, goRelaxedRowSelect];
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
  end;}
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
  itm.OnClick:= Self.OnExportGridAsXls;
  itm.Hint:= SExportGridAsXlsCommandHint;
  itm.Caption:= SExportGridAsXlsCommandCaption;

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
  OldCursor : TCursor;
  Intf : ImGrid;
begin
  Result := false;
  if FGrid.GetInterface(SImGridInterface, Intf) then
  begin
    Intf.ReadSettings(FSettings);
    frm := TGridSettingsForm.Create(nil);
    try
      frm.Init(FSettings);
      if frm.ShowModal = mrOk then
      begin
        OldCursor:= Screen.Cursor;
        try
          Screen.Cursor:= crHourGlass;
          Intf.ApplySettings(FSettings);
        finally
          Screen.Cursor:= OldCursor;
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
    if FGrid.GetInterface(SImGridInterface, Intf) then
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
  if FGrid.GetInterface(SImGridInterface, Intf) then
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
        begin
          LoadSummaryDefinitionsFromXmlElement(Intf.GetSummaryManager.GetSummaryDefinitions, cursor.Elements[0]);
        end;
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
  if FGrid.GetInterface(SImGridInterface, Intf) then
  begin
    Intf.ReadSettings(FSettings);
    aXMLElement.SetAttribute('version', '1');
    SaveGridColumnsSettingToXmlElement(FSettings, aXMLElement.AddElement('columns'));
    if Assigned(FFormulaFields) then
      SaveFormulaFieldsToXmlElement(FFormulaFields, aXMLElement.AddElement('formulaFields'));
    if Assigned(Intf.GetSummaryManager) then
      SaveSummaryDefinitionsToXmlElement(Intf.GetSummaryManager.GetSummaryDefinitions, aXMLElement.AddElement('summaries'));
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
  if FGrid.GetInterface(SImGridInterface, Intf) then
  begin
    fields := TmFields.Create;
    columns := TmGridColumns.Create;
    CSVBuilder := TmCSVBuilder.Create;
    try
      CSVBuilder.Stream := aStream;
      CSVBuilder.StartWrite;

      Intf.GetColumns(columns);
      Intf.GetFields(fields);

      Intf.GetDataCursor.StartBrowsing;
      try
        for i := 0 to columns.Count - 1 do
        begin
          if columns.Get(i).Visible then
            CSVBuilder.AppendCell(columns.Get(i).Title);
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
                  CSVBuilder.AppendCell('')
                else
                  CSVBuilder.AppendCell(IntToStr(curValue));
              end
              else
              if (curField.DataType = ftFloat) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCell('')
                else
                  CSVBuilder.AppendCell(FloatToStr(curValue));
              end
              else
              if (curField.DataType = ftDate) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCell('')
                else
                  CSVBuilder.AppendCell(DateToStr(curValue));
              end
              else
              if (curField.DataType = ftDateTime) or (curField.DataType = ftTime) or (curField.DataType = ftTimeStamp) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCell('')
                else
                  CSVBuilder.AppendCell(DateTimeToStr(curValue));
              end
              else
              if (curField.DataType = ftBoolean) then
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCell(CSVBuilder.QuoteChar +  CSVBuilder.QuoteChar)
                else
                  CSVBuilder.AppendCell(CSVBuilder.QuoteChar + BoolToStr(curValue, true) + CSVBuilder.QuoteChar);
              end
              else
              begin
                if VarIsNull(curValue) then
                  CSVBuilder.AppendCell(CSVBuilder.QuoteChar +  CSVBuilder.QuoteChar)
                else
                  CSVBuilder.AppendCell(CSVBuilder.QuoteChar + VarToStr(curValue) + CSVBuilder.QuoteChar);
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

procedure TmAbstractGridHelper.ExportGridAsXls(aStream: TStream);
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
begin
  fields := TmFields.Create;
  columns := TmGridColumns.Create;
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');

    if FGrid.GetInterface(SImGridInterface, Intf) then
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
            MyWorksheet.WriteText(0, col, columns.Get(i).Title);
            MyWorksheet.WriteBackgroundColor(0, col, $00D0D0D0);
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
                if (curField.DataType = ftSmallint) or (curField.DataType = ftInteger) or (curField.DataType = ftLargeint) then
                  MyWorksheet.WriteNumber(row, col, curValue, nfGeneral, 0)
                else
                if (curField.DataType = ftFloat) then
                  MyWorksheet.WriteNumber(row, col, curValue)
                else
                if (curField.DataType = ftDate) then
                begin
                  d := curValue;
                  MyWorksheet.WriteDateTime(row, col, round(d), nfShortDate);
                end
                else
                if (curField.DataType = ftDateTime) or (curField.DataType = ftTime) or (curField.DataType = ftTimeStamp) then
                begin
                  d := curValue;
                  if DoublesAreEqual(0, abs(d - round(d))) then
                    MyWorksheet.WriteDateTime(row, col, curValue, nfShortDate)
                  else
                    MyWorksheet.WriteDateTime(row, col, curValue, nfShortDateTime);
                end
                else
                if (curField.DataType = ftBoolean) then
                  MyWorksheet.WriteBoolValue(row, col, curValue)
                else
                  MyWorksheet.WriteText(row, col, VarToStr(curValue));
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
    MyWorkbook.WriteToStream(aStream, sfExcel8);
  finally
    MyWorkbook.Free;
    columns.Free;
    fields.Free;
  end;
end;

procedure TmAbstractGridHelper.OnExportGridAsXls(Sender : TObject);
begin
  ExportGridToFile('XLS');
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

    if FGrid.GetInterface(SImGridInterface, Intf) then
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
begin
  if dgMultiselect in FDBGrid.Options then
  begin
    FDBGrid.ClearSelections;
    FDBGrid.DataSource.DataSet.DisableControls;
    try
      FDBGrid.DataSource.DataSet.First;
      while not FDBGrid.DataSource.DataSet.EOF do
      begin
        FDBGrid.SelectedRows.CurrentRowSelected:= true;
        FDBGrid.DataSource.DataSet.Next;
      end;
    finally
      FDBGrid.DataSource.DataSet.EnableControls;
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
