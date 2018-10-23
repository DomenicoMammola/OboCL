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
  DBGrids, Controls, Menus, LCLIntf,
  ATButtons,
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats, // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  mGridColumnSettings, mXML,
  mGridSettingsForm, mFormulaFieldsConfigurationForm,
  mDBGrid, mNullables,
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
  SConfigureFormulaFieldsCommandHint = 'Configure formula fields..';
  SConfigureGridCommandCaption = 'Configure grid';
  SConfigureFormulaFieldsCommandCaption = 'Configure formula fields';
  SExportGridAsCsvCommandHint = 'Export grid data to csv file';
  SExportGridAsCsvCommandCaption = 'Export to csv file...';
  SExportGridAsXlsCommandHint = 'Export grid data to Excel file (.xls)';
  SExportGridAsXlsCommandCaption = 'Export to Excel file (.xls)...';
  SExportGridAsHtmlCommandHint = 'Export grid data to html file (.html)';
  SExportGridAsHtmlCommandCaption = 'Export to html file (.html)...';

type

  { TmDBGridHelper }

  TmDBGridHelper = class
  protected
    FSettings : TmGridColumnsSettings;
    FDBGrid : TmDBGrid;
    FFormulaFields : TmFormulaFields;
    FSaveDialog: TSaveDialog;
    FConfigurePopupMenu : TPopupMenu;

    function ConfirmFileOverwrite : boolean;
    procedure ExportGridToFile(aFileType : String);
  public
    constructor Create(aGrid : TmDBGrid;aFormulaFields : TmFormulaFields); virtual;
    destructor Destroy; override;

    procedure SetupGrid;
    procedure CreateStandardConfigureMenu(aToolbar : TToolbar; const aConfigureImageIndex : integer);
    //procedure CreateStandardConfigureMenu(aToolbar : TATButtonsToolbar; const aConfigureImageIndex : integer); virtual;

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

    procedure SelectAllRows;

    property Grid : TmDBGrid read FDBGrid;
  end;


function GetLastUsedFolderForExport : TNullableString;

implementation

uses
  SysUtils,
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


function TmDBGridHelper.ConfirmFileOverwrite: boolean;
begin
  Result := MessageDlg(SConfirmFileOverwriteCaption, SConfirmFileOverwriteMessage, mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TmDBGridHelper.ExportGridToFile(aFileType: String);
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
    _LastUsedFolderForExport.Value:= ExtractFileDir(FSaveDialog.FileName);
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

constructor TmDBGridHelper.Create(aGrid : TmDBGrid; aFormulaFields : TmFormulaFields);
begin
  FSettings := TmGridColumnsSettings.Create;
  FDBGrid := aGrid;
  FFormulaFields:= aFormulaFields;
  FSaveDialog := TSaveDialog.Create(nil);
end;

destructor TmDBGridHelper.Destroy;
begin
  FDBGrid := nil;
  FSettings.Free;
  FSaveDialog.Free;
  inherited Destroy;
end;

procedure TmDBGridHelper.SetupGrid;
begin
  FDBGrid.Align:= alClient;
  FDBGrid.AlternateColor:= clMoneyGreen;
  FDBGrid.Flat := True;
  FDBGrid.Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgAutoSizeColumns, dgDisableDelete, dgDisableInsert, dgMultiselect];
end;


procedure TmDBGridHelper.CreateStandardConfigureMenu(aToolbar: TToolbar; const aConfigureImageIndex : integer);
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

function TmDBGridHelper.EditSettings : boolean;
var
  frm : TGridSettingsForm;
  OldCursor : TCursor;
begin
  Result := false;
  FDBGrid.ReadSettings(FSettings);
  frm := TGridSettingsForm.Create(nil);
  try
    frm.Init(FSettings);
    if frm.ShowModal = mrOk then
    begin
      OldCursor:= Screen.Cursor;
      try
        Screen.Cursor:= crHourGlass;
        FDBGrid.ApplySettings(FSettings);
      finally
        Screen.Cursor:= OldCursor;
      end;
      Result := true;
    end;
  finally
    frm.Free;
  end;
end;

procedure TmDBGridHelper.OnEditSettings(Sender: TObject);
begin
  Self.EditSettings;
end;

procedure TmDBGridHelper.OnEditFormulaFields(Sender: TObject);
var
  frm : TFormulaFieldsConfigurationForm;
begin
  frm := TFormulaFieldsConfigurationForm.Create(nil);
  try
    frm.Init(FFormulaFields, FDBGrid.DataSource.DataSet.Fields);
    if frm.ShowModal = mrOk then
    begin
      FDBGrid.ReadSettings(FSettings);
      FDBGrid.DataSource.DataSet.Close;
      FDBGrid.DataSource.DataSet.Open;
      FDBGrid.ApplySettings(FSettings);
    end;
  finally
    frm.Free;
  end;
end;

procedure TmDBGridHelper.LoadSettings(aStream: TStream);
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

procedure TmDBGridHelper.SaveSettings(aStream: TStream);
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

procedure TmDBGridHelper.LoadSettingsFromXML(aXMLElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
begin
  cursor := TmXmlElementCursor.Create(aXMLElement, 'columns');
  try
    LoadGridColumnsSettingFromXmlElement(FSettings, cursor.Elements[0]);
  finally
    cursor.Free;
  end;
  cursor := TmXmlElementCursor.Create(aXMLElement, 'formulaFields');
  try
    if cursor.Count > 0 then
    begin
      LoadFormulaFieldsFromXmlElement(FFormulaFields, cursor.Elements[0]);
      if FFormulaFields.Count > 0 then
      begin
        FDBGrid.DataSource.DataSet.Close;
        FDBGrid.DataSource.DataSet.Open;
      end;
    end;
  finally
    cursor.Free;
  end;
  if Assigned(FDBGrid.SummaryManager) then
  begin
    cursor := TmXmlElementCursor.Create(aXMLElement, 'summaries');
    try
      if cursor.Count > 0 then
      begin
        LoadSummaryDefinitionsFromXmlElement(FDBGrid.SummaryManager.GetSummaryDefinitions, cursor.Elements[0]);
      end;
    finally
      cursor.Free;
    end;
  end;
  FDBGrid.ApplySettings(FSettings);
  if Assigned(FDBGrid.SummaryManager) then
    FDBGrid.SummaryManager.RefreshSummaries;
end;

procedure TmDBGridHelper.SaveSettingsToXML(aXMLElement: TmXMLElement);
begin
  FDBGrid.ReadSettings(FSettings);
  aXMLElement.SetAttribute('version', '1');
  SaveGridColumnsSettingToXmlElement(FSettings, aXMLElement.AddElement('columns'));
  if Assigned(FFormulaFields) then
    SaveFormulaFieldsToXmlElement(FFormulaFields, aXMLElement.AddElement('formulaFields'));
  if Assigned(FDBGrid.SummaryManager) then
    SaveSummaryDefinitionsToXmlElement(FDBGrid.SummaryManager.GetSummaryDefinitions, aXMLElement.AddElement('summaries'));
end;

procedure TmDBGridHelper.ExportGridAsCsv(aStream: TStream);
var
  tmpFields : TFields;
  i, rn : integer;
  str, sep : AnsiString;
begin
  FDBGrid.DataSource.DataSet.DisableControls;
  try
    tmpFields := FDBGrid.DataSource.DataSet.Fields;
    sep := '';
    str := '';
    for i := 0 to tmpFields.Count - 1 do
    begin
      str := str + sep + tmpFields[i].DisplayLabel;
      sep := ';';
    end;
    str := str + sLineBreak;

    aStream.WriteBuffer(PAnsiChar(str)^,Length(str));
    rn := FDBGrid.DataSource.DataSet.RecNo;

    FDBGrid.DataSource.DataSet.First;
    while not FDBGrid.DataSource.DataSet.EOF do
    begin
      sep := '';
      str := '';
      for i := 0 to tmpFields.Count - 1 do
      begin
        str := str + sep + tmpFields[i].AsString;
        sep := ';';
      end;

      str := str + sLineBreak;
      aStream.WriteBuffer(PAnsiChar(str)^,Length(str));

      FDBGrid.DataSource.DataSet.Next;
    end;
    FDBGrid.DataSource.DataSet.RecNo:= rn;


  finally
    FDBGrid.DataSource.DataSet.EnableControls;
  end;
end;

procedure TmDBGridHelper.OnExportGridAsCsv(Sender : TObject);
begin
  ExportGridToFile('CSV');
end;

{$IFDEF FPC}

procedure TmDBGridHelper.ExportGridAsXls(aStream: TStream);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet : TsWorksheet;
  i, rn, row, col : integer;
  tmpField : TField;
begin
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');

    FDBGrid.DataSource.DataSet.DisableControls;
    try
      col := 0;
      for i := 0 to FDBGrid.Columns.Count - 1 do
      begin
        if FDBGrid.Columns.Items[i].Visible then
        begin
          MyWorksheet.WriteText(0, col, FDBGrid.Columns.Items[i].Title.Caption);
          MyWorksheet.WriteBackgroundColor(0, col, $00D0D0D0);
          inc (col);
        end;
      end;

      rn := FDBGrid.DataSource.DataSet.RecNo;
      row := 1;

      FDBGrid.DataSource.DataSet.First;
      while not FDBGrid.DataSource.DataSet.EOF do
      begin
        col := 0;

        for i := 0 to FDBGrid.Columns.Count - 1 do
        begin
          if FDBGrid.Columns.Items[i].Visible then
          begin
            tmpField := FDBGrid.Columns.Items[i].Field;
            if not tmpField.IsNull then
            begin
              if (tmpField.DataType = ftSmallint) or (tmpField.DataType = ftInteger) or (tmpField.DataType = ftLargeint) then
                MyWorksheet.WriteNumber(row, col, tmpField.AsFloat, nfGeneral, 0)
              else
              if (tmpField.DataType = ftFloat) then
                MyWorksheet.WriteNumber(row, col, tmpField.AsFloat)
              else
              if (tmpField.DataType = ftDate) then
                MyWorksheet.WriteDateTime(row, col, round(tmpField.AsDateTime), nfShortDate)
              else
              if (tmpField.DataType = ftDateTime) or (tmpField.DataType = ftTime) or (tmpField.DataType = ftTimeStamp) then
              begin
                if tmpField.AsDateTime = round(tmpField.AsDateTime) then
                  MyWorksheet.WriteDateTime(row, col, tmpField.AsDateTime, nfShortDate)
                else
                  MyWorksheet.WriteDateTime(row, col, tmpField.AsDateTime, nfShortDateTime);
              end
              else
              if (tmpField.DataType = ftBoolean) then
                MyWorksheet.WriteBoolValue(row, col, tmpField.AsBoolean)
              else
                MyWorksheet.WriteText(row, col, tmpField.AsString);
            end;
            inc(col);
          end;
        end;

        inc(row);
        FDBGrid.DataSource.DataSet.Next;
      end;
      FDBGrid.DataSource.DataSet.RecNo:= rn;

    finally
      FDBGrid.DataSource.DataSet.EnableControls;
    end;
    MyWorkbook.WriteToStream(aStream, sfExcel8);
  finally
    MyWorkbook.Free;
  end;
end;

procedure TmDBGridHelper.OnExportGridAsXls(Sender : TObject);
begin
  ExportGridToFile('XLS');
end;

procedure TmDBGridHelper.ExportGridAsHtml(aStream: TStream);
var
  htmlDoc : TStringList;
  i, rn, row, col : integer;
  tmpField : TField;
begin
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

    FDBGrid.DataSource.DataSet.DisableControls;
    try
      col := 0;
      htmlDoc.Add('<tr>');
      for i := 0 to FDBGrid.Columns.Count - 1 do
      begin
        if FDBGrid.Columns.Items[i].Visible then
        begin
          htmlDoc.Add('<th>' + FDBGrid.Columns.Items[i].Title.Caption + '</th>');
          inc (col);
        end;
      end;
      htmlDoc.Add('</tr>');

      rn := FDBGrid.DataSource.DataSet.RecNo;
      row := 1;


      FDBGrid.DataSource.DataSet.First;
      while not FDBGrid.DataSource.DataSet.EOF do
      begin
        col := 0;

        htmlDoc.Add('<tr>');

        for i := 0 to FDBGrid.Columns.Count - 1 do
        begin
          if FDBGrid.Columns.Items[i].Visible then
          begin
            htmlDoc.Add('<td>');
            tmpField := FDBGrid.Columns.Items[i].Field;
            if not tmpField.IsNull then
            begin
              if (tmpField.DataType = ftSmallint) or (tmpField.DataType = ftInteger) or (tmpField.DataType = ftLargeint) then
                htmlDoc.Add(IntToStr(tmpField.AsInteger))
              else
              if (tmpField.DataType = ftFloat) then
              begin
                if (tmpField as TFloatField).DisplayFormat <> '' then
                  htmlDoc.Add(FormatFloat((tmpField as TFloatField).DisplayFormat, tmpField.AsFloat))
                else
                  htmlDoc.Add(FloatToStr(tmpField.AsFloat));
              end
              else
              if (tmpField.DataType = ftDate) then
              begin
                htmlDoc.Add(DateToStr(tmpField.AsDateTime));
              end
              else
              if (tmpField.DataType = ftDateTime) or (tmpField.DataType = ftTimeStamp) then
              begin
                htmlDoc.Add(DateTimeToStr(tmpField.AsDateTime));
              end
              else
              if (tmpField.DataType = ftTime) then
              begin
                htmlDoc.Add(TimeToStr(tmpField.AsDateTime));
              end
              else
              if (tmpField.DataType = ftBoolean) then
              begin
                htmlDoc.Add(BoolToStr(tmpField.AsBoolean, true));
              end
              else
                htmlDoc.Add(tmpField.AsString);
            end;
            htmlDoc.Add('</td>');
            inc(col);
          end;
        end;
        htmlDoc.Add('</tr>');

        inc(row);
        FDBGrid.DataSource.DataSet.Next;
      end;
      FDBGrid.DataSource.DataSet.RecNo:= rn;

    finally
      FDBGrid.DataSource.DataSet.EnableControls;
    end;
    htmlDoc.Add('</table>');

    for i := 0 to FDBGrid.SummaryManager.GetSummaryValues.Count - 1 do
    begin
      htmlDoc.Add('<p>' + FDBGrid.SummaryManager.GetSummaryValues.Get(i).FormattedValue + '</p>');
    end;

    htmlDoc.Add('</body>');
    htmlDoc.Add('</html>');
    htmlDoc.SaveToStream(aStream);
  finally
    htmlDoc.Free;
  end;
end;

procedure TmDBGridHelper.OnExportGridAsHtml(Sender: TObject);
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
