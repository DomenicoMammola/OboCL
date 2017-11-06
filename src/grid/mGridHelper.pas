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
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats, // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  mGridColumnSettings, mXML,
  mGridSettingsForm, mFormulaFieldsConfigurationForm,
  mDBGrid,
  mVirtualDatasetFormulas;

resourcestring
  SCSVFileDescription = 'Comma Separated Values files';
  SExcelFileDescription = 'Excel 97-2003 files';
  SUnableToWriteFileMessage = 'Unable to write file. Check if the file is open by another application. If so, close it and run this command again. Detail:';
  SConfirmFileOverwriteCaption = 'Confirm';
  SConfirmFileOverwriteMessage = 'The selected file already exists. Overwrite it?';
  SWantToOpenFileMessage = 'Do you want to open the file?';
  SConfigureCommandHint = 'Configure..';
  SConfigureGridCommandHint = 'Configure grid..';
  SConfigureFormulaFieldsCommandHint = 'Configure formula fields..';
  SConfigureGridCommandCaption = 'Configure grid';
  SConfigureFormulaFieldsCommandCaption = 'Configure formula fields';
  SExportGridAsCsvCommandHint = 'Export grid data to csv file';
  SExportGridAsCsvCommandCaption = 'Export to csv file..';
  SExportGridAsXlsCommandHint = 'Export grid data to Excel file (.xls)';
  SExportGridAsXlsCommandCaption = 'Export to Excel file (.xls)..';

type

  { TmDBGridHelper }

  TmDBGridHelper = class
  strict private
    FSettings : TmGridColumnsSettings;
    FDBGrid : TmDBGrid;
    FFormulaFields : TmFormulaFields;
    FSaveDialog: TSaveDialog;
    function ConfirmFileOverwrite : boolean;
    procedure ExportGridToFile(aFileType : String);
  public
    constructor Create(aGrid : TmDBGrid;aFormulaFields : TmFormulaFields);
    destructor Destroy; override;

    procedure SetupGrid;
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

    procedure SelectAllRows;

    property Grid : TmDBGrid read FDBGrid;
  end;

implementation

uses
  SysUtils,
  mVirtualDatasetFormulasToXml, mGridColumnSettingsToXml;

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
  end;
  if FSaveDialog.Execute then
  begin
    if FileExists(FSaveDialog.FileName) then
    begin
      if not ConfirmFileOverwrite then
        exit;
    end;
    try
      oldCursor := Screen.Cursor;
      try
        Screen.Cursor:= crHourGlass;
        fs := TFileStream.Create(FSaveDialog.FileName, fmCreate);
        try
          if aFileType = 'CSV' then
            Self.ExportGridAsCsv(fs)
          else if aFileType = 'XLS' then
            Self.ExportGridAsXls(fs);
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
  tmpConfigurePopupMenu : TPopupMenu;
  tmp : TToolButton;
  itm : TMenuItem;
begin
  tmpConfigurePopupMenu := TPopupMenu.Create(aToolbar);
//  tmpConfigurePopupMenu.Images := aToolbar.Images;

  tmp := TToolButton.Create(aToolbar);
  tmp.Style:= tbsDropDown;
  tmp.DropdownMenu := tmpConfigurePopupMenu;
  tmp.ImageIndex := aConfigureImageIndex;
  tmp.Parent := aToolbar;
  tmp.Hint := SConfigureCommandHint;
  itm := TMenuItem.Create(tmpConfigurePopupMenu);
  tmpConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnEditSettings;
  itm.Hint:= SConfigureGridCommandHint;
  itm.Caption:= SConfigureGridCommandCaption;
  if Assigned(FFormulaFields) then
  begin
    itm := TMenuItem.Create(tmpConfigurePopupMenu);
    tmpConfigurePopupMenu.Items.Add(itm);
    itm.OnClick:= Self.OnEditFormulaFields;
    itm.Hint:= SConfigureFormulaFieldsCommandHint;
    itm.Caption:= SConfigureFormulaFieldsCommandCaption;
  end;

  itm := TMenuItem.Create(tmpConfigurePopupMenu);
  itm.Caption:= '-';
  tmpConfigurePopupMenu.Items.Add(itm);

  itm := TMenuItem.Create(tmpConfigurePopupMenu);
  tmpConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsCsv;
  itm.Hint:= SExportGridAsCsvCommandHint;
  itm.Caption:= SExportGridAsCsvCommandCaption;

  itm := TMenuItem.Create(tmpConfigurePopupMenu);
  tmpConfigurePopupMenu.Items.Add(itm);
  itm.OnClick:= Self.OnExportGridAsXls;
  itm.Hint:= SExportGridAsXlsCommandHint;
  itm.Caption:= SExportGridAsXlsCommandCaption;

  tmp := TToolButton.Create(aToolbar);
  tmp.Style:= tbsDivider;
  tmp.Parent := aToolbar;
end;

function TmDBGridHelper.EditSettings : boolean;
var
  frm : TGridSettingsForm;
begin
  Result := false;
  FDBGrid.ReadSettings(FSettings);
  frm := TGridSettingsForm.Create(nil);
  try
    frm.Init(FSettings);
    if frm.ShowModal = mrOk then
    begin
      FDBGrid.ApplySettings(FSettings);
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
    frm.Init(FFormulaFields);
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
  FDBGrid.ApplySettings(FSettings);
end;

procedure TmDBGridHelper.SaveSettingsToXML(aXMLElement: TmXMLElement);
begin
  FDBGrid.ReadSettings(FSettings);
  aXMLElement.SetAttribute('version', '1');
  SaveGridColumnsSettingToXmlElement(FSettings, aXMLElement.AddElement('columns'));
  if Assigned(FFormulaFields) then
    SaveFormulaFieldsToXmlElement(FFormulaFields, aXMLElement.AddElement('formulaFields'));
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
  tmpFields : TFields;
  i, rn, row, col : integer;
  tmpField : TField;
begin
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');

    FDBGrid.DataSource.DataSet.DisableControls;
    try
      tmpFields := FDBGrid.DataSource.DataSet.Fields;
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
(*      for i := 0 to tmpFields.Count - 1 do
      begin
        if tmpFields[i].Visible then
        begin
          MyWorksheet.WriteText(0, col, tmpFields[i].DisplayLabel);
          MyWorksheet.WriteBackgroundColor(0, col, $00D0D0D0);
          inc (col);
        end;
      end;*)

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
              if (tmpField.DataType = ftDate) or (tmpField.DataType = ftDateTime) or (tmpField.DataType = ftTime) or (tmpField.DataType = ftTimeStamp) then
                MyWorksheet.WriteDateTime(row, col, tmpField.AsDateTime)
              else
              if (tmpField.DataType = ftBoolean) then
                MyWorksheet.WriteBoolValue(row, col, tmpField.AsBoolean)
              else
                MyWorksheet.WriteText(row, col, tmpField.AsString);
            end;
            inc(col);
          end;
        end;
(*        for i := 0 to tmpFields.Count - 1 do
        begin
          if tmpFields[i].Visible then
          begin
            if not tmpFields[i].IsNull then
            begin
              if (tmpFields[i].DataType = ftSmallint) or (tmpFields[i].DataType = ftInteger) or (tmpFields[i].DataType = ftLargeint) then
                MyWorksheet.WriteNumber(row, col, tmpFields[i].AsFloat, nfGeneral, 0)
              else
              if (tmpFields[i].DataType = ftFloat) then
                MyWorksheet.WriteNumber(row, col, tmpFields[i].AsFloat)
              else
              if (tmpFields[i].DataType = ftDate) or (tmpFields[i].DataType = ftDateTime) or (tmpFields[i].DataType = ftTime) or (tmpFields[i].DataType = ftTimeStamp) then
                MyWorksheet.WriteDateTime(row, col, tmpFields[i].AsDateTime)
              else
              if (tmpFields[i].DataType = ftBoolean) then
                MyWorksheet.WriteBoolValue(row, col, tmpFields[i].AsBoolean)
              else
                MyWorksheet.WriteText(row, col, tmpFields[i].AsString);
            end;
            inc(col);
          end;
        end;*)

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

procedure TmDBGridHelper.SelectAllRows;
begin
  if dgMultiselect in FDBGrid.Options then
  begin
    FDBGrid.DataSource.DataSet.DisableControls;
    try
      FDBGrid.ClearSelections;
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

end.
