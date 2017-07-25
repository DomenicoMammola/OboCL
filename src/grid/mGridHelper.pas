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
  Classes, DB, Dialogs, Forms,
  DBGrids, Controls, Menus,
  {$IFDEF FPC}
  fpstypes, fpspreadsheet, fpsallformats,
  {$ENDIF}
  mGridColumnSettings, mXML, mGridSettingsForm, mSortConditions, mGridIcons,
  mDatasetInterfaces;

type

  { TmDBGridHelper }

  TmDBGridHelper = class
  strict private
    FSettings : TmGridColumnsSettings;
    FDBGrid : TDBGrid;
    FSortManager : ISortableDatasetManager;
    FGridIcons: TmGridIconsDataModule;
    FHeaderPopupMenu : TPopupMenu;
    FCurrentGridCol : longint;
    FGridPopupMenu : TPopupMenu;
    FSaveDialog: TSaveDialog;
    // original events
    FOriginalOnTitleClick : TDBGridClickEvent;
    FOriginalOnMouseDown : TMouseEvent;
    procedure OnTitleClick (Column: TColumn);
    procedure OnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BuildHeaderPopupMenu;
    function ConfirmFileOverwrite : boolean;
    procedure ExportGridToFile(aFileType : String);
  public
    constructor Create(aGrid : TDBGrid);
    destructor Destroy; override;
    function EditSettings : boolean;
    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);
    procedure EnableSort (aSortManager : ISortableDatasetManager);
    procedure EnableHeaderPopupMenu (aOriginalGridPopupMenu : TPopupMenu);
    procedure ExportGridAsCsv (aStream : TStream); overload;
    procedure ExportGridAsCsv (Sender : TObject); overload;
    procedure ExportGridAsXls (aStream : TStream); overload;
    procedure ExportGridAsXls (Sender : TObject); overload;

    property Grid : TDBGrid read FDBGrid;
  end;

implementation

uses
  SysUtils;

{ TmDBGridHelper }


// inspired by http://forum.lazarus.freepascal.org/index.php?topic=24510.0
procedure TmDBGridHelper.OnTitleClick(Column: TColumn);
var
  tmpSortType : TSortType;
  i, idx : integer;
begin
  try
    tmpSortType := stAscending;

    // remove every arrow from column captions
    for i := 0 to FDBGrid.Columns.Count - 1 do
      FDBGrid.Columns[i].Title.ImageIndex := -1;

    // analize current filter
    if (FSortManager.GetSorted) and (FSortManager.GetSortByConditions.Count > 0) and (FSortManager.GetSortByConditions.Items[0].FieldName = Column.FieldName) then
    begin
      if FSortManager.GetSortByConditions.Items[0].SortType = stAscending then
        tmpSortType:= stDescending
      else
      begin
        FSortManager.ClearSort;
        exit;
      end
    end;

    // set new sort condition
    FSortManager.GetSortByConditions.Clear;
    with FSortManager.GetSortByConditions.Add do
    begin
      FieldName:= Column.FieldName;
      SortType:= tmpSortType;
    end;

    // do sort
    if FSortManager.Sort then
    begin
      if tmpSortType = stAscending then
        idx := 0
      else
        idx := 1;
      FDBGrid.Columns[Column.Index].Title.ImageIndex := idx;
    end;
  finally
    if Assigned(FOriginalOnTitleClick) then
      FOriginalOnTitleClick(Column);
  end;
end;

procedure TmDBGridHelper.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tmpCol, tmpRow : longint;
begin
  // https://www.codeproject.com/Articles/199506/Improving-Delphi-TDBGrid
  if Button = mbRight then
  begin
    if Y < FDBGrid.DefaultRowHeight then
    begin
       FDBGrid.PopupMenu := FHeaderPopupMenu;
       FDBGrid.MouseToCell(X, Y, tmpCol, tmpRow);
       FCurrentGridCol := tmpCol;
    end
    else
       FDBGrid.PopupMenu := FGridPopupMenu;
  end;

  if Assigned(FOriginalOnMouseDown) then
    FOriginalOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TmDBGridHelper.BuildHeaderPopupMenu;
var
  tmpMenuItem : TMenuItem;
begin
  if not Assigned(FHeaderPopupMenu) then
  begin
    FHeaderPopupMenu:= TPopupMenu.Create(FDBGrid);
    tmpMenuItem := TMenuItem.Create(FHeaderPopupMenu);
    tmpMenuItem.Caption:= 'Filter values..';
    FHeaderPopupMenu.Items.Add(tmpMenuItem);
    tmpMenuItem := TMenuItem.Create(FHeaderPopupMenu);
    tmpMenuItem.Caption:= 'Add summary..';
    FHeaderPopupMenu.Items.Add(tmpMenuItem);
  end;
end;

function TmDBGridHelper.ConfirmFileOverwrite: boolean;
begin
  Result := MessageDlg('Confirm', 'The selected file already exists. Overwrite it?', mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TmDBGridHelper.ExportGridToFile(aFileType: String);
var
  fs : TFileStream;
  oldCursor : TCursor;
begin
  if aFileType = 'CSV' then
  begin
    FSaveDialog.DefaultExt:= 'csv';
    FSaveDialog.Filter:='Comma Separated Values files|*.csv';
  end
  else
  if aFileType = 'XLS' then
  begin
    FSaveDialog.DefaultExt:= 'xls';
    FSaveDialog.Filter:='Excel 97-2003 files|*.xls';
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
    except
      on E:Exception do
      begin
        MessageDlg('Unable to write file. Check if the file is open by another application. If so, close it and run this command again.', mtInformation, [mbOk], 0);
      end;
    end;
  end;

end;

constructor TmDBGridHelper.Create(aGrid : TDBGrid);
begin
  FSettings := TmGridColumnsSettings.Create;
  FSortManager := nil;
  FDBGrid := aGrid;
  FSaveDialog := TSaveDialog.Create(nil);
end;

destructor TmDBGridHelper.Destroy;
begin
  FSortManager := nil;
  FDBGrid := nil;
  FSettings.Free;
  FSaveDialog.Free;
  inherited Destroy;
end;

function TmDBGridHelper.EditSettings : boolean;
var
  frm : TGridSettingsForm;
begin
  Result := false;
  ReadSettingsFromGrid(FSettings, FDBGrid);
  frm := TGridSettingsForm.Create(nil);
  try
    frm.Init(FSettings);
    if frm.ShowModal = mrOk then
    begin
      ApplySettingsToGrid(FSettings, FDBGrid);
      Result := true;
    end;
  finally
    frm.Free;
  end;
end;

procedure TmDBGridHelper.LoadSettings(aStream: TStream);
var
  doc : TmXmlDocument;
  cursor : TmXmlElementCursor;
begin
  doc := TmXmlDocument.Create;
  try
    doc.LoadFromStream(aStream);
    cursor := TmXmlElementCursor.Create(doc.RootElement, 'columns');
    try
      FSettings.LoadFromXmlElement(cursor.Elements[0]);
    finally
      cursor.Free;
    end;
  finally
    doc.Free;
  end;
  ApplySettingsToGrid(FSettings, FDBGrid);
end;

procedure TmDBGridHelper.SaveSettings(aStream: TStream);
var
  doc : TmXmlDocument;
  root : TmXmlElement;
begin
  ReadSettingsFromGrid(FSettings, FDBGrid);
  doc := TmXmlDocument.Create;
  try
    root := doc.CreateRootElement('configuration');
    root.SetAttribute('version', '1');
    FSettings.SaveToXmlElement(root.AddElement('columns'));
    doc.SaveToStream(aStream);
  finally
    doc.Free;
  end;
end;

procedure TmDBGridHelper.EnableSort(aSortManager : ISortableDatasetManager);
begin
  if Assigned(FDBGrid) then
  begin
    FGridIcons:= TmGridIconsDataModule.Create(FDBGrid);
    FDBGrid.TitleImageList := FGridIcons.GridImageList;
    FOriginalOnTitleClick := FDBGrid.OnTitleClick;
    FDBGrid.OnTitleClick:= Self.OnTitleClick;
    FSortManager:= aSortManager;
  end;
end;

procedure TmDBGridHelper.EnableHeaderPopupMenu(aOriginalGridPopupMenu : TPopupMenu);
begin
  if not Assigned(FHeaderPopupMenu) then
    Self.BuildHeaderPopupMenu;
  FGridPopupMenu := aOriginalGridPopupMenu;
  FOriginalOnMouseDown := FDBGrid.OnMouseDown;
  FDBGrid.OnMouseDown:= Self.OnMouseDown;
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

procedure TmDBGridHelper.ExportGridAsCsv(Sender : TObject);
begin
  ExportGridToFile('CSV');
end;

{$IFDEF FPC}

procedure TmDBGridHelper.ExportGridAsXls(aStream: TStream);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet : TsWorksheet;
  tmpFields : TFields;
  i, rn, row : integer;
begin
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');

    FDBGrid.DataSource.DataSet.DisableControls;
    try
      tmpFields := FDBGrid.DataSource.DataSet.Fields;
      for i := 0 to tmpFields.Count - 1 do
      begin
        MyWorksheet.WriteText(0, i, tmpFields[i].DisplayLabel);
        MyWorksheet.WriteBackgroundColor(0, i, $00D0D0D0);
      end;

      rn := FDBGrid.DataSource.DataSet.RecNo;
      row := 1;

      FDBGrid.DataSource.DataSet.First;
      while not FDBGrid.DataSource.DataSet.EOF do
      begin
        for i := 0 to tmpFields.Count - 1 do
        begin
          if not tmpFields[i].IsNull then
          begin
            if (tmpFields[i].DataType = ftSmallint) or (tmpFields[i].DataType = ftInteger) or (tmpFields[i].DataType = ftLargeint) then
              MyWorksheet.WriteNumber(row, i, tmpFields[i].AsFloat, nfGeneral, 0)
            else
            if (tmpFields[i].DataType = ftFloat) then
              MyWorksheet.WriteNumber(row, i, tmpFields[i].AsFloat)
            else
            if (tmpFields[i].DataType = ftDate) or (tmpFields[i].DataType = ftDateTime) or (tmpFields[i].DataType = ftTime) or (tmpFields[i].DataType = ftTimeStamp) then
              MyWorksheet.WriteDateTime(row, i, tmpFields[i].AsDateTime)
            else
            if (tmpFields[i].DataType = ftBoolean) then
              MyWorksheet.WriteBoolValue(row, i, tmpFields[i].AsBoolean)
            else
              MyWorksheet.WriteText(row, i, tmpFields[i].AsString);
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

procedure TmDBGridHelper.ExportGridAsXls(Sender : TObject);
begin
  ExportGridToFile('XLS');
end;

{$ELSE}

procedure TmDBGridHelper.ExportGridAsXls(aStream: TStream);
begin
  // TODO
end;
{$ENDIF}

end.
