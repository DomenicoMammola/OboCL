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
  Classes,
  DBGrids, Controls,
  mGridColumnSettings, mXML, mGridSettingsForm;

type

  { TmDBGridHelper }

  TmDBGridHelper = class
  strict private
    FSettings : TmGridColumnsSettings;
    FDBGrid : TDBGrid;
  public
    constructor Create;
    destructor Destroy; override;
    function EditSettings : boolean;
    procedure LoadSettings (aStream : TStream);
    procedure SaveSettings (aStream : TStream);

    property Grid : TDBGrid read FDBGrid write FDBGrid;
  end;

implementation

{ TmDBGridHelper }

constructor TmDBGridHelper.Create;
begin
  FSettings := TmGridColumnsSettings.Create;
end;

destructor TmDBGridHelper.Destroy;
begin
  FSettings.Free;
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

end.
