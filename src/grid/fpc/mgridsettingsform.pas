// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGridSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls,
  mGridColumnsSettingsFrame, mformulafieldsconfigurationframe, mGridColumnSettings;


resourcestring
  STabColumnsSettings = 'Columns';
  STabFormulaFieldsConfiguration = 'Calculated fields';

type

  { TGridSettingsForm }

  TGridSettingsForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    PCSettings: TPageControl;
    TSFormulaFields: TTabSheet;
    TSColumns: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FColumnsSettingsFrame : TGridColumnsSettingsFrame;
    FFormulaFieldsFrame: TFormulaFieldsConfFrame;
  public
    procedure Init (aSettings : TmGridColumnsSettings);
  end;


implementation

{$R *.lfm}

{ TGridSettingsForm }

procedure TGridSettingsForm.FormCreate(Sender: TObject);
begin
  TSColumns.Caption:= STabColumnsSettings;
  TSFormulaFields.Caption:= STabFormulaFieldsConfiguration;

  FColumnsSettingsFrame := TGridColumnsSettingsFrame.Create(Self);
  FColumnsSettingsFrame.Parent := TSColumns;
  FColumnsSettingsFrame.Align:= alClient;

  FFormulaFieldsFrame := TFormulaFieldsConfFrame.Create(Self);
  FFormulaFieldsFrame.Parent := TSFormulaFields;
  FFormulaFieldsFrame.Align:= alClient;
end;

procedure TGridSettingsForm.OkBtnClick(Sender: TObject);
begin
  FColumnsSettingsFrame.UpdateSettings;
  Self.ModalResult:= mrOk;
end;

procedure TGridSettingsForm.Init(aSettings: TmGridColumnsSettings);
begin
  FColumnsSettingsFrame.Init(aSettings);
  TSFormulaFields.Visible:= false;
end;

end.

