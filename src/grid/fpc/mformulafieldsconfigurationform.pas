// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFormulaFieldsConfigurationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, DB,
  mformulafieldsconfigurationframe,
  mVirtualDatasetFormulas;


type
  { TFormulaFieldsConfigurationForm }

  TFormulaFieldsConfigurationForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FFormulaFieldsFrame: TFormulaFieldsConfFrame;
  public
    procedure Init (aFormulaFields: TmFormulaFields; const aFields: TFields);
  end;


implementation

{$R *.lfm}

{ TFormulaFieldsConfigurationForm }

procedure TFormulaFieldsConfigurationForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TFormulaFieldsConfigurationForm.OkBtnClick(Sender: TObject);
begin
  if not FFormulaFieldsFrame.Check then
    exit;
  FFormulaFieldsFrame.UpdateFormulaFields;
  Self.ModalResult:= mrOk;
end;

procedure TFormulaFieldsConfigurationForm.Init(aFormulaFields: TmFormulaFields; const aFields: TFields);
begin
  FFormulaFieldsFrame := TFormulaFieldsConfFrame.Create(Self);
  FFormulaFieldsFrame.Parent := Self;
  FFormulaFieldsFrame.Align:= alClient;
  FFormulaFieldsFrame.Init(aFormulaFields, aFields);
end;

end.

