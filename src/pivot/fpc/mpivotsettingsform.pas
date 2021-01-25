// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPivotSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls,
  mPivotSettingsFrame, mPivoter;


resourcestring
  STabFieldsSettings = 'Fields';

type

  { TPivotSettingsForm }

  TPivotSettingsForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    PCSettings: TPageControl;
    TSFields: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FPivotFieldsSettingsFrame : TPivotFieldsSettingsFrame;
  public
    procedure Init (aPivoter : TmPivoter);
  end;


implementation

{$R *.lfm}

{ TPivotSettingsForm }

procedure TPivotSettingsForm.FormCreate(Sender: TObject);
begin
  TSFields.Caption:= STabFieldsSettings;

  FPivotFieldsSettingsFrame := TPivotFieldsSettingsFrame.Create(Self);
  FPivotFieldsSettingsFrame.Parent := TSFields;
  FPivotFieldsSettingsFrame.Align:= alClient;
end;

procedure TPivotSettingsForm.OkBtnClick(Sender: TObject);
begin
  FPivotFieldsSettingsFrame.UpdateSettings;
  Self.ModalResult:= mrOk;
end;

procedure TPivotSettingsForm.Init(aPivoter : TmPivoter);
begin
  FPivotFieldsSettingsFrame.Init(aPivoter);
end;

end.

