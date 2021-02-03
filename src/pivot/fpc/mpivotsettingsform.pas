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
  mPivotFieldsSettingsFrame, mPivotPropertiesFrame, mPivoter;


resourcestring
  STabFieldsSettings = 'Fields';
  STabProperties = 'Properties';

type

  { TPivotSettingsForm }

  TPivotSettingsForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    PCSettings: TPageControl;
    TSProperties: TTabSheet;
    TSFields: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FPivotFieldsSettingsFrame : TPivotFieldsSettingsFrame;
    FPivotPropertiesFrame : TPivotPropertiesFrame;
  public
    procedure Init (aPivoter : TmPivoter);
    function SomethingChanged : boolean;

    procedure UpdateSettingsInPivot (aPivoter: TmPivoter);
  end;


implementation

{$R *.lfm}

{ TPivotSettingsForm }

procedure TPivotSettingsForm.FormCreate(Sender: TObject);
begin
  TSFields.Caption:= STabFieldsSettings;
  TSProperties.Caption:= STabProperties;

  FPivotFieldsSettingsFrame := TPivotFieldsSettingsFrame.Create(Self);
  FPivotFieldsSettingsFrame.Parent := TSFields;
  FPivotFieldsSettingsFrame.Align:= alClient;

  FPivotPropertiesFrame := TPivotPropertiesFrame.Create(Self);
  FPivotPropertiesFrame.Parent := TSProperties;
  FPivotPropertiesFrame.Align := alClient;
end;

procedure TPivotSettingsForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TPivotSettingsForm.Init(aPivoter : TmPivoter);
begin
  FPivotFieldsSettingsFrame.Init(aPivoter);
  FPivotPropertiesFrame.Init(aPivoter);
end;

function TPivotSettingsForm.SomethingChanged: boolean;
begin
  Result := FPivotFieldsSettingsFrame.SomethingChanged or FPivotPropertiesFrame.SomethingChanged;
end;

procedure TPivotSettingsForm.UpdateSettingsInPivot(aPivoter: TmPivoter);
begin
  FPivotFieldsSettingsFrame.UpdateSettings(aPivoter);
  FPivotPropertiesFrame.UpdateSettings(aPivoter);
end;

end.

