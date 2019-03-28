// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mCellDecorationsConfigurationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, DB,
  mcelldecorationsconfigurationframe,
  mCellDecorations, mFields;

resourcestring
  SCaptionCellDecorationsConfigurationForm = 'Cell decorations';

type
  { TCellDecorationsConfigurationForm }

  TCellDecorationsConfigurationForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FCellDecorationsFrame: TCellDecorationsConfFrame;
  public
    procedure Init (aCellDecorations: TmCellDecorations; const aFields: TmFields);
  end;


implementation

uses
  mFormSetup;


{$R *.lfm}

{ TCellDecorationsConfigurationForm }

procedure TCellDecorationsConfigurationForm.FormCreate(Sender: TObject);
begin
  Self.Caption:= SCaptionCellDecorationsConfigurationForm;
  SetupFormAndCenter(Self);
end;

procedure TCellDecorationsConfigurationForm.OkBtnClick(Sender: TObject);
begin
  if not FCellDecorationsFrame.Check then
    exit;
  FCellDecorationsFrame.UpdateCellDecorations;
  Self.ModalResult:= mrOk;
end;

procedure TCellDecorationsConfigurationForm.Init(aCellDecorations: TmCellDecorations; const aFields: TmFields);
begin
  FCellDecorationsFrame := TCellDecorationsConfFrame.Create(Self);
  FCellDecorationsFrame.Parent := Self;
  FCellDecorationsFrame.Align:= alClient;
  FCellDecorationsFrame.Init(aCellDecorations, aFields);
end;

end.

