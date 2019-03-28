// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDecorationEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls, ColorBox, DB,
  mCellDecorations;

resourcestring
  SCaptionDecorationEditForm = 'Edit decoration';

type
  { TDecorationEditForm }

  TDecorationEditForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    CBBackgroundColor: TCheckBox;
    CBTextColor: TCheckBox;
    CBBold: TCheckBox;
    CBItalic: TCheckBox;
    ColorBoxBackground: TColorBox;
    ColorBoxText: TColorBox;
    OkBtn: TBitBtn;
    procedure CBBackgroundColorClick(Sender: TObject);
    procedure CBTextColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  strict private
    FCellDecoration: TmCellDecoration;
  public
    procedure Init (const aDecoration: TmCellDecoration);
    procedure ApplyChanges;
  end;


implementation

uses
  mFormSetup, KAParser;

{$R *.lfm}

{ TDecorationEditForm }

procedure TDecorationEditForm.FormCreate(Sender: TObject);
begin
  Self.Caption := SCaptionDecorationEditForm;

  SetupFormAndCenter(Self);
end;

procedure TDecorationEditForm.CBBackgroundColorClick(Sender: TObject);
begin
  ColorBoxBackground.Enabled:= (Sender as TCheckBox).Checked;
end;

procedure TDecorationEditForm.CBTextColorClick(Sender: TObject);
begin
  ColorBoxText.Enabled:= (Sender as TCheckBox).Checked;
end;

procedure TDecorationEditForm.FormDestroy(Sender: TObject);
begin
end;

procedure TDecorationEditForm.FormShow(Sender: TObject);
begin
  //
end;

procedure TDecorationEditForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TDecorationEditForm.Init(const aDecoration: TmCellDecoration);
begin
  FCellDecoration := aDecoration;
  CBBackgroundColor.Checked:= FCellDecoration.BackgroundColor.NotNull;
  if FCellDecoration.BackgroundColor.NotNull then
  begin
    ColorBoxBackground.Selected:= FCellDecoration.BackgroundColor.Value;
    ColorBoxBackground.Enabled:= true;
  end
  else
    ColorBoxBackground.Selected:= clNone;

  CBTextColor.Checked:= FCellDecoration.TextColor.NotNull;
  if FCellDecoration.TextColor.NotNull then
  begin
    ColorBoxText.Selected:= FCellDecoration.TextColor.Value;
    ColorBoxText.Enabled:= true;
  end
  else
    ColorBoxText.Selected:= clNone;
  CBBold.Checked := FCellDecoration.TextBold.AsBoolean;
  CBItalic.Checked := FCellDecoration.TextItalic.AsBoolean;
end;

procedure TDecorationEditForm.ApplyChanges;
begin
  if CBBackgroundColor.Checked and (ColorBoxBackground.Selected <> clNone) then
  begin
    FCellDecoration.BackgroundColor.Value:= ColorBoxBackground.Selected;
  end
  else
    FCellDecoration.BackgroundColor.IsNull:= true;

  if CBTextColor.Checked and (ColorBoxText.Selected <> clNone) then
  begin
    FCellDecoration.TextColor.Value:= ColorBoxText.Selected;
  end
  else
    FCellDecoration.TextColor.IsNull:= true;

  if CBBold.Checked then
    FCellDecoration.TextBold.Value:= true
  else
    FCellDecoration.TextBold.IsNull:= true;

  if CBItalic.Checked then
    FCellDecoration.TextItalic.Value:= true
  else
    FCellDecoration.TextItalic.IsNull:= true;
end;


end.

