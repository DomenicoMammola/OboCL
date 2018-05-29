// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mlookupform;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, DB,
  mLookupPanel, mDataProviderInterfaces;

resourcestring
  SLookupFormMissingValueCaption = 'Warning';
  SLookupFormMissingValueWarning = 'No value selected.';


type

  { TmLookupFrm }

  TmLookupFrm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FLookupPanel : TmLookupPanel;
    FSelectedValue : Variant;
    FSelectedDisplayLabel : String;
    procedure OnSelectValue (const aKeyValue: variant; const aDisplayLabel: string);
  public
    { public declarations }
    procedure Init (const aDataProvider : IVDDataProvider; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList);

    property SelectedValue: variant read FSelectedValue;
    property SelectedDisplayLabel: string read FSelectedDisplayLabel;
  end;


implementation

uses
  variants;

{$R *.lfm}

{ TmLookupFrm }

procedure TmLookupFrm.FormShow(Sender: TObject);
begin
  FLookupPanel.SetFocusOnFilter;
end;

procedure TmLookupFrm.FormCreate(Sender: TObject);
begin
  FLookupPanel := TmLookupPanel.Create(Self);
  FLookupPanel.Parent := Self;
  FLookupPanel.Align:= alClient;
  FLookupPanel.OnSelectAValue:= @OnSelectValue;
  FSelectedValue:= Null;
  FSelectedDisplayLabel:= '';
end;

procedure TmLookupFrm.OkBtnClick(Sender: TObject);
begin
  FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel);
  if not VarIsNull(FSelectedValue) then
    ModalResult := mrOk
  else
    MessageDlg(SLookupFormMissingValueCaption, SLookupFormMissingValueWarning, mtInformation, [mbOk], 0);
end;


procedure TmLookupFrm.OnSelectValue(const aKeyValue: variant; const aDisplayLabel: string);
begin
  FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel);
  ModalResult := mrOk;
end;


procedure TmLookupFrm.Init(const aDataProvider : IVDDataProvider; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList);
begin
  FLookupPanel.Init(aDataProvider, aFieldNames, aKeyFieldName, aDisplayFieldNames);
end;

end.

