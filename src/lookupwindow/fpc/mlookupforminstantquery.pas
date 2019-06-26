// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mlookupformInstantQuery;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, DB,
  mLookupPanelInstantQuery, mDataProviderInterfaces;

resourcestring
  SLookupFormMissingValueCaption = 'Warning';
  SLookupFormMissingValueWarning = 'No value selected.';


type

  { TmLookupInstantQueryFrm }

  TmLookupInstantQueryFrm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FLookupPanel : TmLookupPanelInstantQuery;
    FSelectedValue : Variant;
    FSelectedDisplayLabel : String;
    procedure OnSelectValue (const aKeyValue: variant; const aDisplayLabel: string);
  public
    { public declarations }
    procedure Init (const aInstantQueryManager : IVDInstantQueryManager; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList); overload;
    procedure Init (const aInstantQueryManager : IVDInstantQueryManager); overload;

    property SelectedValue: variant read FSelectedValue;
    property SelectedDisplayLabel: string read FSelectedDisplayLabel;
  end;


implementation

uses
  variants,
  mFormSetup;

{$R *.lfm}

{ TmLookupInstantQueryFrm }

procedure TmLookupInstantQueryFrm.FormShow(Sender: TObject);
begin
  FLookupPanel.SetFocusOnFilter;
end;

procedure TmLookupInstantQueryFrm.FormCreate(Sender: TObject);
begin
  FLookupPanel := TmLookupPanelInstantQuery.Create(Self);
  FLookupPanel.Parent := Self;
  FLookupPanel.Align:= alClient;
  FLookupPanel.OnSelectAValue:= @OnSelectValue;
  FSelectedValue:= Null;
  FSelectedDisplayLabel:= '';
  SetupFormAndCenter(Self, 0.8);
end;

procedure TmLookupInstantQueryFrm.OkBtnClick(Sender: TObject);
begin
  FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel);
  if not VarIsNull(FSelectedValue) then
    ModalResult := mrOk
  else
    MessageDlg(SLookupFormMissingValueCaption, SLookupFormMissingValueWarning, mtInformation, [mbOk], 0);
end;


procedure TmLookupInstantQueryFrm.OnSelectValue(const aKeyValue: variant; const aDisplayLabel: string);
begin
  FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel);
  ModalResult := mrOk;
end;


procedure TmLookupInstantQueryFrm.Init(const aInstantQueryManager : IVDInstantQueryManager; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList);
begin
  FLookupPanel.Init(aInstantQueryManager, aFieldNames, aKeyFieldName, aDisplayFieldNames);
end;

procedure TmLookupInstantQueryFrm.Init(const aInstantQueryManager : IVDInstantQueryManager);
begin
  FLookupPanel.Init(aInstantQueryManager);
end;

end.

