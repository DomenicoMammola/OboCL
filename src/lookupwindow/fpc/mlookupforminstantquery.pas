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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FLookupPanel : TmLookupPanelInstantQuery;
    FSelectedValue : Variant;
    FSelectedDisplayLabel : String;
    FSelectedDatum : IVDDatum;
    procedure OnSelectValueDatum (const aKeyValue: variant; const aDisplayLabel: string; var aDatum : IVDDatum);
  public
    { public declarations }
    procedure Init (const aInstantQueryManager : IVDInstantQueryManager; const aFieldNames : TStringList; const aKeyFieldName : string; const aDisplayFieldNames : TStringList); overload;
    procedure Init (const aInstantQueryManager : IVDInstantQueryManager); overload;

    property SelectedValue: variant read FSelectedValue;
    property SelectedDisplayLabel: string read FSelectedDisplayLabel;
    property SelectedDatum: IVDDatum read FSelectedDatum;
  end;


implementation

uses
  variants,
  mFormSetup, mMagnificationFactor;

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
  FLookupPanel.OnSelectAValue:= @OnSelectValueDatum;
  FSelectedValue:= Null;
  FSelectedDisplayLabel:= '';
  FSelectedDatum:= nil;
  BottomPanel.Height:= ScaleForMagnification(50, true);
  OkBtn.Width := ScaleForMagnification(75, true);
  OkBtn.Height := ScaleForMagnification(30, true);
  OkBtn.Left := 0;
  OkBtn.Top := ScaleForMagnification(8, true);
  OkBtn.Anchors:= [akTop, akRight];
  CancelBtn.Width := ScaleForMagnification(75, true);
  CancelBtn.Height := ScaleForMagnification(30, true);
  CancelBtn.Left := OkBtn.Left + ScaleForMagnification(10, true) + OkBtn.Width;
  CancelBtn.Top := OkBtn.Top;
  CancelBtn.Anchors:= [akTop, akRight];

  SetupFormAndCenter(Self, 0.8);

  CancelBtn.Left:= BottomPanel.Width - ScaleForMagnification(10, true) - CancelBtn.Width;
  OkBtn.Left:= CancelBtn.Left - OkBtn.Width - ScaleForMagnification(10, true);
end;

procedure TmLookupInstantQueryFrm.FormDestroy(Sender: TObject);
begin
  if Assigned(FSelectedDatum) then
    FSelectedDatum.AsObject.Free;
end;

procedure TmLookupInstantQueryFrm.OkBtnClick(Sender: TObject);
begin
  if Assigned(FSelectedDatum) then
    FSelectedDatum.AsObject.Free;

  FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel, FSelectedDatum);
  if not VarIsNull(FSelectedValue) then
    ModalResult := mrOk
  else
    MessageDlg(SLookupFormMissingValueCaption, SLookupFormMissingValueWarning, mtInformation, [mbOk], 0);
end;


procedure TmLookupInstantQueryFrm.OnSelectValueDatum (const aKeyValue: variant; const aDisplayLabel: string; var aDatum : IVDDatum);
begin
  //FLookupPanel.GetSelectedValues(FSelectedValue, FSelectedDisplayLabel, );
  FSelectedValue:= aKeyValue;
  FSelectedDisplayLabel:= aDisplayLabel;
  if Assigned(FSelectedDatum) then
    FSelectedDatum.AsObject.Free;
  FSelectedDatum := aDatum;
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

