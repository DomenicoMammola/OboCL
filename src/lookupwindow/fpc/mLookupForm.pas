// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLookupForm;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, DB,
  mLookupPanel;

resourcestring
  SMissingValueCaption = 'Warning';
  SMissingValueWarning = 'No value selected.';


type

  { TmLookupWindow }

  TmLookupWindow = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FLookupPanel : TmLookupPanel;
    function GetSelectedDisplayLabel: string;
    function GetSelectedValue: variant;
    procedure OnSelectValue (const aKeyValue: variant; const aDisplayLabel: string);
    procedure GetSelectedValues (out aKeyValue: variant; out aDisplayLabel : string);
  public
    { public declarations }
    procedure Init (aValues : TDataset; aFieldNames : TStringList; aKeyFieldName, aDisplayLabelFieldName : string);

    property SelectedValue: variant read GetSelectedValue;
    property SelectedDisplayLabel: string read GetSelectedDisplayLabel;
  end;


implementation

uses
  variants;

{$R *.lfm}

{ TmLookupWindow }

procedure TmLookupWindow.FormShow(Sender: TObject);
begin
  FLookupPanel.SetFocusOnFilter;
end;

procedure TmLookupWindow.FormCreate(Sender: TObject);
begin
  FLookupPanel := TmLookupPanel.Create(Self);
  FLookupPanel.Parent := Self;
  FLookupPanel.Align:= alClient;
  FLookupPanel.OnSelectAValue:= @OnSelectValue;
end;

procedure TmLookupWindow.OkBtnClick(Sender: TObject);
var
  tmpDisplayLabel: string;
  tmpValue: Variant;
begin
  FLookupPanel.GetSelectedValues(tmpValue, tmpDisplayLabel);
  if not VarIsNull(tmpValue) then
    ModalResult := mrOk
  else
    MessageDlg(SMissingValueCaption, SMissingValueWarning, mtInformation, [mbOk], 0);
end;

procedure TmLookupWindow.GetSelectedValues (out aKeyValue: variant; out aDisplayLabel : string);
begin
  FLookupPanel.GetSelectedValues(aKeyValue, aDisplayLabel);
end;

procedure TmLookupWindow.OnSelectValue(const aKeyValue: variant; const aDisplayLabel: string);
begin
  ModalResult := mrOk;
end;

function TmLookupWindow.GetSelectedValue: variant;
var
  tmpDisplayLabel : string;
  tmpValue : variant;
begin
  Self.GetSelectedValues(tmpValue, tmpDisplayLabel);
  Result := tmpValue;
end;

function TmLookupWindow.GetSelectedDisplayLabel: string;
var
  tmpDisplayLabel : string;
  tmpValue : variant;
begin
  Self.GetSelectedValues(tmpValue, tmpDisplayLabel);
  Result := tmpDisplayLabel;
end;

procedure TmLookupWindow.Init(aValues : TDataset; aFieldNames : TStringList; aKeyFieldName, aDisplayLabelFieldName : string);
begin
  FLookupPanel.Init(aValues, aFieldNames, aKeyFieldName, aDisplayLabelFieldName);
end;

end.

