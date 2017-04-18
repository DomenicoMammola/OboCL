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
  Classes, SysUtils, FileUtil, ListViewFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, ComCtrls, DB,
  mLookupPanel;

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
    function GetSelected: string;
    procedure OnSelectValue (aKeyValue : String);
  public
    { public declarations }
    procedure Init (aValues : TDataset; aFieldNames : TStringList; aKeyFieldName : string);
    property Selected : string read GetSelected;
  end;


implementation

//uses
//  LCLType;

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
begin
  if FLookupPanel.GetSelectedRowKey <> '' then
    ModalResult := mrOk
  else
    MessageDlg('Warning', 'No value selected.', mtInformation, [mbOk], 0);
end;

function TmLookupWindow.GetSelected: string;
begin
  Result := FLookupPanel.GetSelectedRowKey;
end;

procedure TmLookupWindow.OnSelectValue(aKeyValue: String);
begin
  ModalResult := mrOk;
end;

procedure TmLookupWindow.Init(aValues : TDataset; aFieldNames : TStringList; aKeyFieldName : string);
begin
  FLookupPanel.Init(aValues, aFieldNames, aKeyFieldName);
end;

end.

