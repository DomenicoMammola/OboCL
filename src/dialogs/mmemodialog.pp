// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
unit mMemoDialog;

{$mode objfpc}
{$H+}

interface

uses
  Forms, Controls, StdCtrls, ExtCtrls, Buttons, Classes;

type

  { TmMemoForm }

  TmMemoForm = class(TForm)
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    MainMemo: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure LoadTextFile (const aFileName: string);

    class procedure Show (const aOwner: TComponent; const aFileName : string);
    class function ShowAndEdit (const aOwner : TComponent; const aSourceText: string) : String;
  end;


implementation

uses
  SysUtils, LazFileUtils,
  mFormSetup;

{$R *.lfm}

{ TmMemoForm }

procedure TmMemoForm.FormShow(Sender: TObject);
begin
  MainMemo.SetFocus;
end;

procedure TmMemoForm.FormCreate(Sender: TObject);
begin
  SetupFormAndCenter(Self);
end;

procedure TmMemoForm.LoadTextFile(const aFileName: string);
begin
  MainMemo.Clear;
  if FileExistsUTF8(aFileName) then
  begin
    MainMemo.Lines.LoadFromFile(aFileName);
    Self.Caption:= ExtractFileNameOnly(aFileName) + ExtractFileExt(aFileName);
  end;
end;

class procedure TmMemoForm.Show(const aOwner: TComponent; const aFileName: string);
var
  tmp : TmMemoForm;
begin
  if FileExistsUTF8(aFileName) then
  begin
    tmp := TmMemoForm.Create(aOwner);
    try
      tmp.LoadTextFile(aFileName);
      tmp.WindowState:= wsMaximized;
      tmp.MainMemo.ReadOnly:= true;
      tmp.OkBtn.Visible:= false;
      tmp.ShowModal;
    finally
      tmp.Free;
    end;
  end;
end;

class function TmMemoForm.ShowAndEdit(const aOwner: TComponent; const aSourceText: string): String;
var
  tmp : TmMemoForm;
begin
  tmp := TmMemoForm.Create(aOwner);
  try
    tmp.MainMemo.ReadOnly:= false;
    tmp.MainMemo.Text := aSourceText;
    if tmp.ShowModal = mrOk then
      Result := tmp.MainMemo.Text
    else
      Result := aSourceText;
  finally
    tmp.Free;
  end;
end;

end.

