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
    CloseBtn: TBitBtn;
    MainMemo: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure LoadTextFile (const aFileName: string);

    class procedure Show (const aOwner: TComponent; const aFileName : string);
  end;


implementation

uses
  SysUtils, LazFileUtils;

{$R *.lfm}

{ TmMemoForm }

procedure TmMemoForm.FormShow(Sender: TObject);
begin
  MainMemo.SetFocus;
end;

procedure TmMemoForm.FormCreate(Sender: TObject);
begin
  if Screen.MonitorCount > 1 then
  begin
    Left := Screen.Monitors[0].Left;
    Top := Screen.Monitors[0].Top;
  end;
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
      tmp.ShowModal;
    finally
      tmp.Free;
    end;
  end;
end;

end.

