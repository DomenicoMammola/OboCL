// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mProgressForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLIntf, windows,
  mProgressClasses;

type

  { TProgressForm }
  TProgressForm = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FProgress : TmAbstractProgress;
    procedure Advance (const aMsg : string);
    procedure RefreshProgress (aProgress : TObject);
    procedure RemoveProgress(aProgress : TObject);
  public
    procedure AddProgress (aProgress : TmAbstractProgress);
  end;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.FormShow(Sender: TObject);
begin
end;

procedure TProgressForm.FormHide(Sender: TObject);
begin
end;

procedure TProgressForm.Advance(const aMsg: string);
begin
  Label1.Caption:= aMsg;
  if ProgressBar1.Position = ProgressBar1.Max then
    ProgressBar1.Position:= 0
  else
    ProgressBar1.Position:= ProgressBar1.Position + 1;
end;

procedure TProgressForm.AddProgress(aProgress: TmAbstractProgress);
begin
  FProgress := aProgress;
  FProgress.OnRefresh:= @Self.RefreshProgress;
  FProgress.OnRemove:= @Self.RemoveProgress;
end;

procedure TProgressForm.RefreshProgress(aProgress: TObject);
begin
  if not Self.Visible then
    Self.Show;
  Self.Advance((aProgress as TmAbstractProgress).Caption);
end;

procedure TProgressForm.RemoveProgress(aProgress: TObject);
begin
  Self.Hide;
end;

end.

