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
  StdCtrls, LCLIntf, ExtCtrls, contnrs,
  mProgressClasses, mBaseClassesAsObjects, mMaps, mMicroGames,
  Biru_FreshFruit, Biru;

type

  { TProgressForm }
  TProgressForm = class(TForm)
    Shape1: TShape;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FListBox : TListBox;
    FAnimation : TBiruFreshFruit;
    FGames: TExtraGPan;
    FProgresses : TmStringDictionary;
    FGarbage : TObjectList;
    FStartTime : TDateTime;
  private
    { private declarations }
    procedure Advance (const aMsg : string; const aIndex : integer);
    procedure RefreshProgress (aProgress : TmAbstractProgress);
    procedure RemoveProgress(aProgress : TmAbstractProgress);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress (aProgress : TmAbstractProgress);
  end;

implementation

uses
  dateutils;

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.FormShow(Sender: TObject);
begin
end;

procedure TProgressForm.FormHide(Sender: TObject);
begin
end;

procedure TProgressForm.Advance(const aMsg: string; const aIndex : integer);
begin
  if FListBox.Count >= aIndex then
  begin
    FListBox.Items[aIndex] := aMsg;

    if (not FGames.Visible) and (SecondsBetween(Now, FStartTime) > 120) then
    begin
      FGames.Visible:= true;
      FGames.State:= Scramble;
      FGames.Start:= true;
      FAnimation.StopAnimation;
      FAnimation.Visible:= false;
    end;
  end;
end;

procedure TProgressForm.AddProgress(aProgress: TmAbstractProgress);
var
  tmp : TIntegerObject;
begin
  aProgress.OnRefresh:= @Self.RefreshProgress;
  aProgress.OnRemove:= @Self.RemoveProgress;
  tmp := TIntegerObject.Create(FListBox.Count);
  FGarbage.Add(tmp);
  FListBox.Items.Add(aProgress.Caption);
  FProgresses.Add(aProgress.Id, tmp);
end;

procedure TProgressForm.RefreshProgress(aProgress: TmAbstractProgress);
var
  tmp : TIntegerObject;
begin
  if not Self.Visible then
  begin
    Self.Show;
    FAnimation.Visible:= true;
    FGames.Visible := false;
    FAnimation.PlayAnimation;
    FStartTime:= Now;
  end;
  tmp := FProgresses.Find(aProgress.Id) as TIntegerObject;
  if Assigned(tmp) then
    Self.Advance(aProgress.Caption, tmp.Value);
end;

procedure TProgressForm.RemoveProgress(aProgress: TmAbstractProgress);
begin
  FProgresses.Remove(aProgress.Id);
  if FProgresses.Count = 0 then
  begin
    if FAnimation.Visible then
      FAnimation.StopAnimation;
    if FGames.Visible then
      FGames.Start:= false;
    Self.Hide;
    FListBox.Clear;
    FGarbage.Clear;
  end;
end;

constructor TProgressForm.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  FAnimation := TBiruFreshFruit.Create(Self);
  FAnimation.Parent := Self;
  Self.Height:= FAnimation.Height;
  FAnimation.Align:= alLeft;
  i := Random(3);
  case i of
    0 : FAnimation.Shape:= bsApple;
    1 : FAnimation.Shape:= bsBanana;
    2 : FAnimation.Shape:= bsCherry;
    3 : FAnimation.Shape:= bsKiwi;
  end;
  FAnimation.Animation:= tatBouncing;

  FGames := TExtraGPan.Create(Self);
  FGames.Parent := Self;
  FGames.Align:= alLeft;
  FGames.Width:= FAnimation.Width;
  FGames.Visible := false;

  FListBox := TListBox.Create(Self);
  FListBox.Parent := Self;
  FListBox.Align := alClient;
  FProgresses := TmStringDictionary.Create();
  FGarbage := TObjectList.Create(true);

  Self.ShowInTaskBar:= stNever;
end;

destructor TProgressForm.Destroy;
begin
  FProgresses.Free;
  FGarbage.Free;
  inherited Destroy;
end;

end.

