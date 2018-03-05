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
  StdCtrls, LCLIntf, windows, contnrs,
  mProgressClasses, mBaseClassesAsObjects, mMaps,
  Biru_FreshFruit, Biru;

type

  { TProgressForm }
  TProgressForm = class(TForm)
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FListBox : TListBox;
    FAnimation : TBiruFreshFruit;
    FProgresses : TmStringDictionary;
    FGarbage : TObjectList;
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
    FListBox.Items[aIndex] := aMsg;
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
  i : integer;
  tmp : TIntegerObject;
begin
  if not Self.Visible then
  begin
    Self.Show;
    FAnimation.PlayAnimation;
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
    FAnimation.StopAnimation;
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
  FListBox := TListBox.Create(Self);
  FListBox.Parent := Self;
  FListBox.Align := alClient;
  FProgresses := TmStringDictionary.Create();
  FGarbage := TObjectList.Create(true);
end;

destructor TProgressForm.Destroy;
begin
  FProgresses.Free;
  FGarbage.Free;
  inherited Destroy;
end;

end.

