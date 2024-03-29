// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit progressform;

{$mode objfpc}{$H+}

{$define nogame}
// {$define noanimation}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLIntf, ExtCtrls, contnrs,
  mProgressClasses, mBaseClassesAsObjects, mMaps, mMicroGames,
  Biru, Biru_FreshFruit;

type

  { TProgressForm }
  TProgressForm = class(TForm)
    Shape1: TShape;
  strict private
    FListBox : TListBox;
    {$ifndef noanimation}
    FAnimation : TBiruFreshFruit;
    {$endif}
    {$ifndef nogame}
    FGames: TExtraGPan;
    {$endif}
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

procedure TProgressForm.Advance(const aMsg: string; const aIndex : integer);
begin
  if FListBox.Count >= aIndex then
  begin
    FListBox.Items[aIndex] := aMsg;
    FListBox.ItemIndex:= FListBox.Count - 1;
    {$ifndef nogame}
    if (not FGames.Visible) and (SecondsBetween(Now, FStartTime) > 120) then
    begin
      FGames.Visible:= true;
      FGames.State:= Scramble;
      FGames.Start:= true;
      FAnimation.StopAnimation;
      FAnimation.Visible:= false;
    end;
    {$endif}
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
    {$ifndef noanimation}
    FAnimation.Visible:= true;
    {$endif}
    {$ifndef nogame}
    FGames.Visible := false;
    {$endif}
    {$ifndef noanimation}
    FAnimation.PlayAnimation;
    {$endif}
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
    {$ifndef noanimation}
    if FAnimation.Visible then
      FAnimation.StopAnimation;
    {$endif}
    {$ifndef nogame}
    if FGames.Visible then
      FGames.Start:= false;
    {$endif}
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
  {$ifndef noanimation}
  FAnimation := TBiruFreshFruit.Create(Self);
  FAnimation.Parent := Self;
  Self.Height:= FAnimation.Height;
  FAnimation.Align:= alLeft;
  i := Random(4);
  case i of
    0 : FAnimation.Shape:= bsApple;
    1 : FAnimation.Shape:= bsBanana;
    2 : FAnimation.Shape:= bsCherry;
    3 : FAnimation.Shape:= bsKiwi;
  end;
  i := Random(3);
  case i of
    0 : FAnimation.Animation:= tatBouncing;
    1 : FAnimation.Animation:= tatScrolling;
    2 : FAnimation.Animation:= tatSizing;
  end;
  {$endif}

  {$ifndef nogame}
  FGames := TExtraGPan.Create(Self);
  FGames.Parent := Self;
  FGames.Align:= alLeft;
  FGames.Width:= FAnimation.Width;
  FGames.Visible := false;
  {$endif}

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

