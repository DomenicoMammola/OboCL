// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit PilePanel;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls;

type

  { TPilePanel }

  TPilePanel = class (TCustomPanel)
  strict private
    FList : TList;
    FActivePanelIndex : integer;
    procedure SetActivePanelIndex(AValue: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function AddPanel : TPanel;

    property ActivePanelIndex : integer read FActivePanelIndex write SetActivePanelIndex;
  end;



implementation

{ TPilePanel }

procedure TPilePanel.SetActivePanelIndex(AValue: integer);
var
  i : integer;
begin
  if FActivePanelIndex=AValue then
    exit;
  if (AValue < 0) or (AValue > FList.Count - 1) then
    exit;
  FActivePanelIndex := AValue;
  for i := 0 to FList.Count - 1 do
    TPanel(FList.Items[i]).Visible:= (i = FActivePanelIndex);
end;

constructor TPilePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BorderStyle:= bsNone;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;

  FList := TList.Create;
end;

destructor TPilePanel.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TPilePanel.AddPanel: TPanel;
begin
  Result := TPanel.Create(Self);
  Result.Parent := Self;
  Result.Align:= alClient;
  Result.BorderStyle:= bsNone;
  Result.BevelInner:= bvNone;
  Result.BevelOuter:= bvNone;
  FList.Add(Result);
  Self.SetActivePanelIndex(FList.Count -1);
end;

end.
