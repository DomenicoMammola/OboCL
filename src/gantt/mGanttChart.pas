// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttChart;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Controls, ExtCtrls, Classes,
  ATScrollBar,
  mGantt, mTimeruler, mTimerulerEvents;

type

  { TmGanttChart }

  TmGanttChart = class (TCustomPanel)
  strict private
    FTimeruler : TmTimeruler;
    FGantt : TmGantt;
    FLeftPanel : TCustomPanel;
    FRightPanel : TCustomPanel;
    FGridWidth : integer;
    FHorizontalScrollbar : TATScroll;
    FVerticalScrollbar : TATScroll;
    procedure OnChangeHorizonalScrollbar (Sender : TObject);
    procedure OnTimerulerDateChanged (Sender : TObject);
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rebuild;

    property TimeRuler : TmTimeruler read FTimeRuler;
    property GridWidth : integer read FGridWidth write FGridWidth;
  end;


implementation

uses
  Forms;

{ TmGanttChart }

procedure TmGanttChart.OnChangeHorizonalScrollbar(Sender: TObject);
begin
  FTimeruler.OnDateChanged:= nil;
  try
    FTimeRuler.CurrentDate:= FTimeRuler.MainTimeline.Scale.AddTicks(FTimeRuler.MinDate, (Sender as TATScroll).Position);
  finally
    FTimeruler.OnDateChanged:= Self.OnTimerulerDateChanged;
  end;
end;

procedure TmGanttChart.OnTimerulerDateChanged(Sender: TObject);
begin
  FHorizontalScrollbar.OnChange:= nil;
  try
    FHorizontalScrollbar.Position := FTimeRuler.MainTimeline.Scale.TicksBetween(FTimeruler.MinDate, FTimeruler.CurrentDate);
  finally
    FHorizontalScrollbar.OnChange:= OnChangeHorizonalScrollbar;
  end;
end;

constructor TmGanttChart.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGridWidth := 150;

  Self.BorderStyle:= bsNone;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;

  FHorizontalScrollbar := TATScroll.Create(Self);
  FHorizontalScrollbar.Parent := Self;
  FHorizontalScrollbar.Align:= alBottom;
  FHorizontalScrollbar.Kind:= sbHorizontal;
  FHorizontalScrollbar.OnChange:= Self.OnChangeHorizonalScrollbar;

  FVerticalScrollbar := TATScroll.Create(Self);
  FVerticalScrollbar.Kind := sbVertical;
  FVerticalScrollbar.Parent := Self;
  FVerticalScrollbar.Align:= alRight;
  FVerticalScrollbar.Width:= FHorizontalScrollbar.Height;

  FLeftPanel := TCustomPanel.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align:= alLeft;
  FLeftPanel.BorderStyle:= bsNone;
  FLeftPanel.BevelInner:= bvNone;
  FLeftPanel.BevelOuter:= bvNone;
  FLeftPanel.Width:= FGridWidth;

  FRightPanel := TPanel.Create(Self);
  FRightPanel.Parent := Self;
  FRightPanel.BorderStyle:= bsNone;
  FRightPanel.BevelInner:= bvNone;
  FRightPanel.BevelOuter:= bvNone;
  FRightPanel.Align:= alClient;

  FTimeruler := TmTimeruler.Create(FRightPanel);
  FTimeruler.Parent := FRightPanel;
  FTimeruler.Align:= alTop;
  FTimeruler.OnDateChanged:= Self.OnTimerulerDateChanged;

  FGantt := TmGantt.Create(FRightPanel);
  FGantt.Parent := FRightPanel;
  FGantt.TimeRuler := FTimeruler;
  FGantt.Align := alClient;
end;

destructor TmGanttChart.Destroy;
begin
  inherited Destroy;
end;

procedure TmGanttChart.Rebuild;
begin
  FTimeruler.OnDateChanged:= nil;
  FHorizontalScrollbar.OnChange:= nil;
  try
    FTimeRuler.Rebuild;
    FHorizontalScrollbar.Min:= 1;
    FHorizontalScrollbar.Max:= FTimeRuler.MainTimeline.Scale.TicksBetween(FTimeRuler.MinDate, FTimeRuler.MaxDate);
    FHorizontalScrollbar.Position:= FTimeruler.MainTimeline.Scale.TicksBetween(FTimeruler.MinDate, FTimeruler.CurrentDate);
  finally
    FTimeruler.OnDateChanged:= Self.OnTimerulerDateChanged;;
    FHorizontalScrollbar.OnChange:= Self.OnChangeHorizonalScrollbar;
  end;
end;

end.
