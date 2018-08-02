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
  mGantt, mGanttHead,
  mTimeruler, mGanttDataProvider;

type

  { TmGanttChart }

  TmGanttChart = class (TCustomPanel)
  strict private
    FTimeruler : TmTimeruler;
    FGantt : TmGantt;
    FGanttHead : TmGanttHead;
    FRightPanel : TCustomPanel;
    FHorizontalScrollbar : TATScroll;
    FVerticalScrollbar : TATScroll;
    function GetDataProvider: TmGanttDataProvider;
    procedure OnChangeHorizonalScrollbar (Sender : TObject);
    procedure OnChangeVerticalScrollbar (Sender : TObject);
    procedure OnTimerulerDateChanged (Sender : TObject);
    procedure SetDataProvider(AValue: TmGanttDataProvider);
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rebuild;

    property TimeRuler : TmTimeruler read FTimeRuler;
    property Gantt : TmGantt read FGantt;
    property Head : TmGanttHead read FGanttHead;
    property DataProvider : TmGanttDataProvider read GetDataProvider write SetDataProvider;
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

procedure TmGanttChart.OnChangeVerticalScrollbar(Sender: TObject);
begin
  FGanttHead.TopRow:= (Sender as TATScroll).Position;
end;

function TmGanttChart.GetDataProvider: TmGanttDataProvider;
begin
  Result := FGanttHead.DataProvider;
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

procedure TmGanttChart.SetDataProvider(AValue: TmGanttDataProvider);
begin
  FGanttHead.DataProvider := AValue;
end;

constructor TmGanttChart.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

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
  FVerticalScrollbar.OnChange:= Self.OnChangeVerticalScrollbar;

  FGanttHead := TmGanttHead.Create(Self);
  FGanttHead.Parent := Self;
  FGanttHead.Align:= alLeft;
  FGanttHead.Width:= 150;

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

  FGanttHead.Timeruler := FTimeruler;

  FGantt := TmGantt.Create(FRightPanel);
  FGantt.Parent := FRightPanel;
  FGantt.TimeRuler := FTimeruler;
  FGantt.Head := FGanttHead;
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
  FVerticalScrollbar.OnChange:= nil;
  try
    FTimeRuler.Rebuild;
    FHorizontalScrollbar.Min:= 1;
    FHorizontalScrollbar.Max:= FTimeRuler.MainTimeline.Scale.TicksBetween(FTimeRuler.MinDate, FTimeRuler.MaxDate);
    FHorizontalScrollbar.Position:= FTimeruler.MainTimeline.Scale.TicksBetween(FTimeruler.MinDate, FTimeruler.CurrentDate);
    FVerticalScrollbar.Min := 0;
    FVerticalScrollbar.Max:= FGanttHead.DataProvider.RowCount - 1;
    FVerticalScrollbar.Position:= FGanttHead.TopRow;
  finally
    FTimeruler.OnDateChanged:= Self.OnTimerulerDateChanged;
    FVerticalScrollbar.OnChange:= Self.OnChangeVerticalScrollbar;
    FHorizontalScrollbar.OnChange:= Self.OnChangeHorizonalScrollbar;
  end;
end;

end.
