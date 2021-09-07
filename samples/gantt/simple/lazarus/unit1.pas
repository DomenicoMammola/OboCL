unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs, Menus,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider,
  SimpleGantt;

type


  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGanttChart : TmGanttChart;
    FDataProvider : TTestDataProvider;
    FBarsPopupMenu : TPopupMenu;
    function OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
    function OnAllowResizingGanttBar(aBar: TmGanttBarDatum) : boolean;
    procedure OnClickOnBar (aBar: TmGanttBarDatum);
    procedure OnDblClickOnBar (aBar: TmGanttBarDatum);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{$IFDEF FPC}{$IFDEF DEBUG}uses LazLogger;{$ENDIF}{$ENDIF}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmp : TmTimeline;
  barsMenuItem : TMenuItem;
begin
  FDataProvider := TTestDataProvider.Create;

  FGanttChart := TmGanttChart.Create(Self);
  FGanttChart.Align:= alClient;
  FGanttChart.Parent := Self;

  FGanttChart.TimeRuler.Height:= 100;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleMonth);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>mmm';
  tmp.Color:= clSkyBlue;
  tmp.Flex:=4;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleWeek);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>Week <xx>';
  tmp.Color:= clMedGray;
  tmp.Flex:=4;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleDay);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>dd';
  tmp.Color:= clMoneyGreen;
  tmp.Flex:=4;
  FGanttChart.TimeRuler.MinDate:= EncodeDate(2018,1,1);
  FGanttChart.TimeRuler.MaxDate:= EncodeDate(2018,6,30);
  FGanttChart.TimeRuler.CurrentDate:= FGanttChart.TimeRuler.MinDate;
  FGanttChart.DataProvider := FDataProvider;
  FGanttChart.Head.CellsColor:= clMoneyGreen;

  FGanttChart.Gantt.AllowMovingBar:= @Self.OnAllowMovingGanttBar;
  FGanttChart.Gantt.AllowResizeBar:= @Self.OnAllowResizingGanttBar;
  FGanttChart.Gantt.OnClickOnBar:= @Self.OnClickOnBar;
  FGanttChart.Gantt.OnDblClickOnBar:= @Self.OnDblClickOnBar;

  FBarsPopupMenu := TPopupMenu.Create(FGanttChart.Gantt);
  FGanttChart.Gantt.BarsPopupMenu := FBarsPopupMenu;

  barsMenuItem := TMenuItem.Create(FBarsPopupMenu);
  FBarsPopupMenu.Items.Add(barsMenuItem);
  barsMenuItem.Caption:= 'do something';

  FGanttChart.Rebuild;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDataProvider.Free;
end;

function TForm1.OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
begin
  Result := false;
end;


function TForm1.OnAllowResizingGanttBar(aBar: TmGanttBarDatum) : boolean;
begin
  Result := false;
end;


procedure TForm1.OnClickOnBar(aBar: TmGanttBarDatum);
begin
  ShowMessage('Clicked on bar');
end;

procedure TForm1.OnDblClickOnBar(aBar: TmGanttBarDatum);
begin
  ShowMessage('Double clicked on bar');
end;

end.

