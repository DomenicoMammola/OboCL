unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Menus,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider,
  mTimeruler,
  HatchingGantt;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FGanttChart : TmGanttChart;
    FDataProvider : TTestDataProvider;
    FBarsPopupMenu : TPopupMenu;
    function OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
    function OnAllowResizingGanttBar(aBar: TmGanttBarDatum) : boolean;
    procedure OnDrawBucket(Sender: TmTimeruler; ACanvas: TCanvas; Timeline: TmTimeline; ARect: TRect; ADate: TDateTime);
    procedure OnClickOnBar (aBar: TmGanttBarDatum);
    procedure OnDblClickOnBar (aBar: TmGanttBarDatum);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  dateutils,
  mGanttGraphics, mDateTimeUtility;

{$R *.dfm}

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
  FGanttChart.TimeRuler.OnDrawBucket:=OnDrawBucket;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleMonth);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>mmm';
  tmp.Color:= clTeal;
  tmp.Flex:=4;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleWeek);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>Week <xx>';
  tmp.Color:= clLtGray;
  tmp.Flex:=4;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleDay);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>dd';
  tmp.Color:= clWhite;
  tmp.Flex:=4;
  tmp.OwnerDraw:= true;
  FGanttChart.TimeRuler.MinDate:= EncodeDate(2021,8,1);
  FGanttChart.TimeRuler.MaxDate:= EncodeDate(2021,10,30);
  FGanttChart.TimeRuler.CurrentDate:= FGanttChart.TimeRuler.MinDate;
  FGanttChart.DataProvider := FDataProvider;
  FGanttChart.Head.CellsColor:= clMoneyGreen;

  FGanttChart.Gantt.AllowMovingBar:= OnAllowMovingGanttBar;
  FGanttChart.Gantt.AllowResizeBar:= OnAllowResizingGanttBar;
  FGanttChart.Gantt.OnClickOnBar:= OnClickOnBar;
  FGanttChart.Gantt.OnDblClickOnBar:= OnDblClickOnBar;

  FBarsPopupMenu := TPopupMenu.Create(FGanttChart.Gantt);
  FGanttChart.Gantt.BarsPopupMenu := FBarsPopupMenu;

  barsMenuItem := TMenuItem.Create(FBarsPopupMenu);
  FBarsPopupMenu.Items.Add(barsMenuItem);
  barsMenuItem.Caption:= 'do something';

  FGanttChart.Rebuild;
end;

function TForm1.OnAllowMovingGanttBar(aBar: TmGanttBarDatum): boolean;
begin
  Result := false;
end;

function TForm1.OnAllowResizingGanttBar(aBar: TmGanttBarDatum): boolean;
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

procedure TForm1.OnDrawBucket(Sender: TmTimeruler; ACanvas: TCanvas; Timeline: TmTimeline; ARect: TRect; ADate: TDateTime);
var
  d : integer;
begin
  d := DayOfTheWeek(aDate);
  if (d = 7) or (d = 6) then
    ACanvas.Brush.Color := clRed;
  DrawBucketBox(ACanvas, ARect, ExtFormatDateTime(Timeline.Scale.DisplayFormat, ADate), Timeline.Alignment);
end;

end.
