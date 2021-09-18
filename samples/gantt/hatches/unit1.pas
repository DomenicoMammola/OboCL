unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs, Menus,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider, mDateTimeUtility,
  mTimeruler;

type

  { TTestDataProvider }

  TTestDataProvider = class (TmGanttDataProvider)
  private
    FBars : TObjectList;
    FHatches : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function RowCount : integer; override;
    procedure GetGanttBars (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aGanttBars : TmGanttBarDataList); override;
    procedure GetHatches (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aHatches : TmGanttHatchDataList); override;
  end;

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
    procedure OnDrawBucket(Sender: TmTimeruler; ACanvas: TCanvas; Timeline: TmTimeline; ARect: TRect; ADate: TDateTime);
    procedure OnClickOnBar (aBar: TmGanttBarDatum);
    procedure OnDblClickOnBar (aBar: TmGanttBarDatum);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  dateutils,
  mGraphicsUtility, mGanttGraphics;

{$R *.lfm}

{$IFDEF FPC}{$IFDEF DEBUG}uses LazLogger;{$ENDIF}{$ENDIF}

{ TTestDataProvider }

constructor TTestDataProvider.Create;
var
  i : integer;
  tmp : TmGanttBarDatum;
  tmpList : TObjectList;
  dt : TDateTime;
begin
  inherited;
  FBars := TObjectList.Create(true);
  for i := 0 to 8 do
  begin
    tmpList := TObjectList.Create(true);
    FBars.Add(tmpList);

    tmp := TmGanttBarDatum.Create;
    tmp.StartTime:= EncodeDate(2021, 8, 1) + (Random(300) / 10);
    dt := tmp.StartTime + (Random(200) / 10);
    tmp.EndTime:= dt;
    tmp.Color:= GenerateRandomColor;
    tmpList.Add(tmp);
  end;
  FHatches := TObjectList.Create(true);
end;

destructor TTestDataProvider.Destroy;
begin
  FBars.Free;
  FHatches.Free;
  inherited Destroy;
end;

function TTestDataProvider.RowCount: integer;
begin
  Result := FBars.Count;
end;

procedure TTestDataProvider.GetGanttBars(const aRowIndex: integer; const aStartDate, aEndDate: TDateTime; aGanttBars: TmGanttBarDataList);
var
  curBar : TmGanttBarDatum;
  curList : TObjectList;
  i : integer;
begin
  aGanttBars.Clear;
  if aRowIndex >= FBars.Count  then
    exit;
  curList := FBars.Items[aRowIndex] as TObjectList;
  for i := 0 to curList.Count -1 do
  begin
    curBar := curList.Items[i] as TmGanttBarDatum;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
    begin
      aGanttBars.Add(curBar);
      {$IFDEF DEBUG}
      DebugLn('Intersect OK - curBar.StartTime:' + DateTimeToStr(curBar.StartTime) + ' curBar.EndTime:' + DateTimeToStr(curBar.EndTime) +
        ' aStartDate:' + DateTimeToStr(aStartDate) + ' aEndDate:' + DateTimeToStr(aEndDate));
      {$ENDIF}
    end;
  end;
end;

procedure TTestDataProvider.GetHatches(const aRowIndex: integer; const aStartDate, aEndDate: TDateTime; aHatches: TmGanttHatchDataList);
var
  dow : word;
  sd, ed, i : integer;
  monday : integer;
  htc : TmGanttHatchDatum;
begin
  FHatches.Clear;
  dow := DayOfTheWeek(aStartDate);
  sd := trunc(aStartDate);
  ed := trunc(aEndDate);

  if (dow = 1) then
    monday := sd
  else
    monday := sd + (8 - dow);

  while monday <= ed do
  begin
    htc := TmGanttHatchDatum.Create;
    FHatches.Add(htc);
    aHatches.Add(htc);
    htc.StartTime:= monday;
    htc.EndTime:= EndOfTheDay(monday);
    htc.Color:= clRed;
    monday := monday + 7;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmp : TmTimeline;
  barsMenuItem : TMenuItem;
  d: TDateTime;
begin
  FDataProvider := TTestDataProvider.Create;

  FGanttChart := TmGanttChart.Create(Self);
  FGanttChart.Align:= alClient;
  FGanttChart.Parent := Self;

  FGanttChart.TimeRuler.Height:= 100;
  FGanttChart.TimeRuler.OnDrawBucket:=@OnDrawBucket;
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

procedure TForm1.OnDrawBucket(Sender: TmTimeruler; ACanvas: TCanvas; Timeline: TmTimeline; ARect: TRect; ADate: TDateTime);
var
  d : integer;
begin
  d := DayOfTheWeek(aDate);
  if (d = 7) or (d = 6) then
    ACanvas.Brush.Color := clRed;
  DrawBucketBox(ACanvas, ARect, ExtFormatDateTime(Timeline.Scale.DisplayFormat, ADate), Timeline.Alignment);
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

