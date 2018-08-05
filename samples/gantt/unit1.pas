unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider, mDateTimeUtility;

type

  { TTestDataProvider }

  TTestDataProvider = class (TmGanttDataProvider)
  private
    FBars : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function RowCount : integer; override;
    procedure GetGanttBars (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aGanttBars : TList); override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGanttChart : TmGanttChart;
    FDataProvider : TTestDataProvider;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TTestDataProvider }

constructor TTestDataProvider.Create;
var
  i : integer;
  tmp : TmGanttBarDatum;
begin
  inherited;
  FBars := TObjectList.Create(true);
  for i := 0 to 23 do
  begin
    tmp := TmGanttBarDatum.Create;
    tmp.StartTime:= EncodeDate(2018, 1, i + 1);
    tmp.EndTime:= tmp.StartTime + Random(100) / 10;
    tmp.Color:= clYellow;
    FBars.Add(tmp);
  end;
end;

destructor TTestDataProvider.Destroy;
begin
  FBars.Free;
  inherited Destroy;
end;

function TTestDataProvider.RowCount: integer;
begin
  Result := FBars.Count;
end;

procedure TTestDataProvider.GetGanttBars(const aRowIndex: integer; const aStartDate, aEndDate: TDateTime; aGanttBars: TList);
var
  curBar : TmGanttBarDatum;
begin
  aGanttBars.Clear;
  curBar := FBars.Items[aRowIndex] as TmGanttBarDatum;
  if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
    aGanttBars.Add(curBar);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmp : TmTimeline;
begin
  FDataProvider := TTestDataProvider.Create;

  FGanttChart := TmGanttChart.Create(Self);
  FGanttChart.Align:= alClient;
  FGanttChart.Parent := Self;

  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleMonth);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>mmm';
  tmp.Color:= clSkyBlue;
  tmp.Flex:=4;
  tmp := FGanttChart.TimeRuler.AddTimeline(TmScaleDay);
  tmp.Scale.DisplayFormat:= '<UPPERCASE>dd';
  tmp.Color:= clSkyBlue;
  tmp.Flex:=4;
  FGanttChart.TimeRuler.CurrentDate:= Now;
  FGanttChart.TimeRuler.MinDate:= EncodeDate(2018,1,1);
  FGanttChart.TimeRuler.MaxDate:= EncodeDate(2018,12,31);
  FGanttChart.DataProvider := FDataProvider;
  FGanttChart.Head.CellsColor:= clMoneyGreen;
  FGanttChart.Rebuild;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDataProvider.Free;
end;

end.

