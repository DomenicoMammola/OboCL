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

  TTaskExperimentGanttBarDatum = class (TmGanttBarDatum)
  strict private
    FExperimentId: integer;
    FHeadTask : boolean;
  public
    property ExperimentId : integer read FExperimentId write FExperimentId;
    property HeadTask : boolean read FHeadTask write FHeadTask;
  end;

  { TTestExperimentsDataProvider }

  TTestExperimentsDataProvider = class (TmGanttDataProvider)
  private
    FExperiment1Bars : TObjectList;
    FExperiment2Bars : TObjectList;
    FExperiment3Bars : TObjectList;
    FColorExperiment1 : TColor;
    FColorExperiment2 : TColor;
    FColorExperiment3 : TColor;
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
    FDataProvider2 : TTestExperimentsDataProvider;
    function OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
    procedure OnMovingGanttBar (aBar: TmGanttBarDatum);
  public

  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}{$IFDEF DEBUG}uses LazLogger;{$ENDIF}{$ENDIF}

{ TTestExperimentsDataProvider }

constructor TTestExperimentsDataProvider.Create;
var
  tmp : TTaskExperimentGanttBarDatum;
  dt : TDateTime;
begin
  inherited Create;
  FExperiment1Bars := TObjectList.Create(true);
  FExperiment2Bars := TObjectList.Create(true);
  FExperiment3Bars := TObjectList.Create(true);

  FColorExperiment1:= clMoneyGreen;
  FColorExperiment2:= clPurple;
  FColorExperiment3:= clAqua;

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= EncodeDate(2018, 1, 1);
  dt := tmp.StartTime + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.HeadTask:= true;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.HeadTask:= false;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.HeadTask:= false;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= EncodeDate(2018, 1, 6);
  dt := tmp.StartTime + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= true;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= false;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= false;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= EncodeDate(2018, 1, 10);
  dt := tmp.StartTime + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= true;
  FExperiment3Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= false;
  FExperiment3Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= false;
  FExperiment3Bars.Add(tmp);

end;

destructor TTestExperimentsDataProvider.Destroy;
begin
  FExperiment1Bars.Free;
  FExperiment2Bars.Free;
  FExperiment3Bars.Free;

  inherited Destroy;
end;

function TTestExperimentsDataProvider.RowCount: integer;
begin
  Result := 3;
end;


procedure TTestExperimentsDataProvider.GetGanttBars(const aRowIndex: integer; const aStartDate, aEndDate: TDateTime; aGanttBars: TList);
var
  i, k : integer;
  curBar, b1, b2 : TmGanttBarDatum;
begin
  if aRowIndex = 0 then
  begin
    curBar := FExperiment1Bars.Items[0] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment1;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment2Bars.Items[0] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment2;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment3Bars.Items[0] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment3;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
  end
  else if aRowIndex = 1 then
  begin
    curBar := FExperiment1Bars.Items[1] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment1;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment2Bars.Items[1] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment2;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment3Bars.Items[1] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment3;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
  end
  else
  begin
    curBar := FExperiment1Bars.Items[2] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment1;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment2Bars.Items[2] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment2;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
    curBar := FExperiment3Bars.Items[2] as TmGanttBarDatum;
    curBar.Color:= FColorExperiment3;
    if Intersect(curBar.StartTime, curBar.EndTime, aStartDate, aEndDate) then
      aGanttBars.Add(curBar);
  end;

  i := 0;

  while i <= aGanttBars.Count -1 do
  begin
    b1 := TmGanttBarDatum(aGanttBars.Items[i]);
    for k := i + 1 to aGanttBars.Count -1 do
    begin
      b2 := TmGanttBarDatum(aGanttBars.Items[k]);
      if Intersect(b1.StartTime, b1.EndTime, b2.StartTime, b2.EndTime) then
      begin
        b1.Color:= clRed;
        b2.Color := clRed;
      end;
    end;
    inc(i);
  end;

end;



{$R *.lfm}

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
  for i := 0 to 15 do
  begin
    tmpList := TObjectList.Create(true);
    FBars.Add(tmpList);

    tmp := TmGanttBarDatum.Create;
    tmp.StartTime:= EncodeDate(2018, 1, i + 1);
    dt := tmp.StartTime + Random(100) / 10;
    tmp.EndTime:= dt;
    tmp.Color:= clYellow;
    tmpList.Add(tmp);

    tmp := TmGanttBarDatum.Create;
    tmp.StartTime:= dt + Random(400) / 100;
    tmp.EndTime:= tmp.StartTime + Random(100) / 10;
    tmp.Color:= clRed;
    tmpList.Add(tmp);
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmp : TmTimeline;
begin
  FDataProvider := TTestDataProvider.Create;
  FDataProvider2 := TTestExperimentsDataProvider.Create;

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
  FGanttChart.TimeRuler.MinDate:= EncodeDate(2018,1,1);
  FGanttChart.TimeRuler.MaxDate:= EncodeDate(2018,6,30);
  FGanttChart.TimeRuler.CurrentDate:= FGanttChart.TimeRuler.MinDate;
  FGanttChart.DataProvider := FDataProvider2;
  FGanttChart.Head.CellsColor:= clMoneyGreen;

  FGanttChart.Gantt.AllowMovingBar := @Self.OnAllowMovingGanttBar;
  FGanttChart.Gantt.OnMovingBar:= @Self.OnMovingGanttBar;

  FGanttChart.Rebuild;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDataProvider.Free;
  FDataProvider2.Free;
end;

function TForm1.OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
begin
  Result := (aBar as TTaskExperimentGanttBarDatum).HeadTask;
end;

procedure TForm1.OnMovingGanttBar(aBar: TmGanttBarDatum);
var
  list : TObjectList;
  i : integer;
  prevBar, curBar : TTaskExperimentGanttBarDatum;
  dt : TDateTime;
begin
  if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 1 then
    list := FDataProvider2.FExperiment1Bars
  else if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 2 then
    list := FDataProvider2.FExperiment2Bars
  else
    list := FDataProvider2.FExperiment3Bars;

  prevBar := list.Items[0] as TTaskExperimentGanttBarDatum;
  curBar := list.Items[1] as TTaskExperimentGanttBarDatum;
  dt := curBar.StartTime;
  curBar.StartTime:= prevBar.EndTime;
  curBar.EndTime:= curBar.EndTime + (curBar.StartTime - dt);

  prevBar := curBar;
  curBar := list.Items[2] as TTaskExperimentGanttBarDatum;
  dt := curBar.StartTime;
  curBar.StartTime:= prevBar.EndTime;
  curBar.EndTime:= curBar.EndTime + (curBar.StartTime - dt);
end;

end.

