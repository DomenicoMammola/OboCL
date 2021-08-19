unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs, Menus, StdCtrls,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider, mDateTimeUtility;

type


  { TTaskExperimentGanttBarDatum }

  TTaskExperimentGanttBarDatum = class (TmGanttBarDatum)
  strict private
    FId: integer;
    FExperimentId: integer;
    FHeadTask : boolean;
    FOriginalLength : double;
    FMinLength : double;
    FMaxLength : double;
  private
    procedure SetMinMaxLength; // it cannot be resized more -/+ 20%
  public
    property ExperimentId : integer read FExperimentId write FExperimentId;
    property HeadTask : boolean read FHeadTask write FHeadTask;
    property Id : integer read FId write FId;
    property OriginalLength : double read FOriginalLength write FOriginalLength;
    property MinLength : double read FMinLength write FMinLength;
    property MaxLength : double read FMaxLength write FMaxLength;
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
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGanttChart : TmGanttChart;
    FDataProvider : TTestExperimentsDataProvider;
    FBarsPopupMenu : TPopupMenu;
    function OnAllowMovingGanttBar(aBar: TmGanttBarDatum) : boolean;
    procedure OnMovingGanttBar (aBar: TmGanttBarDatum);
    procedure OnAllowResizingGanttBar(aBar: TmGanttBarDatum);
    procedure OnResizingGanttBar (aBar: TmGanttBarDatum);
    procedure OnClickOnBar (aBar: TmGanttBarDatum);
    procedure OnDblClickOnBar (aBar: TmGanttBarDatum);
    procedure OnPopupBarsMenu (aSender : TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{ TTaskExperimentGanttBarDatum }

procedure TTaskExperimentGanttBarDatum.SetMinMaxLength;
var
  le : double;
begin
  le := Self.EndTime - Self.StartTime;
  MinLength:= le * 0.8;
  MaxLength:= le * 1.2;
end;

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
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.Id := 1;
  tmp.HeadTask:= true;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.Id := 2;
  tmp.HeadTask:= false;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment1;
  tmp.ExperimentId:= 1;
  tmp.Id := 3;
  tmp.HeadTask:= false;
  FExperiment1Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= EncodeDate(2018, 1, 6);
  dt := tmp.StartTime + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= true;
  tmp.Id := 4;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= false;
  tmp.Id := 5;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment2;
  tmp.ExperimentId:= 2;
  tmp.HeadTask:= false;
  tmp.Id := 6;
  FExperiment2Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= EncodeDate(2018, 1, 10);
  dt := tmp.StartTime + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= true;
  tmp.Id := 7;
  FExperiment3Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= false;
  tmp.Id := 8;
  FExperiment3Bars.Add(tmp);

  tmp := TTaskExperimentGanttBarDatum.Create;
  tmp.StartTime:= dt;
  dt := dt + Random(100) / 10;
  tmp.EndTime:= dt;
  tmp.SetMinMaxLength;
  tmp.Color:= FColorExperiment3;
  tmp.ExperimentId:= 3;
  tmp.HeadTask:= false;
  tmp.Id := 9;
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


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmp : TmTimeline;
  barsMenuItem : TMenuItem;
begin
  FDataProvider := TTestExperimentsDataProvider.Create;

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
  FGanttChart.DataProvider := FDataProvider;
  FGanttChart.Head.CellsColor:= clMoneyGreen;

  FGanttChart.Gantt.AllowMovingBar := @Self.OnAllowMovingGanttBar;
  FGanttChart.Gantt.OnMovingBar:= @Self.OnMovingGanttBar;
  FGanttChart.Gantt.OnResizingBar:= @Self.OnResizingGanttBar;
  // FGanttChart.Gantt.OnClickOnBar:= @Self.OnClickOnBar;
  FGanttChart.Gantt.OnDblClickOnBar:= @Self.OnDblClickOnBar;

  FBarsPopupMenu := TPopupMenu.Create(FGanttChart.Gantt);
  FGanttChart.Gantt.BarsPopupMenu := FBarsPopupMenu;
  FBarsPopupMenu.OnPopup:= @Self.OnPopupBarsMenu;

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
  Result := (aBar as TTaskExperimentGanttBarDatum).HeadTask;
end;

procedure TForm1.OnMovingGanttBar(aBar: TmGanttBarDatum);
var
  list : TObjectList;
  prevBar, curBar : TTaskExperimentGanttBarDatum;
  dt : TDateTime;
begin
  if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 1 then
    list := FDataProvider.FExperiment1Bars
  else if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 2 then
    list := FDataProvider.FExperiment2Bars
  else
    list := FDataProvider.FExperiment3Bars;

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

procedure TForm1.OnAllowResizingGanttBar(aBar: TmGanttBarDatum);
begin
  //
end;

procedure TForm1.OnResizingGanttBar(aBar: TmGanttBarDatum);
var
  list : TObjectList;
  prevBar, curBar : TTaskExperimentGanttBarDatum;
  dt : TDateTime;
  i, k : integer;
  curLength : double;
begin
  curLength:= aBar.EndTime - aBar.StartTime;

  if curLength < (aBar as TTaskExperimentGanttBarDatum).MinLength then
    aBar.EndTime:= aBar.StartTime + (aBar as TTaskExperimentGanttBarDatum).MinLength
  else if curLength > (aBar as TTaskExperimentGanttBarDatum).MaxLength then
    aBar.EndTime:= aBar.StartTime + (aBar as TTaskExperimentGanttBarDatum).MaxLength;

  if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 1 then
    list := FDataProvider.FExperiment1Bars
  else if (aBar as TTaskExperimentGanttBarDatum).ExperimentId = 2 then
    list := FDataProvider.FExperiment2Bars
  else
    list := FDataProvider.FExperiment3Bars;

  for i := 0 to list.Count -1 do
  begin
    curBar := list.Items[i] as TTaskExperimentGanttBarDatum;
    if curBar.Id = (aBar as TTaskExperimentGanttBarDatum).Id then
    begin
      prevBar := aBar as TTaskExperimentGanttBarDatum;
      for k := i + 1 to list.Count -1 do
      begin
        curBar := list.Items[k] as TTaskExperimentGanttBarDatum;
        dt := curBar.StartTime;
        curBar.StartTime:= prevBar.EndTime;
        curBar.EndTime:= curBar.EndTime + (curBar.StartTime - dt);
        prevBar := curBar;
      end;
      break;
    end;
  end;
end;

procedure TForm1.OnClickOnBar(aBar: TmGanttBarDatum);
begin
  ShowMessage('Clicked on bar #' + IntToStr((aBar as TTaskExperimentGanttBarDatum).Id));
end;

procedure TForm1.OnDblClickOnBar(aBar: TmGanttBarDatum);
begin
  ShowMessage('Double clicked on bar #' + IntToStr((aBar as TTaskExperimentGanttBarDatum).Id));
end;

procedure TForm1.OnPopupBarsMenu(aSender: TObject);
begin
  if Assigned(FGanttChart.Gantt.SelectedBar) then
    FBarsPopupMenu.Items[0].Caption:= 'Do something with bar #' + IntToStr((FGanttChart.Gantt.SelectedBar as TTaskExperimentGanttBarDatum).Id)
  else
    FBarsPopupMenu.Items[0].Caption:= 'Do nothing';
end;

end.

