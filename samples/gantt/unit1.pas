unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  mGanttChart, mTimerulerScales, mTimerulerTimelines, mGanttDataProvider;

type

  { TTestDataProvider }

  TTestDataProvider = class (TmGanttDataProvider)
  public
    function RowCount : integer; override;
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

function TTestDataProvider.RowCount: integer;
begin
  Result := 24;
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
  FGanttChart.TimeRuler.MinDate:= Now - 100;
  FGanttChart.TimeRuler.MaxDate:= Now + 100;
  FGanttChart.DataProvider := FDataProvider;
  FGanttChart.Head.CellsColor:= clMoneyGreen;
  FGanttChart.Rebuild;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDataProvider.Free;
end;

end.

