unit HatchingGantt;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs,
  mGanttDataProvider;

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
    function GetHeadText (const aRowIndex : integer): String; override;
  end;


implementation

uses
  sysutils, dateutils, Graphics,
  mGraphicsUtility, mGanttGraphics, mDateTimeUtility;

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
      {$ifdef fpc}
      {$IFDEF DEBUG}
      DebugLn('Intersect OK - curBar.StartTime:' + DateTimeToStr(curBar.StartTime) + ' curBar.EndTime:' + DateTimeToStr(curBar.EndTime) +
       ' aStartDate:' + DateTimeToStr(aStartDate) + ' aEndDate:' + DateTimeToStr(aEndDate));
     {$ENDIF}
     {$endif}
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

function TTestDataProvider.GetHeadText(const aRowIndex: integer): String;
begin
  Result:= 'Resource #' + IntToStr(aRowIndex);
end;


end.
