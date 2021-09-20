unit SimpleGantt;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, Classes,
  mGanttDataProvider;

type
  { TTestDataProvider }

  TTestDataProvider = class (TmGanttDataProvider)
  private
    FBars : TObjectList;
    FRows : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function RowCount : integer; override;
    procedure GetGanttBars (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aGanttBars : TmGanttBarDataList); override;
    function GetHeadText (const aRowIndex : integer): String; override;
  end;



implementation

uses
  sysutils,
  mGraphicsUtility, mDateTimeUtility;

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
    FRows := TStringList.Create;
    for i := 0 to 1500 do
    begin
      tmpList := TObjectList.Create(true);
      FBars.Add(tmpList);

      tmp := TmGanttBarDatum.Create;
      tmp.StartTime:= EncodeDate(2018, 1, 1) + (Random(300) / 10);
      dt := tmp.StartTime + (Random(200) / 10);
      tmp.EndTime:= dt;
      tmp.Color:= GenerateRandomColor;
      tmpList.Add(tmp);

      tmp := TmGanttBarDatum.Create;
      tmp.StartTime:= dt + (Random(200) / 100);
      tmp.EndTime:= tmp.StartTime + (Random(200) / 10);
      dt := tmp.EndTime;
      tmp.Color:= GenerateRandomColor;
      tmpList.Add(tmp);

      tmp := TmGanttBarDatum.Create;
      tmp.StartTime:= dt + (Random(200) / 100);
      tmp.EndTime:= tmp.StartTime + (Random(200) / 10);
      tmp.Color:= GenerateRandomColor;
      tmpList.Add(tmp);

      FRows.Add(IntToStr(i));
    end;
  end;

  destructor TTestDataProvider.Destroy;
  begin
    FBars.Free;
    FRows.Free;
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
        {$IFDEF FPC}{$IFDEF DEBUG}
        DebugLn('Intersect OK - curBar.StartTime:' + DateTimeToStr(curBar.StartTime) + ' curBar.EndTime:' + DateTimeToStr(curBar.EndTime) +
          ' aStartDate:' + DateTimeToStr(aStartDate) + ' aEndDate:' + DateTimeToStr(aEndDate));
        {$ENDIF}{$ENDIF}
      end;
    end;
  end;

  function TTestDataProvider.GetHeadText(const aRowIndex: integer): String;
begin
  Result:= FRows.Strings[aRowIndex];
end;

end.
