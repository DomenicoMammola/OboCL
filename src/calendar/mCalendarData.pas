unit mCalendarData;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

type
  TOnGetCaptionCalendar = function (aDate : TDateTime) : string of object;

  { TmCalendarData }

  TmCalendarData = class
  strict private
    FRefDate : TDateTime;
    FOnGetCaption : TOnGetCaptionCalendar;
  public
    constructor Create (aRefDate : TDateTime);
    property OnGetCaption : TOnGetCaptionCalendar read FOnGetCaption write FOnGetCaption;
  end;

  TmCalendarDataMatrix = class
  strict private
    FData : array of array of TmCalendarData;
  public
    constructor Create (aWidth;
    destructor Destroy; override;

  end;

implementation

{ TmCalendarData }

constructor TmCalendarData.Create(aRefDate: TDateTime);
begin
  FRefDate:= aRefDate;
end;

end.
