// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
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
