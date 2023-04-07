// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCalendarGUIClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mCalendarClasses;

type

  { TmCalendarMouseMoveData }

  TmCalendarMouseMoveData = class
  strict private
    FDay : integer;
    FMouseOnDays : boolean;
    FMouseOnAppointments : boolean;
    FAppointment : TmCalendarAppointment;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function Appointment : TmCalendarAppointment;

    property Day : integer read FDay write FDay;
    property MouseOnDays : boolean read FMouseOnDays write FMouseOnDays;
    property MouseOnAppointments : boolean read FMouseOnAppointments write FMouseOnAppointments;
  end;

implementation

uses
  sysutils;

{ TmCalendarMouseMoveData }

constructor TmCalendarMouseMoveData.Create;
begin
  Self.Clear;
  FAppointment := nil;
end;

destructor TmCalendarMouseMoveData.Destroy;
begin
  FreeAndNil(FAppointment);
  inherited Destroy;
end;

procedure TmCalendarMouseMoveData.Clear;
begin
  FMouseOnDays:= false;
  FMouseOnAppointments:= false;
  FDay := 0;
  FreeAndNil(FAppointment);
end;

function TmCalendarMouseMoveData.Appointment: TmCalendarAppointment;
begin
  if not Assigned(FAppointment) then
    FAppointment := TmCalendarAppointment.Create;
  Result := FAppointment;
end;

end.
