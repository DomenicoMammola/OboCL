// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mCalendarClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, Graphics;

type

  { TmCalendarAppointment }

  TmCalendarAppointment = class
  strict private
    FDescription : String;
    FColor : TColor;
  public
    constructor Create;
    destructor Destroy; override;

    property Description : String read FDescription write FDescription;
    property Color : TColor read FColor write FColor;
  end;

  { TmCalendarAppointments }

  TmCalendarAppointments = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Add : TmCalendarAppointment;
    function Get(const aIndex: integer): TmCalendarAppointment;
    procedure Clear;
  end;


implementation

{ TmCalendarAppointments }

constructor TmCalendarAppointments.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmCalendarAppointments.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmCalendarAppointments.Count: integer;
begin
  Result := FList.Count;
end;

function TmCalendarAppointments.Add: TmCalendarAppointment;
begin
  Result := TmCalendarAppointment.Create;
  FList.Add(Result);
end;

function TmCalendarAppointments.Get(const aIndex: integer): TmCalendarAppointment;
begin
  Result := FList.Items[aIndex] as TmCalendarAppointment;
end;

procedure TmCalendarAppointments.Clear;
begin
  FList.Clear;
end;

{ TmCalendarAppointment }

constructor TmCalendarAppointment.Create;
begin
  FDescription := '';
  FColor := clYellow;
end;

destructor TmCalendarAppointment.Destroy;
begin
  inherited Destroy;
end;

end.
