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
  Classes, Contnrs, Graphics;

type

  { TmCalendarAppointment }

  TmCalendarAppointment = class
  strict private
    FUniqueId : String;
    FDescription : String;
    FColor : TColor;
    FDrawnRect : TRect;
    FIcon : TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const aSource : TmCalendarAppointment);

    property UniqueId : String read FUniqueId write FUniqueId;
    property Description : String read FDescription write FDescription;
    property Color : TColor read FColor write FColor;
    property DrawnRect : TRect read FDrawnRect write FDrawnRect;
    property Icon : TBitmap read FIcon write FIcon;
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
  FIcon := nil;
end;

destructor TmCalendarAppointment.Destroy;
begin
  inherited Destroy;
end;

procedure TmCalendarAppointment.Assign(const aSource: TmCalendarAppointment);
begin
  FUniqueId := aSource.UniqueId;
  FDescription := aSource.Description;
  FColor := aSource.Color;
end;

end.
