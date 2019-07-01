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

type

  { TmCalendarMouseMoveData }

  TmCalendarMouseMoveData = class
  strict private
    FDay : integer;
    FClickOnDays : boolean;
  public
    constructor Create;
    procedure Clear;

    property Day : integer read FDay write FDay;
    property ClickOnDays : boolean read FClickOnDays write FClickOnDays;
  end;

implementation

{ TmCalendarMouseMoveData }

constructor TmCalendarMouseMoveData.Create;
begin
  Self.Clear;
end;

procedure TmCalendarMouseMoveData.Clear;
begin
  FClickOnDays:= false;
  FDay := 0;
end;

end.
