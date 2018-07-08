// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttGUIClasses;

interface

uses
  Classes,
  mTimerulerTimelines;

type

  { TmGanttHeadMouseMoveData }

  TmGanttHeadMouseMoveData = class
  public
    ClickOnHead : boolean;
    ClickOnCell : boolean;
    ClickOnCellDelimiter : boolean;

    Distance : integer;
    Origin : integer;
    OriginalRowHeight : integer;
    CalculatedIncrement : double;
    RowIndex : integer;
    constructor Create;
    procedure Clear;
  end;

  { TmTimerulerMouseMoveData }

  TmTimerulerMouseMoveData = class
  public
    Timeline: TmTimeline;

    ClickOnTimelines : boolean;
    ClickOnBucket : boolean;
    ClickOnBucketDelimiter : boolean;

    Distance : double;
    DistanceInTicks: integer;
    LastCalculatedOneBucketWidth : double;
    constructor Create;
    procedure Clear;
  end;



implementation

{ TmTimerulerMouseMoveData }

constructor TmTimerulerMouseMoveData.Create;
begin
  Self.Clear;
end;

procedure TmTimerulerMouseMoveData.Clear;
begin
  Timeline := nil;
  DistanceInTicks := 0;
  LastCalculatedOneBucketWidth := -1;
  ClickOnTimelines := false;
  ClickOnBucket := false;
  ClickOnBucketDelimiter := false;
end;

{ TmGanttHeadMouseMoveData }

constructor TmGanttHeadMouseMoveData.Create;
begin
  Self.Clear;
end;

procedure TmGanttHeadMouseMoveData.Clear;
begin
  Distance := 0;
  Origin := 0;
  OriginalRowHeight:= 0;
  CalculatedIncrement:= 0;
  RowIndex := 0;
  ClickOnHead := false;
  ClickOnCell := false;
  ClickOnCellDelimiter := false;
end;

end.
