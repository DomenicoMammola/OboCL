// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mTimerulerDefs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mTimerulerTimelines;

const
  WEEKS_IN_A_YEAR : double = 52.143;
  WEEKS_IN_A_MONTH : double = 4.34525;
  DAYS_IN_A_MONTH : double = 30.41666666666667;


type

    _TmMouseMoveData = class
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

{ _TmMouseMoveData }

constructor _TmMouseMoveData.Create;
begin
  Self.Clear;
end;

procedure _TmMouseMoveData.Clear;
begin
  Timeline := nil;
  DistanceInTicks := 0;
  LastCalculatedOneBucketWidth := -1;
  ClickOnTimelines := false;
  ClickOnBucket := false;
  ClickOnBucketDelimiter := false;
end;


end.

