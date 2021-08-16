// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, Graphics, Classes,
  mGanttEvents;

type

  { TmGanttBarDatum }

  TmGanttBarDatum = class
  strict private
    FStartTime : TDateTime;
    FEndTime : TDateTime;
    FColor : TColor;
    FBorderColor : TColor;
    FBarLabel : String;
    FZOrder : integer;
    FBarRect : TRect;
  public
    constructor Create; virtual;

    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Color : TColor read FColor write FColor;
    property BorderColor : TColor read FBorderColor write FBorderColor;
    property ZOrder : Integer read FZOrder write FZOrder;
    property BarLabel : String read FBarLabel write FBarLabel;

    property BarRect : TRect read FBarRect write FBarRect;
  end;

  { TmGanttDataProvider }

  TmGanttDataProvider = class abstract
  strict private
    FEventsSubscriptions: TObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function RowCount : integer; virtual; abstract;
    procedure GetGanttBars (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aGanttBars : TList); virtual; abstract;
    function SubscribeToEvents(SubscriberClass: TmGanttDataProviderEventsSubscriptionClass) : TmGanttDataProviderEventsSubscription;
    procedure UnsubscribeFromEvents(Subscription: TmGanttDataProviderEventsSubscription);
  end;

implementation
uses
  Math,
  mGraphicsUtility;

{ TmGanttBarDatum }

constructor TmGanttBarDatum.Create;
begin
  FStartTime:= 0;
  FEndTime:= 0;
  FColor:= clBlue;
  FBorderColor:= DarkerColor(FColor, 20);
  FZOrder:= 0;
  FBarLabel := '';
end;

{ TmGanttDataProvider }

constructor TmGanttDataProvider.Create;
begin
  FEventsSubscriptions:= TObjectList.Create(true);
end;

destructor TmGanttDataProvider.Destroy;
begin
  FEventsSubscriptions.Destroy;
  inherited Destroy;
end;

function TmGanttDataProvider.SubscribeToEvents(SubscriberClass: TmGanttDataProviderEventsSubscriptionClass): TmGanttDataProviderEventsSubscription;
var
  newSubscription : TmGanttDataProviderEventsSubscription;
begin
  newSubscription := SubscriberClass.Create();
  FEventsSubscriptions.Add(newSubscription);
  Result := newSubscription;
end;

procedure TmGanttDataProvider.UnsubscribeFromEvents(Subscription: TmGanttDataProviderEventsSubscription);
var
  i : integer;
begin
  i := FEventsSubscriptions.IndexOf(Subscription);
  if (i >= 0) then
    FEventsSubscriptions.Delete(i);
end;

end.
