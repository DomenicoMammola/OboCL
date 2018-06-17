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
  contnrs,
  mGanttEvents;

type

  { TmGanttDataProvider }

  TmGanttDataProvider = class abstract
  strict private
    FTopRow : integer;
    FEventsSubscriptions: TObjectList;
    procedure SetTopRow(AValue: integer);
    procedure Scrolled;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function RowCount : integer; virtual; abstract;
    function SubscribeToEvents(SubscriberClass: TmGanttDataProviderEventsSubscriptionClass) : TmGanttDataProviderEventsSubscription;
    procedure UnsubscribeFromEvents(Subscription: TmGanttDataProviderEventsSubscription);
  public
    property TopRow : integer read FTopRow write SetTopRow;
  end;

implementation
uses
  Math;

{ TmGanttDataProvider }

procedure TmGanttDataProvider.SetTopRow(AValue: integer);
begin
  if FTopRow = AValue then Exit;
  FTopRow:= max(0,AValue);
  Scrolled;
end;

procedure TmGanttDataProvider.Scrolled;
begin
  //
end;

constructor TmGanttDataProvider.Create;
begin
  FTopRow:= 0;
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
