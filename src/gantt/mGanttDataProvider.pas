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
  contnrs, Graphics, Classes, Types,
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

  { TmGanttBarDataList }

  TmGanttBarDataList = class
  strict private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    procedure Add(const aBarDatum: TmGanttBarDatum);
    function Get(const aIndex : integer): TmGanttBarDatum;
    procedure Clear;
  end;

  { TmGanttHatchDatum }

  TmGanttHatchDatum = class
  strict private
    FStartTime : TDateTime;
    FEndTime : TDateTime;
    FColor : TColor;
    FHatchRect : TRect;
  public
    constructor Create; virtual;

    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Color : TColor read FColor write FColor;

    property HatchRect : TRect read FHatchRect write FHatchRect;
  end;

  { TmGanttHatchDataList }

  TmGanttHatchDataList = class
  strict private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    procedure Add(const aHatchDatum: TmGanttHatchDatum);
    function Get(const aIndex : integer): TmGanttHatchDatum;
    procedure Clear;
  end;


  { TmGanttDataProvider }

  TmGanttDataProvider = class abstract
  strict private
    FEventsSubscriptions: TObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function RowCount : integer; virtual; abstract;
    procedure GetGanttBars (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aGanttBars : TmGanttBarDataList); virtual; abstract;
    procedure GetHatches (const aRowIndex : integer; const aStartDate, aEndDate : TDateTime; aHatches : TmGanttHatchDataList); virtual;
    function GetHeadText (const aRowIndex : integer): String; virtual;
    function SubscribeToEvents(SubscriberClass: TmGanttDataProviderEventsSubscriptionClass) : TmGanttDataProviderEventsSubscription;
    procedure UnsubscribeFromEvents(Subscription: TmGanttDataProviderEventsSubscription);
  end;

implementation
uses
  Math,
  mGraphicsUtility;

{ TmGanttHatchDataList }

constructor TmGanttHatchDataList.Create;
begin
  FList := TList.Create;
end;

destructor TmGanttHatchDataList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmGanttHatchDataList.Count: integer;
begin
  Result := FList.Count;
end;

procedure TmGanttHatchDataList.Add(const aHatchDatum: TmGanttHatchDatum);
begin
  FList.Add(aHatchDatum);
end;

function TmGanttHatchDataList.Get(const aIndex: integer): TmGanttHatchDatum;
begin
  Result := TmGanttHatchDatum(FList.Items[aIndex]);
end;

procedure TmGanttHatchDataList.Clear;
begin
  FList.Free;
end;

{ TmGanttHatchDatum }

constructor TmGanttHatchDatum.Create;
begin
  FStartTime:= 0;
  FEndTime:= 0;
  FColor:= clNone;
end;

{ TmGanttBarDataList }

constructor TmGanttBarDataList.Create;
begin
  FList := TList.Create;
end;

destructor TmGanttBarDataList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmGanttBarDataList.Count: integer;
begin
  Result := FList.Count;
end;

procedure TmGanttBarDataList.Add(const aBarDatum: TmGanttBarDatum);
begin
  FList.Add(aBarDatum);
end;

function TmGanttBarDataList.Get(const aIndex: integer): TmGanttBarDatum;
begin
  Result := TmGanttBarDatum(FList.Items[aIndex]);
end;

procedure TmGanttBarDataList.Clear;
begin
  FList.Clear;
end;

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

procedure TmGanttDataProvider.GetHatches(const aRowIndex: integer; const aStartDate, aEndDate: TDateTime; aHatches: TmGanttHatchDataList);
begin
  // do nothing
end;

function TmGanttDataProvider.GetHeadText(const aRowIndex: integer): String;
begin
  Result := '';
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
