// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mTimerulerTimelines;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses

  Classes, Graphics, dateutils,
  mTimerulerScales;

type
  TmTimeline = class(TCollectionItem)
  protected
    FAlignment: TAlignment;
    FScale: TmScale;
    FFlex: Integer;
    FFont: TFont;
    FColor: TColor;
    FOwnerDraw: Boolean;
    FParentFont: Boolean;
    FParentColor : Boolean;

    procedure SetAlignment(Value: TAlignment);
    procedure SetScale(Value: TmScale);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetFlex(Value: Integer);
    procedure SetParentFont(Value: Boolean);
    procedure SetParentColor(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  public
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property Flex: Integer read FFlex write SetFlex default 1;
    property OwnerDraw: Boolean read FOwnerDraw write FOwnerDraw default false;
    property ParentColor: Boolean read FParentColor write SetParentColor default true;
    property ParentFont: Boolean read FParentFont write SetParentFont default true;
    property Scale: TmScale read FScale write SetScale;
  end;

  TmTimelines = class(TCollection)
  protected
    function  GetTimeline(Index: Integer): TmTimeline;
    procedure SetTimeLine(Index: Integer; Value: TmTimeline);
  public
    constructor Create; virtual;
    function Add: TmTimeline;
    function GetFlexTotal : integer;

    property Items[Index: Integer]: TmTimeline read GetTimeline write SetTimeLine; default;
  end;


implementation

uses
  SysUtils;

constructor TmTimeline.Create(Collection: TCollection);
begin
  inherited;
  FOwnerDraw := False;
  FFont := TFont.Create;
  FColor := clBtnFace;
  FAlignment := taCenter;
  FParentColor := True;
  FParentFont := True;
  Flex := 1;
end;

destructor TmTimeline.Destroy;
begin
  FFont.Destroy;
  if Assigned(FScale) then
     FreeAndNil(FScale);
  inherited;
end;

procedure TmTimeline.Assign(Source: TPersistent);
begin
  if Source is TmTimeline then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      FAlignment := TmTimeline(Source).Alignment;
      FScale := TmTimeline(Source).Scale;
      Flex := TmTimeline(Source).Flex;
      FFont.Assign(TmTimeline(Source).Font);
      FColor := TmTimeline(Source).Color;
      FOwnerDraw := TmTimeline(Source).OwnerDraw;
      FParentFont := TmTimeline(Source).ParentFont;
      FParentColor := TmTimeline(Source).ParentColor;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TmTimeline.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TmTimeline.SetScale(Value: TmScale);
begin
  if Value <> FScale then
  begin
    FScale := Value;
    Changed(False);
  end;
end;

procedure TmTimeline.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FParentColor := False;
    FColor := Value;
    Changed(False);
  end;
end;

procedure TmTimeline.SetFont(Value: TFont);
begin
  FParentFont := False;
  FFont.Assign(Value);
  Changed(False);
end;

procedure TmTimeline.SetFlex(Value: Integer);
begin
  if Value <> FFlex then
  begin
    FFlex := Value;
    Changed(False);
  end;
end;

procedure TmTimeline.SetParentFont(Value: Boolean);
begin
  if Value <> FParentFont then
  begin
    FParentFont := Value;
    Changed(False);
  end;
end;

procedure TmTimeline.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    Changed(False);
  end;
end;

function TmTimelines.GetTimeline(Index: Integer): TmTimeline;
begin
  Result := TmTimeline(inherited Items[Index]);
end;

procedure TmTimelines.SetTimeLine(Index: Integer; Value: TmTimeline);
begin
  TmTimeline(inherited Items[Index]).Assign(Value);
end;

constructor TmTimelines.Create;
begin
  inherited Create(TmTimeline);

end;

function TmTimelines.Add: TmTimeline;
begin
  Result := TmTimeline(inherited Add);
end;

function TmTimelines.GetFlexTotal: integer;
var
  i, dummy : integer;
begin
  dummy := 0;
  for i := 0 to Self.Count -1 do
  begin
    inc (dummy, Self.GetTimeline(i).Flex);
  end;
  Result := dummy;
end;

end.
