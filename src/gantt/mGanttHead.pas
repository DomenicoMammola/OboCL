// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGanttHead;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, Controls, Graphics,
  mGanttDataProvider;

type

  { TmGanttHead }

  TmGanttHead = class(TCustomControl)
  strict private
    FDoubleBufferedBitmap: Graphics.TBitmap;
    FRowHeight : integer;
    FDataProvider : TmGanttDataProvider;
    FTopRow : integer;
    procedure DoPaintTo(aCanvas: TCanvas; aRect: TRect);
    procedure SetTopRow(AValue: integer);
    procedure Scrolled;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property DataProvider : TmGanttDataProvider read FDataProvider write FDataProvider;
    property RowHeight : integer read FRowHeight write FRowHeight;
    property Color;
    property TopRow : integer read FTopRow write SetTopRow;
  end;

implementation

uses
  math, Forms,
  mTimerulerGraphics;

{ TmGanttHead }

procedure TmGanttHead.DoPaintTo(aCanvas: TCanvas; aRect: TRect);
begin
  aCanvas.Lock;
  try
    aCanvas.Pen.Mode := pmCopy;
    aCanvas.Brush.Color := Self.Color;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);
    if Assigned(FDataProvider) then
    begin

    end;
  finally
    aCanvas.Unlock;
  end;
end;

procedure TmGanttHead.SetTopRow(AValue: integer);
begin
  if FTopRow=AValue then Exit;
  FTopRow:= max(0,AValue);
  Scrolled;
end;

procedure TmGanttHead.Scrolled;
begin
  Self.Invalidate;
end;

procedure TmGanttHead.Paint;
var
  drawingRect: TRect;
  tmpCanvas: TCanvas;
begin
  inherited Paint;

  drawingRect := ClientRect;

  if DoubleBuffered then
  begin
    tmpCanvas := FDoubleBufferedBitmap.Canvas;
    tmpCanvas.Font.Assign(Self.Font);
  end
  else
    tmpCanvas := Self.Canvas;

  DoPaintTo(tmpCanvas, drawingRect);
end;

constructor TmGanttHead.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowHeight:= 18;
  FTopRow:= 0;
  FDoubleBufferedBitmap := Graphics.TBitmap.Create;
  {$ifdef fpc}
  DoubleBuffered:= IsDoubleBufferedNeeded;
  {$endif}

  FDoubleBufferedBitmap.SetSize(max(Screen.Width,3000), max(Screen.Height,2000));
  Self.Color:= clLtGray;
end;

destructor TmGanttHead.Destroy;
begin
  FDoubleBufferedBitmap.Free;
  inherited Destroy;
end;

end.
