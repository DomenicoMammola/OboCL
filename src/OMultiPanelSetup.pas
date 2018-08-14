// This is part of the Obo Component Library
//
// -----------------------------------------------------------------------------
//
// ATTRIBUTION: Melander Blog http://melander.dk/articles/splitter
//
// -----------------------------------------------------------------------------
// TSplitter enhanced with grab bar
// The original author is Anders Melander, anders@melander.dk, http://melander.dk
// Copyright © 2008 Anders Melander
// -----------------------------------------------------------------------------
// License:
// Creative Commons Attribution-Share Alike 3.0 Unported
// http://creativecommons.org/licenses/by-sa/3.0/
unit OMultiPanelSetup;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface


implementation

uses
  Graphics, Classes, OMultiPanel;

// -----------------------------------------------------------------------------
//
// ATTRIBUTION: Melander Blog http://melander.dk/articles/splitter
//
// -----------------------------------------------------------------------------
// TSplitter enhanced with grab bar
// The original author is Anders Melander, anders@melander.dk, http://melander.dk
// Copyright © 2008 Anders Melander
// -----------------------------------------------------------------------------
// License:
// Creative Commons Attribution-Share Alike 3.0 Unported
// http://creativecommons.org/licenses/by-sa/3.0/
// -----------------------------------------------------------------------------
procedure MultiPanelPaintSizingBar(Sender: TOCustomMultiPanel;aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean);
var
  R: TRect;
  X, Y: integer;
  DX, DY: integer;
  i: integer;
  Brush: TBitmap;
  xColor: TColor;
const
  cPointCount = 6;
begin
  R := aBarRect;
  if aHover then
    xColor := Sender.SplitterHoverColor
   else
    xColor := Sender.SplitterColor;

  if xColor <> clNone then begin
    aCanvas.Brush.Color := xColor;
    aCanvas.FillRect(R);
  end;

  X := (R.Left+R.Right) div 2;
  Y := (R.Top+R.Bottom) div 2;
  if Sender.PanelType = ptHorizontal then begin
    DX := 0;
    DY := 3;
  end else begin
    DX := 3;
    DY := 0;
  end;
  Dec(X, DX*cPointCount div 2);
  Dec(Y, DY*cPointCount div 2);

  Brush := Graphics.TBitmap.Create;
  try
    Brush.Width := 2;
    Brush.Height := 2;
    Brush.Canvas.Brush.Color := clBtnHighlight;
    Brush.Canvas.FillRect(Rect(0,0,2,2));
    Brush.Canvas.Pixels[0, 0] := clBtnShadow;
    for I := 0 to cPointCount do
    begin
      aCanvas.Draw(X, Y, Brush);
      Inc(X, DX);
      Inc(Y, DY);
    end;
  finally
    Brush.Free;
  end;
end;

initialization
  OMP_SplitterHoverColor := $CCCCCC;
  OMP_SplitterSize := 5;
  OMP_OnPaintSizingBar := MultiPanelPaintSizingBar;

end.
