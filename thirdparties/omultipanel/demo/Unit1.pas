unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OMultiPanel, StdCtrls, ComCtrls, Grids;

type
  TForm1 = class(TForm)
    OMultiPanel1: TOMultiPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    OMultiPanel2: TOMultiPanel;
    StringGrid1: TStringGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses ShellApi;

procedure MultiPanelPaintSizingBar(Sender: TOCustomMultiPanel;
  aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean);
//ATTRIBUTION: Melander Blog http://melander.dk/articles/splitter
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

  Brush := TBitmap.Create;
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

{ TForm1 }

initialization
  OMP_SplitterHoverColor := $CCCCCC;
  OMP_SplitterSize := 5;
  OMP_OnPaintSizingBar := MultiPanelPaintSizingBar;

end.
