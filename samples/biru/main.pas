unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Biru;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PanelBottom: TPanel;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBiru : TBiru;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  tmpBitmap, tmpBitmapMask : TBitmap;
begin
  FBiru := TBiru.Create(Self);
  FBiru.Parent := Self;
  FBiru.Top := 0;
  FBiru.Left := 0;

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromFile('apple.bmp');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromFile('apple_mask.bmp');
  FBiru.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  tmpBitmap := TBitmap.Create;
  tmpBitmap.LoadFromFile('strawberry.bmp');
  tmpBitmapMask := TBitmap.Create;
  tmpBitmapMask.LoadFromFile('strawberry_mask.bmp');
  FBiru.AddBiruImageAndMask(tmpBitmap, tmpBitmapMask);

  FBiru.FixedBackground.LoadFromFile('longbackground.bmp');
  FBiru.ScrollingBackground.LoadFromFile('longbackground.bmp'); //'scrollingbackground_vegetables.bmp');

  FBiru.ImageIndex := 0;
  FBiru.Animation:= tatBouncing;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FBiru.StopAnimation;
  case RadioGroup1.ItemIndex of
    0: FBiru.Animation:= tatBouncing;
    1: FBiru.Animation := tatScrolling;
    2: FBiru.Animation:= tatSizing;
  end;
  FBiru.PlayAnimation;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FBiru.StopAnimation;
end;

end.

