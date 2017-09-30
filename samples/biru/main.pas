unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Biru, Biru_FreshFruit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FBiru : TBiruFreshFruit;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBiru := TBiruFreshFruit.Create(Self);
  FBiru.Parent := Self;
  FBiru.Top := 0;
  FBiru.Left := 0;
  FBiru.Shape := bsCherry;
  FBiru.Animation:= tatBouncing;
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

