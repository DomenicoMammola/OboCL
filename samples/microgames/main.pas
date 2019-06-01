unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mMicroGames;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FGamesPanel : TExtraGPan;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGamesPanel := TExtraGPan.Create(Self);
  FGamesPanel.Parent := Self;
  FGamesPanel.Align := alClient;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FGamesPanel.State:= Scramble;
  FGamesPanel.TRows:= 20;
  FGamesPanel.Start:= true;
end;

end.

