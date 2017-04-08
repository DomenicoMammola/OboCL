unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, mFilterPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    FFilterPanel : TmFilterPanel;
    { private declarations }
    procedure CreateFilterPanel;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateFilterPanel;
end;

procedure TForm1.CreateFilterPanel;
var
  tmp : TmEditFilterConditionPanel;
  i : integer;
begin
  FFilterPanel := TmFilterPanel.Create(Self);
  FFilterPanel.Parent := Self;
  FFilterPanel.Align := alTop;
  for i := 1 to 10 do
  begin
    tmp := TmEditFilterConditionPanel.Create(FFilterPanel);
    FFilterPanel.AddFilterCondition(tmp);
    tmp.SetFilterCaption('Field' + IntToStr(i));
    if odd(i) then
      tmp.Flex := 3;
  end;
end;

end.

