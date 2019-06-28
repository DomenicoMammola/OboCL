unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  {$ifdef fpc}{$ifdef debug} LazLogger,{$endif}{$endif}
  mCalendar;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FCalendar : TmCalendar;
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
  FCalendar := TmCalendar.Create(Self);
  FCalendar.Parent := Self;
  FCalendar.Align := alClient;
  FCalendar.HorizontalItems:= 2;
  FCalendar.VerticalItems:= 2;
end;

end.

