unit mProgressForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
  private
    { private declarations }
  public
    procedure Advance (const aMsg : string);
  end;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.Advance(const aMsg: string);
begin
  Label1.Caption:= aMsg;
  if ProgressBar1.Position = ProgressBar1.Max then
    ProgressBar1.Position:= 0
  else
    ProgressBar1.Position:= ProgressBar1.Position + 1;
end;

end.

