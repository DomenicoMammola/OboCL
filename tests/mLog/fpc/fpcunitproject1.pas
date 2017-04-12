program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, TestLogger, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

