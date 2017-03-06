program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestmXML;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

