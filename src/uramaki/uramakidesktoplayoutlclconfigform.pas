unit UramakiDesktopLayoutLCLConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, ComCtrls;

type

  { TDesktopLayoutConfigForm }

  TDesktopLayoutConfigForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    TopPanel: TPanel;
    TreeView1: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$R *.lfm}

end.

