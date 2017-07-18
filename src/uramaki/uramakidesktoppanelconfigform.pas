unit UramakiDesktopPanelConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TUramakiPanelConfigurationForm }

  TUramakiPanelConfigurationForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorButton: TColorButton;
    EditCaption: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  public
    procedure Init (const aCaption : String; const aColor : TColor);
    procedure GetValues (var aCaption : String; var aColor : TColor);
  end;


implementation

{$R *.lfm}

{ TUramakiPanelConfigurationForm }

procedure TUramakiPanelConfigurationForm.Init(const aCaption: String; const aColor: TColor);
begin
  EditCaption.Text:= aCaption;
  ColorButton.ButtonColor:= aColor;
end;

procedure TUramakiPanelConfigurationForm.GetValues(var aCaption: String; var aColor: TColor);
begin
  aCaption := EditCaption.Text;
  aColor:= ColorButton.ButtonColor;
end;

end.

