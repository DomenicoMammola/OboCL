unit mIntegerDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, SpinEx;

type

  { TmIntegerForm }

  TmIntegerForm = class(TForm)
    CancelBtn: TBitBtn;
    Label1: TLabel;
    OKBtn: TBitBtn;
    SpinEdit: TSpinEditEx;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


  function QueryInteger (const aOwner : TComponent; const aTitle, aMessage : String; const aDefaultValue, aMinValue, aMaxValue : integer; out aValue : integer) : boolean;

implementation

uses
  mFormSetup;

function QueryInteger(const aOwner : TComponent; const aTitle, aMessage: String; const aDefaultValue, aMinValue, aMaxValue: integer; out aValue: integer): boolean;
var
  dlg : TmIntegerForm;
begin
  Result := false;

  dlg := TmIntegerForm.Create(aOwner);
  try
    dlg.Caption:= aTitle;
    dlg.Label1.Caption:= aMessage;
    dlg.SpinEdit.MinValue:= aMinValue;
    dlg.SpinEdit.MaxValue:= aMaxValue;
    dlg.SpinEdit.Value:= aDefaultValue;

    if dlg.ShowModal = mrOK then
    begin
      Result := true;
      aValue:= dlg.SpinEdit.Value;
    end;
  finally
    dlg.Free;
  end;
end;

{$R *.lfm}

{ TmIntegerForm }

procedure TmIntegerForm.FormCreate(Sender: TObject);
begin
  SetupFormAndCenter(Self);
end;

end.

