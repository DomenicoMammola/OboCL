unit OMultiPanel_reg;

interface

procedure Register;

implementation

uses Classes, OMultiPanel;

{$R .\OMultiPanel_Icon.dcr}

procedure Register;
begin
  RegisterComponents('Kluug.net', [TOMultiPanel]);
end;

end.
