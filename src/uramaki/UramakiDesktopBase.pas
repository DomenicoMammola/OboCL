unit UramakiDesktopBase;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls,
  UramakiBase;

type

  { TUramakiDesktopPlate }

  TUramakiDesktopPlate = class (TUramakiPlate)
  protected
    FParentPanel : TPanel;
  public
    // here the plate must create every component it needs to visualize data
    procedure LinkToPanel (aParentPanel : TPanel); virtual;
  end;

implementation

{ TUramakiDesktopPlate }

procedure TUramakiDesktopPlate.LinkToPanel(aParentPanel: TPanel);
begin
  FParentPanel := aParentPanel;
end;

end.
