unit UramakiFrameworkConnector;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  { TUramakiFrameworkConnector }

  TUramakiFrameworkConnector = class
  public
    procedure PleaseRefreshMyChilds (const aPlate : TObject);
  end;

implementation

{ TUramakiFrameworkConnector }

procedure TUramakiFrameworkConnector.PleaseRefreshMyChilds(const aPlate: TObject);
begin
  //
end;

end.
