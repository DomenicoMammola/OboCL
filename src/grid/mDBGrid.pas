unit mDBGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DBGrids;

type
  TmDBGrid = class(TDBGrid)
  public
    property OnSelection;
  end;

implementation

end.
