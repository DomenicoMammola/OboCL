unit mDBGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DBGrids, Grids;

type
  TmDBGrid = class(TDBGrid)
  public
    property OnSelection;
  end;

implementation

end.
