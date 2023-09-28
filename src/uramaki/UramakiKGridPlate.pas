unit UramakiKGridPlate;

interface
uses
  Classes, Controls, Grids,
  kgrids,
  mQuickReadOnlyVirtualDataSet, mKGridHelper,
  mGridHelper, mDataProviderInterfaces, UramakiBase,
  UramakiBaseGridPlate;

type

  { TUramakiKGridPlate }

  TUramakiKGridPlate = class(TUramakiBaseGridPlate)
  protected
    FGrid: TKGrid;
    FProvider : TReadOnlyVirtualDatasetProvider;
    FGridHelper : TmKGridHelper;
    FUramakiGridHelper : TUramakiGridHelper;
    FRunningDoUpdateChilds : boolean;
    procedure OnFilterDataset(Sender : TObject);
    procedure SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode); override;
    procedure UpdateChildsIfNeeded (const aUpdateThemAnyWay : boolean); override;
    procedure DoSelectAll (Sender : TObject); override;
    procedure DoAutoAdjustColumns(Sender : TObject); override;
    function GetUramakiGridHelper : IUramakiGridHelper; override;
    function GetGridHelper : TmAbstractGridHelper; override;
    procedure ConvertSelectionToUramakiRoll (aUramakiRoll : TUramakiRoll; aDoFillRollFromDatasetRow : TDoFillRollFromDatasetRow); override;
    procedure DoProcessRefreshChilds; override;
    procedure GetSelectedItems (const aKeyFieldName : string; aList : TList); override;
//    procedure OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
//    procedure OnCellClick(Column: TColumn);
    procedure SelectItems(const aDataProvider : IVDDataProvider; const aKeyValues : TStringList);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisableControls; override;
    procedure EnableControls; override;
    procedure RefreshDataset; override;
  end;


implementation

uses
  mKGridUtils, mWaitCursor;

{ TUramakiKGridPlate }

procedure TUramakiKGridPlate.OnFilterDataset(Sender: TObject);
begin

end;

procedure TUramakiKGridPlate.SetAutomaticChildsUpdateMode(AValue: TUramakiGridChildsAutomaticUpdateMode);
begin

end;

procedure TUramakiKGridPlate.UpdateChildsIfNeeded(const aUpdateThemAnyWay: boolean);
begin

end;

procedure TUramakiKGridPlate.DoSelectAll(Sender: TObject);
begin

end;

procedure TUramakiKGridPlate.DoAutoAdjustColumns(Sender: TObject);
begin
  TWaitCursor.ShowWaitCursor('TUramakiKGridPlate.DoAutoAdjustColumns');
  try
    mKGridUtils.AutoSizeColumns(FGrid);
  finally
    TWaitCursor.UndoWaitCursor('TUramakiKGridPlate.DoAutoAdjustColumns');
  end;
end;

function TUramakiKGridPlate.GetUramakiGridHelper: IUramakiGridHelper;
begin
  Result := FUramakiGridHelper;
end;

function TUramakiKGridPlate.GetGridHelper: TmAbstractGridHelper;
begin
  Result := FGridHelper;
end;

procedure TUramakiKGridPlate.ConvertSelectionToUramakiRoll(aUramakiRoll: TUramakiRoll; aDoFillRollFromDatasetRow: TDoFillRollFromDatasetRow);
begin

end;

procedure TUramakiKGridPlate.DoProcessRefreshChilds;
begin

end;

procedure TUramakiKGridPlate.GetSelectedItems(const aKeyFieldName: string; aList: TList);
begin

end;

//procedure TUramakiKGridPlate.OnSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
//begin
//
//end;
//
//procedure TUramakiKGridPlate.OnCellClick(Column: TColumn);
//begin
//
//end;

procedure TUramakiKGridPlate.SelectItems(const aDataProvider: IVDDataProvider; const aKeyValues: TStringList);
begin

end;

constructor TUramakiKGridPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGrid := TKGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align:= alClient;
  //FGrid.SummaryPanel := FSummaryPanel;

  FProvider := TReadOnlyVirtualDatasetProvider.Create;
  // FGrid.DataProvider:= FProvider;
  //Self.FDataset.OnFilter:= Self.OnFilterDataset;

  FGridHelper:= TmKGridHelper.Create(FGrid, FProvider.FormulaFields);
  FUramakiGridHelper := TUramakiGridHelper.Create(Self, FGridHelper);
  FGridHelper.SetupGrid;
  //FGrid.SortManager := Self.FDataset.SortManager;
  //FGrid.FilterManager := Self.FDataset.FilterManager;
  //FGrid.SummaryManager := Self.FDataset.SummaryManager;
  //FGrid.ColumnsHeaderMenuVisible:= true;

  FRunningDoUpdateChilds := false;
  Self.AutomaticChildsUpdateMode:= cuOnChangeSelection;
end;

destructor TUramakiKGridPlate.Destroy;
begin
  FGridHelper.Free;
  FUramakiGridHelper.Free;
  FProvider.Free;

  inherited Destroy;
end;

procedure TUramakiKGridPlate.DisableControls;
begin

end;

procedure TUramakiKGridPlate.EnableControls;
begin

end;

procedure TUramakiKGridPlate.RefreshDataset;
begin
  FGridHelper.RefreshDataProvider;
end;

end.
