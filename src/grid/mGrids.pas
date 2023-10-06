// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGrids;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes, contnrs,
  DBGrids, Grids,
  mGridColumnSettings, mDataProviderInterfaces, mFields;

resourcestring
  SFilterValuesMenuCaption = 'Filter by value...';
  SEditFiltersMenuCaption = 'Edit filters...';
  SRemoveFiltersMenuCaption = 'Remove all filters';
  SAddSummaryMenuCaption = 'Add summary...';
  SRemoveSummariesMenuCaption = 'Remove all summaries';

const
  SImGridInterface = '{E6B14B5B-6A38-429B-B411-F5FAA655E69C}';


type

  { TmGridColumn }

  TmGridColumn = class
  strict private
    FTitle : String;
    FVisible : boolean;
    FFieldName : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(aSource : TColumn); overload;
    procedure Assign(aSource : TGridColumn); overload;
    procedure Assign(aSource : TmField); overload;

    property Title : String read FTitle write FTitle;
    property Visible : boolean read FVisible write FVisible;
    property FieldName : String read FFieldName write FFieldName;
  end;

  { TmGridColumns }

  TmGridColumns = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get(const aIndex : integer): TmGridColumn;
    function Add: TmGridColumn;
    procedure Clear;
  end;

  ImGridCursor = interface
    ['{4BF81888-A79E-4C9E-A8AF-5C27DFA093FC}']
    procedure StartBrowsing;
    procedure EndBrowsing;
    procedure First;
    procedure Next;
    function EOF: boolean;
    function GetValueByFieldName(const aFieldName : String): Variant;
  end;

  ImGrid = interface
    [SImGridInterface]
    procedure ReadSettings(aSettings : TmGridColumnsSettings);
    procedure ApplySettings(aSettings : TmGridColumnsSettings);
    procedure RefreshDataProvider(const aReloadFields: boolean);
    function GetSummaryManager : ISummaryDatasetManager;
    procedure GetFields(aFields : TmFields);
    function GetDataCursor : ImGridCursor;
    procedure GetColumns(aColumns : TmGridColumns);
  end;

implementation

uses
  mBaseClassesAsObjects;

{ TmGridColumn }

constructor TmGridColumn.Create;
begin
  FTitle:= '';
  FVisible:= true;
  FFieldName:= '';
end;

destructor TmGridColumn.Destroy;
begin
  inherited Destroy;
end;

procedure TmGridColumn.Assign(aSource: TColumn);
begin
  FVisible:= aSource.Visible;
  FTitle:= aSource.Title.Caption;
  FFieldName:= aSource.FieldName;
end;

procedure TmGridColumn.Assign(aSource: TGridColumn);
begin
  FVisible:= aSource.Visible;
  FTitle := aSource.Title.Caption;
  if (aSource.Tag > 0) and (TObject(aSource.Tag) is TStringObject) then
    FFieldName := TStringObject(aSource.Tag).Value;
end;

procedure TmGridColumn.Assign(aSource: TmField);
begin
  FVisible:= aSource.Visible;
  FTitle := aSource.DisplayLabel;
  FFieldName := aSource.FieldName;
end;

{ TmGridColumns }

constructor TmGridColumns.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmGridColumns.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmGridColumns.Count: integer;
begin
  Result := FList.Count;
end;

function TmGridColumns.Get(const aIndex: integer): TmGridColumn;
begin
  Result := FList.Items[aIndex] as TmGridColumn;
end;

function TmGridColumns.Add: TmGridColumn;
begin
  Result := TmGridColumn.Create;
  FList.Add(Result);
end;

procedure TmGridColumns.Clear;
begin
  FList.Clear;
end;

end.
