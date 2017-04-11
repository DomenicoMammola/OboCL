// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFilter;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs;

type
  TmFilterOperator = (foUnknown, foEq, foGtOrEq, foLtOrEq, foLike);

  { TmFilter }

  TmFilter = class
  strict private
    FFieldName : string;
    FFilterOperator : TmFilterOperator;
    FValue : Variant;
  public
    constructor Create;
    destructor Destroy; override;

    property FieldName : string read FFieldName write FFieldName;
    property FilterOperator : TmFilterOperator read FFilterOperator write FFilterOperator;
    property Value : Variant read FValue write FValue;
  end;


  { TmFilters }

  TmFilters = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add : TmFilter; overload;
    procedure Add(aFilter : TmFilter); overload;
    procedure Clear;
    function Count : integer;
    function Get(aIndex : integer) : TmFilter;
  end;
implementation

{ TmFilters }

constructor TmFilters.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmFilters.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmFilters.Add: TmFilter;
begin
  Result := TmFilter.Create;
  FList.Add(Result);
end;

procedure TmFilters.Add(aFilter: TmFilter);
begin
  FList.Add(aFilter);
end;

procedure TmFilters.Clear;
begin
  FList.Clear;
end;

function TmFilters.Count: integer;
begin
  Result := FList.Count;
end;

function TmFilters.Get(aIndex: integer): TmFilter;
begin
  Result := FList.Items[aIndex] as TmFilter;
end;

{ TmFilter }

constructor TmFilter.Create;
begin
  FFilterOperator:= foUnknown;
end;

destructor TmFilter.Destroy;
begin
  inherited Destroy;
end;

end.
