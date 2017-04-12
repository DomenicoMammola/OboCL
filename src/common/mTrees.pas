// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mTrees;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs;

type
  TmTreeNode = class;

  TmTreeNodeList = class
  strict private
    FList : TObjectList;
    FParent : TmTreeNode;
  public
    constructor Create (aParent : TmTreeNode);
    destructor Destroy; override;
    function Count : integer;
    function Get(aIndex : integer) : TmTreeNode;
    procedure Add(aNode : TmTreeNode); overload;
    function Add : TmTreeNode; overload;
    procedure Remove(aNode : TmTreeNode); overload;
    procedure Remove(aIndex : integer); overload;
  end;

  TmTreeNode = class
  strict private
    FChilds : TmTreeNodeList;
    FData : TObject;
    FOwnsData : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Childs : TmTreeNodeList read FChilds;
    property Data : TObject read FData write FData;
    property OwnsData : boolean read FOwnsData write FOwnsData;
  end;

  TmTree = class
  strict private
    FRoot : TmTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateRoot : TmTreeNode;

    property Root : TmTreeNode read FRoot;
  end;

  TmForest = class
  strict private
    FRoots : TmTreeNodeList;
  public
    constructor Create;
    destructor Destroy; override;

    property Roots : TmTreeNodeList read FRoots;
  end;

implementation

uses
  SysUtils;

{ TmTreeNode }

constructor TmTreeNode.Create;
begin
  FChilds := TmTreeNodeList.Create(Self);
  FOwnsData := false;
  FData := nil;
end;

destructor TmTreeNode.Destroy;
begin
  if FOwnsData then
    FreeAndNil(FData);
  FChilds.Free;
  inherited;
end;

{ TmForest }

constructor TmForest.Create;
begin
  FRoots := TmTreeNodeList.Create(nil);
end;

destructor TmForest.Destroy;
begin
  FRoots.Free;
  inherited;
end;

{ TmTreeNodeList }

procedure TmTreeNodeList.Add(aNode: TmTreeNode);
begin
  FList.Add(aNode);
end;

function TmTreeNodeList.Add: TmTreeNode;
begin
  Result := TmTreeNode.Create;
  Self.Add(Result);
end;

function TmTreeNodeList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TmTreeNodeList.Create(aParent : TmTreeNode);
begin
  FList := TObjectList.Create(true);
  FParent := aParent;
end;

destructor TmTreeNodeList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TmTreeNodeList.Get(aIndex: integer): TmTreeNode;
begin
  Result := FList.Items[aIndex] as TmTreeNode;
end;

procedure TmTreeNodeList.Remove(aNode: TmTreeNode);
begin
  FList.Remove(aNode);
end;

procedure TmTreeNodeList.Remove(aIndex: integer);
begin
  FList.Delete(aIndex);
end;

{ TmTree }

constructor TmTree.Create;
begin
  FRoot := nil;
end;

function TmTree.CreateRoot: TmTreeNode;
begin
  if not Assigned(FRoot) then
  begin
    FRoot := TmTreeNode.Create;
    Result := FRoot;
  end
  else
    raise Exception.Create('Root node already exists');
end;

destructor TmTree.Destroy;
begin
  if Assigned(FRoot) then
    FreeAndNil(FRoot);
  inherited;
end;

end.
