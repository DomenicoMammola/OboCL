// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualFileSystem;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs,
  mTrees;

type
  TmFile = class
  protected
    FName : String;
    FID : String;
    FOwner : String;
    FFileDescriptor : String;
  public
    property ID : string read FID write FID;
    property Name : String read FName write FName;
    property Owner : String read FOwner write FOwner;
    property FileDescriptor : String read FFileDescriptor write FFileDescriptor;
  end;

  TmConfigurationFile = class
  protected
    FUseAsDefault : Boolean;
  public
    property UseAsDefault : Boolean read FBoolean write FBoolean;
  end;

  { TmFiles }

  TmFiles = class sealed
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function Get(aIndex : integer) : TmFile;
  end;

  { TmFolder }

  TmFolder = class
  strict private
    FFiles : TmFiles;
    FTreeNode : TmTreeNode;
    FName : string;
    procedure SetTreeNode(AValue: TmTreeNode);
  public
    constructor Create; override;
    destructor Destroy; override;

    function SubfoldersCount : integer;
    function Subfolder (aIndex : integer) : TmFolder;

    property Files : TmFiles read FFiles;
    property TreeNode : TmTreeNode read FTreeNode write SetTreeNode;
    property Name : String read FName write FName;
  end;

  TmAbstractFileSystemManager = class abstract
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh (aUser : String); virtual; abstract;
  end;

implementation

{ TmFiles }

constructor TmFiles.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmFiles.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmFiles.Count: integer;
begin
  Result := FList.Count;
end;

function TmFiles.Get(aIndex: integer): TmFile;
begin
  Result := FList.Items[aIndex] as TmFile;
end;

{ TmFolder }

procedure TmFolder.SetTreeNode(AValue: TmTreeNode);
begin
  if FTreeNode=AValue then Exit;
  FTreeNode:=AValue;
  FTreeNode.OwnsData:= true;
  FTreeNode.Data := Self;
end;

constructor TmFolder.Create;
begin
  inherited Create;
  FFiles := TmFiles.Create;
end;

destructor TmFolder.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TmFolder.SubfoldersCount: integer;
begin
  if Assigned(FTreeNode) then
    Result := FTreeNode.Childs.Count
  else
    Result := 0;
end;

function TmFolder.Subfolder(aIndex: integer): TmFolder;
begin
  if Assigned(FTreeNode) then
    Result := FTreeNode.Childs.Get(aIndex).Data as TmFolder
  else
    Result := nil;
end;

end.
