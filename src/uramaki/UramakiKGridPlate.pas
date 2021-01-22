// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiKGridPlate;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}


uses
  kgrids, Classes, Controls,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}
  mPivoter, mDataProviderInterfaces, mFilterPanel, mFilter, mXML, mPivotSettings,
  UramakiBase, UramakiToolbar;

const
  WM_USER_REFRESHCHILDS = WM_USER + 11;
  WM_USER_CLEARCHILDS = WM_USER + 12;

type

  { TUramakiKGridAsPivotPlate }

  TUramakiKGridAsPivotPlate = class(TUramakiPlate)
  strict private
    procedure InvokeChildsClear;
  protected
    FPivoter : TmPivoter;
    FDataProvider : IVDDataProvider;
    FGrid : TKGrid;
    FFilterPanel : TmFilterPanel;
    FToolbar : TUramakiToolbar;

    procedure OnClearFilter (Sender : TObject);
    procedure OnExecuteFilter (Sender : TObject);
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;
    procedure ProcessClearChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_CLEARCHILDS;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(aDataProvider : IVDDataProvider);
    procedure ClearPivot;
    procedure BuildPivot;
    procedure LoadConfigurationFromXML (aXMLElement : TmXmlElement); override;
    procedure SaveConfigurationToXML (aXMLElement : TmXmlElement); override;
    procedure Clear; override;

    property Pivoter : TmPivoter read FPivoter;
  end;



implementation

uses
  sysutils,
  mWaitCursor;

{ TUramakiKGridAsPivotPlate }

procedure TUramakiKGridAsPivotPlate.InvokeChildsClear;
begin
  PostMessage(Self.Handle, WM_USER_CLEARCHILDS, 0, 0);
end;

procedure TUramakiKGridAsPivotPlate.OnClearFilter(Sender: TObject);
begin
  //
end;

procedure TUramakiKGridAsPivotPlate.OnExecuteFilter(Sender: TObject);
var
  tmpFilters : TmFilters;
begin
  try
    TWaitCursor.ShowWaitCursor('TUramakiKGridAsPivotPlate.OnExecuteFilter');

    tmpFilters := TmFilters.Create;
    try
      FFilterPanel.GetFilters(tmpFilters);
      Self.ClearPivot;
      Self.ReloadData(tmpFilters);
      Self.BuildPivot;
    finally
      tmpFilters.Free;
    end;
  finally
    TWaitCursor.UndoWaitCursor('TUramakiKGridAsPivotPlate.OnExecuteFilter');
  end;
end;

procedure TUramakiKGridAsPivotPlate.ProcessClearChilds(var Message: TLMessage);
begin
  EngineMediator.PleaseClearMyChilds(Self);
end;

constructor TUramakiKGridAsPivotPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPivoter := TmPivoter.Create;
  FGrid := TKGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;

end;

destructor TUramakiKGridAsPivotPlate.Destroy;
begin
  FPivoter.Free;
  FreeAndNil(FToolbar);
  inherited Destroy;
end;

procedure TUramakiKGridAsPivotPlate.Init(aDataProvider: IVDDataProvider);
begin
  FDataProvider := aDataProvider;
  FPivoter.DataProvider := FDataProvider;
end;

procedure TUramakiKGridAsPivotPlate.ClearPivot;
begin

end;

procedure TUramakiKGridAsPivotPlate.BuildPivot;
begin

end;

procedure TUramakiKGridAsPivotPlate.LoadConfigurationFromXML(aXMLElement: TmXmlElement);
begin

end;

procedure TUramakiKGridAsPivotPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
begin

end;

procedure TUramakiKGridAsPivotPlate.Clear;
begin
  Self.ClearPivot;
  FPivoter.Clear;
  if Assigned(FPivoter.DataProvider) then
    FPivoter.DataProvider.Clear;
  InvokeChildsClear;
end;

end.
