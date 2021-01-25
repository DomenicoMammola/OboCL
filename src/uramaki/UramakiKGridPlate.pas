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
  kgrids, Classes, Controls, Menus,
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  {$ENDIF}
  mPivoter, mDataProviderInterfaces, mFilterPanel, mFilter, mXML,
  UramakiBase, UramakiToolbar;

const
  WM_USER_REFRESHCHILDS = WM_USER + 11;
  WM_USER_CLEARCHILDS = WM_USER + 12;

resourcestring
  SConfigureCommandHint = 'Configure...';
  SConfigurePivotCommandHint = 'Configure pivot...';
  SConfigurePivotCommandCaption = 'Configure pivot';


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
    FConfigurePopupMenu : TPopupMenu;

    procedure OnClearFilter (Sender : TObject);
    procedure OnExecuteFilter (Sender : TObject);
    procedure ReloadData (aFilters : TmFilters); virtual; abstract;
    procedure ProcessClearChilds(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message WM_USER_CLEARCHILDS;
    procedure CreateToolbar(aImageList : TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex : integer);
    procedure OnEditSettings(Sender : TObject);
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
  mWaitCursor, mPivotSettings, mPivotSettingsForm;

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

procedure TUramakiKGridAsPivotPlate.CreateToolbar(aImageList: TImageList; aConfigureImageIndex, aRefreshChildsImageIndex, aGridCommandsImageIndex: integer);
var
  mItm : TMenuItem;
begin
  FToolbar := TUramakiToolbar.Create(Self);
  FToolbar.Images := aImageList;
  FToolbar.Parent := Self;

  FConfigurePopupMenu := TPopupMenu.Create(FToolbar);

  with FToolbar.AddDropDownButton(FConfigurePopupMenu) do
  begin
    Hint := SConfigureCommandHint;
    ImageIndex:= aConfigureImageIndex;
    Kind := bkIcon;
  end;

  mItm := TMenuItem.Create(FConfigurePopupMenu);
  FConfigurePopupMenu.Items.Add(mItm);
  mItm.OnClick:= Self.OnEditSettings;
  mItm.Hint:= SConfigurePivotCommandHint;
  mItm.Caption:= SConfigurePivotCommandCaption;

  FToolbar.Update;
end;

procedure TUramakiKGridAsPivotPlate.OnEditSettings(Sender: TObject);
var
  frm : TPivotSettingsForm;
begin
  frm := TPivotSettingsForm.Create(nil);
  try
    frm.Init(FPivoter);
    if frm.ShowModal = mrOk then
    begin
      try
        TWaitCursor.ShowWaitCursor('TUramakiKGridAsPivotPlate.OnEditSettings');
        //Intf.ApplySettings(FSettings);
      finally
        TWaitCursor.UndoWaitCursor('TUramakiKGridAsPivotPlate.OnEditSettings');
      end;
    end;
  finally
    frm.Free;
  end;
end;

constructor TUramakiKGridAsPivotPlate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPivoter := TmPivoter.Create;
  FGrid := TKGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.RowCount:= 0;
  FGrid.ColCount:= 0;

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
var
  cursor : TmXmlElementCursor;
begin
  cursor := TmXmlElementCursor.Create(aXMLElement, 'pivot');
  try
    if cursor.Count = 1 then
      LoadPivotConfigurationToXML(FPivoter.VerticalGroupByDefs, FPivoter.HorizontalGroupByDefs, FPivoter.SummaryDefinitions, cursor.Elements[0]);
  finally
    cursor.Free;
  end;
end;

procedure TUramakiKGridAsPivotPlate.SaveConfigurationToXML(aXMLElement: TmXmlElement);
begin
  SavePivotConfigurationToXML(FPivoter.VerticalGroupByDefs, FPivoter.HorizontalGroupByDefs, FPivoter.SummaryDefinitions, aXMLElement.AddElement('pivot'));
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
