// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mEditingFormLinesConfigurationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls, Contnrs;


resourcestring
  SLinesOrderFormCaption = 'Order lines';

type

  { TEditingFormLineSettings }

  TEditingFormLineSettings = class
  strict private
    FName : String;
    FCaption : String;
  public
    constructor Create(const aName, aCaption : String);
    property Name : String read FName write FName;
    property Caption : String read FCaption write FCaption;
  end;

  { TEditingFormLinesSettings }

  TEditingFormLinesSettings = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function Get(const aIndex: integer): TEditingFormLineSettings;
    procedure Add(aLineSettings : TEditingFormLineSettings);
    procedure Clear;
  end;

  { TEditingFormLinesSettingsForm }

  TEditingFormLinesSettingsForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    LinesListBox: TListBox;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinesListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LinesListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LinesListBoxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FGarbage : TObjectList;
  public
    procedure Init(const aSettings : TEditingFormLinesSettings);
    procedure ExtractSettings (aLinesNames : TStringList);
  end;


implementation
uses
  mFormSetup, mBaseClassesAsObjects;

{$R *.lfm}

{ TEditingFormLinesSettings }

constructor TEditingFormLinesSettings.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TEditingFormLinesSettings.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEditingFormLinesSettings.Count: integer;
begin
  Result := FList.Count;
end;

function TEditingFormLinesSettings.Get(const aIndex: integer): TEditingFormLineSettings;
begin
  Result := FList.Items[aIndex] as TEditingFormLineSettings;
end;

procedure TEditingFormLinesSettings.Add(aLineSettings: TEditingFormLineSettings);
begin
  FList.Add(aLineSettings);
end;

procedure TEditingFormLinesSettings.Clear;
begin
  FList.Clear;
end;

{ TEditingFormLineSettings }

constructor TEditingFormLineSettings.Create(const aName, aCaption: String);
begin
  FName := aName;
  FCaption := aCaption;
end;

{ TEditingFormLinesSettingsForm }

procedure TEditingFormLinesSettingsForm.FormCreate(Sender: TObject);
begin
  FGarbage := TObjectList.Create(true);
  SetupFormAndCenter(Self, 0.8);
  Self.Caption := SLinesOrderFormCaption;
end;

procedure TEditingFormLinesSettingsForm.FormDestroy(Sender: TObject);
begin
  FGarbage.Free;
end;

procedure TEditingFormLinesSettingsForm.LinesListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  prev, dest : integer;
  pt : TPoint;
  old: String;
  oldObj : TObject;
begin
  prev := LinesListBox.ItemIndex;
  if prev < 0 then
    exit;
  pt.x:= X;
  pt.y:= Y;
  dest := LinesListBox.ItemAtPos(pt, true);
  if dest = prev then
    exit;
  old := LinesListBox.Items[prev];
  oldObj := LinesListBox.Items.Objects[prev];

  if dest >= 0 then
  begin
    LinesListBox.DeleteSelected;
    if dest < prev then
      LinesListBox.Items.InsertObject(dest, old, oldObj)
    else
      LinesListBox.Items.InsertObject(dest - 1, old, oldObj);
  end;
end;

procedure TEditingFormLinesSettingsForm.LinesListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = LinesListBox);
end;

procedure TEditingFormLinesSettingsForm.LinesListBoxStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  //
end;

procedure TEditingFormLinesSettingsForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TEditingFormLinesSettingsForm.Init(const aSettings: TEditingFormLinesSettings);
var
  i : integer;
  tmp : TStringObject;
begin
  LinesListBox.Items.Clear;
  FGarbage.Clear;
  for i := 0 to aSettings.Count - 1 do
  begin
    tmp := TStringObject.Create(aSettings.Get(i).Name);
    FGarbage.Add(tmp);
    LinesListBox.AddItem(aSettings.Get(i).Caption, tmp);
  end;
  if LinesListBox.Count > 0 then
    LinesListBox.ItemIndex:= 0;
end;

procedure TEditingFormLinesSettingsForm.ExtractSettings(aLinesNames : TStringList);
var
  i : integer;
begin
  aLinesNames.Clear;
  for i := 0 to LinesListBox.Count - 1 do
    aLinesNames.Add((LinesListBox.Items.Objects[i] as TStringObject).Value);
end;


end.

