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
  SLinesOrderFormCaption = 'Order of the lines';

type

  { TEditingFormLineSettings }

  TEditingFormLineSettings = class
  strict private
    FMandatory: boolean;
    FName : String;
    FCaption : String;
  public
    constructor Create(const aName, aCaption : String; const aMandatory : boolean); overload;
    constructor Create; overload;
    property Name : String read FName write FName;
    property Caption : String read FCaption write FCaption;
    property Mandatory : boolean read FMandatory write FMandatory;
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
    function Add : TEditingFormLineSettings;
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
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OkBtnClick(Sender: TObject);
  private
    FSettings : TEditingFormLinesSettings;
  public
    procedure Init(const aSettings : TEditingFormLinesSettings);
    procedure ExtractSettings (aLinesNames : TStringList);
  end;


implementation
uses
  LCLType,
  mFormSetup;

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

function TEditingFormLinesSettings.Add: TEditingFormLineSettings;
begin
  Result := TEditingFormLineSettings.Create;
  Self.Add(Result);
end;

procedure TEditingFormLinesSettings.Clear;
begin
  FList.Clear;
end;

{ TEditingFormLineSettings }

constructor TEditingFormLineSettings.Create(const aName, aCaption: String; const aMandatory : boolean);
begin
  FName := aName;
  FCaption := aCaption;
  FMandatory := aMandatory;
end;

constructor TEditingFormLineSettings.Create;
begin
  Self.Create('', '', false);
end;

{ TEditingFormLinesSettingsForm }

procedure TEditingFormLinesSettingsForm.FormCreate(Sender: TObject);
begin
  FSettings := TEditingFormLinesSettings.Create;
  SetupFormAndCenter(Self, 0.8);
  Self.Caption := SLinesOrderFormCaption;
  LinesListBox.OnDrawItem:= @Self.LBDrawItem;
  LinesListBox.Style:= lbOwnerDrawFixed;
  LinesListBox.ItemHeight:= 20;
end;

procedure TEditingFormLinesSettingsForm.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
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
      if dest = LinesListBox.Items.Count then
        LinesListBox.Items.AddObject(old, oldObj)
      else
        LinesListBox.Items.InsertObject(dest, old, oldObj);
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

procedure TEditingFormLinesSettingsForm.LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;
  tmpListBox : TListBox;
  tmpSettings : TEditingFormLineSettings;
  str : String;
begin
  tmpListBox := Control as TListBox;
  if odSelected in State then
  begin
    if tmpListBox.Focused then
      tmpListBox.Canvas.Brush.Color := clHighlight
    else
      tmpListBox.Canvas.Brush.Color := clGray;
    tmpListBox.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    if (Index mod 2 = 0) then
      aColor:= clWhite
    else
      aColor := clMoneyGreen;
    tmpListBox.Canvas.Brush.Color:=aColor;
  end;

  tmpListBox.Canvas.FillRect(ARect);
  if Assigned(tmpListBox.Items.Objects[index]) then
  begin
    tmpSettings:= tmpListBox.Items.Objects[index] as TEditingFormLineSettings;
    str := tmpSettings.Caption;
    if tmpSettings.Mandatory then
      str := str + ' *';
    tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top+2, str);
  end
  else
    tmpListBox.Canvas.TextRect(ARect, 2, ARect.Top+2, tmpListBox.Items[Index]);
end;

procedure TEditingFormLinesSettingsForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TEditingFormLinesSettingsForm.Init(const aSettings: TEditingFormLinesSettings);
var
  i : integer;
  tmp : TEditingFormLineSettings;
begin
  LinesListBox.Items.Clear;
  FSettings.Clear;
  for i := 0 to aSettings.Count - 1 do
  begin
    tmp := FSettings.Add;
    tmp.Name := aSettings.Get(i).Name;
    tmp.Caption:= aSettings.Get(i).Caption;
    tmp.Mandatory:= aSettings.Get(i).Mandatory;
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
    aLinesNames.Add((LinesListBox.Items.Objects[i] as TEditingFormLineSettings).Name);
end;


end.

