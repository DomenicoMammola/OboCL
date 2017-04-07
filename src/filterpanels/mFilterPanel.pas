// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFilterPanel;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Controls, Classes, StdCtrls, StrUtils,
  ExtCtrls, EditBtn;

type

  { TmFilterConditionPanel }

  TmFilterConditionPanel = class (TCustomPanel)
  protected
    function CreateStandardLabel : TLabel;
    function FormatFilterCaption (aValue : String) : String;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); virtual; abstract;
    function GetFilterValue : Variant; virtual; abstract;
    procedure Clear; virtual; abstract;
  end;

  { TmDateFilterConditionPanel }

  TmDateFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FDateEdit : TDateEdit;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure Clear; override;
  end;

  { TmEditFilterConditionPanel }

  TmEditFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FEdit : TEdit;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure Clear; override;
  end;

  { TmComboFilterConditionPanel }

  TmComboFilterConditionPanel = class (TmFilterConditionPanel)
  private
    FLabel : TLabel;
    FCombobox: TComboBox;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure Clear; override;
  end;

  { TmExecuteFilterPanel }

  TmExecuteFilterPanel = class (TmFilterConditionPanel)
  private
    FClearButton : TButton;
    FFilterButton : TButton;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetFilterCaption (aValue : String); override;
    function GetFilterValue : Variant; override;
    procedure Clear; override;
  end;

  { TmFilterPanel }

  TmFilterPanel = class (TCustomFlowPanel)
  strict private
    FFilterConditionPanels : TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddFilterCondition (aPanel : TmFilterConditionPanel);
    procedure ClearAll();
  end;


implementation

uses
  SysUtils;

{ TmExecuteFilterPanel }

constructor TmExecuteFilterPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FClearButton := TButton.Create(Self);
  FClearButton.Parent := Self;
  FClearButton.Height:= Self.Height div 2;
  FClearButton.Align:= alTop;
  FClearButton.Caption := 'Ripulisci';
  FFilterButton := TButton.Create(Self);
  FFilterButton.Parent := Self;
  FFilterButton.Align := alClient;
  FFilterButton.Caption := 'Filtra';
end;

procedure TmExecuteFilterPanel.SetFilterCaption(aValue: String);
begin
  // do nothing
end;

function TmExecuteFilterPanel.GetFilterValue: Variant;
begin
  Result := null;
end;

procedure TmExecuteFilterPanel.Clear;
begin
  // do nothing
end;

{ TmFilterPanel }

constructor TmFilterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := alTop;
  Self.AutoSize := true;
  Self.BevelOuter:= bvNone;
  Self.BevelInner:= bvNone;
  FFilterConditionPanels := TList.Create;
end;

destructor TmFilterPanel.Destroy;
begin
  FFilterConditionPanels.Free;
  inherited Destroy;
end;

procedure TmFilterPanel.AddFilterCondition(aPanel: TmFilterConditionPanel);
begin
  FFilterConditionPanels.Add(aPanel);
  aPanel.Parent := Self;
end;

procedure TmFilterPanel.ClearAll;
var
  i : integer;
begin
  for i := 0 to FFilterConditionPanels.Count -1 do
  begin
    TmFilterConditionPanel(FFilterConditionPanels[i]).Clear;
  end;
end;

{ TmEditFilterConditionPanel }

constructor TmEditFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alBottom;
  FEdit.Text := '';
  FLabel := Self.CreateStandardLabel;
end;

procedure TmEditFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmEditFilterConditionPanel.GetFilterValue: Variant;
begin
  if FEdit.Text = '' then
    Result := null
  else
    Result := FEdit.Text;
end;

procedure TmEditFilterConditionPanel.Clear;
begin
  FEdit.Text:= '';
end;

{ TmComboFilterConditionPanel }

constructor TmComboFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCombobox := TComboBox.Create(Self);
  FComboBox.Parent := Self;
  FComboBox.Align:= alBottom;
  FComboBox.Style:= csDropDownList;
  FLabel := Self.CreateStandardLabel;
end;

procedure TmComboFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmComboFilterConditionPanel.GetFilterValue: Variant;
begin
  if FComboBox.ItemIndex < 0 then
    Result := Null
  else
    Result := FComboBox.Items[FComboBox.ItemIndex];
end;

procedure TmComboFilterConditionPanel.Clear;
begin
  FCombobox.ItemIndex:= -1;
end;

{ TmDateFilterConditionPanel }

constructor TmDateFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDateEdit := TDateEdit.Create(Self);
  FDateEdit.Parent := Self;
  FDateEdit.Align:= alBottom;
  FLabel := Self.CreateStandardLabel;
end;

procedure TmDateFilterConditionPanel.SetFilterCaption(aValue: String);
begin
  FLabel.Caption := Self.FormatFilterCaption(aValue);
end;

function TmDateFilterConditionPanel.GetFilterValue: Variant;
begin
  if FDateEdit.Text = '' then
    Result := Null
  else
    Result := FDateEdit.Date;
end;

procedure TmDateFilterConditionPanel.Clear;
begin
  FDateEdit.Text:= '';
end;

{ TmFilterConditionPanel }

function TmFilterConditionPanel.CreateStandardLabel: TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.Caption := '';
  Result.WordWrap:= true;
  Result.Alignment:= taCenter;
end;

function TmFilterConditionPanel.FormatFilterCaption(aValue: String) : String;
begin
  if not AnsiEndsText(':', aValue) then
    Result := aValue + ':'
  else
    Result := aValue;
end;

constructor TmFilterConditionPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.Width := 100;
  Self.Caption := '';
  Self.Height := 40;
end;

end.
