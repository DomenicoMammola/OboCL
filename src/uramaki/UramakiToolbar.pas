// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit UramakiToolbar;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, Menus, contnrs,
  ATButtons, ATFlatToolbar;

type

  TUramakiToolbarButtonKind = (bkIcon, bkText);

  { TUramakiToolbarItem }

  TUramakiToolbarItem = class
  strict private
    FATButton : TATButton;
    FKind : TUramakiToolbarButtonKind;
    function GetEnabled: boolean;
    procedure SetEnabled(AValue: boolean);
  private
    function GetCaption: String;
    function GetHint: String;
    function GetImageIndex: integer;
    function GetOnClick: TNotifyEvent;
    function GetKind: TUramakiToolbarButtonKind;
    procedure SetCaption(AValue: String);
    procedure SetHint(AValue: String);
    procedure SetImageIndex(AValue: integer);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetKind(AValue: TUramakiToolbarButtonKind);
    property ATButton : TATButton read FATButton write FATButton;
  public
    constructor Create;
    destructor Destroy; override;
    property Hint : String read GetHint write SetHint;
    property Caption : String read GetCaption write SetCaption;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property ImageIndex : integer read GetImageIndex write SetImageIndex;
    property OnClick : TNotifyEvent read GetOnClick write SetOnClick;
    property Kind : TUramakiToolbarButtonKind read GetKind write SetKind;
  end;


  { TUramakiToolbar }

  TUramakiToolbar = class (TComponent)
  strict private
    FPanel: TPanel;
    FToolbar: TATFlatToolbar;
    FGarbage : TObjectList;
    function GetImages: TImageList;
    function GetParent: TWinControl;
    procedure SetImages(AValue: TImageList);
    procedure SetParent(AValue: TWinControl);
  public
    constructor Create(TheOwner: TComponent); overload; override;
    constructor Create(TheOwner: TComponent; const aVertical : boolean); overload;
    destructor Destroy; override;
    function AddButton : TUramakiToolbarItem;
    function AddDropDownButton (aMenu: TPopupMenu): TUramakiToolbarItem;
    procedure AddSeparator;
    procedure Update;

    property Images : TImageList read GetImages write SetImages;
    property Parent : TWinControl read GetParent write SetParent;
  end;

implementation

{ TUramakiToolbarItem }

function TUramakiToolbarItem.GetEnabled: boolean;
begin
  Result := FATButton.Enabled;
end;

procedure TUramakiToolbarItem.SetEnabled(AValue: boolean);
begin
  FATButton.Enabled:= aValue;
end;

function TUramakiToolbarItem.GetCaption: String;
begin
  Result := FATButton.Caption;
end;

function TUramakiToolbarItem.GetHint: String;
begin
  Result := FATButton.Hint;
end;

function TUramakiToolbarItem.GetImageIndex: integer;
begin
  Result := FATButton.ImageIndex;
end;

function TUramakiToolbarItem.GetOnClick: TNotifyEvent;
begin
  Result := FATButton.OnClick;
end;

function TUramakiToolbarItem.GetKind: TUramakiToolbarButtonKind;
begin
  Result := FKind;
end;

procedure TUramakiToolbarItem.SetCaption(AValue: String);
begin
  FATButton.Caption:= aValue;
end;

procedure TUramakiToolbarItem.SetHint(AValue: String);
begin
  FATButton.Hint:= aValue;
end;

procedure TUramakiToolbarItem.SetImageIndex(AValue: integer);
begin
  FATButton.ImageIndex:= aValue;
end;

procedure TUramakiToolbarItem.SetOnClick(AValue: TNotifyEvent);
begin
  FATButton.OnClick:= aValue;
end;

procedure TUramakiToolbarItem.SetKind(AValue: TUramakiToolbarButtonKind);
begin
  FKind:= aValue;
  if (FATButton.PopupMenu <> nil) then
  begin
    if aValue = bkText then
      FATButton.Kind := abuTextChoice// abuTextArrow
    else
      FATButton.Kind:= abuIconOnly; // abuIconArrow;
  end
  else
  begin
    if aValue = bkText then
      FATButton.Kind := abuTextOnly
    else
      FATButton.Kind:= abuIconOnly;
  end;
  FATButton.Arrow:= false;
end;

constructor TUramakiToolbarItem.Create;
begin
  FATButton := nil;
  FKind:= bkText;
end;

destructor TUramakiToolbarItem.Destroy;
begin
  inherited Destroy;
end;

{ TUramakiToolbar }

function TUramakiToolbar.GetImages: TImageList;
begin
  Result := FToolbar.Images;
end;

function TUramakiToolbar.GetParent: TWinControl;
begin
  Result := FPanel.Parent;
end;

procedure TUramakiToolbar.SetImages(AValue: TImageList);
var
  i : integer;
begin
  FToolbar.Images := aValue;
  for i := 0 to FToolbar.ButtonCount - 1 do
    FToolbar.Buttons[i].Images := aValue;
  FToolbar.UpdateControls(true)
end;

procedure TUramakiToolbar.SetParent(AValue: TWinControl);
begin
  FPanel.Parent := aValue;
end;

constructor TUramakiToolbar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPanel := TPanel.Create(TheOwner);
  //FPanel.Parent := TheOwner;
  FPanel.Align:= alTop;
  FToolbar := TATFlatToolbar.Create(FPanel);
  FToolbar.Parent := FPanel;
  FPanel.BorderStyle:= bsSingle;
  FPanel.BevelInner:= bvNone;
  FPanel.BevelOuter:= bvNone;
  FPanel.Height:= 34;
  FToolbar.Align:= alTop;
  FToolbar.Height:= 32;
  //FToolbar.Images := aImageList;
  FToolbar.ShowHint:= true;
  FGarbage:= TObjectList.Create(true);
end;

constructor TUramakiToolbar.Create(TheOwner: TComponent; const aVertical: boolean);
begin
  Self.Create(TheOwner);
  if aVertical then
  begin
    FPanel.Align:= alLeft;
    FPanel.Width := 34;
    FToolbar.Vertical:= aVertical;
    FToolbar.Align:= alLeft;
    FToolbar.Width:= 32;
  end;
end;

destructor TUramakiToolbar.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

function TUramakiToolbar.AddButton: TUramakiToolbarItem;
begin
  Result := TUramakiToolbarItem.Create;
  FGarbage.Add(Result);
  FToolbar.AddButton(-1, nil, '', '', '', true);
  Result.ATButton := FToolbar.Buttons[FToolbar.ButtonCount - 1];
  Result.ATButton.Kind:= abuTextOnly;
  Result.ATButton.Images := FToolbar.Images;
end;

function TUramakiToolbar.AddDropDownButton (aMenu: TPopupMenu): TUramakiToolbarItem;
begin
  Result := TUramakiToolbarItem.Create;
  FGarbage.Add(Result);
  FToolbar.AddDropdown(0, aMenu);
  Result.ATButton := FToolbar.Buttons[FToolbar.ButtonCount - 1];
  Result.ATButton.Kind:= abuTextChoice; // abuTextArrow;
  Result.ATButton.Images := FToolbar.Images;
end;

procedure TUramakiToolbar.AddSeparator;
begin
  FToolbar.AddSep;
end;

procedure TUramakiToolbar.Update;
begin
  FToolbar.UpdateControls;
end;

end.
