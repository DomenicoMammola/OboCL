// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDateEdit;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, EditBtn, Graphics,
  mUtility;

type

  { TmDateEdit }

  TmDateEdit = class (TDateEdit)
  strict private
    FOnExtCustomDate: TCustomDateEvent;
    FCustomGlyph : TBitmap;
    procedure OnInternalCustomDate (Sender : TObject; var ADate : string);
  protected
    property OnCustomDate; // hide the original event
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnExtCustomDate: TCustomDateEvent read FOnExtCustomDate write FOnExtCustomDate;
  end;

implementation

uses
  SysUtils, LResources;

{ TmDateEdit }

procedure TmDateEdit.OnInternalCustomDate(Sender: TObject; var ADate: string);
var
  tmpDate : TDateTime;
begin
  if TryToUnderstandDateString(aDate, tmpDate) then
    ADate:= DateToStr(tmpDate);
  if Assigned(OnExtCustomDate) then
    OnExtCustomDate(Sender, ADate);
end;

constructor TmDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.OnCustomDate:= OnInternalCustomDate;
  FCustomGlyph:= TBitmap.Create;
  FCustomGlyph.LoadFromLazarusResource('mdateedit');
  Self.Glyph := FCustomGlyph;
end;

destructor TmDateEdit.Destroy;
begin
  FCustomGlyph.Free;
  inherited Destroy;
end;

initialization
  {$i mdateedit_gliph.lrs}

end.
