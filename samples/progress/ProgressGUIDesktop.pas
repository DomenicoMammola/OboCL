// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit ProgressGUIDesktop;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Forms,
  mProgressClasses,
  progressform;

type

  { TmProgressGUIDesktop }

  TmProgressGUIDesktop = class(TmProgressGUI)
  strict private
    FDlg : TProgressForm;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddProgress(aProgress : TmAbstractProgress); override;
  end;

implementation

{ TmProgressGUIDesktop }

constructor TmProgressGUIDesktop.Create;
begin
  FDlg := TProgressForm.Create(nil);
end;

destructor TmProgressGUIDesktop.Destroy;
begin
  FDlg.Free;
  inherited Destroy;
end;

procedure TmProgressGUIDesktop.AddProgress(aProgress: TmAbstractProgress);
begin
  FDlg.AddProgress(aProgress);
end;


initialization
  GetProgressGUIFactory.RegisterProgressGUIClass(TmProgressGUIDesktop);

end.
