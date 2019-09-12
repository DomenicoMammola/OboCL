// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mProgressGUIConsole;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  mProgressClasses;

type

  { TmProgressGUIConsole }

  TmProgressGUIConsole = class (TmProgressGUI)
  strict private
    procedure RefreshProgress (aProgress : TmAbstractProgress);
    procedure RemoveProgress(aProgress : TmAbstractProgress);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddProgress(aProgress : TmAbstractProgress); override;
  end;

implementation

{ TmProgressGUIConsole }

procedure TmProgressGUIConsole.RefreshProgress(aProgress: TmAbstractProgress);
begin
  writeln(aProgress.Caption);
end;

procedure TmProgressGUIConsole.RemoveProgress(aProgress: TmAbstractProgress);
begin
  //
end;

constructor TmProgressGUIConsole.Create;
begin
  //
end;

destructor TmProgressGUIConsole.Destroy;
begin
  inherited Destroy;
end;

procedure TmProgressGUIConsole.AddProgress(aProgress: TmAbstractProgress);
begin
  aProgress.OnRefresh:= Self.RefreshProgress;
  aProgress.OnRemove:= Self.RemoveProgress;
end;

initialization
  GetProgressGUIFactory.RegisterProgressGUIClass(TmProgressGUIConsole);

end.
