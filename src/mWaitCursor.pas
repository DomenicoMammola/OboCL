// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mWaitCursor;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  { TWaitCursor }

  TWaitCursor = class
  public
    class procedure ShowWaitCursor (const aId: String);
    class procedure UndoWaitCursor (const aId: String);
  end;

implementation

uses
  Forms, Controls,
  mMaps;

var
  _cursors : TmStringDictionary;

type

  { TCursorShell }

  TCursorShell = class
  public
    Cursor : TCursor;
    constructor Create(const aCursor: TCursor);
  end;

{ TCursorShell }

constructor TCursorShell.Create(const aCursor: TCursor);
begin
  Self.Cursor := aCursor;
end;

{ TWaitCursor }

class procedure TWaitCursor.ShowWaitCursor(const aId: String);
begin
  _cursors.Add(aId, TCursorShell.Create(Screen.Cursor));
  Screen.Cursor:= crHourGlass;
  {$IFDEF LINUX}
  Application.ProcessMessages;
  {$ENDIF}
end;

class procedure TWaitCursor.UndoWaitCursor(const aId: String);
var
  curShell : TCursorShell;
begin
  curShell := _cursors.Find(aId) as TCursorShell;
  if Assigned(curShell) then
  begin
    Screen.Cursor := curShell.Cursor;
    _cursors.Remove(aId);
  end;
end;

initialization
  _cursors := TmStringDictionary.Create(true);

finalization
  _cursors.Free;

end.
