// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// This can be uses as a workaround for this problem:
// https://forum.lazarus.freepascal.org/index.php?topic=16308.0
//
unit mCalendarDialog;

{$mode objfpc}
{$H+}

interface

uses
  Forms, Controls, Calendar, ButtonPanel, Classes;

type

  { TmCalendarForm }

  TmCalendarForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Calendar: TCalendar;
    procedure CalendarDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

  { TmCalendarDialog }

  TmCalendarDialog = class
  strict private
    FCurrentDate : TDate;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute (const aCaption : String = '') : boolean;

    property Date : TDate read FCurrentDate write FCurrentDate;
  end;

implementation

uses
  SysUtils;

{$R *.lfm}

{ TmCalendarForm }

procedure TmCalendarForm.FormShow(Sender: TObject);
begin
  Calendar.SetFocus;
end;

procedure TmCalendarForm.CalendarDblClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

{ TmCalendarDialog }

constructor TmCalendarDialog.Create;
begin
  Self.FCurrentDate:= SysUtils.Date;
end;

destructor TmCalendarDialog.Destroy;
begin
  inherited Destroy;
end;

function TmCalendarDialog.Execute(const aCaption : String = ''): boolean;
var
  Frm : TmCalendarForm;
begin
  Frm := TmCalendarForm.Create(nil);
  try
    if aCaption <> '' then
      Frm.Caption:= aCaption;
    Frm.Calendar.DateTime:= Self.FCurrentDate;
    if Frm.ShowModal = mrOk then
    begin
      Result := true;
      FCurrentDate:= Frm.Calendar.DateTime;
    end
    else
      Result := false;
  finally
    Frm.Free;
  end;
end;

end.

