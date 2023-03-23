unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  {$ifdef fpc}{$ifdef debug} LazLogger,{$endif}{$endif}
  mCalendar, mCalendarClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FCalendar : TmCalendar;
    FPopupMenu : TPopupMenu;
    procedure CheckSelected (aSender : TObject);
    procedure getAppointments (const aDate : TDate; const aAppointments: tmCalendarAppointments);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  mIntList;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  mi : TMenuItem;
begin
  FCalendar := TmCalendar.Create(Self);
  FCalendar.Parent := Self;
  FCalendar.Align := alClient;
  FCalendar.CaptionSize:= 24;
  FCalendar.Rows:= 1;
  FCalendar.Cols:= 2;
  FCalendar.Style := csAppointmentsList;
  FCalendar.OnGetAppointments:= @getAppointments;
  FPopupMenu := TPopupMenu.Create(Self);
  FCalendar.PopupMenu := FPopupMenu;
  mi := TMenuItem.Create(FPopupMenu);
  FPopupMenu.Items.Add(mi);
  mi.Caption:= 'Get selected';
  mi.OnClick:= @CheckSelected;
end;

procedure TForm1.CheckSelected(aSender: TObject);
begin
  ShowMessage('Selected ' + IntToStr(FCalendar.SelectedBuckets.Count) + ' days');
end;

procedure TForm1.getAppointments(const aDate: TDate; const aAppointments: tmCalendarAppointments);
begin
  if DayOfWeek(aDate) = 1 then
  begin
    with aAppointments.Add do
    begin
      Description := 'Appointment of ' + DateToStr(aDate);
      Color := clRed;
      UniqueId:= DateToStr(aDate);
    end;
  end;
end;

end.

