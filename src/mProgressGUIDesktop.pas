unit mProgressGUIDesktop;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, syncobjs,
  mProgressClasses, mProgressForm;

type

  { TmProgressGUIDesktop }

  TmProgressGUIDesktop = class(TmProgressGUI)
  strict private
    FDlg : TProgressForm;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddProgress(aProgress : TmAbstractProgress); override;
    procedure RemoveProgress(aProgress : TmAbstractProgress); override;
    procedure RefreshProgress (aProgress : TmAbstractProgress); override;
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
  //
end;

procedure TmProgressGUIDesktop.RemoveProgress(aProgress: TmAbstractProgress);
begin
  FDlg.Hide;
end;

procedure TmProgressGUIDesktop.RefreshProgress(aProgress: TmAbstractProgress);
begin
  if not FDlg.Visible then
  begin
    FDlg.Show;
  end;
  FDlg.Advance(aProgress.Caption);
end;

initialization
  GetProgressGUIFactory.RegisterProgressGUIClass(TmProgressGUIDesktop);

end.
