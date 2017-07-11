unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  UramakiBase, UramakiEngine, UramakiDesktop,
  Unit1;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDesktopManager : TUramakiDesktopManager;
    FTransformer : TSimpleTransformer;
    FPublisher : TStupidPublisher;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTransformer := TSimpleTransformer.Create;
  FPublisher := TStupidPublisher.Create;

  FDesktopManager := TUramakiDesktopManager.Create;
  FDesktopManager.Init(Self);
  FDesktopManager.AddTransformer(FTransformer);
  FDesktopManager.AddPublisher(FPublisher);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDesktopManager.Free;
  FTransformer.Free;
  FPublisher.Free;
end;

end.

