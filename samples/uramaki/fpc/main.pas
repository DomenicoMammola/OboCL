unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  UramakiBase, UramakiEngine, UramakiDesktop,
  Unit1;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MI_Save: TMenuItem;
    MI_Configure: TMenuItem;
    MI_Load: TMenuItem;
    MI_Add: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MI_AddClick(Sender: TObject);
    procedure MI_ConfigureClick(Sender: TObject);
    procedure MI_LoadClick(Sender: TObject);
    procedure MI_SaveClick(Sender: TObject);
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
  FDesktopManager.FillAddRootWidgetMenu(MI_Add);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDesktopManager.Free;
  FTransformer.Free;
  FPublisher.Free;
end;

procedure TForm1.MI_AddClick(Sender: TObject);
begin

end;

procedure TForm1.MI_ConfigureClick(Sender: TObject);
begin
  FDesktopManager.ShowConfigurationForm;
end;

procedure TForm1.MI_LoadClick(Sender: TObject);
var
  tmpStream : TFileStream;
begin
  tmpStream := TFileStream.Create('layout.xml', fmOpenRead or fmShareDenyWrite);
  try
    FDesktopManager.LoadFromStream(tmpStream);
  finally
    tmpStream.Free;
  end;
end;

procedure TForm1.MI_SaveClick(Sender: TObject);
var
  tmpStream : TFileStream;
begin
  tmpStream := TFileStream.Create('layout.xml', fmOpenWrite);
  try
    FDesktopManager.SaveToStream(tmpStream);
  finally
    tmpStream.Free;
  end;
end;

end.

