unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, contnrs, mVirtualDataset, mListDataset, db, memds,
  VDataset;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    MemDataset1: TMemDataset;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    FListDataset : TmListDataset;
    FList : TList;
    FBidone : TObjectList;
    TempDataset : TVirtualDataset;
    FProvider : TListVirtualDatasetDataProvider;
    function GetData (aObject : TObject; aPropertyName : string) : variant;
    procedure CreateProvider;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i : integer;
  c : TCiccio;
begin
  FList := TList.Create;
  FBidone := TObjectList.Create(true);
  for i := 1 to 1000 do
  begin
    c := TCiccio.Create;
    c.ValueFloat:= i / 2;
    c.ValueInteger := i;
    c.ValueString:= 'ciccio' + IntToStr(i);
    FList.Add(c);
    FBidone.Add(c);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FListDataset := TmListDataset.Create(Self, FList, @GetData);
  FListDataset.Active := true;
  DataSource1.DataSet := FListDataset;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CreateProvider;
  TempDataset := TVirtualDataset.Create(Self);
  TempDataset.DatasetDataProvider := FProvider;
  TempDataset.Active := true;
  DataSource1.DataSet := TempDataset;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProvider);
  FreeAndNil(FListDataset);
  FreeAndNil(FList);
  FreeAndNil(FBidone);
  FreeAndNil(TempDataset);
  FreeAndNil(FProvider);
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

function TForm1.GetData(aObject: TObject; aPropertyName: string): variant;
begin
  if aPropertyName = 'VDouble' then
    Result := (aObject as TCiccio).ValueFloat
  else
  if aPropertyName = 'VInteger' then
    Result := (aObject as TCiccio).ValueInteger
  else
    Result := (aObject as TCiccio).ValueString;
end;

procedure TForm1.CreateProvider;
var
  i : integer;
  Dummy : TCiccio;
begin
  FProvider := TListVirtualDatasetDataProvider.Create;

  for i := 0 to 100 do
  begin
    Dummy := TCiccio.Create;
    Dummy.ValueString := 'CICCIO' + IntToStr(i);
    Dummy.ValueInteger := i;
    Dummy.ValueFloat := (i + 1) / 2;
    FProvider.Add(Dummy);
  end;
end;

end.

