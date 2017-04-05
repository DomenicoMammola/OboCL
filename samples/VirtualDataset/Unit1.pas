unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  mVirtualDataSet, Contnrs,
   Vcl.StdCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls; //, DDuce.Components.ListDataSet;

type
  TListVirtualDatasetDataProvider = class (TVirtualDatasetDataProvider)
  private
    FList : TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetFieldValue (AField: TField; AIndex: Integer; var AValue: variant); override;
    procedure DeleteRecord (AIndex :integer); override;
    procedure EditRecord (AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
  end;

  TForm1 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    TempDataset : TVirtualDataset;
    TempList : TList;
    GarbageCollector : TObjectList;
    FProvider : TListVirtualDatasetDataProvider;
    function GetValue (aObject : TObject; aPropertyName : string) : variant;
  public
    { Public declarations }
  end;

  TCiccio = class
  strict private
    FValueString : string;
    FValueInteger : integer;
    FValueFloat : double;
  published
    property ValueString : string read FValueString write FValueString;
    property ValueInteger : integer read FValueInteger write FValueInteger;
    property ValueFloat : double read FValueFloat write FValueFloat;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TempDataset := TVirtualDataset.Create(Self);
  TempDataset.DatasetDataProvider := FProvider;
  TempDataset.Active := true;
  DataSource1.DataSet := TempDataset;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage(TCiccio(TempList.Items[0]).ValueString);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : integer;
  Dummy : TCiccio;
begin
  TempList := TList.Create;
  GarbageCollector := TObjectList.Create(true);
  for i := 0 to 100 do
  begin
    Dummy := TCiccio.Create;
    Dummy.ValueString := 'CICCIO' + IntToStr(i);
    Dummy.ValueInteger := i;
    Dummy.ValueFloat := (i + 1) / 2;
    TempList.Add(Dummy);
    GarbageCollector.Add(Dummy);
  end;
  FProvider := TListVirtualDatasetDataProvider.Create;
  FProvider.FList := TempList;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TempList.Free;
  GarbageCollector.Free;
  FProvider.Free;
end;

function TForm1.GetValue(aObject: TObject; aPropertyName: string): variant;
begin
  if aPropertyName = 'ValueString' then
    Result := (AObject as TCiccio).ValueString
  else
  if aPropertyName = 'ValueInteger' then
    Result := (AObject as TCiccio).ValueInteger
  else
  if aPropertyName = 'ValueFloat' then
    Result := (AObject as TCiccio).ValueFloat;
end;

{ TListVirtualDatasetDataProvider }

constructor TListVirtualDatasetDataProvider.Create;
begin
  inherited;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueString';
    Id := 0;
    DataType := ftWideString;
    Size := 30;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueInteger';
    Id := 1;
    DataType := ftInteger;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueFloat';
    Id := 2;
    DataType := ftFloat;
  end;
end;

procedure TListVirtualDatasetDataProvider.DeleteRecord(AIndex: integer);
begin
  inherited;

end;

destructor TListVirtualDatasetDataProvider.Destroy;
begin

  inherited;
end;

procedure TListVirtualDatasetDataProvider.EditRecord(AIndex: integer; AModifiedFields: TList);
var
  O : TCiccio;
  i : integer;
  F : TField;
begin
  O := TObject(FList[AIndex]) as TCiccio;
  for i := 0 to AModifiedFields.Count - 1 do
  begin
    F := TObject(AModifiedFields.Items[i]) as TField;
    if F.FieldName = 'ValueString' then
    begin
      O.ValueString := F.AsString;
    end
    else
    if F.FieldName = 'ValueInteger' then
    begin
      O.ValueInteger := F.AsInteger;
    end
    else
    begin
      O.ValueFloat := F.AsFloat;
    end;
  end;

end;

procedure TListVirtualDatasetDataProvider.GetFieldValue(AField: TField;
  AIndex: Integer; var AValue: variant);
var
  AObject : TCiccio;
begin
  if AIndex >= 0 then
  begin
    AObject := TCiccio(FList.Items[AIndex]);
    if AField.FieldName = 'ValueString' then
      AValue := (AObject as TCiccio).ValueString
    else
    if AField.FieldName = 'ValueInteger' then
      AValue := (AObject as TCiccio).ValueInteger
    else
    if AField.FieldName = 'ValueFloat' then
      AValue := (AObject as TCiccio).ValueFloat;
  end;
end;

function TListVirtualDatasetDataProvider.GetRecordCount: integer;
begin
  Result := FList.Count;
end;

procedure TListVirtualDatasetDataProvider.InsertRecord(AIndex: integer;
  AModifiedFields: TList);
begin
  inherited;

end;

end.
