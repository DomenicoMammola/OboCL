unit VDataset;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,
  contnrs,
  mVirtualDataSet, mVirtualFieldDefs;

type

  TCiccio = class
  private
    FVString : string;
    FVDouble : double;
    FVInteger : integer;
  public
  published
    property ValueString : string read FVString write FVString;
    property ValueFloat : double read FVDouble write FVDouble;
    property ValueInteger : integer read FVInteger write FVInteger;
  end;


  { TListVirtualDatasetDataProvider }

  TListVirtualDatasetDataProvider = class (TVirtualDatasetDataProvider)
  private
    FList : TList;
    FGarbageCollector : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add (AObj : TObject);

    procedure GetFieldValue (AField: TField; AIndex: Integer; var AValue: variant); override;
    procedure DeleteRecord (AIndex :integer); override;
    procedure EditRecord (AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
  end;

implementation

{ TListVirtualDatasetDataProvider }

constructor TListVirtualDatasetDataProvider.Create;
begin
  inherited;
  FList := TList.Create;
  FGarbageCollector:= TObjectList.Create(true);;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueString';
    DataType := vftString;
    Size := 30;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueInteger';
    DataType := vftInteger;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueFloat';
    DataType := vftFloat;
  end;
end;

procedure TListVirtualDatasetDataProvider.DeleteRecord(AIndex: integer);
begin
  //
end;

destructor TListVirtualDatasetDataProvider.Destroy;
begin
  FList.Free;
  FGarbageCollector.Free;
  inherited;
end;

procedure TListVirtualDatasetDataProvider.Add(AObj: TObject);
begin
  FList.Add(AObj);
  FGarbageCollector.Add(AObj);
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
  //

end;


end.
