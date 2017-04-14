unit mGridOptions;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs, Grids, DBGrids,
  mMaps, mXML;

type

  { TmGridFieldOptions }

  TmGridFieldOptions = class
  private
    FDisplayFormat: String;
    FFieldName : String;
    FDisplayLabel : String;
    FVisible : boolean;
    FWidth : integer;
    FSortOrder : integer;
  public
    constructor Create;
    procedure Clear;

    procedure ExtractFromColumn (aColumn : TColumn);
    procedure ApplyToColumn(aColumn : TColumn);

    procedure SaveToXmlElement (aElement : TmXmlElement);
    procedure LoadFromXmlElement(aElement : TmXmlElement);

    property DisplayLabel : String read FDisplayLabel write FDisplayLabel;
    property Visible : Boolean read FVisible write FVisible;
    property DisplayFormat : String read FDisplayFormat write FDisplayFormat;
    property Width : Integer read FWidth write FWidth;
    property SortOrder : integer read FSortOrder write FSortOrder;
  end;

  { TmGridFieldsOptions }

  TmGridFieldsOptions = class
  strict private
    FMap : TmStringDictionary;
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddOptionsForField (aFieldName : String) : TmGridFieldOptions;
    function GetOptionsForField (aFieldName : String) : TmGridFieldOptions;

    procedure SaveToXmlElement (aElement : TmXmlElement);
    procedure LoadFromXmlElement (aElement : TmXmlElement);

    procedure ExtractFromGrid(aGrid : TDBGrid);
    procedure ApplyToGrid(aGrid : TDBGrid);

    procedure Clear;
  end;


implementation

uses
  SysUtils;

{ TmGridFieldsOptions }

constructor TmGridFieldsOptions.Create;
begin
  FList := TObjectList.Create(true);
  FMap := TmStringDictionary.Create();
end;

destructor TmGridFieldsOptions.Destroy;
begin
  FMap.Free;
  FList.Free;
  inherited Destroy;
end;

function TmGridFieldsOptions.AddOptionsForField(aFieldName: String): TmGridFieldOptions;
var
  tmp : String;
  oldOptions : TmGridFieldOptions;
begin
  tmp := Uppercase(aFieldName);
  oldOptions := FMap.Find(tmp) as TmGridFieldOptions;
  if Assigned(oldOptions) then
  begin
    FMap.Remove(tmp);
    FList.Remove(oldOptions);
  end;
  Result := TmGridFieldOptions.Create;
  Result.FFieldName:= aFieldName;
  FList.Add(Result);
  FMap.Add(Uppercase(aFieldName), Result);
end;

function TmGridFieldsOptions.GetOptionsForField(aFieldName: String): TmGridFieldOptions;
begin
  Result := FMap.Find(Uppercase(aFieldName)) as TmGridFieldOptions;
end;

procedure TmGridFieldsOptions.ExtractFromGrid(aGrid: TDBGrid);
var
  op : TmGridFieldOptions;
  i : integer;
begin
  for i := 0 to aGrid.Columns.Count - 1 do
  begin
    op := Self.AddOptionsForField(aGrid.Columns.Items[i].FieldName);
    op.ExtractFromColumn(aGrid.Columns.Items[i]);
  end;
end;

procedure TmGridFieldsOptions.ApplyToGrid(aGrid: TDBGrid);
var
  op : TmGridFieldOptions;
  i : integer;
begin
  for i := 0 to aGrid.Columns.Count - 1 do
  begin
    op := Self.GetOptionsForField(aGrid.Columns.Items[i].FieldName);
    op.ApplyToColumn(aGrid.Columns.Items[i]);
  end;
end;

procedure TmGridFieldsOptions.Clear;
begin
  FList.Clear;
  FMap.Clear;
end;

{ TmGridFieldOptions }

constructor TmGridFieldOptions.Create;
begin
  Clear;
end;

procedure TmGridFieldOptions.Clear;
begin
  FFieldName := '';
  FDisplayLabel := '';
  FVisible := true;
  FDisplayFormat := '';
  FWidth := -1;
  FSortOrder := -1;
end;

procedure TmGridFieldOptions.ExtractFromColumn(aColumn: TColumn);
begin
  Self.Visible:= aColumn.Visible;
  Self.DisplayFormat:= aColumn.DisplayFormat;
  Self.DisplayLabel:= aColumn.Title.Caption;
  Self.SortOrder:= aColumn.Index;
  Self.Width:= aColumn.Width;
end;

procedure TmGridFieldOptions.ApplyToColumn(aColumn: TColumn);
begin
  aColumn.Visible := Self.Visible;
  aColumn.DisplayFormat := Self.DisplayFormat;
  aColumn.Title.Caption := Self.DisplayLabel;
  aColumn.Width:= Self.Width;
  if Self.SortOrder >= 0 then
    aColumn.Index := Self.SortOrder;
end;

procedure TmGridFieldOptions.SaveToXmlElement(aElement: TmXmlElement);
begin
  aElement.SetAttribute('visible', BoolToStr(FVisible, true));
  aElement.SetAttribute('displayFormat', FDisplayFormat);
  aElement.SetAttribute('displayLabel', FDisplayLabel);
  aElement.SetIntegerAttribute('width', FWidth);
  aElement.SetIntegerAttribute('sortOrder', FSortOrder);
end;

procedure TmGridFieldOptions.LoadFromXmlElement(aElement: TmXmlElement);
begin
  FVisible := StrToBool(aElement.GetAttribute('visible', 'true'));
  FDisplayFormat := aElement.GetAttribute('displayFormat', '');
  FDisplayLabel := aElement.GetAttribute('displayLabel', '');
  FWidth := aElement.GetIntegerAttribute('width', -1);
  FSortOrder := aElement.GetIntegerAttribute('sortOrder', -1);
end;

end.
