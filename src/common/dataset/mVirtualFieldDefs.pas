unit mVirtualFieldDefs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB;

type

  TVirtualFieldDefType = (vftUnknown, vftInteger, vftBoolean, vftString, vftFloat, vftCurrency,
    vftDate, vftTime, vftDateTime, vftWideString, vftLargeInt, vftTimeStamp, vftBCD);


  { TVirtualFieldDef }

  TVirtualFieldDef = class(TCollectionItem)
  strict private
    FName : string;
    FDataType : TVirtualFieldDefType;
    FSize : integer;
    FReadOnly : boolean;
    FRequired : boolean;
    FPrecision : integer;
    procedure SetDataType(AValue: TVirtualFieldDefType);
  public
    constructor Create(ACollection: TCollection); override;
    property Name : string read FName write FName;
    property DataType : TVirtualFieldDefType read FDataType write SetDataType;
    property Size : integer read FSize write FSize default 0;
    property Required : boolean read FRequired write FRequired default false;
    property ReadOnly : boolean read FReadOnly write FReadOnly default false;
    property Precision : integer read FPrecision write FPrecision default 0;
  end;

  TVirtualFieldDefs = class(TCollection)
  private
    function GetVirtualFieldDef(I: Integer): TVirtualFieldDef;
  public
    constructor Create;
    function AddFieldDef: TVirtualFieldDef;
    property VirtualFieldDefs[I: Integer]: TVirtualFieldDef read GetVirtualFieldDef; default;
  end;

  function FromTVirtualFieldDefTypeToTFieldType (aSource : TVirtualFieldDefType) : TFieldType;

implementation

function FromTVirtualFieldDefTypeToTFieldType(aSource: TVirtualFieldDefType): TFieldType;
begin
  Result := ftUnknown;
  case aSource of
    vftInteger : Result := ftInteger;
    vftBoolean : Result := ftBoolean;
    vftString : Result := ftString;
    vftFloat : Result := ftFloat;
    vftCurrency: Result := ftCurrency;
    vftDate : Result := ftDate;
    vftTime : Result := ftTime;
    vftDateTime : Result := ftDateTime;
    vftWideString : Result := ftWideString;
    vftLargeInt : Result := ftLargeint;
    vftTimeStamp : Result := ftTimeStamp;
    vftBCD : Result := ftBCD;
  end;
end;

{ TVirtualFieldDef }

procedure TVirtualFieldDef.SetDataType(AValue: TVirtualFieldDefType);
begin
  if FDataType=AValue then Exit;
  FDataType:=AValue;
end;

constructor TVirtualFieldDef.Create(ACollection: TCollection);
begin
  inherited;

  FDataType:= vftUnknown;
  FName := '';
end;

{ TVirtualFieldDefs }

function TVirtualFieldDefs.AddFieldDef: TVirtualFieldDef;
begin
  Result := Add as TVirtualFieldDef;
end;


constructor TVirtualFieldDefs.Create;
begin
  inherited Create(TVirtualFieldDef);
end;

function TVirtualFieldDefs.GetVirtualFieldDef(I: Integer): TVirtualFieldDef;
begin
  Result := Items[I] as TVirtualFieldDef;
end;

end.
