unit mListDataset;

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  Db, mVirtualDataSet;

type

  TmGetValueOfProperty = function (aObject : TObject; aPropertyName : string) : variant of object;

  TmListDataset = class(TCustomVirtualDataset)
  strict private
    FList             : TList;
    FDataPropertyName : string;
    FUpdateCount      : Integer;
    FGetValueFunction : TmGetValueOfProperty;

    procedure SetList(const Value: TList);
    procedure SetDataPropertyName(Value: string);
//    function GetCurrent: TValue;

    procedure LoadFieldDefsFromRtti(AList : TList; AFieldDefs : TFieldDefs);
    procedure LoadFieldDefsFromFields(aFields: TFields; aFieldDefs: TFieldDefs);
  protected
    function GetRecordCount: Integer; override;

    // Overrides
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(
          Field : TField;
          Index : Integer;
      var Value : variant
    ); override;
    procedure DoPostData(Index: Integer); override;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    procedure MasterChanged(Sender: TObject); override;
    procedure RefreshDataListFromSource;
    procedure OnListChanged(Sender: TObject);

  public
    constructor Create(AOwner : TComponent; AList  : TList; AGetValueFunction : TmGetValueOfProperty); reintroduce; virtual;
    procedure BeforeDestruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function Locate(
      const KeyFields : string;
      const KeyValues : Variant;
            Options   : TLocateOptions
    ): Boolean; override;

  published
    property List: TList read FList write SetList;

//    property Current: TValue read GetCurrent;

    property DataPropertyName: string
      read FDataPropertyName write SetDataPropertyName;

    property MasterSource;
    property FieldDefs;
    property Fields;

    // Events
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnDeleteError;
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

implementation

//uses
//  DDuce.Logger;

resourcestring
  SDataListNotAssigned = 'List property not set, cannot open Dataset.';
  sIllegalVarType = 'Illegal variant type';

{$REGION 'TmListDataset'}
{$REGION 'construction and destruction'}
constructor TmListDataset.Create(AOwner: TComponent; AList: TList; AGetValueFunction : TmGetValueOfProperty);
begin
  inherited Create(AOwner);
  FList := AList;
  FGetValueFunction := AGetValueFunction;
end;

procedure TmListDataset.BeforeDestruction;
begin
  FList := nil;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TmListDataset.SetList(const Value: TList);
begin
  if Assigned(Value) then
  begin
    FList := Value;
    InternalOpen;
  end;
end;

procedure TmListDataset.SetDataPropertyName(Value: string);
begin
  CheckInactive;
  if Value <> FDataPropertyName then
    FDataPropertyName := Value;
end;

(*function TmListDataset.GetCurrent: TValue;
begin
  if Assigned(List) then
    Result := List[CurrentRecord]
  else
    Result := TValue.Empty;
end;*)

function TmListDataset.GetRecordCount: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := -1;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TmListDataset.DoDeleteRecord(Index: Integer);
begin
  BeginUpdate;
  try
    FList.Delete(Index);
  finally
    EndUpdate;
  end;
end;

procedure TmListDataset.DoGetFieldValue(Field: TField; Index: Integer; var Value: variant);
var
  O: TObject;
begin
  if Index < FList.Count then
  begin
    O := TObject(FList[Index]);
    if Field.Index >= 0 then
    begin
      Value := FGetValueFunction(O, Field.FieldName);
      if not VarIsEmpty(Value) then begin
        if VarIsArray(Value) then
          raise Exception.Create(sIllegalVarType);
      end;
    end
    else
      Value := Null;
  end;
end;

procedure TmListDataset.DoPostData(Index: Integer);
var
  O  : TObject;
  F  : TField;
//  TD : PTypeData;
  i : integer;
begin
  O := nil;
  BeginUpdate;
  try
    if State in dsEditModes then
    begin
      if State = dsEdit then
      begin
        O := TObject(FList[Index]);
      end
      else if State = dsInsert then
      begin

(*
        {$IFDEF DSHARP}
        TD := GetTypeData(FList.ItemType);
        {$ENDIF}
        {$IFDEF SPRING}
        TD := GetTypeData(FList.ElementType);
        {$ENDIF}
        *)
        O := TObject(FList.Items[0]).ClassType.Create;
        // in newer Delphis we can use:
        //    O := FList.ItemType.TypeData.ClassType.Create
        if Index <> -1 then
          FList.Insert(Index, O)
        else
          FList.Add(O);
      end;
      for i := 0 to ModifiedFields.Count - 1 do
      begin
        F := TObject(ModifiedFields.Items[i]) as TField;
        SetPropValue(O, F.FieldName, F.AsVariant);
      end;
    end;
  finally
    EndUpdate;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TmListDataset.LoadFieldDefsFromRtti(AList: TList; AFieldDefs: TFieldDefs);
var
  FT : TFieldType;
  FS : Integer;
  FD : TFieldDef;
  S  : string;
  NumOfProps : integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  FirstObject : TObject;
  i : integer;
  PropType: PTypeInfo;
begin
  if not Assigned(AList) then
    Exit;

  if AList.Count = 0 then
    Exit;

  FirstObject := TObject(AList[0]);
  NumOfProps := GetTypeData(FirstObject.ClassInfo)^.PropCount;
  if NumOfProps > 0 then begin
    GetMem(PropList, NumOfProps * SizeOf(Pointer));
    try
      GetPropInfos(FirstObject.ClassInfo, PropList);
      for i := 0 to NumOfProps - 1 do begin
        PropInfo := PropList^[i];
        if PropInfo = nil then continue;
        if IsStoredProp(FirstObject, PropInfo) then
        begin
          FT := ftUnknown;
          FS := 0;
          S := PropInfo^.Name;
          PropType := {$IFDEF FPC}PropInfo^.PropType{$ELSE}PPropInfo(PropInfo)^.PropType^{$ENDIF};
          case PropType^.Kind of
            tkInteger:
            begin
              FT := ftInteger;
            end;
            tkEnumeration:
            begin
              if S = 'System.Boolean' then
                FT := ftBoolean
              else
              begin
                FT := ftWideString;
                FS := 250;
              end;
            end;
            tkFloat:
            begin
              if S = 'System.TDateTime' then
                FT := ftDateTime
              else if S = 'System.TDate' then
                FT := ftDate
              else if S = 'System.TTime' then
                FT := ftTime
              else
                FT := ftFloat;
            end;
            tkChar:
            begin
              FT := ftFixedChar;
              FS := 1;
            end;
            tkWChar:
            begin
              FT := ftFixedWideChar;
              FS := 2;
            end;
            tkString, tkLString{$IFDEF FPC}, tkAString{$ENDIF}:
            begin
              FT := ftString;
              FS := 2500;
            end;
            tkWString, tkUString:
            begin
              FT := ftWideString;
              FS := 2500;
            end;
            tkSet:
            begin
              FT := ftWideString;
              FS := 250;
            end;
            tkArray, tkVariant:
            begin
              FT := ftVariant;
            end;
            tkInt64:
            begin
              FT := ftLargeint;
            end;
            tkClass, tkMethod, tkRecord, tkInterface, tkDynArray, tkUnknown
             {$IFNDEF FPC}, tkClassRef, tkPointer, tkProcedure{$ENDIF}:
            begin
              FT := ftUnknown;
            end;
          end;

          if FT <> ftUnknown then // create Field definitiion for current property
          begin
            FD             := AFieldDefs.AddFieldDef;
            FD.Name        := PropInfo^.Name;
            FD.DisplayName := PropInfo^.Name;
            FD.DataType    := FT;
            if FS <> 0 then
              FD.Size := FS;
//            if not PropInfo.IsWritable then
//              FD.Attributes := FD.Attributes + [faReadonly];
          end;
        end;
      end;
    finally
      FreeMem(PropList, NumOfProps * SizeOf(Pointer));
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TmListDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  if Fields.Count > 0 then
    LoadFieldDefsFromFields(Fields, FieldDefs)
  else
    LoadFieldDefsFromRtti(FList, FieldDefs);
end;

procedure TmListDataset.InternalOpen;
begin
  if Assigned(MasterSource) and (FDataPropertyName <> '') then
    RefreshDataListFromSource
  else if not Assigned(List) then
    VirtualDatasetError(SDataListNotAssigned, Self);

  InternalFirst;

  BookmarkSize := SizeOf(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  Fields.Clear;
  //if DefaultFields then
    CreateFields;

  BindFields(True);
  RecordBufferSize := SizeOf(TRecordInfo) + (Fields.Count * SizeOf(Variant));
end;

function TmListDataset.IsCursorOpen: Boolean;
begin
  Result := FList <> nil;
end;

procedure TmListDataset.MasterChanged(Sender: TObject);
begin
  RefreshDataListFromSource;
  inherited;
end;

procedure TmListDataset.OnListChanged(Sender: TObject);
begin
  if Active then
    Refresh;
end;

procedure TmListDataset.RefreshDataListFromSource;
var
  F : TField;
  I : IInterface;
  L : TList;
begin
  FList := nil;
  if (MasterSource <> nil) and (FDataPropertyName <> '') then
  begin
    F := MasterDataLink.Dataset.FindField(FDataPropertyName);
{$IFNDEF FPC}
    if (F <> nil) and (F is TInterfaceField) then
    begin
      I := (F as TInterfaceField).Value;
//      if Supports(I, IList, L) then
        FList := L;
    end;
{$ENDIF}
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TmListDataset.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TmListDataset.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

function TmListDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  Result := False;
end;
{$ENDREGION}
{$ENDREGION}

procedure TmListDataset.LoadFieldDefsFromFields(aFields: TFields; aFieldDefs: TFieldDefs);
var
  i: integer;
  LField: TField;
  LFieldDef: TFieldDef;
begin
  for I := 0 to aFields.Count - 1 do
  begin
    LField := aFields[I];
    if FieldDefs.IndexOf(LField.FieldName) = -1 then
    begin
      LFieldDef := aFieldDefs.AddFieldDef;
      LFieldDef.Name := LField.FieldName;
      LFieldDef.DataType := LField.DataType;
      LFieldDef.Size := LField.Size;
      if LField.Required then
        LFieldDef.Attributes := [DB.faRequired];
      if LField.ReadOnly then
        LFieldDef.Attributes := LFieldDef.Attributes + [DB.faReadonly];
      if (LField.DataType = ftBCD) and (LField is TBCDField) then
        LFieldDef.Precision := TBCDField(LField).Precision;
    end;
  end;
end;

end.
