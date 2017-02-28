// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mIntList;

interface

uses Classes
{$IFNDEF FPC}
  ,System.Types
{$ENDIF};

type

  { A list of integer}
  TCustomIntegerList = class(TList)
  protected
    function Get(Index: Integer): NativeInt; virtual;
    procedure Put(Index: Integer; const Value: NativeInt);
    procedure Inst(Index : Integer; Num: NativeInt);
  public
    function GetText : string;
    function Add(Num: NativeInt): Integer; virtual;
    procedure AddIntegers (IntegerList : TCustomIntegerList);
    function First: NativeInt;
    function IndexOf(Num: NativeInt): Integer; virtual;
    function Last: NativeInt;
    function Remove(Num: NativeInt): Integer;
    procedure Pack(RemoveNum: NativeInt);
    procedure Sort; virtual;
    procedure SaveToFile (const FileName : string);
    procedure SaveToStream (Stream: TStream);

    property Items[Index: Integer]: NativeInt read Get write Put; default;
    property Nums[Index: Integer]: NativeInt read Get write Put;
  end;

  TIntegerList = class (TCustomIntegerList)
  public
    procedure Insert(Index: integer; Num: NativeInt);
  end;


  TSortedIntegerList = class (TCustomIntegerList)
  private
    FSorted : boolean;
    function InternalGet(Index: Integer): NativeInt;
  protected
    function Get(Index: Integer): NativeInt; override;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function IndexOf(Num: NativeInt): Integer; override;
    procedure Sort; override;
    procedure AddSorted(Num: NativeInt);
  end;


implementation

uses
  SysUtils;

function SortBySmall(Item1, Item2: Pointer): Integer;
begin
  if NativeInt( Item1 )>NativeInt( Item2 ) then Result := 1
    else if NativeInt( Item1 )<NativeInt( Item2 ) then Result := -1
      else Result := 0;
end;

function TCustomIntegerList.Get(Index: Integer): NativeInt;
begin
  Result := NativeInt( inherited Get( Index ) );
end;

function TCustomIntegerList.GetText: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Self.Count - 1 do
  begin
    if i = 0 then
      Result := IntToStr(Self.Nums[i])
    else
      Result := Result + '#' + sLineBreak + IntToStr(Self.Nums[i]);
  end;
end;

procedure TCustomIntegerList.Put(Index: Integer; const Value: NativeInt);
begin
  inherited Put( Index, Pointer( Value ) );
end;

function TCustomIntegerList.Add(Num: NativeInt): Integer;
begin
  Result := inherited Add( Pointer( Num ) );
end;

function TCustomIntegerList.First: NativeInt;
begin
  Result := Get( 0 );
end;

function TCustomIntegerList.IndexOf(Num: NativeInt): Integer;
begin
  Result := inherited IndexOf( Pointer( Num ) );
end;

procedure TCustomIntegerList.Inst(Index: Integer; Num : NativeInt);
begin
  inherited Insert( Index, Pointer( Num ) );
end;

function TCustomIntegerList.Last: NativeInt;
begin
  Result := Get( Count-1 );
end;

function TCustomIntegerList.Remove(Num: NativeInt): Integer;
begin
   Result := inherited Remove( Pointer( Num ) );
end;

procedure TCustomIntegerList.Pack(RemoveNum: NativeInt);
var
  idx : Integer;
begin
  idx := 0;
  while idx<Count do begin
    if Get( idx )=RemoveNum then Delete( idx )
      else inc( idx );
  end;
end;

procedure TCustomIntegerList.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomIntegerList.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := GetText;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TCustomIntegerList.Sort;
begin
  inherited Sort({$IFDEF FPC}@{$ENDIF}SortBySmall );
end;

procedure TCustomIntegerList.AddIntegers(IntegerList: TCustomIntegerList);
var
  ICounter : integer;
begin
  for ICounter := 0 to IntegerList.Count -1 do
  begin
    Add(IntegerList.Items[ICounter]);
  end;
end;

{ TSortedIntegerList }

procedure TSortedIntegerList.AddSorted(Num: NativeInt);
  procedure InsertInto (aStart, aEnd : integer);
  var
    tempValue, midIndex : integer;
  begin
    tempValue := (aEnd - aStart);
    if tempValue <= 1 then
      Self.Inst(aStart + 1, Num)
    else
    begin
      MidIndex := aStart + (tempValue div 2);
      if InternalGet(MidIndex) < Num then
        InsertInto(MidIndex, aEnd)
      else
        InsertInto(aStart, MidIndex);
    end;
  end;
var
  tempCount : integer;
begin
  Self.Sort;
  tempCount := Self.Count;
  if tempCount = 0 then
    inherited Add(Num)
  else
  begin
    if Num < Self.InternalGet(0) then
      Self.Inst(0, Num)
    else
    if Num > Self.InternalGet(tempCount-1) then
      inherited Add(Num)
    else
    begin
      InsertInto(0, tempCount - 1);
    end;
  end;
  Self.FSorted := true;
end;


function TSortedIntegerList.Get(Index: Integer): NativeInt;
begin
  if not FSorted then
    Self.Sort;
  Result := InternalGet(Index);
end;

function TSortedIntegerList.IndexOf(Num: NativeInt): Integer;
  function MidIndexOf(aStart, aEnd : integer) : integer;
  var
    tempValue, midIndex : integer;
  begin
    Result := -1;
    tempValue := (aEnd - aStart);
    if tempValue <= 1 then
    begin
      if Num = Self.Get(aStart) then
        Result := aStart
      else
      if Num = Self.Get(aEnd) then
        Result := aEnd;
    end
    else
    begin
      MidIndex := aStart + (tempValue div 2);
      if Get(MidIndex) < Num then
        Result := MidIndexOf(MidIndex, aEnd)
      else
        Result := MidIndexOf(aStart, MidIndex);
    end;
  end;
var
  tempLast : integer;
begin
  if not FSorted then
    Self.Sort;

  Result := -1;

  if Self.Count = 0 then
    exit;

  if Num < Self.Get(0) then
    exit
  else
  begin
    tempLast := Count - 1;
    if Num > Self.Get(tempLast) then
      exit
    else
      Result := MidIndexOf(0, tempLast);
  end;
end;

function TSortedIntegerList.InternalGet(Index: Integer): NativeInt;
begin
Result := inherited Get(Index);
end;

procedure TSortedIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  FSorted := false;
end;

procedure TSortedIntegerList.Sort;
begin
  if FSorted then
    exit;
  inherited;
  FSorted := true;
end;

{ TIntegerList }

procedure TIntegerList.Insert(Index: Integer; Num: NativeInt);
begin
  Inst(Index, Num);
end;

end.
