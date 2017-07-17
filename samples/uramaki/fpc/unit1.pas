unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls,
  UramakiBase, UramakiDesktopBase;

type

  { TSampleUramakiRoll }

  TSampleUramakiRoll = class (TUramakiRoll)
  private
    FList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetId : string;
    function GetMyId : string; override;
    function GetDescription : string; override;

    property List : TStringList read FList;
  end;

  { TSimpleTransformer }

  TSimpleTransformer = class (TUramakiTransformer)
  public
    function GetMyId : String; override;
    function GetDescription : String; override;

    function GetInputUramakiId : String; override;
    function GetOutputUramakiId : String; override;

    function Transform (aInput : TUramakiRoll; aContext : TUramakiTransformationContext) : TUramakiRoll; override;
  end;

  { TStupidPlate }

  TStupidPlate = class (TUramakiPlate)
  private
    FMemo : TMemo;
  public
    function GetUramakiRoll(const aUramakiId: String) : TUramakiRoll; override;
    procedure GetAvailableUramakiRolls(aUramakiRollIdList: TStringList); override;
    procedure Build;
  end;

  { TStupidPublisher }

  TStupidPublisher = class (TUramakiPublisher)
  public
    function GetMyId : String; override;
    function GetDescription : String; override;

    function GetInputUramakiId : String; override;

    function CreatePlate (aParent : TPanel) : TUramakiPlate; override;

    procedure Publish(aInput : TUramakiRoll; aPlate : TUramakiPlate; aContext : TUramakiPublicationContext); override;
  end;

implementation

{ TStupidPublisher }

function TStupidPublisher.GetMyId: String;
begin
  Result := 'stupid-memo';
end;

function TStupidPublisher.GetDescription: String;
begin
  Result := 'a stupid memo';
end;

function TStupidPublisher.GetInputUramakiId: String;
begin
  Result := TSampleUramakiRoll.GetId;
end;

function TStupidPublisher.CreatePlate(aParent : TPanel): TUramakiPlate;
begin
  Result := TStupidPlate.Create(aParent);
  Result.Parent := aParent;
  (Result as TStupidPlate).Build;
end;

procedure TStupidPublisher.Publish(aInput: TUramakiRoll; aPlate: TUramakiPlate; aContext: TUramakiPublicationContext);
begin
  (aPlate as TStupidPlate).FMemo.Lines.AddStrings((aInput as TSampleUramakiRoll).List);
end;

{ TStupidPlate }


function TStupidPlate.GetUramakiRoll(const aUramakiId: String): TUramakiRoll;
var
  tmp : TSampleUramakiRoll;
begin
  Result := nil;
  if aUramakiId = TSampleUramakiRoll.GetId then
  begin
    tmp := TSampleUramakiRoll.Create;
    tmp.List.Add(FMemo.Lines.Text);
    Result := tmp;
  end;
end;

procedure TStupidPlate.GetAvailableUramakiRolls(aUramakiRollIdList: TStringList);
begin
  aUramakiRollIdList.Add(TSampleUramakiRoll.GetId);
end;

procedure TStupidPlate.Build;
begin
  FMemo:= TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align:= alClient;
  FMemo.BorderStyle:= bsNone;
end;


{ TSampleUramakiRoll }

constructor TSampleUramakiRoll.Create;
begin
  FList := TStringList.Create;
end;

destructor TSampleUramakiRoll.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

class function TSampleUramakiRoll.GetId: string;
begin
  Result := 'sample1';
end;

function TSampleUramakiRoll.GetMyId: string;
begin
  Result := TSampleUramakiRoll.GetId;
end;

function TSampleUramakiRoll.GetDescription: string;
begin
  Result := 'a string list';
end;

{ TSimpleTransformer }

function TSimpleTransformer.GetMyId: String;
begin
  Result := 'simple_transformer';
end;

function TSimpleTransformer.GetDescription: String;
begin
  Result := 'sample data';
end;

function TSimpleTransformer.GetInputUramakiId: String;
begin
  Result := NULL_URAMAKI_ID;
end;

function TSimpleTransformer.GetOutputUramakiId: String;
begin
  Result := TSampleUramakiRoll.GetId;
end;

function TSimpleTransformer.Transform(aInput: TUramakiRoll; aContext: TUramakiTransformationContext) : TUramakiRoll;
var
  tmp : TSampleUramakiRoll;
begin
  tmp := TSampleUramakiRoll.Create;
  Result := tmp;
  tmp.List.Add('one');
  tmp.List.Add('two');
  tmp.List.Add('three');
end;

end.

