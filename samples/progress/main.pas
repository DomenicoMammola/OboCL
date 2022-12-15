unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Contnrs,
  mThreads, mProgress, mProgressClasses,
  ProgressGUIDesktop;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    const MANY_PETS = 100;
  private
    FCyclesSheep, FCyclesCow, FCyclesDonkey : integer;
    FCyclesPets : array [1..MANY_PETS] of integer;
    FGarbage : TObjectList;

    procedure DoSheepJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure DoCowJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure DoDonkeyJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure DoPetJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure OnEndJob(const aJobsResult : TJobResults);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  mBaseClassesAsObjects;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FCyclesSheep := 0;
  with BatchExecutor.QueueJob do
  begin
    DoJobProcedure := @DoSheepJob;
    Description:= 'Sample job';
    TrapExceptions:= false;
  end;
  BatchExecutor.Execute(Self, @OnEndJob);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FCyclesSheep := 0;
  FCyclesCow:= 0;
  FCyclesDonkey:= 0;
  with BatchExecutor.QueueJob do
  begin
    DoJobProcedure := @DoSheepJob;
    Description:= 'Sheep job';
    TrapExceptions:= false;
  end;
  with BatchExecutor.QueueJob do
  begin
    DoJobProcedure := @DoCowJob;
    Description:= 'Cow job';
    TrapExceptions:= false;
  end;
  with BatchExecutor.QueueJob do
  begin
    DoJobProcedure := @DoDonkeyJob;
    Description:= 'Donkey job';
    TrapExceptions:= false;
  end;
  BatchExecutor.Execute(Self, @OnEndJob);

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : integer;
  shell : TIntegerObject;
begin
  for i := 1 to MANY_PETS do
  begin
    FCyclesPets[i] := 0;
    with BatchExecutor.QueueJob do
    begin
      DoJobProcedure:= @DoPetJob;
      Description := 'Pet #' + IntToStr(i);
      TrapExceptions:= false;
      shell := TIntegerObject.Create(i);
      Data := shell;
      FGarbage.Add(Data);
    end;
  end;

  BatchExecutor.Execute(Self, @OnEndJob);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FGarbage := TObjectList.Create(true);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGarbage.Free;
end;

procedure TForm1.DoSheepJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesSheep < 100 do
  begin
    aProgress.Notify('Sheep jumps ' + IntToStr(FCyclesSheep) + ' times');
    Sleep (100);
    inc(FCyclesSheep);
  end;
  aProgress.Notify('Sheep done!');
end;

procedure TForm1.DoCowJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesCow < 100 do
  begin
    aProgress.Notify('Cow jumps ' + IntToStr(FCyclesCow) + ' times');
    Sleep (Random(300) + 1);
    inc(FCyclesCow);
  end;
  aProgress.Notify('Cow done!');
end;

procedure TForm1.DoDonkeyJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesDonkey < 100 do
  begin
    aProgress.Notify('Donkey jumps ' + IntToStr(FCyclesDonkey) + ' times');
    Sleep (150);
    inc(FCyclesDonkey);
  end;
  aProgress.Notify('Donkey done!');
end;

procedure TForm1.DoPetJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
var
  shell : TIntegerObject;
begin
  shell := aData as TIntegerObject;
  while FCyclesPets[shell.Value] < 100 do
  begin
    aProgress.Notify('Puppy #' + IntToStr(shell.Value) + ' jumps ' + IntToStr(FCyclesPets[shell.Value]) + ' times');
    Sleep (Random(300) + 1);
    inc(FCyclesPets[shell.Value]);
  end;
  aProgress.Notify('Puppy #' + IntToStr(shell.Value) + ' done!');
end;

procedure TForm1.OnEndJob(const aJobsResult: TJobResults);
begin
  ShowMessage('All done and well done!');
end;

end.

