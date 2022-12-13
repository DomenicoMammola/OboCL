unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mThreads, mProgress, mProgressClasses,
  ProgressGUIDesktop;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCyclesSheep, FCyclesCow, FCyclesDonkey : integer;

    procedure DoSheepJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure DoCowJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure DoDonkeyJob(aProgress: ImProgress; aData: TObject; aJobResult : TJobResult);
    procedure OnEndJob(const aJobsResult : TJobResults);
  public

  end;

var
  Form1: TForm1;

implementation


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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TForm1.DoSheepJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesSheep < 100 do
  begin
    aProgress.Notify('Sheep #' + IntToStr(FCyclesSheep));
    Sleep (100);
    inc(FCyclesSheep);
  end;
  aProgress.Notify('Sheep done!');
end;

procedure TForm1.DoCowJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesCow < 100 do
  begin
    aProgress.Notify('Cow #' + IntToStr(FCyclesCow));
    Sleep (Random(300) + 1);
    inc(FCyclesCow);
  end;
  aProgress.Notify('Cow done!');
end;

procedure TForm1.DoDonkeyJob(aProgress: ImProgress; aData: TObject; aJobResult: TJobResult);
begin
  while FCyclesDonkey < 100 do
  begin
    aProgress.Notify('Donkey #' + IntToStr(FCyclesDonkey));
    Sleep (150);
    inc(FCyclesDonkey);
  end;
  aProgress.Notify('Donkey done!');
end;

procedure TForm1.OnEndJob(const aJobsResult: TJobResults);
begin
  ShowMessage('All done and well done!');
end;

end.

