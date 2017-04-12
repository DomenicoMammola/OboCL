unit TestLogger;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  {$IFDEF FPC}fpcunit, testutils, testregistry,
  {$ELSE}TestFramework,{$ENDIF}
  Classes, mLog, mLogPublishers;

type

  TTestLogger = class(TTestCase)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleLog;
    procedure TestDeactiveActive;
    procedure TestFileLog;
  end;


implementation

uses
  SysUtils, Dialogs, Forms;

var
  FMemoPublisher : TmMemoPublisher;
  FFilePublisher : TmFilePublisher;

procedure TTestLogger.SetUp;
begin
  inherited;
end;

procedure TTestLogger.TearDown;
begin
  inherited;
end;


procedure TTestLogger.TestDeactiveActive;
var
  tmp : TmLog;
begin
//  logManager.RemovePublisher(FMemoPublisher);
  FMemoPublisher.Active := false;
  tmp := logManager.AddLog('test1');
  try
    tmp.Info('not visible');
    FMemoPublisher.Active := true;
    tmp.Info('visible');
  finally
    tmp.Free;
  end;

end;

procedure TTestLogger.TestFileLog;
var
  tmp : TmLog;
begin
  if not Assigned(FFilePublisher) then
  begin
    FFilePublisher := TmFilePublisher.Create;
    logManager.AddPublisher(FFilePublisher);
    FFilePublisher.Active := true;
    FFilePublisher.FileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'log.txt';
  end;

  tmp := logManager.AddLog('test1');
  try
    tmp.Error('error1');
    tmp.Error('error2');
  finally
    tmp.Free;
  end;
end;

procedure TTestLogger.TestSimpleLog;
var
  tmp : TmLog;
begin
  FMemoPublisher.Active := true;
  tmp := logManager.AddLog('test1');
  try
    tmp.Debug('ciccio');
  finally
    tmp.Free;
  end;
end;

initialization
  Randomize;
  // Register any test cases with the test runner
  {$IFDEF FPC}
  RegisterTest(TTestLogger);
  {$ELSE}
  RegisterTest(TTestLogger.Suite);
  {$ENDIF}
  FMemoPublisher := TmMemoPublisher.Create;
  FMemoPublisher.Active := true;
  logManager.AddPublisher(FMemoPublisher);
  FFilePublisher := nil;

finalization
  FMemoPublisher.Free;
  if Assigned(FFilePublisher) then
    FFilePublisher.Free;


end.

