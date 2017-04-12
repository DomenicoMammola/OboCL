// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLog;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Contnrs, SyncObjs;

type

  TmLogMessageLevel = (mlDebug, mlInfo, mlWarning, mlError);

  TmLogMessage = class
  strict private
    FMessage : string;
    FContext : string;
    FLevel : TmLogMessageLevel;
    FDateTime : TDateTime;
  private
    procedure SetMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
    procedure CopyFrom (aMessage : TmLogMessage);

    property Message : string read FMessage;
    property Context : string read FContext;
    property Level : TmLogMessageLevel read FLevel;
    property DateTime : TDateTime read FDateTime;
  end;

  TmLogMessageList = class
  strict private
    FList : TObjectList;
    FCriticalSection : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
    function PullMessage: TmLogMessage;
  end;

  TmLogManager = class;

  TmLog = class
  private
    FContext : string;
    FLogManager : TmLogManager;
  public
    procedure Debug(aMessage : string);
    procedure Info(aMessage : string);
    procedure Warning(aMessage : string);
    procedure Error(aMessage : string);
  end;

  TmLogPublisher = class
  private
    FActive : boolean;
  protected
    function GetFormattedString (aContext, aLevel, aMessage : string; aDate : TDateTime) : string;
    procedure AfterCreate; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; virtual; abstract;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); virtual; abstract;
    function Level : TmLogMessageLevel; virtual; abstract;

    property Active : boolean read FActive write FActive;
  end;

  TmLogPublisherClass = class of TmLogPublisher;

  TmLogManager = class
  strict private
    FThread : TThread;
    FEndThreadEvent : TEvent;

    FVCLThread : TThread;
    FVCLEndThreadEvent : TEvent;
  private
    procedure PushMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
  public
    function AddLog (loggerContext : string): TmLog;
    procedure AddPublisher(aPublisher:TmLogPublisher);
    procedure RemovePublisher(aPublisher:TmLogPublisher);

    constructor Create;
    destructor Destroy; override;
  end;

  function logManager : TmLogManager;
  function logMessageLevelToStr (aLevel : TmLogMessageLevel) : string;

implementation

uses
  SysUtils;

var
  InternalLogManager : TmLogManager;

type
  TmLogPublisherThread = class (TThread)
  strict private
    FStartEvent : TEvent;
    FEndEvent : TEvent;
    FLogManager : TmLogManager;
    FMessages : TmLogMessageList;
    FPublishersCriticalSection : TCriticalSection;
    FPublishers : TObjectList;

    FCurrentPublisher : TmLogPublisher;
    FCurrentMsg : TmLogMessage;

    procedure PublishOnMainThread;
    function LevelsAreCompatible(levelOfMessage, levelOfPublisher : TmLogMessageLevel) : boolean;
  protected
    procedure Execute; override;
  public
    constructor Create (aEndEvent : TEvent; aLogManager : TmLogManager);
    destructor Destroy; override;
    procedure AddPublisher (aPublisher : TmLogPublisher);
    procedure RemovePublisher(aPublisher : TmLogPublisher);

    property StartEvent : TEvent read FStartEvent;
    property EndEvent : TEvent read FEndEvent;
    property Messages : TmLogMessageList read FMessages;
  end;

  TmDeleteLogPublisherThread = class (TThread)
  private
    FPublisher : TmLogPublisher;
    procedure DeletePublisher;
  protected
    procedure Execute; override;
  public
    constructor Create (aPublisher : TmLogPublisher);
  end;

{ TmLogManager }


procedure TmLogManager.PushMessage(aLevel : TmLogMessageLevel; aContext, aMessage : string);
begin
  (FThread as TmLogPublisherThread).Messages.PushMessage(aLevel, aContext, aMessage);
  (FThread as TmLogPublisherThread).StartEvent.SetEvent;

  (FVCLThread as TmLogPublisherThread).Messages.PushMessage(aLevel, aContext, aMessage);
  (FVCLThread as TmLogPublisherThread).StartEvent.SetEvent;
end;



procedure TmLogManager.RemovePublisher(aPublisher: TmLogPublisher);
begin
  if aPublisher.ActInsideMainThread then
    (FVCLThread as TmLogPublisherThread).RemovePublisher(aPublisher)
  else
    (FThread as TmLogPublisherThread).RemovePublisher(aPublisher);
end;

constructor TmLogManager.Create;
begin
  FEndThreadEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, ''){$ENDIF};
  FThread := TmLogPublisherThread.Create(FEndThreadEvent, Self);

  FVCLEndThreadEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, ''){$ENDIF};
  FVCLThread := TmLogPublisherThread.Create(FVCLEndThreadEvent, Self);
end;

procedure TmLogManager.AddPublisher(aPublisher:TmLogPublisher);
begin
  if aPublisher.ActInsideMainThread then
    (FVCLThread as TmLogPublisherThread).AddPublisher(aPublisher)
  else
    (FThread as TmLogPublisherThread).AddPublisher(aPublisher);
end;

function TmLogManager.AddLog(loggerContext: string): TmLog;
begin
  Result := TmLog.Create;
  Result.FContext := loggerContext;
  Result.FLogManager := LogManager;
end;


destructor TmLogManager.Destroy;
begin
  (FThread as TmLogPublisherThread).Terminate;
  (FThread as TmLogPublisherThread).StartEvent.SetEvent;
  FEndThreadEvent.WaitFor(INFINITE);
  FEndThreadEvent.Free;

  (FVCLThread as TmLogPublisherThread).Terminate;
  (FVCLThread as TmLogPublisherThread).StartEvent.SetEvent;
  FVCLEndThreadEvent.WaitFor(INFINITE);
  FVCLEndThreadEvent.Free;

  inherited;
end;



{ TmLog }

procedure TmLog.Debug(aMessage: string);
begin
  FLogManager.PushMessage(mlDebug, Self.FContext, aMessage);
end;

procedure TmLog.Error(aMessage: string);
begin
  FLogManager.PushMessage(mlError, Self.FContext, aMessage);
end;

procedure TmLog.Info(aMessage: string);
begin
  FLogManager.PushMessage(mlInfo, Self.FContext, aMessage);
end;

procedure TmLog.Warning(aMessage: string);
begin
  FLogManager.PushMessage(mlWarning, Self.FContext, aMessage);
end;

{ TmLogPublisherThread }

procedure TmLogPublisherThread.AddPublisher(aPublisher: TmLogPublisher);
begin
  FPublishersCriticalSection.Acquire;
  try
    FPublishers.Add(aPublisher);
  finally
    FPublishersCriticalSection.Leave;
  end;
end;

constructor TmLogPublisherThread.Create(aEndEvent : TEvent; aLogManager : TmLogManager);
begin
  inherited Create(false);
  FStartEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, ''){$ENDIF};
  FEndEvent := aEndEvent;
  FLogManager := aLogManager;
  FMessages := TmLogMessageList.Create;
  FPublishers := TObjectList.Create(false);
  FPublishersCriticalSection := TCriticalSection.Create;
end;

destructor TmLogPublisherThread.Destroy;
begin
  FStartEvent.Free;
  FMessages.Free;
  FPublishers.Free;
  FPublishersCriticalSection.Free;
  inherited;
end;

procedure TmLogPublisherThread.Execute;
var
  i : integer;
begin
  FStartEvent.WaitFor(INFINITE);
  FStartEvent.ResetEvent;

  if not Self.Terminated then
  begin
    try
      while not Self.Terminated do
      begin
        FCurrentMsg := FMessages.PullMessage;
        while (FCurrentMsg <> nil) do
        begin
          if not Self.Terminated then
          begin
            FPublishersCriticalSection.Acquire;
            try
              for i := 0 to FPublishers.Count - 1 do
              begin
                FCurrentPublisher := FPublishers.Items[i] as TmLogPublisher;
                if FCurrentPublisher.Active then
                begin
                  if LevelsAreCompatible(FCurrentMsg.Level, FCurrentPublisher.Level) then
                  begin
                    if FCurrentPublisher.ActInsideMainThread then
                      Self.Synchronize(PublishOnMainThread)
                    else
                      FCurrentPublisher.Publish(FCurrentMsg.Context, logMessageLevelToStr(FCurrentMsg.Level), FCurrentMsg.Message, FCurrentMsg.DateTime);
                  end;
                end;

              end;
            finally
              FPublishersCriticalSection.Leave;
            end;
            FCurrentMsg := FMessages.PullMessage;
          end
          else
            break;
        end;

        if not Self.Terminated then
          FStartEvent.WaitFor(INFINITE);
      end;
    except
      on e: Exception do
      begin
        FEndEvent.SetEvent;
        raise;
      end;
    end;
  end;

  FEndEvent.SetEvent;
end;

function TmLogPublisherThread.LevelsAreCompatible(levelOfMessage, levelOfPublisher: TmLogMessageLevel): boolean;
begin
  Result := integer(levelOfMessage) >= integer (levelOfPublisher);
end;

procedure TmLogPublisherThread.PublishOnMainThread;
begin
  if Assigned(FCurrentPublisher) and FCurrentPublisher.Active and Assigned(FCurrentMsg) and (not Self.Terminated) then
    FCurrentPublisher.Publish(FCurrentMsg.Context, logMessageLevelToStr(FCurrentMsg.Level), FCurrentMsg.Message, FCurrentMsg.DateTime);
end;

procedure TmLogPublisherThread.RemovePublisher(aPublisher: TmLogPublisher);
begin
  FPublishersCriticalSection.Acquire;
  try
    FPublishers.Remove(aPublisher);
  finally
    FPublishersCriticalSection.Leave;
  end;
end;

function logManager : TmLogManager;
begin
 if not Assigned(InternalLogManager) then
   InternalLogManager := TmLogManager.Create;
 Result := InternalLogManager;
end;

{ TmLogPublisher }


constructor TmLogPublisher.Create;
begin
  FActive := false;
  Self.AfterCreate;
end;

destructor TmLogPublisher.Destroy;
begin
  inherited;
end;

function TmLogPublisher.GetFormattedString(aContext, aLevel, aMessage: string; aDate: TDateTime): string;
begin
  Result := '[' + DateTimeToStr(aDate) + '] [' + aLevel + '] [' + aContext + '] ' + aMessage;
end;

{ TmLogMessage }


procedure TmLogMessage.CopyFrom(aMessage: TmLogMessage);
begin
  Self.FLevel := aMessage.FLevel;
  Self.FContext := aMessage.FContext;
  Self.FDateTime := aMessage.DateTime;
  Self.FMessage := aMessage.FMessage;
end;

procedure TmLogMessage.SetMessage(aLevel : TmLogMessageLevel; aContext, aMessage : string);
begin
  Self.FLevel := aLevel;
  Self.FContext := aContext;
  Self.FMessage := aMessage;
  Self.FDateTime := Now;
end;


function logMessageLevelToStr (aLevel : TmLogMessageLevel) : string;
begin
  case aLevel of
    mlDebug: Result := 'DEBUG';
    mlInfo: Result := 'INFO';
    mlWarning: Result := 'WARNING';
    mlError : Result := 'ERROR'
  else
    Result := 'UNKNOWN';
  end;
end;

{ TmDeleteLogPublisherThread }

constructor TmDeleteLogPublisherThread.Create(aPublisher: TmLogPublisher);
begin
  inherited Create(false);
  FPublisher := aPublisher;
  Self.FreeOnTerminate := true;
end;

procedure TmDeleteLogPublisherThread.DeletePublisher;
begin
end;

procedure TmDeleteLogPublisherThread.Execute;
begin
(*  FPublisher.FCriticalSection.Acquire;
  logManager.FPublishersCriticalSection.Acquire;
  try
    Self.Synchronize(DeletePublisher);
  finally
    logManager.FPublishersCriticalSection.Free;
  end;*)
end;

{ TmLogMessageList }

constructor TmLogMessageList.Create;
begin
  FList := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TmLogMessageList.Destroy;
begin
  FList.Free;
  FCriticalSection.Free;
  inherited;
end;

function TmLogMessageList.PullMessage: TmLogMessage;
var
  msg : TmLogMessage;
begin
  FCriticalSection.Acquire;
  try
    if FList.Count > 0 then
    begin
      msg := FList.Items[0] as TmLogMessage;
      Result := TmLogMessage.Create;
      Result.CopyFrom(msg);
      FList.Delete(0);
    end
    else
      Result := nil;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TmLogMessageList.PushMessage(aLevel: TmLogMessageLevel; aContext, aMessage: string);
var
  msg : TmLogMessage;
begin
  FCriticalSection.Acquire;
  try
    msg := TmLogMessage.Create;
    msg.SetMessage(aLevel, aContext, aMessage);
    FList.Add(msg);
  finally
    FCriticalSection.Leave;
  end;
end;

initialization

finalization

if Assigned(InternalLogManager) then
  FreeAndNil(InternalLogManager);

end.
