// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLogPublishers;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Forms, StdCtrls, Controls, SysUtils,
  mLog;

type
  TmMemoPublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel : TmLogMessageLevel;
    FForm : TForm;
    FMemo : TMemo;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
  end;

  TmFilePublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel : TmLogMessageLevel;
    FFileName : TFileName;
    FLogFile: Text;
    FFileAssigned : boolean;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
    property FileName : TFileName read FFileName write FFileName;
  end;

implementation

{ TmMemoPublishe }

function TmMemoPublisher.ActInsideMainThread: boolean;
begin
  Result := true;
end;

procedure TmMemoPublisher.AfterCreate;
begin
  FCurrentLevel := mlDebug;
  FForm := nil;
end;

destructor TmMemoPublisher.Destroy;
begin
  FForm.Free;
  inherited;
end;

function TmMemoPublisher.Level: TmLogMessageLevel;
begin
  Result := FCurrentLevel;
end;

procedure TmMemoPublisher.Publish(aContext, aLevel, aMessage : string; aDate : TDateTime);
begin
  if FForm = nil then
  begin
    FForm := TForm.Create(nil);
    FMemo := TMemo.Create(FForm);
    FMemo.Parent := FForm;
    FMemo.Align := alClient;
    FForm.Show;
  end;
  FMemo.Lines.Append(Self.GetFormattedString(aContext, aLevel, aMessage, aDate));
end;

{ TmFilePublisher }

function TmFilePublisher.ActInsideMainThread: boolean;
begin
  Result := false;
end;

procedure TmFilePublisher.AfterCreate;
begin
  FFileName := '';
  FFileAssigned := false;
end;

destructor TmFilePublisher.Destroy;
begin
  if FFileAssigned then
    Flush(FLogFile);
  inherited;
end;

function TmFilePublisher.Level: TmLogMessageLevel;
begin
  Result := FCurrentLevel;
end;

procedure TmFilePublisher.Publish(aContext, aLevel, aMessage: string; aDate: TDateTime);
begin
  if FFileName <> '' then
  begin
    if not FFileAssigned then
    begin
      Assign(FLogFile, FFileName);
      if FileExists(FFileName) then
        Append(FLogFile)
      else
        Rewrite(FLogFile);
      FFileAssigned := true;
    end;
    WriteLn(FLogFile, Self.GetFormattedString(aContext, aLevel, aMessage, aDate));
    Flush(FLogFile);
  end;
end;

end.
