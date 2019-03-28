// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFormulaEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, DB,
  SynEdit, SynCompletion,
  mFields;

resourcestring
  SCaptionFormulaEditForm = 'Edit formula';

type
  { TFormulaEditForm }

  TFormulaEditForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    PageControl: TPageControl;
    TSEditFormula: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  strict private
    FEditor : TSynEdit;
    FEditorCompletion: TSynCompletion;
    FFunctionsList : TStringList;
    procedure OnEditorCompletionExecute(Sender: TObject);
    procedure OnEditorCompletionSearchPosition(var APosition: integer);
  public
    procedure Init (const aFormula : String; const aFields: TStringList);
    function GetFormula : String;
  end;


implementation

uses
  mFormSetup, KAParser;

{$R *.lfm}

{ TFormulaEditForm }

procedure TFormulaEditForm.FormCreate(Sender: TObject);
begin
  Self.Caption := SCaptionFormulaEditForm;
  FEditor := TSynEdit.Create(TSEditFormula);
  FEditor.Parent := TSEditFormula;
  FEditor.Align:= alClient;
  FFunctionsList := TStringList.Create;

  SetupFormAndCenter(Self);
end;

procedure TFormulaEditForm.FormDestroy(Sender: TObject);
begin
  FFunctionsList.Free;
end;

procedure TFormulaEditForm.FormShow(Sender: TObject);
begin
  FEditor.SetFocus;
end;

procedure TFormulaEditForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TFormulaEditForm.OnEditorCompletionExecute(Sender: TObject);
var
  i : integer;

  procedure Add(s: String);
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
      FEditorCompletion.ItemList.Add(s);
  end;

begin
  FEditorCompletion.ItemList.Clear;
  for i := 0 to FFunctionsList.Count - 1 do
    Add(FFunctionsList.Strings[i]);
end;

procedure TFormulaEditForm.OnEditorCompletionSearchPosition(var APosition: integer);
var
  i : integer;

  procedure Add(s: String);
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
      FEditorCompletion.ItemList.Add(s);
  end;
begin
  FEditorCompletion.ItemList.Clear;
  for i := 0 to FFunctionsList.Count - 1 do
    Add(FFunctionsList.Strings[i]);

  if FEditorCompletion.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TFormulaEditForm.Init(const aFormula : String; const aFields: TStringList);
begin
  GetFunctionsList(FFunctionsList, false);
  FFunctionsList.AddStrings(aFields);

  FEditorCompletion:= TSynCompletion.Create(Self);
  FEditorCompletion.Editor := FEditor;
  //FEditorCompletion.EndOfTokenChr:= '()[].';
  FEditorCompletion.OnExecute:= @OnEditorCompletionExecute;
  FEditorCompletion.OnSearchPosition:= @OnEditorCompletionSearchPosition;
  FEditorCompletion.LinesInWindow:= 12;

  FEditor.Text := aFormula;

  //FEditor.SetFocus;
  FEditor.CaretX:= 0;
  FEditor.CaretY:= 0;
end;

function TFormulaEditForm.GetFormula: String;
begin
  Result := Trim(FEditor.Text);
end;

end.

