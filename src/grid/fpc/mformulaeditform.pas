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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls, DB, Contnrs,
  SynEdit, SynCompletion,
  Types;

resourcestring
  SCaptionFormulaEditForm = 'Edit formula';

type
  { TFormulaEditForm }

  TFormulaEditForm = class(TForm)
    BottomPanel: TPanel;
    CancelBtn: TBitBtn;
    LBHelp: TListBox;
    OkBtn: TBitBtn;
    PageControl: TPageControl;
    Splitter1: TSplitter;
    TSEditFormula: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBHelpDblClick(Sender: TObject);
    procedure LBHelpDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OkBtnClick(Sender: TObject);
  strict private
    FEditor : TSynEdit;
    FEditorCompletion: TSynCompletion;
    FFunctionsList : TStringList;
    FGarbage : TObjectList;
    procedure OnEditorCompletionExecute(Sender: TObject);
    procedure OnEditorCompletionSearchPosition(var APosition: integer);
  public
    procedure Init (const aFormula : String; const aFields: TStringList);
    function GetFormula : String;
  end;


implementation

uses
  Math,
  mFormSetup, KAParser, KAParserHelp, mMathUtility;

const CATEGORY_CONSTANT_COLOR = clSkyBlue;
const CATEGORY_MATHEMATICAL_COLOR = clGreen;
const CATEGORY_LOGICAL_COLOR = clPurple;
const CATEGORY_TEXT_COLOR = clRed;
const CATEGORY_GENERAL_COLOR = clGray;
const CATEGORY_DATETIME_COLOR = clBlue;

type
  TKAParserSintaxTokenHelpShell = class
  public
    help : TKAParserSintaxTokenHelp;
  end;

{$R *.lfm}

{ TFormulaEditForm }

procedure TFormulaEditForm.FormCreate(Sender: TObject);
begin
  Self.Caption := SCaptionFormulaEditForm;
  FEditor := TSynEdit.Create(TSEditFormula);
  FEditor.Parent := TSEditFormula;
  FEditor.Align:= alClient;
  FFunctionsList := TStringList.Create;
  FGarbage := TObjectList.Create(true);

  SetupFormAndCenter(Self);
end;

procedure TFormulaEditForm.FormDestroy(Sender: TObject);
begin
  FGarbage.Free;
  FFunctionsList.Free;
end;

procedure TFormulaEditForm.FormShow(Sender: TObject);
begin
  FEditor.SetFocus;
end;

procedure TFormulaEditForm.LBHelpDblClick(Sender: TObject);
var
  s : String;
begin
  if (LBHelp.ItemIndex >= 0) and (LBHelp.Items.Count > 0) then
  begin
    s := (LBHelp.Items.Objects[LBHelp.ItemIndex] as TKAParserSintaxTokenHelpShell).help.sintax;
    if (LBHelp.Items.Objects[LBHelp.ItemIndex] as TKAParserSintaxTokenHelpShell).help.numOfParameters <> '0' then
      s := s + ' (';
    FEditor.InsertTextAtCaret(s);
    FEditor.SetFocus;
  end;
end;

procedure TFormulaEditForm.LBHelpDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  sh : TKAParserSintaxTokenHelpShell;
  lst : TListBox;
  str : String;
  xcoord : integer;
  i, p : integer;
  cl : TColor;
begin
  lst := Control as TListBox;
  lst.Canvas.Brush.Color := lst.Color;
  lst.Canvas.FillRect(ARect);
  sh := lst.Items.Objects[Index] as TKAParserSintaxTokenHelpShell;
  if sh.help.category = TKAParserEditorHelp.CATEGORY_CONSTANT then
    cl := CATEGORY_CONSTANT_COLOR
  else if sh.help.category = TKAParserEditorHelp.CATEGORY_DATETIME then
    cl := CATEGORY_DATETIME_COLOR
  else if sh.help.category = TKAParserEditorHelp.CATEGORY_LOGICAL then
    cl := CATEGORY_LOGICAL_COLOR
  else if sh.help.category = TKAParserEditorHelp.CATEGORY_MATHEMATICAL then
    cl := CATEGORY_MATHEMATICAL_COLOR
  else if sh.help.category = TKAParserEditorHelp.CATEGORY_TEXT then
    cl := CATEGORY_TEXT_COLOR
  else
    cl := CATEGORY_GENERAL_COLOR;
  str := '[' + sh.help.category + ']';

  xcoord := ARect.Left + 2;
  lst.Canvas.Brush.Color := cl;
  lst.Canvas.Font.Color:= clWhite;
  lst.Canvas.TextOut(xcoord, ARect.Top, str);

  lst.Canvas.Brush.Color := lst.Color;
  xcoord := max(xcoord + 85, xcoord + lst.Canvas.TextWidth(str));
  str := sh.help.sintax;
  if sh.help.numOfParameters <> '0' then
  begin
    if sh.help.numOfParameters = '1' then
      str := str + '( )'
    else
    if RightStr(sh.help.numOfParameters, 1) = '+' then
    begin
      p := 0;
      if TryToConvertToInteger(LeftStr(sh.help.numOfParameters, Length(sh.help.numOfParameters) -1), p) then
      begin
        str := str + '( ';
        for i := 1 to p-1 do
          str := str + ', ';
        str := str + '...)';
      end;
    end
    else
    begin
      p := 0;
      if TryToConvertToInteger(sh.help.numOfParameters, p) then
      begin
        str := str + '( ';
        for i := 1 to p-1 do
          str := str + ', ';
        str := str + ')';
      end;
    end;
  end;

  lst.Canvas.Font.Color:= lst.Font.Color;
  lst.Canvas.Font.Style:= [fsBold];
  lst.Canvas.TextOut(xcoord, ARect.Top, str);

  xcoord := xcoord + lst.Canvas.TextWidth(str) + 5;
  str := sh.help.description;
  lst.Canvas.Font.Style:= [fsItalic];
  lst.Canvas.TextOut(xcoord, ARect.Top, str);

  //str := '[' + sh.help.category + '] ' + sh.help.sintax + ' : ' + sh.help.description;
  //lst.Canvas.TextOut(ARect.Left + 2, ARect.Top, str);
end;

procedure TFormulaEditForm.OkBtnClick(Sender: TObject);
begin
  Self.ModalResult:= mrOk;
end;

procedure TFormulaEditForm.OnEditorCompletionExecute(Sender: TObject);
var
  i : integer;

  procedure Add(s: String);
  var
    h : TKAParserSintaxTokenHelp;
    sh : TKAParserSintaxTokenHelpShell;
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
    begin
      FEditorCompletion.ItemList.Add(s);
      h := KAParserHelp.TKAParserEditorHelp.GetHelp(s);
      if h.description <> '' then
      begin
        sh := TKAParserSintaxTokenHelpShell.Create;
        sh.help := h;
        FGarbage.Add(sh);
        LBHelp.AddItem(h.description, sh);
      end;
    end;
  end;

begin
  FEditorCompletion.ItemList.Clear;
  LBHelp.Clear;
  FGarbage.Clear;
  for i := 0 to FFunctionsList.Count - 1 do
    Add(FFunctionsList.Strings[i]);
end;

procedure TFormulaEditForm.OnEditorCompletionSearchPosition(var APosition: integer);
var
  i : integer;

  procedure Add(s: String);
  var
    h : TKAParserSintaxTokenHelp;
    sh : TKAParserSintaxTokenHelpShell;
  begin
    if (FEditorCompletion.CurrentString = '') or (pos(lowercase(FEditorCompletion.CurrentString), lowercase(s)) = 1) then
    begin
      FEditorCompletion.ItemList.Add(s);
      h := KAParserHelp.TKAParserEditorHelp.GetHelp(s);
      if h.description <> '' then
      begin
        sh := TKAParserSintaxTokenHelpShell.Create;
        sh.help := h;
        FGarbage.Add(sh);
        LBHelp.AddItem(h.description, sh);
      end;
    end;
  end;
begin
  FEditorCompletion.ItemList.Clear;
  LBHelp.Clear;
  FGarbage.Clear;
  for i := 0 to FFunctionsList.Count - 1 do
    Add(FFunctionsList.Strings[i]);

  if FEditorCompletion.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TFormulaEditForm.Init(const aFormula : String; const aFields: TStringList);
var
  i : integer;
  h : TKAParserSintaxTokenHelp;
  sh : TKAParserSintaxTokenHelpShell;
begin
  GetFunctionsList(FFunctionsList, false);
  for i := 0 to FFunctionsList.Count - 1 do
  begin
    h := KAParserHelp.TKAParserEditorHelp.GetHelp(FFunctionsList.Strings[i]);
    if h.description <> '' then
    begin
      sh := TKAParserSintaxTokenHelpShell.Create;
      sh.help := h;
      FGarbage.Add(sh);
      LBHelp.AddItem(h.description, sh);
    end;
  end;

  FFunctionsList.AddStrings(aFields);

  FEditorCompletion:= TSynCompletion.Create(Self);
  FEditorCompletion.Editor := FEditor;
  FEditorCompletion.OnExecute:= @OnEditorCompletionExecute;
  FEditorCompletion.OnSearchPosition:= @OnEditorCompletionSearchPosition;
  FEditorCompletion.LinesInWindow:= 12;

  FEditor.Text := aFormula;

  FEditor.CaretX:= 0;
  FEditor.CaretY:= 0;
end;

function TFormulaEditForm.GetFormula: String;
begin
  Result := Trim(FEditor.Text);
end;

end.

