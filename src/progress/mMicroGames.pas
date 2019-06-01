// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// ************************************************************************
// This code was found in internet, in its original form, many time ago
// and then here modified.
//
// Unfortunately original source site was missing and googling was
// unsuccessful so, sorry, cannot put here any attribution.
// ************************************************************************

unit mMicroGames;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, Graphics,
  Buttons, StdCtrls,
  {$IFDEF WINDOWS} Windows, {$ELSE} Types, {$ENDIF}
  LclIntf, LMessages, LclType, InterfaceBase;

type

  TState = (StPanel, Scramble, Tetris, Arcanoid, Figures);
  TTSAKey = (Lft, Rgh, Drp, Rtt, Down, Up, Stp);
  TColorTAFShapes = (colorFixed, colorFixed2, colorOne, colorRandom);
  TFSquare = (Big, Small);

  TScrambler = class(TCustomPanel)
  private
    Timer1: TTimer;
    FOnTime, FOnOver, FOnMove, FOnPaint: TNotifyEvent;
    FFirstFocus, FStart, FPause: boolean;
    FScrambleKey: TTSAKey;
    FMoves: integer;
    str: string[2];
    x, y, he, wi: integer;
    FRowsColumns: word;
    But: array[1..8, 1..8] of TBitBtn;
    FScrambleKeyDown: word;
    {$ifdef windows}
    procedure WMNCHitTest(var Mes: TWMNCHitTest); message WM_NCHitTest;
    {$endif}
    procedure ButtonClick(Sender: TObject);
    procedure ButtonKeyDown(i, j: integer);
    procedure TimerOnTimer(Sender: TObject);
    procedure Time; dynamic;
    procedure Over; dynamic;
    procedure Move; dynamic;
    procedure Detect;
    procedure SLeft; virtual;
    procedure SRight; virtual;
    procedure SUp; virtual;
    procedure SDown; virtual;
    procedure SetStart(Value: boolean);
    procedure SetPause(Value: boolean);
    procedure SetScrambleKey(Value: TTSAKey);
    procedure SetScrambleKeyDown(Value: word);
    procedure ShowHide;
    procedure ShowHideKey(i, j: integer);
    procedure ButtonsInit; virtual;
    procedure ButtonsPos; virtual;
    procedure ButtonsColors; virtual;
  protected
    procedure Resize; override;
    procedure Paint; override;
  public
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property Color;
    property Canvas;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property Moves: integer read FMoves;
    property OnTime: TNotifyEvent read FOnTime write FOnTime;
    property OnOver: TNotifyEvent read FOnOver write FOnOver;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Start: boolean read FStart write SetStart;
    property FirstFocus: boolean read FFirstFocus;
    property Pause: boolean read FPause write SetPause;
    property RowsColumns: word read FRowsColumns write FRowsColumns;
    property ScrambleKey: TTSAKey read FScrambleKey write SetScrambleKey;
    property ScrambleKeyDown: word read FScrambleKeyDown write SetScrambleKeyDown;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TTetris = class(TCustomPanel)
  private
    Timer1, Timer2: TTimer;
    Panel1: TPanel;
    FColorF, FColorShapesPen, FOneColorShapes: TColor;
    FColorShapes: TColorTAFShapes;
    FWidthShapesPen: integer;
    FShapesShape: TShapeType;
    FShapesBrushStyle: TBrushStyle;
    FTetrisKeyDown: word;
    FDelay: integer;
    FStart, FPause, FNextSColor, FFirstFocus: boolean;
    FNextShape: word;
    FOnDelete, FOnOver, FOnContact, FOnTime, FOnPaint: TNotifyEvent;
    FTetrisKey: TTSAKey;
    x, he, wi: word;
    FRows: word;
    FColumns: word;
    Sh: array[1..48, 1..24] of TShape;
    procedure SetDelay(Value: integer);
    procedure SetStart(Value: boolean);
    procedure SetPause(Value: boolean);
    procedure SetColorF(Value: TColor);
    procedure SetColorShapes(Value: TColorTAFShapes);
    procedure SetOneColorShapes(Value: TColor);
    procedure SetColorShapesPen(Value: TColor);
    procedure SetWidthShapesPen(Value: integer);
    procedure SetShapesShape(Value: TShapeType);
    procedure SetShapesBrushStyle(Value: TBrushStyle);
    procedure SetNextShape(Value: word);
    procedure SetNextSColor(Value: boolean);
    procedure SetTetrisKey(Value: TTSAKey);
    procedure SetTetrisKeyDown(Value: word);
    {$ifdef windows}
    procedure WMNCHitTest(var Mes: TWMNCHitTest); message WM_NCHitTest;
    {$endif}
    procedure TimerOnTimer(Sender: TObject);
    procedure CreateShapes;
    procedure Left; virtual;
    procedure Right; virtual;
    procedure Rotate; virtual;
    procedure DropDown; virtual;
    procedure CreateShape(var Shape: TShape; i, j, r: shortint);
    procedure Timer2OnTimer(Sender: TObject);
    procedure Time; dynamic;
    procedure Delete; dynamic;
    procedure Contact; dynamic;
    procedure Over; dynamic;
    procedure EndFlying; virtual;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BorderWidth;
    property Canvas;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property ColorF: TColor read FColorF write SetColorF;
    property Start: boolean read FStart write SetStart;
    property Pause: boolean read FPause write SetPause;
    property FirstFocus: boolean read FFirstFocus;
    property TetrisKey: TTSAKey read FTetrisKey write SetTetrisKey;
    property TetrisKeyDown: word read FTetrisKeyDown write SetTetrisKeyDown;
    property NextSColor: boolean read FNextSColor write SetNextSColor;
    property NextShape: word read FNextShape write SetNextShape;
    property ColorShapes: TColorTAFShapes read FColorShapes write SetColorShapes;
    property OneColorShapes: TColor read FOneColorShapes write SetOneColorShapes;
    property ShapesShape: TShapeType read FShapesShape write SetShapesShape;
    property ShapesBrushStyle: TBrushStyle read FShapesBrushStyle
      write SetShapesBrushStyle;
    property ColorShapesPen: TColor read FColorShapesPen write SetColorShapesPen;
    property WidthShapesPen: integer read FWidthShapesPen write SetWidthShapesPen;
    property OnOver: TNotifyEvent read FOnOver write FOnOver;
    property OnContact: TNotifyEvent read FOnContact write FOnContact;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property OnTime: TNotifyEvent read FOnTime write FOnTime;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Delay: integer read FDelay write SetDelay;
    property Rows: word read FRows write FRows;
    property Columns: word read FColumns write FColumns;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TArkanoid = class(TCustomPanel)
  private
    Timer1, Timer2: TTimer;
    FColorBl, FColorT, FColorShapesPen, FOneColorShapes: TColor;
    FColorShapes: TColorTAFShapes;
    FShapesShape: TShapeType;
    FShapesBrushStyle: TBrushStyle;
    FDelay: integer;
    FBSpeed, FSSpeed, FTN, FTL, FTS, FBHW, FCreSquShape, FWidthShapesPen,
    FLevels, FLevel, FSteps: integer;
    FStart, FPause, FFirstFocus, FStrongShapes, FImmDelShapes, FEndStop: boolean;
    FOnDelete, FOnOver, FOnContact, FOnNewBall, FOnTime, FOnEndLevel,
    FOnPaint: TNotifyEvent;
    FArcanoidKey: TTSAKey;
    FArcanoidKeyDown, FFriction: word;
    FBalls, FBll: integer;
    x, he, wi: word;
    FRows: word;
    FColumns: word;
    Sh: array[1..48, 1..24] of TShape;
    ShT, ShF, ShB, ShS: TShape;
    TX, FX, FY, BX, BY: double;
    procedure SetDelay(Value: integer);
    procedure SetStart(Value: boolean);
    procedure SetPause(Value: boolean);
    procedure SetEndStop(Value: boolean);
    procedure SetColorBl(Value: TColor);
    procedure SetColorT(Value: TColor);
    procedure SetBalls(Value: integer);
    procedure SetSteps(Value: integer);
    procedure SetFriction(Value: word);
    procedure SetCreSquShape(Value: integer);
    procedure SetColorShapes(Value: TColorTAFShapes);
    procedure SetOneColorShapes(Value: TColor);
    procedure SetColorShapesPen(Value: TColor);
    procedure SetWidthShapesPen(Value: integer);
    procedure SetShapesShape(Value: TShapeType);
    procedure SetShapesBrushStyle(Value: TBrushStyle);
    procedure SetBSpeed(Value: integer);
    procedure SetSSpeed(Value: integer);
    procedure SetTN(Value: integer);
    procedure SetTL(Value: integer);
    procedure SetTS(Value: integer);
    procedure SetBHW(Value: integer);
    procedure SetStrongShapes(Value: boolean);
    procedure SetImmDelShapes(Value: boolean);
    procedure SetArcanoidKey(Value: TTSAKey);
    procedure SetArcanoidKeyDown(Value: word);
    {$ifdef windows}
    procedure WMNCHitTest(var Mes: TWMNCHitTest); message WM_NCHitTest;
    {$endif}
    procedure TimerOnTimer(Sender: TObject);
    procedure Timer2OnTimer(Sender: TObject);
    procedure CreateShapes; virtual;
    procedure Left; virtual;
    procedure Right; virtual;
    procedure BSUp; virtual;
    procedure Stop; virtual;
    procedure NextLevel;
    procedure Time; dynamic;
    procedure Delete; dynamic;
    procedure Contact; dynamic;
    procedure Over; dynamic;
    procedure NewBall; dynamic;
    procedure EndLevel; dynamic;
    procedure EndFlying; virtual;
    procedure ShapesColors(i, j: integer); virtual;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BorderWidth;
    property Canvas;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property ColorBl: TColor read FColorBl write SetColorBl;
    property ColorT: TColor read FColorT write SetColorT;
    property Start: boolean read FStart write SetStart;
    property Pause: boolean read FPause write SetPause;
    property FirstFocus: boolean read FFirstFocus;
    property ArcanoidKey: TTSAKey read FArcanoidKey write SetArcanoidKey;
    property ArcanoidKeyDown: word read FArcanoidKeyDown write SetArcanoidKeyDown;
    property Balls: integer read FBalls write SetBalls;
    property Steps: integer read FSteps write SetSteps;
    property Friction: word read FFriction write SetFriction;
    property Bll: integer read FBll;
    property TN: integer read FTN write SetTN;
    property TL: integer read FTL write SetTL;
    property TS: integer read FTS write SetTS;
    property BHW: integer read FBHW write SetBHW;
    property Levels: integer read FLevels write FLevels;
    property Level: integer read Flevel;
    property StrongShapes: boolean read FStrongShapes write SetStrongShapes;
    property EndStop: boolean read FEndStop write SetEndStop;
    property ImmDelShapes: boolean read FImmDelShapes write SetImmDelShapes;
    property ColorShapes: TColorTAFShapes read FColorShapes write SetColorShapes;
    property CreSquShape: integer read FCreSquShape write SetCreSquShape;
    property OneColorShapes: TColor read FOneColorShapes write SetOneColorShapes;
    property ShapesShape: TShapeType read FShapesShape write SetShapesShape;
    property ShapesBrushStyle: TBrushStyle read FShapesBrushStyle
      write SetShapesBrushStyle;
    property ColorShapesPen: TColor read FColorShapesPen write SetColorShapesPen;
    property WidthShapesPen: integer read FWidthShapesPen write SetWidthShapesPen;
    property OnOver: TNotifyEvent read FOnOver write FOnOver;
    property OnContact: TNotifyEvent read FOnContact write FOnContact;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnNewBall: TNotifyEvent read FOnNewBall write FOnNewBall;
    property OnTime: TNotifyEvent read FOnTime write FOnTime;
    property OnEndLevel: TNotifyEvent read FOnEndLevel write FOnEndLevel;
    property Delay: integer read FDelay write SetDelay;
    property BSpeed: integer read FBSpeed write SetBSpeed;
    property SSpeed: integer read FSSpeed write SetSSpeed;
    property Rows: word read FRows write FRows;
    property Columns: word read FColumns write FColumns;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TFigure = class(TCustomLabel)
  private
    FOver: boolean;
    FLineColor: TColor;
    FOnPaint: TNotifyEvent;
    FFig: word;
    FNumb: integer;
    FMP, Pix: boolean;
    FOnMove: TNotifyEvent;
    procedure WidthsHeights;
    procedure Draw;
    procedure Coeff;
    procedure Move; dynamic;
    procedure SetNumb(Value: integer);
    procedure SetOver(Value: boolean);
    procedure SetLineColor(Value: TColor);
  public
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove
      (Shift: TShiftState; X, Y: integer); override;
  protected
    procedure Paint; override;
  public
    property Over: boolean read FOver write SetOver default False;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Fig: word read FFig write FFig;
    property Numb: integer read FNumb write SetNumb;
    property LineColor: TColor read FLineColor write SetLineColor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TFigures = class(TCustomPanel)
  private
    Fgr1, Fgr2, Fgr3, Fgr4, Fgr5: TFigure;
    Timer1: TTimer;
    FMoves: integer;
    FWidthHeight: word;
    FSquare: TFSquare;
    FColorShapes: TColorTAFShapes;
    FOnFigureMove, FOnPaint, FOnOver, FOnTime: TNotifyEvent;
    FStart, FPause, FFirstFocus: boolean;
    procedure Colors; virtual;
    procedure Detect;
    procedure SetStart(Value: boolean);
    procedure SetSquare(Value: TFSquare);
    procedure SetPause(Value: boolean);
    procedure SetColorShapes(Value: TColorTAFShapes);
    procedure SetWidthHeight(Value: word);
    procedure FigureMove(Sender: TObject);
    procedure Time; dynamic;
    procedure Over; dynamic;
    procedure TimerOnTimer(Sender: TObject);
    {$ifdef windows}
    procedure WMNCHitTest(var Mes: TWMNCHitTest); message WM_NCHitTest;
    {$endif}
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BorderWidth;
    property Canvas;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property Start: boolean read FStart write SetStart;
    property Pause: boolean read FPause write SetPause;
    property Moves: integer read FMoves;
    property Square: TFSquare read FSquare write SetSquare;
    property FirstFocus: boolean read FFirstFocus;
    property WidthHeight: word read FWidthHeight write SetWidthHeight;
    property ColorShapes: TColorTAFShapes read FColorShapes write SetColorShapes;
    property OnOver: TNotifyEvent read FOnOver write FOnOver;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnFigureMove: TNotifyEvent read FOnFigureMove write FOnFigureMove;
    property OnTime: TNotifyEvent read FOnTime write FOnTime;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TExtraGPan = class(TPanel)
  private
    Scrmbl1: TScrambler;
    Ttrs1: TTetris;
    Rcnd1: TArkanoid;
    Fgrs1: TFigures;
    Pict: TPicture;
    FBStPicture, FAOverPicture: string;
    fx, fy, fcx, fcy, FWidthTShapesPen, FWidthAShapesPen, FABSpeed,
    FASSpeed, FATN, FATL, FATS, FABHW, FSMoves, FFMoves, FTCoeff, FACoeff,
    FTScores, FAScores, FTLines, FAShapes, FACreSquShape, FALevel,
    FALevels, FASteps: integer;
    FTime: real;
    FStyle: longint;
    FState: TState;
    FFSquare: TFSquare;
    FShowScoresTime, FWithCaption, FAImmDelShapes, FStart, FPause,
    FNextSColorT, FShowAllDlgs, FPicStretch, FAStrongShapes, FTimerFocus,
    FDefKeys, FAEndStop: boolean;
    FTDelay, FADelay: integer;
    FFWidthHeight: word;
    FGKey: TTSAKey;
    FABalls, FABlls: integer;
    FColorTB, FColorTF, FColorSB, FColorAB, FColorABl, FColorAT, FColorTShapesPen,
    FColorFB, FColorAShapesPen, FOneColorTShapes, FOneColorAShapes: TColor;
    FColorTShapes, FColorAShapes, FColorFShapes: TColorTAFShapes;
    FShapesTShape, FShapesAShape: TShapeType;
    FShapesTBrushStyle, FShapesABrushStyle: TBrushStyle;
    FOnScrambleOver, FOnScrambleTime, FOnScrambleMove, FOnFiguresMove,
    FOnArcanoidTime, FOnTetrisTime, FOnArcanoidOver, FOnTetrisOver,
    FOnTetrisDelete, FOnArcanoidDelete, FOnTetrisContact, FOnArcanoidContact,
    FOnArcanoidNewBall, FOnArcanoidEndLevel, FOnFiguresTime,
    FOnFiguresOver: TNotifyEvent;
    FNextTShape, FTColumns, FAColumns, FSRowsColumns, FTRows, FARows, FAFriction: word;
    procedure SetState(Value: TState);
    procedure SetStart(Value: boolean);
    procedure SetPause(Value: boolean);
    procedure SetPicStretch(Value: boolean);
    procedure SetAStrongShapes(Value: boolean);
    procedure SetAImmDelShapes(Value: boolean);
    procedure SetGKey(Value: TTSAKey);
    procedure SetColorTF(Value: TColor);
    procedure SetColorTB(Value: TColor);
    procedure SetColorSB(Value: TColor);
    procedure SetColorAB(Value: TColor);
    procedure SetColorFB(Value: TColor);
    procedure SetColorABl(Value: TColor);
    procedure SetColorAT(Value: TColor);
    procedure SetABalls(Value: integer);
    procedure SetASteps(Value: integer);
    procedure SetAFriction(Value: word);
    procedure SetACreSquShape(Value: integer);
    procedure SetOneColorTShapes(Value: TColor);
    procedure SetOneColorAShapes(Value: TColor);
    procedure SetColorTShapes(Value: TColorTAFShapes);
    procedure SetColorAShapes(Value: TColorTAFShapes);
    procedure SetColorFShapes(Value: TColorTAFShapes);
    procedure SetShapesTShape(Value: TShapeType);
    procedure SetShapesAShape(Value: TShapeType);
    procedure SetShapesTBrushStyle(Value: TBrushStyle);
    procedure SetShapesABrushStyle(Value: TBrushStyle);
    procedure SetColorTShapesPen(Value: TColor);
    procedure SetColorAShapesPen(Value: TColor);
    procedure SetWidthTShapesPen(Value: integer);
    procedure SetWidthAShapesPen(Value: integer);
    procedure SetTDelay(Value: integer);
    procedure SetADelay(Value: integer);
    procedure SetAEndStop(Value: boolean);
    procedure SetNextSColorT(Value: boolean);
    procedure SetNextTShape(Value: word);
    procedure SetShowScoresTime(Value: boolean);
    procedure SetShowAllDlgs(Value: boolean);
    procedure SetFWidthHeight(Value: word);
    procedure ChangeState;
    procedure ChangeParams;
    procedure GamePanKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TetrisDelete(Sender: TObject);
    procedure ArcanoidDelete(Sender: TObject);
    procedure TetrisOver(Sender: TObject);
    procedure TetrisContact(Sender: TObject);
    procedure ArcanoidContact(Sender: TObject);
    procedure ArcanoidNewBall(Sender: TObject);
    procedure ScrambleOver(Sender: TObject);
    procedure ScrambleMove(Sender: TObject);
    procedure FiguresMove(Sender: TObject);
    procedure ArcanoidOver(Sender: TObject);
    procedure FiguresOver(Sender: TObject);
    procedure ScramblePaint(Sender: TObject);
    procedure ArcanoidPaint(Sender: TObject);
    procedure TetrisPaint(Sender: TObject);
    procedure FiguresPaint(Sender: TObject);
    procedure ArcanoidEndLevel(Sender: TObject);
    procedure STAFTime(Sender: TObject);
    procedure SetSRowsColumns(Value: word);
    procedure SeTTRows(Value: word);
    procedure SeTTColumns(Value: word);
    procedure SeTARows(Value: word);
    procedure SeTAColumns(Value: word);
    procedure SetBStPicture(Value: string);
    procedure SetFSquare(Value: TFSquare);
    procedure SetAOverPicture(Value: string);
    procedure SetWithCaption(Value: boolean);
    procedure SetTCoeff(Value: integer);
    procedure SetACoeff(Value: integer);
    procedure SetALevels(Value: integer);
    procedure SetABSpeed(Value: integer);
    procedure SetASSpeed(Value: integer);
    procedure SetATN(Value: integer);
    procedure SetATL(Value: integer);
    procedure SetATS(Value: integer);
    procedure SetABHW(Value: integer);
    procedure SetTimerFocus(Value: boolean);
    procedure SetDefKeys(Value: boolean);
    procedure PanelSetFocus;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ABlls: integer read FABlls;
    property ALevel: integer read FALevel default 1;
    property AShapes: integer read FAShapes default 0;
    property AScores: integer read FAScores default 0;
    property GKey: TTSAKey read FGKey write SetGKey;
    property Pause: boolean read FPause write SetPause default False;
    property Time: real read FTime;
    property NextTShape: word read FNextTShape write SetNextTShape default 7;
    property TLines: integer read FTLines default 0;
    property TScores: integer read FTScores default 0;
    property SMoves: integer read FSMoves;
    property FMoves: integer read FFMoves;
  published
    property State: TState read FState write SetState default stPanel;
    property Start: boolean read FStart write SetStart default False;
    property PicStretch: boolean read FPicStretch write SetPicStretch default True;
    property ColorSB: TColor read FColorSB write SetColorSB default clBlack;
    property ColorTB: TColor read FColorTB write SetColorTB default clBlack;
    property ColorAB: TColor read FColorAB write SetColorAB default clBlack;
    property ColorFB: TColor read FColorFB write SetColorFB default clBlack;
    property ColorTF: TColor read FColorTF write SetColorTF default clBlack;
    property ColorABl: TColor read FColorABl write SetColorABl default clLime;
    property ColorAT: TColor read FColorAT write SetColorAT default clYellow;
    property ColorTShapes: TColorTAFShapes read FColorTShapes
      write SetColorTShapes default colorFixed;
    property ColorAShapes: TColorTAFShapes read FColorAShapes
      write SetColorAShapes default colorFixed2;
    property ColorFShapes: TColorTAFShapes read FColorFShapes
      write SetColorFShapes default colorFixed2;
    property ColorAShapesPen: TColor read FColorAShapesPen
      write SetColorAShapesPen default clWhite;
    property ColorTShapesPen: TColor read FColorTShapesPen
      write SetColorTShapesPen default clWhite;
    property WidthTShapesPen: integer read FWidthTShapesPen
      write SetWidthTShapesPen default 1;
    property WidthAShapesPen: integer read FWidthAShapesPen
      write SetWidthAShapesPen default 1;
    property ACreSquShape: integer read FACreSquShape write SetACreSquShape default 2;
    property FSquare: TFSquare read FFSquare write SetFSquare default Big;
    property TDelay: integer read FTDelay write SetTDelay default 200;
    property ADelay: integer read FADelay write SetADelay default 40;
    property NextSColorT: boolean read FNextSColorT write SetNextSColorT default False;
    property AStrongShapes: boolean
      read FAStrongShapes write SetAStrongShapes default True;
    property AEndStop: boolean read FAEndStop write SetAEndStop default False;
    property AIMMDelShapes: boolean
      read FAImmDelShapes write SetAImmDelShapes default False;
    property TimerFocus: boolean read FTimerFocus write SetTimerFocus default False;
    property DefKeys: boolean read FDefKeys write SetDefKeys default True;
    property OneColorTShapes: TColor read FOneColorTShapes
      write SetOneColorTShapes default clBtnFace;
    property OneColorAShapes: TColor read FOneColorAShapes
      write SetOneColorAShapes default clBtnFace;
    property SRowsColumns: word read FSRowsColumns write SetSRowsColumns default 4;
    property ABSpeed: integer read FABSpeed write SetABSpeed default 150;
    property ASSpeed: integer read FASSpeed write SetASSpeed default 75;
    property ShapesTShape: TShapeType
      read FShapesTShape write SetShapesTShape default stRoundRect;
    property ShapesAShape: TShapeType
      read FShapesAShape write SetShapesAShape default stRoundRect;
    property ShapesTBrushStyle: TBrushStyle read FShapesTBrushStyle
      write SetShapesTBrushStyle default bsSolid;
    property ShapesABrushStyle: TBrushStyle read FShapesABrushStyle
      write SetShapesABrushStyle default bsSolid;
    property ShowScoresTime: boolean
      read FShowScoresTime write SetShowScoresTime default True;
    property ShowAllDlgs: boolean read FShowAllDlgs write SetShowAllDlgs default True;
    property AOverPicture: string read FAOverPicture write SetAOverPicture;
    property TCoeff: integer read FTCoeff write SetTCoeff default 1;
    property ACoeff: integer read FACoeff write SetACoeff default 1;
    property BStPicture: string read FBStPicture write SetBStPicture;
    property TRows: word read FTRows write SeTTRows default 20;
    property TColumns: word read FTColumns write SeTTColumns default 10;
    property ATN: integer read FATN write SetATN default 100;
    property ATL: integer read FATL write SetATL default 150;
    property ATS: integer read FATS write SetATS default 75;
    property ABHW: integer read FABHW write SetABHW default 75;
    property ALevels: integer read FALevels write SetALevels default 10;
    property ARows: word read FARows write SeTARows default 30;
    property AColumns: word read FAColumns write SeTAColumns default 11;
    property ABalls: integer read FABalls write SetABalls default 3;
    property ASteps: integer read FASteps write SetASteps default 100;
    property AFriction: word read FAFriction write SetAFriction default 25;
    property FWidthHeight: word read FFWidthHeight write SetFWidthHeight default 65;
    property WithCaption: boolean read FWithCaption write SetWithCaption default True;
    property OnScrambleOver: TNotifyEvent read FOnScrambleOver write FOnScrambleOver;
    property OnScrambleMove: TNotifyEvent read FOnScrambleMove write FOnScrambleMove;
    property OnFiguresMove: TNotifyEvent read FOnFiguresMove write FOnFiguresMove;
    property OnArcanoidOver: TNotifyEvent read FOnArcanoidOver write FOnArcanoidOver;
    property OnArcanoidEndLevel: TNotifyEvent
      read FOnArcanoidEndLevel write FOnArcanoidEndLevel;
    property OnScrambleTime: TNotifyEvent read FOnScrambleTime write FOnScrambleTime;
    property OnFiguresTime: TNotifyEvent read FOnFiguresTime write FOnFiguresTime;
    property OnArcnoidTime: TNotifyEvent read FOnArcanoidTime write FOnArcanoidTime;
    property OnTetrisTime: TNotifyEvent read FOnTetrisTime write FOnTetrisTime;
    property OnTetrisOver: TNotifyEvent read FOnTetrisOver write FOnTetrisOver;
    property OnTetrisDelete: TNotifyEvent read FOnTetrisDelete write FOnTetrisDelete;
    property OnFiguresOver: TNotifyEvent read FOnFiguresOver write FOnFiguresOver;
    property OnArcanoidDelete: TNotifyEvent read FOnArcanoidDelete
      write FOnArcanoidDelete;
    property OnTetrisContact: TNotifyEvent read FOnTetrisContact write FOnTetrisContact;
    property OnArcanoidContact: TNotifyEvent
      read FOnArcanoidContact write FOnArcanoidContact;
  end;


implementation

uses
  Dialogs, sysutils;

var
  temp, Tmp: string;
  m, n, ni, nj, num, CSS, BW2, BH2: integer;
  AClYel, ACLLim, AClBlu, AClPur, BllUp, Fr, BUpShS: boolean;
  SpB, SpT, SpBX, SpBY, SpFX, SpFY, SpTA, SpBXT, SpBYT, Sq2: single;
  ArcStop, ArcStart: boolean;
  F125, F375, F25, F5, F75, F625, S375, S75, S625, S125, S25, S0125, Q75,
  Q125, Q5, Q1, Q25, MX, MY, wi, he: integer;


constructor TExtraGPan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := StPanel;
  FStart := False;
  FPause := False;
  FTime := 0.0;
  FSRowsColumns := 4;
  FTRows := 20;
  FTColumns := 10;
  FARows := 30;
  FAColumns := 11;
  FABalls := 3;
  FNextTShape := 7;
  FTScores := 0;
  FTLines := 0;
  FAScores := 0;
  FAShapes := 0;
  FColorSB := clBlack;
  FColorTB := clBlack;
  FColorAB := clBlack;
  FColorFB := clBlack;
  FColorTF := clBlack;
  FColorABl := clLime;
  FColorAT := clYellow;
  FColorTShapes := colorFixed;
  FColorAShapes := colorFixed2;
  FColorFShapes := colorFixed2;
  FOneColorTShapes := clBtnFace;
  FOneColorAShapes := clBtnFace;
  FColorTShapesPen := clWhite;
  FColorAShapesPen := clWhite;
  FAStrongShapes := True;
  FAImmDelShapes := False;
  FAEndStop := False;
  FWidthTShapesPen := 1;
  FWidthAShapesPen := 1;
  FTDelay := 200;
  FADelay := 40;
  FASteps := 100;
  FAFriction := 25;
  FNextSColorT := False;
  FShapesTShape := stRoundRect;
  FShapesTBrushStyle := bsSolid;
  FShapesAShape := stRoundRect;
  FShapesABrushStyle := bsSolid;
  FShowScoresTime := True;
  FACreSquShape := 2;
  FPicStretch := True;
  FABSpeed := 150;
  FASSpeed := 75;
  FTimerFocus := False;
  FDefKeys := True;
  FTCoeff := 1;
  FACoeff := 1;
  FALevels := 10;
  FAOverPicture := '';
  FBStPicture := '';
  FWithCaption := True;
  FShowAllDlgs := True;
  FATN := 100;
  FATL := 150;
  FATS := 75;
  FABHW := 75;
  FFWidthHeight := 70;
  FFSquare := Big;
  Scrmbl1 := nil;
  Ttrs1 := nil;
  Rcnd1 := nil;
  Pict := nil;
end;

destructor TExtraGPan.Destroy;
begin
  if Ttrs1 <> nil then
    Ttrs1.Free;
  if Scrmbl1 <> nil then
    Scrmbl1.Free;
  if Rcnd1 <> nil then
    Rcnd1.Free;
  if Fgrs1 <> nil then
    Fgrs1.Free;
  if Pict <> nil then
    Pict.Free;
  inherited Destroy;
end;

procedure TExtraGPan.Resize;
begin
  inherited Resize;
  if State = stPanel then
    exit;
  if Height < 80 then
    Height := 80;
  if Width < 80 then
    Width := 80;
end;

procedure TExtraGPan.ChangeParams;
begin
  FStyle := GetWindowLong(Handle, GWL_STYLE);
  if FWithCaption then
    FStyle := (FStyle xor WS_Caption xor WS_THICKFRAME)
  else
    FStyle := (FStyle xor WS_THICKFRAME);
  SetWindowLong(Handle, GWL_STYLE, FStyle);
  if FState = StPanel then
    SetWindowPos(Handle, HWND_TOP, fx, fy, fcx, fcy, SWP_DRAWFRAME)
  else
  begin
    fx := Left;
    fy := Top;
    fcx := Width;
    fcy := Height;
    SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_DRAWFRAME or SWP_NOACTIVATE);
  end;
end;

procedure TExtraGPan.TetrisOver(Sender: TObject);
begin
  if Ttrs1 <> nil then
    Ttrs1.Start := False;
  FStart := False;
  if assigned(FOnTetrisOver) then
    FOnTetrisOver(Self);
  try
    if FAOverPicture <> '' then
    begin
      try
        if Pict = nil then
          Pict := TPicture.Create;
        Pict.LoadFromFile(FAOverPicture);
      except
        ShowMessage('Cannot Open ' + FAOverPicture);
        Pict.Free;
        Pict := nil;
        FAOverPicture := '';
      end;
    end;
  finally
    if Ttrs1 <> nil then
      Ttrs1.Invalidate;
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.TetrisContact(Sender: TObject);
begin
  FNextTShape := TTrs1.NextShape;
  if assigned(FOnTetrisContact) then
    FOnTetrisContact(Self);
end;

procedure TExtraGPan.ArcanoidContact(Sender: TObject);
begin
  if assigned(FOnArcanoidContact) then
    FOnArcanoidContact(Self);
end;

procedure TExtraGPan.ScrambleMove(Sender: TObject);
begin
  FSMoves := Scrmbl1.Moves;
  if assigned(FOnScrambleMove) then
    FOnScrambleMove(Self);
  PanelSetFocus;
end;

procedure TExtraGPan.FiguresMove(Sender: TObject);
begin
  FFMoves := Fgrs1.Moves;
  if assigned(FOnFiguresMove) then
    FOnFiguresMove(Self);
  PanelSetFocus;
end;

procedure TExtraGPan.ArcanoidNewBall(Sender: TObject);
begin
  if assigned(FOnArcanoidNewBall) then
    FOnArcanoidNewBall(Self);
  PanelSetFocus;
end;

procedure TExtraGPan.TetrisDelete(Sender: TObject);
begin
  FTLines := FTLines + 1;
  FTScores := FTScores + FTCoeff;
  if assigned(FOnTetrisDelete) then
    FOnTetrisDelete(Self);
  PanelSetFocus;
end;

procedure TExtraGPan.ArcanoidDelete(Sender: TObject);
begin
  FAShapes := FAShapes + 1;
  FAScores := FAScores + FACoeff;
  if assigned(FOnArcanoidDelete) then
    FOnArcanoidDelete(Self);
  PanelSetFocus;
end;

procedure TExtraGPan.ScrambleOver(Sender: TObject);
begin
  if Scrmbl1 <> nil then
    Scrmbl1.Start := False;
  FStart := False;
  if assigned(FOnScrambleOver) then
    FOnScrambleOver(Self);
  try
    if FAOverPicture <> '' then
    begin
      try
        if Pict = nil then
          Pict := TPicture.Create;
        Pict.LoadFromFile(FAOverPicture);
      except
        ShowMessage('Cannot Open ' + FAOverPicture);
        Pict.Free;
        Pict := nil;
        FAOverPicture := '';
      end;
    end;
  finally
    if Scrmbl1 <> nil then
      Scrmbl1.Invalidate;
    PanelSetFocus;
  end;
end;


procedure TExtraGPan.ArcanoidOver(Sender: TObject);
begin
  if Rcnd1 <> nil then
    Rcnd1.Start := False;
  FStart := False;
  if Rcnd1.Level = Rcnd1.Levels + 1 then
    FALevel := FALevels;
  if assigned(FOnArcanoidOver) then
    FOnArcanoidOver(Self);
  try
    if FAOverPicture <> '' then
    begin
      try
        if Pict = nil then
          Pict := TPicture.Create;
        Pict.LoadFromFile(FAOverPicture);
      except
        ShowMessage('Cannot Open ' + FAOverPicture);
        Pict.Free;
        Pict := nil;
        FAOverPicture := '';
      end;
    end;
  finally
    if FShowScoresTime = False then
      Caption := ''
    else
    begin
      Str(Time: 6: 0, Tmp);
      Caption := 'Score: ' + IntToStr(FAScores) + ' Level:' + IntToStr(FAlevel);
    end;
    if Rcnd1 <> nil then
      Rcnd1.Invalidate;
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.FiguresOver(Sender: TObject);
begin
  if Fgrs1 <> nil then
    Fgrs1.Start := False;
  FStart := False;
  if assigned(FOnFiguresOver) then
    FOnFiguresOver(Self);
  try
    if FAOverPicture <> '' then
    begin
      try
        if Pict = nil then
          Pict := TPicture.Create;
        Pict.LoadFromFile(FAOverPicture);
      except
        ShowMessage('Cannot open ' + FAOverPicture);
        Pict.Free;
        Pict := nil;
        FAOverPicture := '';
      end;
    end;
  finally
    if Fgrs1 <> nil then
      Fgrs1.Invalidate;
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.ScramblePaint(Sender: TObject);
var
  i, j: integer;
begin
  if Scrmbl1 <> nil then
    with Scrmbl1 do
      if Start = False then
        if Pict <> nil then
          if PicStretch then
            Canvas.StretchDraw(ClientRect, Pict.Graphic)
          else
          begin
            for i := 0 to Width div Pict.Graphic.Width do
              for j := 0 to Height div Pict.Graphic.Height do
                Canvas.Draw(i * Pict.Graphic.Width, j * Pict.Graphic.Height, Pict.Graphic);
          end;
end;

procedure TExtraGPan.ArcanoidPaint(Sender: TObject);
var
  i, j: integer;
begin
  if Rcnd1 <> nil then
    with Rcnd1 do
      if Start = False then
        if Pict <> nil then
          if PicStretch then
            Canvas.StretchDraw(ClientRect, Pict.Graphic)
          else
          begin
            for i := 0 to Width div Pict.Graphic.Width do
              for j := 0 to Height div Pict.Graphic.Height do
                Canvas.Draw(i * Pict.Graphic.Width, j * Pict.Graphic.Height, Pict.Graphic);
          end;
end;

procedure TExtraGPan.TetrisPaint(Sender: TObject);
var
  i, j: integer;
begin
  if Ttrs1 <> nil then
    with Ttrs1 do
      if Start = False then
        if Pict <> nil then
          if PicStretch then
            Canvas.StretchDraw(ClientRect, Pict.Graphic)
          else
          begin
            for i := 0 to Width div Pict.Graphic.Width do
              for j := 0 to Height div Pict.Graphic.Height do
                Canvas.Draw(i * Pict.Graphic.Width, j * Pict.Graphic.Height, Pict.Graphic);
          end;
end;

procedure TExtraGPan.FiguresPaint(Sender: TObject);
var
  i, j: integer;
begin
  if Fgrs1 <> nil then
    with Fgrs1 do
      if Start = False then
        if Pict <> nil then
          if PicStretch then
            Canvas.StretchDraw(ClientRect, Pict.Graphic)
          else
          begin
            for i := 0 to Width div Pict.Graphic.Width do
              for j := 0 to Height div Pict.Graphic.Height do
                Canvas.Draw(i * Pict.Graphic.Width, j * Pict.Graphic.Height, Pict.Graphic);
          end;
end;

procedure TExtraGPan.ArcanoidEndLevel(Sender: TObject);
begin
  if assigned(FOnArcanoidEndLevel) then
    FOnArcanoidEndLevel(Self);
  if Rcnd1 <> nil then
  begin
    Rcnd1.Rows := FARows;
    Rcnd1.Columns := FAColumns;
    FALevel := Rcnd1.Level;
  end;
end;

procedure TExtraGPan.STAFTime;
begin
  if FTimerFocus then
    PanelSetFocus;
  FTime := FTime + 1.0;
  if FState = Scramble then
    if Scrmbl1 <> nil then
    begin
      if assigned(FOnScrambleTime) then
        FOnScrambleTime(Self);
      if FShowScoresTime = False then
      begin
        Caption := '';
        exit;
      end;
      Str(Time: 6: 0, Tmp);
      Caption := ' Moves: ' + IntToStr(FSMoves);
      exit;
    end;
  if FState = Figures then
    if Fgrs1 <> nil then
    begin
      if assigned(FOnFiguresTime) then
        FOnFiguresTime(Self);
      if FShowScoresTime = False then
      begin
        Caption := '';
        exit;
      end;
      Str(Time: 6: 0, Tmp);
      Caption := ' Moves: ' + IntToStr(FFMoves);
      exit;
    end;
  if FState = Arcanoid then
    if Rcnd1 <> nil then
    begin
      if assigned(FOnArcanoidTime) then
        FOnArcanoidTime(Self);
      if FShowScoresTime = False then
      begin
        Caption := '';
        exit;
      end;
      Str(Time: 6: 0, Tmp);
      Caption := 'Score: ' + IntToStr(FAScores) + ' Balls: ' +
        IntToStr(Rcnd1.Bll) + ' Level:' + IntToStr(FAlevel);
      exit;
    end;
  if State = Tetris then
    if Ttrs1 <> nil then
    begin
      if assigned(FOnTetrisTime) then
        FOnTetrisTime(Self);
      if ShowScoresTime = False then
      begin
        Caption := '';
        exit;
      end;
      Str(Time: 6: 0, Tmp);
      Caption := 'Score: ' + IntToStr(FTScores);
    end;
end;

procedure TExtraGPan.GamePanKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not FDefKeys then
    Exit;
  case State of
    Tetris:
      if Ttrs1 <> nil then
        if (TTrs1.Start) and (not Ttrs1.Pause) then
          Ttrs1.TetrisKeyDown := key;
    Scramble:
      if Scrmbl1 <> nil then
        if (Scrmbl1.Start) and (not Scrmbl1.Pause) then
          Scrmbl1.ScrambleKeyDown := Key;
    Arcanoid:
      if Rcnd1 <> nil then
        if (Rcnd1.Start) and (not Rcnd1.Pause) then
          Rcnd1.ArcanoidKeyDown := key;
  end;
  PanelSetFocus;
end;

procedure TExtraGPan.ChangeState;
begin
  if FState = StPanel then
  begin
    if Scrmbl1 <> nil then
    begin
      Scrmbl1.Free;
      Scrmbl1 := nil;
      ChangeParams;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else if Ttrs1 <> nil then
    begin
      Ttrs1.Free;
      Ttrs1 := nil;
      ChangeParams;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else if Rcnd1 <> nil then
    begin
      Rcnd1.Free;
      Rcnd1 := nil;
      ChangeParams;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else if Fgrs1 <> nil then
    begin
      Fgrs1.Free;
      Fgrs1 := nil;
      ChangeParams;
      Caption := temp;
      FStart := False;
      FPause := False;
    end;
    if Pict <> nil then
    begin
      Pict.Free;
      Pict := nil;
    end;
  end
  else
  if State = Tetris then
  begin
    if Scrmbl1 <> nil then
    begin
      Scrmbl1.Free;
      Scrmbl1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Rcnd1 <> nil then
    begin
      Rcnd1.Free;
      Rcnd1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Fgrs1 <> nil then
    begin
      Fgrs1.Free;
      Fgrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
      ChangeParams;
    temp := Caption;
    Caption := 'PLEASE WAIT';
    Ttrs1 := TTetris.Create(Self);
    with Ttrs1 do
    begin
      Start := False;
      Columns := FTColumns;
      Rows := FTRows;
      Align := AlClient;
      Pause := False;
      Caption := '';
      Color := FColorTB;
      ColorF := FColorTF;
      ColorShapes := FColorTShapes;
      NextSColor := FNextSColorT;
      NextShape := FNextTShape;
      ShapesBrushStyle := FShapesTBrushStyle;
      ColorShapesPen := FColorTShapesPen;
      WidthShapesPen := FWidthTShapesPen;
      ShapesShape := FShapesTShape;
      OneColorShapes := FOneColorTShapes;
      Delay := FTDelay;
      OnKeyDown := GamePanKeyDown;
      OnDelete := TetrisDelete;
      OnOver := TetrisOver;
      OnContact := TetrisContact;
      OnTime := STAFTime;
      OnPaint := TetrisPaint;
      TabStop := True;
      Parent := Self;
    end;
    FTLines := 0;
    case ShowScoresTime of
      True: Caption := 'Tetris';
      False: Caption := '';
    end;
    if Pict <> nil then
    begin
      Pict.Free;
      Pict := nil;
    end;
    try
      if FBStPicture <> '' then
        if Pict = nil then
          Pict := TPicture.Create;
      if Pict <> nil then
        Pict.LoadFromFile(FBStPicture);
    except
      ShowMessage('Cannot Open ' + FBStPicture);
      Pict.Free;
      Pict := nil;
      FBStPicture := '';
    end;
    Ttrs1.Invalidate;
  end
  else
  if State = Scramble then
  begin
    if Ttrs1 <> nil then
    begin
      Ttrs1.Free;
      Ttrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Rcnd1 <> nil then
    begin
      Rcnd1.Free;
      Rcnd1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Fgrs1 <> nil then
    begin
      Fgrs1.Free;
      Fgrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
      ChangeParams;
    temp := Caption;
    Caption := 'PLEASE WAIT';
    Scrmbl1 := TScrambler.Create(Self);
    with Scrmbl1 do
    begin
      Start := False;
      RowsColumns := FSRowsColumns;
      Align := AlClient;
      Caption := '';
      Color := FColorSB;
      Pause := False;
      OnKeyDown := GamePanKeyDown;
      OnTime := STAFTime;
      OnOver := ScrambleOver;
      OnMove := ScrambleMove;
      OnPaint := ScramblePaint;
      TabStop := True;
      Parent := Self;
    end;
    FTime := 0.;
    if FShowScoresTime = True then
      Caption := 'Scramble'
    else
      Caption := '';
    if Pict <> nil then
    begin
      Pict.Free;
      Pict := nil;
    end;
    try
      if FBStPicture <> '' then
        if Pict = nil then
          Pict := TPicture.Create;
      if Pict <> nil then
        Pict.LoadFromFile(FBStPicture);
    except
      ShowMessage('Cannot Open ' + FBStPicture);
      Pict.Free;
      Pict := nil;
      FBStPicture := '';
    end;
    Scrmbl1.Invalidate;
  end
  else
  if State = Arcanoid then
  begin
    if Ttrs1 <> nil then
    begin
      Ttrs1.Free;
      Ttrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Scrmbl1 <> nil then
    begin
      Scrmbl1.Free;
      Scrmbl1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Fgrs1 <> nil then
    begin
      Fgrs1.Free;
      Fgrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
      ChangeParams;
    temp := Caption;
    Caption := 'PLEASE WAIT';
    Rcnd1 := TArkanoid.Create(Self);
    with Rcnd1 do
    begin
      Start := False;
      Align := AlClient;
      Caption := '';
      Rows := FARows;
      Columns := FAColumns;
      Delay := FADelay;
      Color := FColorAB;
      BSpeed := FABSpeed;
      SSpeed := FASSpeed;
      ColorBl := FColorABl;
      ColorT := FColorAT;
      Pause := False;
      ShapesBrushStyle := FShapesABrushStyle;
      ColorShapesPen := FColorAShapesPen;
      WidthShapesPen := FWidthAShapesPen;
      OneColorShapes := FOneColorAShapes;
      ColorShapes := FColorAShapes;
      ShapesShape := FShapesAShape;
      StrongShapes := FAStrongShapes;
      ImmDelShapes := FAImmDelShapes;
      CreSquShape := FACreSquShape;
      Balls := FABalls;
      Levels := FALevels;
      EndStop := FAEndStop;
      BHW := FABHW;
      TN := FATN;
      TL := FATL;
      TS := FATS;
      Steps := FASteps;
      Friction := FAFriction;
      OnKeyDown := GamePanKeyDown;
      OnDelete := ArcanoidDelete;
      OnContact := ArcanoidContact;
      OnNewBall := ArcanoidNewBall;
      OnOver := ArcanoidOver;
      OnEndLevel := ArcanoidEndLevel;
      OnTime := STAFTime;
      OnPaint := ArcanoidPaint;
      TabStop := True;
      Parent := Self;
    end;
    FTime := 0.;
    if FShowScoresTime = True then
      Caption := 'Arcanoid'
    else
      Caption := '';
    if Pict <> nil then
    begin
      Pict.Free;
      Pict := nil;
    end;
    try
      if FBStPicture <> '' then
        if Pict = nil then
          Pict := TPicture.Create;
      if Pict <> nil then
        Pict.LoadFromFile(FBStPicture);
    except
      ShowMessage('Cannot Open ' + FBStPicture);
      Pict.Free;
      Pict := nil;
      FBStPicture := '';
    end;
    Rcnd1.Invalidate;
  end
  else
  if State = Figures then
  begin
    if Ttrs1 <> nil then
    begin
      Ttrs1.Free;
      Ttrs1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Scrmbl1 <> nil then
    begin
      Scrmbl1.Free;
      Scrmbl1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
    if Rcnd1 <> nil then
    begin
      Rcnd1.Free;
      Rcnd1 := nil;
      Caption := temp;
      FStart := False;
      FPause := False;
    end
    else
      ChangeParams;
    temp := Caption;
    Caption := 'PLEASE WAIT';
    Fgrs1 := TFigures.Create(Self);
    with Fgrs1 do
    begin
      Start := False;
      Align := AlClient;
      Caption := '';
      WidthHeight := FFWidthHeight;
      Color := FColorFB;
      Pause := False;
      ColorShapes := FColorFShapes;
      Square := FFSquare;
      OnOver := FiguresOver;
      OnTime := STAFTime;
      OnFigureMove := FiguresMove;
      OnPaint := FiguresPaint;
      TabStop := True;
      Parent := Self;
    end;
    FTime := 0.;
    if FShowScoresTime = True then
      Caption := 'Figures'
    else
      Caption := '';
    if Pict <> nil then
    begin
      Pict.Free;
      Pict := nil;
    end;
    try
      if FBStPicture <> '' then
        if Pict = nil then
          Pict := TPicture.Create;
      if Pict <> nil then
        Pict.LoadFromFile(FBStPicture);
    except
      ShowMessage('Cannot Open ' + FBStPicture);
      Pict.Free;
      Pict := nil;
      FBStPicture := '';
    end;
    Fgrs1.Invalidate;
  end;
end;

procedure TExtraGPan.SetTCoeff(Value: integer);
begin
  try
    if FTCoeff <> Value then
      FTCoeff := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetACoeff(Value: integer);
begin
  try
    if FACoeff <> Value then
      FACoeff := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetGKey(Value: TTSAKey);
begin
  try
    if (State = Scramble) and (Scrmbl1 <> nil) and (not (Value in [Drp..Rtt])) then
    begin
      if FGKey <> Value then
        FGKey := Value;
      if FStart then
        if not FPause then
          Scrmbl1.ScrambleKey := FGKey;
    end;
    if (State = Tetris) and (Ttrs1 <> nil) and (Value in [Lft..Rtt]) then
    begin
      if FGKey <> Value then
        FGKey := Value;
      if FStart then
        if not FPause then
          TTrs1.TetrisKey := FGKey;
    end;
    if (State = Arcanoid) and (Rcnd1 <> nil) and (Value in [lft, rgh, up, stp]) then
    begin
      if FGKey <> Value then
        FGKey := Value;
      if FStart then
        if not FPause then
          Rcnd1.ArcanoidKey := FGKey;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShowScoresTime(Value: boolean);
begin
  try
    if FShowScoresTime <> Value then
      FShowScoresTime := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetState(Value: TState);
begin
  if FState <> Value then
  begin
    try
      FState := Value;
    finally
      if not (csDesigning in ComponentState) then
        ChangeState;
    end;
  end;
end;

procedure TExtraGPan.SetStart(Value: boolean);
begin
  if csDesigning in ComponentState then
  begin
    FStart := Value;
    exit;
  end;
  if FState = Tetris then
    if TTrs1 <> nil then
      if FStart <> Value then
        if FStart = False then
        begin
          Ttrs1.Invalidate;
          FStart := Value;
          FTScores := 0;
          FTLines := 0;
          Ttrs1.Caption := '';
          FTLines := 0;
          FTime := 0.0;
          case ShowScoresTime of
            True: Caption := 'Score: 0';
            False: Caption := '';
          end;
          if Pict <> nil then
          begin
            Pict.Free;
            Pict := nil;
          end;
          with Ttrs1 do
          begin
            Rows := FTRows;
            Columns := FTColumns;
            Start := True;
          end;
        end;
  if FState = Arcanoid then
    if Rcnd1 <> nil then
      if FStart <> Value then
        if FStart = False then
        begin
          Rcnd1.Invalidate;
          FStart := Value;
          FAScores := 0;
          FAShapes := 0;
          FALevel := 1;
          FTime := 0.0;
          Rcnd1.Caption := '';
          case ShowScoresTime of
            True: Caption :=
                'Score: 0 Balls: ' + IntToStr(FABalls) + ' Level: 1';
            False: Caption := '';
          end;
          if Pict <> nil then
          begin
            Pict.Free;
            Pict := nil;
          end;
          with Rcnd1 do
          begin
            Balls := FABalls;
            Levels := FALevels;
            Rows := FARows;
            Columns := FAColumns;
            Start := True;
          end;
        end;
  if FState = Scramble then
    if Scrmbl1 <> nil then
      if FStart <> Value then
        if FStart = False then
        begin
          Scrmbl1.Invalidate;
          FStart := Value;
          FTime := 0.;
          FSMoves := 0;
          if FShowScoresTime = True then
            Caption := ' Moves: 0'
          else
            Caption := '';
          if Pict <> nil then
          begin
            Pict.Free;
            Pict := nil;
          end;
          with Scrmbl1 do
          begin
            Caption := '';
            RowsColumns := FSRowsColumns;
            Start := True;
          end;
        end;
  if FState = Figures then
    if Fgrs1 <> nil then
      if FStart <> Value then
        if FStart = False then
        begin
          Fgrs1.Invalidate;
          FStart := Value;
          FTime := 0.;
          FFMoves := 0;
          if FShowScoresTime = True then
            Caption := ' Moves: 0'
          else
            Caption := '';
          if Pict <> nil then
          begin
            Pict.Free;
            Pict := nil;
          end;
          with Fgrs1 do
          begin
            Caption := '';
            Square := FFSquare;
            WidthHeight := FFWidthHeight;
            Start := True;
          end;
        end;
  PanelSetFocus;
end;

procedure TExtraGPan.SetPause(Value: boolean);
begin
  try
    if csDesigning in ComponentState then
      exit;
    if FState = Scramble then
      if Scrmbl1 <> nil then
        if FPause <> Value then
        begin
          if FStart = False then
          begin
            PanelSetFocus;
            Exit;
          end;
          FPause := Value;
          Scrmbl1.Pause := Value;
          if not FPause then
            Scrmbl1.Align := AlClient;
        end;
    if FState = Tetris then
      if Ttrs1 <> nil then
        if FPause <> Value then
        begin
          if FStart = False then
          begin
            PanelSetFocus;
            Exit;
          end;
          FPause := Value;
          Ttrs1.Pause := Value;
          if not FPause then
            Ttrs1.Align := AlClient;
        end;
    if FState = Arcanoid then
      if Rcnd1 <> nil then
        if FPause <> Value then
        begin
          if FStart = False then
          begin
            PanelSetFocus;
            Exit;
          end;
          FPause := Value;
          Rcnd1.Pause := Value;
          if not FPause then
            Rcnd1.Align := AlClient;
        end;
    if FState = Figures then
      if Fgrs1 <> nil then
        if FPause <> Value then
        begin
          if FStart = False then
          begin
            PanelSetFocus;
            Exit;
          end;
          FPause := Value;
          Fgrs1.Pause := Value;
          if not FPause then
            Fgrs1.Align := AlClient;
        end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorTF(Value: TColor);
begin
  try
    if FColorTF <> Value then
    begin
      FColorTF := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.ColorF := FColorTF;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorABl(Value: TColor);
begin
  try
    if FColorABl <> Value then
    begin
      FColorABl := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ColorBl := FColorABl;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorAT(Value: TColor);
begin
  try
    if FColorAT <> Value then
    begin
      FColorAT := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ColorT := FColorAT;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetTDelay(Value: integer);
begin
  try
    if FTDelay <> Value then
      FTDelay := Value;
    if State = Tetris then
      if Ttrs1 <> nil then
        TTrs1.Delay := FTDelay;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetADelay(Value: integer);
begin
  try
    if Value <= 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    begin
      if FADelay <> Value then
        FADelay := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.Delay := FADelay;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorTShapes(Value: TColorTAFShapes);
begin
  try
    if FColorTShapes <> Value then
    begin
      FColorTShapes := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.ColorShapes := FColorTShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorAShapes(Value: TColorTAFShapes);
begin
  try
    if FColorAShapes <> Value then
    begin
      FColorAShapes := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ColorShapes := FColorAShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorFShapes(Value: TColorTAFShapes);
begin
  try
    if (Value = ColorOne) or (Value = ColorRandom) then
      MessageDlg('This Value is Available only for Tetris and Arcanoid',
        mtError, [mbOK], 0)
    else
    if FColorFShapes <> Value then
    begin
      FColorFShapes := Value;
      if State = Figures then
        if Fgrs1 <> nil then
        begin
          Fgrs1.ColorShapes := FColorFShapes;
          if Fgrs1.Start then
            Fgrs1.Invalidate;
        end;
    end;
  finally
    PanelSetFocus;
  end;
end;


procedure TExtraGPan.SetWithCaption(Value: boolean);
begin
  try
    if (not (csDesigning in ComponentState)) and (State <> stPanel) then
    begin
      MessageDlg('At run time change this option only if State=stPanel',
        mtError, [mbOK], 0);
    end
    else
    if FWithCaption <> Value then
      FWithCaption := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetOneColorTShapes(Value: TColor);
begin
  try
    if FOneColorTShapes <> Value then
    begin
      FOneColorTShapes := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.OneColorShapes := FOneColorTShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAStrongShapes(Value: boolean);
begin
  try
    if FAStrongShapes <> Value then
    begin
      FAStrongShapes := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.StrongShapes := FAStrongShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAImmDelShapes(Value: boolean);
begin
  try
    if FAImmDelShapes <> Value then
    begin
      FAImmDelShapes := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ImmDelShapes := FAImmDelShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAEndStop(Value: boolean);
begin
  try
    if FAEndStop <> Value then
    begin
      FAEndStop := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.EndStop := FAEndStop;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetALevels(Value: integer);
begin
  if not Value in [1..10] then
  begin
    MessageDlg('Value must be in [1..10]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FALevels <> Value then
    begin
      FALevels := Value;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('Restart Arcanoid to have effect this option',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetFWidthHeight(Value: word);
begin
  if not Value in [40..90] then
  begin
    MessageDlg('Value must be in [40..90]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FFWidthHeight <> Value then
    begin
      FFWidthHeight := Value;
      if State = Figures then
        if Fgrs1.Start then
          Fgrs1.WidthHeight := FFWidthHeight;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetTimerFocus(Value: boolean);
begin
  try
    if FTimerFocus <> Value then
      FTimerFocus := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetDefKeys(Value: boolean);
begin
  try
    if FDefKeys <> Value then
      FDefKeys := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetPicStretch(Value: boolean);
begin
  try
    if FPicStretch <> Value then
      FPicStretch := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShowAllDlgs(Value: boolean);
begin
  try
    if FShowAllDlgs <> Value then
      FShowAllDlgs := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetOneColorAShapes(Value: TColor);
begin
  try
    if FOneColorAShapes <> Value then
    begin
      FOneColorAShapes := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.OneColorShapes := FOneColorAShapes;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorTShapesPen(Value: TColor);
begin
  try
    if FColorTShapesPen <> Value then
    begin
      FColorTShapesPen := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.ColorShapesPen := FColorTShapesPen;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorAShapesPen(Value: TColor);
begin
  try
    if FColorAShapesPen <> Value then
    begin
      FColorAShapesPen := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ColorShapesPen := FColorAShapesPen;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetWidthTShapesPen(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FWidthTShapesPen <> Value then
    begin
      FWidthTShapesPen := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.WidthShapesPen := FWidthTShapesPen;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetWidthAShapesPen(Value: integer);
begin
  try
    if not Value >= 0 then
      MessageDlg('Value must be >=0', mtError, [mbOK], 0)
    else
    if FWidthAShapesPen <> Value then
    begin
      FWidthAShapesPen := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.WidthShapesPen := FWidthAShapesPen;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetATN(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FATN <> Value then
    begin
      FATN := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.TN := FATN;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('This option will have effect with the new ball',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetATL(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FATL <> Value then
    begin
      FATL := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.TL := FATL;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('This option will have effect with the new ball',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetACreSquShape(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FACreSquShape <> Value then
    begin
      FACreSquShape := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.CreSquShape := FACreSquShape;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetATS(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FATS <> Value then
    begin
      FATS := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.TS := FATS;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('This option will have effect with the new ball',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetABHW(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FABHW <> Value then
    begin
      FABHW := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.BHW := FABHW;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('This option will have effect with the new ball',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorTB(Value: TColor);
begin
  try
    if FColorTB <> Value then
    begin
      FColorTB := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.Color := FColorTB;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetABalls(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    begin
      if FABalls <> Value then
        FABalls := Value;
      if State = Arcanoid then
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg('Restart Arcanoid to have effect this option',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetASteps(Value: integer);
begin
  try
    if not Value > 0 then
      MessageDlg('Value must be >0', mtError, [mbOK], 0)
    else
    if FASteps <> Value then
      FASteps := Value;
    if State = Arcanoid then
      if Rcnd1 <> nil then
      begin
        Rcnd1.Steps := FASteps;
        if Rcnd1.Start then
          if FShowAllDlgs then
            MessageDlg
            ('This option will have effect with the new ball', mtInformation, [mbOK], 0);
      end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAFriction(Value: word);
begin
  try
    if not Value in [0..100] then
      MessageDlg('Value must be in [0..100]', mtError, [mbOK], 0)
    else
    begin
      if FAFriction <> Value then
        FAFriction := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.Friction := FAFriction;
    end;
  finally
    PanelSetFocus;
  end;
end;


procedure TExtraGPan.SetColorSB(Value: TColor);
begin
  try
    if FColorSB <> Value then
    begin
      FColorSB := Value;
      if State = Scramble then
        if Scrmbl1 <> nil then
          Scrmbl1.Color := FColorSB;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorAB(Value: TColor);
begin
  try
    if FColorAB <> Value then
    begin
      FColorAB := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.Color := FColorAB;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetColorFB(Value: TColor);
begin
  try
    if FColorFB <> Value then
    begin
      FColorFB := Value;
      if State = Figures then
        if Fgrs1 <> nil then
          Fgrs1.Color := FColorFB;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetABSpeed(Value: integer);
begin
  try
    if FABSpeed <> Value then
    begin
      FABSpeed := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
        begin
          Rcnd1.BSpeed := FABSpeed;
          if Rcnd1.Start then
            if FShowAllDlgs then
              MessageDlg('This option will have effect with the new ball',
                mtInformation, [mbOK], 0);
        end;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetASSpeed(Value: integer);
begin
  try
    if FASSpeed <> Value then
    begin
      FASSpeed := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
        begin
          Rcnd1.SSpeed := FASSpeed;
          if Rcnd1.Start then
            if FShowAllDlgs then
              MessageDlg('This option will have effect with the new ball',
                mtInformation, [mbOK], 0);
        end;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetNextSColorT(Value: boolean);
begin
  try
    if FNextSColorT <> Value then
    begin
      FNextSColorT := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          Ttrs1.NextSColor := FNextSColorT;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetNextTShape(Value: word);
begin
  if not Value in [1..7] then
  begin
    MessageDlg('Value must be in [1..7]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FNextTShape <> Value then
    begin
      FNextTShape := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
        begin
          Ttrs1.NextShape := FNextTShape;
        end;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShapesTShape(Value: TShapeType);
begin
  try
    if FShapesTShape <> Value then
    begin
      FShapesTShape := Value;
      if State = Tetris then
        if TTrs1 <> nil then
          Ttrs1.ShapesShape := FShapesTShape;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShapesAShape(Value: TShapeType);
begin
  try
    if FShapesAShape <> Value then
    begin
      FShapesAShape := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ShapesShape := FShapesAShape;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShapesTBrushStyle(Value: TBrushStyle);
begin
  try
    if FShapesTBrushStyle <> Value then
    begin
      FShapesTBrushStyle := Value;
      if State = Tetris then
        if TTrs1 <> nil then
          Ttrs1.ShapesBrushStyle := FShapesTBrushStyle;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetShapesABrushStyle(Value: TBrushStyle);
begin
  try
    if FShapesABrushStyle <> Value then
    begin
      FShapesABrushStyle := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          Rcnd1.ShapesBrushStyle := FShapesABrushStyle;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetBStPicture(Value: string);
begin
  try
    if FBStPicture <> Value then
      FBStPicture := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAOverPicture(Value: string);
begin
  try
    if FAOverPicture <> Value then
      FAOverPicture := Value;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SeTTRows(Value: word);
begin
  if not Value in [6..48] then
  begin
    MessageDlg('Value must be in [6..48]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FTRows <> Value then
    begin
      FTRows := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          if not Ttrs1.Start then
            Ttrs1.Rows := FTRows
          else
          if FShowAllDlgs then
            MessageDlg('Restart Tetris to have effect this option',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SeTARows(Value: word);
begin
  if not Value in [14..48] then
  begin
    MessageDlg('Value must be in [14..48]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FARows <> Value then
    begin
      FARows := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          if not Rcnd1.Start then
            Rcnd1.Rows := FARows
          else
          if FShowAllDlgs then
            MessageDlg
            ('This option will have effect with the new level', mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SeTTColumns(Value: word);
begin
  if not Value in [6..24] then
  begin
    MessageDlg('Value must be in [6..24]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FTColumns <> Value then
    begin
      FTColumns := Value;
      if State = Tetris then
        if Ttrs1 <> nil then
          if not Ttrs1.Start then
            Ttrs1.Columns := FTColumns
          else
          if FShowAllDlgs then
            MessageDlg('Restart Tetris to have effect this option',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetFSquare(Value: TFSquare);
begin
  try
    if FFSquare <> Value then
    begin
      FFSquare := Value;
      if State = Figures then
        if Fgrs1 <> nil then
          if not Fgrs1.Start then
            Fgrs1.Square := FFSquare
          else
          if FShowAllDlgs then
            MessageDlg('Restart Figures to have effect this option',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetAColumns(Value: word);
begin
  if not Value in [6..24] then
  begin
    MessageDlg('Value must be in [6..24]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FAColumns <> Value then
    begin
      FAColumns := Value;
      if State = Arcanoid then
        if Rcnd1 <> nil then
          if not Rcnd1.Start then
            Rcnd1.Columns := FAColumns
          else
          if FShowAllDlgs then
            MessageDlg
            ('This option will have effect with the new level',
              mtInformation, [mbOK], 0);
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.SetSRowsColumns(Value: word);
begin
  if not Value in [2..8] then
  begin
    MessageDlg('Value must be in [2..8]', mtError, [mbOK], 0);
    PanelSetFocus;
    exit;
  end;
  try
    if FSRowsColumns <> Value then
    begin
      FSRowsColumns := Value;
      if State = Scramble then
        if Scrmbl1 <> nil then
        begin
          if not Scrmbl1.Start then
            Scrmbl1.RowsColumns := FSRowsColumns
          else
          if FShowAllDlgs then
            MessageDlg('Restart Scramble to have effect this option',
              mtInformation, [mbOK], 0);
        end;
    end;
  finally
    PanelSetFocus;
  end;
end;

procedure TExtraGPan.PanelSetFocus;
begin
  if (not FDefKeys) or Pause or not Start then
    exit;
  case State of
    Scramble:
      if Scrmbl1 <> nil then
        if Scrmbl1.FirstFocus then
          if not Scrmbl1.Focused then
            Scrmbl1.SetFocus;
    Tetris:
      if Ttrs1 <> nil then
        if Ttrs1.FirstFocus then
          if not Ttrs1.Focused then
            TTrs1.SetFocus;
    Arcanoid:
      if Rcnd1 <> nil then
        if Rcnd1.FirstFocus then
          if not Rcnd1.Focused then
            Rcnd1.SetFocus;
  end;
end;

{ S c r a m b l e }

constructor TScrambler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  Timer1 := nil;
  TabStop := True;
end;

destructor TScrambler.Destroy;
var
  i, j: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
      if But[i, j] <> nil then
      begin
        But[i, j].Free;
        But[i, j] := nil;
      end;
  if Timer1 <> nil then
    Timer1.Free;
  inherited Destroy;
end;

procedure TScrambler.Paint;
begin
  inherited Paint;
  if not FFirstFocus then
  begin
    if CanFocus then
      SetFocus;
    FFirstFocus := True;
  end;
  if assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TScrambler.Resize;
var
  i, j: integer;
begin
  inherited Resize;
  if FStart = False then
    Exit;
  he := Trunc(Height div FRowsColumns);
  wi := Trunc(Width div FRowsColumns);
  x := (Width - wi * FRowsColumns) div 2;
  y := (Height - he * FRowsColumns) div 2;
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
      if But[i, j] <> nil then
        But[i, j].SetBounds(wi * Pred(j) + x, he * Pred(i) + y, wi, he);
end;

procedure TScrambler.ButtonsInit;
var
  i, j: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
      if But[i, j] <> nil then
      begin
        But[i, j].Free;
        But[i, j] := nil;
      end;
  he := Trunc(Height div FRowsColumns);
  wi := Trunc(Width div FRowsColumns);
  x := (Width - wi * FRowsColumns) div 2;
  y := (Height - he * FRowsColumns) div 2;
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
    begin
      if But[i, j] = nil then
        But[i, j] := TBitBtn.Create(Self);
      with But[i, j] do
      begin
        Parent := Self;
        Hide;
        Tag := Pred(i) * FRowsColumns + j;
        Caption := IntToStr(Tag);
        Name := 'bitbtn' + Caption;
        OnClick := ButtonClick;
        SetBounds(wi * Pred(j) + x, he * Pred(i) + y, wi, he);
        TabStop := False;
      end;
    end;
end;

procedure TScrambler.ButtonsPos;
var
  l, max, i, j: integer;
  temp: shortint;
begin
  num := Sqr(FRowsColumns);
  str := IntToStr(num);
  max := 10000 + Random(10000);
  i := FRowsColumns;
  j := FRowsColumns;
  for l := 1 to max do
  begin
    m := Random(2);
    n := Pred(m * 2);
    ni := i + n;
    nj := j + n;
    if nj in [1..FRowsColumns] then
    begin
      But[i, j].Tag := But[i, nj].Tag;
      But[i, nj].Tag := num;
      temp := j;
      j := nj;
      nj := temp;
    end
    else
    if ni in [1..FRowsColumns] then
    begin
      But[i, j].Tag := But[ni, j].Tag;
      But[ni, j].Tag := num;
      temp := i;
      i := ni;
      ni := temp;
    end;
  end;
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
    begin
      But[i, j].Caption := IntToStr(But[i, j].Tag);
      if But[i, j].Tag <> num then
        But[i, j].Show;
    end;
end;

procedure TScrambler.ButtonsColors;
var
  i, j: integer;
begin
  num := Sqr(FRowsColumns);
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
    begin
      if But[i, j].Caption = IntToStr(j + Pred(i) * FRowsColumns) then
        But[i, j].Font.Color := clGreen
      else
        But[i, j].Font.Color := clRed;
      if But[i, j].Tag <> num then
        But[i, j].Show;
    end;
end;

procedure TScrambler.SetStart(Value: boolean);
begin
  if FStart <> Value then
    FStart := Value;
  if FStart = True then
  begin
    Align := alClient;
    ButtonsInit;
    ButtonsPos;
    ButtonsColors;
    FFirstFocus := False;
    if Timer1 = nil then
    begin
      Timer1 := TTimer.Create(Self);
      Timer1.Interval := 1000;
      Timer1.OnTimer := TimerOnTimer;
      Timer1.Enabled := True;
    end;
  end;
  FMoves := 0;
end;

procedure TScrambler.SetScrambleKey(Value: TTSAKey);
begin
  if FScrambleKey <> Value then
    FScrambleKey := Value;
  case FScrambleKey of
    Lft: SLeft;
    Rgh: SRight;
    Down: SDown;
    Up: SUp;
  end;
end;

procedure TScrambler.SetPause(Value: boolean);
var
  i, j: integer;
begin
  if FPause <> Value then
    FPause := Value;
  if not FStart then
    exit;
  if FPause then
  begin
    if Timer1 <> nil then
      Timer1.Enabled := False;
    for i := 1 to FRowsColumns do
      for j := 1 to FRowsColumns do
        if But[i, j] <> nil then
          But[i, j].Enabled := False;
  end
  else
  begin
    if Timer1 <> nil then
      Timer1.Enabled := True;
    for i := 1 to FRowsColumns do
      for j := 1 to FRowsColumns do
        if But[i, j] <> nil then
          But[i, j].Enabled := True;
  end;
end;

procedure TScrambler.SLeft;
var
  i, j: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 2 to FRowsColumns do
      if But[i, Pred(j)].Visible = False then
      begin
        But[i, j].SetFocus;
        ButtonKeyDown(i, j);
        exit;
      end;
end;

procedure TScrambler.SRight;
var
  i, j: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 1 to Pred(FRowsColumns) do
      if But[i, Succ(j)].Visible = False then
      begin
        But[i, j].SetFocus;
        ButtonKeyDown(i, j);
        exit;
      end;
end;

procedure TScrambler.SUp;
var
  i, j: integer;
begin
  for j := 1 to FRowsColumns do
    for i := 2 to FRowsColumns do
      if But[Pred(i), j].Visible = False then
      begin
        But[i, j].SetFocus;
        ButtonKeyDown(i, j);
        exit;
      end;
end;

procedure TScrambler.SDown;
var
  i, j: integer;
begin
  for j := 1 to FRowsColumns do
    for i := 1 to Pred(FRowsColumns) do
      if But[Succ(i), j].Visible = False then
      begin
        But[i, j].SetFocus;
        ButtonKeyDown(i, j);
        exit;
      end;
end;

procedure TScrambler.SetScrambleKeyDown(Value: word);
begin
  FScrambleKeyDown := Value;
  case FScrambleKeyDown of
    vk_numpad2: SDown;
    vk_numpad4: SLeft;
    vk_numpad6: SRight;
    vk_numpad8: SUp;
  end;
end;

procedure TScrambler.TimerOnTimer(Sender: TObject);
begin
  if FStart then
    Time;
end;

procedure TScrambler.Time;
begin
  if assigned(FOnTime) then
    FOnTime(Self);
end;

procedure TScrambler.Detect;
var
  i, j: integer;
  k: word;
begin
  k := 0;
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
      if But[i, j].Font.Color = clGreen then
        Inc(k);
  if k = Sqr(FRowsColumns) - 1 then
    Over;
end;

procedure TScrambler.ShowHideKey(i, j: integer);
var
  m: integer;
begin
  for m := 0 to 1 do
  begin
    n := Pred(m * 2);
    nj := j + n;
    ni := i + n;
    if nj in [1..FRowsColumns] then
      if But[i, nj].Caption = Str then
      begin
        But[i, nj].Caption := But[i, j].Caption;
        if But[i, nj].Caption = IntToStr(nj + Pred(i) * FRowsColumns) then
          But[i, nj].Font.Color := clGreen
        else
          But[i, nj].Font.Color := clRed;
        But[i, nj].Show;
        But[i, j].Hide;
        But[i, j].Caption := Str;
        exit;
      end;
    if ni in [1..FRowsColumns] then
      if But[ni, j].Caption = Str then
      begin
        But[ni, j].Caption := But[i, j].Caption;
        if But[ni, j].Caption = IntToStr(j + Pred(ni) * FRowsColumns) then
          But[ni, j].Font.Color := clGreen
        else
          But[ni, j].Font.Color := clRed;
        But[ni, j].Show;
        But[i, j].Hide;
        But[i, j].Caption := Str;
        exit;
      end;
  end;
end;

procedure TScrambler.ShowHide;
var
  i, j, m: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
    begin
      if But[i, j].focused then
      begin
        for m := 0 to 1 do
        begin
          n := Pred(m * 2);
          nj := j + n;
          ni := i + n;
          if nj in [1..FRowsColumns] then
            if But[i, nj].Caption = Str then
            begin
              But[i, nj].Caption := But[i, j].Caption;
              if But[i, nj].Caption = IntToStr(nj + Pred(i) * FRowsColumns) then
                But[i, nj].Font.Color := clGreen
              else
                But[i, nj].Font.Color := clRed;
              But[i, nj].Show;
              But[i, j].Hide;
              But[i, j].Caption := Str;
              exit;
            end;
          if ni in [1..FRowsColumns] then
            if But[ni, j].Caption = Str then
            begin
              But[ni, j].Caption := But[i, j].Caption;
              if But[ni, j].Caption = IntToStr(j + Pred(ni) * FRowsColumns) then
                But[ni, j].Font.Color := clGreen
              else
                But[ni, j].Font.Color := clRed;
              But[ni, j].Show;
              But[i, j].Hide;
              But[i, j].Caption := Str;
              exit;
            end;
        end;
      end;
    end;
end;

procedure TScrambler.ButtonKeyDown(i, j: integer);
begin
  ShowHideKey(i, j);
  SetFocus;
  Move;
  Detect;
end;

procedure TScrambler.ButtonClick(Sender: TObject);
begin
  ShowHide;
  SetFocus;
  Move;
  Detect;
end;

{$ifdef windows}
procedure TScrambler.WMNCHitTest(var Mes: TWMNCHitTest);
begin
  inherited;
  if Mes.Result = htClient then
    Mes.Result := htCaption;
  SetFocus;
end;
{$endif}

procedure TScrambler.Move;
begin
  Inc(FMoves);
  if Assigned(FOnMove) then
    FOnMove(Self);
end;

procedure TScrambler.Over;
var
  i, j: integer;
begin
  for i := 1 to FRowsColumns do
    for j := 1 to FRowsColumns do
      if But[i, j] <> nil then
        But[i, j].Hide;
  if Timer1 <> nil then
  begin
    Timer1.Free;
    Timer1 := nil;
  end;
  if assigned(FOnOver) then
    FOnOver(Self);
end;

{ T e t r i s }

constructor TTetris.Create(AOwner: TComponent);
var
  i, j: integer;
begin
  inherited Create(AOwner);
  Randomize;
  FStart := False;
  Timer1 := TTimer.Create(Self);
  Timer1.OnTimer := TimerOnTimer;
  Timer2 := TTimer.Create(Self);
  Timer2.OnTimer := Timer2OnTimer;
  Panel1 := TPanel.Create(Self);
  with Panel1 do
  begin
    Parent := Self;
    Hide;
    Align := alBottom;
    BevelInner := bvNone;
    BevelOuter := bvRaised;
    BorderStyle := bsSingle;
    Color := ColorF;
    Height := 0;
  end;
  FRows := 20;
  FColumns := 10;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      Sh[i, j] := nil;
end;

destructor TTetris.Destroy;
var
  i, j: integer;
begin
  if Timer1 <> nil then
  begin
    Timer1.Free;
    Timer1 := nil;
  end;
  if Timer2 <> nil then
  begin
    Timer2.Free;
    Timer2 := nil;
  end;
  if Panel1 <> nil then
  begin
    Panel1.Free;
    Panel1 := nil;
  end;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
      begin
        Sh[i, j].Free;
        Sh[i, j] := nil;
      end;
  inherited Destroy;
end;

procedure TTetris.Timer2OnTimer(Sender: TObject);
begin
  if FStart then
    Time;
end;

procedure TTetris.Time;
begin
  if assigned(FOnTime) then
    FOnTime(Self);
end;

procedure TTetris.TimerOnTimer(Sender: TObject);
var
  i, j: integer;
  k, i1: shortint;
  ii, jj: array [1..4] of shortint;
  Cont: boolean;
begin
  k := 0;
  Cont := False;
  for j := 1 to FColumns do
    for i := FRows downto 1 do
      if Sh[i, j] <> nil then
        if Sh[i, j].Tag <> 0 then
        begin
          Inc(k);
          ii[k] := i;
          jj[k] := j;
          i1 := Succ(i);
          if (i = FRows) or ((Sh[i1, j] <> nil) and (Sh[i1, j].Tag = 0)) then
            Cont := True;
          if k = 4 then
            break;
        end;
  if k = 4 then
  begin
    if Cont then
    begin
      for k := 1 to 4 do
      begin
        i := ii[k];
        j := jj[k];
        Sh[i, j].Tag := 0;
      end;
      EndFlying;
    end
    else
      for k := 1 to 4 do
      begin
        i := ii[k];
        j := jj[k];
        i1 := Succ(i);
        Sh[i1, j] := TShape.Create(Self);
        CreateShape(Sh[i1, j], i1, j, Sh[i, j].Tag);
        Sh[i, j].Free;
        Sh[i, j] := nil;
      end;
  end;
end;

procedure TTetris.CreateShape(var Shape: TShape; i, j, r: shortint);
var
  c: shortint;
  r1, r2, r3: word;
begin
  with Shape do
  begin
    Hide;
    Parent := Self;
    tag := r;
    Shape := FShapesShape;
    Brush.Style := FShapesBrushStyle;
    Pen.Color := FColorShapesPen;
    Pen.Width := FWidthShapesPen;
    case FColorShapes of
      colorOne: Brush.Color := FOneColorShapes;
      colorRandom:
      begin
        r1 := Random(256);
        r2 := Random(256);
        r3 := Random(256);
        if (Random(2) = 0) then
          r1 := 255
        else
        if (Random(2) = 0) then
          r2 := 255
        else
          r3 := 255;
        Brush.Color := RGB(r1, r2, r3);
      end;
      colorFixed2:
      begin
        c := Abs(r);
        case c of
          1, 8, 15, 22: Brush.Color := clAqua;
          2, 9, 16, 23: Brush.Color := clRed;
          3, 10, 17, 24: Brush.Color := clLime;
          4, 11, 18, 25: Brush.Color := clYellow;
          5, 12, 19, 26: Brush.Color := clBlue;
          6, 13, 20, 27: Brush.Color := clFuchsia;
          7, 14, 21, 28: Brush.Color := clSilver;
        end;
      end;
      colorFixed:
      begin
        c := Abs(r);
        case c of
          1, 8, 15, 22: Brush.Color := clMaroon;
          2, 9, 16, 23: Brush.Color := clGreen;
          3, 10, 17, 24: Brush.Color := clOlive;
          4, 11, 18, 25: Brush.Color := clNavy;
          5, 12, 19, 26: Brush.Color := clPurple;
          6, 13, 20, 27: Brush.Color := clTeal;
          7, 14, 21, 28: Brush.Color := clGray;
        end;
      end;
    end;
    SetBounds(wi * Pred(j) + x, he * Pred(i), wi, he);
    Show;
  end;
end;

procedure TTetris.CreateShapes;
var
  i1, j1, i2, j2, i3, j3, i4, j4, ij: shortint;
begin
  ;
  ij := Trunc(FColumns div 2);
  case FNextShape of
    1:
    begin
      i1 := 1;
      j1 := ij;
      i2 := 1;
      j2 := Succ(ij);
      i3 := 2;
      j3 := ij;
      i4 := 3;
      j4 := ij;
    end;
    2:
    begin
      i1 := 1;
      j1 := ij;
      i2 := 1;
      j2 := Pred(ij);
      i3 := 2;
      j3 := ij;
      i4 := 3;
      j4 := ij;
    end;
    3:
    begin
      i1 := 2;
      j1 := ij;
      i2 := 2;
      j2 := Pred(ij);
      i3 := 2;
      j3 := Succ(ij);
      i4 := 1;
      j4 := ij;
    end;
    4:
    begin
      i1 := 2;
      j1 := ij;
      i2 := 2;
      j2 := Succ(ij);
      i3 := 1;
      j3 := Succ(ij);
      i4 := 3;
      j4 := ij;
    end;
    5:
    begin
      i1 := 2;
      j1 := ij;
      i2 := 2;
      j2 := Pred(ij);
      i3 := 1;
      j3 := Pred(ij);
      i4 := 3;
      j4 := ij;
    end;
    6:
    begin
      i1 := 1;
      j1 := ij;
      i2 := 1;
      j2 := Pred(ij);
      i3 := 1;
      j3 := Succ(ij);
      i4 := 1;
      j4 := Succ(j3);
    end;
    else
    begin
      i1 := 2;
      j1 := ij;
      i2 := 2;
      j2 := Succ(ij);
      i3 := 1;
      j3 := Succ(ij);
      i4 := 1;
      j4 := ij;
    end;
  end;
  if (Sh[i1, ij] <> nil) or (Sh[i2, j2] <> nil) or (Sh[i3, j3] <> nil) or
    (Sh[i4, j4] <> nil) then
    Over
  else
  begin
    Sh[i1, j1] := TShape.Create(Self);
    Sh[i2, j2] := TShape.Create(Self);
    Sh[i3, j3] := TShape.Create(Self);
    Sh[i4, j4] := TShape.Create(Self);
    CreateShape(Sh[i1, j1], i1, j1, FNextShape);
    CreateShape(Sh[i2, j2], i2, j2, -FNextShape);
    CreateShape(Sh[i3, j3], i3, j3, -FNextShape);
    CreateShape(Sh[i4, j4], i4, j4, -FNextShape);
    FNextShape := Random(7) + 1;
    SetNextShape(FNextShape);
    Contact;
  end;
end;

procedure TTetris.Delete;
begin
  if assigned(FOnDelete) then
    FOnDelete(Self);
end;

procedure TTetris.Contact;
begin
  if assigned(FOnContact) then
    FOnContact(Self);
end;

procedure TTetris.SetColorShapes(Value: TColorTAFShapes);
begin
  if FColorShapes <> Value then
    FColorShapes := Value;
end;

procedure TTetris.SetOneColorShapes(Value: TColor);
begin
  if FOneColorShapes <> Value then
    FOneColorShapes := Value;
end;

procedure TTetris.SetColorShapesPen(Value: TColor);
begin
  if FColorShapesPen <> Value then
    FColorShapesPen := Value;
end;

procedure TTetris.SetShapesShape(Value: TShapeType);
begin
  if FShapesShape <> Value then
    FShapesShape := Value;
end;

procedure TTetris.SetDelay(Value: integer);
begin
  if FDelay <> Value then
    FDelay := Value;
  Timer1.Interval := FDelay;
end;

procedure TTetris.SetStart(Value: boolean);
begin
  if FStart = Value then
    exit;
  FStart := Value;
  if FStart then
  begin
    FNextShape := Random(7) + 1;
    FFirstFocus := False;
    Align := alClient;
    CreateShapes;
    he := Trunc(Height div FRows);
    wi := Trunc(Width div FColumns);
    x := (Width - FColumns * wi) div 2;
    if Panel1 <> nil then
    begin
      Panel1.Height := Height - FRows * he;
      Panel1.Show;
    end;
  end;
end;

procedure TTetris.SetPause(Value: boolean);
begin
  if (FPause = Value) or (Timer1 = nil) or (Timer2 = nil) then
    exit;
  FPause := Value;
  Timer1.Enabled := not FPause;
  Timer2.Enabled := not FPause;
end;

procedure TTetris.SetShapesBrushStyle(Value: TBrushStyle);
begin
  if FShapesBrushStyle <> Value then
    FShapesBrushStyle := Value;
end;

procedure TTetris.SetNextShape(Value: word);
begin
  if FNextShape <> Value then
    FNextShape := Value;
  if FNextSColor = False then
    exit;
  if FColorShapes = ColorFixed then
    case FNextShape of
      1: Color := clMaroon;
      2: Color := clGreen;
      3: Color := clOlive;
      4: Color := clNavy;
      5: Color := clPurple;
      6: Color := clTeal;
      7: Color := clGray;
    end;
  if FColorShapes = ColorFixed2 then
    case FNextShape of
      1: Color := clAqua;
      2: Color := clRed;
      3: Color := clLime;
      4: Color := clYellow;
      5: Color := clBlue;
      6: Color := clFuchsia;
      7: Color := clSilver;
    end;
end;

procedure TTetris.SetNextSColor(Value: boolean);
begin
  if FNextSColor <> Value then
    FNextSColor := Value;
end;

procedure TTetris.SetWidthShapesPen(Value: integer);
begin
  if FWidthShapesPen <> Value then
    FWidthShapesPen := Value;
end;


procedure TTetris.SetColorF(Value: TColor);
begin
  if FColorF <> Value then
  begin
    FColorF := Value;
    Panel1.Color := FColorF;
  end;
end;

procedure TTetris.SetTetrisKey(Value: TTSAKey);
begin
  if FTetrisKey <> Value then
    FTetrisKey := Value;
  case FTetrisKey of
    Lft: Left;
    Rgh: Right;
    Drp: DropDown;
    Rtt: Rotate;
  end;
end;

procedure TTetris.Over;
var
  i, j: integer;
begin
  FStart := False;
  for i := FRows downto 1 do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
      begin
        Sh[i, j].Free;
        Sh[i, j] := nil;
      end;
  Panel1.Hide;
  if assigned(FOnOver) then
    FOnOver(Self);
end;

procedure TTetris.EndFlying;
var
  i, j: integer;
  l, n: shortint;
  k: word;
begin
  Timer1.Interval := FDelay;
  for n := 1 to 4 do
    for i := FRows downto 1 do
    begin
      k := 0;
      for j := 1 to FColumns do
        if Sh[i, j] <> nil then
          k := k + 1;
      if k = FColumns then
      begin
        Delete;
        for l := i downto 2 do
        begin
          for j := 1 to FColumns do
          begin
            Sh[l, j].Free;
            Sh[l, j] := nil;
          end;
          for j := 1 to FColumns do
            if Sh[Pred(l), j] <> nil then
            begin
              Sh[l, j] := TShape.Create(Self);
              CreateShape(Sh[l, j], l, j, 0);
              Sh[l, j].Brush.Color := Sh[Pred(l), j].Brush.Color;
            end;
        end;
      end;
    end;
  CreateShapes;
end;

procedure TTetris.Left;
var
  i, j: integer;
  k, j1: shortint;
  II, JJ: array [1..4] of shortint;
begin
  k := 0;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        if Sh[i, j].Tag <> 0 then
        begin
          j1 := Pred(j);
          if (j = 1) or ((Sh[i, j1] <> nil) and (Sh[i, j1].Tag = 0)) then
            exit;
          Inc(k);
          II[k] := i;
          JJ[k] := j;
          if k = 4 then
            break;
        end;
  if k = 4 then
  begin
    for k := 1 to 4 do
    begin
      i := II[k];
      j := JJ[k];
      j1 := Pred(j);
      Sh[i, j1] := TShape.Create(Self);
      CreateShape(Sh[i, j1], i, j1, Sh[i, j].Tag);
      Sh[i, j].Free;
      Sh[i, j] := nil;
    end;
  end;
end;

procedure TTetris.Right;
var
  i, j: integer;
  k, j1: shortint;
  II, JJ: array [1..4] of shortint;
begin
  k := 0;
  for i := 1 to FRows do
    for j := FColumns downto 1 do
      if Sh[i, j] <> nil then
        if Sh[i, j].Tag <> 0 then
        begin
          j1 := Succ(j);
          if (j = FColumns) or ((Sh[i, j1] <> nil) and (Sh[i, j1].Tag = 0)) then
            exit;
          Inc(k);
          II[k] := i;
          JJ[k] := j;
          if k = 4 then
            break;
        end;
  if k = 4 then
  begin
    for k := 1 to 4 do
    begin
      i := II[k];
      j := JJ[k];
      j1 := Succ(j);
      Sh[i, j1] := TShape.Create(Self);
      CreateShape(Sh[i, j1], i, j1, Sh[i, j].Tag);
      Sh[i, j].Free;
      Sh[i, j] := nil;
    end;
  end;
end;

procedure TTetris.Rotate;
var
  i, j: integer;
  i1, j1, i11, j11, i2, j2, i22, j22, i18, i19, j8, j9: shortint;
begin
  i19 := Pred(FRows);
  i18 := Pred(i19);
  j9 := Pred(FColumns);
  j8 := Pred(j9);
  for j := 1 to FColumns do
    for i := FRows downto 1 do
      if Sh[i, j] <> nil then
        if Sh[i, j].Tag > 0 then
        begin

          j1 := Pred(j);
          j11 := Succ(j);
          j2 := Pred(j1);
          j22 := Succ(j11);
          i1 := Pred(i);
          i11 := Succ(i);
          i2 := Pred(i1);
          i22 := Succ(i11);

          if (Sh[i, j].Tag = 1) and (Sh[i, j1] = nil) and
            (Sh[i, j2] = nil) and (j > 2) then
          begin
            Sh[i, j11].Free;
            Sh[i22, j].Free;
            Sh[i, j11] := nil;
            Sh[i22, j] := nil;
            Sh[i, j1] := TShape.Create(Self);
            Sh[i, j2] := TShape.Create(Self);
            CreateShape(Sh[i, j1], i, j1, -8);
            CreateShape(Sh[i, j2], i, j2, -8);
            Sh[i, j].Tag := 8;
            exit;
          end;

          if (Sh[i, j].Tag = 8) and (Sh[i1, j] = nil) and
            (Sh[i2, j] = nil) and (i > 2) then
          begin
            Sh[i11, j].Free;
            Sh[i, j2].Free;
            Sh[i11, j] := nil;
            Sh[i, j2] := nil;
            Sh[i1, j] := TShape.Create(Self);
            Sh[i2, j] := TShape.Create(Self);
            CreateShape(Sh[i1, j], i1, j, -15);
            CreateShape(Sh[i2, j], i2, j, -15);
            Sh[i, j].Tag := 15;
            exit;
          end;

          if (Sh[i, j].Tag = 15) and (Sh[i, j11] = nil) and
            (Sh[i, j22] = nil) and (j < j9) then
          begin
            Sh[i, j1].Free;
            Sh[i2, j].Free;
            Sh[i, j1] := nil;
            Sh[i2, j] := nil;
            Sh[i, j11] := TShape.Create(Self);
            Sh[i, j22] := TShape.Create(Self);
            CreateShape(Sh[i, j11], i, j11, -22);
            CreateShape(Sh[i, j22], i, j22, -22);
            Sh[i, j].Tag := 22;
            exit;
          end;

          if (Sh[i, j].Tag = 22) and (Sh[i11, j] = nil) and
            (Sh[i22, j] = nil) and (i < i19) then
          begin
            Sh[i, j22].Free;
            Sh[i1, j].Free;
            Sh[i, j22] := nil;
            Sh[i1, j] := nil;
            Sh[i22, j] := TShape.Create(Self);
            Sh[i11, j] := TShape.Create(Self);
            CreateShape(Sh[i22, j], i22, j, -1);
            CreateShape(Sh[i11, j], i11, j, -1);
            Sh[i, j].Tag := 1;
            exit;
          end;

          if (Sh[i, j].Tag = 2) and (Sh[i, j2] = nil) and (Sh[i1, j] = nil) and
            (j > 2) and (i > 1) then
          begin
            Sh[i11, j].Free;
            Sh[i22, j].Free;
            Sh[i11, j] := nil;
            Sh[i22, j] := nil;
            Sh[i, j2] := TShape.Create(Self);
            Sh[i1, j] := TShape.Create(Self);
            CreateShape(Sh[i, j2], i, j2, -9);
            CreateShape(Sh[i1, j], i1, j, -9);
            Sh[i, j].Tag := 9;
            exit;
          end;

          if (Sh[i, j].Tag = 9) and (Sh[i, j11] = nil) and
            (Sh[i2, j] = nil) and (i > 2) and (j <= j9) then
          begin
            Sh[i, j11] := TShape.Create(Self);
            Sh[i2, j] := TShape.Create(Self);
            CreateShape(Sh[i, j11], i, j11, -16);
            CreateShape(Sh[i2, j], i2, j, -16);
            Sh[i, j].Tag := 16;
            Sh[i, j1].Free;
            Sh[i, j2].Free;
            Sh[i, j1] := nil;
            Sh[i, j2] := nil;
            exit;
          end;

          if (Sh[i, j].Tag = 16) and (Sh[i11, j] = nil) and
            (Sh[i, j22] = nil) and (j < j9) and (i <= i19) then
          begin
            Sh[i1, j].Free;
            Sh[i2, j].Free;
            Sh[i1, j] := nil;
            Sh[i2, j] := nil;
            Sh[i, j22] := TShape.Create(Self);
            Sh[i11, j] := TShape.Create(Self);
            CreateShape(Sh[i, j22], i, j22, -23);
            CreateShape(Sh[i11, j], i11, j, -23);
            Sh[i, j].Tag := 23;
            exit;
          end;

          if (Sh[i, j].Tag = 23) and (Sh[i22, j] = nil) and
            (Sh[i, j1] = nil) and (i < i19) and (j > 1) then
          begin
            Sh[i, j11].Free;
            Sh[i, j22].Free;
            Sh[i, j11] := nil;
            Sh[i, j22] := nil;
            Sh[i22, j] := TShape.Create(Self);
            Sh[i, j1] := TShape.Create(Self);
            CreateShape(Sh[i22, j], i22, j, -2);
            CreateShape(Sh[i, j1], i, j1, -2);
            Sh[i, j].Tag := 2;
            exit;
          end;

          if (Sh[i, j].Tag = 3) and (Sh[i11, j] = nil) and (i <= i19) then
          begin
            Sh[i, j1].Free;
            Sh[i, j1] := nil;
            Sh[i11, j] := TShape.Create(Self);
            CreateShape(Sh[i11, j], i11, j, -10);
            Sh[i, j].Tag := 10;
            exit;
          end;

          if (Sh[i, j].Tag = 10) and (Sh[i, j1] = nil) and (j > 1) then
          begin
            Sh[i1, j].Free;
            Sh[i1, j] := nil;
            Sh[i, j1] := TShape.Create(Self);
            CreateShape(Sh[i, j1], i, j1, -17);
            Sh[i, j].Tag := 17;
            exit;
          end;

          if (Sh[i, j].Tag = 17) and (Sh[i1, j] = nil) and (i > 1) then
          begin
            Sh[i, j11].Free;
            Sh[i, j11] := nil;
            Sh[i1, j] := TShape.Create(Self);
            CreateShape(Sh[i1, j], i1, j, -24);
            Sh[i, j].Tag := 24;
            exit;
          end;

          if (Sh[i, j].Tag = 24) and (Sh[i, j11] = nil) and (j <= j9) then
          begin
            Sh[i11, j].Free;
            Sh[i11, j] := nil;
            Sh[i, j11] := TShape.Create(Self);
            CreateShape(Sh[i, j11], i, j11, -3);
            Sh[i, j].Tag := 3;
            exit;
          end;

          if (Sh[i, j].Tag = 4) and (Sh[i1, j1] = nil) and
            (Sh[i1, j] = nil) and (j > 1) and (i > 1) then
          begin
            Sh[i1, j1] := TShape.Create(Self);
            Sh[i1, j] := TShape.Create(Self);
            CreateShape(Sh[i1, j1], i1, j1, -11);
            CreateShape(Sh[i1, j], i1, j, -11);
            Sh[i1, j11].Free;
            Sh[i11, j].Free;
            Sh[i1, j11] := nil;
            Sh[i11, j] := nil;
            Sh[i, j].Tag := 11;
            exit;
          end;

          if (Sh[i, j].Tag = 11) and (Sh[i, j1] = nil) and
            (Sh[i11, j1] = nil) and (i < FRows) then
          begin
            Sh[i, j1] := TShape.Create(Self);
            Sh[i11, j1] := TShape.Create(Self);
            CreateShape(Sh[i, j1], i, j1, -18);
            CreateShape(Sh[i11, j1], i11, j1, -18);
            Sh[i1, j1].Free;
            Sh[i, j11].Free;
            Sh[i1, j1] := nil;
            Sh[i, j11] := nil;
            Sh[i, j].Tag := 18;
            exit;
          end;

          if (Sh[i, j].Tag = 18) and (Sh[i11, j] = nil) and
            (Sh[i11, j11] = nil) and (j < FColumns) then
          begin
            Sh[i11, j] := TShape.Create(Self);
            Sh[i11, j11] := TShape.Create(Self);
            CreateShape(Sh[i11, j], i11, j, -25);
            CreateShape(Sh[i11, j11], i11, j11, -25);
            Sh[i11, j1].Free;
            Sh[i1, j].Free;
            Sh[i11, j1] := nil;
            Sh[i1, j] := nil;
            Sh[i, j].Tag := 25;
            exit;
          end;

          if (Sh[i, j].Tag = 25) and (Sh[i, j11] = nil) and
            (Sh[i1, j11] = nil) and (j < j9) then
          begin
            Sh[i, j11] := TShape.Create(Self);
            Sh[i1, j11] := TShape.Create(Self);
            CreateShape(Sh[i, j11], i, j11, -4);
            CreateShape(Sh[i1, j11], i1, j + 1, -4);
            Sh[i, j1].Free;
            Sh[i11, j11].Free;
            Sh[i, j1] := nil;
            Sh[i11, j11] := nil;
            Sh[i, j].Tag := 4;
            exit;
          end;

          if (Sh[i, j].Tag = 5) and (Sh[i, j11] = nil) and
            (Sh[i11, j1] = nil) and (j < FColumns) then
          begin
            Sh[i, j11] := TShape.Create(Self);
            Sh[i11, j1] := TShape.Create(Self);
            CreateShape(Sh[i, j11], i, j11, -12);
            CreateShape(Sh[i11, j1], i11, j1, -12);
            Sh[i, j1].Free;
            Sh[i1, j1].Free;
            Sh[i, j1] := nil;
            Sh[i1, j1] := nil;
            Sh[i, j].Tag := 12;
            exit;
          end;

          if (Sh[i, j].Tag = 12) and (Sh[i1, j] = nil) and
            (Sh[i11, j11] = nil) and (i > 1) then
          begin
            Sh[i1, j] := TShape.Create(Self);
            Sh[i11, j11] := TShape.Create(Self);
            CreateShape(Sh[i1, j], i1, j, -19);
            CreateShape(Sh[i11, j11], i11, j11, -19);
            Sh[i11, j1].Free;
            Sh[i11, j].Free;
            Sh[i11, j1] := nil;
            Sh[i11, j] := nil;
            Sh[i, j].Tag := 19;
            exit;
          end;

          if (Sh[i, j].Tag = 19) and (Sh[i1, j11] = nil) and
            (Sh[i, j1] = nil) and (i > 1) then
          begin
            Sh[i1, j11] := TShape.Create(Self);
            Sh[i, j1] := TShape.Create(Self);
            CreateShape(Sh[i1, j11], i1, j11, -26);
            CreateShape(Sh[i, j1], i, j1, -26);
            Sh[i, j11].Free;
            Sh[i11, j11].Free;
            Sh[i, j11] := nil;
            Sh[i11, j11] := nil;
            Sh[i, j].Tag := 26;
            exit;
          end;

          if (Sh[i, j].Tag = 26) and (Sh[i11, j] = nil) and
            (Sh[i1, j1] = nil) and (i < FRows) then
          begin
            Sh[i11, j] := TShape.Create(Self);
            Sh[i1, j1] := TShape.Create(Self);
            CreateShape(Sh[i11, j], i11, j, -5);
            CreateShape(Sh[i1, j1], i1, j1, -5);
            Sh[i1, j].Free;
            Sh[i1, j11].Free;
            Sh[i1, j] := nil;
            Sh[i1, j11] := nil;
            Sh[i, j].Tag := 5;
            exit;
          end;

          if (Sh[i, j].Tag = 6) and (Sh[i1, j] = nil) and
            (Sh[i11, j] = nil) and (Sh[i22, j] = nil) and (i in [2..i18]) then
          begin
            Sh[i1, j] := TShape.Create(Self);
            Sh[i11, j] := TShape.Create(Self);
            Sh[i22, j] := TShape.Create(Self);
            CreateShape(Sh[i1, j], i1, j, -13);
            CreateShape(Sh[i11, j], i11, j, -13);
            CreateShape(Sh[i22, j], i22, j, -13);
            Sh[i, j1].Free;
            Sh[i, j11].Free;
            Sh[i, j22].Free;
            Sh[i, j1] := nil;
            Sh[i, j11] := nil;
            Sh[i, j22] := nil;
            Sh[i, j].Tag := 13;
            exit;
          end;

          if (Sh[i, j].Tag = 13) and (Sh[i, j1] = nil) and
            (Sh[i, j11] = nil) and (Sh[i, j2] = nil) and (j in [3..j9]) then
          begin
            Sh[i, j1] := TShape.Create(Self);
            Sh[i, j11] := TShape.Create(Self);
            Sh[i, j2] := TShape.Create(Self);
            CreateShape(Sh[i, j1], i, j1, -20);
            CreateShape(Sh[i, j11], i, j11, -20);
            CreateShape(Sh[i, j2], i, j2, -20);
            Sh[i1, j].Free;
            Sh[i11, j].Free;
            Sh[i22, j].Free;
            Sh[i1, j] := nil;
            Sh[i11, j] := nil;
            Sh[i22, j] := nil;
            Sh[i, j].Tag := 20;
            exit;
          end;

          if (Sh[i, j].Tag = 20) and (Sh[i1, j] = nil) and
            (Sh[i11, j] = nil) and (Sh[i2, j] = nil) and (i in [3..i19]) then
          begin
            Sh[i1, j] := TShape.Create(Self);
            Sh[i11, j] := TShape.Create(Self);
            Sh[i2, j] := TShape.Create(Self);
            CreateShape(Sh[i1, j], i1, j, -27);
            CreateShape(Sh[i11, j], i11, j, -27);
            CreateShape(Sh[i2, j], i2, j, -27);
            Sh[i, j2].Free;
            Sh[i, j1].Free;
            Sh[i, j11].Free;
            Sh[i, j2] := nil;
            Sh[i, j1] := nil;
            Sh[i, j11] := nil;
            Sh[i, j].Tag := 27;
            exit;
          end;

          if (Sh[i, j].Tag = 27) and (Sh[i, j1] = nil) and
            (Sh[i, j11] = nil) and (Sh[i, j22] = nil) and (j in [2..j8]) then
          begin
            Sh[i, j1] := TShape.Create(Self);
            Sh[i, j11] := TShape.Create(Self);
            Sh[i, j22] := TShape.Create(Self);
            CreateShape(Sh[i, j1], i, j1, -6);
            CreateShape(Sh[i, j11], i, j11, -6);
            CreateShape(Sh[i, j22], i, j22, -6);
            Sh[i1, j].Free;
            Sh[i2, j].Free;
            Sh[i11, j].Free;
            Sh[i1, j] := nil;
            Sh[i2, j] := nil;
            Sh[i11, j] := nil;
            Sh[i, j].Tag := 6;
            exit;
          end;
        end;
end;

procedure TTetris.DropDown;
begin
  Timer1.Interval := 20;
end;

procedure TTetris.SetTetrisKeyDown(Value: word);
begin
  FTetrisKeyDown := Value;
  case FTetrisKeyDown of
    VK_NumPad2: DropDown;
    VK_NumPad4: Left;
    VK_NumPad5: Rotate;
    VK_NumPad6: Right;
  end;
end;

procedure TTetris.Paint;
begin
  inherited Paint;
  if not FFirstFocus then
  begin
    if CanFocus then
      SetFocus;
    FFirstFocus := True;
  end;
  if assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TTetris.Resize;
var
  i, j: integer;
begin
  inherited Resize;
  if Panel1 <> nil then
    if FStart = False then
    begin
      Panel1.Height := 0;
      exit;
    end;
  if (FRows = 0) or (FColumns = 0) then
    exit;
  he := Trunc(Height div FRows);
  wi := Trunc(Width div FColumns);
  x := (Width - FColumns * wi) div 2;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        Sh[i, j].SetBounds(wi * Pred(j) + x, he * Pred(i), wi, he);
  if Panel1 <> nil then
    Panel1.SetBounds(0, FRows * he, Width, Height - FRows * he);
end;

{$ifdef windows}
procedure TTetris.WMNCHitTest(var Mes: TWMNCHitTest);
begin
  inherited;
  if Mes.Result = htClient then
    Mes.Result := htCaption;
  SetFocus;
end;
{$endif}

{ A r c a n o i d }

constructor TArkanoid.Create(AOwner: TComponent);
var
  i, j: integer;
begin
  inherited Create(AOwner);
  Randomize;
  FStart := False;
  Timer1 := nil;
  Timer2 := nil;
  for i := 1 to 48 do
    for j := 1 to 24 do
      Sh[i, j] := nil;
  ShT := nil;
  ShB := nil;
  ShF := nil;
  ShS := nil;
end;

destructor TArkanoid.Destroy;
var
  i, j: integer;
begin
  if Timer1 <> nil then
  begin
    Timer1.Free;
    Timer1 := nil;
  end;
  if Timer2 <> nil then
  begin
    Timer2.Free;
    Timer2 := nil;
  end;
  for i := 1 to 48 do
    for j := 1 to 24 do
      if Sh[i, j] <> nil then
      begin
        Sh[i, j].Free;
        Sh[i, j] := nil;
      end;
  if ShT <> nil then
  begin
    ShT.Free;
    ShT := nil;
  end;
  if ShB <> nil then
  begin
    ShB.Free;
    ShB := nil;
  end;
  if ShF <> nil then
  begin
    ShF.Free;
    ShF := nil;
  end;
  if ShS <> nil then
  begin
    ShS.Free;
    ShS := nil;
  end;
  inherited Destroy;
end;

function Signum(A: double): shortint;
begin
  Result := 0;
  if A > 0.0 then
    Result := 1
  else if A = 0.0 then
    Result := 0
  else if A < 0.0 then
    Result := -1;
end;

procedure TArkanoid.Delete;
begin
  if (ShF.Visible = False) and (ShS.Visible = False) then
  begin
    n := Random(CSS);
    if n <= 7 then
    begin
      if AclYel then
        if (n = 0) or (n = 7) then
          n := 2;
      case n of
        0: ShF.Brush.Color := clLime;
        1: ShF.Brush.Color := clYellow;
        2: ShF.Brush.Color := clRed;
        3: ShF.Brush.Color := clBlue;
        4: ShF.Brush.Color := clPurple;
        5: ShF.Brush.Color := clFuchsia;
        6: ShF.Brush.Color := clAqua;
        7: ShF.Brush.Color := clSilver;
      end;
      ShF.Show;
      ShF.Top := 0;
      FY := 0.0;
      FX := 0.25 + Random * 0.5;
      ShF.Left := Round(Width * FX);
      SpFY := Abs(SpTA * (SSpeed / 100));
      SpFX := 0.0;
    end;
  end;
  ShF.SendToBack;
  if assigned(FOnDelete) then
    FOnDelete(Self);
end;

procedure TArkanoid.Contact;
begin
  if assigned(FOnContact) then
    FOnContact(Self);
end;

procedure TArkanoid.NewBall;
begin
  if assigned(FOnNewBall) then
    FOnNewBall(Self);
end;

procedure TArkanoid.SetColorShapes(Value: TColorTAFShapes);
var
  i, j: integer;
begin
  if FColorShapes = Value then
    exit;
  FColorShapes := Value;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        ShapesColors(i, j);
end;

procedure TArkanoid.SetOneColorShapes(Value: TColor);
var
  i, j: integer;
begin
  if FOneColorShapes = Value then
    exit;
  FOneColorShapes := Value;
  if FColorShapes = ColorOne then
    for i := 1 to FRows do
      for j := 1 to FColumns do
        if Sh[i, j] <> nil then
          ShapesColors(i, j);
end;

procedure TArkanoid.SetColorShapesPen(Value: TColor);
var
  i, j: integer;
begin
  if FColorShapesPen <> Value then
    FColorShapesPen := Value;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        Sh[i, j].Pen.Color := FColorShapesPen;
end;

procedure TArkanoid.SetShapesShape(Value: TShapeType);
var
  i, j: integer;
begin
  if FShapesShape <> Value then
    FShapesShape := Value;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        Sh[i, j].Shape := FShapesShape;
end;

procedure TArkanoid.SetBalls(Value: integer);
begin
  if FBalls <> Value then
    FBalls := Value;
end;

procedure TArkanoid.SetDelay(Value: integer);
begin
  if FDelay <> Value then
    FDelay := Value;
  if Timer1 <> nil then
    Timer1.Interval := FDelay;
end;

procedure TArkanoid.SetSteps(Value: integer);
begin
  if FSteps <> Value then
    FSteps := Value;
end;

procedure TArkanoid.SetFriction(Value: word);
begin
  if FFriction <> Value then
    FFriction := Value;
end;

procedure TArkanoid.ShapesColors(i, j: integer);
var
  c: integer;
begin
  case FColorShapes of
    ColorOne: Sh[i, j].Brush.Color := FOneColorShapes;
    ColorRandom: Sh[i, j].Brush.Color :=
        RGB(Random(256), Random(256), Random(256));
    ColorFixed2:
    begin
      c := i - 7 * Trunc(i / 7);
      case c of
        0: Sh[i, j].Brush.Color := clSilver;
        1: Sh[i, j].Brush.Color := clAqua;
        2: Sh[i, j].Brush.Color := clRed;
        3: Sh[i, j].Brush.Color := clLime;
        4: Sh[i, j].Brush.Color := clYellow;
        5: Sh[i, j].Brush.Color := clBlue;
        6: Sh[i, j].Brush.Color := clFuchsia;
      end;
    end;
    colorFixed:
    begin
      c := i - 7 * Trunc(i / 7);
      case c of
        1: Sh[i, j].Brush.Color := clMaroon;
        2: Sh[i, j].Brush.Color := clGreen;
        3: Sh[i, j].Brush.Color := clOlive;
        4: Sh[i, j].Brush.Color := clNavy;
        5: Sh[i, j].Brush.Color := clPurple;
        6: Sh[i, j].Brush.Color := clTeal;
        0: Sh[i, j].Brush.Color := clGray;
      end;
    end;
  end;
end;

procedure TArkanoid.CreateShapes;
var
  i, j: integer;
begin
  case FLevel of
    1:
      for i := FRows div 2 - 8 to FRows div 2 - 3 do
        for j := 1 to FColumns do
          if (i > 2) and (Sh[i, j] = nil) then
            Sh[i, j] := TShape.Create(Self);
    2:
    begin
      for i := FRows div 4 - 3 to FRows div 4 do
        for j := 1 to FColumns do
          if (i > 2) and (j > 0) and (Sh[i, j] = nil) then
            Sh[i, j] := TShape.Create(Self);
      for i := FRows div 2 - 4 to FRows div 2 - 1 do
        for j := 1 to FColumns do
          if Sh[i, j] = nil then
            Sh[i, j] := TShape.Create(Self);
    end;

    3:
      for i := 2 to FRows div 2 do
        for j := 1 to FColumns do
          if (Odd(i)) and (Odd(j)) then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);

    4:
      for i := 3 to FRows div 2 - 1 do
        for j := 1 to FColumns do
          if not Odd(j) then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);
    5:
      for i := 3 to FRows div 2 - 1 do
        for j := 1 to FColumns do
          if Odd(j) then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);
    6:
      for i := 3 to FRows div 2 - 1 do
        for j := 1 to FColumns do
          if (Odd(j) and (not Odd(i))) or (Odd(i) and (not Odd(j))) then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);
    7:
      for i := 2 to (FRows div 2) do
        for j := 1 to FColumns do
          if not Odd(i) then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);
    8:
      for i := 2 to FRows div 2 do
        for j := 1 to FColumns do
          if Random(2) = 1 then
            if Sh[i, j] = nil then
              Sh[i, j] := TShape.Create(Self);

    9:
      for i := 3 to FRows div 2 - 1 do
        for j := 2 to FColumns - 1 do
          if Sh[i, j] = nil then
            Sh[i, j] := TShape.Create(Self);
    10:
      for i := 1 to FRows do
        for j := 1 to FColumns do
          if (i = j) or (i = FColumns - j + 1) or
            ((j = 1) and (i <= FColumns)) or (i = 1) or
            ((j = FColumns) and (i <= FColumns)) or (i = FColumns) then
            if i + 2 <= FRows - 4 then
              if Sh[i, j] = nil then
                Sh[i + 2, j] := TShape.Create(Self);
  end;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
      begin
        Sh[i, j].Hide;
        Sh[i, j].Parent := Self;
      end;
  if ShT = nil then
  begin
    ShT := TShape.Create(Self);
    ShT.Parent := Self;
    ShT.Shape := stEllipse;
  end;
  if ShB = nil then
  begin
    ShB := TShape.Create(Self);
    ShB.Parent := Self;
    ShB.Shape := stCircle;
  end;
  if ShF = nil then
  begin
    ShF := TShape.Create(Self);
    ShF.Parent := Self;
    ShF.Shape := stSquare;
  end;
  if ShS = nil then
  begin
    ShS := TShape.Create(Self);
    ShS.Parent := Self;
  end;
  ShF.Hide;
  ShS.Hide;
end;

procedure TArkanoid.NextLevel;
var
  i, j: integer;
begin
  SpTA := 1.0 / FSteps;
  CSS := 8 * FCreSquShape;
  AClYel := False;
  AClLim := False;
  AClBlu := False;
  AClPur := False;
  BllUp := True;
  Fr := True;
  CreateShapes;
  he := Trunc(Height / FRows);
  wi := Trunc(Width / FColumns);
  x := (Width - FColumns * wi) div 2;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        with Sh[i, j] do
        begin
          Shape := FShapesShape;
          Brush.Style := FShapesBrushStyle;
          Pen.Color := FColorShapesPen;
          Pen.Width := FWidthShapesPen;
          ShapesColors(i, j);
          SetBounds(wi * Pred(j) + x, he * Pred(i), wi, he);
          Show;
        end;
  ShT.Width := Round(wi * (FTN / 100));
  ShT.Height := he div 2;
  TX := 0.25 + 0.5 * random;
  ShT.Left := Round(TX * Width);
  ShT.Top := Round(Height * Pred(FRows) div FRows);
  ShB.Height := Round(he * (BHW / 100));
  ShB.Width := Round(he * (BHW / 100));
  BW2 := ShB.Width div 2;
  BH2 := ShB.Height div 2;
  ShB.Brush.Color := FColorBl;
  ShT.Brush.Color := FColorT;
  ShF.Brush.Style := bsSolid;
  ShF.Pen.Color := Color;
  ShT.Pen.Color := Color;
  ShB.Pen.Color := Color;
  if he < 6 then
  begin
    ShF.Pen.Color := ShF.Brush.Color;
    ShT.Pen.Color := ShT.Brush.Color;
    ShB.Pen.Color := ShB.Brush.Color;
  end;
  BX := TX + 0.5 * wi / Width;
  BY := (ShT.Top - ShB.Height) / Height;
  ShB.Left := Trunc(BX * Width);
  ShB.Top := Trunc(Height * BY);
  ShF.Width := ShB.Width;
  ShF.Height := ShB.Height;
  SpB := SpTA * (BSpeed / 100);
  SpBX := 0.0;
  SpBY := 0.0;
  SpT := 0.0;
  SpFY := 0.0;
  SpFX := 0.0;
  ShS.Top := Height - (FRows div 3) * he;
  ShS.Height := 2;
  ShS.Width := Width;
  ShS.Hide;
  Timer1.Enabled := True;
  Timer2.Enabled := True;
  ArcStart := False;
end;

procedure TArkanoid.SetStart(Value: boolean);
begin
  if FStart = Value then
    exit;
  FStart := Value;
  n := 0;
  ni := 0;
  if FStart = True then
  begin
    FFirstFocus := False;
    if Timer1 = nil then
      Timer1 := TTimer.Create(Self);
    if Timer2 = nil then
      Timer2 := TTimer.Create(Self);
    Timer1.Enabled := False;
    Timer1.Interval := FDelay;
    Timer1.OnTimer := TimerOnTimer;
    Timer2.Enabled := False;
    Timer2.Interval := 1000;
    Timer2.OnTimer := Timer2OnTimer;
    FBll := FBalls;
    FLevel := 1;
    Align := alClient;
    NextLevel;
  end;
end;

procedure TArkanoid.TimerOnTimer(Sender: TObject);
var
  D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, r, c, LT, TP, i, j: integer;
  SpTmp: single;
begin
  BX := BX + SpBX;
  BY := BY + SpBY;
  D1 := Round(BX * Width);
  D2 := Round(Height * BY);
  D3 := ShB.Width;
  D4 := ShB.Height;
  TX := TX + SpT;
  D5 := Round(TX * Width);
  D6 := ShT.Top;
  D7 := ShT.Width;
  D8 := ShT.Height;
  ShB.SetBounds(D1, D2, D3, D4);
  ShT.SetBounds(D5, D6, D7, D8);
  if ShF.Visible then
  begin
    FY := FY + SpFY;
    FX := FX + SpFX;
    D1 := Round(FX * Width);
    D2 := Round(FY * Height);
    D3 := ShF.Width;
    D4 := ShF.Height;
    ShF.SetBounds(D1, D2, D3, D4);
    if (ShF.Top < 0) or (ShF.Top + ShF.Height >= Height) then
    begin
      ShF.Hide;
      ShF.Brush.Color := clWhite;
      ShF.Brush.Style := bsSolid;
      ShF.Top := 0;
    end;
  end;
  D1 := ShT.Width div 12;
  if (ShT.Left + D1 <= 0) or ((ShT.Left + ShT.Width - D1) >= Width) then
    if not FEndStop then
    begin
      SpT := -SpT;
      if (not ArcStart) or (not BllUp) then
        SpBX := SpT;
      if (AClLim) and (not Fr) then
        SpFX := SpT;
    end
    else
      Stop;
  if ArcStart and ((not AClYel) or ((AClYel) and (BllUp))) then
  begin
    if (ShB.Left <= 0) then
      SpBX := abs(SpBX)
    else
    if ((ShB.Left + ShB.Width) >= Width) then
      SpBX := -abs(SpBX);
    if ShS.Visible then
      if ((BUpShS) and (ShB.Top + ShB.Height >= ShS.Top)) or
        ((not BUpShS) and (ShB.Top <= ShS.Top + ShS.Height)) then
        SpBY := -SpBY;
    if (ShB.Top + ShB.Height) >= Height then
    begin
      EndFlying;
      exit;
    end;
    if ShB.Top <= 0 then
      SpBY := -SpBY;
  end;
  D1 := ShB.Left + BW2;
  D2 := ShB.Top + BH2;
  D3 := ShB.Top + ShB.Height;
  D4 := D3 - Trunc(SpBY * Height);
  D5 := ShB.Top - Trunc(SpBY * Height);
  D7 := ShB.Left + ShB.Width;
  D8 := D7 - Trunc(SpBX * Width);
  D9 := ShB.Left - Trunc(SpBX * Width);
  c := D1 div wi + 1;
  r := D2 div he + 1;
  for i := r + 1 downto r - 1 do
    if i in [1..FRows] then
      for j := c - 1 to c + 1 do
        if j in [1..FColumns] then
          if Sh[i, j] <> nil then
            if Sh[i, j].Brush.Color <> clBtnHighLight then
            begin
              D6 := Sh[i, j].Top + he;
              D10 := Sh[i, j].Left + wi;
              if ((Sh[i, j].Left - BW2) <= D1) and (D10 + BW2 >= D1) then
                if ((D6 >= ShB.Top) and (D6 <= D5) and
                  (SpBY < 0)) or ((Sh[i, j].Top <= D3) and
                  (Sh[i, j].Top >= D4) and (SpBY > 0)) then
                  if not StrongShapes then
                  begin
                    if not ImmDelShapes then
                    begin
                      Sh[i, j].Brush.Color := clBtnHighlight;
                      Sh[i, j].Pen.Color := Color;
                      Sh[i, j].Brush.Style := bsDiagCross;
                      SpBY := -SpBY;
                      Delete;
                      break;
                    end
                    else
                    begin
                      Sh[i, j].Free;
                      Sh[i, j] := nil;
                      SpBY := -SpBY;
                      Delete;
                      break;
                    end;
                  end
                  else
                  if Sh[i, j].Brush.Color = clBlack then
                  begin
                    Sh[i, j].Brush.Color := clGray;
                    SpBY := -SpBY;
                    break;
                  end
                  else
                  if Sh[i, j].Brush.Color = clGray then
                  begin
                    Sh[i, j].Brush.Color := clSilver;
                    SpBY := -SpBY;
                    break;
                  end
                  else
                  if Sh[i, j].Brush.Color = clSilver then
                  begin
                    Sh[i, j].Brush.Color :=
                      RGB(Random(256), Random(256), Random(256));
                    SpBY := -SpBY;
                    break;
                  end
                  else
                  if not ImmDelShapes then
                  begin
                    Sh[i, j].Brush.Color := clBtnHighlight;
                    Sh[i, j].Pen.Color := Color;
                    Sh[i, j].Brush.Style := bsDiagCross;
                    SpBY := -SpBY;
                    Delete;
                    break;
                  end
                  else
                  begin
                    Sh[i, j].Free;
                    Sh[i, j] := nil;
                    SpBY := -SpBY;
                    Delete;
                    break;
                  end;
              if ((Sh[i, j].Top - BH2) <= D2) and ((D6 + BH2) >= D2) then
                if ((D10 >= ShB.Left) and (D10 <= D9) and (SpBX < 0)) or
                  ((Sh[i, j].Left <= D7) and (Sh[i, j].Left >= D8) and (SpBX > 0)) then
                  if not StrongShapes then
                  begin
                    if not ImmDelShapes then
                    begin
                      Sh[i, j].Brush.Color := clBtnHighlight;
                      Sh[i, j].Pen.Color := Color;
                      Sh[i, j].Brush.Style := bsCross;
                      SpBX := -SpBX;
                      Delete;
                      break;
                    end
                    else
                    begin
                      Sh[i, j].Free;
                      Sh[i, j] := nil;
                      SpBX := -SpBX;
                      Delete;
                      break;
                    end;
                  end
                  else
                  if Sh[i, j].Brush.Color = clBlack then
                  begin
                    Sh[i, j].Brush.Color := clGray;
                    SpBX := -SpBX;
                    break;
                  end
                  else
                  if Sh[i, j].Brush.Color = clGray then
                  begin
                    Sh[i, j].Brush.Color := clSilver;
                    SpBX := -SpBX;
                    break;
                  end
                  else
                  if Sh[i, j].Brush.Color = clSilver then
                  begin
                    Sh[i, j].Brush.Color :=
                      RGB(Random(256), Random(256), Random(256));
                    SpBX := -SpBX;
                    break;
                  end
                  else
                  if not ImmDelShapes then
                  begin
                    Sh[i, j].Brush.Color := clBtnHighlight;
                    Sh[i, j].Pen.Color := Color;
                    Sh[i, j].Brush.Style := bsCross;
                    SpBX := -SpBX;
                    Delete;
                    break;
                  end
                  else
                  begin
                    Sh[i, j].Free;
                    Sh[i, j] := nil;
                    SpBX := -SpBX;
                    Delete;
                    break;
                  end;
            end;
  if ((ShT.Left) <= D1) and ((ShT.Left + Sht.Width) >= D1) then
    if (ShT.Top <= D3) and (ShT.Top >= D4) and (SpBY > 0) then
    begin
      if AClYel then
      begin
        SpBYT := SpBY;
        SpBXT := SpBX;
        SpBY := 0.0;
        SpBX := SpT;
        BllUp := False;
      end
      else
      begin
        if (SpT <= 0.0) and (SpBX >= 0.0) and (ShB.Left <= ShT.Left + BW2) then
        begin
          SpTmp := SpBY;
          SpBY := -SpBX;
          SpBX := -SpTmp;
        end
        else
        if (SpT >= 0.0) and (SpBX <= 0.0) and (ShB.Left >=
          ShT.Left + ShT.Width - ShB.Width) then
        begin
          SpTmp := SpBY;
          SpBY := SpBX;
          SpBX := SpTmp;
        end
        else
        begin
          SpBX := SpBX + SpT * (FFriction / 100);
          if Abs(SpBX) >= 0.95 * SpB then
            SpBX := Signum(SpBX) * SpB * 0.95
          else
          if Abs(SpBX) <= 0.05 * SpB then
            SpBX := Signum(SpBX) * SpB * 0.05;
          SpBY := -Signum(SpBY) * Sqrt(Sqr(SpB) - Sqr(SpBX));
        end;
      end;
      if ShT.Visible = False then
        ShT.Show;
    end;
  if (AClLim) and (SpFY < 0) then
  begin
    D1 := ShF.Left + BW2;
    D2 := ShF.Top + BH2;
    D5 := ShF.Top - Trunc(SpFY * Height);
    c := D1 div wi + 1;
    r := D2 div he + 1;
    for i := r + 1 downto r - 1 do
      if i in [1..FRows] then
        for j := c - 1 to c + 1 do
          if j in [1..FColumns] then
            if Sh[i, j] <> nil then
              if Sh[i, j].Brush.Color <> clBtnHighLight then
              begin
                D6 := Sh[i, j].Top + he;
                D10 := Sh[i, j].Left + wi;
                if (D1 >= Sh[i, j].Left - BW2) and (D1 <= (D10 + BW2)) then
                  if (D6 >= ShF.Top) and (D6 <= D5) then
                    if not StrongShapes then
                    begin
                      if not ImmDelShapes then
                      begin
                        Sh[i, j].Brush.Color := clBtnHighlight;
                        Sh[i, j].Pen.Color := Color;
                        Sh[i, j].Brush.Style := bsBDiagonal;
                        SpFY := -SpFY;
                        Delete;
                      end
                      else
                      begin
                        Sh[i, j].Free;
                        Sh[i, j] := nil;
                        SpBY := -SpBY;
                        Delete;
                        break;
                      end;
                    end
                    else
                    if Sh[i, j].Brush.Color = clBlack then
                    begin
                      Sh[i, j].Brush.Color := clGray;
                      SpFY := -SpFY;
                      break;
                    end
                    else
                    if Sh[i, j].Brush.Color = clGray then
                    begin
                      Sh[i, j].Brush.Color := clSilver;
                      SpFY := -SpFY;
                      break;
                    end
                    else
                    if Sh[i, j].Brush.Color = clSilver then
                    begin
                      Sh[i, j].Brush.Color :=
                        RGB(Random(256), Random(256), Random(256));
                      SpFY := -SpFY;
                      break;
                    end
                    else
                    if not ImmDelShapes then
                    begin
                      Sh[i, j].Brush.Color := clBtnHighlight;
                      Sh[i, j].Pen.Color := Color;
                      Sh[i, j].Brush.Style := bsBDiagonal;
                      SpFY := -SpFY;
                      Delete;
                      break;
                    end
                    else
                    begin
                      Sh[i, j].Free;
                      Sh[i, j] := nil;
                      SpFY := -SpFY;
                      Delete;
                      break;
                    end;
              end;
  end;
  if (ShF.Visible) and (SpFY > 0) then
  begin
    D1 := ShF.Left + BW2;
    D3 := ShF.Top + ShF.Height;
    D4 := D3 - Trunc(SpFY * Height);
    if ((ShT.Left) <= D1) and ((ShT.Left + ShT.Width) >= D1) then
      if (ShT.Top <= D3) and (ShT.Top >= D4) then
      begin
        if ShF.Brush.Color = clRed then
        begin
          AClYel := False;
          AClLim := False;
          BY := 1.0;
        end;
        if ShF.Brush.Color = clBlue then
          if AclBlu then
          begin
            AClBlu := False;
            ShT.Width := Round(wi * (FTN / 100));
          end
          else
          begin
            AClBlu := True;
            AClPur := False;
            ShT.Width := Trunc(wi * (FTL / 100));
          end;
        if ShF.Brush.Color = clPurple then
          if AClPur then
          begin
            AClPur := False;
            ShT.Width := Trunc(wi * (FTN / 100));
          end
          else
          begin
            AClPur := True;
            AClBlu := False;
            ShT.Width := Trunc(wi * (FTS / 100));
          end;
        if ShF.Brush.Color = clLime then
        begin
          AClLim := True;
          SpFY := 0.0;
          SpFX := SpT;
          Fr := False;
        end;
        if ShF.Brush.Color = clYellow then
          if AClYel then
          begin
            if not BllUp then
              BSUp;
            AClYel := False;
          end
          else
          begin
            AClYel := True;
            BllUp := True;
          end;
        if ShF.Brush.Color = clAqua then
        begin
          FBll := FBll + 1;
          NewBall;
        end;
        if ShF.Brush.Color = clSilver then
        begin
          ShS.Brush.Color := clSilver;
          if (Color = clSilver) or (Color = clBtnFace) then
            ShS.Brush.Color := clWhite;
          ShS.Pen.Color := ShS.Brush.Color;
          ShS.Show;
          if ShB.Top + ShB.Height < ShS.Top then
            BUpShS := True
          else
            BUpShS := False;
          ni := 0;
          nj := Random(30) + 10;
        end;
        if ShF.Brush.Color = clFuchsia then
          ShT.Hide
        else
          ShT.Show;
        if ShF.Brush.Color <> clLime then
        begin
          ShF.Brush.Color := clWhite;
          if not ImmDelShapes then
            ShF.Brush.Style := bsDiagCross
          else
            ShF.Hide;
        end;
      end;
  end;
end;

procedure TArkanoid.Timer2OnTimer(Sender: TObject);
var
  i, j, k: integer;
begin
  if Timer1.Enabled = False then
    exit;
  if FStart then
    Time;
  k := 0;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
      begin
        Inc(k);
        if Sh[i, j].Brush.Color = clBtnHighlight then
        begin
          Sh[i, j].Free;
          Sh[i, j] := nil;
        end;
      end;
  if k = 0 then
    EndLevel;
end;

procedure TArkanoid.EndFlying;
begin
  ShB.Free;
  ShB := nil;
  FBll := FBll - 1;
  Contact;
  if FBll <= 0 then
  begin
    Over;
  end
  else
  begin
    ShT.Width := Round(wi * (FTN / 100));
    if ShT.Visible = False then
    begin
      ShT.Show;
      TX := 0.25 + 0.5 * random;
      ShT.Left := Round(TX * Width);
    end;
    SpTA := 1.0 / Steps;
    ShB := TShape.Create(Self);
    ShB.Hide;
    ShB.Parent := Self;
    ShB.Shape := stCircle;
    ShB.Pen.Width := FWidthShapesPen;
    ShB.Height := Round(he * (BHW / 100));
    ShB.Width := Round(he * (BHW / 100));
    BW2 := ShB.Width div 2;
    BH2 := ShB.Height div 2;
    ShB.Brush.Color := FColorBl;
    if he < 6 then
      ShB.Pen.Color := Shb.Brush.Color
    else
      ShB.Pen.Color := Color;
    BX := TX + 0.5 * wi / Width;
    BY := (ShT.Top - ShB.Height) / Height;
    ShB.Left := Trunc(BX * Width);
    ShB.Top := Trunc(Height * BY);
    SpB := SpTA * BSpeed / 100;
    SpT := 0.0;
    SpBX := 0.0;
    SpBY := 0.0;
    ShB.Show;
    ShF.Hide;
    ShF.Brush.Color := clWhite;
    ShF.Brush.Style := bsSolid;
    ShF.Top := 0;
    AClYel := False;
    AClLim := False;
    AClBlu := False;
    AClPur := False;
    ShS.Hide;
    BllUp := True;
    ArcStart := False;
    Fr := True;
  end;
end;

procedure TArkanoid.EndLevel;
begin
  FLevel := FLevel + 1;
  if FLevel <= FLevels then
  begin
    if assigned(FOnEndLevel) then
      FOnEndLevel(Self);
    NextLevel;
  end
  else
    Over;
end;

procedure TArkanoid.Time;
begin
  if ShS.Visible then
  begin
    Inc(ni);
    if ni = nj then
      ShS.Hide;
  end;
  if assigned(FOnTime) then
    FOnTime(Self);
end;

procedure TArkanoid.SetPause(Value: boolean);
var
  i, j: integer;
begin
  if (FPause = Value) or (Timer1 = nil) or (Timer2 = nil) then
    exit;
  FPause := Value;
  Timer1.Enabled := not FPause;
  Timer2.Enabled := not FPause;
  if FStart then
    if FPause then
      for i := 1 to FRows do
        for j := 1 to FColumns do
          if Sh[i, j] <> nil then
            if Sh[i, j].Brush.Color = clBtnHighlight then
            begin
              Sh[i, j].Free;
              Sh[i, j] := nil;
            end;
end;

procedure TArkanoid.SetShapesBrushStyle(Value: TBrushStyle);
var
  i, j: integer;
begin
  if FShapesBrushStyle <> Value then
    FShapesBrushStyle := Value;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        Sh[i, j].Brush.Style := FShapesBrushStyle;
end;

procedure TArkanoid.SetStrongShapes(Value: boolean);
begin
  if FStrongShapes <> Value then
    FStrongShapes := Value;
end;

procedure TArkanoid.SetEndStop(Value: boolean);
begin
  if FEndStop <> Value then
    FEndStop := Value;
end;

procedure TArkanoid.SetImmDelShapes(Value: boolean);
begin
  if FImmDelShapes <> Value then
    FImmDelShapes := Value;
end;

procedure TArkanoid.SetTN(Value: integer);
begin
  if FTN <> Value then
    FTN := Value;
end;

procedure TArkanoid.SetCreSquShape(Value: integer);
begin
  if FCreSquShape <> Value then
    FCreSquShape := Value;
  CSS := 8 * FCreSquShape;
end;

procedure TArkanoid.SetTL(Value: integer);
begin
  if FTL <> Value then
    FTL := Value;
end;

procedure TArkanoid.SetTS(Value: integer);
begin
  if FTS <> Value then
    FTS := Value;
end;

procedure TArkanoid.SetBHW(Value: integer);
begin
  if FBHW <> Value then
    FBHW := Value;
end;

procedure TArkanoid.SetWidthShapesPen(Value: integer);
var
  i, j: integer;
begin
  if FWidthShapesPen = Value then
    exit;
  FWidthShapesPen := Value;
  for i := 1 to FColumns do
    for j := 1 to FRows do
      if Sh[i, j] <> nil then
        Sh[i, j].Pen.Width := FWidthShapesPen;
end;

procedure TArkanoid.SetColorBl(Value: TColor);
begin
  if FColorBl = Value then
    exit;
  FColorBl := Value;
  if ShB <> nil then
    ShB.Brush.Color := FColorBl;
end;

procedure TArkanoid.SetColorT(Value: TColor);
begin
  if FColorT = Value then
    exit;
  FColorT := Value;
  if ShT <> nil then
    ShT.Brush.Color := FColorT;
end;

procedure TArkanoid.SetBSpeed(Value: integer);
begin
  if FBSpeed <> Value then
    FBSpeed := Value;
end;

procedure TArkanoid.SetSSpeed(Value: integer);
begin
  if FSSpeed <> Value then
    FSSpeed := Value;
end;

procedure TArkanoid.SetArcanoidKey(Value: TTSAKey);
begin
  if FArcanoidKey <> Value then
    FArcanoidKey := Value;
  case FArcanoidKey of
    Lft: Left;
    Rgh: Right;
    Up: BSUp;
    Stp: Stop;
  end;
end;

procedure TArkanoid.Left;
begin
  SpT := -Abs(SpTA);
  ArcStop := False;
  if ((AClYel) and (not BllUp)) or (not ArcStart) then
    SpBX := SpT;
  if (AClLim) and (not Fr) then
    SpFX := SpT;
end;

procedure TArkanoid.Right;
begin
  SpT := Abs(SpTA);
  ArcStop := False;
  if ((AClYel) and (not BllUp)) or (not ArcStart) then
    SpBX := SpT;
  if (AClLim) and (not Fr) then
    SpFX := SpT;
end;

procedure TArkanoid.Stop;
begin
  SpT := 0.0;
  ArcStop := True;
  if ((AClYel) and (not BllUp)) or (not ArcStart) then
    SpBX := 0.0;
  if (AClLim) and (not Fr) then
    SpFX := 0.0;
end;

procedure TArkanoid.BSUp;
begin
  if not ArcStart then
  begin
    ArcStart := True;
    SpBX := 0.3 + Random * 0.6;
    if SpT < 0 then
      SpBX := -SpBX;
    SpBY := -Sqrt(1.0 - Sqr(SpBX));
    SpBX := SpBX * SpB;
    SpBY := SpBY * SpB;
    exit;
  end;
  if not BllUp then
  begin
    BllUp := True;
    if SpT < 0 then
      SpBX := -Abs(SpBXT)
    else if SpT > 0 then
      SpBX := Abs(SpBXT)
    else if SpT = 0 then
      SpBX := SpBXT;
    SpBY := -SpBYT;
    exit;
  end;
  if (not AClLim) or Fr then
    exit;
  Fr := True;
  SpFX := 0.0;
  SpFY := -Abs(SpTA * (SSpeed / 100));
end;

procedure TArkanoid.SetArcanoidKeyDown(Value: word);
begin
  FArcanoidKeyDown := Value;
  case FArcanoidKeyDown of
    VK_Numpad2: Stop;
    VK_NumPad4: Left;
    VK_NumPad6: Right;
    VK_NumPad8: BSUp;
  end;
end;

procedure TArkanoid.Paint;
begin
  inherited Paint;
  if not FFirstFocus then
  begin
    if CanFocus then
      SetFocus;
    FFirstFocus := True;
  end;
  if assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TArkanoid.Over;
var
  i, j: integer;
begin
  FStart := False;
  if Timer1 <> nil then
  begin
    Timer1.Free;
    Timer1 := nil;
  end;
  if Timer2 <> nil then
  begin
    Timer2.Free;
    Timer2 := nil;
  end;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
      begin
        Sh[i, j].Free;
        Sh[i, j] := nil;
      end;
  if ShT <> nil then
  begin
    ShT.Free;
    ShT := nil;
  end;
  if ShB <> nil then
  begin
    ShB.Free;
    ShB := nil;
  end;
  if ShF <> nil then
  begin
    ShF.Free;
    ShF := nil;
  end;
  if ShS <> nil then
  begin
    ShS.Free;
    ShS := nil;
  end;
  if assigned(FOnOver) then
    FOnOver(Self);
end;

procedure TArkanoid.Resize;
var
  i, j: integer;
begin
  inherited Resize;
  if FStart = False then
    exit;
  he := Trunc(Height div FRows);
  wi := Trunc(Width div FColumns);
  x := (Width - FColumns * wi) div 2;
  for i := 1 to FRows do
    for j := 1 to FColumns do
      if Sh[i, j] <> nil then
        Sh[i, j].SetBounds(wi * Pred(j) + x, he * Pred(i), wi, he);
  if ShB <> nil then
  begin
    ShB.Height := Round(he * (BHW / 100));
    ShB.Width := ShB.Height;
  end;
  if AClBlu then
    ShT.Width := Round(wi * (FTL / 100))
  else
  if AClPur then
    ShT.Width := Round(wi * (FTS / 100))
  else
  if ShT <> nil then
    ShT.SetBounds(ShT.Left, Round(Height * Pred(FRows) div FRows),
      Round(wi * (FTN / 100)), Round(He div 2));
  if ShF <> nil then
  begin
    ShF.Height := Round(he * (BHW / 100));
    ShF.Width := ShF.Height;
    if ShF.Visible then
    begin
      ShF.Top := Round(FY * Height);
      ShF.Left := Round(FX * Width);
    end;
  end;
  if ShS <> nil then
    ShS.SetBounds(0, Height - (FRows div 3) * he, Width, 2);
  if he < 6 then
  begin
    if ShB <> nil then
      ShB.Pen.Color := ShB.Brush.Color;
    if ShF <> nil then
      ShF.Pen.Color := ShF.Brush.Color;
    if ShT <> nil then
      ShT.Pen.Color := ShT.Brush.Color;
  end
  else
  begin
    if ShB <> nil then
      ShB.Pen.Color := Color;
    if ShF <> nil then
      ShF.Pen.Color := Color;
    if ShT <> nil then
      ShT.Pen.Color := Color;
  end;
end;

{$ifdef windows}
procedure TArkanoid.WMNCHitTest(var Mes: TWMNCHitTest);
begin
  inherited;
  if Mes.Result = htClient then
    Mes.Result := htCaption;
  SetFocus;
end;
{$endif}

{ F i g u r e s }

constructor TFigures.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  Fgr1 := nil;
  Fgr2 := nil;
  Fgr3 := nil;
  Fgr4 := nil;
  Fgr5 := nil;
  FFirstFocus := False;
  Timer1 := nil;
  Align := alClient;
  TabStop := True;
  Sq2 := Sqrt(2.0);
end;

destructor TFigures.Destroy;
begin
  if Timer1 <> nil then
    Timer1.Free;
  if Fgr1 <> nil then
    Fgr1.Free;
  if Fgr2 <> nil then
    Fgr2.Free;
  if Fgr3 <> nil then
    Fgr3.Free;
  if Fgr4 <> nil then
    Fgr4.Free;
  if Fgr5 <> nil then
    Fgr5.Free;
  inherited Destroy;
end;

procedure TFigures.Paint;
begin
  inherited Paint;
  if not FFirstFocus then
  begin
    if CanFocus then
      SetFocus;
    FFirstFocus := True;
  end;
  if assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TFigures.SetStart(Value: boolean);
begin
  if FStart <> Value then
    FStart := Value;
  if not FStart then
    exit;
  FMoves := 0;
  if Fgr1 <> nil then
  begin
    Fgr1.Free;
    Fgr1 := nil;
  end;
  if Fgr2 <> nil then
  begin
    Fgr2.Free;
    Fgr2 := nil;
  end;
  if Fgr3 <> nil then
  begin
    Fgr3.Free;
    Fgr3 := nil;
  end;
  if Fgr4 <> nil then
  begin
    Fgr4.Free;
    Fgr4 := nil;
  end;
  if Fgr5 <> nil then
  begin
    Fgr5.Free;
    Fgr5 := nil;
  end;
  if Timer1 = nil then
  begin
    Timer1 := TTimer.Create(Self);
    Timer1.Interval := 1000;
    Timer1.OnTimer := TimerOnTimer;
  end;
  if Height > Width then
    num := Trunc((Width * FWidthHeight) / 100)
  else
    num := Trunc((Height * FWidthHeight) / 100);
  Fgr1 := TFigure.Create(Self);
  with Fgr1 do
  begin
    Parent := Self;
    Fig := 1;
    OnMove := FigureMove;
    Numb := Num;
  end;
  Fgr2 := TFigure.Create(Self);
  with Fgr2 do
  begin
    Parent := Self;
    if FSquare = Big then
      Fig := 2
    else
      Fig := 6;
    OnMove := FigureMove;
    Numb := Num;
  end;
  Fgr3 := TFigure.Create(Self);
  with Fgr3 do
  begin
    Parent := Self;
    Fig := 3;
    OnMove := FigureMove;
    Numb := Num;
  end;
  Fgr4 := TFigure.Create(Self);
  with Fgr4 do
  begin
    Parent := Self;
    Fig := 4;
    OnMove := FigureMove;
    Numb := Num;
  end;
  Fgr5 := TFigure.Create(Self);
  with Fgr5 do
  begin
    Parent := Self;
    Fig := 5;
    OnMove := FigureMove;
    Numb := Num;
  end;
  Colors;
end;

procedure TFigures.Colors;
begin
  case FColorShapes of
    ColorFixed:
    begin
      if Fgr1 <> nil then
        Fgr1.Canvas.Pen.Color := clGreen;
      if Fgr2 <> nil then
        Fgr2.Canvas.Pen.Color := clMaroon;
      if Fgr3 <> nil then
        Fgr3.Canvas.Pen.Color := clOlive;
      if Fgr4 <> nil then
        Fgr4.Canvas.Pen.Color := clNavy;
      if Fgr5 <> nil then
        Fgr5.Canvas.Pen.Color := clPurple;
    end;
    ColorFixed2:
    begin
      if Fgr1 <> nil then
        Fgr1.Canvas.Pen.Color := clAqua;
      if Fgr2 <> nil then
        Fgr2.Canvas.Pen.Color := clRed;
      if Fgr3 <> nil then
        Fgr3.Canvas.Pen.Color := clLime;
      if Fgr4 <> nil then
        Fgr4.Canvas.Pen.Color := clBlue;
      if Fgr5 <> nil then
        Fgr5.Canvas.Pen.Color := clFuchsia;
    end;
  end;
end;

procedure TFigures.SetSquare(Value: TFSquare);
begin
  if Value <> FSquare then
    FSquare := Value;
end;

procedure TFigures.Over;
var
  TempColor: TColor;
begin
  FStart := False;
  if Timer1 <> nil then
  begin
    Timer1.Free;
    Timer1 := nil;
  end;
  TempColor := clYellow;
  if FColorShapes = ColorFixed2 then
    if Color <> clSilver then
      TempColor := clSilver
    else
      TempColor := clYellow;
  if FColorShapes = ColorFixed then
    if Color <> clGray then
      TempColor := clGray
    else
      TempColor := clTeal;
  Fgr1.LineColor := TempColor;
  Fgr2.LineColor := TempColor;
  Fgr3.LineColor := TempColor;
  Fgr4.LineColor := TempColor;
  Fgr5.LineColor := TempColor;
  Fgr1.Over := True;
  Fgr2.Over := True;
  Fgr3.Over := True;
  Fgr4.Over := True;
  Fgr5.Over := True;
  if assigned(FOnOver) then
    FOnOver(Self);
end;

procedure TFigures.SetColorShapes(Value: TColorTAFShapes);
begin
  if FColorShapes = Value then
    exit;
  FColorShapes := Value;
  Colors;
end;

procedure TFigures.SetWidthHeight(Value: word);
begin
  if FWidthHeight = Value then
    exit;
  FWidthHeight := Value;
  Resize;
end;

procedure TFigures.SetPause(Value: boolean);
begin
  if not FStart then
    exit;
  if FPause <> Value then
    FPause := Value;
  if Timer1 <> nil then
    Timer1.Enabled := not FPause;
end;

procedure TFigures.FigureMove(Sender: TObject);
begin
  Inc(FMoves);
  if Assigned(FOnFigureMove) then
    FOnFigureMove(Self);
  Detect;
end;

procedure TFigures.Detect;
var
  a: real;
  F375, F25, F5, F75, F625, Q75, Q125, Q5, Q1, Q25, Q15, x1, y1, x2, y2,
  x3, y3, x4, y4, x5, y5: integer;
  t1, t2, t3, t4, t5: word;

  function Sh(b, c: integer): boolean;
  begin
    Result := Abs(b - c) < 10;
  end;

begin
  a := 0.375 * Num;
  F375 := Trunc(a);
  F75 := 2 * F375;
  a := 0.625 * Num;
  F625 := Trunc(a);
  a := Num * 0.25;
  F25 := Trunc(a);
  F5 := 2 * F25;
  a := a / Sq2;
  Q25 := Trunc(a);
  Q5 := Trunc(2 * a);
  Q75 := Trunc(3 * a);
  Q1 := Trunc(4 * a);
  Q125 := Trunc(5 * a);
  Q15 := Trunc(6 * a);
  x1 := Fgr1.Left;
  y1 := Fgr1.Top;
  x2 := Fgr2.Left;
  y2 := Fgr2.Top;
  x3 := Fgr3.Left;
  y3 := Fgr3.Top;
  x4 := Fgr4.Left;
  y4 := Fgr4.Top;
  x5 := Fgr5.Left;
  y5 := Fgr5.Top;
  t1 := Fgr1.Tag;
  t2 := Fgr2.Tag;
  t3 := Fgr3.Tag;
  t4 := Fgr4.Tag;
  t5 := Fgr5.Tag;
  if ((FSquare = Big) and (((t1 = 2) and (t2 = 1) and (Odd(t3)) and (t4 = 8) and (t5 = 2) and
    (Sh(x4, x3) and Sh(x2, x3) and Sh(x5 - F375, x3) and Sh(x1 - F625, x3) and
    Sh(y2 - F25, y3) and Sh(y4, y3) and Sh(y1, y3) and Sh(y5 - F375, y3)) or
    ((t1 = 4) and (t2 = 3) and (Odd(t3)) and (t4 = 2) and (t5 = 4) and Sh(x2 + F75, x3) and
    Sh(x4 + F375, x3) and Sh(x1 + F5, x3) and Sh(x5 + F75, x3) and Sh(y2, y3) and Sh(y4, y3) and
    Sh(y5 - F375, y3) and Sh(y1 - F625, y3)) or ((t1 = 6) and (t2 = 5) and (Odd(t3)) and
    (t4 = 4) and (t5 = 6) and Sh(x1 + F75, x3) and Sh(x4 + F75, x3) and Sh(x5 + F75, x3) and
    Sh(x2 + F5, x3) and Sh(y2, y5) and Sh(y1 - F25, y5) and Sh(y3 - F75, y5) and Sh(y4 - F375, y5)) or
    ((t1 = 8) and (t2 = 7) and (Odd(t3)) and (t4 = 6) and (t5 = 8) and Sh(x3, x4) and
    Sh(x3, x1) and Sh(x3, x2 - F25) and Sh(x3, x5 - F375) and Sh(y1, y4) and Sh(y1, y5) and
    Sh(y1, y3 - F75) and Sh(y1, y2 - F25)) or ((t1 = 3) and (t2 = 2) and (not Odd(t3)) and
    (t4 = 1) and (t5 = 3) and Sh(x2, x5 - Q75) and Sh(x2, x3 - Q75) and Sh(x2, x4 - Q75) and
    Sh(x2, x1 - Q125) and Sh(y3, y4 - Q25) and Sh(y3, y2 - Q25) and Sh(y3, y1 - Q1) and Sh(y3, y5 - Q1)) or
    ((t1 = 5) and (t2 = 4) and (not Odd(t3)) and (t4 = 3) and (t5 = 5) and Sh(y2, y5 - Q75) and
    Sh(y2, y3 - Q75) and Sh(y2, y4 - Q75) and Sh(y2, y1 - Q125) and Sh(x5, x1 - Q25) and Sh(x5, x2 - Q25) and
    Sh(x5, x4 - Q1) and Sh(x5, x3 - Q15)) or ((t1 = 7) and (t2 = 6) and (not Odd(t3)) and
    (t4 = 5) and (t5 = 7) and Sh(x1, x4) and Sh(x1, x5 - Q75) and Sh(x1, x3 - Q75) and
    Sh(x1, x2 - Q125) and Sh(y5, y1 - Q25) and Sh(y5, y2 - Q25) and Sh(y5, y4 - Q1) and Sh(y5, y3 - Q15)) or
    ((t1 = 1) and (t2 = 8) and (not Odd(t3)) and (t4 = 7) and (t5 = 1) and Sh(x3, x4 - Q25) and
    Sh(x3, x2 - Q25) and Sh(x3, x1 - Q1) and Sh(x3, x5 - Q1) and Sh(y1, y4) and Sh(y1, y3 - Q75) and
    Sh(y1, y5 - Q75) and Sh(y1, y2 - Q125))))) or ((FSquare = Small) and
    (((t1 = 1) and (t2 = 7) and (not Odd(t3)) and (t4 = 5) and (t5 = 1) and
    (Sh(x1, x4) and Sh(x1, x5) and Sh(x1, x2 - Q75) and Sh(x1, x3 - Q75) and
    Sh(y1, y4) and Sh(y1, y5 - Q75) and Sh(y1, y3 - Q5) and Sh(y1, y2 - Q75)) or
    ((t1 = 3) and (t2 = 1) and (not Odd(t3)) and (t4 = 7) and (t5 = 3) and Sh(x2, x5) and
    Sh(x2, x3 - Q25) and Sh(x2, x1 - Q5) and Sh(x2, x4 - Q5) and Sh(y1, y4) and Sh(y1, y5) and
    Sh(y1, y2 - Q75) and Sh(y1, y3 - Q75)) or ((t1 = 5) and (t2 = 3) and (not Odd(t3)) and
    (t4 = 1) and (t5 = 5) and Sh(x2, x3) and Sh(x2, x4) and Sh(x2, x1 - Q5) and Sh(x2, x5 - Q25) and
    Sh(y2, y5) and Sh(y2, y3 - Q25) and Sh(y2, y1 - Q5) and Sh(y2, y4 - Q5)) or
    ((t1 = 7) and (t2 = 5) and (not Odd(t3)) and (t4 = 3) and (t5 = 7) and Sh(x4, x1) and
    Sh(x4, x3 - Q5) and Sh(x4, x2 - Q75) and Sh(x4, x5 - Q75) and Sh(y4, y2) and Sh(y4, y3) and
    Sh(y4, y5 - Q25) and Sh(y4, y1 - Q5)) or ((t1 = 2) and (t2 = 8) and (Odd(t3)) and
    (t4 = 6) and (t5 = 2) and Sh(x5, x1 - F25) and Sh(x5, x2 - F375) and Sh(x5, x3 - F625) and
    Sh(x5, x4 - F625) and Sh(y1, y4) and Sh(y1, y3 - F75) and Sh(y1, y5 - F375) and Sh(y1, y2 - Num)) or
    ((t1 = 4) and (t2 = 2) and (Odd(t3)) and (t4 = 8) and (t5 = 4) and Sh(x2, x3 - F25) and
    Sh(x2, x5 - F25) and Sh(x2, x1 - F5) and Sh(x2, x4 - F25) and Sh(y5, y1 - F25) and Sh(y5, y2 - F375) and
    Sh(y5, y3 - F625) and Sh(y5, y4 - F625)) or ((t1 = 6) and (t2 = 4) and (Odd(t3)) and
    (t4 = 2) and (t5 = 6) and Sh(x4, x3 - F375) and Sh(x4, x1 - F625) and Sh(x4, x2 - F375) and
    Sh(x4, x5 - F625) and Sh(y2, y3 - F25) and Sh(y2, y5 - F25) and Sh(y2, y4 - F25) and Sh(y2, y1 - F5)) or
    ((t1 = 8) and (t2 = 6) and (Odd(t3)) and (t4 = 4) and (t5 = 6) and Sh(x1, x4) and
    Sh(x1, x5 - F375) and Sh(x1, x3 - F75) and Sh(x1, x2 - Num) and Sh(y4, y3 - F375) and Sh(y4, y2 - F375) and
    Sh(y4, y5 - F625) and Sh(y4, y1 - F625))))) then
    Over;
end;

procedure TFigures.Time;
begin
  if assigned(FOnTime) then
    FOnTime(Self);
end;

procedure TFigures.Resize;
begin
  inherited Resize;
  if Height > Width then
    num := Trunc((Width * FWidthHeight) / 100)
  else
    num := Trunc((Height * FWidthHeight) / 100);
  if Fgr1 <> nil then
    Fgr1.Numb := Num;
  if Fgr2 <> nil then
    Fgr2.Numb := Num;
  if Fgr3 <> nil then
    Fgr3.Numb := Num;
  if Fgr4 <> nil then
    Fgr4.Numb := Num;
  if Fgr5 <> nil then
    Fgr5.Numb := Num;
end;

procedure TFigures.TimerOnTimer(Sender: TObject);
begin
  if FStart then
    Time;
end;

{$ifdef windows}
procedure TFigures.WMNCHitTest(var Mes: TWMNCHitTest);
begin
  inherited;
  if (not FStart) or (FPause) then
    if Mes.Result = htClient then
      Mes.Result := htCaption;
  SetFocus;
end;
{$endif}

constructor TFigure.Create(AOwner: TComponent);
begin
  Randomize;
  FMP := False;
  Sq2 := Sqrt(2.0);
  inherited Create(AOwner);
  Left := 5 + Random(10);
  Top := 5 + Random(10);
  FOver := False;
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psSolid;
  TransParent := True;
  Tag := Random(8) + 1;
end;

destructor TFigure.Destroy;
begin
  inherited Destroy;
end;

procedure TFigure.Draw;
begin
  if not FOver then
    case Fig of
      1:
        case Tag of
          1:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(1, 1)]);
          2:
            Canvas.PolyLine
            ([Classes.Point(F375 + 1, 1), Classes.Point(F375 + 1, F75 + 1), Classes.Point(1, F375 + 1), Classes.Point(F375 + 1, 1)]);
          3:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, 1), Classes.Point(1, 1)]);
          4:
            Canvas.PolyLine
            ([Classes.Point(F375 + 1, 1), Classes.Point(F75 + 1, F375 + 1), Classes.Point(1, F375 + 1), Classes.Point(F375 + 1, 1)]);
          5:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(S375 + 1, 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(1, 1)]);
          6:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(F375 + 1, F375 + 1), Classes.Point(1, F75 + 1), Classes.Point(1, 1)]);
          7:
            Canvas.PolyLine
            ([Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(S375 + 1, 1), Classes.Point(1, S375 + 1)]);
          8:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(F75 + 1, 1), Classes.Point(F375 + 1, F375 + 1), Classes.Point(1, 1)]);
        end;
      2:
        case Tag of
          1:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, F75 + 1), Classes.Point(1, 1)]);
          2:
            Canvas.PolyLine
            ([Classes.Point(S375 + 1, 1), Classes.Point(S375 + 1, S75 + 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, 1)]);
          3:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, 1), Classes.Point(1, 1)]);
          4:
            Canvas.PolyLine
            ([Classes.Point(S375 + 1, 1), Classes.Point(S75 + 1, S375 + 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, 1)]);
          5:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(F75 + 1, 1), Classes.Point(F75 + 1, F75 + 1), Classes.Point(1, 1)]);
          6:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(1, S75 + 1), Classes.Point(1, 1)]);
          7:
            Canvas.PolyLine
            ([Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, F75 + 1), Classes.Point(F75 + 1, 1), Classes.Point(1, F75 + 1)]);
          8:
            Canvas.PolyLine
            ([Classes.Point(1, 1), Classes.Point(S75 + 1, 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(1, 1)]);
        end;
      3:
        if Odd(Tag) then
          Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(F25 + 1, 1), Classes.Point(F25 + 1, F25 + 1),
            Classes.Point(1, F25 + 1), Classes.Point(1, 1)])
        else
          Canvas.PolyLine([Classes.Point(S0125 + 1, 1), Classes.Point(S25 + 1, S0125 + 1), Classes.Point(S0125 + 1, S25 + 1),
            Classes.Point(1, S0125 + 1), Classes.Point(S0125 + 1, 1)]);
      4:
        case Tag of
          1:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(Q25 + 1, Q25 + 1), Classes.Point(Q5 + 1, 1), Classes.Point(Q125 + 1, Q75 + 1), Classes.Point(1, Q75 + 1),
              Classes.Point(1, 1)]);
          2:
            Canvas.PolyLine([Classes.Point(F375 + 1, 1), Classes.Point(F375 + 1, F25 + 1), Classes.Point(F625 + 1, F25 + 1),
              Classes.Point(F625 + 1, FNumb + 1), Classes.Point(1, F375 + 1), Classes.Point(F375 + 1, 1)]);
          3:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(Q75 + 1, 1), Classes.Point(Q5 + 1, Q25 + 1), Classes.Point(Q75 + 1, Q5 + 1), Classes.Point(1, Q125 + 1),
              Classes.Point(1, 1)]);
          4:
            Canvas.PolyLine([Classes.Point(1, F625 + 1), Classes.Point(F625 + 1, 1), Classes.Point(FNumb + 1, F375 + 1),
              Classes.Point(F75 + 1, F375 + 1), Classes.Point(F75 + 1, F625 + 1), Classes.Point(1, F625 + 1)]);
          5:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(Q125 + 1, 1), Classes.Point(Q125 + 1, Q75 + 1), Classes.Point(Q1 + 1, Q5 + 1), Classes.Point(Q75 + 1, Q75 + 1),
              Classes.Point(1, 1)]);
          6:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(F625 + 1, F625 + 1), Classes.Point(F25 + 1, FNumb + 1), Classes.Point(F25 + 1, F75 + 1),
              Classes.Point(1, F75 + 1), Classes.Point(1, 1)]);
          7:
            Canvas.PolyLine([Classes.Point(Q75 + 1, 1), Classes.Point(Q75 + 1, Q125 + 1), Classes.Point(1, Q125 + 1), Classes.Point(Q25 + 1, Q1 + 1), Classes.Point(1, Q75 + 1),
              Classes.Point(Q75 + 1, 1)]);
          8:
            Canvas.PolyLine([Classes.Point(F25 + 1, 1), Classes.Point(FNumb + 1, 1), Classes.Point(F375 + 1, F625 + 1), Classes.Point(1, F25 + 1),
              Classes.Point(F25 + 1, F25 + 1), Classes.Point(F25 + 1, 1)]);
        end;
      5:
        case Tag of
          1:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(Q75 + 1, 1), Classes.Point(Q1 + 1, Q25 + 1), Classes.Point(Q75 + 1, Q5 + 1),
              Classes.Point(1, Q5 + 1), Classes.Point(1, 1)]);
          2:
            Canvas.PolyLine([Classes.Point(F25 + 1, 1), Classes.Point(F625 + 1, F375 + 1), Classes.Point(F625 + 1, F625 + 1), Classes.Point(F375 + 1,
              F625 + 1), Classes.Point(1, F25 + 1), Classes.Point(F25 + 1, 1)]);
          3:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(1, Q75 + 1), Classes.Point(Q25 + 1, Q1 + 1), Classes.Point(Q5 + 1, Q75 + 1), Classes.Point(Q5 + 1, 1),
              Classes.Point(1, 1)]);
          4:
            Canvas.PolyLine([Classes.Point(F375 + 1, 1), Classes.Point(F625 + 1, F25 + 1), Classes.Point(F25 + 1, F625 + 1), Classes.Point(
              1, F625 + 1), Classes.Point(1, F375 + 1), Classes.Point(F375 + 1, 1)]);
          5:
            Canvas.PolyLine([Classes.Point(1, Q25 + 1), Classes.Point(Q25 + 1, 1), Classes.Point(Q1 + 1, 1), Classes.Point(Q1 + 1, Q5 + 1), Classes.Point(Q25 + 1, Q5 + 1),
              Classes.Point(1, Q25 + 1)]);
          6:
            Canvas.PolyLine([Classes.Point(1, 1), Classes.Point(F25 + 1, 1), Classes.Point(F625 + 1, F375 + 1), Classes.Point(F375 + 1, F625 + 1),
              Classes.Point(1, F25 + 1), Classes.Point(1, 1)]);
          7:
            Canvas.PolyLine([Classes.Point(Q25 + 1, 1), Classes.Point(1, Q25 + 1), Classes.Point(1, Q1 + 1), Classes.Point(Q5 + 1, Q1 + 1), Classes.Point(
              Q5 + 1, Q25 + 1), Classes.Point(Q25 + 1, 1)]);
          8:
            Canvas.PolyLine([Classes.Point(F625 + 1, 1), Classes.Point(F625 + 1, F25 + 1), Classes.Point(F25 + 1, F625 + 1), Classes.Point(
              1, F375 + 1), Classes.Point(F375 + 1, 1), Classes.Point(F625 + 1, 1)]);
        end;
      6:
        case Tag of
          1:
            Canvas.Polyline([Classes.Point(1, 1), Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, Q5 + 1), Classes.Point(1, 1)]);
          2:
            Canvas.Polyline([Classes.Point(F25 + 1, 0), Classes.Point(F25 + 1, F5), Classes.Point(1, F25), Classes.Point(F25 + 1, 0)]);
          3:
            Canvas.Polyline([Classes.Point(1, 1), Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, 1), Classes.Point(1, 1)]);
          4:
            Canvas.Polyline([Classes.Point(F25, 1), Classes.Point(F5, F25 + 1), Classes.Point(0, F25 + 1), Classes.Point(F25, 1)]);
          5:
            Canvas.Polyline([Classes.Point(1, 1), Classes.Point(Q5 + 1, 1), Classes.Point(Q5 + 1, Q5 + 1), Classes.Point(1, 1)]);
          6:
            Canvas.Polyline([Classes.Point(1, 0), Classes.Point(F25 + 1, F25), Classes.Point(1, F5), Classes.Point(1, 0)]);
          7:
            Canvas.Polyline([Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, Q5 + 1), Classes.Point(Q5 + 1, 1), Classes.Point(1, Q5 + 1)]);
          8:
            Canvas.Polyline([Classes.Point(0, 1), Classes.Point(F5, 1), Classes.Point(F25, F25 + 1), Classes.Point(0, 1)]);
        end;
    end
  else
  begin
    with Canvas do
    begin
      if Pen.Color = FLineColor then
        exit;
      Brush.Color := Pen.Color;
      Brush.Style := bsSolid;
      Pen.Color := FLineColor;
    end;
    case Fig of
      1:
        case Tag of
          1:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, S375 + 1)]);
          2:
            Canvas.Polygon([Classes.Point(F375 + 1, 1), Classes.Point(F375 + 1, F75 + 1), Classes.Point(1, F375 + 1)]);
          3:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, 1)]);
          4:
            Canvas.Polygon([Classes.Point(F375 + 1, 1), Classes.Point(F75 + 1, F375 + 1), Classes.Point(1, F375 + 1)]);
          5:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(S375 + 1, 1), Classes.Point(S375 + 1, S375 + 1)]);
          6:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F375 + 1, F375 + 1), Classes.Point(1, F75 + 1)]);
          7:
            Canvas.Polygon([Classes.Point(1, S375 + 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(S375 + 1, 1)]);
          8:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F75 + 1, 1), Classes.Point(F375 + 1, F375 + 1)]);
        end;
      2:
        case Tag of
          1:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, F75 + 1)]);
          2:
            Canvas.Polygon([Classes.Point(S375 + 1, 1), Classes.Point(S375 + 1, S75 + 1), Classes.Point(1, S375 + 1)]);
          3:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, 1)]);
          4:
            Canvas.Polygon([Classes.Point(S375 + 1, 1), Classes.Point(S75 + 1, S375 + 1), Classes.Point(1, S375 + 1)]);
          5:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F75 + 1, 1), Classes.Point(F75 + 1, F75 + 1)]);
          6:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(S375 + 1, S375 + 1), Classes.Point(1, S75 + 1)]);
          7:
            Canvas.Polygon([Classes.Point(1, F75 + 1), Classes.Point(F75 + 1, F75 + 1), Classes.Point(F75 + 1, 1)]);
          8:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(S75 + 1, 1), Classes.Point(S375 + 1, S375 + 1)]);
        end;
      3:
        if Odd(Tag) then
          Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F25 + 1, 1), Classes.Point(F25 + 1, F25 + 1), Classes.Point(1, F25 + 1)])
        else
          Canvas.Polygon
          ([Classes.Point(S0125 + 1, 1), Classes.Point(S25 + 1, S0125 + 1), Classes.Point(S0125 + 1, S25 + 1), Classes.Point(1, S0125 + 1)]);
      4:
        case Tag of
          1:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(Q25 + 1, Q25 + 1), Classes.Point(Q5 + 1, 1), Classes.Point(Q125 + 1, Q75 + 1), Classes.Point(1, Q75 + 1)]);
          2:
            Canvas.Polygon([Classes.Point(F375 + 1, 1), Classes.Point(F375 + 1, F25 + 1), Classes.Point(F625 + 1, F25 + 1),
              Classes.Point(F625 + 1, FNumb + 1), Classes.Point(1, F375 + 1)]);
          3:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(Q75 + 1, 1), Classes.Point(Q5 + 1, Q25 + 1), Classes.Point(Q75 + 1, Q5 + 1), Classes.Point(1, Q125 + 1)]);
          4:
            Canvas.Polygon([Classes.Point(1, F625 + 1), Classes.Point(F625 + 1, 1), Classes.Point(FNumb + 1, F375 + 1),
              Classes.Point(F75 + 1, F375 + 1), Classes.Point(F75 + 1, F625 + 1)]);
          5:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(Q125 + 1, 1), Classes.Point(Q125 + 1, Q75 + 1), Classes.Point(Q1 + 1, Q5 + 1), Classes.Point(Q75 + 1, Q75 + 1)]);
          6:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F625 + 1, F625 + 1), Classes.Point(F25 + 1, FNumb + 1), Classes.Point(F25 + 1, F75 + 1),
              Classes.Point(1, F75 + 1)]);
          7:
            Canvas.Polygon([Classes.Point(Q75 + 1, 1), Classes.Point(Q75 + 1, Q125 + 1), Classes.Point(1, Q125 + 1), Classes.Point(Q25 + 1,
              Q1 + 1), Classes.Point(1, Q75 + 1)]);
          8:
            Canvas.Polygon([Classes.Point(F25 + 1, 1), Classes.Point(FNumb + 1, 1), Classes.Point(F375 + 1, F625 + 1), Classes.Point(1, F25 + 1),
              Classes.Point(F25 + 1, F25 + 1)]);
        end;
      5:
        case Tag of
          1:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(Q75 + 1, 1), Classes.Point(Q1 + 1, Q25 + 1), Classes.Point(Q75 + 1, Q5 + 1), Classes.Point(1, Q5 + 1)]);
          2:
            Canvas.Polygon([Classes.Point(F25 + 1, 1), Classes.Point(F625 + 1, F375 + 1), Classes.Point(F625 + 1, F625 + 1), Classes.Point(F375 + 1,
              F625 + 1), Classes.Point(1, F25 + 1)]);
          3:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, Q75 + 1), Classes.Point(Q25 + 1, Q1 + 1), Classes.Point(Q5 + 1, Q75 + 1), Classes.Point(Q5 + 1, 1)]);
          4:
            Canvas.Polygon([Classes.Point(F375 + 1, 1), Classes.Point(F625 + 1, F25 + 1), Classes.Point(F25 + 1, F625 + 1), Classes.Point(
              1, F625 + 1), Classes.Point(1, F375 + 1)]);
          5:
            Canvas.Polygon([Classes.Point(1, Q25 + 1), Classes.Point(Q25 + 1, 1), Classes.Point(Q1 + 1, 1), Classes.Point(Q1 + 1, Q5 + 1), Classes.Point(Q25 + 1, Q5 + 1)]);
          6:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(F25 + 1, 1), Classes.Point(F625 + 1, F375 + 1), Classes.Point(F375 + 1, F625 + 1), Classes.Point(1, F25 + 1)]);
          7:
            Canvas.Polygon([Classes.Point(Q25 + 1, 1), Classes.Point(1, Q25 + 1), Classes.Point(1, Q1 + 1), Classes.Point(Q5 + 1, Q1 + 1), Classes.Point(Q5 + 1, Q25 + 1)]);
          8:
            Canvas.Polygon([Classes.Point(F625 + 1, 1), Classes.Point(F625 + 1, F25 + 1), Classes.Point(F25 + 1, F625 + 1), Classes.Point(
              1, F375 + 1), Classes.Point(F375 + 1, 1)]);
        end;
      6:
        case Tag of
          1:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, Q5 + 1)]);
          2:
            Canvas.Polygon([Classes.Point(F25 + 1, 0), Classes.Point(F25 + 1, F5), Classes.Point(1, F25)]);
          3:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, 1)]);
          4:
            Canvas.Polygon([Classes.Point(F25, 1), Classes.Point(F5, F25 + 1), Classes.Point(0, F25 + 1)]);
          5:
            Canvas.Polygon([Classes.Point(1, 1), Classes.Point(Q5 + 1, 1), Classes.Point(Q5 + 1, Q5 + 1)]);
          6:
            Canvas.Polygon([Classes.Point(1, 0), Classes.Point(F25 + 1, F25), Classes.Point(1, F5)]);
          7:
            Canvas.Polygon([Classes.Point(1, Q5 + 1), Classes.Point(Q5 + 1, Q5 + 1), Classes.Point(Q5 + 1, 1)]);
          8:
            Canvas.Polygon([Classes.Point(0, 1), Classes.Point(F5, 1), Classes.Point(F25, F25 + 1)]);
        end;
    end;
  end;
end;

procedure TFigure.Coeff;
var
  a: real;
begin
  S0125 := Trunc(0.125 * FNumb * Sq2);
  S25 := 2 * S0125;
  a := 0.375 * FNumb;
  F375 := Trunc(a);
  S375 := Trunc(a * Sq2);
  F75 := 2 * F375;
  S75 := 2 * S375;
  a := 0.625 * FNumb;
  F625 := Trunc(a);
  S625 := Trunc(a * Sq2);
  F125 := 2 * F625;
  S125 := 2 * S625;
  a := FNumb * 0.25;
  F25 := Trunc(a);
  F5 := 2 * F25;
  Q25 := Trunc(a / Sq2);
  Q5 := 2 * Q25;
  Q1 := 2 * Q5;
  Q75 := 3 * Q25;
  Q125 := 5 * Q25;
end;

procedure TFigure.WidthsHeights;
var
  S1, S2: integer;
begin
  case Fig of
    1:
      if Odd(Tag) then
      begin
        wi := Q75 + 3;
        he := wi;
      end
      else
      if (Tag = 4) or (Tag = 8) then
      begin
        wi := F75 + 2;
        he := wi div 2 + 2;
      end
      else
      if (Tag = 2) or (Tag = 6) then
      begin
        he := F75 + 2;
        wi := he div 2 + 2;
      end;
    2:
      if Odd(Tag) then
      begin
        wi := F75 + 2;
        he := wi;
      end
      else
      if (Tag = 4) or (Tag = 8) then
      begin
        wi := S75 + 2;
        he := wi div 2 + 2;
      end
      else
      if (Tag = 2) or (Tag = 6) then
      begin
        he := S75 + 2;
        wi := he div 2 + 2;
      end;
    3:
      if Odd(Tag) then
      begin
        wi := F25 + 2;
        he := wi;
      end
      else
      begin
        wi := S25 + 2;
        he := wi;
      end;
    4:
      if (Tag = 1) or (Tag = 5) then
      begin
        wi := Q125 + 2;
        he := Q75 + 2;
      end
      else
      if (Tag = 3) or (Tag = 7) then
      begin
        he := Q125 + 2;
        wi := Q75 + 2;
      end
      else
      if (Tag = 2) or (Tag = 6) then
      begin
        he := FNumb + 2;
        wi := F625 + 2;
      end
      else
      if (Tag = 4) or (Tag = 8) then
      begin
        wi := FNumb + 2;
        he := F625 + 2;
      end;
    5:
    begin
      S1 := Trunc(FNumb / Sq2);
      S2 := Trunc(FNumb / (2 * Sq2));
      if (Tag = 1) or (Tag = 5) then
      begin
        wi := S1 + 2;
        he := S2 + 2;
      end
      else
      if (Tag = 3) or (Tag = 7) then
      begin
        he := S1 + 2;
        wi := S2 + 2;
      end
      else
      if (Tag = 2) or (Tag = 6) then
      begin
        he := F625 + 2;
        wi := he;
      end
      else
      if (Tag = 4) or (Tag = 8) then
      begin
        he := F625 + 2;
        wi := he;
      end;
    end;
    6:
      if Odd(Tag) then
      begin
        wi := Q5 + 2;
        he := wi;
      end
      else
      if (Tag = 4) or (Tag = 8) then
      begin
        wi := F5 + 2;
        he := wi div 2 + 2;
      end
      else
      if (Tag = 2) or (Tag = 6) then
      begin
        he := F5 + 2;
        wi := he div 2 + 2;
      end;
  end;
end;

procedure TFigure.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
    FLineColor := Value;
end;

procedure TFigure.Paint;
begin
  inherited Paint;
  Draw;
end;

procedure TFigure.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Pix then
  begin
    if Button = mbLeft then
      FMP := False;
    if Left < 1 then
      Left := 1;
    if Top < 1 then
      Top := 1;
    if Left + Width > Parent.Width - 1 then
      Left := Parent.Width - Width - 1;
    if Top + Height > Parent.Height - 1 then
      Top := Parent.Height - Height - 1;
    Pix := False;
    with Canvas.Pen do
    begin
      Width := 2;
      Style := psSolid;
    end;
    SendToBack;
    Move;
    Invalidate;
    Exit;
  end;
  SendToBack;
end;

procedure TFigure.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i, j: longint;
  k: word;
  LT, TP: integer;
  Col: TColor;
begin
  inherited MouseDown(Button, Shift, X, Y);
  Pix := False;
  Col := Canvas.Pen.Color;
  j := y;
  k := 0;
  for i := X to Width do
    if Canvas.Pixels[i, j] = Col then
    begin
      Inc(k);
      break;
    end;
  for i := X downto 0 do
    if Canvas.Pixels[i, j] = Col then
    begin
      Inc(k);
      break;
    end;
  if k <> 2 then
  begin
    k := 0;
    i := x;
    for j := y to Height do
      if Canvas.Pixels[i, j] = Col then
      begin
        Inc(k);
        break;
      end;
    for j := Y downto 0 do
      if Canvas.Pixels[i, j] = Col then
      begin
        Inc(k);
        break;
      end;
  end;
  if k = 2 then
    Pix := True;
  if not Pix then
    exit;
  if Button = mbLeft then
  begin
    FMP := True;
    MX := X;
    MY := Y;
    with Canvas.Pen do
    begin
      Width := 1;
      Style := psDot;
    end;
    Invalidate;
  end
  else
  if Button = mbRight then
  begin
    if (ssCtrl in Shift) then
      Tag := Tag - 1
    else
      Tag := Tag + 1;
    if Tag = 0 then
      Tag := 8
    else if Tag = 9 then
      Tag := 1;
    LT := Width;
    TP := Height;
    wi := LT;
    he := TP;
    WidthsHeights;
    LT := Left - (wi - LT) div 2;
    TP := Top - (he - TP) div 2;
    SetBounds(LT, TP, wi, he);
  end;
end;

procedure TFigure.MouseMove(Shift: TShiftState; X, Y: integer);
var
  LT, TP: integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not FMP then
    exit;
  LT := Left + (X - MX);
  TP := Top + (Y - MY);
  SetBounds(LT, TP, Width, Height);
end;

procedure TFigure.SetOver(Value: boolean);
begin
  if FOver <> Value then
    FOver := Value;
end;

procedure TFigure.SetNumb(Value: integer);
var
  LT, TP: integer;
begin
  if FNumb = Value then
    Exit;
  FNumb := Value;
  Coeff;
  LT := Width;
  TP := Height;
  wi := LT;
  he := TP;
  WidthsHeights;
  if LT <> 0 then
    LT := Trunc(Abs((Left * wi) / LT));
  if TP <> 0 then
    TP := Trunc(Abs((Top * he) / TP));
  if LT < 1 then
    LT := 1;
  if TP < 1 then
    TP := 1;
  if LT + wi > Parent.Width - 1 then
    LT := Parent.Width - wi - 1;
  if TP + he > Parent.Height - 1 then
    TP := Parent.Height - he - 1;
  SetBounds(LT, TP, wi, he);
end;

procedure TFigure.Move;
begin
  if Assigned(FOnMove) then
    FOnMove(Self);
end;


end.
