// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSpreadsheetAsPivotHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,

  mPivoter, mIntList, mMaps,
  mPivoterToVirtualGrid, mVirtualGridSpreadsheet;

type

  { TmSpreadsheetAsPivotHelper }

  TmSpreadsheetAsPivotHelper = class (TmVirtualGridAsPivotHelper)
  strict private
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ExportPivotAsXls(aStream: TStream; const aPivoter : TmPivoter);
  end;

implementation


uses
  sysutils, LCLType,
  {$IFDEF FPC}
  fpstypes, fpspreadsheet,
  fpsallformats // necessary to register all the input/output formats that fpspreadsheet can handle
  {$ENDIF}
  ;

{ TmSpreadsheetAsPivotHelper }

constructor TmSpreadsheetAsPivotHelper.Create;
begin
  inherited Create;
end;

destructor TmSpreadsheetAsPivotHelper.Destroy;
begin
  inherited Destroy;
end;

procedure TmSpreadsheetAsPivotHelper.ExportPivotAsXls(aStream: TStream; const aPivoter : TmPivoter);
var
  MyWorkbook : TsWorkbook;
  MyWorksheet : TsWorksheet;
  sp : TmSpreadsheetAsVirtualGrid;
begin
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('Sheet1');
    sp := TmSpreadsheetAsVirtualGrid.Create;
    try
      {$IFDEF WINDOWS}
      sp.DefaultFont.FontName:= 'Calibri';
      {$ELSE}
      sp.DefaultFont.FontName:= 'Arial';
      {$ENDIF}
      sp.DefaultFont.FontSize:= 10;
      sp.DefaultRowHeight:= 14;

      sp.Init(MyWorksheet);
      Self.InternalInit(aPivoter, sp);

      Self.ApplyPivotToGrid;
    finally
      sp.Free;
    end;
    MyWorkbook.WriteToStream(aStream, sfExcel8);
  finally
    MyWorkbook.Free;
  end;
end;

end.
