// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mThumbnails;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

function GeneratePNGThumbnail (const aSourceFile, aThumbnailsFolder : String; const aMaxWidth, aMaxHeight: word; out aThumbnailFileName: String; out aError: String): boolean;

implementation

uses
  Classes, Graphics, sysutils,
  mMutool;

function _GeneratePNGThumbnail(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;
var
  sourcePicture : TPicture;
  thumbnail : TPortableNetworkGraphic;
  rateWidth, rateHeight : Extended;
  r : TRect;
begin
  Result := false;
  try
    sourcePicture := TPicture.Create;
    thumbnail := TPortableNetworkGraphic.Create;
    try
      sourcePicture.LoadFromFile(aSourceFile);
      rateWidth := aMaxWidth / sourcePicture.Width;
      rateHeight := aMaxHeight / sourcePicture.Height;
      if rateWidth > rateHeight then
        rateWidth := rateHeight;
      thumbnail.SetSize(round(sourcePicture.Width * rateWidth), round(sourcePicture.Height * rateHeight));
      r := Rect(0, 0, thumbnail.Width, thumbnail.Height);
      thumbnail.Canvas.AntialiasingMode := amON;
      thumbnail.Canvas.StretchDraw(Rect(0, 0, 100, 100), sourcePicture.Graphic);
      thumbnail.SaveToFile(aThumbnailFile);
    finally
      sourcePicture.Free;
      thumbnail.Free;
    end;
  except
    on e: Exception do
    begin
      aError := e.Message;
      exit;
    end;
  end;
  Result := true;
end;

function GeneratePNGThumbnail(const aSourceFile, aThumbnailsFolder: String; const aMaxWidth, aMaxHeight: word; out aThumbnailFileName: String; out aError: String): boolean;
var
  GraphicClass: TGraphicClass;
  ext : String;
begin
  Result := false;
  ext := LowerCase(ExtractFileExt(aSourceFile));
  aThumbnailFileName := ChangeFileExt(IncludeTrailingPathDelimiter(aThumbnailsFolder) + ExtractFileName(aSourceFile), '.png');

  if ext = '.pdf' then
  begin
    try
      TMutoolToolbox.ExtractThumbnailOfFrontPageFromPdf(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight);
      Result := true;
    except
      on e: Exception do
      begin
        aError := e.Message;
        exit;
      end;
    end;
  end
  else
  begin
    GraphicClass := GetGraphicClassForFileExtension(ext);
    if GraphicClass <> nil then
      Result := _GeneratePNGThumbnail(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight, aError)
    else
      exit;
  end;
end;

end.
