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
  mMutool, mUtility, mGraphicsUtility, mXPdf;

function GeneratePNGThumbnail(const aSourceFile, aThumbnailsFolder: String; const aMaxWidth, aMaxHeight: word; out aThumbnailFileName: String; out aError: String): boolean;
var
  GraphicClass: TGraphicClass;
  ext : String;
begin
  Result := false;
  ext := LowerCase(ExtractFileExt(aSourceFile));
  aThumbnailFileName := ChangeFileExt(IncludeTrailingPathDelimiter(aThumbnailsFolder) + ExtractFileName(aSourceFile) + GenerateRandomIdString(5), '.png');

  if ext = '.pdf' then
  begin
    try
      //Result := TMutoolToolbox.ExtractThumbnailOfFrontPageFromPdf(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight);
      Result := TXPdfToolbox.ExtractThumbnailOfFrontPageFromPdf(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight);
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
      Result := GeneratePNGThumbnailOfImage(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight, aError)
    else
      exit;
  end;
end;

end.
