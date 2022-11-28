// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit PDFCreatorProject;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  contnrs, Classes,
  mNullables;

type
  TPDFCreatorProjectFile = class
  strict private
    const PAGES_SUBFOLDER = 'pages';
    const THUMBNAILS_SUBFOLDER = 'thumbnails';
  strict private
    FFileName : TNullableString;
    FFileFolder : TNullableString;
    FPages : TStringList;
    FThumbnails : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetActualFileNameWithPath : String;
    function GetPagesFolder : String;
    function GetPagesThumbsFolder : String;
    procedure Clear;

    property FileName : TNullableString read FFileName;
    property FileFolder : TNullableString read FFileFolder;
    property Pages : TStringList read FPages;
    property Thumbnails : TStringList read FThumbnails;
  end;


  TPDFCreatorProject = class
  strict private
    const TEMPORARY_SUBFOLDER = 'temp';
  strict private
    FFiles : TObjectList;
    FBaseFolder : TNullableString;
    FNewFile : TPDFCreatorProjectFile;
  public
    constructor Create;
    destructor Destroy; override;
    function AddFile : TPDFCreatorProjectFile;
    procedure RemoveFile (const aFile : TPDFCreatorProjectFile);
    function GetTempFolder : String;
    procedure Clear;

    property BaseFolder : TNullableString read FBaseFolder;
    property NewFile : TPDFCreatorProjectFile read FNewFile;
  end;

implementation
uses
  SysUtils;

{ TPDFCreatorProject }

constructor TPDFCreatorProject.Create;
begin
  FFiles := TObjectList.Create(true);
  FBaseFolder := TNullableString.Create;
  FNewFile := TPDFCreatorProjectFile.Create;
end;

destructor TPDFCreatorProject.Destroy;
begin
  FBaseFolder.Free;
  FFiles.Free;
  FNewFile.Free;
  inherited Destroy;
end;

function TPDFCreatorProject.AddFile: TPDFCreatorProjectFile;
begin
  Result := TPDFCreatorProjectFile.Create;
  FFiles.Add(Result);
  Result.FileFolder.Value:= IncludeTrailingPathDelimiter(FBaseFolder.AsString) + IntToStr(FFiles.Count);
end;

procedure TPDFCreatorProject.RemoveFile(const aFile: TPDFCreatorProjectFile);
begin
  FFiles.Remove(aFile);
end;

function TPDFCreatorProject.GetTempFolder: String;
begin
  Result := IncludeTrailingPathDelimiter(FBaseFolder.AsString) + TEMPORARY_SUBFOLDER;
end;

procedure TPDFCreatorProject.Clear;
begin
  FNewFile.Clear;
  FFiles.Clear;
  FBaseFolder.IsNull:= true;
end;

{ TPDFCreatorProjectFile }

constructor TPDFCreatorProjectFile.Create;
begin
  FFileName := TNullableString.Create;
  FFileFolder := TNullableString.Create;
  FPages := TStringList.Create;
  FThumbnails := TStringList.Create;
end;

destructor TPDFCreatorProjectFile.Destroy;
begin
  FFileName.Free;
  FFileFolder.Free;
  FPages.Free;
  FThumbnails.Free;
  inherited Destroy;
end;

function TPDFCreatorProjectFile.GetActualFileNameWithPath: String;
begin
  Result := IncludeTrailingPathDelimiter(FileFolder.AsString) + ExtractFileName(FileName.AsString);
end;

function TPDFCreatorProjectFile.GetPagesFolder: String;
begin
  Result := IncludeTrailingPathDelimiter(FileFolder.AsString) + PAGES_SUBFOLDER;
end;

function TPDFCreatorProjectFile.GetPagesThumbsFolder: String;
begin
  Result := IncludeTrailingPathDelimiter(GetPagesFolder) + THUMBNAILS_SUBFOLDER;
end;

procedure TPDFCreatorProjectFile.Clear;
begin
  FFileName.IsNull := true;
  FFileFolder.IsNull := true;
  FPages.Clear;
  FThumbnails.Clear;
end;

end.
