unit mFileOpenSaveLCL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, StdCtrls,

  mVirtualFileSystem;

type

  TmFileOperation = (fmOpen, fmSave);

  { TmFileOpenSave }

  TmFileOpenSave = class(TForm)
    ButtonPanel: TButtonPanel;
    EditFileName: TEdit;
    LVFiles: TListView;
    PanelCurrentFolder: TPanel;
    Splitter1: TSplitter;
    TVFolders: TTreeView;
    procedure EditFileNameEditingDone(Sender: TObject);
    procedure LVFilesDblClick(Sender: TObject);
    procedure LVFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure OKButtonClick(Sender: TObject);
    procedure TVFoldersSelectionChanged(Sender: TObject);
  private
    FCurrentFolder : TmFolder;
    FCurrentFile : TmFile;
    FSaveFileData : TmFileData;
    FFileOperation : TmFileOperation;
    FManager : TmAbstractFileSystemManager;
    procedure AddNode (aParent : TTreeNode; aFolder : TmFolder);
    procedure RefreshFiles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (aFileSystemManager : TmAbstractFileSystemManager; const aFileOperation : TmFileOperation);
    function GetFileData: TmFileData;
  end;


implementation

{$R *.lfm}

{ TmFileOpenSave }

procedure TmFileOpenSave.TVFoldersSelectionChanged(Sender: TObject);
begin
  if TVFolders.SelectionCount > 0 then
  begin
    FCurrentFolder := TmFolder(TVFolders.Selected.Data);
    PanelCurrentFolder.Caption:= FCurrentFolder.Path;
  end
  else
  begin
    FCurrentFolder := nil;
    PanelCurrentFolder.Caption:= '';
  end;
  RefreshFiles;
end;

procedure TmFileOpenSave.LVFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  EditFileName.Text := '';
  if Assigned(Item) and Assigned (FCurrentFolder) then
  begin
    EditFileName.Text := TmFile(Item.Data).FileData.FileName;
    FCurrentFile := TmFile(Item.Data);
  end;
end;

procedure TmFileOpenSave.OKButtonClick(Sender: TObject);
begin
  if FFileOperation = fmOpen then
  begin
    if not Assigned(FCurrentFile) then
      ModalResult:= mrCancel;
  end
  else if FFileOperation = fmSave then
  begin
    if not Assigned(FCurrentFolder) or (trim(EditFileName.Text) = '') then
      ModalResult := mrCancel;
  end;
end;

procedure TmFileOpenSave.LVFilesDblClick(Sender: TObject);
begin
  if Assigned(FCurrentFile) then
  begin
    FCurrentFile := TmFile(LVFiles.Selected.Data);
    Self.ModalResult:= mrOk;
  end;
end;

procedure TmFileOpenSave.EditFileNameEditingDone(Sender: TObject);
begin
  EditFileName.Text:= FManager.ValidateFileName(EditFileName.Text);
end;

procedure TmFileOpenSave.AddNode(aParent: TTreeNode; aFolder: TmFolder);
var
  newNode : TTreeNode;
  i : integer;
begin
  newNode := TVFolders.Items.AddChild(aParent, aFolder.Name);
  newNode.Data:= aFolder;
  for i := 0 to aFolder.Folders.Count - 1 do
  begin
    AddNode(newNode, aFolder.Folders.Items[i]);
  end;
end;

procedure TmFileOpenSave.RefreshFiles;
var
  i : integer;
begin
  LVFiles.BeginUpdate;
  try
    LVFiles.Clear;
    if Assigned(FCurrentFolder) then
    begin
      for i := 0 to FCurrentFolder.Files.Count - 1 do
      begin
        LVFiles.AddItem(FCurrentFolder.Files[i].FileData.Name, FCurrentFolder.Files[i]);
      end;
    end;
  finally
    LVFiles.EndUpdate;
  end;
end;

constructor TmFileOpenSave.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSaveFileData := TmFileData.Create;
end;

destructor TmFileOpenSave.Destroy;
begin
  FSaveFileData.Free;
  inherited Destroy;
end;

procedure TmFileOpenSave.Init(aFileSystemManager: TmAbstractFileSystemManager; const aFileOperation : TmFileOperation);
var
  i : integer;
begin
  FFileOperation:= aFileOperation;
  FManager := aFileSystemManager;

  TVFolders.BeginUpdate;
  try
    TVFolders.Items.Clear;
    for i := 0 to aFileSystemManager.Roots.Count - 1 do
    begin
      Self.AddNode(nil, aFileSystemManager.Roots.Items[i]);
    end;
  finally
    TVFolders.EndUpdate;
  end;
  if FFileOperation = fmOpen then
    EditFileName.ReadOnly:= true;
end;

function TmFileOpenSave.GetFileData: TmFileData;
begin
  Result := nil;
  if FFileOperation = fmOpen then
  begin
    if Assigned(FCurrentFile) then
    begin
      Result := FCurrentFile.FileData;
    end;
  end
  else if FFileOperation = fmSave then
  begin
    FSaveFileData.FileName:= EditFileName.Text;
    FSaveFileData.Path:= FCurrentFolder.Path;
    Result := FSaveFileData;
  end;
end;

end.

