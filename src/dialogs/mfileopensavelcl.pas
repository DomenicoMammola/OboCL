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
    Splitter1: TSplitter;
    TVFolders: TTreeView;
    procedure LVFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TVFoldersSelectionChanged(Sender: TObject);
  private
    FCurrentFolder : TmFolder;
    FCurrentFile : TmFile;
    FFileOperation : TmFileOperation;
    procedure AddNode (aParent : TTreeNode; aFolder : TmFolder);
    procedure RefreshFiles;
  public
    procedure Init (aFileSystemManager : TmAbstractFileSystemManager; const aFileOperation : TmFileOperation);
  end;


implementation

{$R *.lfm}

{ TmFileOpenSave }

procedure TmFileOpenSave.TVFoldersSelectionChanged(Sender: TObject);
begin
  if TVFolders.SelectionCount > 0 then
    FCurrentFolder := TmFolder(TVFolders.Selected.Data)
  else
    FCurrentFolder := nil;
  RefreshFiles;
end;

procedure TmFileOpenSave.LVFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  EditFileName.Text := '';
  if Assigned(Item) and Assigned (FCurrentFolder) then
  begin
    EditFileName := TmFile(Item.Data).DisplayName;
    FCurrentFile := TmFile(Item.Data);
  end;
end;

procedure TmFileOpenSave.AddNode(aParent: TTreeNode; aFolder: TmFolder);
var
  newNode : TTreeNode;
  i : integer;
begin
  newNode := TVFolders.Items.Add(aParent, aFolder.DisplayName);
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
        LVFiles.AddItem(FCurrentFolder.Files[i].DisplayName, FCurrentFolder.Files[i]);
      end;
    end;
  finally
    LVFiles.EndUpdate;
  end;
end;

procedure TmFileOpenSave.Init(aFileSystemManager: TmAbstractFileSystemManager; const aFileOperation : TmFileOperation);
var
  i : integer;
begin
  FFileOperation:= aFileOperation;
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
end;

end.

