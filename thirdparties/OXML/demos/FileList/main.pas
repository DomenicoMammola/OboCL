unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ComCtrls, ExtCtrls,
  OEncoding, OXmlUtils, OXmlPDOM, OXmlReadWrite, OXmlPSeq;

type
  TfMain = class(TForm)
    Label2: TLabel;
    eFileName: TEdit;
    Label10: TLabel;
    Label1: TLabel;
    cobDrive: TDriveComboBox;
    cobCodePage: TComboBox;
    Label9: TLabel;
    bDriveSaveDOM: TButton;
    Bevel1: TBevel;
    bDriveLoadDOM: TButton;
    tvDrive: TTreeView;
    Label11: TLabel;
    Bevel2: TBevel;
    Label3: TLabel;
    mDescription: TMemo;
    rgOutputFormat: TRadioGroup;
    chbPreserveWhiteSpace: TCheckBox;
    bDriveSaveDirect: TButton;
    bDriveLoadSeq: TButton;
    bvl1: TBevel;
    Lbl1: TLabel;
    LblTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cobDriveChange(Sender: TObject);
    procedure bDriveSaveDOMClick(Sender: TObject);
    procedure bDriveSaveDirectClick(Sender: TObject);
    procedure bDriveLoadDOMClick(Sender: TObject);
    procedure bDriveLoadSeqClick(Sender: TObject);
  private
    DocPath: string;
    procedure StartDOM(const Dir: string;
      const Element: PXMLNode; const MaxLevel: Integer);
    procedure StartDirect(const Dir: string;
      var Element: TXMLWriterElement; const MaxLevel: Integer);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

const
  USE_DIR_LEVELS = 6;//directory levels to write

{$R *.DFM}

procedure XML2TreeViewDOM(const FileName: String;
  const PreserveWhiteSpace: Boolean; const TreeView: TTreeView;
  var EllapsedMS, ItemsCreated: Integer);
var
  RootNode: TTreeNode;
  XMLDoc: IXMLDocument;
  xStartMS: Cardinal;

  procedure AddNode(const Parent: TTreeNode; const Node: PXMLNode);
  var
    i: Integer;
    SubNode: PXMLNode;
    TreeNode: TTreeNode;
    NodeName: string;
  begin
    for i := 0 to Node.ChildNodes.Count - 1 do begin
      SubNode := Node.ChildNodes[i];
      NodeName := SubNode.NodeName;

      if NodeName = 'dir' then
        NodeName := SubNode.GetAttribute('name');
      if NodeName = 'file' then
        NodeName := SubNode.GetAttribute('name');

      TreeNode := TreeView.Items.AddChild(Parent, NodeName);
      Inc(ItemsCreated);
      AddNode(TreeNode, SubNode);
    end;
  end;
begin
  ItemsCreated := 0;

  XMLDoc := CreateXMLDoc;
  if PreserveWhiteSpace then
    XMLDoc.WhiteSpaceHandling := wsPreserveInTextOnly
  else
    XMLDoc.WhiteSpaceHandling := wsTrim;


  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    xStartMS := GetTickCount;
    XMLDoc.LoadFromFile(FileName);
    if XMLDoc.DocumentElement <> nil then begin
      RootNode := TreeView.Items.Add(nil,
        Format('DRIVE %s', [XMLDoc.DocumentElement.GetAttribute('name')]));
      AddNode(RootNode, XMLDoc.DocumentElement);
    end else
      RootNode := nil;
    EllapsedMS := GetTickCount-xStartMS;

    if Assigned(RootNode) then
      RootNode.Expand(False);
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure XML2TreeViewSeq(const FileName: String;
  const PreserveWhiteSpace: Boolean; const TreeView: TTreeView;
  var EllapsedMS, ItemsCreated: Integer);
var
  RootNode: TTreeNode;
  XMLSeq: TXMLSeqParser;
  xStartMS: Cardinal;

  procedure AddNode(const Parent: TTreeNode);
  var
    SubNode: PXMLNode;
    SubNodeOpened: Boolean;
    TreeNode: TTreeNode;
    NodeName: string;
  begin
    while XMLSeq.ReadNextChildElementHeader(SubNode, SubNodeOpened) do
    begin
      NodeName := SubNode.GetAttribute('name');

      TreeNode := TreeView.Items.AddChild(Parent, NodeName);
      Inc(ItemsCreated);
      if SubNodeOpened then
        AddNode(TreeNode);
    end;
  end;

var
  DriveNode: PXMLNode;
  DriveNodeOpened: Boolean;
begin
  ItemsCreated := 0;
  TreeView.Items.BeginUpdate;
  XMLSeq := TXMLSeqParser.Create;
  try
    XMLSeq.InitFile(FileName);

    TreeView.Items.Clear;

    if PreserveWhiteSpace then
      XMLSeq.WhiteSpaceHandling := wsPreserveInTextOnly
    else
      XMLSeq.WhiteSpaceHandling := wsTrim;

    xStartMS := GetTickCount;

    XMLSeq.GoToPath('/drive');
    if XMLSeq.ReadNextChildElementHeader(DriveNode, DriveNodeOpened) then
    begin
      RootNode := TreeView.Items.Add(nil,
        Format('DRIVE %s', [DriveNode.GetAttribute('name')]));

      if DriveNodeOpened then
        AddNode(RootNode);
    end else
      RootNode := nil;

    EllapsedMS := GetTickCount-xStartMS;

    if Assigned(RootNode) then
      RootNode.Expand(False);
  finally
    TreeView.Items.EndUpdate;
    XMLSeq.Free;
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  DocPath := ExtractFilePath(ExpandFileName(ExtractFilePath(Application.ExeName) + '..\doc\dummy.xml'));

  cobDrive.Drive := ' ';
  cobDrive.Drive := ExtractFileDrive(Application.ExeName)[1];

  cobCodePage.Items.Add('utf-8');
  cobCodePage.Items.Add('utf-16');
  cobCodePage.Items.Add('windows-1250');
  cobCodePage.Items.Add('windows-1251');
  cobCodePage.Items.Add('windows-1252');
  cobCodePage.Items.Add('iso-8859-1');
  cobCodePage.Items.Add('iso-8859-2');
  cobCodePage.ItemIndex := 0;

  ReportMemoryLeaksOnShutdown := True;
end;

procedure TfMain.bDriveLoadSeqClick(Sender: TObject);
var
  EllapsedMS, ItemsCreated: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    XML2TreeViewSeq(eFileName.Text, chbPreserveWhiteSpace.Checked,
      tvDrive, EllapsedMS, ItemsCreated);
  finally
    Screen.Cursor := crDefault;
  end;

  LblTime.Caption := FloatToStr(EllapsedMS/1000) + ' s : '+IntToStr(ItemsCreated);
end;

procedure TfMain.bDriveSaveDirectClick(Sender: TObject);
var
  XMLWriter: TXMLWriter;
  RootElement: TXMLWriterElement;
  xT: Cardinal;
  xEncoding: TEncoding;
begin
  xT := GetTickCount;

  Screen.Cursor := crHourGlass;
  XMLWriter := TXMLWriter.Create;
  try
    XMLWriter.InitFile(eFileName.Text);
    if TEncoding.EncodingFromAlias(cobCodePage.Items[cobCodePage.ItemIndex], xEncoding) then
      XMLWriter.Encoding := xEncoding;

    try
      XMLWriter.WriterSettings.IndentType := TXmlIndentType(rgOutputFormat.ItemIndex);
      XMLWriter.XMLDeclaration;

      // create root element
      RootElement := XMLWriter.OpenElementR('drive');
      RootElement.Attribute('name', cobDrive.Drive);

      // start recursive scan of selected drive
      StartDirect(cobDrive.Drive + ':', RootElement, USE_DIR_LEVELS);
      RootElement.CloseElement;
      // save document to file
      ForceDirectories(ExtractFileDir(eFileName.Text));
    finally
      XMLWriter.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  LblTime.Caption := FloatToStr((GetTickCount-xT)/1000) + ' s';
end;

procedure TfMain.bDriveSaveDOMClick(Sender: TObject);
var
  PI: PXMLNode;
  XMLDoc: IXMLDocument;
  RootElement: PXMLNode;
  xT: Cardinal;
begin
  xT := GetTickCount;

  Screen.Cursor := crHourGlass;
  try
    XMLDoc := CreateXMLDoc;
    XMLDoc.WriterSettings.IndentType := TXmlIndentType(rgOutputFormat.ItemIndex);
    // create xml declaration
    PI := XMLDoc.Node.AddXMLDeclaration;
    PI.AddAttribute('version', '1.0');
    PI.AddAttribute('encoding', cobCodePage.Items[cobCodePage.ItemIndex]);

    // create root element
    RootElement := XMLDoc.Node.AddChild('drive');

    RootElement.SetAttribute('name', cobDrive.Drive);
    // start recursive scan of selected drive
    StartDOM(cobDrive.Drive + ':', RootElement, USE_DIR_LEVELS);
    // save document to file
    ForceDirectories(ExtractFileDir(eFileName.Text));
    XMLDoc.SaveToFile(eFileName.Text);
  finally
    Screen.Cursor := crDefault;
  end;

  LblTime.Caption := FloatToStr((GetTickCount-xT)/1000) + ' s';
end;

procedure TfMain.StartDirect(const Dir: string;
  var Element: TXMLWriterElement; const MaxLevel: Integer);
var
  sr: TSearchRec;
  SingleFile,
  SubDirectory: TXMLWriterElement;
  Attributes: string;
begin
  if MaxLevel < 0 then
    Exit;

  if FindFirst(Dir + '\*.*', faAnyFile, sr) = 0 then begin
    try
      repeat
        if (sr.Attr and faDirectory) > 0 then begin
          if (sr.Name <> '') and (sr.Name[1] <> '.') then begin
            // sub-directory
            SubDirectory := Element.OpenElementR('dir');
            SubDirectory.Attribute('name', sr.Name);
            StartDirect(Dir + '\' + sr.Name, SubDirectory, MaxLevel-1);
            SubDirectory.CloseElement;
          end
        end
        else begin
          SingleFile := Element.OpenElementR('file');
          SingleFile.Attribute('name', sr.Name);
          SingleFile.Attribute('size', IntToStr(sr.Size));
          Attributes := '';
          if (sr.Attr and faReadOnly) > 0 then
            Attributes := Attributes + 'R';
          if (sr.Attr and faHidden) > 0 then
            Attributes := Attributes + 'H';
          if (sr.Attr and faSysFile) > 0 then
            Attributes := Attributes + 'S';
          if (sr.Attr and faArchive) > 0 then
            Attributes := Attributes + 'A';
          SingleFile.Attribute('attr', Attributes);
          SingleFile.CloseElement;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TfMain.StartDOM(const Dir: string;
  const Element: PXMLNode; const MaxLevel: Integer);
var
  sr: TSearchRec;
  SingleFile,
  SubDirectory: PXMLNode;
  Attributes: string;
begin
  if MaxLevel < 0 then
    Exit;

  if FindFirst(Dir + '\*.*', faAnyFile, sr) = 0 then begin
    try
      repeat
        if (sr.Attr and faDirectory) > 0 then begin
          if (sr.Name <> '') and (sr.Name[1] <> '.') then begin
            // sub-directory
            SubDirectory := Element.AddChild('dir');
            SubDirectory.SetAttribute('name', sr.Name);
            StartDOM(Dir + '\' + sr.Name, SubDirectory, MaxLevel-1);
          end
        end
        else begin
          SingleFile := Element.AddChild('file');
          SingleFile.SetAttribute('name', sr.Name);
          SingleFile.SetAttribute('size', IntToStr(sr.Size));
          Attributes := '';
          if (sr.Attr and faReadOnly) > 0 then
            Attributes := Attributes + 'R';
          if (sr.Attr and faHidden) > 0 then
            Attributes := Attributes + 'H';
          if (sr.Attr and faSysFile) > 0 then
            Attributes := Attributes + 'S';
          if (sr.Attr and faArchive) > 0 then
            Attributes := Attributes + 'A';
          SingleFile.SetAttribute('attr', Attributes);
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TfMain.bDriveLoadDOMClick(Sender: TObject);
var
  EllapsedMS, ItemsCreated: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    XML2TreeViewDOM(eFileName.Text, chbPreserveWhiteSpace.Checked,
      tvDrive, EllapsedMS, ItemsCreated);
  finally
    Screen.Cursor := crDefault;
  end;

  LblTime.Caption := FloatToStr(EllapsedMS/1000) + ' s : '+IntToStr(ItemsCreated);
end;

procedure TfMain.cobDriveChange(Sender: TObject);
begin
  eFileName.Text := Format('%sdrive_%s.xml', [DocPath, cobDrive.Drive]);
end;

end.

