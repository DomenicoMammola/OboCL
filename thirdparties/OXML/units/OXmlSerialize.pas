unit OXmlSerialize;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    commercial
    Please see the /license.txt file for more information.

}

{
  OXmlSerialize.pas

  Automatic XML serializer/deserializer with basic properties supported
  by TypInfo (all Delphi versions and also FPC).

  Properties have to be published in order to be handled by SerDes!
  Objects must be descendants of TPersistent.

  Supported types:
    - Ordinal (Integer, enum, set, char, WideChar).
    - String (string, WideString).
    - Float (Date, Time, DateTime, Float).
    - Int64
    - Objects (TPersistent descendant).
    - Collections (TCollection and TCollectionItem descendants).

}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes, System.TypInfo, System.DateUtils,
    {$IFDEF O_GENERICS}
    System.Generics.Collections,
    {$ENDIF}
  {$ELSE}
  SysUtils, Classes, TypInfo,
    {$IFDEF O_GENERICS}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF O_GENERICS}
  ODictionary,
  {$ENDIF}
  OWideSupp, OEncoding, OTextReadWrite, OXmlReadWrite, OXmlPDOM, OXmlPSeq,
  OXmlUtils;

type
  TCustomXMLSerDes = class(TObject)
  private
    fUseRoot: Boolean;
    fCollectionStyle: TXMLSerializeCollectionStyle;
  protected
    procedure DoCreate; virtual;

    procedure SetUseRoot(const aUseRoot: Boolean); virtual;
  public
    constructor Create;
  public
    //use root
    property UseRoot: Boolean read fUseRoot write SetUseRoot;
    //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
    //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
    property CollectionStyle: TXMLSerializeCollectionStyle read fCollectionStyle write fCollectionStyle;
  end;

  TXMLSerializer = class(TCustomXMLSerDes)
  private
    fWriter: TXMLWriter;
    fRootElementWritten: Boolean;

    fXMLDeclaration: TXMLWriterDeclaration;
    fRootNodeName: OWideString;
    fWriteDefaultValues: Boolean;
  private
    function GetWriterSettings: TXMLWriterSettings;
    procedure SetRootNodeName(const aRootNodeName: OWideString);
  protected
    procedure SetUseRoot(const aUseRoot: Boolean); override;

    procedure DoCreate; override;
    procedure DoInit;

    procedure WriteRootStartElement;
    procedure WriteRootEndElement;

    procedure WriteObjectProperties(const aObject: TPersistent;
      var ioElement: TXMLWriterElement);
    procedure WriteObjectProperty(const aObject: TPersistent; const aPropInfo: PPropInfo;
      var ioElement: TXMLWriterElement);
    procedure WriteCollectionItems(const aCollection: TCollection;
      var ioElement: TXMLWriterElement);
    procedure WriteStringsItems(const aStrings: TStrings;
      var ioElement: TXMLWriterElement);
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLSerializer or call ReleaseDocument!

    procedure InitFile(const aFileName: OWideString);
    procedure InitStream(const aStream: TStream; const aOwnsStream: Boolean = False);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //Write object to XML
    //  aElementName: use custom XML element name
    //  aWriteObjectType: if true, write the real object type to "type" XML attribute
    procedure WriteObject(const aObject: TPersistent); overload;
    procedure WriteObject(const aObject: TPersistent;
      const aElementName: OWideString; const aWriteObjectType: Boolean = True); overload;
  public
    //write XML declaration <?xml ?>
    property XMLDeclaration: TXMLWriterDeclaration read fXMLDeclaration;

    //use root
    //  - true: a document root (RootNodeName) will be written
    //  please note that if you disable it (UseRoot = false) and
    //  write more objects to the XML, the XML won't be valid because
    //  XML documents can have only one root
    property UseRoot;
    //custom root node
    property RootNodeName: OWideString read fRootNodeName write SetRootNodeName;
    //write object properties with default values?
    property WriteDefaultValues: Boolean read fWriteDefaultValues write fWriteDefaultValues;
    //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
    //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
    property CollectionStyle;

    //XML writer settings
    property WriterSettings: TXMLWriterSettings read GetWriterSettings;
  end;

  TXMLDeserializer = class(TCustomXMLSerDes)
  private
    fXMLParser: TXMLSeqParser;
    fRootNode, fCurrentElementNode: PXMLNode;
    fErrorHandling: TXMLDeserializeErrorHandling;

    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IOTextParseError;
  protected
    procedure SetUseRoot(const aUseRoot: Boolean); override;

    procedure DoCreate; override;
    procedure DoInit;

    procedure ReadObjectProperties(const aObject: TPersistent; const aElementNode: PXMLNode);
    procedure ReadObjectProperty(const aObject: TPersistent; const aPropInfo: PPropInfo;
      const aElementNode: PXMLNode; var ioPropNameIndex: TXMLNodeIndex);
    procedure ReadCollectionItems(const aCollection: TCollection;
      const aEnumerationNode: PXMLNode; const aChildName: OWideString);
    procedure ReadStringItems(const aStrings: TStrings;
      const aEnumerationNode: PXMLNode);
  public
    destructor Destroy; override;
  public
    //The Init* procedures open and initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached or you call ReleaseDocument!

    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil);
    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aForceEncoding: TEncoding = nil);
    //init XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitXML(const aXML: OWideString);
    procedure InitXML_UTF8(const aXML: OUTF8Container);
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //use root - please use the same setting here as in TXMLSerialize
    property UseRoot;

    //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
    //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
    property CollectionStyle;
  public
    //following functions and properties can be called only during parsing (after Init* has been called).

    //Find next element
    //  outElementName -> name of the XML element used (is the same with outType if TXMLSerializer.WriteObject was not executed with custom name)
    //  outType -> type of the object - differs from outElementName only if TXMLSerializer.WriteObject was executed with custom name
    function ReadObjectInfo(var outElementName: OWideString): Boolean; overload;
    function ReadObjectInfo(var outElementName, outType: OWideString): Boolean; overload;
    //If an element was found with ReadElementInfo, read it into an instance
    procedure ReadObject(const aObject: TPersistent);
    //Read object from any other (externally-defined) XML element node
    procedure ReadObjectFromNode(const aObject: TPersistent; const aElementNode: PXMLNode);

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read GetParseError;

    //What happens when an invalid XML value is found
    property ErrorHandling: TXMLDeserializeErrorHandling read fErrorHandling write fErrorHandling;
  end;

  EXMLSerializer = class(Exception);
  EXMLDeserializer = class(Exception);

implementation

uses
  OXmlLng, OHashedStrings
  {$IFDEF O_DELPHI_2007_DOWN}
  {$IFNDEF O_KYLIX}
  , Controls//definition of TTime and TDate
  {$ENDIF}
  {$ENDIF};

{ TCustomXMLSerDes }

constructor TCustomXMLSerDes.Create;
begin
  DoCreate;

  inherited Create;
end;

procedure TCustomXMLSerDes.DoCreate;
begin
  fUseRoot := True;
end;

procedure TCustomXMLSerDes.SetUseRoot(const aUseRoot: Boolean);
begin
  fUseRoot := aUseRoot;
end;

{ TXMLSerializer }

constructor TXMLSerializer.Create;
begin
  inherited Create;
end;

constructor TXMLSerializer.Create(const aStream: TStream);
begin
  inherited Create;

  InitStream(aStream);
end;

destructor TXMLSerializer.Destroy;
begin
  ReleaseDocument;
  fWriter.Free;
  fXMLDeclaration.Free;

  inherited;
end;

procedure TXMLSerializer.DoCreate;
begin
  inherited DoCreate;

  fWriter := TXMLWriter.Create;
  fXMLDeclaration := TXMLWriterDeclaration.Create;

  fRootNodeName := 'oxmlserializer';
end;

procedure TXMLSerializer.DoInit;
begin
  fRootElementWritten := False;
end;

function TXMLSerializer.GetWriterSettings: TXMLWriterSettings;
begin
  Result := fWriter.WriterSettings;
end;

procedure TXMLSerializer.InitFile(const aFileName: OWideString);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLSerializer.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  fWriter.InitStream(aStream, aOwnsStream);

  DoInit;
end;

procedure TXMLSerializer.ReleaseDocument;
begin
  WriteRootEndElement;

  fWriter.ReleaseDocument;
end;

procedure TXMLSerializer.SetRootNodeName(const aRootNodeName: OWideString);
begin
  if fRootElementWritten then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeRootNodeName)
  else
    fRootNodeName := aRootNodeName;
end;

procedure TXMLSerializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fRootElementWritten then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeUseRootDataWritten)
  else
    inherited SetUseRoot(aUseRoot);
end;

procedure TXMLSerializer.WriteObject(const aObject: TPersistent);
begin
  WriteObject(aObject, aObject.ClassName, False);
end;

procedure TXMLSerializer.WriteCollectionItems(const aCollection: TCollection;
  var ioElement: TXMLWriterElement);
var
  xColElem: TXMLWriterElement;
  xColItem: TCollectionItem;
  I: Integer;
begin
  if aCollection.Count = 0 then
    Exit;

  ioElement.FinishOpenElement;
  if CollectionStyle = csOXml then
    ioElement.OpenElementR('_oxmlcollection', xColElem{%H-}, stFinish);

  for I := 0 to aCollection.Count-1 do
  begin
    xColItem := aCollection.Items[I];
    case CollectionStyle of
      csOXml: WriteObject(xColItem, 'i', False);
      csOmniXML: WriteObject(xColItem, xColItem.ClassName, False);
    end;
  end;

  if CollectionStyle = csOXml then
    xColElem.CloseElement;
end;

procedure TXMLSerializer.WriteObject(const aObject: TPersistent;
  const aElementName: OWideString; const aWriteObjectType: Boolean);
var
  xElement: TXMLWriterElement;
begin
  WriteRootStartElement;

  if (GetTypeData(aObject.ClassInfo)^.PropCount = 0) and
     not (aObject is TCollection) and//always write TCollection
     not (aObject is TStrings)//always write TStrings
  then
    Exit;

  fWriter.OpenElementR(aElementName, xElement{%H-});
  if aWriteObjectType and (aElementName <> aObject.ClassName) then
    xElement.Attribute('type', aObject.ClassName);
  WriteObjectProperties(aObject, xElement);
  xElement.CloseElement;
end;

procedure TXMLSerializer.WriteObjectProperties(const aObject: TPersistent;
  var ioElement: TXMLWriterElement);
var
  I: Integer;
  xPropCount: Integer;
  xPropList: PPropList;
  xPropInfo: PPropInfo;
begin
  xPropCount := GetTypeData(aObject.ClassInfo)^.PropCount;
  if xPropCount > 0 then
  begin
    GetMem(xPropList, xPropCount*SizeOf(Pointer));
    try
      GetPropInfos(aObject.ClassInfo, xPropList);
      for I := 0 to xPropCount-1 do
      begin
        xPropInfo := xPropList^[I];
        if Assigned(xPropInfo) and IsStoredProp(aObject, xPropInfo) then
          WriteObjectProperty(aObject, xPropInfo, ioElement);
      end;
    finally
      FreeMem(xPropList, xPropCount*SizeOf(Pointer));
    end;
  end;

  if aObject is TCollection then
    WriteCollectionItems(TCollection(aObject), ioElement)
  else
  if aObject is TStrings then
    WriteStringsItems(TStrings(aObject), ioElement);
end;

procedure TXMLSerializer.WriteObjectProperty(const aObject: TPersistent;
  const aPropInfo: PPropInfo; var ioElement: TXMLWriterElement);

  procedure _Write(const bValue: OWideString);
  var
    xPropElement: TXMLWriterElement;
  begin
    ioElement.OpenElementR(SymbolNameToString(@aPropInfo^.Name), xPropElement{%H-});
    xPropElement.Text(bValue, False);
    xPropElement.CloseElement(False);
  end;

  procedure _WriteClass(const bObject: TObject);
  var
    xPropElement: TXMLWriterElement;
  begin
    if bObject is TPersistent then
    begin
      if GetTypeData(aObject.ClassInfo)^.PropCount = 0 then
        Exit;

      ioElement.OpenElementR(SymbolNameToString(@aPropInfo^.Name), xPropElement{%H-});
      WriteObjectProperties(TPersistent(bObject), xPropElement);
      xPropElement.CloseElement;
    end;
  end;
var
  xPropType: PTypeInfo;
  xOrdValue: Integer;
  xFloatValue: Double;
begin
  if Assigned(aPropInfo^.GetProc) then
  begin
    xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};
    case xPropType^.Kind of
      tkInteger, tkChar, tkWChar, {$IFDEF FPC}tkUChar, tkBool,{$ENDIF} tkEnumeration, tkSet:
      begin
        xOrdValue := GetOrdProp(aObject, aPropInfo);
        if fWriteDefaultValues or (aPropInfo^.Default <> xOrdValue) then
        case xPropType^.Kind of
          {$IFDEF FPC}tkBool: _Write(IntToStr(xOrdValue));{$ENDIF}//save boolean values as integer
          tkInteger: _Write(IntToStr(xOrdValue));
          tkChar: _Write(OWideString(Char(xOrdValue)));
          tkWChar: _Write(OWideString(WideChar(xOrdValue)));
          {$IFDEF FPC}tkUChar: _Write(OWideString(UnicodeChar(xOrdValue)));{$ENDIF}
          tkEnumeration: _Write(GetEnumName(xPropType, xOrdValue));
          tkSet: _Write(GetSetProp(aObject, aPropInfo, False));
        end;
      end;
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString {$ENDIF}:
        _Write(GetStrProp(aObject, aPropInfo));
      {$IFDEF O_HASBYTESTRINGS}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        _Write({$IFDEF FPC}UTF8Encode{$ENDIF}(GetWideStrProp(aObject, aPropInfo)));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        xFloatValue := GetFloatProp(aObject, aPropInfo);
        if (xPropType = System.TypeInfo(TDateTime)) then
          _Write(ISODateTimeToStr(xFloatValue))
        {$IFNDEF O_KYLIX}
        else if (xPropType = System.TypeInfo(TTime)) then
          _Write(ISOTimeToStr(xFloatValue))
        else if (xPropType = System.TypeInfo(TDate)) then
          _Write(ISODateToStr(xFloatValue))
        {$ENDIF}
        else
          _Write(ISOFloatToStr(xFloatValue));
      end;
      tkInt64:
        _Write(IntToStr(GetInt64Prop(aObject, aPropInfo)));
      tkClass:
        _WriteClass(GetObjectProp(aObject, aPropInfo));
    end;
  end;
end;

procedure TXMLSerializer.WriteRootEndElement;
begin
  if UseRoot and fRootElementWritten then
    fWriter.CloseElement(fRootNodeName);
end;

procedure TXMLSerializer.WriteRootStartElement;
begin
  if not fRootElementWritten then
  begin
    fXMLDeclaration.WriteIfEnabled(fWriter);

    if UseRoot then
      fWriter.OpenElement(fRootNodeName, stFinish);

    fRootElementWritten := True;
  end;
end;

procedure TXMLSerializer.WriteStringsItems(const aStrings: TStrings;
  var ioElement: TXMLWriterElement);
var
  xStringsElem, xStrElem: TXMLWriterElement;
  I: Integer;
begin
  if aStrings.Count = 0 then
    Exit;

  ioElement.FinishOpenElement;
  if CollectionStyle = csOXml then
    ioElement.OpenElementR('_oxmlstrings', xStringsElem{%H-}, stFinish);

  for I := 0 to aStrings.Count-1 do
  begin
    case CollectionStyle of
      csOXml: ioElement.OpenElementR('i', xStrElem{%H-}, stFinish);
      csOmniXML: ioElement.OpenElementR('l'+IntToStr(I), xStrElem{%H-}, stFinish);
    end;
    xStrElem.Text(aStrings[I], False);
    xStrElem.CloseElement(False);
  end;

  if CollectionStyle = csOXml then
    xStringsElem.CloseElement;
end;

{ TXMLDeserializer }

destructor TXMLDeserializer.Destroy;
begin
  fXMLParser.Free;

  inherited;
end;

procedure TXMLDeserializer.DoCreate;
begin
  inherited DoCreate;

  fXMLParser := TXMLSeqParser.Create;

  fErrorHandling := dehRaiseException;
end;

procedure TXMLDeserializer.DoInit;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

function TXMLDeserializer.GetApproxStreamPosition: OStreamInt;
begin
  Result := fXMLParser.ApproxStreamPosition;
end;

function TXMLDeserializer.GetParseError: IOTextParseError;
begin
  Result := fXMLParser.ParseError;
end;

function TXMLDeserializer.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fXMLParser.ReaderSettings;
end;

function TXMLDeserializer.GetStreamSize: OStreamInt;
begin
  Result := fXMLParser.StreamSize;
end;

procedure TXMLDeserializer.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aBufferLength, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitFile(aFileName, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitStream(aStream, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitXML(const aXML: OWideString);
begin
  fXMLParser.InitXML(aXML);
  DoInit;
end;

procedure TXMLDeserializer.InitXML_UTF8(const aXML: OUTF8Container);
begin
  fXMLParser.InitXML_UTF8(aXML);
  DoInit;
end;

procedure TXMLDeserializer.ReadCollectionItems(const aCollection: TCollection;
  const aEnumerationNode: PXMLNode; const aChildName: OWideString);
var
  xItemNode: PXMLNode;
  xNewItem: TCollectionItem;
  xChildNameId: OHashedStringsIndex;
begin
  aCollection.BeginUpdate;
  try
    aCollection.Clear;

    xChildNameId := aEnumerationNode.OwnerDocument.IndexOfString(aChildName);
    if xChildNameId < 0 then
      Exit;

    xItemNode := aEnumerationNode.FirstChild;
    while Assigned(xItemNode) do
    begin
      if xItemNode.NodeNameId = xChildNameId then
      begin
        xNewItem := aCollection.Add;

        ReadObjectFromNode(xNewItem, xItemNode);
      end;
      xItemNode := xItemNode.NextSibling;
    end;
  finally
    aCollection.EndUpdate;
  end;
end;

procedure TXMLDeserializer.ReadObject(const aObject: TPersistent);
begin
  if not Assigned(fCurrentElementNode) then
    raise EXMLDeserializer.Create(OXmlLng_WrongDeserializerSequence);

  ReadObjectFromNode(aObject, fCurrentElementNode);

  fCurrentElementNode := nil;
end;

procedure TXMLDeserializer.ReadObjectFromNode(const aObject: TPersistent;
  const aElementNode: PXMLNode);
begin
  ReadObjectProperties(aObject, aElementNode);
end;

function TXMLDeserializer.ReadObjectInfo(var outElementName,
  outType: OWideString): Boolean;
var
  xRootNodeOpen: Boolean;
begin
  if UseRoot and not Assigned(fRootNode) then
  begin
    Result :=
      fXMLParser.ReadNextChildElementHeader(fRootNode, xRootNodeOpen{%H-}) and//no root element
      xRootNodeOpen;//there are no elements in root

    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;
  end;

  repeat
    Result := fXMLParser.ReadNextChildNode({%H-}fCurrentElementNode);
    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;

    Result := (fCurrentElementNode.NodeType = ntElement);
  until Result;

  //Result = true here
  outElementName := fCurrentElementNode.NodeName;
  outType := fCurrentElementNode.GetAttribute('type');
  if outType = '' then
    outType := outElementName;
end;

function TXMLDeserializer.ReadObjectInfo(var outElementName: OWideString): Boolean;
var
  xType: OWideString;
begin
  Result := ReadObjectInfo(outElementName, xType{%H-});
end;

procedure TXMLDeserializer.ReadObjectProperties(const aObject: TPersistent;
  const aElementNode: PXMLNode);
var
  I: Integer;
  xPropCount: Integer;
  xPropList: PPropList;
  xPropInfo: PPropInfo;
  xPropNameIndex: TXMLNodeIndex;
  xEnumerationNode: PXMLNode;
begin
  xPropNameIndex := nil;
  try
    xPropCount := GetTypeData(aObject.ClassInfo)^.PropCount;
    if xPropCount > 0 then
    begin
      GetMem(xPropList, xPropCount*SizeOf(Pointer));
      try
        GetPropInfos(aObject.ClassInfo, xPropList);
        for I := 0 to xPropCount-1 do
        begin
          xPropInfo := xPropList^[I];
          if Assigned(xPropInfo) then
            ReadObjectProperty(aObject, xPropInfo, aElementNode, xPropNameIndex);
        end;
      finally
        FreeMem(xPropList, xPropCount*SizeOf(Pointer));
      end;
    end;
  finally
    xPropNameIndex.Free;
  end;

  if (aObject is TCollection) then
  begin
    case CollectionStyle of
      csOXml:
        if aElementNode.SelectNode('_oxmlcollection', xEnumerationNode{%H-}) then
          ReadCollectionItems(TCollection(aObject), xEnumerationNode, 'i');
      csOmniXML:
        ReadCollectionItems(TCollection(aObject), aElementNode, TCollection(aObject).ItemClass.ClassName);
    end;
  end else
  if (aObject is TStrings) then
  begin
    case CollectionStyle of
      csOXml:
        if aElementNode.SelectNode('_oxmlstrings', xEnumerationNode{%H-}) then
          ReadStringItems(TStrings(aObject), xEnumerationNode);
      csOmniXML:
        ReadStringItems(TStrings(aObject), aElementNode);
    end;
  end;
end;

procedure TXMLDeserializer.ReadObjectProperty(const aObject: TPersistent;
  const aPropInfo: PPropInfo; const aElementNode: PXMLNode;
  var ioPropNameIndex: TXMLNodeIndex);

  procedure _ReadClass(const bPropElement: PXMLNode);
  var
    xPropObject: TObject;
  begin
    xPropObject := GetObjectProp(aObject, aPropInfo);
    if Assigned(xPropObject) and (xPropObject is TPersistent) then
      ReadObjectProperties(TPersistent(xPropObject), bPropElement);
  end;
var
  xPropType: PTypeInfo;
  xPropElement: PXMLNode;
  xStrValue: OWideString;
  {$IFDEF FPC}
  xUStrValue: UnicodeString;
  {$ENDIF}
  xOrdValue: Integer;
  xDoubleValue: Double;
  xExtendedValue: Extended;
  xResult: Boolean;
  xInt64Value: int64;

  procedure _Raise;
  begin
    raise EXMLDeserializer.CreateFmt(OXmlLng_InvalidValue, [xStrValue, xPropElement.NodePath, xPropType^.Name]);
  end;
  function _RaiseOrd: Boolean;
  begin
    Result := fErrorHandling <> dehIgnore;
    case fErrorHandling of
      dehRaiseException: _Raise;
      dehUseDefaultValue: xOrdValue := 0;
    end;
  end;
begin
  if not Assigned(aPropInfo^.GetProc) then
    Exit;

  if not aElementNode.FindChildWithIndex(SymbolNameToString(@aPropInfo^.Name),
    xPropElement{%H-}, ioPropNameIndex)
  then
    Exit;

  xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};

  if xPropType^.Kind = tkClass then
  begin
    _ReadClass(xPropElement);
  end else
  begin
    xStrValue := xPropElement.Text;
    case xPropType^.Kind of
      tkInteger, tkChar, tkWChar, {$IFDEF FPC}tkUChar, tkBool,{$ENDIF} tkEnumeration:
      begin
        case xPropType^.Kind of
          tkInteger {$IFDEF FPC}, tkBool{$ENDIF}://save boolean values as integer
            if not TryStrToInt(xStrValue, xOrdValue) then
            if not _RaiseOrd then
              Exit;
          tkChar {$IFNDEF FPC}, tkWChar{$ENDIF}:
            if (Length(xStrValue) = 1) then
              xOrdValue := Integer(xStrValue[1])
            else
            if not _RaiseOrd then
              Exit;
          {$IFDEF FPC}
          tkWChar, tkUChar:
            begin
              xUStrValue := UTF8Decode(xStrValue)[1];
              if (Length(xUStrValue) = 1) then
                xOrdValue := Integer(xUStrValue[1])
              else
              if not _RaiseOrd then
                Exit;
            end;
          {$ENDIF}
          tkEnumeration:
            begin
              xOrdValue := GetEnumValue(xPropType, xStrValue);
              if xOrdValue < GetTypeData(xPropType)^.MinValue then
              if not _RaiseOrd then
                Exit;
            end;
        else
          xOrdValue := 0;
        end;
        SetOrdProp(aObject, aPropInfo, xOrdValue);
      end;
      tkSet:
        SetSetProp(aObject, aPropInfo, xStrValue);//TODO: maybe some kind of check as well?
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString{$ENDIF}:
        SetStrProp(aObject, aPropInfo, xStrValue);
      {$IFDEF O_HASBYTESTRINGS}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        SetWideStrProp(aObject, aPropInfo, {$IFDEF FPC}UTF8Decode{$ENDIF}(xStrValue));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        if (xPropType = System.TypeInfo(TDateTime)) then
        begin
          xResult := ISOTryStrToDateTime(xStrValue, TDateTime(xDoubleValue){%H-});
          xExtendedValue := xDoubleValue;
        {$IFNDEF O_KYLIX}
        end else if (xPropType = System.TypeInfo(TTime)) then
        begin
          xResult := ISOTryStrToTime(xStrValue, TDateTime(xDoubleValue){%H-});
          xExtendedValue := xDoubleValue;
        end else if (xPropType = System.TypeInfo(TDate)) then
        begin
          xResult := ISOTryStrToDate(xStrValue, TDateTime(xDoubleValue){%H-});
          xExtendedValue := xDoubleValue;
        {$ENDIF}
        end else
          xResult := ISOTryStrToFloat(xStrValue, xExtendedValue{%H-});

        if not xResult then
          case fErrorHandling of
            dehRaiseException: _Raise;
            dehUseDefaultValue: xExtendedValue := 0;
            dehIgnore: Exit;
          end;

        SetFloatProp(aObject, aPropInfo, xExtendedValue)
      end;
      tkInt64:
      begin
        if not TryStrToInt64(xStrValue, xInt64Value) then
          case fErrorHandling of
            dehRaiseException: _Raise;
            dehUseDefaultValue: xInt64Value := 0;
            dehIgnore: Exit;
          end;
        SetInt64Prop(aObject, aPropInfo, xInt64Value);
      end;
      tkClass:
        _ReadClass(xPropElement);
    end;
  end;
end;

procedure TXMLDeserializer.ReadStringItems(const aStrings: TStrings;
  const aEnumerationNode: PXMLNode);
var
  xItemNode: PXMLNode;
  xChildNameId: OHashedStringsIndex;
begin
  aStrings.BeginUpdate;
  try
    aStrings.Clear;

    if CollectionStyle = csOXml then
    begin
      xChildNameId := aEnumerationNode.OwnerDocument.IndexOfString('i');
      if xChildNameId < 0 then
        Exit;
    end else
      xChildNameId := Low(xChildNameId);

    xItemNode := aEnumerationNode.FirstChild;
    while Assigned(xItemNode) do
    begin
      case CollectionStyle of
        csOXml:
          if (xItemNode.NodeNameId = xChildNameId) then//OXml style: <i>string</i>
            aStrings.Add(xItemNode.Text);
        csOmniXML:
          if (xItemNode.NodeName = 'l'+IntToStr(aStrings.Count)) then//OmniXml style: <l???>string</l???> (??? is counter)
            aStrings.Add(xItemNode.Text);
      end;

      xItemNode := xItemNode.NextSibling;
    end;
  finally
    aStrings.EndUpdate;
  end;
end;

procedure TXMLDeserializer.ReleaseDocument;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

procedure TXMLDeserializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fXMLParser.ApproxStreamPosition > 0 then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeUseRootDataRead)
  else
    inherited SetUseRoot(aUseRoot);
end;

end.
