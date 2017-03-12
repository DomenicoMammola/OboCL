unit OXmlSAX;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlSAX.pas

  SAX implementation.

  Event-based XML parser.
    -> Events in FPC or older Delphi versions.
    -> Events + anonymous methods in D2009+.
    -> Use the PauseParsing procedure to pause the parsing.
       Parsing can be continued by calling TSAXParser.ContinueParsing again.

  External class handler support: TSAXHandler
    -> Assign a Handler to TSAXParser and you will be able to catch SAX events
       with an external object.
    -> TSAXHandler can be nested! Use AddChildHandler to handle children elements.

  See /unittest/OXmlUnitTests.pas:
      TOXmlUnitTest.Test_TSAXHandler_Test1
    for example code.
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
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Collections,
    {$ELSE}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp, OXmlUtils, OTextReadWrite, OXmlReadWrite, OEncoding, OHashedStrings;

type
  TSAXParser = class;

  //The clue of TSAXAttribute is to reduce string operations to an minimum.
  // -> therefore TSAXAttribute just uses TXMLReaderToken.TokenName and .TokenValue.
  //    You can explicitely cast TXMLReaderToken to TSAXAttribute and back!
  TSAXAttribute = {$IFDEF O_PACKED}packed{$ENDIF} {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fToken: TXMLReaderToken;
  public
    property NodeName: OWideString read fToken.TokenName;
    property NodeValue: OWideString read fToken.TokenValue;
  end;

  TSAXAttributes = class;
  PSAXAttribute = ^TSAXAttribute;

  TSAXAttributeEnumerator = {$IFDEF O_PACKED}packed{$ENDIF} {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fIndex: OHashedStringsIndex;
    fSAXAttributes: TSAXAttributes;
  public
    procedure Init(const aSAXAttributes: TSAXAttributes);
    function GetCurrent: PSAXAttribute;
    function MoveNext: Boolean;
  public
    property Current: PSAXAttribute read GetCurrent;
  end;

  TSAXAttributes = class(TObject)
  private
    fAttributeTokens: TXMLReaderTokenList;
    fIndex: TOVirtualHashedStrings;//for fast Find/IndexOf function -> use only for more than attribute limit
    fIndexUsed: Boolean;

    fIteratorCurrent: Integer;//for fast Next & Prev

    function GetPrevNext(var ioAttrEnum: PSAXAttribute; const aInc: Integer): Boolean;
    function GetAttributeItem(const aIndex: Integer): PSAXAttribute;
    function GetCount: Integer;

    function GetKeyByIndex(const aIndex: OStringIndex): OWideString;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure CreateIndex;
  public
    function IndexOf(const aAttrName: OWideString): Integer;
    function Has(const aAttrName: OWideString): Boolean;
    //find attribute
    function Find(const aAttrName: OWideString; var outAttrValue: OWideString): Boolean;
    //get attribute
    function Get(const aAttrName: OWideString): OWideString;
    //get attribute, if attr does not exist, return aDefaultValue
    function GetDef(const aAttrName, aDefaultValue: OWideString): OWideString;

    function First: PSAXAttribute;
    function Last: PSAXAttribute;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNext(var ioAttrEnum: PSAXAttribute): Boolean;
    function GetPrevious(var ioAttrEnum: PSAXAttribute): Boolean;

    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: PSAXAttribute read GetAttributeItem; default;

    function GetEnumerator: TSAXAttributeEnumerator;
  end;

  {$IFDEF O_ANONYMOUS_METHODS}
  TSAXNotifyEvent = reference to procedure(aSaxParser: TSAXParser);
  TSAXTextEvent = reference to procedure(aSaxParser: TSAXParser;
    const aText: OWideString);
  TSAXStartElementEvent = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString; const aAttributes: TSAXAttributes);
  TSAXEndElementEvent = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString);
  TSAXXMLDeclarationEvent = reference to procedure(aSaxParser: TSAXParser;
    const aAttributes: TSAXAttributes);
  TSAXProcessingInstructionEvent = reference to procedure(aSaxParser: TSAXParser;
    const aTarget, aContent: OWideString);
  {$ELSE}
  TSAXNotifyEvent = procedure(Sender: TSAXParser) of object;
  TSAXTextEvent = procedure(Sender: TSAXParser; const aText: OWideString) of object;
  TSAXStartElementEvent = procedure(Sender: TSAXParser; const aName: OWideString;
    const aAttributes: TSAXAttributes) of object;
  TSAXEndElementEvent = procedure(Sender: TSAXParser; const aName: OWideString) of object;
  TSAXXMLDeclarationEvent = procedure(aSaxParser: TSAXParser;
    const aAttributes: TSAXAttributes) of object;
  TSAXProcessingInstructionEvent = procedure(Sender: TSAXParser; const aTarget, aContent: OWideString) of object;
  {$ENDIF}

  TSAXHandler = class(TObject)
  private
    fChildHandlers: TOHashedStringObjDictionary;
    fActiveChildHandler: TSAXHandler;
    fThisElementStartLevel: Integer;
    fThisElementName: OWideString;

    fOnStartDocument: TSAXNotifyEvent;
    fOnEndDocument: TSAXNotifyEvent;
    fOnXMLDeclaration: TSAXXMLDeclarationEvent;

    fOnStartThisElement: TSAXStartElementEvent;
    fOnEndThisElement: TSAXEndElementEvent;
    fOnThisCharacters: TSAXTextEvent;
    fOnThisComment: TSAXTextEvent;
    fOnThisProcessingInstruction: TSAXProcessingInstructionEvent;

    fOnStartOtherElement: TSAXStartElementEvent;
    fOnEndOtherElement: TSAXEndElementEvent;
    fOnOtherCharacters: TSAXTextEvent;
    fOnOtherComment: TSAXTextEvent;
    fOnOtherProcessingInstruction: TSAXProcessingInstructionEvent;
  private
    procedure _DoOnStartDocument(Sender: TSAXParser);
    procedure _DoOnEndDocument(Sender: TSAXParser);
    procedure _DoOnXMLDeclaration(Sender: TSAXParser; const aAttributes: TSAXAttributes);
    procedure _DoOnCharacters(Sender: TSAXParser; const aText: OWideString);
    procedure _DoOnComment(Sender: TSAXParser; const aText: OWideString);
    procedure _DoOnProcessingInstruction(Sender: TSAXParser; const aTarget, aContent: OWideString);
    procedure _DoOnStartElement(Sender: TSAXParser; const aName: OWideString;
      const aAttributes: TSAXAttributes);
    procedure _DoOnEndElement(Sender: TSAXParser; const aName: OWideString);
  protected
    procedure DoOnStartDocument(Sender: TSAXParser); virtual;
    procedure DoOnEndDocument(Sender: TSAXParser); virtual;
    procedure DoOnXMLDeclaration(Sender: TSAXParser; const aAttributes: TSAXAttributes); virtual;

    procedure DoOnStartThisElement(Sender: TSAXParser; const aName: OWideString;
      const aAttributes: TSAXAttributes); virtual;
    procedure DoOnEndThisElement(Sender: TSAXParser; const aName: OWideString); virtual;
    procedure DoOnThisCharacters(Sender: TSAXParser; const aText: OWideString); virtual;
    procedure DoOnThisComment(Sender: TSAXParser; const aText: OWideString); virtual;
    procedure DoOnThisProcessingInstruction(Sender: TSAXParser; const aTarget, aContent: OWideString); virtual;

    procedure DoOnStartOtherElement(Sender: TSAXParser; const aName: OWideString;
      const aAttributes: TSAXAttributes); virtual;
    procedure DoOnEndOtherElement(Sender: TSAXParser; const aName: OWideString); virtual;
    procedure DoOnOtherCharacters(Sender: TSAXParser; const aText: OWideString); virtual;
    procedure DoOnOtherComment(Sender: TSAXParser; const aText: OWideString); virtual;
    procedure DoOnOtherProcessingInstruction(Sender: TSAXParser; const aTarget, aContent: OWideString); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    //The beginning of a document. The SAX parser will invoke this method only once, before any other event callbacks.
    property OnStartDocument: TSAXNotifyEvent read fOnStartDocument write fOnStartDocument;
    //reached the end of the document
    property OnEndDocument: TSAXNotifyEvent read fOnEndDocument write fOnEndDocument;
    //XML Declaration <?xml ... ?>
    property OnXMLDeclaration: TSAXXMLDeclarationEvent read fOnXMLDeclaration write fOnXMLDeclaration;


    //start of element assigned with this handler
    property OnStartThisElement: TSAXStartElementEvent read fOnStartThisElement write fOnStartThisElement;
    //end of element assigned with this handler </a> or <a />
    property OnEndThisElement: TSAXEndElementEvent read fOnEndThisElement write fOnEndThisElement;
    //text or CData within element assigned with this handler
    property OnThisCharacters: TSAXTextEvent read fOnThisCharacters write fOnThisCharacters;
    //comment within element assigned with this handler
    property OnThisComment: TSAXTextEvent read fOnThisComment write fOnThisComment;
    //Processing Instruction <?php ... ?> within element assigned with this handler
    property OnThisProcessingInstruction: TSAXProcessingInstructionEvent read fOnThisProcessingInstruction write fOnThisProcessingInstruction;

    //start of a (child) element not assigned with any handler - if the target element is assigned with a handler, OnStartThisElement of that handler is called instead
    property OnStartOtherElement: TSAXStartElementEvent read fOnStartOtherElement write fOnStartOtherElement;
    //end of a (child) element not assigned with any handler - if the target element is assigned with a handler, OnEndThisElement of that handler is called instead
    property OnEndOtherElement: TSAXEndElementEvent read fOnEndOtherElement write fOnEndOtherElement;
    //text or CData within a (child) element not assigned with this handler
    property OnOtherCharacters: TSAXTextEvent read fOnOtherCharacters write fOnOtherCharacters;
    //comment within a (child) element not assigned with this handler
    property OnOtherComment: TSAXTextEvent read fOnOtherComment write fOnOtherComment;
    //Processing Instruction <?php ... ?> within a (child) element not assigned with this handler
    property OnOtherProcessingInstruction: TSAXProcessingInstructionEvent read fOnOtherProcessingInstruction write fOnOtherProcessingInstruction;
  public
    //Add handler for child elements, aElementName must be valid (not empty) and it must not be registered
    //  don't free aHandler, it will be destroyed automatically
    procedure AddChildHandler(const aElementName: OWideString; const aHandler: TSAXHandler);
    //Remove ChildHandler assigned with an element name. It will be destroyed automatically.
    //  Return false if not found, otherwise True.
    function RemoveChildHandler(const aElementName: OWideString): Boolean;
    //Get handler for element name, return nil if not found
    function FindChildHandler(const aElementName: OWideString): TSAXHandler;
  end;

  TSAXParserState = (spsIdle, spsRunning, spsPaused, spsStopping, spsStopped);

  TSAXParser = class(TObject)
  private
    fURL: OWideString;

    fReader: TXMLReader;
    fDataRead: Boolean;
    fParserState: TSAXParserState;
    fAttributes: TSAXAttributes;
    fHandler: TSAXHandler;

    fOnStartDocument: TSAXNotifyEvent;
    fOnEndDocument: TSAXNotifyEvent;
    fOnCharacters: TSAXTextEvent;
    fOnComment: TSAXTextEvent;
    fOnXMLDeclaration: TSAXXMLDeclarationEvent;
    fOnProcessingInstruction: TSAXProcessingInstructionEvent;
    fOnStartElement: TSAXStartElementEvent;
    fOnEndElement: TSAXEndElementEvent;

    fParseError: IOTextParseError;
    fWhiteSpaceHandling: TXMLWhiteSpaceHandling;
    fPreserveWhiteSpaceTree: array of TXMLPreserveWhiteSpace;

  private
    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
    function GetApproxStreamPosition: OStreamInt;
    function GetParsingStopped: Boolean;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
    procedure SetHandler(const aHandler: TSAXHandler);
    procedure SetWhiteSpaceHandling(
      const aWhiteSpaceHandling: TXMLWhiteSpaceHandling);

    function GetDoPreserveWhiteSpace: Boolean;
  protected
    procedure DoOnStartDocument; virtual;
    procedure DoOnEndDocument; virtual;
    procedure DoOnCharacters(const aText: OWideString); virtual;
    procedure DoOnComment(const aText: OWideString); virtual;
    procedure DoOnXMLDeclaration(const aAttributes: TSAXAttributes); virtual;
    procedure DoOnProcessingInstruction(const aTarget, aContent: OWideString); virtual;
    procedure DoOnStartElement(const aName: OWideString;
      const aAttributes: TSAXAttributes); virtual;
    procedure DoOnEndElement(const aName: OWideString); virtual;

  protected
    function StartParsing: Boolean;
    procedure FinishedParsing;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //The Parse* functions open an XML document and start parsing it
    //  they return "True" if the document was sucessfully parsed to the end
    //  (they return "False" if the parsing has been stopped with aStop parameter)

    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil): Boolean;
    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //parse XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function ParseXML(const aXML: OWideString): Boolean;
    function ParseXML_UTF8(const aXML: OUTF8Container): Boolean;
    //parse document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean; overload;
    function ParseBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil): Boolean; overload;
  public
    //call PauseParsing from an event or anonymous method to pause parsing
    procedure PauseParsing;
    //parsing can be continued with ResumeParsing when paused
    function ResumeParsing: Boolean;
    //call StopParsing from an event or anonymous method to stop parsing
    //  When stopped, parsing cannot be continued again.
    procedure StopParsing;
  public
    //The beginning of a document. The SAX parser will invoke this method only once, before any other event callbacks.
    property OnStartDocument: TSAXNotifyEvent read fOnStartDocument write fOnStartDocument;
    //reached the end of the document
    property OnEndDocument: TSAXNotifyEvent read fOnEndDocument write fOnEndDocument;
    //text or CData
    property OnCharacters: TSAXTextEvent read fOnCharacters write fOnCharacters;
    //comment
    property OnComment: TSAXTextEvent read fOnComment write fOnComment;
    //XML Declaration <?xml ... ?>
    property OnXMLDeclaration: TSAXXMLDeclarationEvent read fOnXMLDeclaration write fOnXMLDeclaration;
    //Processing Instruction <?php ... ?>
    property OnProcessingInstruction: TSAXProcessingInstructionEvent read fOnProcessingInstruction write fOnProcessingInstruction;
    //start of an element <a href="title">
    property OnStartElement: TSAXStartElementEvent read fOnStartElement write fOnStartElement;
    //end of an element </a> or <a />
    property OnEndElement: TSAXEndElementEvent read fOnEndElement write fOnEndElement;

    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //document whitespace handling
    property WhiteSpaceHandling: TXMLWhiteSpaceHandling read fWhiteSpaceHandling write SetWhiteSpaceHandling;
  public
    //following functions and properties can be called only from events or anonymous methods during parsing

    //functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    property ParserState: TSAXParserState read fParserState write fParserState;
    //determines if parsing has been stopped with the StopParsing procedure
    property ParsingStopped: Boolean read GetParsingStopped;

    //Handler callback object
    property Handler: TSAXHandler read fHandler write SetHandler;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read fParseError;
  end;

  ESAXParserException = class(Exception);
  ESAXHandlerException = class(Exception);
  ESAXHandlerInputException = class(ESAXHandlerException);

implementation

uses
  OXmlLng;

{ TSAXHandler }

procedure TSAXHandler.AddChildHandler(const aElementName: OWideString;
  const aHandler: TSAXHandler);
var
  xNew: Boolean;
  xIndex: OHashedStringsIndex;
begin
  if not OXmlValidName(aElementName) then
    raise ESAXHandlerInputException.CreateFmt(OXmlLng_InvalidElementName, [aElementName]);

  xIndex := fChildHandlers.Add(aElementName, xNew{%H-});
  if not xNew then
    raise ESAXHandlerInputException.CreateFmt(OXmlLng_ElementNameAlreadyRegistered, [aElementName]);

  fChildHandlers.Objects[xIndex] := aHandler;
  {%H-}aHandler.fThisElementName := aElementName;
end;

constructor TSAXHandler.Create;
begin
  inherited Create;

  fChildHandlers := TOHashedStringObjDictionary.Create;
  fChildHandlers.OwnsObjects := True;

  fThisElementStartLevel := -1;
end;

destructor TSAXHandler.Destroy;
begin
  fChildHandlers.Free;

  inherited Destroy;
end;

procedure TSAXHandler.DoOnThisCharacters(Sender: TSAXParser;
  const aText: OWideString);
begin
  if Assigned(fOnThisCharacters) then
    fOnThisCharacters(Sender, aText);
end;

procedure TSAXHandler.DoOnThisComment(Sender: TSAXParser;
  const aText: OWideString);
begin
  if Assigned(fOnThisComment) then
    fOnThisComment(Sender, aText);
end;

procedure TSAXHandler.DoOnThisProcessingInstruction(Sender: TSAXParser;
  const aTarget, aContent: OWideString);
begin
  if Assigned(fOnThisProcessingInstruction) then
    fOnThisProcessingInstruction(Sender, aTarget, aContent);
end;

procedure TSAXHandler.DoOnEndDocument(Sender: TSAXParser);
begin
  if Assigned(fOnEndDocument) then
    fOnEndDocument(Sender);
end;

procedure TSAXHandler.DoOnEndOtherElement(Sender: TSAXParser;
  const aName: OWideString);
begin
  if Assigned(fOnEndOtherElement) then
    fOnEndOtherElement(Sender, aName);
end;

procedure TSAXHandler.DoOnEndThisElement(Sender: TSAXParser;
  const aName: OWideString);
begin
  if Assigned(fOnEndThisElement) then
    fOnEndThisElement(Sender, aName);
end;

procedure TSAXHandler.DoOnOtherCharacters(Sender: TSAXParser;
  const aText: OWideString);
begin
  if Assigned(fOnOtherCharacters) then
    fOnOtherCharacters(Sender, aText);
end;

procedure TSAXHandler.DoOnOtherComment(Sender: TSAXParser;
  const aText: OWideString);
begin
  if Assigned(fOnOtherComment) then
    fOnOtherComment(Sender, aText);
end;

procedure TSAXHandler.DoOnOtherProcessingInstruction(Sender: TSAXParser;
  const aTarget, aContent: OWideString);
begin
  if Assigned(fOnOtherProcessingInstruction) then
    fOnOtherProcessingInstruction(Sender, aTarget, aContent);
end;

procedure TSAXHandler.DoOnStartDocument(Sender: TSAXParser);
begin
  if Assigned(fOnStartDocument) then
    fOnStartDocument(Sender);
end;

procedure TSAXHandler.DoOnStartOtherElement(Sender: TSAXParser;
  const aName: OWideString; const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnStartOtherElement) then
    fOnStartOtherElement(Sender, aName, aAttributes);
end;

procedure TSAXHandler.DoOnStartThisElement(Sender: TSAXParser;
  const aName: OWideString; const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnStartThisElement) then
    fOnStartThisElement(Sender, aName, aAttributes);
end;

procedure TSAXHandler.DoOnXMLDeclaration(Sender: TSAXParser;
  const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnXMLDeclaration) then
    fOnXMLDeclaration(Sender, aAttributes);
end;

function TSAXHandler.FindChildHandler(const aElementName: OWideString
  ): TSAXHandler;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := fChildHandlers.IndexOf(aElementName);
  if xIndex >= 0 then
    Result := TSAXHandler(fChildHandlers.Objects[xIndex])
  else
    Result := nil;
end;

function TSAXHandler.RemoveChildHandler(const aElementName: OWideString
  ): Boolean;
begin
  Result := fChildHandlers.Delete(aElementName);
end;

procedure TSAXHandler._DoOnCharacters(Sender: TSAXParser;
  const aText: OWideString);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnCharacters(Sender, aText)
  else
  begin
    if Sender.NodePathCount = fThisElementStartLevel then
      DoOnThisCharacters(Sender, aText)
    else
      DoOnOtherCharacters(Sender, aText);
  end;
end;

procedure TSAXHandler._DoOnComment(Sender: TSAXParser; const aText: OWideString
  );
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnComment(Sender, aText)
  else
  begin
    if Sender.NodePathCount = fThisElementStartLevel then
      DoOnThisComment(Sender, aText)
    else
      DoOnOtherComment(Sender, aText);
  end;
end;

procedure TSAXHandler._DoOnEndDocument(Sender: TSAXParser);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnEndDocument(Sender)
  else
    DoOnEndDocument(Sender);
end;

procedure TSAXHandler._DoOnEndElement(Sender: TSAXParser;
  const aName: OWideString);
begin
  if Assigned(fActiveChildHandler) then
  begin
    fActiveChildHandler._DoOnEndElement(Sender, aName);
    if fActiveChildHandler.fThisElementStartLevel = -1 then//fActiveChildHandler was closed
      fActiveChildHandler := nil;
  end else
  begin
    if (fThisElementStartLevel = Sender.GetNodePathCount+1) and (fThisElementName = aName) then
    begin
      fThisElementStartLevel := -1;
      DoOnEndThisElement(Sender, aName);
    end else
      DoOnEndOtherElement(Sender, aName);
  end;
end;

procedure TSAXHandler._DoOnProcessingInstruction(Sender: TSAXParser;
  const aTarget, aContent: OWideString);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnProcessingInstruction(Sender, aTarget, aContent)
  else
  begin
    if Sender.NodePathCount = fThisElementStartLevel then
      DoOnThisProcessingInstruction(Sender, aTarget, aContent)
    else
      DoOnOtherProcessingInstruction(Sender, aTarget, aContent);
  end;
end;

procedure TSAXHandler._DoOnStartDocument(Sender: TSAXParser);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnStartDocument(Sender)
  else
    DoOnStartDocument(Sender);
end;

procedure TSAXHandler._DoOnStartElement(Sender: TSAXParser;
  const aName: OWideString; const aAttributes: TSAXAttributes);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnStartElement(Sender, aName, aAttributes)
  else
  begin
    fActiveChildHandler := FindChildHandler(aName);
    if Assigned(fActiveChildHandler) then
    begin
      fActiveChildHandler.fThisElementStartLevel := Sender.GetNodePathCount;
      fActiveChildHandler.DoOnStartThisElement(Sender, aName, aAttributes)
    end else
      DoOnStartOtherElement(Sender, aName, aAttributes);
  end;
end;

procedure TSAXHandler._DoOnXMLDeclaration(Sender: TSAXParser;
  const aAttributes: TSAXAttributes);
begin
  if Assigned(fActiveChildHandler) then
    fActiveChildHandler._DoOnXMLDeclaration(Sender, aAttributes)
  else
    DoOnXMLDeclaration(Sender, aAttributes);
end;

{ TSAXParser }

constructor TSAXParser.Create;
begin
  inherited Create;

  fReader := TXMLReader.Create;
end;

destructor TSAXParser.Destroy;
begin
  fReader.Free;
  fAttributes.Free;

  inherited;
end;

procedure TSAXParser.DoOnCharacters(const aText: OWideString);
begin
  if Assigned(fOnCharacters) then
    fOnCharacters(Self, aText);
  if Assigned(fHandler) then
    fHandler._DoOnCharacters(Self, aText);
end;

procedure TSAXParser.DoOnComment(const aText: OWideString);
begin
  if Assigned(fOnComment) then
    fOnComment(Self, aText);
  if Assigned(fHandler) then
    fHandler._DoOnComment(Self, aText);
end;

procedure TSAXParser.DoOnEndDocument;
begin
  if Assigned(fOnEndDocument) then
    fOnEndDocument(Self);
  if Assigned(fHandler) then
    fHandler._DoOnEndDocument(Self);
end;

procedure TSAXParser.DoOnEndElement(const aName: OWideString);
begin
  if Assigned(fOnEndElement) then
    fOnEndElement(Self, aName);
  if Assigned(fHandler) then
    fHandler._DoOnEndElement(Self, aName);
end;

procedure TSAXParser.DoOnProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if Assigned(fOnProcessingInstruction) then
    fOnProcessingInstruction(Self, aTarget, aContent);
  if Assigned(fHandler) then
    fHandler._DoOnProcessingInstruction(Self, aTarget, aContent);
end;

procedure TSAXParser.DoOnStartDocument;
begin
  if Assigned(fOnStartDocument) then
    fOnStartDocument(Self);
  if Assigned(fHandler) then
    fHandler._DoOnStartDocument(Self);
end;

procedure TSAXParser.DoOnStartElement(const aName: OWideString;
  const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnStartElement) then
    fOnStartElement(Self, aName, aAttributes);
  if Assigned(fHandler) then
    fHandler._DoOnStartElement(Self, aName, aAttributes);
end;

procedure TSAXParser.DoOnXMLDeclaration(const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnXMLDeclaration) then
    fOnXMLDeclaration(Self, aAttributes);
  if Assigned(fHandler) then
    fHandler._DoOnXMLDeclaration(Self, aAttributes);
end;

procedure TSAXParser.FinishedParsing;
begin
  if not (fParserState in [spsIdle, spsStopping]) then
  begin
    fReader.SetAttributeTokens(nil);
    FreeAndNil(fAttributes);
    fReader.ReleaseDocument;
    fURL := '';
    if fParserState = spsStopping then
      fParserState := spsStopped
    else
      fParserState := spsIdle;
  end;
end;

function TSAXParser.GetApproxStreamPosition: OStreamInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TSAXParser.GetDoPreserveWhiteSpace: Boolean;
var
  I: Integer;
begin
  for I := fReader.NodePathCount-1 downto 0 do
  begin
    case fPreserveWhiteSpaceTree[I] of
      pwPreserve: begin
        Result := True;
        Exit;
      end;
      pwDefault: begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TSAXParser.GetNodePath(const aIndex: Integer): OWideString;
begin
  Result := fReader.NodePath[aIndex];
end;

function TSAXParser.GetNodePathCount: Integer;
begin
  Result := fReader.NodePathCount;
end;

function TSAXParser.GetParsingStopped: Boolean;
begin
  Result := fParserState in [spsStopped, spsStopping];
end;

function TSAXParser.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fReader.ReaderSettings;
end;

function TSAXParser.GetStreamSize: OStreamInt;
begin
  Result := fReader.StreamSize;
end;

function TSAXParser.ParseBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetBuffer(aBuffer);

    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetPointer(@aBuffer, aBufferLength);

    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TOFileStream;
begin
  fURL := aFileName;

  xStream := TOFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  try
    fReader.InitStream(aStream, aForceEncoding);
    fReader.URL := fURL;

    Result := StartParsing;
  finally
    fReader.ReleaseDocument;
  end;
end;

function TSAXParser.ParseXML(const aXML: OWideString): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString(aXML);

    Result := ParseStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseXML_UTF8(const aXML: OUTF8Container): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString_UTF8(aXML);

    Result := ParseStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;

procedure TSAXParser.PauseParsing;
begin
  if fParserState = spsRunning then
    fParserState := spsPaused;
end;

procedure TSAXParser.NodePathAssignTo(const aNodePath: TOWideStringList);
begin
  fReader.NodePathAssignTo(aNodePath);
end;

function TSAXParser.NodePathAsString: OWideString;
begin
  Result := fReader.NodePathAsString;
end;

function TSAXParser.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.StartParsing: Boolean;
begin
  fParseError := nil;
  fDataRead := False;
  fParserState := spsPaused;

  fAttributes.Free;
  fAttributes := TSAXAttributes.Create;
  fReader.SetAttributeTokens(fAttributes.fAttributeTokens);

  Result := ResumeParsing;
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

function TSAXParser.ResumeParsing: Boolean;
  procedure _StartDocument;
  begin
    DoOnStartDocument;
    fDataRead := True;
  end;
var
  xReaderToken: PXMLReaderToken;
  xValue: OWideString;
begin
  if fParserState <> spsPaused then
    raise ESAXParserException.Create(OXmlLng_CannotResumeNotPaused);

  fParserState := spsRunning;
  Result := True;
  try
    while (fParserState = spsRunning) and fReader.ReadNextToken(xReaderToken{%H-}) do
    begin
      case xReaderToken.TokenType of
        rtText, rtCData, rtEntityReference:
          if fDataRead or not OXmlIsWhiteSpace(xReaderToken.TokenValue)
          then//omit empty text before root node
          begin
            if not fDataRead then
              _StartDocument;

            xValue := xReaderToken.TokenValue;
            if (fWhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(xValue)
            then begin
              xValue := '';
            end else if
              (fWhiteSpaceHandling = wsTrim) or
              ((fWhiteSpaceHandling = wsAutoTag) and not GetDoPreserveWhiteSpace)
            then begin
              xValue := OTrim(xValue);
            end;

            if xValue <> '' then
              DoOnCharacters(xValue);
          end;
      else//case
        if not fDataRead then
          _StartDocument;
        case xReaderToken.TokenType of
          rtFinishOpenElementClose, rtFinishOpenElement:
          begin
            fAttributes.CreateIndex;
            if (fWhiteSpaceHandling = wsAutoTag) then
            begin
              if (fReader.NodePathCount-1) > High(fPreserveWhiteSpaceTree) then
                SetLength(fPreserveWhiteSpaceTree, Length(fPreserveWhiteSpaceTree)+20);

              if fAttributes.Find(XML_XML_SPACE, xValue{%H-}) then
                fPreserveWhiteSpaceTree[fReader.NodePathCount-1] := OXmlStrToPreserve(xValue)
              else
                fPreserveWhiteSpaceTree[fReader.NodePathCount-1] := pwInherit;
            end;
            DoOnStartElement(xReaderToken.TokenName, fAttributes);
            if xReaderToken.TokenType = rtFinishOpenElementClose then
              DoOnEndElement(xReaderToken.TokenName);
          end;
          rtFinishXMLDeclarationClose:
          begin
            fAttributes.CreateIndex;
            DoOnXMLDeclaration(fAttributes);
          end;
          rtCloseElement: DoOnEndElement(xReaderToken.TokenName);
          rtComment: DoOnComment(xReaderToken.TokenValue);
          rtProcessingInstruction: DoOnProcessingInstruction(xReaderToken.TokenName, xReaderToken.TokenValue);
        end;
      end;
    end;

    if fDataRead and (fParserState = spsRunning) and not Assigned(fReader.ParseError) then
      DoOnEndDocument;
  finally
    FinishedParsing;
    if Assigned(fReader.ParseError) then
    begin
      fParseError := fReader.ParseError;

      Result := False;
      FinishedParsing;
    end;
  end;
end;

procedure TSAXParser.SetHandler(const aHandler: TSAXHandler);
begin
  if fHandler = aHandler then
    Exit;

  fHandler := aHandler;
end;

procedure TSAXParser.SetWhiteSpaceHandling(
  const aWhiteSpaceHandling: TXMLWhiteSpaceHandling);
begin
  if fDataRead then
    raise ESAXParserException.Create(OXmlLng_CannotChangeWhiteSpaceHandlingDataRead);

  if fWhiteSpaceHandling = aWhiteSpaceHandling then Exit;
  fWhiteSpaceHandling := aWhiteSpaceHandling;
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

procedure TSAXParser.StopParsing;
begin
  if fParserState in [spsRunning, spsPaused] then
    fParserState := spsStopping;
end;

{ TSAXAttributes }

constructor TSAXAttributes.Create;
begin
  inherited;

  fAttributeTokens := TXMLReaderTokenList.Create;
  fIndex := TOVirtualHashedStrings.Create(GetKeyByIndex);
end;

procedure TSAXAttributes.CreateIndex;
var
  I: Integer;
begin
  fIndexUsed := Count > XMLUseIndexNodeLimit;
  if fIndexUsed then
  begin
    fIndex.Clear(False);
    for I := 0 to fAttributeTokens.Count-1 do
      fIndex.Add(I);
  end;
end;

destructor TSAXAttributes.Destroy;
begin
  fAttributeTokens.Free;
  fIndex.Free;

  inherited;
end;

function TSAXAttributes.Find(const aAttrName: OWideString;
  var outAttrValue: OWideString): Boolean;
var
  I: Integer;
begin
  I := IndexOf(aAttrName);
  Result := I >= 0;
  if Result then
    outAttrValue := fAttributeTokens[I].TokenValue
  else
    outAttrValue := '';
end;

function TSAXAttributes.Has(const aAttrName: OWideString): Boolean;
begin
  Result := (IndexOf(aAttrName) >= 0);
end;

function TSAXAttributes.IndexOf(const aAttrName: OWideString): Integer;
var
  I: Integer;
begin
  if fIndexUsed then//hash index used
    Result := fIndex.StringIndexOf(aAttrName)
  else
  begin//hash index not used
    for I := 0 to Count-1 do
    if fAttributeTokens[I].TokenName = aAttrName then
    begin
      Result := I;
      Exit;
    end;
    Result := -1;
  end;
end;

function TSAXAttributes.First: PSAXAttribute;
begin
  if fAttributeTokens.Count >= 0 then
    Result := PSAXAttribute(fAttributeTokens[0])
  else
    Result := nil;
end;

function TSAXAttributes.GetAttributeItem(const aIndex: Integer): PSAXAttribute;
begin
  Result := PSAXAttribute(fAttributeTokens[aIndex]);
end;

function TSAXAttributes.Get(const aAttrName: OWideString): OWideString;
begin
  Find(aAttrName, Result{%H-});
end;

function TSAXAttributes.GetCount: Integer;
begin
  Result := fAttributeTokens.Count;
end;

function TSAXAttributes.GetDef(const aAttrName,
  aDefaultValue: OWideString): OWideString;
var
  I: Integer;
begin
  I := IndexOf(aAttrName);
  if I >= 0 then
    Result := fAttributeTokens[I].TokenValue
  else
    Result := aDefaultValue;
end;

function TSAXAttributes.GetNext(
  var ioAttrEnum: PSAXAttribute): Boolean;
begin
  Result := GetPrevNext(ioAttrEnum, +1);
end;

function TSAXAttributes.GetPrevious(var ioAttrEnum: PSAXAttribute): Boolean;
begin
  Result := GetPrevNext(ioAttrEnum, -1);
end;

function TSAXAttributes.GetPrevNext(var ioAttrEnum: PSAXAttribute;
  const aInc: Integer): Boolean;
var
  xCount: Integer;
begin
  //same code as TXMLResNodeList.GetPrevNext
  Result := False;
  xCount := Count;
  if xCount = 0 then
  begin
    ioAttrEnum := nil;
    Exit;
  end;

  if Assigned(ioAttrEnum) then
  begin
    //get prev/next
    if not(
       (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) and
       (PSAXAttribute(fAttributeTokens[fIteratorCurrent]) = ioAttrEnum))
    then//ioAttrEnum is NOT the last iterator -> we have to find it
      fIteratorCurrent := fAttributeTokens.IndexOf(PXMLReaderToken(ioAttrEnum));

    if (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) then
    begin
      fIteratorCurrent := fIteratorCurrent + aInc;
      Result := (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount);
      if Result then
        ioAttrEnum := PSAXAttribute(fAttributeTokens[fIteratorCurrent])
      else
        ioAttrEnum := nil;
    end;
  end else
  begin
    //return first or last element
    if aInc > 0 then
      fIteratorCurrent := 0
    else
      fIteratorCurrent := xCount-1;
    ioAttrEnum := PSAXAttribute(fAttributeTokens[fIteratorCurrent]);
    Result := True;
  end;
end;

function TSAXAttributes.GetKeyByIndex(const aIndex: OStringIndex): OWideString;
begin
  Result := fAttributeTokens[aIndex].TokenName;
end;

function TSAXAttributes.Last: PSAXAttribute;
begin
  if fAttributeTokens.Count >= 0 then
    Result := PSAXAttribute(fAttributeTokens[fAttributeTokens.Count-1])
  else
    Result := nil;
end;

function TSAXAttributes.GetEnumerator: TSAXAttributeEnumerator;
begin
  Result.Init(Self);
end;

{ TSAXAttributeEnumerator }

procedure TSAXAttributeEnumerator.Init(const aSAXAttributes: TSAXAttributes);
begin
  fIndex := -1;
  fSAXAttributes := aSAXAttributes;
end;

function TSAXAttributeEnumerator.GetCurrent: PSAXAttribute;
begin
  Result := fSAXAttributes[fIndex];
end;

function TSAXAttributeEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex < fSAXAttributes.Count - 1);
  if Result then
    Inc(fIndex);
end;

end.
