unit OJsonUtils;
{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OJsonUtils.pas

  !!! Beta version - everything can change here !!!

  Abstract classes and definitions that are used in OJsonReadWrite.pas and
    OJsonUtf8ReadWrite.pas.

  See /unittest/OXmlUnitTests.pas:
      TOXmlUnitTest.Test_OJSON_TCustomJSONWriter_Test1
      TOXmlUnitTest.Test_OJSON_TCustomJSONReader_Test1
    for example code.

  TCustomJSONWriter
    - fast sequential JSON writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      and the writer takes care of valid JSON escaping

    - indentation is not supported yet
    - new line handling is not supported yet

  TCustomJSONReader
    - a standalone direct JSON parser.
    - new line handling is not supported yet.
    - source file must be a valid JSON, there is no error handling.
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
  {$ELSE}
  Contnrs,
  {$ENDIF}

  TypInfo,
  OBufferedStreams, OEncoding, OWideSupp, OXmlUtils;

type
  TJSONWriterState = (wsDocument, wsArray, wsObject);
  TCustomJSONWriter = class;
  TCustomJSONWriterClass = class of TCustomJSONWriter;
  TCustomJSONWriter = class(TObject)
  protected
    fStateTree: array of TJSONWriterState;
    fStateTreeCurrent: Integer;
    fStateObjectValueAfterPairName: Boolean;
    fNextPairValueNeedsSeparator: Boolean;

    procedure EnterState(const aState: TJSONWriterState); {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure ExitState; {$IFDEF O_INLINE}inline;{$ENDIF}
    function CheckState(const aState: TJSONWriterState): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure CheckWriteValue;
    procedure CheckWritePair;
  protected
    procedure WriteChar(const aChar: Byte); virtual; abstract;
    procedure WriteText(const aText: OWideString); virtual; abstract;//escaped text
    procedure WriteTextUnicode(const aText: OUnicodeString); virtual; abstract;
    procedure WriteTextUTF8(const aText: OUTF8Container); virtual; abstract;

    procedure WriteChars(const aChar1, aChar2: Byte); {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteString(const aString: string); {$IFDEF O_INLINE}inline;{$ENDIF}//MUST BE ASCII
    procedure Separator; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteObject; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteArray; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteNumber(const aNumber: Integer); overload; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteNumber(const aNumber: Extended); overload; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteBoolean(const aBoolean: Boolean); {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure WriteNull; {$IFDEF O_INLINE}inline;{$ENDIF}
  protected
    procedure DoCreate; virtual;
  public
    //The Init* procedures initialize a document for writing.
    // The document is written in UTF8.
    // Please note that the file/stream/... is locked until you destroy
    // TCustomJSONWriter or call ReleaseDocument!

    procedure InitFile(const aFileName: OWideString); virtual; abstract;
    procedure InitStream(const {%H-}aStream: TStream; const {%H-}aOwnsStream: Boolean = False); virtual; abstract;

    //write the whole temporary buffer to the destination stream (if used)
    procedure EnsureTempBufferWritten; virtual; abstract;
    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument; virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(const aFileName: OWideString); overload; virtual;
    constructor Create(const aStream: TStream; const aOwnsStream: Boolean = False); overload; virtual;
    destructor Destroy; override;
  public
    function OpenArray: TCustomJSONWriter; overload;// "["
    function OpenArray(const aPairName: OWideString): TCustomJSONWriter; overload;
    function OpenArrayUnicode(const aPairName: OUnicodeString): TCustomJSONWriter;
    function OpenArrayUTF8(const aPairName: OUTF8Container): TCustomJSONWriter;
    function CloseArray: TCustomJSONWriter; // "]"

    function OpenObject: TCustomJSONWriter; overload; // "{"
    function OpenObject(const aPairName: OWideString): TCustomJSONWriter; overload;
    function OpenObjectUnicode(const aPairName: OUnicodeString): TCustomJSONWriter;
    function OpenObjectUTF8(const aPairName: OUTF8Container): TCustomJSONWriter;
    function CloseObject: TCustomJSONWriter; // "}"

    procedure PairName(const aString: OWideString);
    procedure PairNameUnicode(const aString: OUnicodeString);
    procedure PairNameUTF8(const aString: OUTF8Container);

    function Text(const aText: OWideString): TCustomJSONWriter; overload;
    function Text(const aPairName, aText: OWideString): TCustomJSONWriter; overload;
    function TextUnicode(const aText: OUnicodeString): TCustomJSONWriter; overload;
    function TextUTF8(const aText: OUTF8Container): TCustomJSONWriter; overload;
    function TextUnicode(const aPairName, aText: OUnicodeString): TCustomJSONWriter; overload;
    function TextUTF8(const aPairName, aText: OUTF8Container): TCustomJSONWriter; overload;

    function Number(const aNumber: Integer): TCustomJSONWriter; overload;
    function Number(const aPairName: OWideString; const aNumber: Integer): TCustomJSONWriter; overload;
    function Number(const aNumber: Extended): TCustomJSONWriter; overload;
    function Number(const aPairName: OWideString; const aNumber: Extended): TCustomJSONWriter; overload;
    function NumberUnicode(const aPairName: OUnicodeString; const aNumber: Integer): TCustomJSONWriter; overload;
    function NumberUTF8(const aPairName: OUTF8Container; const aNumber: Integer): TCustomJSONWriter; overload;
    function NumberUnicode(const aPairName: OUnicodeString; const aNumber: Extended): TCustomJSONWriter; overload;
    function NumberUTF8(const aPairName: OUTF8Container; const aNumber: Extended): TCustomJSONWriter; overload;

    function Boolean(const aBoolean: Boolean): TCustomJSONWriter; overload;
    function Boolean(const aPairName: OWideString; const aBoolean: Boolean): TCustomJSONWriter; overload;
    function BooleanUnicode(const aPairName: OUnicodeString; const aBoolean: Boolean): TCustomJSONWriter;
    function BooleanUTF8(const aPairName: OUTF8Container; const aBoolean: Boolean): TCustomJSONWriter;

    function Null: TCustomJSONWriter; overload;
    function Null(const aPairName: OWideString): TCustomJSONWriter; overload;
    function NullUnicode(const aPairName: OUnicodeString): TCustomJSONWriter;
    function NullUTF8(const aPairName: OUTF8Container): TCustomJSONWriter;
  end;

  TJSONReaderTokenType = (ttOpenObject, ttCloseObject,
    ttOpenArray, ttCloseArray, ttPairName, ttValue, ttSeparator);
  TJSONReaderValueType = (vtString, vtNumber, vtObject, vtArray, vtBoolean, vtNull);

  TRPropList = class(TObject)
  private
    fPropList: PPropList;
    fPropCount: Integer;
  public
    property PropList: PPropList read fPropList;
    property PropCount: Integer read fPropCount;
  public
    constructor Create(const aClass: TClass);
    destructor Destroy; override;
  end;

  TCustomJSONReaderToken = class(TObject)
  protected
    fBooleanValue: Boolean;
    fExtendedValue: Extended;
    fTokenType: TJSONReaderTokenType;
    fValueType: TJSONReaderValueType;

  protected
    function GetPairName: OWideString; virtual; abstract;
    function GetPairNameUTF8: OUTF8Container; virtual; abstract;
    function GetPairNameUnicode: OUnicodeString; virtual; abstract;

    function GetStringValue: OWideString; virtual; abstract;
    function GetStringValueUTF8: OUTF8Container; virtual; abstract;
    function GetStringValueUnicode: OUnicodeString; virtual; abstract;

    function GetBooleanValue: Boolean; virtual;
    function GetDoubleValue: Double; virtual;
    function GetExtendedValue: Extended; virtual;
    function GetIntegerValue: Integer; virtual;
    function GetInt64Value: Int64; virtual;

    function SamePairName(const aShortStringPointer: PByte): Boolean; virtual; abstract;
  public
    property TokenType: TJSONReaderTokenType read fTokenType write fTokenType;
    property ValueType: TJSONReaderValueType read fValueType write fValueType;

    property PairName: OWideString read GetPairName;
    property PairNameUTF8: OUTF8Container read GetPairNameUTF8;
    property PairNameUnicode: OUnicodeString read GetPairNameUnicode;

    property StringValue: OWideString read GetStringValue;
    property StringValueUTF8: OUTF8Container read GetStringValueUTF8;
    property StringValueUnicode: OUnicodeString read GetStringValueUnicode;

    property IntegerValue: Integer read GetIntegerValue;
    property Int64Value: Int64 read GetInt64Value;
    property DoubleValue: Double read GetDoubleValue;
    property ExtendedValue: Extended read GetExtendedValue;
    property BooleanValue: Boolean read GetBooleanValue;
  end;

  TCustomJSONReader = class;
  TCustomJSONReaderClass = class of TCustomJSONReader;
  TMethodCreateAndReadItem = function(const aSender: TCustomJSONReader; const aPropList: TRPropList): TObject of object;

  TCustomJSONReader = class(TObject)
  {$IFDEF O_GENERICS}
  public type
    TObjectList = TObjectList<TObject>;
  {$ENDIF}
  protected
    procedure DoCreate; virtual;
    function GetReaderToken: TCustomJSONReaderToken; virtual; abstract;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;
  public
    //The Init* procedures initialize a JSON document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TCustomJSONReader or you call ReleaseDocument!

    //init document from UTF8 file
    procedure InitFile(const aFileName: OWideString); overload; virtual; abstract;
    //init document from UTF8 file
    procedure InitStream(const aStream: TStream); overload; virtual; abstract;
    //init JSON from OWideString
    procedure InitString(const aJSON: OWideString); virtual; abstract;
    //init JSON from UTF8 string
    procedure InitString_UTF8(const aJSON: OUTF8Container); virtual; abstract;
    //init document from UTF8 TBytes buffer
    procedure InitBytes(const aBuffer: TBytes); virtual;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer); virtual; abstract;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument; virtual; abstract;
  public
    property LastToken: TCustomJSONReaderToken read GetReaderToken;

    //use ReadNextToken for reading next JSON token
    function ReadNextToken(var outToken: TCustomJSONReaderToken): Boolean; virtual; abstract;

    function GoToObject(var outToken: TCustomJSONReaderToken): Boolean;
    procedure ReadObject(const aObject: TObject); overload;
    procedure ReadObject(const aObject: TObject; const aPropList: TRPropList); overload;
    procedure ReadObjectOrArrayToObjectList(const aList: TObjectList; const aItemClass: TClass); overload;
    procedure ReadObjectOrArrayToObjectList(const aList: TObjectList; const aItemClass: TClass;
      const aCreateAndReadItem: TMethodCreateAndReadItem); overload;
    procedure ReadObjectProperty(const aObject: TObject; const aPropInfo: PPropInfo);
  end;

  EJSONWriterException = class(Exception);
  EJSONWriterWrongStructure = class(EJSONWriterException);


function OJSONIsWhiteSpaceChar(const aChar: Byte): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF} overload;
function OJSONIsWhiteSpaceChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF} overload;
function OJSONIsNumberChar(const aChar: Byte): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF} overload;
function OJSONIsNumberChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF} overload;


implementation

uses
  OXmlLng
  {$IFDEF O_DELPHI_2007_DOWN}
  , Controls//definition of TTime and TDate
  {$ENDIF};

function OJSONIsWhiteSpaceChar(const aChar: Byte): Boolean;
begin
  case aChar of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OJSONIsWhiteSpaceChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OJSONIsNumberChar(const aChar: Byte): Boolean;
begin
  case aChar of
    Ord('0')..Ord('9'), Ord('e'), Ord('E'), Ord('.'), Ord('+'), Ord('-'): Result := True;
  else
    Result := False;
  end;
end;

function OJSONIsNumberChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    Ord('0')..Ord('9'), Ord('e'), Ord('E'), Ord('.'), Ord('+'), Ord('-'): Result := True;
  else
    Result := False;
  end;
end;

{ TCustomJSONWriter }

function TCustomJSONWriter.Boolean(const aBoolean: Boolean): TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TCustomJSONWriter.Boolean(const aPairName: OWideString;
  const aBoolean: Boolean): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TCustomJSONWriter.BooleanUnicode(const aPairName: OUnicodeString;
  const aBoolean: Boolean): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TCustomJSONWriter.BooleanUTF8(const aPairName: OUTF8Container;
  const aBoolean: Boolean): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TCustomJSONWriter.CheckState(const aState: TJSONWriterState): Boolean;
begin
  Result := (fStateTree[fStateTreeCurrent] = aState);
end;

procedure TCustomJSONWriter.CheckWritePair;
begin
  if not (CheckState(wsObject) and not fStateObjectValueAfterPairName)//write pair only in object
  then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotWritePair);

  if fNextPairValueNeedsSeparator then
    Separator;

  fNextPairValueNeedsSeparator := True;
end;

procedure TCustomJSONWriter.CheckWriteValue;
begin
  if not(
     (CheckState(wsObject) and fStateObjectValueAfterPairName) or
      CheckState(wsArray) or//write value in an array
     (CheckState(wsDocument) and not fNextPairValueNeedsSeparator))//write value at the beginning of a document
  then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotWriteValue);

  if fNextPairValueNeedsSeparator and not fStateObjectValueAfterPairName then
    Separator;

  fNextPairValueNeedsSeparator := True;
end;

function TCustomJSONWriter.CloseArray: TCustomJSONWriter;
begin
  if not CheckState(wsArray) then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotCloseArray);

  ExitState;
  WriteChar(Ord(']'));
  Result := Self;
end;

function TCustomJSONWriter.CloseObject: TCustomJSONWriter;
begin
  if not CheckState(wsObject) then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotCloseObject);

  ExitState;
  WriteChar(Ord('}'));
  Result := Self;
end;

constructor TCustomJSONWriter.Create(const aFileName: OWideString);
begin
  inherited Create;

  DoCreate;

  InitFile(aFileName);
end;

constructor TCustomJSONWriter.Create(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream, aOwnsStream);
end;

constructor TCustomJSONWriter.Create;
begin
  inherited Create;

  DoCreate;

  InitStream(TMemoryStream.Create, True);
end;

destructor TCustomJSONWriter.Destroy;
begin
  ReleaseDocument;

  inherited Destroy;
end;

procedure TCustomJSONWriter.DoCreate;
begin
  SetLength(fStateTree, 1);
  fStateTree[0] := wsDocument;
end;

procedure TCustomJSONWriter.EnterState(const aState: TJSONWriterState);
begin
  if High(fStateTree) = fStateTreeCurrent then
    SetLength(fStateTree, Length(fStateTree)*2);

  Inc(fStateTreeCurrent);
  fStateTree[fStateTreeCurrent] := aState;
  fNextPairValueNeedsSeparator := False;
end;

procedure TCustomJSONWriter.ExitState;
begin
  if fStateTreeCurrent = 0 then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CloseTooMany);

  Dec(fStateTreeCurrent);
  fNextPairValueNeedsSeparator := True;
end;

function TCustomJSONWriter.Null(const aPairName: OWideString): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteNull;
  Result := Self;
end;

function TCustomJSONWriter.Null: TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteNull;
  Result := Self;
end;

function TCustomJSONWriter.NullUnicode(const aPairName: OUnicodeString
  ): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNull;
  Result := Self;
end;

function TCustomJSONWriter.NullUTF8(const aPairName: OUTF8Container
  ): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNull;
  Result := Self;
end;

function TCustomJSONWriter.Number(const aNumber: Extended): TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.Number(const aNumber: Integer): TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.Number(const aPairName: OWideString;
  const aNumber: Extended): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.Number(const aPairName: OWideString;
  const aNumber: Integer): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.NumberUnicode(const aPairName: OUnicodeString;
  const aNumber: Extended): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.NumberUnicode(const aPairName: OUnicodeString;
  const aNumber: Integer): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.NumberUTF8(const aPairName: OUTF8Container;
  const aNumber: Extended): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.NumberUTF8(const aPairName: OUTF8Container;
  const aNumber: Integer): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TCustomJSONWriter.OpenArray(const aPairName: OWideString
  ): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteArray;
  Result := Self;
end;

function TCustomJSONWriter.OpenArray: TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteArray;
  Result := Self;
end;

function TCustomJSONWriter.OpenArrayUnicode(const aPairName: OUnicodeString
  ): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteArray;
  Result := Self;
end;

function TCustomJSONWriter.OpenArrayUTF8(const aPairName: OUTF8Container
  ): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteArray;
  Result := Self;
end;

function TCustomJSONWriter.OpenObject(const aPairName: OWideString
  ): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteObject;
  Result := Self;
end;

function TCustomJSONWriter.OpenObject: TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteObject;
  Result := Self;
end;

function TCustomJSONWriter.OpenObjectUnicode(const aPairName: OUnicodeString
  ): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteObject;
  Result := Self;
end;

function TCustomJSONWriter.OpenObjectUTF8(const aPairName: OUTF8Container
  ): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteObject;
  Result := Self;
end;

procedure TCustomJSONWriter.PairName(const aString: OWideString);
begin
  CheckWritePair;

  WriteText(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TCustomJSONWriter.PairNameUnicode(const aString: OUnicodeString);
begin
  CheckWritePair;

  WriteTextUnicode(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TCustomJSONWriter.PairNameUTF8(const aString: OUTF8Container);
begin
  CheckWritePair;

  WriteTextUTF8(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TCustomJSONWriter.Separator;
begin
  WriteChars(Ord(','), 32);
end;

function TCustomJSONWriter.Text(const aPairName, aText: OWideString
  ): TCustomJSONWriter;
begin
  PairName(aPairName);
  WriteText(aText);
  Result := Self;
end;

function TCustomJSONWriter.Text(const aText: OWideString): TCustomJSONWriter;
begin
  CheckWriteValue;
  WriteText(aText);
  Result := Self;
end;

function TCustomJSONWriter.TextUnicode(const aPairName, aText: OUnicodeString
  ): TCustomJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteTextUnicode(aText);
  Result := Self;
end;

function TCustomJSONWriter.TextUnicode(const aText: OUnicodeString
  ): TCustomJSONWriter;
begin
  CheckWriteValue;

  WriteTextUnicode(aText);
  Result := Self;
end;

function TCustomJSONWriter.TextUTF8(const aPairName, aText: OUTF8Container
  ): TCustomJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteTextUTF8(aText);
  Result := Self;
end;

function TCustomJSONWriter.TextUTF8(const aText: OUTF8Container
  ): TCustomJSONWriter;
begin
  CheckWriteValue;

  WriteTextUTF8(aText);
  Result := Self;
end;

procedure TCustomJSONWriter.WriteArray;
begin
  EnterState(wsArray);
  WriteChar(Ord('['));
  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteBoolean(const aBoolean: Boolean);
begin
  if aBoolean then
    WriteString('true')
  else
    WriteString('false');

  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteChars(const aChar1, aChar2: Byte);
begin
  WriteChar(aChar1);
  WriteChar(aChar2);
end;

procedure TCustomJSONWriter.WriteNull;
begin
  WriteChars(Ord('n'), Ord('u'));
  WriteChars(Ord('l'), Ord('l'));
  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteNumber(const aNumber: Extended);
begin
  WriteString(ISOFloatToStr(aNumber));

  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteNumber(const aNumber: Integer);
begin
  WriteString(IntToStr(aNumber));

  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteObject;
begin
  EnterState(wsObject);
  WriteChar(Ord('{'));
  fStateObjectValueAfterPairName := False;
end;

procedure TCustomJSONWriter.WriteString(const aString: string);
var
  I: Integer;
begin
  for I := 1 to Length(aString) do
    WriteChar(Ord(aString[I]) and $FF);
end;

{ TCustomJSONReaderToken }

function TCustomJSONReaderToken.GetBooleanValue: Boolean;
begin
  Assert((fValueType = vtBoolean) and (fTokenType = ttValue));
  Result := fBooleanValue;
end;

function TCustomJSONReaderToken.GetDoubleValue: Double;
begin
  Assert((fValueType = vtNumber) and (fTokenType = ttValue));
  Result := fExtendedValue;
end;

function TCustomJSONReaderToken.GetExtendedValue: Extended;
begin
  Assert((fValueType = vtNumber) and (fTokenType = ttValue));
  Result := fExtendedValue;
end;

function TCustomJSONReaderToken.GetInt64Value: Int64;
var
  xNumber: Double;
begin
  xNumber := GetDoubleValue;
  Assert(Abs(Frac(xNumber)) < 1e-12);//Assert(SameValue(Frac(xNumber)), 0)) <<-- SameValue is not in Delphi 5
  Result := Round(xNumber);
end;

function TCustomJSONReaderToken.GetIntegerValue: Integer;
begin
  Result := GetInt64Value;
end;

{ TRPropList }

constructor TRPropList.Create(const aClass: TClass);
begin
  fPropCount := GetTypeData(aClass.ClassInfo)^.PropCount;
  GetMem(fPropList, PropCount*SizeOf(Pointer));
  GetPropInfos(aClass.ClassInfo, fPropList);
end;

destructor TRPropList.Destroy;
begin
  FreeMem(PropList);
end;

{ TCustomJSONReader }

constructor TCustomJSONReader.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

constructor TCustomJSONReader.Create;
begin
  inherited Create;

  DoCreate;
end;

procedure TCustomJSONReader.DoCreate;
begin

end;

function TCustomJSONReader.GoToObject(var outToken: TCustomJSONReaderToken): Boolean;
begin
  while ReadNextToken(outToken) do
    if outToken.TokenType = ttOpenObject then
    begin
      Result := True;
      Exit;
    end;

  Result := False
end;

procedure TCustomJSONReader.InitBytes(const aBuffer: TBytes);
begin
  InitBuffer(aBuffer[0], Length(aBuffer));
end;

procedure TCustomJSONReader.ReadObject(const aObject: TObject);
var
  xPropList: TRPropList;
begin
  xPropList := TRPropList.Create(aObject.ClassType);
  try
    ReadObject(aObject, xPropList);
  finally
    xPropList.Free;
  end;
end;

procedure TCustomJSONReader.ReadObject(const aObject: TObject;
  const aPropList: TRPropList);
var
  I: Integer;
  xPropInfo: PPropInfo;
  xNameToken: TCustomJSONReaderToken;
begin
  if LastToken.TokenType = ttOpenObject then
    ReadNextToken(xNameToken{%H-})
  else
    xNameToken := LastToken;

  while (xNameToken.TokenType = ttPairName) do
  begin
    //find property
    for I := 0 to aPropList.PropCount-1 do
    begin
      xPropInfo := aPropList.PropList^[I];
      if xNameToken.SamePairName(@xPropInfo^.Name) then
      begin
        ReadObjectProperty(aObject, xPropInfo);
        Break;
      end;
    end;

    if not (
      ReadNextToken(xNameToken) and//read separator
      (xNameToken.TokenType = ttSeparator) and
      ReadNextToken(xNameToken))//read next token (pair name / close object)
    then
      Break;
  end;
end;

procedure TCustomJSONReader.ReadObjectOrArrayToObjectList(const aList: TObjectList;
  const aItemClass: TClass);
var
  x: TMethodCreateAndReadItem;
begin
  x := nil;
  ReadObjectOrArrayToObjectList(aList, aItemClass, x);
end;

procedure TCustomJSONReader.ReadObjectOrArrayToObjectList(const aList: TObjectList;
  const aItemClass: TClass; const aCreateAndReadItem: TMethodCreateAndReadItem);
var
  xToken: TCustomJSONReaderToken;
  xIsArray: Boolean;
  xNew: TObject;
  xPropList: TRPropList;
begin
  if not ReadNextToken(xToken{%H-}) then
    Exit;

  xIsArray := xToken.TokenType = ttOpenArray;
  if xIsArray then
    ReadNextToken(xToken);

  if xToken.TokenType <> ttOpenObject then
    Exit;

  xPropList := TRPropList.Create(aItemClass);
  try
    while xToken.TokenType = ttOpenObject do
    begin
      if Assigned(aCreateAndReadItem) then
        xNew := aCreateAndReadItem(Self, xPropList)
      else
      begin
        xNew := aItemClass.Create;
        ReadObject(xNew, xPropList);
      end;
      aList.Add(xNew);

      if not(
        xIsArray and
        ReadNextToken(xToken) and//read separator ','
        (xToken.TokenType = ttSeparator) and
        ReadNextToken(xToken))//read next token if last was separator (expected ttOpenObject)
      then
        Break;
    end;//while
  finally
    xPropList.Free;
  end;
end;

procedure TCustomJSONReader.ReadObjectProperty(const aObject: TObject;
  const aPropInfo: PPropInfo);

  procedure _ReadClass;
  var
    xPropObject: TObject;
  begin
    xPropObject := GetObjectProp(aObject, aPropInfo);
    if Assigned(xPropObject) then
    begin
      ReadObject(xPropObject);

      (*if (xPropObject is TCollection) then
      begin
        case CollectionStyle of
          csOXml:
            if bPropElement.SelectNode('_oxmlcollection', {%H-}xEnumerationNode) then
              ReadCollectionItems(TCollection(xPropObject), xEnumerationNode, 'i');
          csOmniXML:
            ReadCollectionItems(TCollection(xPropObject), bPropElement, TCollection(xPropObject).ItemClass.ClassName);
        end;
      end;*)
    end;
  end;
var
  xPropType: PTypeInfo;
  xStrValue: OWideString;
  xOrdValue: Integer;
  xFloatValue: Double;
  xValueToken: TCustomJSONReaderToken;
begin
  if not Assigned(aPropInfo^.GetProc) then
    Exit;

  xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};

  if xPropType^.Kind = tkClass then
  begin
    _ReadClass;
  end else
  begin
    if not(ReadNextToken(xValueToken{%H-}) and (xValueToken.TokenType = ttValue)) then
      Exit;

    case xPropType^.Kind of
      {$IFDEF FPC}
      tkBool:
        SetOrdProp(aObject, aPropInfo, Integer(xValueToken.GetBooleanValue));
      {$ENDIF}
      tkInteger, tkEnumeration:
        SetOrdProp(aObject, aPropInfo, xValueToken.GetInt64Value);
      tkChar, tkWChar{$IFDEF FPC}, tkUChar{$ENDIF}:
      begin
        xStrValue := xValueToken.GetStringValue;
        case xPropType^.Kind of
          tkChar {$IFNDEF FPC}, tkWChar{$ENDIF}: xOrdValue := Integer(xStrValue[1]);
          {$IFDEF FPC}tkWChar, tkUChar: xOrdValue := Integer(UTF8Decode(xStrValue)[1]);{$ENDIF}
        else
          xOrdValue := 0;
        end;
        SetOrdProp(aObject, aPropInfo, xOrdValue);
      end;
      tkSet:
        SetSetProp(aObject, aPropInfo, xValueToken.GetStringValue);
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString{$ENDIF}:
        SetStrProp(aObject, aPropInfo, xValueToken.GetStringValue);
      {$IFDEF O_HASBYTESTRINGS}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        SetWideStrProp(aObject, aPropInfo, {$IFDEF FPC}UTF8Decode{$ENDIF}(xValueToken.GetStringValue));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        if (xPropType = System.TypeInfo(TDateTime)) then
          xFloatValue := ISOStrToDateTime(xValueToken.GetStringValue)
        else if (xPropType = System.TypeInfo(TTime)) then
          xFloatValue := ISOStrToTime(xValueToken.GetStringValue)
        else if (xPropType = System.TypeInfo(TDate)) then
          xFloatValue := ISOStrToDate(xValueToken.GetStringValue)
        else
          xFloatValue := xValueToken.GetDoubleValue;
        SetFloatProp(aObject, aPropInfo, xFloatValue);
      end;
      tkInt64:
        SetInt64Prop(aObject, aPropInfo, StrToInt64(xValueToken.GetStringValue));
      tkClass:
        _ReadClass;
    end;
  end;
end;

end.

