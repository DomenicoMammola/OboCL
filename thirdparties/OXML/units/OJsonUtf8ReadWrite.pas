unit OJsonUtf8ReadWrite;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OJsonUtf8ReadWrite.pas

  !!! Beta version - everything can change here !!!

  See /unittest/OXmlUnitTests.pas:
      TOXmlUnitTest.Test_OJSON_TCustomJSONWriter_Test1
      TOXmlUnitTest.Test_OJSON_TCustomJSONReader_Test1
    for example code.

  TJSONWriterUTF8
    - fast sequential JSON writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      and the writer takes care of valid JSON escaping

    - indentation is not supported yet
    - new line handling is not supported yet

    - Outputs is always UTF8. Use *UTF8 functions for no conversion.
    - BOM is not written.
    - Faster than TJSONWriter but can write UTF-8 only.

  TJSONReaderUTF8
    - a standalone direct JSON parser.
    - new line handling is not supported yet.
    - source file must be a valid JSON, there is no error handling.
    - Input is UTF8, output can be either UTF8 or string.
    - Use if you want to read UTF8 strings.
    - Faster than TJSONWriter but can load UTF-8 only.
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

  TypInfo,
  OBufferedStreams, OEncoding, OWideSupp, OXmlUtils, OTextReadWrite, OJsonUtils;

type
  TJSONWriterUTF8 = class(TCustomJSONWriter)
  private
    fStream: TStream;
    fStreamPosition: OStreamInt;
    fOwnsStream: Boolean;
  protected
    procedure WriteChar(const aChar: Byte); override;
    procedure WriteText(const aText: OWideString); override;//escaped text
    procedure WriteTextUnicode(const aText: OUnicodeString); override;
    procedure WriteTextUTF8(const aText: OUTF8Container); override;
  public
    procedure InitFile(const aFileName: OWideString); override;
    procedure InitStream(const aStream: TStream; const aOwnsStream: Boolean = False); override;

    //write the whole temporary buffer to the destination stream (if used)
    procedure EnsureTempBufferWritten; override;
    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument; override;
  public
    function BufferSize: OStreamInt;
    procedure AsBuffer(var Buffer);//Buffer must be of size BufferSize
    function AsJSON: OUTF8Container;
    function AsBytes: TBytes;
    function AsEncodingBuffer: TEncodingBuffer;
    function AsString: OWideString;
  end;

  TJSONReaderUTF8 = class;

  TJSONReaderUTF8Token = class(TCustomJSONReaderToken)
  protected
    fPairNameUTF8: OUTF8Container;
    fStringValueUTF8: OUTF8Container;

    function GetPairName: OWideString; override;
    function GetPairNameUTF8: OUTF8Container; override;
    function GetPairNameUnicode: OUnicodeString; override;

    function GetStringValue: OWideString; override;
    function GetStringValueUTF8: OUTF8Container; override;
    function GetStringValueUnicode: OUnicodeString; override;

    function SamePairName(const aShortStringPointer: PByte): Boolean; override;
  public
    property PairNameUTF8: OUTF8Container read GetPairNameUTF8;
    property StringValueUTF8: OUTF8Container read GetStringValueUTF8;
  end;

  TJSONReaderUTF8 = class(TCustomJSONReader)
  protected
    fReader: TOCustomUTF8Reader;
    fReaderToken: TJSONReaderUTF8Token;
    fReadBuffer: TOByteBuffer;

    procedure DoCreate; override;
    function GetReaderToken: TCustomJSONReaderToken; override;

    function CheckRead(const aChars: string): Boolean;
    function ReadNumber(var outNumber: Extended): Boolean;
    function ReadString: Boolean;
  public
    destructor Destroy; override;
  public
    //The Init* procedures initialize a JSON document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TJSONReaderUTF8 or you call ReleaseDocument!

    //init document from UTF8 file
    procedure InitFile(const aFileName: OWideString); override;
    //init document from UTF8 file
    procedure InitStream(const aStream: TStream); override;
    //init JSON from OWideString
    procedure InitString(const aJSON: OWideString); override;
    //init JSON from UTF8 string
    procedure InitString_UTF8(const aJSON: OUTF8Container); override;
    //init document from UTF8 buffer
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer); override;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument; override;
  public
    //use ReadNextToken for reading next JSON token
    function ReadNextToken(var outToken: TCustomJSONReaderToken): Boolean; override;
  end;

implementation

{ TJSONReaderUTF8Token }

function TJSONReaderUTF8Token.GetPairName: OWideString;
begin
  Result := OUTF8ContainerToWide(GetPairNameUTF8);
end;

function TJSONReaderUTF8Token.GetPairNameUnicode: OUnicodeString;
begin
  Result := OUTF8ContainerToUnicode(GetPairNameUTF8);
end;

function TJSONReaderUTF8Token.GetPairNameUTF8: OUTF8Container;
begin
  Assert(fTokenType = ttPairName);
  Result := fPairNameUTF8;
end;

function TJSONReaderUTF8Token.GetStringValue: OWideString;
begin
  Result := OUTF8ContainerToWide(GetStringValueUTF8);
end;

function TJSONReaderUTF8Token.GetStringValueUnicode: OUnicodeString;
begin
  Result := OUTF8ContainerToUnicode(GetStringValueUTF8);
end;

function TJSONReaderUTF8Token.GetStringValueUTF8: OUTF8Container;
begin
  Assert((fValueType = vtString) and (fTokenType = ttValue));
  Result := fStringValueUTF8;
end;

function TJSONReaderUTF8Token.SamePairName(const aShortStringPointer: PByte
  ): Boolean;
var
  I: Integer;
  xShortStringPointer: PByte;
begin
  if not Assigned(aShortStringPointer) then
  begin
    Result := False;
    Exit;
  end;

  Result := Length(fPairNameUTF8) = aShortStringPointer^;//first is length
  if not Result then
    Exit;

  xShortStringPointer := aShortStringPointer;
  for I := 1 to aShortStringPointer^ do
  begin
    Inc(xShortStringPointer);
    if Ord(fPairNameUTF8[I]) <> xShortStringPointer^ then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TJSONReaderUTF8 }

function TJSONReaderUTF8.CheckRead(const aChars: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aChars) do
  begin
    fReader.IncCurrentChar;
    if fReader.EOF or (fReader.CurrentChar^ <> (Ord(aChars[I]) and $FF)) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TJSONReaderUTF8.DoCreate;
begin
  inherited DoCreate;

  fReader := nil;
  fReaderToken := TJSONReaderUTF8Token.Create;
  fReadBuffer := TOByteBuffer.Create;
end;

destructor TJSONReaderUTF8.Destroy;
begin
  fReaderToken.Free;
  fReadBuffer.Free;
  fReader.Free;

  inherited Destroy;
end;

function TJSONReaderUTF8.GetReaderToken: TCustomJSONReaderToken;
begin
  Result := fReaderToken;
end;

procedure TJSONReaderUTF8.InitBuffer(const aBuffer; const aBufferLength: Integer
  );
begin
  ReleaseDocument;

  fReader.Free;
  fReader := TOUTF8StreamReader.Create;
  TOUTF8StreamReader(fReader).InitBuffer(aBuffer, aBufferLength);
end;

procedure TJSONReaderUTF8.InitFile(const aFileName: OWideString);
begin
  ReleaseDocument;

  fReader.Free;
  fReader := TOUTF8StreamReader.Create;
  TOUTF8StreamReader(fReader).InitFile(aFileName);
end;

procedure TJSONReaderUTF8.InitString_UTF8(const aJSON: OUTF8Container);
begin
  ReleaseDocument;
  fReader.Free;
  fReader := TOUTF8StringReader.Create(aJSON);
end;

procedure TJSONReaderUTF8.InitStream(const aStream: TStream);
begin
  ReleaseDocument;

  fReader.Free;
  fReader := TOUTF8StreamReader.Create;
  TOUTF8StreamReader(fReader).InitStream(aStream);
end;

procedure TJSONReaderUTF8.InitString(const aJSON: OWideString);
begin
  InitString_UTF8(OWideToUTF8Container(aJSON));
end;

function TJSONReaderUTF8.ReadNextToken(var outToken: TCustomJSONReaderToken): Boolean;
var
  myOutToken: TJSONReaderUTF8Token;
begin
  outToken := fReaderToken;
  myOutToken := fReaderToken as TJSONReaderUTF8Token;
  Result := not fReader.EOF;
  if not Result then
    Exit;

  while OJSONIsWhiteSpaceChar(fReader.CurrentChar^) and fReader.IncCurrentChar do//jump over spaces
  begin
  end;

  case fReader.CurrentChar^ of
    Ord(','):
    begin
      myOutToken.fTokenType := ttSeparator;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('{'):
    begin
      myOutToken.fTokenType := ttOpenObject;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('}'):
    begin
      myOutToken.fTokenType := ttCloseObject;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('['):
    begin
      myOutToken.fTokenType := ttOpenArray;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord(']'):
    begin
      myOutToken.fTokenType := ttCloseArray;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('t'):
    begin
      Result := CheckRead('rue');
      if Result then//true
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtBoolean;
        myOutToken.fBooleanValue := True;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('f'):
    begin
      Result := CheckRead('alse');
      if Result then//false
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtBoolean;
        myOutToken.fBooleanValue := False;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('n'):
    begin
      Result := CheckRead('ull');
      if Result then//null
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtNull;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('0')..Ord('9'), Ord('+'), Ord('-'):
    begin
      Result := ReadNumber(myOutToken.fExtendedValue);
      if Result then
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtNumber;
      end;
    end;
    Ord('"'):
    begin
      Result := ReadString;
      if Result then
      begin
        fReader.IncCurrentChar;
        while OJSONIsWhiteSpaceChar(fReader.CurrentChar^) and fReader.IncCurrentChar do//jump over spaces
        begin
        end;

        if fReader.CurrentChar^ = Ord(':') then
        begin
          myOutToken.fTokenType := ttPairName;
          fReadBuffer.GetOUTF8Container(myOutToken.fPairNameUTF8);
          fReader.IncCurrentChar;
        end else
        begin
          myOutToken.fTokenType := ttValue;
          myOutToken.fValueType := vtString;
          fReadBuffer.GetOUTF8Container(myOutToken.fStringValueUTF8);
        end;
      end;
    end;
  else
    Result := False;//error
  end;
end;

function TJSONReaderUTF8.ReadNumber(var outNumber: Extended): Boolean;
begin
  fReadBuffer.Clear(False);
  repeat
    fReadBuffer.WriteChar(Char(fReader.CurrentChar^));//MUST BE WriteChar
  until not(fReader.IncCurrentChar and OJSONIsNumberChar(fReader.CurrentChar^));

  Result := ISOTryStrToFloat(fReadBuffer.GetString, outNumber);//MUST BE GetString <-> WriteChar
end;

function TJSONReaderUTF8.ReadString: Boolean;
var
  I: Integer;
  xHex: string;
  xHexC: Integer;
  xHexC_UTF8: OUTF8Container;
begin
  Result := False;

  fReadBuffer.Clear(False);
  while fReader.IncCurrentChar do
  begin
    case fReader.CurrentChar^ of
      Ord('\'):
      begin
        if not fReader.IncCurrentChar then Exit;
        case fReader.CurrentChar^ of
          Ord('\'), Ord('/'), Ord('"'): fReadBuffer.WriteByte(fReader.CurrentChar^);
          Ord('b'): fReadBuffer.WriteByte(8);
          Ord('t'): fReadBuffer.WriteByte(9);
          Ord('n'): fReadBuffer.WriteByte(10);
          Ord('f'): fReadBuffer.WriteByte(12);
          Ord('r'): fReadBuffer.WriteByte(13);
          Ord('u'):
          begin
            SetLength(xHex, 5);
            xHex[1] := '$';
            for I := 2 to Length(xHex) do
            begin
              if not fReader.IncCurrentChar then Exit;
              xHex[I] := Char(fReader.CurrentChar^);
            end;
            if not TryStrToInt(xHex, xHexC) then Exit;
            if (xHexC and $FF00) = 0 then//two-byte character
              fReadBuffer.WriteByte(xHexC and $FF)
            else
            begin//four-byte unicode character
              xHexC_UTF8 := OUnicodeToUTF8Container(OUnicodeChar(xHexC and $FFFF));
              for I := OUTF8Container_FirstElement to Length(xHexC_UTF8)-1+OUTF8Container_FirstElement do
                fReadBuffer.WriteByte(Ord(xHexC_UTF8[I]));
            end;
          end;
        else//not a valid escape
          Exit;//error
        end;
      end;
      Ord('"'): Break;
    else
      fReadBuffer.WriteByte(fReader.CurrentChar^);
    end;//case
  end;//while

  Result := True;
end;

procedure TJSONReaderUTF8.ReleaseDocument;
begin
  if Assigned(fReader) then
    fReader.ReleaseDocument;
end;

{ TJSONWriterUTF8 }

procedure TJSONWriterUTF8.AsBuffer(var Buffer);
var
  xPos: OStreamInt;
begin
  //no need to call EnsureTempBufferWritten -> it's written in BufferSize
  if BufferSize > 0 then
  begin
    xPos := fStream.Position;
    fStream.Position := fStreamPosition;
    fStream.Read(Buffer, BufferSize);
    fStream.Position := xPos;
  end;
end;

function TJSONWriterUTF8.AsBytes: TBytes;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[0]);
end;

function TJSONWriterUTF8.AsEncodingBuffer: TEncodingBuffer;
begin
  {$IFDEF O_DELPHI_2009_UP}
  Result := AsBytes;
  {$ELSE}
  Result := AsJSON;
  {$ENDIF}
end;

function TJSONWriterUTF8.AsJSON: OUTF8Container;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[OUTF8Container_FirstElement]);
end;

function TJSONWriterUTF8.AsString: OWideString;
begin
  Result := TEncoding.UTF8.BufferToString(AsEncodingBuffer);
end;

function TJSONWriterUTF8.BufferSize: OStreamInt;
begin
  Result := fStream.Size;
end;

procedure TJSONWriterUTF8.EnsureTempBufferWritten;
begin
  if fStream is TOBufferedWriteStream then
    TOBufferedWriteStream(fStream).EnsureTempBufferWritten;
end;

procedure TJSONWriterUTF8.InitFile(const aFileName: OWideString);
begin
  InitStream(
    TOBufferedWriteStream.Create(
      TOFileStream.Create(aFileName, fmCreate), True),
    True);
end;

procedure TJSONWriterUTF8.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  ReleaseDocument;

  fStream := aStream;
  fStreamPosition := aStream.Position;
  fOwnsStream := aOwnsStream;
end;

procedure TJSONWriterUTF8.ReleaseDocument;
begin
  if Assigned(fStream) then
  begin
    EnsureTempBufferWritten;
    if fOwnsStream then
      fStream.Free;
    fStream := nil;
  end;
end;

procedure TJSONWriterUTF8.WriteChar(const aChar: Byte);
begin
  fStream.WriteBuffer(aChar, SizeOf(aChar));
end;

procedure TJSONWriterUTF8.WriteText(const aText: OWideString);
begin
  {$IFDEF FPC}
  WriteTextUTF8(aText);
  {$ELSE}
  WriteTextUnicode(aText);
  {$ENDIF}
end;

procedure TJSONWriterUTF8.WriteTextUnicode(const aText: OUnicodeString);
begin
  WriteTextUTF8(OUnicodeToUTF8Container(aText));
end;

procedure TJSONWriterUTF8.WriteTextUTF8(const aText: OUTF8Container);
var
  I: Integer;
  C: PByte;
const
  cHexCharsLower: array[0..$0F] of Byte =
    (Ord('0'), Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'), Ord('6'), Ord('7'),
     Ord('8'), Ord('9'), Ord('a'), Ord('b'), Ord('c'), Ord('d'), Ord('e'), Ord('f'));
begin
  WriteChar(Ord('"'));
  if Length(aText) > 0 then
  begin
    C := @aText[OUTF8Container_FirstElement];
    for I := 0 to Length(aText)-1 do
    begin
      case C^ of
        Ord('"'), Ord('\'): WriteChars(Ord('\'), C^);
        8: WriteChars(Ord('\'), Ord('b'));
        9: WriteChars(Ord('\'), Ord('t'));
        10: WriteChars(Ord('\'), Ord('n'));
        12: WriteChars(Ord('\'), Ord('f'));
        13: WriteChars(Ord('\'), Ord('r'));
        0..7, 11, 14..31:
        begin
          WriteChars(Ord('\'), Ord('u'));
          WriteChars(Ord('0'), Ord('0'));
          WriteChars(cHexCharsLower[C^ shr 4], cHexCharsLower[C^ and $0F]);
        end;
      else
        WriteChar(C^);
      end;
      Inc(C);
    end;
  end;
  WriteChar(Ord('"'));
  fStateObjectValueAfterPairName := False;
end;

end.

