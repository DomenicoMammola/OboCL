unit OJsonReadWrite;

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

  TJSONWriter
    - fast sequential JSON writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      and the writer takes care of valid JSON escaping

    - indentation is not supported yet
    - new line handling is not supported yet

    - Output is defined by the target encoding.
    - BOM is not written.

  TJSONReader
    - a standalone direct JSON parser.
    - new line handling is not supported yet.
    - source file must be a valid JSON, there is no error handling.
    - Input is defined by the input encoding

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
  TJSONWriter = class(TCustomJSONWriter)
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

  TJSONReader = class;

  TJSONReaderToken = class(TCustomJSONReaderToken)
  protected
    fPairName: OWideString;
    fStringValue: OWideString;

    function GetPairName: OWideString; override;
    function GetPairNameUTF8: OUTF8Container; override;
    function GetPairNameUnicode: OUnicodeString; override;

    function GetStringValue: OWideString; override;
    function GetStringValueUTF8: OUTF8Container; override;
    function GetStringValueUnicode: OUnicodeString; override;

    function SamePairName(const aShortStringPointer: PByte): Boolean; override;
  end;

  TJSONReader = class(TCustomJSONReader)
  protected
    fReader: TOTextReader;
    fReaderToken: TJSONReaderToken;
    fReadBuffer: TOTextBuffer;

    procedure DoCreate; override;
    function GetReaderToken: TCustomJSONReaderToken; override;

    function CheckRead(const aChars: string): Boolean;
    function ReadNumber(const aFirstChar: OWideChar; var outNumber: Extended): Boolean;
    function ReadString: Boolean;
  public
    destructor Destroy; override;
  public
    //The Init* procedures initialize a JSON document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TJSONReaderUTF8 or you call ReleaseDocument!

    //init document from UTF8 file
    procedure InitFile(const aFileName: OWideString); overload; override;
    //init document from file with encoding
    procedure InitFile(const aFileName: OWideString; const aDefaultEncoding: TEncoding); overload;
    //init document from UTF8 stream
    procedure InitStream(const aStream: TStream); overload; override;
    //init document from stream with encoding
    procedure InitStream(const aStream: TStream; const aDefaultEncoding: TEncoding); overload;
    //init JSON from OWideString
    procedure InitString(const aJSON: OWideString); override;
    //init JSON from UTF8 string
    procedure InitString_UTF8(const aJSON: OUTF8Container); override;
    //init document from UTF8 buffer
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer); overload; override;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument; override;
  public
    //use ReadNextToken for reading next JSON token
    function ReadNextToken(var outToken: TCustomJSONReaderToken): Boolean; override;
  end;

implementation

{ TJSONReaderToken }

function TJSONReaderToken.GetPairName: OWideString;
begin
  Assert(fTokenType = ttPairName);
  Result := fPairName;
end;

function TJSONReaderToken.GetPairNameUnicode: OUnicodeString;
begin
  Result := OWideToUnicode(GetPairName);
end;

function TJSONReaderToken.GetPairNameUTF8: OUTF8Container;
begin
  Result := OWideToUTF8Container(GetPairName);
end;

function TJSONReaderToken.GetStringValue: OWideString;
begin
  Assert((fValueType = vtString) and (fTokenType = ttValue));
  Result := fStringValue;
end;

function TJSONReaderToken.GetStringValueUnicode: OUnicodeString;
begin
  Result := OWideToUnicode(GetStringValue);
end;

function TJSONReaderToken.GetStringValueUTF8: OUTF8Container;
begin
  Result := OWideToUTF8Container(GetStringValue);
end;

function TJSONReaderToken.SamePairName(const aShortStringPointer: PByte
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

  Result := Length(fPairName) = aShortStringPointer^;//first is length
  if not Result then
    Exit;

  xShortStringPointer := aShortStringPointer;
  for I := 1 to aShortStringPointer^ do
  begin
    Inc(xShortStringPointer);
    if Ord(fPairName[I]) <> xShortStringPointer^ then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TJSONReader }

function TJSONReader.CheckRead(const aChars: string): Boolean;
var
  I: Integer;
  xC: OWideChar;
begin
  for I := 1 to Length(aChars) do
  begin
    fReader.ReadNextChar(xC{%H-});
    if fReader.EOF or (xC <> OWideChar(aChars[I])) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

destructor TJSONReader.Destroy;
begin
  fReaderToken.Free;
  fReadBuffer.Free;
  fReader.Free;

  inherited Destroy;
end;

procedure TJSONReader.DoCreate;
begin
  inherited DoCreate;

  fReader := TOTextReader.Create;
  fReaderToken := TJSONReaderToken.Create;
  fReadBuffer := TOTextBuffer.Create;
end;

function TJSONReader.GetReaderToken: TCustomJSONReaderToken;
begin
  Result := fReaderToken;
end;

procedure TJSONReader.InitBuffer(const aBuffer; const aBufferLength: Integer);
begin
  fReader.InitBuffer(aBuffer, aBufferLength);
end;

procedure TJSONReader.InitFile(const aFileName: OWideString);
begin
  fReader.InitFile(aFileName, TEncoding.UTF8);
end;

procedure TJSONReader.InitFile(const aFileName: OWideString;
  const aDefaultEncoding: TEncoding);
begin
  fReader.InitFile(aFileName, aDefaultEncoding);
end;

procedure TJSONReader.InitString_UTF8(const aJSON: OUTF8Container);
begin
  fReader.InitString_UTF8(aJSON);
end;

procedure TJSONReader.InitStream(const aStream: TStream);
begin
  fReader.InitStream(aStream);
end;

procedure TJSONReader.InitStream(const aStream: TStream;
  const aDefaultEncoding: TEncoding);
begin
  fReader.InitStream(aStream, aDefaultEncoding);
end;

procedure TJSONReader.InitString(const aJSON: OWideString);
begin
  fReader.InitString(aJSON);
end;

function TJSONReader.ReadNextToken(var outToken: TCustomJSONReaderToken): Boolean;
var
  myOutToken: TJSONReaderToken;
  xC: OWideChar;
begin
  outToken := fReaderToken;
  myOutToken := fReaderToken as TJSONReaderToken;
  Result := not fReader.EOF;
  if not Result then
    Exit;

  while fReader.ReadNextChar({%H-}xC) and OJSONIsWhiteSpaceChar(xC) do//jump over spaces
  begin
  end;

  case Ord(xC) of
    Ord(','):
    begin
      myOutToken.fTokenType := ttSeparator;
      Result := True;
    end;
    Ord('{'):
    begin
      myOutToken.fTokenType := ttOpenObject;
      Result := True;
    end;
    Ord('}'):
    begin
      myOutToken.fTokenType := ttCloseObject;
      Result := True;
    end;
    Ord('['):
    begin
      myOutToken.fTokenType := ttOpenArray;
      Result := True;
    end;
    Ord(']'):
    begin
      myOutToken.fTokenType := ttCloseArray;
      Result := True;
    end;
    Ord('t'):
    begin
      Result := CheckRead('rue');
      if Result then//true
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtBoolean;
        myOutToken.fBooleanValue := True;
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
      end;
    end;
    Ord('n'):
    begin
      Result := CheckRead('ull');
      if Result then//null
      begin
        myOutToken.fTokenType := ttValue;
        myOutToken.fValueType := vtNull;
      end;
    end;
    Ord('0')..Ord('9'), Ord('+'), Ord('-'):
    begin
      Result := ReadNumber(xC, myOutToken.fExtendedValue);
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
        while fReader.ReadNextChar(xC) and OJSONIsWhiteSpaceChar(xC) do//jump over spaces
        begin
        end;

        if xC = ':' then
        begin
          myOutToken.fTokenType := ttPairName;
          fReadBuffer.GetBuffer(myOutToken.fPairName);
        end else
        begin
          myOutToken.fTokenType := ttValue;
          myOutToken.fValueType := vtString;
          fReadBuffer.GetBuffer(myOutToken.fStringValue);
          fReader.UndoRead;
        end;
      end;
    end;
  else
    Result := False;//error
  end;
end;

function TJSONReader.ReadNumber(const aFirstChar: OWideChar;
  var outNumber: Extended): Boolean;
var
  xC: OWideChar;
begin
  xC := aFirstChar;
  fReadBuffer.Clear(False);
  repeat
    fReadBuffer.WriteChar(xC);//MUST BE WriteChar
  until not(fReader.ReadNextChar(xC) and OJSONIsNumberChar(xC));

  if not fReader.Eof then
    fReader.UndoRead;

  Result := ISOTryStrToFloat(fReadBuffer.GetBuffer, outNumber);//MUST BE GetString <-> WriteChar
end;

function TJSONReader.ReadString: Boolean;
var
  I: Integer;
  xHex: string;
  xHexC: Integer;
  xC: OWideChar;
begin
  Result := False;

  fReadBuffer.Clear(False);
  while fReader.ReadNextChar({%H-}xC) do
  begin
    case Ord(xC) of
      Ord('\'):
      begin
        if not fReader.ReadNextChar(xC) then Exit;
        case Ord(xC) of
          Ord('\'), Ord('/'), Ord('"'): fReadBuffer.WriteChar(xC);
          Ord('b'): fReadBuffer.WriteChar(#08);
          Ord('t'): fReadBuffer.WriteChar(#09);
          Ord('n'): fReadBuffer.WriteChar(#10);
          Ord('f'): fReadBuffer.WriteChar(#12);
          Ord('r'): fReadBuffer.WriteChar(#13);
          Ord('u'):
          begin
            SetLength(xHex, 5);
            xHex[1] := '$';
            for I := 2 to Length(xHex) do
            begin
              if not fReader.ReadNextChar(xC) then Exit;
              xHex[I] := Char(xC);
            end;
            if not TryStrToInt(xHex, xHexC) then Exit;
            fReadBuffer.WriteChar(OWideChar(xHexC and $FFFF))
          end;
        else//not a valid escape
          Exit;//error
        end;
      end;
      Ord('"'): Break;
    else
      fReadBuffer.WriteChar(xC);
    end;//case
  end;//while

  Result := True;
end;

procedure TJSONReader.ReleaseDocument;
begin
  if Assigned(fReader) then
    fReader.ReleaseDocument;
end;

{ TJSONWriter }

procedure TJSONWriter.AsBuffer(var Buffer);
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

function TJSONWriter.AsBytes: TBytes;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[0]);
end;

function TJSONWriter.AsEncodingBuffer: TEncodingBuffer;
begin
  {$IFDEF O_DELPHI_2009_UP}
  Result := AsBytes;
  {$ELSE}
  Result := AsJSON;
  {$ENDIF}
end;

function TJSONWriter.AsJSON: OUTF8Container;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[OUTF8Container_FirstElement]);
end;

function TJSONWriter.AsString: OWideString;
begin
  Result := TEncoding.UTF8.BufferToString(AsEncodingBuffer);
end;

function TJSONWriter.BufferSize: OStreamInt;
begin
  Result := fStream.Size;
end;

procedure TJSONWriter.EnsureTempBufferWritten;
begin
  if fStream is TOBufferedWriteStream then
    TOBufferedWriteStream(fStream).EnsureTempBufferWritten;
end;

procedure TJSONWriter.InitFile(const aFileName: OWideString);
begin
  InitStream(
    TOBufferedWriteStream.Create(
      TOFileStream.Create(aFileName, fmCreate), True),
    True);
end;

procedure TJSONWriter.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  ReleaseDocument;

  fStream := aStream;
  fStreamPosition := aStream.Position;
  fOwnsStream := aOwnsStream;
end;

procedure TJSONWriter.ReleaseDocument;
begin
  if Assigned(fStream) then
  begin
    EnsureTempBufferWritten;
    if fOwnsStream then
      fStream.Free;
    fStream := nil;
  end;
end;

procedure TJSONWriter.WriteChar(const aChar: Byte);
begin
  fStream.WriteBuffer(aChar, SizeOf(aChar));
end;

procedure TJSONWriter.WriteText(const aText: OWideString);
begin
  {$IFDEF FPC}
  WriteTextUTF8(aText);
  {$ELSE}
  WriteTextUnicode(aText);
  {$ENDIF}
end;

procedure TJSONWriter.WriteTextUnicode(const aText: OUnicodeString);
begin
  WriteTextUTF8(OUnicodeToUTF8Container(aText));
end;

procedure TJSONWriter.WriteTextUTF8(const aText: OUTF8Container);
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

