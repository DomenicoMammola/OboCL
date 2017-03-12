program OXmlTest;

uses
  Forms,
  OXmlTestUnit in 'OXmlTestUnit.pas' {Form1},
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlDOMVendor in '..\..\units\OXmlDOMVendor.pas',
  OXmlCDOM in '..\..\units\OXmlCDOM.pas',
  OXmlCSeq in '..\..\units\OXmlCSeq.pas',
  OXmlPSeq in '..\..\units\OXmlPSeq.pas',
  OXmlSerialize in '..\..\units\OXmlSerialize.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
