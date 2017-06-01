unit UramakiBase;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, SysUtils,
  UramakiFrameworkConnector;

type
  TUramakiException = class (Exception);

  TUramakiPlate = class;

  TUraStreamFormat = (usfXML, usfJSON);

  TUramakiRoll = class abstract
  public
    const NULL_URAMAKI_ID = '**NULL**';
  public
    function GetMyId : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    function CanBeCached : boolean; virtual; abstract;

    procedure Init; virtual; abstract;
    procedure BeforeRead; virtual; abstract;
    procedure AfterRead; virtual; abstract;
  end;

  TUramakiPlate = class;

  IUramakiFrameworkConnector = interface
    procedure PleaseRefreshMyChilds (aPlate : TUramakiPlate);
  end;

  { TUramakiPlate }

  TUramakiPlate = class abstract
  private
    FInstanceIdentifier : TGuid;
  protected
    FFrameworkConnector : IUramakiFrameworkConnector;
  public
    constructor Create (aFrameworkConnector : IUramakiFrameworkConnector); virtual;
    destructor Destroy; override;

    function GetUramaki(const aUramakiId: String) : TUramakiRoll; virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;

    property InstanceIdentifier : TGuid read FInstanceIdentifier write FInstanceIdentifier;
  end;

  TUramakiPublicationContext = class abstract
  public
    procedure SaveTo (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
    procedure LoadFrom (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
  end;

  { TUramakiPublisher }

  TUramakiPublisher = class abstract
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual; abstract;

    function GetInputUramakiId : String; virtual; abstract;

    function CreatePlate (aFrameworkConnector : TUramakiFrameworkConnector) : TUramakiPlate; virtual;
    function CreatePublicationContext : TUramakiPublicationContext; virtual; abstract;
    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;

    procedure Publish(aInput : TUramakiRoll; aPlate : TUramakiPlate; aContext : TUramakiPublicationContext); virtual; abstract;
  end;

  TUramakiTransformationContext = class abstract
  public
    procedure SaveTo (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
    procedure LoadFrom (aStream : TStream; const aFormat: TUramakiStreamFormat); virtual; abstract;
  end;

  TUramakiTransformer = class abstract
  public
    function GetMyId : String; virtual; abstract;
    function GetDescription : String; virtual; abstract;
    function GetHelp : String; virtual; abstract;

    function GetInputUramakiId : String; virtual; abstract;
    function GetOutputUramakiId : String; virtual; abstract;

    function CreateTransformationContext : TUramakiTransformationContext; virtual; abstract;

    function Configure (aInput : TUramakiRoll; aContext : TUramakiTransformationContext); virtual; abstract;
    function Transform (aInput : TUramakiRoll; aContext : TUramakiTransformationContext); virtual; abstract;

    procedure StartTransaction(const aTransactionId : TGuid); virtual; abstract;
    procedure EndTransaction(const aTransactionId: TGuid); virtual; abstract;
  end;


implementation

uses
  SysUtils;

{ TUramakiPlate }

constructor TUramakiPlate.Create(aFrameworkConnector: TUramakiFrameworkConnector);
begin
  FFrameworkConnector:= aFrameworkConnector;
end;

destructor TUramakiPlate.Destroy;
begin
  inherited Destroy;
end;

end.
