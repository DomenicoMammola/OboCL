unit mPivotPropertiesFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  mPivoter;


resourcestring
  SCheckBoxVerticalGrandTotal = 'Show vertical grand totals';
  SCheckBoxHorizontalGrandTotal = 'Show horizontal grand totals';

type

  { TPivotPropertiesFrame }

  TPivotPropertiesFrame = class(TFrame)
  private
    FCBVerticalGrandTotal : TCheckBox;
    FCBHorizontalGrandTotal : TCheckBox;

    FSomethingChanged : boolean;
    procedure OnCBChange(aSender : TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init (const aPivoter : TmPivoter);
    procedure UpdateSettings(aPivoter : TmPivoter);

    property SomethingChanged : boolean read FSomethingChanged;
  end;

implementation

{$R *.lfm}

{ TPivotPropertiesFrame }

procedure TPivotPropertiesFrame.OnCBChange(aSender: TObject);
begin
  FSomethingChanged:= true;
end;

constructor TPivotPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCBVerticalGrandTotal := TCheckBox.Create(Self);
  FCBVerticalGrandTotal.Parent := Self;
  FCBVerticalGrandTotal.Align:= alTop;
  FCBVerticalGrandTotal.Caption:= SCheckBoxVerticalGrandTotal;
  FCBVerticalGrandTotal.OnChange:= @OnCBChange;

  FCBHorizontalGrandTotal := TCheckBox.Create(Self);
  FCBHorizontalGrandTotal.Parent := Self;
  FCBHorizontalGrandTotal.Align:= alTop;
  FCBHorizontalGrandTotal.Caption:= SCheckBoxHorizontalGrandTotal;
  FCBHorizontalGrandTotal.OnChange:= @OnCBChange;

  FSomethingChanged:= false;
end;

destructor TPivotPropertiesFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TPivotPropertiesFrame.Init(const aPivoter: TmPivoter);
begin
  FCBVerticalGrandTotal.Checked:= poVerticalGrandTotal in aPivoter.Options;
  FCBHorizontalGrandTotal.Checked:= poHorizontalGrandTotal in aPivoter.Options;
end;

procedure TPivotPropertiesFrame.UpdateSettings(aPivoter: TmPivoter);
begin
  if FCBVerticalGrandTotal.Checked then
    aPivoter.Options:= aPivoter.Options + [poVerticalGrandTotal]
  else
    aPivoter.Options:= aPivoter.Options - [poVerticalGrandTotal];

  if FCBHorizontalGrandTotal.Checked then
    aPivoter.Options:= aPivoter.Options + [poHorizontalGrandTotal]
  else
    aPivoter.Options:= aPivoter.Options - [poHorizontalGrandTotal];

end;

end.

