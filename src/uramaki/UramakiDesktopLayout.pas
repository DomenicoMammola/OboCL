unit UramakiDesktopLayout;
                          sdfsdf
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  TContainerType = (ctVertical, ctHorizontal, ctTabbed);

  TUramakiDesktopLayoutItem = class
  strict private

  end;

  TUramakiDesktopLayoutConfContainerItem = class (TUramakiDesktopLayoutItem)
  strict private
    FIsContainer : Boolean;
    FContainerType : TContainerType;
  end;

  function TContainerTypeToString (aValue : TContainerType) : String;
  function StringToTContainerType (aValue : String) : TContainerType;

implementation

function TContainerTypeToString(aValue: TContainerType): String;
begin
  case aValue of
    ctVertical : Result := 'vertical';
    ctHorizontal : Result := 'horizontal';
  else
    Result := 'tabbed';
  end;
end;

function StringToTContainerType(aValue: String): TContainerType;
begin
  if aValue = 'vertical' then
    Result := ctVertical
  else if aValue = 'horizontal' then
    Result := ctHorizontal
  else
    Result := ctTabbed;
end;

end.
