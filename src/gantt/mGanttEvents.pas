unit mGanttEvents;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type
  TmGanttDataProviderEventsSubscription =  class abstract
  public
    procedure Scrolled; virtual; abstract;
  end;

  TmGanttDataProviderEventsSubscriptionClass = class of TmGanttDataProviderEventsSubscription;



implementation

end.
