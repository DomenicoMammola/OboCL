// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
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


  TmGanttHeadEventsSubscription =  class abstract
  public
    procedure LayoutChanged; virtual; abstract;
    procedure Scrolled; virtual; abstract;
  end;

  TmGanttHeadEventsSubscriptionClass = class of TmGanttHeadEventsSubscription;


  TmTimerulerEventsSubscription =  class abstract
  public
    procedure LayoutChanged; virtual; abstract;
    procedure DateChanged(const OldDate: TDateTime); virtual; abstract;
  end;

  TmTimerulerEventsSubscriptionClass = class of TmTimerulerEventsSubscription;

implementation

end.
