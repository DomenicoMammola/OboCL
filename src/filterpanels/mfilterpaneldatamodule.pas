unit mFilterPanelDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

type

  { TmFilterPnlDataModule }

  TmFilterPnlDataModule = class(TDataModule)
    FilterPanelExecuteIcons: TImageList;
  public
  end;

implementation

{$R *.lfm}

end.

