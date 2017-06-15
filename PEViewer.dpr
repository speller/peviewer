library PEViewer;
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

{$I 'libprefix.inc'}

uses
  SysUtils,
  Classes,
  MainFormUnit in 'forms\MainFormUnit.pas' {MainForm},
  DLLInterface in 'DLLInterface.pas',
  PEViewerClass in 'PEViewerClass.pas',
  CommonUtilities in 'CommonUtilities.pas',
  PEViewerFrameUnit in 'forms\PEViewerFrameUnit.pas' {PEViewerFrame: TFrame},
  InfoFrameUnit in 'forms\InfoFrameUnit.pas' {InfoFrame: TFrame},
  ImportExportFrameUnit in 'forms\ImportExportFrameUnit.pas' {ImportExportFrame: TFrame},
  HeadersFrameUnit in 'forms\HeadersFrameUnit.pas' {HeadersFrame: TFrame},
  PEUtils in 'PEUtils.pas',
  PETypes in 'PETypes.pas',
  PEHdrUtils in 'PEHdrUtils.pas',
  ResourcesFrameUnit in 'forms\ResourcesFrameUnit.pas' {ResourcesFrame: TFrame},
  DataViewUnit in 'DataViewUnit.pas',
  KOLClasses in 'KOLClasses.pas',
  WinApiWorks in 'WinApiWorks.pas';

{$R *.res}

exports
  {$IFNDEF WIN64}
  ListLoad,
  {$ENDIF}
  ListLoadW,
  ListCloseWindow,
  ListGetDetectString,
  ListSetDefaultParams;

begin
end.
