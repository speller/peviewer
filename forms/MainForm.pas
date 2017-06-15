unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, StdCtrls, PEViewerFrame, Themes, CommonUtilities;

type

  TfrmMain = class(TForm)
    tbMain: TToolBar;
    tbbImEx: TToolButton;
    tbbHdr: TToolButton;
    tbbInfo: TToolButton;
    tbbRes: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure tbMainButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FPlugin: Pointer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Plugin: Pointer read FPlugin write FPlugin;
  end;



implementation
uses
  PEViewerClass;


{$R *.dfm}


{ TfrmMain }


procedure TfrmMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle and not WS_EX_OVERLAPPEDWINDOW;
  Params.Style := Params.Style and not WS_OVERLAPPEDWINDOW;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Fix toolbar buttons width
  if (ThemeServices.ThemesEnabled) then
  begin
    for i := 0 to tbMain.ButtonCount - 1 do
    begin
      tbMain.Buttons[i].Caption := '  ' + tbMain.Buttons[i].Caption;
    end;
  end;
end;

procedure TfrmMain.FormKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  hk: THotKey;
begin
  hk := MakeHotKey(Key, Shift);
  if (hk.AsDWORD = HK_C_TAB.AsDWORD) or (hk.AsDWORD = HK_AC_TAB.AsDWORD) then
  begin
    TPEViewer(Plugin).ShowNextTab;
  end
  else if (hk.AsDWORD = HK_CS_TAB.AsDWORD) or (hk.AsDWORD = HK_ACS_TAB.AsDWORD) then
  begin
    TPEViewer(Plugin).ShowPreviousTab;
  end;
end;

procedure TfrmMain.tbMainButtonClick(Sender: TObject);
begin
  TPEViewer(Plugin).CurrentTab := TCurrentTab(TToolButton(Sender).Tag);
end;

end.
