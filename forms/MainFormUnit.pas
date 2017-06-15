unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, StdCtrls, PEViewerFrameUnit, Themes, CommonUtilities,
  HeadersFrameUnit, Vcl.ExtCtrls, WinApi.CommCtrl, Vcl.Menus, Vcl.ActnList,
  MyToolbarUnit;

type

  TMainFormStringStore = class(TStringStore)
  private
    FImageCorrupted: string;
  public
    constructor Create; override;
  published
    property ImageCorrupted: string read FImageCorrupted write FImageCorrupted;
  end;


  TMainForm = class(TForm)
    pParent: TPanel;
    tbMain: TMyToolBar;
    ToolButton1: TMyToolButton;
    tbbInfo: TMyToolButton;
    ToolButton2: TMyToolButton;
    tbbImEx: TMyToolButton;
    ToolButton3: TMyToolButton;
    tbbHdr: TMyToolButton;
    ToolButton4: TMyToolButton;
    tbbRes: TMyToolButton;
    ToolButton5: TMyToolButton;
    lblErrorMsg: TLabel;
    pmSettings: TPopupMenu;
    miLocalization: TMenuItem;
    tbbSettings: TMyToolButton;
    tbSep7: TMyToolButton;
    miDontUseTCPluginIni: TMenuItem;
    lblImgInfo: TLabel;
    miAutoDetermineCompiler: TMenuItem;
    miSaveLastOpenedTab: TMenuItem;
    procedure tbMainButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FPlugin: TPersistent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustLabels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Plugin: TPersistent read FPlugin write FPlugin;

    procedure Initialize;
    procedure SetImgInfo(const AText: string; AIsError: Boolean);
    procedure SetErrorMsg(const AText: string);
  published
    Strings: TMainFormStringStore;
  end;


  TMyMenuItem = class(TMenuItem)
  private
    FLocalizationName: string;
  public
    property LocalizationName: string read FLocalizationName write FLocalizationName;
  end;



implementation
uses
  PEViewerClass;


{$R *.dfm}


{ TfrmMain }


procedure TMainForm.AdjustLabels;
begin
  lblImgInfo.Left := tbSep7.Left + tbSep7.Width + 20;
  lblErrorMsg.Left := lblImgInfo.Left + lblImgInfo.Width + 10;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Strings := TMainFormStringStore.Create;
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle and not WS_EX_OVERLAPPEDWINDOW;
  Params.Style := Params.Style and not WS_OVERLAPPEDWINDOW;
end;

destructor TMainForm.Destroy;
begin
  Strings.Free;
  inherited;
end;

procedure TMainForm.FormKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  hk: THotKey;
  msg: TWMKey;
  frame: TPEViewerFrame;
begin
  hk := MakeHotKey(Key, Shift);
  if (hk.AsDWORD = HK_C_TAB.AsDWORD) or (hk.AsDWORD = HK_AC_TAB.AsDWORD) then
  begin
    TPEViewer(Plugin).ShowNextTab;
  end
  else if (hk.AsDWORD = HK_CS_TAB.AsDWORD) or (hk.AsDWORD = HK_ACS_TAB.AsDWORD) then
  begin
    TPEViewer(Plugin).ShowPreviousTab;
  end
  else
  begin
    FillChar(msg, SizeOf(msg), 0);
    msg.Msg := WM_KEYDOWN;
    msg.CharCode := Key;
    if (ssShift in Shift) then
      msg.KeyData := msg.KeyData or VK_SHIFT;
    if (ssAlt in Shift) then
      msg.KeyData := msg.KeyData or $20000000;
    if (ssCtrl in Shift) then
      msg.KeyData := msg.KeyData or VK_CONTROL;

    // Dispatch shortcuts to frame's actions
    frame := TPEViewer(Plugin).CurrentFrame;
    if (frame <> nil) then
    begin
      if frame.alActions.IsShortCut(msg) then
        Key := 0;
    end;
    if (Key <> 0) and (Shift = []) or (Key = VK_ESCAPE) then
    begin
      PostMessage(ParentWindow, msg.Msg, TMessage(msg).WParam, TMessage(msg).LParam);
    end;
  end;
end;

procedure TMainForm.Initialize;
begin
  lblImgInfo.Font.Size := tbMain.Font.Size;
  lblErrorMsg.Font.Size := tbMain.Font.Size;
  lblErrorMsg.Font.Color := clRed;
  AdjustLabels;
end;

procedure TMainForm.SetErrorMsg(const AText: string);
begin
  lblErrorMsg.Caption := AText;
end;

procedure TMainForm.SetImgInfo(const AText: string; AIsError: Boolean);
var
  s: string;
begin
  s := AText;
  lblImgInfo.Caption := s;
  if (AIsError) then
    lblImgInfo.Font.Color := clRed
  else
    lblImgInfo.Font.Color := clGreen;
  AdjustLabels;
end;

procedure TMainForm.tbMainButtonClick(Sender: TObject);
begin
  TPEViewer(Plugin).CurrentTab := TCurrentTab(TMyToolButton(Sender).Tag);
end;


{ TMainFormStringStore }


constructor TMainFormStringStore.Create;
begin
  inherited;
  ImageCorrupted := 'Image corrupted';
end;

end.
