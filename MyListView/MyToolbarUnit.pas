unit MyToolbarUnit;

interface
uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, Winapi.CommCtrl, System.Classes, Vcl.Forms, Vcl.Controls, Vcl.Menus,
  Vcl.Graphics, Vcl.StdCtrls, Winapi.RichEdit, Vcl.ToolWin, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ListActns,
  Winapi.ShlObj, Vcl.Themes, Vcl.GraphUtil, System.UITypes, Vcl.ComCtrls;


type

  TMyToolBar = class;
  TMyToolButton = class;

{ TMyToolButtonActionLink }

  TMyToolButtonActionLink = class(TControlActionLink)
  protected
    FClient: TMyToolButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsDropdownMenuLinked: Boolean; override;
    function IsEnableDropdownLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetDropdownMenu(Value: TPopupMenu); override;
    procedure SetEnableDropdown(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TMyToolButtonActionLinkClass = class of TMyToolButtonActionLink;

  TMyToolButton = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FAutoSize: Boolean;
    FDown: Boolean;
    FGrouped: Boolean;
    FImageIndex: TImageIndex;
    FIndeterminate: Boolean;
    FMarked: Boolean;
    FMenuItem: TMenuItem;
    FDropdownMenu: TPopupMenu;
    FEnableDropdown: Boolean;
    FWrap: Boolean;
    FStyle: TToolButtonStyle;
    FUpdateCount: Integer;
    function GetButtonState: Byte;
    function GetIndex: Integer;
    function IsCheckedStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetButtonState(State: Byte);
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetGrouped(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetIndeterminate(Value: Boolean);
    procedure SetMarked(Value: Boolean);
    procedure SetMenuItem(Value: TMenuItem);
    procedure SetStyle(Value: TToolButtonStyle);
    procedure SetWrap(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    FToolBar: TMyToolBar;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RefreshControl; virtual;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SeTMyToolBar(AToolBar: TMyToolBar);
    procedure UpdateControl; virtual;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CheckMenuDropdown: Boolean; dynamic;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Index: Integer read GetIndex;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption;
    property Down: Boolean read FDown write SetDown stored IsCheckedStored default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property Enabled;
    property EnableDropdown: Boolean read FEnableDropdown write SetEnableDropdown default False;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property Height stored False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Indeterminate: Boolean read FIndeterminate write SetIndeterminate default False;
    property Marked: Boolean read FMarked write SetMarked default False;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property ParentShowHint;
    property PopupMenu;
    property Wrap: Boolean read FWrap write SetWrap default False;
    property ShowHint;
    property Style: TToolButtonStyle read FStyle write SetStyle default tbsButton;
    property Visible;
    property Width stored IsWidthStored;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TTBCustomDrawEvent = procedure(Sender: TMyToolBar; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TTBCustomDrawBtnEvent = procedure(Sender: TMyToolBar; Button: TMyToolButton;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TTBAdvancedCustomDrawEvent = procedure(Sender: TMyToolBar; const ARect: TRect;
    Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TTBAdvancedCustomDrawBtnEvent = procedure(Sender: TMyToolBar; Button: TMyToolButton;
    State: TCustomDrawState; Stage: TCustomDrawStage;
    var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean) of object;
  TTBCustomizeQueryEvent = procedure(Sender: TMyToolBar; Index: Integer;
    var Allow: Boolean) of object;
  TTBNewButtonEvent = procedure(Sender: TMyToolBar; Index: Integer;
    var Button: TMyToolButton) of object;
  TTBButtonEvent = procedure(Sender: TMyToolBar; Button: TMyToolButton) of object;


  TMyToolBar = class(TToolWindow)
  private
    FBitmap: TBitmap;
    FAllowTextButtons: Boolean;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FCaption: string;
    FCanvas: TCanvas;
    FCanvasChanged: Boolean;
    FCustomizable: Boolean;
    FCustomizing: Boolean;
    FGradientDrawingOptions: TTBGradientDrawingOptions;
    FGradientDirection: TGradientDirection;
    FDrawingStyle: TTBDrawingStyle;
    FGradientEndColor: TColor;
    FGradientStartColor: TColor;
    FHotTrackColor: TColor;
    FShowCaptions: Boolean;
    FList: Boolean;
    FFlat: Boolean;
    FLastQueryDeleteIndex: Integer;
    FTransparent: Boolean;
    FTransparentSet: Boolean;
    FWrapable: Boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDisabledImageChangeLink: TChangeLink;
    FHotImages: TCustomImageList;
    FHotImageChangeLink: TChangeLink;
    FIndent: Integer;
    FNewStyle: Boolean;
    FNullBitmap: TBitmap;
    FOldHandle: HBitmap;
    FRestoring: Boolean;
    FUpdateCount: Integer;
    FHeightMargin: Integer;
    FSeparators: Integer;
    FOnAdvancedCustomDraw: TTBAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawButton: TTBAdvancedCustomDrawBtnEvent;
    FOnCustomDraw: TTBCustomDrawEvent;
    FOnCustomDrawButton: TTBCustomDrawBtnEvent;
    FOnCustomizeCanDelete: TTBCustomizeQueryEvent;
    FOnCustomizeCanInsert: TTBCustomizeQueryEvent;
    FOnCustomizeNewButton: TTBNewButtonEvent;
    FOnCustomized: TNotifyEvent;
    FOnCustomizeDelete: TTBButtonEvent;
    FOnCustomizeAdded: TTBButtonEvent;
    FOnCustomizing: TNotifyEvent;
    FOnCustomizeReset: TNotifyEvent;
    { Toolbar menu support }
    FCaptureChangeCancels: Boolean;
    FInMenuLoop: Boolean;
    FTempMenu: TPopupMenu;
    FButtonMenu: TMenuItem;
    FMenuButton: TMyToolButton;
    FMenuResult: Boolean;
    FMenuDropped: Boolean;
    FMenu: TMainMenu;
    FCustomizeKeyName: string;
    FCustomizeValueName: string;
    FOurFont: Integer;
    FStockFont: Integer;
    FHideClippedButtons: Boolean;
    class constructor Create;
    function IsGradientEndColorStored: Boolean;
    function ButtonIndex(OldIndex, ALeft, ATop: Integer): Integer;
    procedure CanvasChanged(Sender: TObject);
    function DoGetButton(var NMToolbar: TNMToolbar): Boolean;
    procedure LoadImages(AImages: TCustomImageList);
    function GetButton(Index: Integer): TMyToolButton;
    function GetButtonCount: Integer;
    procedure GetButtonSize(var AWidth, AHeight: Integer);
    function GetRowCount: Integer;
    procedure SetAllowTextButtons(Value: Boolean);
    procedure SetList(Value: Boolean);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWrapable(Value: Boolean);
    procedure InsertButton(Control: TControl);
    procedure RemoveButton(Control: TControl);
    function RefreshButton(Index: Integer): Boolean;
    procedure UpdateButton(Index: Integer);
    procedure UpdateButtons;
    procedure UpdateButtonState(Index: Integer);
    procedure UpdateButtonStates;
    function UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
    function UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
    procedure ClearTempMenu;
    procedure CreateButtons(NewWidth, NewHeight: Integer);
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonHeight(Value: Integer);
    procedure UpdateImages;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageList(Value: HImageList);
    procedure SetImages(Value: TCustomImageList);
    procedure DisabledImageListChange(Sender: TObject);
    procedure SetDisabledImageList(Value: HImageList);
    procedure SetDisabledImages(Value: TCustomImageList);
    procedure HotImageListChange(Sender: TObject);
    procedure SetHotImageList(Value: HImageList);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetIndent(Value: Integer);
    procedure SetMenu(const Value: TMainMenu);
    procedure AdjustControl(Control: TControl);
    procedure RecreateButtons;
    procedure RecreateButtonsFromToolbar;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResizeButtons;
    procedure SaveButtons(Save: Boolean);
    function InternalButtonCount: Integer;
    function ReorderButton(OldIndex, ALeft, ATop: Integer): Integer;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var Message: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMNotifyFormat(var Message: TWMNotifyFormat); message WM_NOTIFYFORMAT;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSysChar(var Message: TWMSysChar); message WM_SYSCHAR;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNChar(var Message: TWMChar); message CN_CHAR;
    procedure CNSysKeyDown(var Message: TWMSysKeyDown); message CN_SYSKEYDOWN;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CNDropDownClosed(var Message: TMessage); message CN_DROPDOWNCLOSED;
    procedure CNNotify(var Message: TWMNotifyTLB); message CN_NOTIFY;
    procedure SetCustomizable(const Value: Boolean);
    procedure SetHideClippedButtons(const Value: Boolean);
    procedure SetGradientDrawingOptions(Value: TTBGradientDrawingOptions);
    procedure SetGradientDirection(Value: TGradientDirection);
    procedure SetGradientEndColor(Value: TColor);
    procedure SetGradientStartColor(Value: TColor);
    procedure SetDrawingStyle(Value: TTBDrawingStyle);
    function Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TTBButton): LRESULT; overload;
{$IFNDEF CLR}
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
{$ENDIF}
  protected
{$IFDEF CLR}
    FButtonHashTable: HashTable;
    procedure ControlChange(Inserting: Boolean; Child: TControl); override;
{$ENDIF}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CancelMenu; dynamic;
    procedure ChangeScale(M, D: Integer); override;
    function CheckMenuDropdown(Button: TMyToolButton): Boolean; dynamic;
    procedure ClickButton(Button: TMyToolButton); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GradientDrawToolBar(const ARect: TRect): Boolean; virtual;
    function GradientDrawButton(Button: TMyToolButton; State: TCustomDrawState;
      var Flags: TTBCustomDrawFlags): Boolean; virtual;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawButton(Button: TMyToolButton; State: TCustomDrawState;
      Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags): Boolean; virtual;
    function DoQueryInsert(Index: Integer): Boolean; virtual;
    function DoQueryDelete(Index: Integer): Boolean; virtual;
    function FindButtonFromAccel(Accel: Word): TMyToolButton;
    procedure InitMenu(Button: TMyToolButton); dynamic;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RepositionButton(Index: Integer);
    procedure RepositionButtons(Index: Integer);
    procedure WndProc(var Message: TMessage); override;
    function WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
    procedure Resize; override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
//    function GetEnumerator: TMyToolBarEnumerator;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function TrackMenu(Button: TMyToolButton): Boolean; dynamic;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TMyToolButton read GetButton;
    property Canvas: TCanvas read FCanvas;
    property CustomizeKeyName: string read FCustomizeKeyName write FCustomizeKeyName;
    property CustomizeValueName: string read FCustomizeValueName write FCustomizeValueName;
    property RowCount: Integer read GetRowCount;
  published
    property Align default alTop;
    property Anchors;
    property AutoSize;
    property BorderWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property Customizable: Boolean read FCustomizable write SetCustomizable default False;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DoubleBuffered;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle: TTBDrawingStyle read FDrawingStyle
      write SetDrawingStyle default dsNormal;
    property EdgeBorders default [];
    property EdgeInner;
    property EdgeOuter;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property GradientEndColor: TColor read FGradientEndColor
      write SetGradientEndColor stored IsGradientEndColorStored;
    property GradientStartColor: TColor read FGradientStartColor
      write SetGradientStartColor default clWindow;
    property Height default 32;
    property HideClippedButtons: Boolean read FHideClippedButtons write SetHideClippedButtons default False;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property HotTrackColor: TColor read FHotTrackColor
      write FHotTrackColor default $00EFD3C6;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 0;
    property List: Boolean read FList write SetList default False;
    property Menu: TMainMenu read FMenu write SetMenu;
    property GradientDirection: TGradientDirection read FGradientDirection
      write SetGradientDirection default gdVertical;
    property GradientDrawingOptions: TTBGradientDrawingOptions read FGradientDrawingOptions
      write SetGradientDrawingOptions default [gdoHotTrack, gdoGradient];
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property AllowTextButtons: Boolean read FAllowTextButtons write SetAllowTextButtons default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Transparent: Boolean read FTransparent write SetTransparent stored FTransparentSet;
    property Visible;
    property Wrapable: Boolean read FWrapable write SetWrapable default True;
    property OnAdvancedCustomDraw: TTBAdvancedCustomDrawEvent
      read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawButton: TTBAdvancedCustomDrawBtnEvent
      read FOnAdvancedCustomDrawButton write FOnAdvancedCustomDrawButton;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw: TTBCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawButton: TTBCustomDrawBtnEvent read FOnCustomDrawButton
      write FOnCustomDrawButton;
    property OnCustomizeAdded: TTBButtonEvent read FOnCustomizeAdded write FOnCustomizeAdded;
    property OnCustomizeCanInsert: TTBCustomizeQueryEvent read FOnCustomizeCanInsert
      write FOnCustomizeCanInsert;
    property OnCustomizeCanDelete: TTBCustomizeQueryEvent read FOnCustomizeCanDelete
      write FOnCustomizeCanDelete;
    property OnCustomized: TNotifyEvent read FOnCustomized write FOnCustomized;
    property OnCustomizeDelete: TTBButtonEvent read FOnCustomizeDelete write FOnCustomizeDelete;
    property OnCustomizing: TNotifyEvent read FOnCustomizing write FOnCustomizing;
    property OnCustomizeNewButton: TTBNewButtonEvent read FOnCustomizeNewButton
      write FOnCustomizeNewButton;
    property OnCustomizeReset: TNotifyEvent read FOnCustomizeReset write FOnCustomizeReset;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


procedure Register;



implementation



uses
{$IFDEF CLR}
  System.Threading, System.Security, System.Security.Permissions, System.IO,
  WinUtils,
{$ELSE}
  Winapi.ActiveX,
{$ENDIF}
  Vcl.Printers, Vcl.Consts, System.RTLConsts, Vcl.ComStrs, Vcl.ActnList, Vcl.StdActns, System.Types,
  Winapi.UxTheme, Winapi.DwmApi, System.StrUtils, Winapi.CommDlg;



{$IFDEF CLR}
var
  { HeaderControl messages }
  _HDM_INSERTITEM: Integer    = HDM_INSERTITEMW;
  _HDM_SETITEM: Integer       = HDM_SETITEMW;

  { StatusBar  messages }
  _SB_SETTEXT: Integer        = SB_SETTEXTW;

  { TabControl messages }
  _TCM_GETITEM: Integer       = TCM_GETITEMW;
  _TCM_SETITEM: Integer       = TCM_SETITEMW;
  _TCM_INSERTITEM: Integer    = TCM_INSERTITEMW;

  { ToolBar messages }
  _TB_SAVERESTORE: Integer    = TB_SAVERESTOREW;
  _TB_ADDSTRING: Integer      = TB_ADDSTRINGW;
  _TB_SETBUTTONINFO: Integer  = TB_SETBUTTONINFOW;
  _TB_INSERTBUTTON: Integer   = TB_INSERTBUTTONW;

  { ComboBoxEx messages }
  _CBEM_GETITEM: Integer      = CBEM_GETITEMW;
  _CBEM_INSERTITEM: Integer   = CBEM_INSERTITEMW;
  _CBEM_SETITEM: Integer      = CBEM_SETITEMW;

  {Animate messages }
  _ACM_OPEN: Integer          = ACM_OPENW;

  { CoolBar messages }
  _RB_INSERTBAND: Integer     = RB_INSERTBANDW;
  _RB_SETBANDINFO: Integer    = RB_SETBANDINFOW;
  _RB_GETBANDINFO: Integer    = RB_GETBANDINFOW;
{$ELSE}
const
  { HeaderControl messages }
  _HDM_INSERTITEM    = HDM_INSERTITEM;
  _HDM_SETITEM       = HDM_SETITEM;

  { StatusBar  messages }
  _SB_SETTEXT        = SB_SETTEXT;

  { TabControl messages }
  _TCM_GETITEM       = TCM_GETITEM;
  _TCM_SETITEM       = TCM_SETITEM;
  _TCM_INSERTITEM    = TCM_INSERTITEM;

  { ToolBar messages }
  _TB_SAVERESTORE    = TB_SAVERESTORE;
  _TB_ADDSTRING      = TB_ADDSTRING;
  _TB_SETBUTTONINFO  = TB_SETBUTTONINFO;
  _TB_INSERTBUTTON   = TB_INSERTBUTTON;

  { ComboBoxEx messages }
  _CBEM_GETITEM      = CBEM_GETITEM;
  _CBEM_INSERTITEM   = CBEM_INSERTITEM;
  _CBEM_SETITEM      = CBEM_SETITEM;

  {Animate messages }
  _ACM_OPEN          = ACM_OPEN;

  { CoolBar messages }
  _RB_INSERTBAND     = RB_INSERTBAND;
  _RB_SETBANDINFO    = RB_SETBANDINFO;
  _RB_GETBANDINFO    = RB_GETBANDINFO;
{$ENDIF}


{ TMyToolButton }

constructor TMyToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if StyleServices.Available then
    ControlStyle := [csSetCaption, csClickEvents]
  else
    ControlStyle := [csCaptureMouse, csSetCaption, csClickEvents];
  Width := 23;
  Height := 22;
  FImageIndex := -1;
  FStyle := tbsButton;
end;

procedure TMyToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Style = tbsDropDown) and (Button = mbLeft) then
    if Enabled or EnableDropdown then
    Down := not Down;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMyToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (Style = tbsDropDown) and MouseCapture then
    Down := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
end;

procedure TMyToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (X >= 0) and (X < ClientWidth) and (Y >= 0) and
    (Y <= ClientHeight) then
  if Style = tbsDropDown then Down := False;
end;

procedure TMyToolButton.Click;
begin
  inherited Click;
end;

procedure TMyToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = DropdownMenu then
      DropdownMenu := nil
    else if AComponent = MenuItem then
      MenuItem := nil;
  end;
end;

procedure TMyToolButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateControl;
  if not (csLoading in ComponentState) and (FToolBar <> nil) and
    (FToolBar.ShowCaptions or (FToolBar.AllowTextButtons and (FStyle = tbsTextButton))) then
  begin
    FToolBar.FButtonWidth := 0;
    FToolBar.FButtonHeight := 0;
    FToolBar.RecreateButtons;
  end;
end;

procedure TMyToolButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Pos: Integer;
  Reordered, NeedsUpdate: Boolean;
  ResizeWidth, ResizeHeight: Boolean;
begin
  if ((ALeft <> Left) or (ATop <> Top) or
    (AWidth <> Width) or (AHeight <> Height)) and
    (FUpdateCount = 0) and not (csLoading in ComponentState) and
    (FToolBar <> nil) then
  begin
    Pos := Index;
    Reordered := FToolBar.ReorderButton(Pos, ALeft, ATop) <> Pos;
    if Reordered then
    begin
      NeedsUpdate := False;
      if Index < Pos then Pos := Index
    end
    else
    begin
      NeedsUpdate := (Style in [tbsSeparator, tbsDivider]) and (AWidth <> Width);
      Reordered := NeedsUpdate;
    end;
    if (Style = tbsDropDown) and ((GetComCtlVersion >= ComCtlVersionIE4) or
      { IE3 doesn't display drop-down arrows }
      not FToolBar.Flat) then
        AWidth := FToolBar.ButtonWidth + AWidth - Width;
    ResizeWidth := not (Style in [tbsSeparator, tbsDivider]) and
      (AWidth <> FToolBar.ButtonWidth);
    ResizeHeight := AHeight <> FToolBar.ButtonHeight;
    if NeedsUpdate then inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    if csDesigning in ComponentState then
    begin
      if ResizeWidth then FToolBar.ButtonWidth := AWidth;
      if ResizeHeight then FToolBar.ButtonHeight := AHeight;
    end;
    if Reordered and not ResizeWidth and not ResizeHeight then
    begin
      if NeedsUpdate then
        if Style in [tbsSeparator, tbsDivider] then
          FToolBar.RefreshButton(Pos)
        else
          FToolBar.UpdateButton(Pos);
      FToolBar.ResizeButtons;
      FToolBar.RepositionButtons(0);
    end
    else
      FToolBar.RepositionButton(Pos);
  end
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

const
  cDropDownWidth = 14;

procedure TMyToolButton.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  if FToolBar = nil then
    Exit;

  if (Style = tbsDropDown) and not FToolbar.Flat and not FToolBar.FMenuDropped
      and (GetComCtlVersion = ComCtlVersionIE5) then
    with Canvas do
    begin
      if not Down then
      begin
        R := Rect(Width - cDropDownWidth, 1, Width, Height);
        DrawEdge(Handle, R, BDR_RAISEDOUTER, BF_TOP or BF_RIGHT or BF_BOTTOM);
        R.Top := 0;
        DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT);
      end
      else
      begin
        R := Rect(Width - cDropDownWidth + 1, -1, Width, Height);
        DrawEdge(Handle, R, BDR_SUNKEN, BF_TOP or BF_RIGHT or BF_BOTTOM);
        DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT);
      end;
    end;

  if Style = tbsDivider then
    with Canvas do
    begin
      R := Rect(Width div 2 - 1, 0, Width, Height);
      if StyleServices.Enabled then
      begin
        Details := StyleServices.GetElementDetails(ttbSeparatorNormal);
        StyleServices.DrawElement(Handle, Details, R);
      end
      else
        DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT)
    end;

  if csDesigning in ComponentState then
    { Draw separator outline }
    if Style in [tbsSeparator, tbsDivider] then
      with Canvas do
      begin
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Color := XorColor;
        Brush.Style := bsClear;
        Rectangle(0, 0, ClientWidth, ClientHeight);
      end
    else
      if (FToolBar.Flat or StyleServices.Enabled) and not Down then
      begin
        R := Rect(0, 0, Width, Height);
        if StyleServices.Enabled then
        begin
          Details := StyleServices.GetElementDetails(ttbButtonHot);
          StyleServices.DrawEdge(Canvas.Handle, Details, R, [eeRaisedInner], [efRect]);
        end
        else
          DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
      end;
end;

const
  ButtonStates: array[TToolButtonState] of Word = (TBSTATE_CHECKED,
    TBSTATE_PRESSED, TBSTATE_ENABLED, TBSTATE_HIDDEN, TBSTATE_INDETERMINATE,
    TBSTATE_WRAP, TBSTATE_ELLIPSES, TBSTATE_MARKED);

  ButtonStyles: array[TToolButtonStyle] of Word = (TBSTYLE_BUTTON, TBSTYLE_CHECK,
    TBSTYLE_DROPDOWN, TBSTYLE_SEP, TBSTYLE_SEP, BTNS_SHOWTEXT);

function TMyToolButton.GetButtonState: Byte;
begin
  Result := 0;
  if FDown then
    if Style = tbsCheck then
      Result := Result or ButtonStates[tbsChecked]
    else
      Result := Result or ButtonStates[tbsPressed];
  if (Enabled or FEnableDropdown) and ((FToolBar = nil) or FToolBar.Enabled) then
    Result := Result or ButtonStates[tbsEnabled];
  if not Visible and not (csDesigning in ComponentState) then
    Result := Result or ButtonStates[tbsHidden];
  if (FIndeterminate or FEnableDropdown) then
    Result := Result or ButtonStates[tbsIndeterminate];
  if FWrap then
    Result := Result or ButtonStates[tbsWrap];
  if FMarked then
    Result := Result or ButtonStates[tbsMarked];
end;

procedure TMyToolButton.SetAutoSize(Value: Boolean);
begin
  if Value <> AutoSize then
  begin
    FAutoSize := Value;
    UpdateControl;
    if not (csLoading in ComponentState) and (FToolBar <> nil) and
      (FToolBar.ShowCaptions or (FToolBar.AllowTextButtons and (FStyle = tbsTextButton))) then
    begin
      FToolBar.FButtonWidth := 0;
      FToolBar.FButtonHeight := 0;
      FToolBar.RecreateButtons;
    end;
  end;
end;

procedure TMyToolButton.SetButtonState(State: Byte);
begin
  FDown := State and (TBSTATE_CHECKED or TBSTATE_PRESSED) <> 0;
  Enabled := State and TBSTATE_ENABLED <> 0;
  if not (csDesigning in ComponentState) then
    Visible := State and TBSTATE_HIDDEN = 0;
  FIndeterminate := not FDown and (State and TBSTATE_INDETERMINATE <> 0);
  FWrap := State and TBSTATE_WRAP <> 0;
  FMarked := State and TBSTATE_MARKED <> 0;
end;

procedure TMyToolButton.SeTMyToolBar(AToolBar: TMyToolBar);
begin
  if FToolBar <> AToolBar then
  begin
    if FToolBar <> nil then
      FToolBar.RemoveButton(Self);
    Parent := AToolBar;
    if AToolBar <> nil then
      AToolBar.InsertButton(Self);
  end;
end;

procedure TMyToolButton.CMVisibleChanged(var Message: TMessage);
var
  Button: TTBButton;
begin
  if not (csDesigning in ComponentState) and (FToolBar <> nil) then
  begin
    with FToolBar do
    begin
      if not (csLoading in ComponentState) then
        HandleNeeded; // Changing visibility requires the toolbar to have a handle
      if Perform(TB_GETBUTTON, Index, Button) <> 0 then
        Perform(TB_HIDEBUTTON, Button.idCommand, MakeLong(Ord(not Self.Visible), 0));
      { Force a resize to occur }
      if AutoSize then
        AdjustSize;
    end;
    UpdateControl;
    FToolBar.RepositionButtons(Index);
  end;
end;

procedure TMyToolButton.CMEnabledChanged(var Message: TMessage);
begin
  if (FToolBar <> nil) and not FEnableDropdown then
    FToolBar.Perform(TB_ENABLEBUTTON, Index, LPARAM(Ord(Enabled)));
end;

procedure TMyToolButton.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := Ord(not (Style in [tbsDivider, tbsSeparator]) or (DragKind = dkDock));
end;

procedure TMyToolButton.SetDown(Value: Boolean);
const
  DownMessage: array[Boolean] of Integer = (TB_PRESSBUTTON, TB_CHECKBUTTON);
begin
  if Value <> FDown then
  begin
    FDown := Value;
    if FToolBar <> nil then
    begin
      FToolBar.Perform(DownMessage[Style = tbsCheck], Index, MakeLong(Ord(Value), 0));
      FToolBar.UpdateButtonStates;
    end;
  end;
end;

procedure TMyToolButton.SetDropdownMenu(Value: TPopupMenu);
begin
  if Value <> FDropdownMenu then
  begin
    FDropdownMenu := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TMyToolButton.SetGrouped(Value: Boolean);
begin
  if FGrouped <> Value then
  begin
    FGrouped := Value;
    UpdateControl;
  end;
end;

procedure TMyToolButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FToolBar <> nil then
    begin
      RefreshControl;
      FToolBar.Perform(TB_CHANGEBITMAP, Index, LPARAM(Value));
      if FToolBar.Transparent or FToolBar.Flat then Invalidate;
      if not (csLoading in ComponentState) and (Style = tbsTextButton) then
        FToolBar.RecreateButtons;
    end;
  end;
end;

procedure TMyToolButton.SetMarked(Value: Boolean);
begin
  if FMarked <> Value then
  begin
    FMarked := Value;
    if FToolBar <> nil then
      FToolBar.Perform(TB_MARKBUTTON, Index, LPARAM(Ord(Value)));
  end;
end;

procedure TMyToolButton.SetIndeterminate(Value: Boolean);
begin
  if FIndeterminate <> Value then
  begin
    if Value then SetDown(False);
    FIndeterminate := Value;
    if FToolBar <> nil then
      FToolBar.Perform(TB_INDETERMINATE, Index, LPARAM(Ord(Value)));
  end;
end;

procedure TMyToolButton.SetMenuItem(Value: TMenuItem);
begin
  { Copy all appropriate values from menu item }
  if Value <> nil then
  begin
    if FMenuItem <> Value then
      Value.FreeNotification(Self);
    Action := Value.Action;
    Caption := Value.Caption;
    Down := Value.Checked;
    Enabled := Value.Enabled;
    Hint := Value.Hint;
    ImageIndex := Value.ImageIndex;
    Visible := Value.Visible;
  end;
  FMenuItem := Value;
end;

procedure TMyToolButton.SetStyle(Value: TToolButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
    if not (csLoading in ComponentState) and (FToolBar <> nil) then
    begin
      if FToolBar.ShowCaptions or (FToolBar.AllowTextButtons and (FStyle = tbsTextButton)) then
      begin
        FToolBar.FButtonWidth := 0;
        FToolBar.FButtonHeight := 0;
        FToolBar.RecreateButtons
      end
      else
      begin
        if Style in [tbsDivider, tbsSeparator] then
          RefreshControl
        else
        if Style = tbsDropDown then
          FToolbar.RecreateButtons
        else
          UpdateControl;
        FToolBar.ResizeButtons;
        FToolbar.RepositionButtons(Index);
      end;
      FToolBar.AdjustSize;
    end;
  end;
end;

procedure TMyToolButton.SetWrap(Value: Boolean);
begin
  if FWrap <> Value then
  begin
    FWrap := Value;
    if FToolBar <> nil then
      RefreshControl;
  end;
end;

procedure TMyToolButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMyToolButton.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TMyToolButton.GetIndex: Integer;
begin
  if FToolBar <> nil then
    Result := FToolBar.FButtons.IndexOf(Self)
  else
    Result := -1;
end;

function TMyToolButton.IsWidthStored: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
end;

procedure TMyToolButton.RefreshControl;
begin
  if (FToolBar <> nil) and FToolBar.RefreshButton(Index) then
  begin

{    R := BoundsRect;
    R.Left := 0;
    ValidateRect(FToolBar.Handle, @R);
    R.Bottom := R.Top;
    R.Top := 0;
    R.Right := FToolBar.ClientWidth;
    ValidateRect(FToolBar.Handle, @R);}
  end;
end;

procedure TMyToolButton.UpdateControl;
begin
  if FToolBar <> nil then FToolBar.UpdateButton(Index);
end;

function TMyToolButton.CheckMenuDropdown: Boolean;
begin
  Result := not (csDesigning in ComponentState) and ((DropdownMenu <> nil) and
    DropdownMenu.AutoPopup or (MenuItem <> nil)) and (FToolBar <> nil) and
    FToolBar.CheckMenuDropdown(Self);
end;

function TMyToolButton.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TMyToolButtonActionLink(ActionLink).IsCheckedLinked;
end;

function TMyToolButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not TMyToolButtonActionLink(ActionLink).IsImageIndexLinked;
end;

procedure TMyToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or not Self.Down then
        Self.Down := Checked;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
  if Sender is TControlAction then
    with TControlAction(Sender) do
    begin
      if not CheckDefaults or (Self.PopupMenu = nil) then
        Self.PopupMenu := PopupMenu;
      if not CheckDefaults or (Self.DropdownMenu = nil) then
        Self.DropdownMenu := DropdownMenu;
      if not CheckDefaults or not Self.EnableDropdown then
        Self.EnableDropdown := EnableDropdown;
    end;
end;

function TMyToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TMyToolButtonActionLink;
end;

procedure TMyToolButton.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Checked := Self.Down;
      ImageIndex := Self.ImageIndex;
    end;
end;

procedure TMyToolButton.ValidateContainer(AComponent: TComponent);
var
  W: Integer;
begin
  inherited ValidateContainer(AComponent);
  { Update non-stored Width and Height if inserting into TMyToolBar }
  if (csLoading in ComponentState) and (AComponent is TMyToolBar) then
  begin
    if Style in [tbsDivider, tbsSeparator] then
      W := Width else
      W := TMyToolBar(AComponent).ButtonWidth;
    SetBounds(Left, Top, W, TMyToolBar(AComponent).ButtonHeight);
  end;
end;

procedure TMyToolButton.SetEnableDropdown(Value: Boolean);
begin
  if FEnableDropdown <> Value then
  begin
    FEnableDropdown := Value;
    if not Enabled and (FToolbar <> nil) then
      if FEnableDropdown then
        FToolBar.Perform(TB_ENABLEBUTTON, Index, LPARAM(Ord(True)))
      else
        FToolbar.Perform(TB_ENABLEBUTTON, Index, LPARAM(Ord(False)));
  end;
end;


{ TMyToolBar }


class constructor TMyToolBar.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TMyToolBar, TToolBarStyleHook);
end;

constructor TMyToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption, csGestures];
  Width := 150;
  Height := 29;
  Align := alTop;
  EdgeBorders := [];
  FButtonWidth := 23;
  FButtonHeight := 22;
  FCustomizable := False;
  FCustomizing := False;
  FNewStyle := True;
  FWrapable := True;
  FButtons := TList.Create;
{$IFDEF CLR}
  FButtonHashTable := HashTable.Create;
{$ENDIF}
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;
  FNullBitmap := TBitmap.Create;
  with FNullBitmap do
  begin
    Width := 1;
    Height := 1;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0,0,1,1));
  end;
  FloatingDockSiteClass := TToolDockForm;
  { The default value for Transparent now depends on if you have
    Themes turned on or off (this only works on XP) }
  FTransparent := StyleServices.Enabled;

  FBitmap := TBitmap.Create;
  GradientStartColor := clWindow;
  GradientEndColor := GetShadowColor(clBtnFace, -25);
  HotTrackColor := $00EFD3C6;
  Flat := True;
  GradientDrawingOptions := [gdoHotTrack, gdoGradient];
  GradientDirection := gdVertical;
  DrawingStyle := dsNormal;
  FLastQueryDeleteIndex := -1;
end;

destructor TMyToolBar.Destroy;
var
  I: Integer;
begin
  FBitmap.Free;
  FNullBitmap.Free;
  FHotImageChangeLink.Free;
  FDisabledImageChangeLink.Free;
  FImageChangeLink.Free;
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TMyToolButton then
      TMyToolButton(FButtons[I]).FToolBar := nil;
  FButtons.Free;
  FCanvas.Free;
{$IFDEF CLR}
  FButtonHashTable.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TMyToolBar.CreateParams(var Params: TCreateParams);
const
  DefaultStyles = CCS_NOPARENTALIGN or CCS_NOMOVEY or CCS_NORESIZE or CCS_NODIVIDER;
  ListStyles: array[Boolean] of DWORD = (0, TBSTYLE_LIST);
  FlatStyles: array[Boolean] of DWORD = (0, TBSTYLE_FLAT);
  FlatOnXp: array[Boolean] of DWORD = (0, TBSTYLE_FLAT);
  TransparentStyles: array[Boolean] of DWORD = (0, TBSTYLE_TRANSPARENT);
  CustomizeStyles: array[Boolean] of DWORD = (0, CCS_ADJUSTABLE);
begin
  FNewStyle := InitCommonControl(ICC_BAR_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, TOOLBARCLASSNAME);
  with Params do
  begin
    Style := Style or DefaultStyles or FlatStyles[FFlat] or ListStyles[FList] or
      TransparentStyles[FTransparent] or CustomizeStyles[FCustomizable] or
      FlatOnXp[StyleServices.Enabled];
    //! WINBUG: Without this style the toolbar has a two pixel margin above
    //! the buttons when ShowCaptions = True.
    if ShowCaptions or AllowTextButtons then
      Style := Style or TBSTYLE_TRANSPARENT;//!
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TMyToolBar.CreateWnd;
var
  DisplayDC: HDC;
  SaveFont, StockFont: HFONT;
  TxtMetric: TTextMetric;
  CurStyle: Cardinal;
begin
  inherited CreateWnd;
  { Maintain backward compatibility with IE3 which always draws drop-down arrows
    for buttons in which Style = tbsDropDown. }
  if GetComCtlVersion >= ComCtlVersionIE4 then
  begin
    CurStyle := Perform(TB_GETEXTENDEDSTYLE, 0, 0) or TBSTYLE_EX_DRAWDDARROWS;
    if GetComCtlVersion >= ComCtlVersionIE501 then
    begin
      if FHideClippedButtons then
        CurStyle := CurStyle or TBSTYLE_EX_HIDECLIPPEDBUTTONS
      else
        CurStyle := CurStyle and not TBSTYLE_EX_HIDECLIPPEDBUTTONS;
      if FAllowTextButtons and FList then
        CurStyle := CurStyle or TBSTYLE_EX_MIXEDBUTTONS
      else
        CurStyle := CurStyle and not TBSTYLE_EX_MIXEDBUTTONS;
    end;
    Perform(TB_SETEXTENDEDSTYLE, 0, LPARAM(CurStyle));
  end;

  FOldHandle := 0;
  StockFont := GetStockObject(SYSTEM_FONT);
  if StockFont <> 0 then
  begin
    DisplayDC := GetDC(0);
    if (DisplayDC <> 0) then
    begin
      SaveFont := SelectObject(DisplayDC, StockFont);
      if (GetTextMetrics(DisplayDC, TxtMetric)) then
        with TxtMetric do
          FHeightMargin := tmHeight - tmInternalLeading - tmExternalLeading + 1;
      SelectObject(DisplayDC, SaveFont);
      ReleaseDC(0, DisplayDC);
    end;
  end;
  RecreateButtons;
  Invalidate;
end;

procedure TMyToolBar.CreateButtons(NewWidth, NewHeight: Integer);

  function ToolButtonVisible: Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to FButtons.Count - 1 do
    begin
      Control := TControl(FButtons[I]);
      if (Control is TMyToolButton) and ((csDesigning in ComponentState) or
        Control.Visible) and not (TMyToolButton(Control).Style in
        [tbsSeparator, tbsDivider]) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  ImageWidth, ImageHeight: Integer;
  I: Integer;
begin
  BeginUpdate;
  try
    HandleNeeded;
{$IFDEF CLR}
    Perform(TB_BUTTONSTRUCTSIZE, Marshal.SizeOf(TypeOf(TTBButton)), 0);
{$ELSE}
    Perform(TB_BUTTONSTRUCTSIZE, SizeOf(TTBButton), 0);
{$ENDIF}
    Perform(TB_SETINDENT, FIndent, 0);
    if FImages <> nil then
    begin
      ImageWidth := FImages.Width;
      ImageHeight := FImages.Height;
    end
    else if FDisabledImages <> nil then
    begin
      ImageWidth := FDisabledImages.Width;
      ImageHeight := FDisabledImages.Height;
    end
    else if FHotImages <> nil then
    begin
      ImageWidth := FHotImages.Width;
      ImageHeight := FHotImages.Height;
    end
    else
    begin
      ImageWidth := 0;
      ImageHeight := 0;
    end;
    Perform(TB_SETBITMAPSIZE, 0, MakeLParam(ImageWidth, ImageHeight));

    { Adjust the working height if there is a visible TMyToolButton whose caption
      height is automatically added by the common control. }
//    if ShowCaptions and ToolButtonVisible then Dec(NewHeight, FHeightMargin);
    { Prevent toolbar from setting default button size }
    if NewWidth <= 0 then NewWidth := 1;
    if NewHeight <= 0 then NewHeight := 1;
    Perform(TB_SETBUTTONSIZE, 0, MakeLParam(NewWidth, NewHeight));
    FButtonWidth := NewWidth;
    FButtonHeight := NewHeight;
  finally
    EndUpdate;
  end;
  { Retrieve current button sizes }
  for I := 0 to InternalButtonCount - 1 do
    Perform(TB_DELETEBUTTON, 0, 0);
  UpdateButtons;
  UpdateImages;
  GetButtonSize(FButtonWidth, FButtonHeight);
end;

procedure TMyToolBar.RepositionButton(Index: Integer);
var
  TBButton: TTBButton;
  Button: TControl;
  R: TRect;
  AdjustY: Integer;
begin
  if (csLoading in ComponentState) or (Perform(TB_GETBUTTON, Index, TBButton) = 0) then
    Exit;

  // If a non ToolButton control's Visible doesn't match
  // the internal button's state it needs to be refreshed
{$IFDEF CLR}
  Button := TControl(FButtonHashTable.Item[TObject(TBButton.dwData)]);
{$ELSE}
  Button := TControl(TBButton.dwData);
{$ENDIF}
  if not (Button is TMyToolButton) then
    if Button.Visible <> ((TBButton.fsState and TBSTATE_HIDDEN) = 0) then
      RefreshButton(Index);

  if Perform(TB_GETITEMRECT, Index, R) <> 0 then
  begin
    if Button is TMyToolButton then
      TMyToolButton(Button).BeginUpdate;
    try
      if not (Button is TMyToolButton) then
        with Button do
        begin
          if Button is TWinControl then
            HandleNeeded;
          { Check for a control that doesn't size and center it }
          BoundsRect := R;
          if Height < R.Bottom - R.Top then
          begin
            AdjustY := (R.Bottom - R.Top - Height) div 2;
            SetBounds(R.Left, R.Top + AdjustY, R.Right - R.Left, Height);
          end;
        end
      else
        Button.BoundsRect := R;
    finally
      if Button is TMyToolButton then
        TMyToolButton(Button).EndUpdate;
    end;
  end;
end;

procedure TMyToolBar.RepositionButtons(Index: Integer);
var
  I: Integer;
begin
  if (csLoading in ComponentState) or (FUpdateCount > 0) then Exit;
  BeginUpdate;
  try
    for I := InternalButtonCount - 1 downto Index do RepositionButton(I);
  finally
    EndUpdate;
  end;
end;

procedure TMyToolBar.GetButtonSize(var AWidth, AHeight: Integer);
var
  LastIndex: Integer;
  R: TRect;
  TBButton: TTBButton;
begin
  if HandleAllocated then
  begin
    if GetComCtlVersion >= ComCtlVersionIE3 then
    begin
      LastIndex := Perform(TB_GETBUTTONSIZE, 0, 0);
      AHeight := LastIndex shr 16;
      AWidth := LastIndex and $FFFF;
    end
    else
    begin
      LastIndex := InternalButtonCount - 1;
      if LastIndex < 0 then Exit;
      while (LastIndex >= 0) and
        (Perform(TB_GETBUTTON, LastIndex, TBButton) <> 0) and
        (TBButton.fsStyle and TBSTYLE_SEP <> 0) do
        Dec(LastIndex);
      if LastIndex < 0 then
      begin
        if Perform(TB_GETITEMRECT, 0, R) <> 0 then
          AHeight := R.Bottom - R.Top;
        Exit;
      end;
      if Perform(TB_GETITEMRECT, LastIndex, R) <> 0 then
      begin
        AHeight := R.Bottom - R.Top;
        AWidth := R.Right - R.Left;
      end;
    end;
  end;
end;

procedure TMyToolBar.SetButtonHeight(Value: Integer);
begin
  if Value <> FButtonHeight then
  begin
    FButtonHeight := Value;
    if (StyleServices.Enabled = True) and Showing then
      RecreateWnd;
    RecreateButtons;
  end;
end;

procedure TMyToolBar.SetButtonWidth(Value: Integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    if (StyleServices.Enabled = True) and Showing then
      RecreateWnd;
    RecreateButtons;
  end;
end;

procedure TMyToolBar.InsertButton(Control: TControl);
var
  FromIndex, ToIndex: Integer;
begin
  if not (Control is TLabel) then
  begin
    if Control is TMyToolButton then TMyToolButton(Control).FToolBar := Self;
    if not (csLoading in Control.ComponentState) then
    begin
      FromIndex := FButtons.IndexOf(Control);
      if FromIndex >= 0 then
        ToIndex := ReorderButton(Fromindex, Control.Left, Control.Top)
      else
      begin
        ToIndex := ButtonIndex(FromIndex, Control.Left, Control.Top);
        FButtons.Insert(ToIndex, Control);
        UpdateItem(_TB_INSERTBUTTON, ToIndex, ToIndex);
      end;
    end
    else
    begin
      ToIndex := FButtons.Add(Control);
      UpdateButton(ToIndex);
    end;
    if Wrapable then
      RepositionButtons(0)
    else
      RepositionButtons(ToIndex);
    RecreateButtons;
  end;
end;

procedure TMyToolBar.RemoveButton(Control: TControl);
var
  I, Pos: Integer;
begin
  I := FButtons.IndexOf(Control);
  if I >= 0 then
  begin
    if Control is TMyToolButton then
      TMyToolButton(Control).FToolBar := nil;
    Pos := FButtons.Remove(Control);
    if FCustomizing and not FRestoring then
      Exit;
{$IFDEF CLR}
    FButtonHashTable.Remove(TObject(Control.GetHashCode));
{$ENDIF}
    Perform(TB_DELETEBUTTON, Pos, 0);
    ResizeButtons;
    if Wrapable then
      RepositionButtons(0)
    else
      RepositionButtons(Pos);
    RecreateButtons;
  end;
end;

function TMyToolBar.UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
const
  CImageNone = -2;
var
  Control: TControl;
  Button: TTBButton;
  CaptionText: string;
{$IFNDEF CLR}
  Len: Integer;
  Buffer: array [0..4095] of Char;
{$ENDIF}
begin
  Control := TControl(FButtons[FromIndex]);
  if Control is TMyToolButton then
    with TMyToolButton(Control) do
    begin
{$IFNDEF CLR}
      FillChar(Button, SizeOf(Button), 0);
{$ENDIF}
      if Style in [tbsSeparator, tbsDivider] then
      begin
        Button.iBitmap := Width;
        Button.idCommand := FromIndex;
      end
      else
      begin
        if ImageIndex < 0 then
          Button.iBitmap := CImageNone
        else
          Button.iBitmap := ImageIndex;
        Button.idCommand := FromIndex;
      end;
      with Button do
      begin
        fsStyle := ButtonStyles[Style];
        if AutoSize and (GetComCtlVersion >= ComCtlVersionIE4) then
          fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
      end;
      Button.fsState := GetButtonState;
      if FGrouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
{$IFDEF CLR}
      Button.dwData := Control.GetHashCode;
      // Add to hashtable to allow access to the actual Button
      // object from the TBButton struct
      if not FButtonHashTable.ContainsValue(Control) then
        FButtonHashTable.Add(TObject(Button.dwData), Control);
      if ShowCaptions or (AllowTextButtons and (Style = tbsTextButton)) then
      begin
        if Caption <> '' then
          CaptionText := Caption + #0#0
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' '#0#0;
        { TB_ADDSTRING requires two null terminators }
        Button.iString := Self.Perform(_TB_ADDSTRING, 0, CaptionText);
{$ELSE}
      Button.dwData := LPARAM(Control);
      if ShowCaptions or (AllowTextButtons and (Style = tbsTextButton)) then
      begin
        if Caption <> '' then
          CaptionText := Caption
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' ';
        StrPLCopy(Buffer, CaptionText, Length(Buffer));
        { TB_ADDSTRING requires two null terminators }
        Len := Length(CaptionText);
        if Len >= Length(Buffer) - 2 then
        begin
          Len := Length(Buffer) - 2;
          Buffer[Len] := #0;
        end;
        Buffer[Len + 1] := #0;
        Button.iString := Self.Perform(TB_ADDSTRING, 0, LPARAM(@Buffer));
{$ENDIF}
      end
      else
        Button.iString := -1;
    end
  else if not (Control is TLabel) then
  begin
{$IFNDEF CLR}
    FillChar(Button, SizeOf(Button), 0);
{$ENDIF}
    Button.fsStyle := ButtonStyles[tbsSeparator];
    Button.iBitmap := Control.Width;
    Button.idCommand := -1;
    if not Control.Visible and not (csDesigning in Control.ComponentState) then
      Button.fsState := Button.fsState or ButtonStates[tbsHidden];
{$IFDEF CLR}
    Button.dwData := Control.GetHashCode;
    // Add to hashtable to allow access to the actual Button
    // object from the TBButton struct
    if not FButtonHashTable.ContainsValue(Control) then
      FButtonHashTable.Add(TObject(Button.dwData), Control);
{$ELSE}
    Button.dwData := LPARAM(Control);
{$ENDIF}
    Button.iString := -1;
  end;
  Result := Self.Perform(Message, ToIndex, Button) <> 0;
  // If more than 2^16 strings are TB_ADDSTRING-ed to the tool bar's string
  // pool, the Windows API assumes iString is a pointer to a null terminated
  // string, not an index in the string pool.  Therefore we have to recreate
  // the toolbar to reset the string pool so the strings display propperly.
  if Button.iString >= 65536 then
    RecreateWnd;
end;

function TMyToolBar.UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
var
  Control: TControl;
  Button: TTBButtonInfo;
  CaptionText: string;
{$IFDEF CLR}
  Buffer: IntPtr;
{$ELSE}
  Len: Integer;
  Buffer: array [0..4095] of Char;
{$ENDIF}
begin
  try
    Control := TControl(FButtons[FromIndex]);
{$IFDEF CLR}
    Button.cbSize := Marshal.SizeOf(Button);
{$ELSE}
    FillChar(Button, SizeOf(Button), 0);
    Button.cbSize := SizeOf(Button);
{$ENDIF}
    if Control is TMyToolButton then
      with TMyToolButton(Control) do
      begin
        Button.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or TBIF_COMMAND
          or TBIF_SIZE;
        if Style in [tbsSeparator, tbsDivider] then
        begin
          Button.idCommand := FromIndex;
        end
        else
        begin
          Button.dwMask := Button.dwMask or TBIF_IMAGE;
          if ImageIndex < 0 then
            Button.iImage := -2 else
            Button.iImage := ImageIndex;
          Button.idCommand := FromIndex;
        end;
        with Button do
        begin
          cx := Width;
          fsStyle := ButtonStyles[Style];
          if AutoSize then fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
          if Grouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
        end;
        Button.fsState := GetButtonState;
{$IFDEF CLR}
        Button.lParam := Control.GetHashCode;
        if ShowCaptions or (AllowTextButtons and (Style = tbsTextButton)) then
        begin
          if Caption <> '' then
            CaptionText := Caption + #0#0
          else
            { Common control requries at least a space is used when showing button
              captions.  If any one button's caption is empty (-1) then none of
              the buttons' captions will not be displayed. }
            CaptionText := ' '#0#0;
          { TB_ADDSTRING requires two null terminators }
          Button.pszText := Marshal.StringToHGlobalAuto(CaptionText);
          Button.cchText := Length(CaptionText);
          Button.dwMask := Button.dwMask or TBIF_TEXT;
{$ELSE}
        Button.lParam := LPARAM(Control);
        if ShowCaptions or (AllowTextButtons and (Style = tbsTextButton)) then
        begin
          if Caption <> '' then
            CaptionText := Caption
          else
            { Common control requries at least a space is used when showing button
              captions.  If any one button's caption is empty (-1) then none of
              the buttons' captions will not be displayed. }
            CaptionText := ' ';

          StrPLCopy(Buffer, CaptionText, Length(Buffer));
          { TB_ADDSTRING requires two null terminators }
          Len := Length(CaptionText);
          if Len >= Length(Buffer) - 2 then
          begin
            Len := Length(Buffer) - 2;
            Buffer[Len] := #0;
          end;
          Buffer[Len + 1] := #0;
          //Button.iString := Self.Perform(TB_ADDSTRING, 0, Longint(@Buffer));
          Button.pszText := Buffer;
          Button.cchText := Length(CaptionText);
          Button.dwMask := Button.dwMask or TBIF_TEXT;
{$ENDIF}
        end
        else
        begin
          Button.pszText := nil;
          Button.cchText := 0;
        end;

        {if Style in [tbsSeparator, tbsDivider] then
        begin
          with Button do
          begin
            if Visible then
            begin
              dwMask := TBIF_STYLE or TBIF_STATE or TBIF_LPARAM;
              fsState := TBSTATE_ENABLED or TBSTATE_WRAP;
              fsStyle := TBSTYLE_BUTTON;
            end;
          end;
        end;}
      end
    else
    begin
      Button.dwMask := TBIF_TEXT or TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or
        TBIF_COMMAND or TBIF_SIZE;
      Button.fsStyle := ButtonStyles[tbsSeparator];
      Button.cx := Control.Width;
      Button.idCommand := -1;
      Button.pszText := nil;
      Button.cchText := 0;
{$IFDEF CLR}
      Button.lParam := Control.GetHashCode;
    end;
    Result := Self.Perform(Message, ToIndex, Button) <> 0;
  finally
    if Buffer <> nil then
      Marshal.FreeHGlobal(Buffer);
  end;
{$ELSE}
      Button.lParam := LPARAM(Control);
    end;
    Result := Self.Perform(Message, ToIndex, LPARAM(@Button)) <> 0;
  finally
  end;
{$ENDIF}
end;

function TMyToolBar.RefreshButton(Index: Integer): Boolean;
var
  Style: Longint;
begin
  if not (csLoading in ComponentState) and (FUpdateCount = 0) then
  begin
    BeginUpdate;
    try
      Style := GetWindowLong(Handle, GWL_STYLE);
      SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
      try
        Result := (Index < InternalButtonCount) and
          UpdateItem(TB_DELETEBUTTON, Index, Index) and
          UpdateItem(_TB_INSERTBUTTON, Index, Index);
      finally
        SetWindowLong(Handle, GWL_STYLE, Style);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    Result := False;
end;

procedure TMyToolBar.UpdateButton(Index: Integer);
var
  Style: Longint;
begin
  if (csLoading in ComponentState) or (FUpdateCount > 0) then Exit;
  BeginUpdate;
  try
    HandleNeeded;
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      if Index < InternalButtonCount then
        UpdateItem2(_TB_SETBUTTONINFO, Index, Index)
      else
        UpdateItem(_TB_INSERTBUTTON, Index, Index);
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TMyToolBar.UpdateButtons;
var
  I: Integer;
  Count: Integer;
  Style: Longint;
begin
  BeginUpdate;
  try
    HandleNeeded;
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      Count := InternalButtonCount;
      for I := 0 to FButtons.Count - 1 do
      begin
        if I < Count then
          UpdateItem2(_TB_SETBUTTONINFO, I, I)
        else
          UpdateItem(_TB_INSERTBUTTON, I, I);
      end;
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
    EndUpdate;
  end;
  RepositionButtons(0);
end;

procedure TMyToolBar.UpdateButtonState(Index: Integer);
var
  TBButton: TTBButton;
{$IFDEF CLR}
  ToolButton: TMyToolButton;
{$ENDIF}
begin
  if (Perform(TB_GETBUTTON, Index, TBButton) <> 0) then
  begin
{$IFDEF CLR}
    ToolButton := TMyToolButton(FButtonHashTable.Item[TObject(TBButton.dwData)]);
    with ToolButton do
{$ELSE}
    with TMyToolButton(TBButton.dwData) do
{$ENDIF}
    begin
      SetButtonState(TBButton.fsState);
      Self.Perform(TB_SETSTATE, Index, MakeLong(GetButtonState, 0));
    end;
  end;
end;

procedure TMyToolBar.UpdateButtonStates;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TMyToolButton then
      UpdateButtonState(I);
end;

procedure TMyToolBar.SetShowCaptions(Value: Boolean);
begin
  if FShowCaptions <> Value then
  begin
    FShowCaptions := Value;
    if not (csLoading in ComponentState) then
      RecreateWnd;
    AdjustSize;
  end;
end;

procedure TMyToolBar.SetAllowTextButtons(Value: Boolean);
begin
  if FAllowTextButtons <> Value then
  begin
    FAllowTextButtons := Value;
    if FAllowTextButtons then // Text buttons only allowed in list mode
      FList := True;
    if not (csLoading in ComponentState) then
      RecreateWnd;
    AdjustSize;
  end;
end;

function TMyToolBar.GetButton(Index: Integer): TMyToolButton;
begin
  Result := TMyToolButton(FButtons[Index]);
end;

function TMyToolBar.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

//function TMyToolBar.GetEnumerator: TMyToolBarEnumerator;
//begin
//  Result := TMyToolBarEnumerator.Create(Self);
//end;

function TMyToolBar.GetRowCount: Integer;
begin
  Result := Perform(TB_GETROWS, 0, 0);
end;

procedure TMyToolBar.SetList(Value: Boolean);
begin
  if FList <> Value then
  begin
    FList := Value;
    if not FList then // Text only buttons only allowed in list mode
      FAllowTextButtons := False;
    RecreateWnd;
  end;
end;

procedure TMyToolBar.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    RecreateWnd;
  end;
end;

procedure TMyToolBar.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    RecreateWnd;
  end;
  FTransparentSet := True;
end;

procedure TMyToolBar.SetWrapable(Value: Boolean);
begin
  if FWrapable <> Value then
  begin
    FWrapable := Value;
    if AutoSize then
      AdjustSize;
    if (gdoGradient in GradientDrawingOptions) and HandleAllocated then
      Repaint;
  end;
end;

procedure TMyToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then Images := nil;
    if AComponent = FHotImages then HotImages := nil;
    if AComponent = FDisabledImages then DisabledImages := nil;
    if AComponent = FMenu then Menu := nil;
  end;
end;

procedure TMyToolBar.LoadImages(AImages: TCustomImageList);
var
  AddBitmap: TTBAddBitmap;
  ReplaceBitmap: TTBReplaceBitmap;
  NewHandle: HBITMAP;

  function GetImageBitmap(ImageList: TCustomImageList): HBITMAP;
  var
    I: Integer;
    Bitmap: TBitmap;
    R: TRect;
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := ImageList.Width * ImageList.Count;
      Bitmap.Height := ImageList.Height;
      R := Rect(0,0,Width,Height);
      with Bitmap.Canvas do
      begin
        Brush.Color := clBtnFace;
        FillRect(R);
      end;
      for I := 0 to ImageList.Count - 1 do
        ImageList_Draw(ImageList.Handle, I, Bitmap.Canvas.Handle,
          I * ImageList.Width, 0, ILD_TRANSPARENT);
      Result := Bitmap.ReleaseHandle;
    finally
      Bitmap.Free;
    end;
  end;

begin
  if AImages <> nil then
    NewHandle := GetImageBitmap(AImages)
  else
    with TBitmap.Create do
    try
      Assign(FNullBitmap);
      NewHandle := ReleaseHandle;
    finally
      Free;
    end;
  if FOldHandle = 0 then
  begin
    AddBitmap.hInst := 0;
    AddBitmap.nID := NewHandle;
{$IFDEF CLR}
    Perform(TB_ADDBITMAP, ButtonCount, AddBitmap);
{$ELSE}
    Perform(TB_ADDBITMAP, ButtonCount, LPARAM(@AddBitmap));
{$ENDIF}
  end
  else
  begin
    with ReplaceBitmap do
    begin
      hInstOld := 0;
      nIDOld := FOldHandle;
      hInstNew := 0;
      nIDNew := NewHandle;
      nButtons := ButtonCount;
    end;
{$IFDEF CLR}
    Perform(TB_REPLACEBITMAP, 0, ReplaceBitmap);
{$ELSE}
    Perform(TB_REPLACEBITMAP, 0, LPARAM(@ReplaceBitmap));
{$ENDIF}
    if FOldHandle <> 0 then DeleteObject(FOldHandle);
  end;
  FOldHandle := NewHandle;
end;

procedure TMyToolBar.UpdateImages;
begin
  if FNewStyle then
  begin
    if FImages <> nil then SetImageList(FImages.Handle);
    if FDisabledImages <> nil then SetDisabledImageList(FDisabledImages.Handle);
    if FHotImages <> nil then SetHotImageList(FHotImages.Handle);
  end
  else
    if HandleAllocated then LoadImages(FImages);
end;

procedure TMyToolBar.ImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = Images) then RecreateButtons;
end;

procedure TMyToolBar.SetImageList(Value: HImageList);
begin
  if HandleAllocated then Perform(TB_SETIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TMyToolBar.SetImages(Value: TCustomImageList);

  function HasDropDownButton: Boolean;
  var
    Button: TMyToolButton;
    i: Integer;
  begin
    Result := False;
    for i := 0 to ButtonCount - 1 do
    begin
      Button := Buttons[i];
      if Button.Style = tbsDropDown then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end
  else
    SetImageList(0);

  // Work around issue with a manifested ToolBars increasing in size
  // when a drop down button is present and tool buttons are added/deleted
  if StyleServices.Enabled and HandleAllocated and HasDropDownButton then
    RecreateWnd
  else
    RecreateButtons;
end;

procedure TMyToolBar.DisabledImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = DisabledImages) then RecreateButtons;
end;

procedure TMyToolBar.SetDisabledImageList(Value: HImageList);
begin
  if HandleAllocated then
    Perform(TB_SETDISABLEDIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TMyToolBar.SetDisabledImages(Value: TCustomImageList);
begin
  if FDisabledImages <> nil then FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if FDisabledImages <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end
  else
    SetDisabledImageList(0);
  RecreateButtons;
end;

procedure TMyToolBar.HotImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = HotImages) then RecreateButtons;
end;

procedure TMyToolBar.SetHotImageList(Value: HImageList);
begin
  if HandleAllocated then
    Perform(TB_SETHOTIMAGELIST, 0, Value);
  Invalidate;
end;

procedure TMyToolBar.SetHotImages(Value: TCustomImageList);
begin
  if FHotImages <> nil then FHotImages.UnRegisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if FHotImages <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end
  else
    SetHotImageList(0);
  RecreateButtons;
end;

procedure TMyToolBar.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    RecreateWnd;
  end;
end;

procedure TMyToolBar.SetMenu(const Value: TMainMenu);
var
  I: Integer;
begin
  if FMenu = Value then exit;
  if csAcceptsControls in ControlStyle then
  begin
    ControlStyle := [csCaptureMouse, csClickEvents,
      csDoubleClicks, csMenuEvents, csSetCaption, csGestures];
    RecreateWnd;
  end;
  ShowCaptions := True;
  if Assigned(FMenu) then
    for I := ButtonCount - 1 downto 0 do
      Buttons[I].Free;

  if Assigned(FMenu) then
    FMenu.RemoveFreeNotification(Self);
  FMenu := Value;
  if not Assigned(FMenu) then exit;
  FMenu.FreeNotification(Self);

  for I := ButtonCount to FMenu.Items.Count - 1 do
  begin
    with TMyToolButton.Create(Self) do
    try
      AutoSize := True;
      Grouped := True;
      Parent := Self;
      Buttons[I].MenuItem := FMenu.Items[I];
    except
      Free;
      raise;
    end;
  end;
  { Copy attributes from each menu item }
  for I := 0 to FMenu.Items.Count - 1 do
    Buttons[i].MenuItem := FMenu.Items[I];
end;

procedure TMyToolBar.RecreateButtons;
begin
  if ([csLoading, csDestroying] * ComponentState = []) or HandleAllocated then
  begin
    CreateButtons(FButtonWidth, FButtonHeight);
    ResizeButtons;
  end;
end;

procedure TMyToolBar.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if FInMenuLoop and FCaptureChangeCancels then CancelMenu;
end;

procedure TMyToolBar.WMKeyDown(var Message: TWMKeyDown);
var
  Item: Integer;
  Button: TMyToolButton;
  P: TPoint;
begin
  if FInMenuLoop then
  begin
    Item := Perform(TB_GETHOTITEM, 0, 0);
    case Message.CharCode of
      VK_RETURN, VK_DOWN:
        begin
          if (Item > -1) and (Item < FButtons.Count) then
          begin
            Button := TMyToolButton(FButtons[Item]);
            P := Button.ClientToScreen(Point(1, 1));
            ClickButton(Button);
          end;
          { Prevent default processing }
          if Message.CharCode = VK_DOWN then Exit;
        end;
      VK_ESCAPE: CancelMenu;
    end;
  end;
  inherited;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TMyToolBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  if Assigned(Menu) then exit;
  for I := 0 to FButtons.Count - 1 do
    Proc(TComponent(FButtons[I]));
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FButtons.IndexOf(Control) = -1) then
      Proc(Control);
  end;
end;

procedure TMyToolBar.Loaded;
var
  I: Integer;
begin
  RecreateButtons;
  { Make sure we dock controls after streaming }
  for I := 0 to ControlCount - 1 do
    Controls[I].HostDockSite := Self;
  inherited Loaded;
  ResizeButtons;
  RepositionButtons(0);
end;

procedure TMyToolBar.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMyToolBar.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TMyToolBar.ResizeButtons;
begin
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    Perform(TB_AUTOSIZE, 0, 0);
    if AutoSize then AdjustSize;
  end;
end;

function TMyToolBar.InternalButtonCount: Integer;
begin
  Result := Perform(TB_BUTTONCOUNT, 0, 0);
end;

function TMyToolBar.ButtonIndex(OldIndex, ALeft, ATop: Integer): Integer;
var
  Dist, Tmp, Head, Tail: Integer;
  Control: TControl;
begin
  if (OldIndex >= 0) and (FButtons.Count <= 1) then
  begin
    Result := OldIndex;
    Exit;
  end;
  { Find row closest to ATop }
  Result := 0;
  if FButtons.Count = 0 then Exit;
  Tmp := 0;
  Head := 0;
  Tail := 0;
  Dist := MaxInt;
  while (Dist > 0) and (Result < FButtons.Count) do
  begin
    if Result <> OldIndex then
    begin
      Control := TControl(FButtons[Result]);
      if (Control is TMyToolButton) and TMyToolButton(Control).Wrap or
        (Result = FButtons.Count - 1) then
      begin
        if Abs(ATop - Control.Top) < Dist then
        begin
          Dist := Abs(ATop - Control.Top);
          Head := Tmp;
          Tail := Result;
        end;
        Tmp := Result + 1;
      end;
    end
    else
      Tail := Result;
    Inc(Result);
  end;
  { Find button on Row closest to ALeft }
  for Result := Head to Tail do
    if (Result <> OldIndex) and (ALeft <= TControl(FButtons[Result]).Left) then
      Break;
  { Return old position if new position is last on the row and old position
    was already the last on the row. }
  if (Result = OldIndex + 1) and (OldIndex in [Head..Tail]) then
    Result := OldIndex;
end;

function TMyToolBar.ReorderButton(OldIndex, ALeft, ATop: Integer): Integer;
var
  Control: TControl;
begin
  Result := ButtonIndex(OldIndex, ALeft, ATop);
  if Result <> OldIndex then
  begin
    { If we are inserting to the right of our deletion then account for shift }
    if OldIndex < Result then Dec(Result);
    Control := TControl(FButtons[OldIndex]);
    FButtons.Delete(OldIndex);
    FButtons.Insert(Result, Control);
    BeginUpdate;
    try
{$IFDEF CLR}
      FButtonHashTable.Remove(TObject(Control.GetHashCode));
{$ENDIF}
      Perform(TB_DELETEBUTTON, OldIndex, 0);
      UpdateItem(_TB_INSERTBUTTON, Result, Result);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMyToolBar.AdjustControl(Control: TControl);
var
  I, Pos: Integer;
  R: TRect;
  Reordered, NeedsUpdate: Boolean;
begin
  Pos := FButtons.IndexOf(Control);
  if Pos = -1 then
    Exit;
  Reordered := ReorderButton(Pos, Control.Left, Control.Top) <> Pos;
  NeedsUpdate := False;
  if Reordered then
  begin
    I := FButtons.IndexOf(Control);
    if I < Pos then Pos := I;
  end
  else
    if Perform(TB_GETITEMRECT, Pos, R) <> 0 then
    begin
      NeedsUpdate := Control.Width <> R.Right - R.Left;
      Reordered := NeedsUpdate;
    end;
  if (csDesigning in ComponentState) and (Control.Height <> ButtonHeight) then
    ButtonHeight := Control.Height
  else
    if Reordered then
    begin
      if NeedsUpdate then
        RefreshButton(Pos);
      ResizeButtons;
      RepositionButtons(0);
    end
    else
      RepositionButton(Pos);
end;

procedure TMyToolBar.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if FUpdateCount > 0 then Exit;
  if AControl = nil then
    RepositionButtons(0)
  else
  begin
    {if (AControl is TLabel) then
      inherited
    else }if not (AControl is TMyToolButton) then
      AdjustControl(AControl);
  end;
end;

procedure TMyToolBar.ChangeScale(M, D: Integer);
begin
  { Scaling isn't a standard behavior for toolbars.  We prevent scaling from
    occurring here. }
end;

procedure TMyToolBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not Transparent then
    inherited
  else
    DefaultHandler(Message);
end;

procedure TMyToolBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if FInMenuLoop then
    Message.Result := DLGC_WANTARROWS;
end;

{ Need to read/write caption ourselves - default wndproc seems to discard it. }

procedure TMyToolBar.WMGetText(var Message: TWMGetText);
{$IFDEF CLR}
var
  S: string;
begin
  with Message do
  begin
    S := FCaption;
    if Length(FCaption) > TextMax - 1 then
      SetLength(S, TextMax - 1);
    Text := S;
    Result := Length(S);
  end;
{$ELSE}
begin
  with Message do
    Result := StrLen(StrLCopy(PChar(Text), PChar(FCaption), TextMax - 1));
{$ENDIF}
end;

procedure TMyToolBar.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FCaption);
end;

procedure TMyToolBar.WMSetText(var Message: TWMSetText);
begin
{$IFDEF CLR}
  FCaption := Message.Text;
{$ELSE}
  with Message do
    SetString(FCaption, Text, StrLen(Text));
{$ENDIF}
end;

procedure TMyToolBar.WMNotifyFormat(var Message: TWMNotifyFormat);
begin
  with Message do
    Result := DefWindowProc(Handle, Msg, From, Command);
end;

procedure TMyToolBar.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  if not AutoSize then
  begin
    W := Width;
    H := Height;
    WrapButtons(W, H);
  end;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TMyToolBar.WMSysChar(var Message: TWMSysChar);
var
  Form: TCustomForm;
begin
  { Default wndproc doesn't re-route WM_SYSCHAR messages to parent. }
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Form.Dispatch(Message);
    Exit;
  end
  else
    inherited;
end;

procedure TMyToolBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  R: TRect;
  NcX, NcY: Integer;
  Rgn1, Rgn2: HRgn;
begin
  { Erase only what's been uncovered when toolbar is flat - avoid flicker }
  if Flat and HandleAllocated and (Parent <> nil) then
  begin
    GetWindowRect(Handle, R);
    NcX := R.Right - R.Left - ClientWidth;
    NcY := R.Bottom - R.Top - ClientHeight;
    Rgn1 := CreateRectRgn(0, 0, Width - NcX, Height - NcY);
    with Message.WindowPos{$IFNDEF CLR}^{$ENDIF} do
      Rgn2 := CreateRectRgn(0, 0, cx - NcY, cy - NcY);
    CombineRgn(Rgn1, Rgn2, Rgn1, RGN_XOR);
    GetRgnBox(Rgn1, R);
    { Allow a 2 pixel buffer }
    Dec(R.Left, 2);
    DeleteObject(Rgn1);
    DeleteObject(Rgn2);
    inherited;
    RedrawWindow(Handle, R, 0, RDW_INVALIDATE or RDW_ERASE);
  end
  else
    inherited;
end;

procedure TMyToolBar.WMWindowPosChanging(var Message: TWMWindowPosChanging);
const
  BackgroundValid = SWP_NOSIZE or SWP_NOMOVE;
var
  R: TRect;
begin
  { Invalidate old background when toolbar is flat and is about to be moved }
  if Transparent and (Message.WindowPos.flags and BackgroundValid <> BackgroundValid) and
    (Parent <> nil) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect(Parent.Handle, R, True);
  end;
  inherited;
end;

function TMyToolBar.WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
var
  Index, NcX, NcY: Integer;
  Vertical: Boolean;
  PrevSize, CurrSize: TPoint;
  R: TRect;
  WrapStates: TBits;

  procedure CalcSize(var CX, CY: Integer);
  var
    IsWrapped: Boolean;
    I, Tmp, X, Y, HeightChange, LWidth: Integer;
    Control: TControl;
    LRect: TRect;
  begin
    CX := 0;
    CY := 0;
    X := Indent;
    Y := 0;
    for I := 0 to FButtons.Count - 1 do
    begin
      Control := TControl(FButtons[I]);
      LWidth := Control.Width;
      if (csDesigning in ComponentState) or Control.Visible then
      begin
        if (Control is TMyToolButton) and (I < FButtons.Count - 1) then
          if WrapStates <> nil then
            IsWrapped := WrapStates[I]
          else
            IsWrapped := TMyToolButton(Control).Wrap
        else
          IsWrapped := False;
        if Control is TMyToolButton and
          (TMyToolButton(Control).Style in [tbsSeparator, tbsDivider]) then
        begin
          { Store the change in height, from the current row to the next row
            after wrapping, in HeightChange. THe IE4 version of comctl32
            considers this height to be the width the last separator on the
            current row - prior versions of comctl32 consider this height to be
            2/3 the width the last separator. }
          HeightChange := LWidth;
          if (GetComCtlVersion < ComCtlVersionIE4) or not Flat and
            (GetComCtlVersion >= ComCtlVersionIE401) then
            HeightChange := HeightChange * 2 div 3;
          if IsWrapped and (I < FButtons.Count - 1) then
          begin
            Tmp := Y + ButtonHeight + HeightChange;
            if Tmp > CY then
              CY := Tmp;
          end
          else
          begin
            Tmp := X + LWidth;
            if Tmp > CX then
              CX := Tmp;
          end;
          if IsWrapped then
            Inc(Y, HeightChange);
        end
        else
        begin
          if (Control is TMyToolButton) and
             (TMyToolButton(Control).Style in [tbsDropDown]) then
            if Perform(TB_GETRECT, I, LRect) <> 0 then
              LWidth := LRect.Right - LRect.Left
            else
              Inc(LWidth, 13);

          Tmp := X + LWidth;
          if Tmp > CX then
            CX := Tmp;
          Tmp := Y + ButtonHeight;
          if Tmp > CY then
            CY := Tmp;
        end;
        if IsWrapped then
        begin
          X := Indent;
          Inc(Y, ButtonHeight);
        end
        else
          Inc(X, LWidth);
      end;
    end;
    { Adjust for 2 pixel top margin when not flat style buttons }
    if (CY > 0) and not Flat then
      Inc(CY, 2);
  end;

  function WrapHorz(CX: Integer): Integer;
  var
    I, J, X: Integer;
    Control: TControl;
    Found: Boolean;
  begin
    Result := 1;
    X := Indent;
    I := 0;
    while I < FButtons.Count do
    begin
      Control := TControl(FButtons[I]);
      if Control is TMyToolButton then
        WrapStates[I] := False;
      if (csDesigning in ComponentState) or Control.Visible then
      begin
        if (X + Control.Width > CX) and (not (Control is TMyToolButton) or
          not (TMyToolButton(Control).Style in [tbsDivider, tbsSeparator])) then
        begin
          Found := False;
          for J := I downto 0 do
            if TControl(FButtons[J]) is TMyToolButton then
              with TMyToolButton(FButtons[J]) do
                if ((csDesigning in ComponentState) or Visible) and
                  (Style in [tbsSeparator, tbsDivider]) then
                begin
                  if not WrapStates[J] then
                  begin
                    Found := True;
                    I := J;
                    X := Indent;
                    WrapStates[J] := True;
                    Inc(Result);
                  end;
                  Break;
                end;
          if not Found then
          begin
            for J := I - 1 downto 0 do
              if TControl(FButtons[J]) is TMyToolButton then
                with TMyToolButton(FButtons[J]) do
                  if (csDesigning in ComponentState) or Visible then
                  begin
                    if not WrapStates[J] then
                    begin
                      Found := True;
                      I := J;
                      X := Indent;
                      WrapStates[J] := True;
                      Inc(Result);
                    end;
                    Break;
                  end;
            if not Found then
              Inc(X, Control.Width);
          end;
        end
        else
          Inc(X, Control.Width);
      end;
      Inc(I);
    end;
  end;

  function WrapSizeVert(var CX, CY: Integer): Integer;
  var
    HorzSize, VertSize, Size, PrevSize: TPoint;
  begin
    PrevSize := Point(-1,-1);
    Size := Point(0,0);
    Result := 0;
    WrapHorz(0);
    CalcSize(VertSize.X, VertSize.Y);
    WrapHorz(MaxInt);
    CalcSize(HorzSize.X, HorzSize.Y);
    while VertSize.X < HorzSize.X do
    begin
      PrevSize := Size;
      Size.X := (VertSize.X + HorzSize.X) div 2;
      Result := WrapHorz(Size.X);
      CalcSize(Size.X, Size.Y);
      if CY < Size.Y then
      begin
        if (VertSize.X = Size.X) and (VertSize.Y = Size.Y) then
        begin
          Result := WrapHorz(HorzSize.X);
          Break;
        end;
        VertSize := Size;
      end
      else if CY > Size.Y then
      begin
        HorzSize := Size;
        if (PrevSize.X = Size.X) and (PrevSize.Y = Size.Y) then Break;
      end
      else
        Break;
    end;
  end;

  function WrapSizeHorz(var CX, CY: Integer): Integer;
  var
    HorzRows, VertRows, Min, Mid, Max: Integer;
    HorzSize: TPoint;
  begin
    Result := 0;
    Min := 0;
    Max := CX;
    HorzRows := WrapHorz(Max);
    VertRows := WrapHorz(0);
    if HorzRows <> VertRows then
      while Min < Max do
      begin
        Mid := (Min + Max) div 2;
        VertRows := WrapHorz(Mid);
        if VertRows = HorzRows then
          Max := Mid
        else
        begin
          if Min = Mid then
          begin
            WrapHorz(Max);
            Break;
          end;
          Min := Mid;
        end;
      end;
    CalcSize(HorzSize.X, HorzSize.Y);
    WrapHorz(HorzSize.X);
  end;

begin
  Result := True;
  if HandleAllocated then
  begin
    Index := InternalButtonCount - 1;
    if (Index >= 0) or not (csDesigning in ComponentState) then
    begin
      WrapStates := nil;
      PrevSize.X := ClientWidth;
      PrevSize.Y := ClientHeight;
      { Calculate non-client border size }
      NcX := Width - PrevSize.X;
      NcY := Height - PrevSize.Y;
      { Remember previous size for comparison }
      R.BottomRight := PrevSize;
      CalcSize(PrevSize.X, PrevSize.Y);
      { Get current window size minus the non-client borders }
      CurrSize := Point(NewWidth - NcX, NewHeight - NcY);

      { Decide best way to calculate layout }
      if Align <> alNone then
        Vertical := Align in [alLeft, alRight]
      else
        Vertical := Abs(CurrSize.X - R.Right) < Abs(CurrSize.Y - R.Bottom);
      if Wrapable then
      begin
        WrapStates := TBits.Create;
        try
          WrapStates.Size := FButtons.Count;
          if Vertical then
            WrapSizeVert(CurrSize.X, CurrSize.Y)
          else
            WrapSizeHorz(CurrSize.X, CurrSize.Y);
          { CurrSize now has optimium dimensions }
          CalcSize(CurrSize.X, CurrSize.Y);
          if (Vertical or (Align = alNone)) and (CurrSize.X <> PrevSize.X) or
            (CurrSize.Y <> PrevSize.Y) then
          begin
            { Enforce changes to Wrap property }
            for Index := 0 to WrapStates.Size - 1 do
              if TControl(FButtons[Index]) is TMyToolButton then
                TMyToolButton(FButtons[Index]).Wrap := WrapStates[Index];
            RepositionButtons(0);
          end
          else
            { Overwrite any changes to buttons' Wrap property }
            UpdateButtonStates;
        finally
          WrapStates.Free;
        end;
      end
      else
        { CurrSize now has optimium dimensions }
        CalcSize(CurrSize.X, CurrSize.Y);
      if AutoSize and (Align <> alClient) then
      begin
        if Vertical or (Align = alNone) then
          NewWidth := CurrSize.X + NcX;
        if not Vertical or (Align = alNone) then
          NewHeight := CurrSize.Y + NcY;
      end;
    end;
  end;
end;

function TMyToolBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := WrapButtons(NewWidth, NewHeight);
end;

{$IFDEF CLR}
procedure TMyToolBar.ControlChange(Inserting: Boolean; Child: TControl);
begin
  inherited;
  if Inserting then
    InsertButton(Child)
  else
    RemoveButton(Child);
end;
{$ELSE}
procedure TMyToolBar.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
    if Inserting then
      InsertButton(Control)
    else
      RemoveButton(Control);
end;
{$ENDIF}

procedure TMyToolBar.CNChar(var Message: TWMChar);
begin
  { We got here through the installed ToolMenuKeyHook }
  if FInMenuLoop and not (csDesigning in ComponentState) then
    with Message do
      if Perform(CM_DIALOGCHAR, CharCode, KeyData) <> 0 then
        Result := 1;
end;

procedure TMyToolBar.CMDialogChar(var Message: TCMDialogChar);

  function ContainsActiveControl: Boolean;
  var
    F: TCustomForm;
  begin
    F := GetParentForm(Self, False);
    if (F <> nil) and (Screen.ActiveControl <> nil) then
      Result := (F = Screen.ActiveControl) or F.ContainsControl(Screen.ActiveControl)
    else
      Result := False;
  end;

var
  Button: TMyToolButton;
begin
  if Enabled and Showing and (ShowCaptions or AllowTextButtons) and ContainsActiveControl then
  begin
    Button := FindButtonFromAccel(Message.CharCode);
    if (Button <> nil) and (not AllowTextButtons or (Button.Style = tbsTextButton)) then
    begin
      { Display a drop-down menu after hitting the accelerator key if IE3
        is installed. Otherwise, fire the OnClick event for IE4. We do this
        because the IE4 version of the drop-down metaphor is more complete,
        allowing the user to click a button OR drop-down its menu. }
      if ((Button.Style <> tbsDropDown) or (GetComCtlVersion < ComCtlVersionIE4)) and
        ((Button.DropdownMenu <> nil) or (Button.MenuItem <> nil)) then
        TrackMenu(Button)
      else
        Button.Click;
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TMyToolBar.CMDockNotification(var Message: TCMDockNotification);
begin
  inherited;
  // Ensure that any dockhost invalidation is properly relayed on to
  // outer dock hosts.
  if (Message.NotifyRec.ClientMsg = CM_INVALIDATEDOCKHOST) then
    InvalidateDockHostSite(Boolean(Message.NotifyRec.MsgLParam));
end;

procedure TMyToolBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Broadcast(Message);
end;

procedure TMyToolBar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TMyToolBar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  { If toolbar is transparent then repaint when parent changes color }
  if Transparent then Invalidate;
end;

procedure TMyToolBar.CNSysKeyDown(var Message: TWMSysKeyDown);
begin
  inherited;
  if (Message.CharCode = VK_MENU) then
    CancelMenu;
end;

procedure TMyToolBar.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TMyToolBar.CNDropDownClosed(var Message: TMessage);
begin
  ClearTempMenu;
  FMenuDropped := False;
  if (GetComCtlVersion = ComCtlVersionIE5) and (FMenuButton <> nil)
    then FMenuButton.Invalidate;
  FCaptureChangeCancels := True;
end;

procedure TMyToolBar.CNNotify(var Message: TWMNotifyTLB);
var
  Button: TMyToolButton;
  DefaultDraw: Boolean;
  CustomDrawn: Boolean;
  R: TRect;
  Flags: TTBCustomDrawFlags;
  LogFont: TLogFont;
{$IFDEF CLR}
  NMTB: TNMToolBar;
  TBCustomDraw: TNMTBCustomDraw;
{$ELSE}
  NMTB: PNMToolBar;
  TBCustomDraw: PNMTBCustomDraw;
{$ENDIF}
begin
  with Message do
    case NMHdr.code of
      TBN_DROPDOWN:
        begin
          NMTB := NMToolBar;
          with NMTB{$IFNDEF CLR}^{$ENDIF} do
          begin
            { We can safely assume that a TBN_DROPDOWN message was generated by a
              TMyToolButton and not any TControl. }
            if Perform(TB_GETBUTTON, iItem, tbButton) <> 0 then
            begin
{$IFDEF CLR}
              Button := TMyToolButton(FButtonHashTable.Item[TObject(tbButton.dwData)]);
{$ELSE}
              Button := TMyToolButton(tbButton.dwData);
{$ENDIF}
              if Button <> nil then
                Button.CheckMenuDropDown;
            end;
          end;
{$IFDEF CLR}
          NMToolBar := NMTB;
{$ENDIF}
        end;
      NM_CUSTOMDRAW:
        begin
          TBCustomDraw := NMTBCustomDraw;
          with TBCustomDraw{$IFNDEF CLR}^{$ENDIF} do
          try
            FCanvas.Lock;
            Result := CDRF_DODEFAULT;
            if (nmcd.dwDrawStage and CDDS_ITEM) = 0 then
            begin
              R := ClientRect;
              case nmcd.dwDrawStage of
                CDDS_PREPAINT:
                  begin
                    CustomDrawn := IsCustomDrawn(dtControl, cdPrePaint);
                    if CustomDrawn or (DrawingStyle = dsGradient) then
                    begin
                      try
                        FCanvas.Handle := nmcd.hdc;
                        FCanvas.Font := Font;
                        FCanvas.Brush := Brush;
                        if CustomDrawn then
                          DefaultDraw := CustomDraw(R, cdPrePaint)
                        else
                          DefaultDraw := GradientDrawToolBar(R);
                        if not DefaultDraw then
                        begin
                          Result := CDRF_SKIPDEFAULT;
                          Exit;
                        end;
                        clrText := ColorToRGB(FCanvas.Font.Color);
                        clrBtnFace := ColorToRGB(FCanvas.Brush.Color);
                      finally
                        FCanvas.Handle := 0;
                      end;
                    end;
                    if IsCustomDrawn(dtItem, cdPrePaint) or
                       IsCustomDrawn(dtItem, cdPreErase) or
                       IsCustomDrawn(dtItem, cdPreErase) or
                       (DrawingStyle = dsGradient) then
                      Result := Result or CDRF_NOTIFYITEMDRAW;
                    if IsCustomDrawn(dtItem, cdPostPaint) then
                      Result := Result or CDRF_NOTIFYPOSTPAINT;
                    if IsCustomDrawn(dtItem, cdPostErase) then
                      Result := Result or CDRF_NOTIFYPOSTERASE;
                  end;
                CDDS_POSTPAINT:
                  if IsCustomDrawn(dtControl, cdPostPaint) then
                    CustomDraw(R, cdPostPaint);
                CDDS_PREERASE:
                  if IsCustomDrawn(dtControl, cdPreErase) then
                    CustomDraw(R, cdPreErase);
                CDDS_POSTERASE:
                  if IsCustomDrawn(dtControl, cdPostErase) then
                    CustomDraw(R, cdPostErase);
              end;
{$IFDEF CLR}
              // write changes back to Message
              NMTBCustomDraw := TBCustomDraw;
{$ENDIF}
            end else
            begin
              Button := nil;
              if Integer(nmcd.dwItemSpec) < FButtons.Count then
              Button := Buttons[nmcd.dwItemSpec];
            if Button = nil then Exit;
              case nmcd.dwDrawStage of
                CDDS_ITEMPREPAINT:
                  begin
                    try
                      FCanvas.Handle := nmcd.hdc;
                      FCanvas.Font := Self.Font;
                      FCanvas.Brush := Self.Brush;
                      FCanvas.Font.OnChange := CanvasChanged;
                      FCanvas.Brush.OnChange := CanvasChanged;
                      FCanvasChanged := False;
                      Flags := [];
                      DefaultDraw := CustomDrawButton(Button,
                        TCustomDrawState(Word(nmcd.uItemState)), cdPrePaint, Flags);
                    if DefaultDraw and (DrawingStyle = dsGradient) then
                    begin
                      Flags := [];
                      DefaultDraw := GradientDrawButton(Button,
                                      TCustomDrawState(Word(nmcd.uItemState)),
                                      Flags);
                    end;
                      if tbNoEdges in Flags then
                        Result := Result or TBCDRF_NOEDGES;
                      if tbHiliteHotTrack in Flags then
                    begin
                        Result := Result or TBCDRF_HILITEHOTTRACK;
                      // clrHighlightHotTrack := HotTrackColor; // should we set this?
                    end;
                      if tbNoOffset in Flags then
                        Result := Result or TBCDRF_NOOFFSET;
                      if tbNoMark in Flags then
                        Result := Result or TBCDRF_NOMARK;
                      if tbNoEtchedEffect in Flags then
                        Result := Result or TBCDRF_NOETCHEDEFFECT;
                      clrText := ColorToRGB(FCanvas.Font.Color);
                      clrBtnFace := ColorToRGB(FCanvas.Brush.Color);
                      if not DefaultDraw then
                      begin
                        Result := Result or CDRF_SKIPDEFAULT;
                        Exit;
                      end
                      else
                      if FCanvasChanged then
                      begin
                        FCanvasChanged := False;
                        FCanvas.Font.OnChange := nil;
                        FCanvas.Brush.OnChange := nil;
{$IFDEF CLR}
                        if GetObject(FCanvas.Font.Handle, Marshal.SizeOf(LogFont), LogFont) <> 0 then
{$ELSE}
                        if GetObject(FCanvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
{$ENDIF}
                        begin
                          FCanvas.Handle := 0;  // disconnect from hdc
                          // don't delete the stock font
                          FOurFont := CreateFontIndirect(LogFont);
                          FStockFont := SelectObject(nmcd.hdc, FOurFont);
                          Result := Result or CDRF_NEWFONT;
{$IFDEF CLR}
                          // write changes back to Message
                          NMTBCustomDraw := TBCustomDraw;
{$ENDIF}
                        end;
                      end;
                      if IsCustomDrawn(dtItem, cdPostPaint) then
                        Result := Result or CDRF_NOTIFYPOSTPAINT;
                    finally
                      FCanvas.Handle := 0;
                    end;
                  end;
                CDDS_ITEMPOSTPAINT:
                begin
                  try
                    FCanvas.Handle := nmcd.hdc;
                    FCanvas.Font := Self.Font;
                    FCanvas.Brush := Self.Brush;
                    if Button <> nil then
                      CustomDrawButton(Button, TCustomDrawState(Word(nmcd.uItemState)),
                        cdPostPaint, Flags);
                  finally
                    FCanvas.Handle := 0;
                  end;
                  //release the font we may have loaned during item drawing.
                  if (FOurFont <> 0) and (FStockFont <> 0) then
                  begin
                    SelectObject(nmcd.hdc, FStockFont);
                    DeleteObject(FOurFont);
                    FOurFont := 0;
                    FStockFont := 0;
                  end;
                end;
                CDDS_ITEMPREERASE:
                  if Button <> nil then
                    CustomDrawButton(Button, TCustomDrawState(Word(nmcd.uItemState)),
                      cdPreErase, Flags);
                CDDS_ITEMPOSTERASE:
                  if Button <> nil then
                    CustomDrawButton(Button, TCustomDrawState(Word(nmcd.uItemState)),
                      cdPostErase, Flags);
              end;
            end;
          finally
            FCanvas.Unlock;
          end;
        end;
      TBN_QUERYINSERT:
        with NMToolbar{$IFNDEF CLR}^{$ENDIF} do
        begin
          if not FCustomizing and (iItem = FLastQueryDeleteIndex) then
            // See comment about dragging buttons below...
            Result := 0
          else
          begin
            Result := Integer(DoQueryInsert(iItem));
            if (tbButton.dwData = 0) and (tbButton.fsStyle = ButtonStyles[tbsSeparator]) then
            begin
              Button := TMyToolButton.Create(Owner);
              Button.Style := tbsSeparator;
              FButtons.Insert(iItem, Button);
              Inc(FSeparators);
            end;
          end;
          FLastQueryDeleteIndex := -1;
        end;
      TBN_QUERYDELETE:
        with NMToolbar{$IFNDEF CLR}^{$ENDIF} do
        begin
          Result := Integer(DoQueryDelete(iItem));
          // Dragging a button holding the SHIFT key and dropping it
          // in it's original place will cause the ToolBar to insert
          // an extra "garbage" button. This causes an AV when the buttons
          // are recreated from the ToolBar. When the customize dialog isn't
          // showing, track the last index of TBN_QUERYDELETE so it can be
          // ignored when the TBN_QUERYINSERT notification is handled.
          if not FCustomizing then
            FLastQueryDeleteIndex := iItem;
        end;
      TBN_GETBUTTONINFOA, TBN_GETBUTTONINFOW:
        begin
          NMTB := NMToolBar;
          if FCustomizing then
          begin
            Result := Integer(DoGetButton(NMTB{$IFNDEF CLR}^{$ENDIF}));
{$IFDEF CLR}
            NMToolBar := NMTB;
{$ENDIF}
          end;
        end;
      TBN_DELETINGBUTTON:
        if FCustomizing and not FRestoring and Assigned(FOnCustomizeDelete) then
        begin
          Button := TMyToolButton(FButtons[NMToolbar.iItem]);
          FOnCustomizeDelete(Self, Button);
        end;
      TBN_BEGINADJUST:
        begin
          FCustomizing := True;
          FSeparators := 0;
          if Assigned(FOnCustomizing) then
            FOnCustomizing(Self);
          if not FRestoring then
            SaveButtons(True);
        end;
      TBN_ENDADJUST:
        begin
          if not FRestoring then
          begin
            RecreateButtonsFromToolbar;
            FCustomizing := False;
          end
          else
          if Assigned(FOnCustomizeReset) then
            FOnCustomizeReset(Self);
          FRestoring := False;
        end;
      TBN_TOOLBARCHANGE:
        begin
          if not FCustomizing then //Buttons were dragged holding SHIFT key down.
            RecreateButtonsFromToolbar;
          if Assigned(FOnCustomized) then
            FOnCustomized(Self);
        end;
      TBN_RESET:
        begin
          FRestoring := True;
          SaveButtons(False);
          RecreateButtonsFromToolbar;
          FRestoring := False;
        end;
    end;
end;

procedure TMyToolBar.RecreateButtonsFromToolbar;
var
  I: Integer;
  ButtonInfo: TTBBUTTON;
  Button: TMyToolButton;
  TBButtonCount: Integer;
begin
  TBButtonCount := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
  FButtons.Clear;
  for I := 0 to TBButtonCount - 1 do
  begin
    SendGetStructMessage(Handle, TB_GETBUTTON, I, ButtonInfo);
    if ButtonInfo.dwData = 0 then
    begin
      Button := TMyToolButton.Create(Owner);
      Button.Style := tbsSeparator;
{$IFDEF CLR}
      // Add to hashtable to allow access to the actual Button
      // object from the TBButton struct
      if not FButtonHashTable.ContainsValue(Button) then
        FButtonHashTable.Add(TObject(Button.GetHashCode), Button);
      ButtonInfo.dwData := Button.GetHashCode;
{$ELSE}
      ButtonInfo.dwData := UIntPtr(Button);
{$ENDIF}
      Button.FToolbar := Self;
      SendMessage(Handle, TB_DELETEBUTTON, I, 0);
      SendStructMessage(Handle, _TB_INSERTBUTTON, I, ButtonInfo);
{$IFDEF CLR}
    end
    else
      Button := TMyToolButton(FButtonHashTable.Item[TObject(ButtonInfo.dwData)]);
    FButtons.Add(Button);
{$ELSE}
    end;
    FButtons.Add(Pointer(ButtonInfo.dwData));
{$ENDIF}
  end;
  RecreateButtons;
end;

type
  TControlAccess = class(TControl);

procedure TMyToolBar.WndProc(var Message: TMessage);
var
  Control: TControl;
  CapControl: TControl;
  Msg: TMsg;
{$IFDEF CLR}
  MouseMsg: TWMMouse;
{$ENDIF}

  function IsToolButtonMouseMsg(var Message: TWMMouse): Boolean;
  begin
    if GetCapture = Handle then
    begin
      CapControl := GetCaptureControl;
      if (CapControl <> nil) and (CapControl.Parent <> Self) then
        CapControl := nil;
    end
    else
      CapControl := nil;
    Control := ControlAtPos(SmallPointToPoint(Message.Pos), False);
    Result := (Control <> nil) and (Control is TMyToolButton) and
      not Control.Dragging;
  end;

  procedure SendDropdownMsg(Button: TMyToolButton);
  var
    Msg: TNMToolBar;
  begin
{$IFNDEF CLR}
    FillChar(Msg, SizeOf(Msg), 0);
{$ENDIF}
    with Msg, hdr do
    begin
      hwndFrom := Handle;
      idFrom := Handle;
      code := TBN_DROPDOWN;
      iItem := Button.Index;
    end;
    SendStructMessage(Handle, WM_NOTIFY, Handle, Msg);
  end;

begin
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_MOUSEMOVE:
        begin
          { Call default wndproc to get buttons to repaint when Flat = True. }
{$IFDEF CLR}
          MouseMsg := TWMMouse.Create(Message);
          if IsToolButtonMouseMsg(MouseMsg) then
{$ELSE}
          if IsToolButtonMouseMsg(TWMMouse(Message)) then
{$ENDIF}
          begin
            { Prevent painting of flat buttons when they are dock clients }
            if TControlAccess(Control).DragMode <> dmAutomatic then
              DefaultHandler(Message);
          end
          else
            DefaultHandler(Message);
        end;
      WM_LBUTTONUP:
        begin
          { Update button states after a click. }
{$IFDEF CLR}
          MouseMsg := TWMMouse.Create(Message);
          if IsToolButtonMouseMsg(MouseMsg) then
{$ELSE}
          if IsToolButtonMouseMsg(TWMMouse(Message)) then
{$ENDIF}
          begin
            DefaultHandler(Message);
            if (CapControl = Control) or (Control is TMyToolButton) then
            begin
              with TMyToolButton(Control) do
                if Down and Grouped and AllowAllUp and (Style = tbsCheck) then
                  Down := False;
              UpdateButtonStates;
            end
            else
              if (CapControl is TMyToolButton) or (TMyToolButton(Control).Style = tbsDropDown) then
                Exit;
          end;
        end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        begin
{$IFDEF CLR}
          MouseMsg := TWMMouse.Create(Message);
          if IsToolButtonMouseMsg(MouseMsg) then
{$ELSE}
          if IsToolButtonMouseMsg(TWMMouse(Message)) then
{$ENDIF}
          begin
            { Check if mouse is clicked on a drop-down button's arrow (for IE4+
              the arrow is within 13 pixels from the right, for IE3 there is no
              distinction - the entire button is used).  If an arrow click is
              detected then don't process this mouse event - a TBN_DROPDOWN
              notification will be created for us by the default wndproc. }
            with TMyToolButton(Control) do
            begin
              { Allow IsControlMouseMsg to deliver message to button.
                This causes the clicking to happen. }
              if FInMenuLoop and Self.MouseCapture then
                MouseCapture := True;
              { When the style is tbsDropDown, and we have IE4+, we need
                to subtract the drop down button width of 14 pixels, otherwise
                it will cause the drop down menu to "stick" down }
              if (Style <> tbsDropDown) or
                 (GetComCtlVersion >= ComCtlVersionIE4) and
{$IFDEF CLR}
                 (MouseMsg.XPos < Left + Width - 14) then
{$ELSE}
                 (TWMMouse(Message).XPos < Left + Width - 14) then
{$ENDIF}
                inherited WndProc(Message);
            end;
            if not Control.Dragging then
              DefaultHandler(Message);

            if (TMyToolButton(Control).Style <> tbsDropDown) and
              ((TMyToolButton(Control).DropdownMenu <> nil) or
              (TMyToolButton(Control).MenuItem <> nil)) then
            begin
              try
                SendDropDownMsg(TMyToolButton(Control));
              finally
                { Here we remove WM_LBUTTONDOWN message sent and instead dispatch
                  it as a WM_LBUTTONUP to get a Click fired. }
                Msg.Message := 0;
                if PeekMessage(Msg, Handle, WM_LBUTTONDOWN, WM_LBUTTONDOWN,
                  PM_REMOVE) and (Msg.Message = WM_QUIT) then
                  PostQuitMessage(Msg.WParam)
                else
                begin
                  Message.Msg := WM_LBUTTONUP;
                  Dispatch(Message);
                end;
              end;
            end;
            Exit;
          end;
        end;
    end
  end;
  inherited WndProc(Message);
end;

procedure TMyToolBar.FlipChildren(AllLevels: Boolean);
begin { do not flip controls }
end;

function TMyToolBar.FindButtonFromAccel(Accel: Word): TMyToolButton;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TMyToolButton then
    begin
      Result := TMyToolButton(FButtons[I]);
      if Result.Visible and Result.Enabled and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;

{ CustomDraw support }

function TMyToolBar.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  if Stage = cdPrePaint then
  begin
    if Target = dtItem then
      Result := Assigned(FOnCustomDrawButton) or Assigned(FOnAdvancedCustomDrawButton)
    else if Target = dtControl then
      Result := Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) or
        Assigned(FOnCustomDrawButton) or Assigned(FOnAdvancedCustomDrawButton)
    else
      Result := False;
  end
  else
  begin
    if Target = dtItem then
      Result := Assigned(FOnCustomDrawButton) or Assigned(FOnAdvancedCustomDrawButton)
    else if Target = dtControl then
      Result := Assigned(FOnAdvancedCustomDraw) or Assigned(FOnAdvancedCustomDrawButton)
    else
      Result := False;
  end;
end;

function TMyToolBar.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, ARect, Result);
  if Assigned(FOnAdvancedCustomDraw) then
    FOnAdvancedCustomDraw(Self, ARect, Stage, Result);
end;

function TMyToolBar.CustomDrawButton(Button: TMyToolButton; State: TCustomDrawState;
  Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawButton) then
    FOnCustomDrawButton(Self, Button, State, Result);
  if Assigned(FOnAdvancedCustomDrawButton) then
    FOnAdvancedCustomDrawButton(Self, button, State, Stage, Flags, Result);
end;

procedure TMyToolBar.CanvasChanged(Sender: TObject);
begin
  FCanvasChanged := True;
end;

function TMyToolBar.GradientDrawButton(Button: TMyToolButton; State: TCustomDrawState;
  var Flags: TTBCustomDrawFlags): Boolean;
const
  cInset = 4;
var
  FillColor: TColor;
  EdgeColor: TColor;
  R: TRect;
  X: Integer;
  Y: Integer;
  Str: string;
  ImageList: TCustomImageList;
begin
  Result := False;
  FBitmap.Canvas.Pen.Assign(Canvas.Pen);
  FBitmap.Canvas.Brush.Assign(Canvas.Brush);
  FBitmap.Canvas.Font.Assign(Canvas.Font);
  ImageList := nil;

  if gdoGradient in GradientDrawingOptions then
  begin
    FBitmap.SetSize(Width, Height);
    GradientFillCanvas(FBitmap.Canvas, FGradientStartColor, FGradientEndColor,
      ClientRect, GradientDirection);
    FBitmap.Canvas.CopyRect(Rect(0, 0, Button.Width, Button.Height),
      FBitmap.Canvas, Button.BoundsRect);
    FBitmap.SetSize(Button.Width, Button.Height);
  end
  else
  begin
    FBitmap.SetSize(Button.Width, Button.Height);
    FBitmap.Canvas.Brush.Color := Button.Color;
    FBitmap.Canvas.Brush.Style := bsSolid;
    FBitmap.Canvas.FillRect(FBitmap.Canvas.ClipRect);
  end;

  if (Button.Style = tbsButton) or (Button.Style = tbsCheck) or
     (Button.Style = tbsDropDown) or (Button.Style = tbsTextButton) then
  begin
    if cdsHot in State then
      ImageList := HotImages;
    if not Button.Enabled then
      ImageList := DisabledImages;
    if ImageList = nil then
      ImageList := Images;

    if (cdsHot in State) or (Button.Down and Button.Enabled) then
    begin
      if (gdoHotTrack in GradientDrawingOptions) then
      begin
        FillColor := HotTrackColor;
        if cdsSelected in State then
          FillColor := GetShadowColor(FillColor, -25);
        EdgeColor := GetShadowColor(FillColor);

        R := Rect(0, 0, Button.Width, Button.Height);

        FBitmap.Canvas.Brush.Color := EdgeColor;
        if Button.Style = tbsDropDown then
          Dec(R.Right, cDropDownWidth - (FBitmap.Canvas.Pen.Width div 2));

        FBitmap.Canvas.FillRect(R);
        InflateRect(R, -FBitmap.Canvas.Pen.Width, -FBitmap.Canvas.Pen.Width);
        FBitmap.Canvas.Brush.Color := FillColor;
        FBitmap.Canvas.FillRect(R);
        InflateRect(R, FBitmap.Canvas.Pen.Width, FBitmap.Canvas.Pen.Width);

        if Button.Style = tbsDropDown then
        begin
          R.Left := R.Right;
          Inc(R.Right, cDropDownWidth - (FBitmap.Canvas.Pen.Width div 2));
          FBitmap.Canvas.Brush.Color := EdgeColor;
          FBitmap.Canvas.FillRect(R);
          InflateRect(R, -FBitmap.Canvas.Pen.Width, -FBitmap.Canvas.Pen.Width);
          FBitmap.Canvas.Brush.Color := FillColor;
          FBitmap.Canvas.FillRect(R);
        end;
      end
      else
      begin
        if Button.Down then
        begin
          FillColor := cl3DDkShadow;
          EdgeColor := cl3DLight;
        end
        else
        begin
          FillColor := cl3DLight;
          EdgeColor := cl3DDkShadow;
        end;

        R := Rect(0, 0, Button.Width, Button.Height);

        Frame3D(FBitmap.Canvas, R, FillColor, EdgeColor, Canvas.Pen.Width);

        if Button.Style = tbsDropDown then
        begin
          FBitmap.Canvas.MoveTo(R.Right - cDropDownWidth, 0);
          FBitmap.Canvas.LineTo(R.Right - cDropDownWidth, Button.Height);
        end;
      end;
    end;

    if (ImageList <> nil) and (Button.ImageIndex >= 0) and (Button.ImageIndex < ImageList.Count) or
       ((ImageList <> nil) and (Button.Style = tbsTextButton)) then
    begin
      if (ShowCaptions and List) or (AllowTextButtons and (Button.Style = tbsTextButton)) then
        X := cInset
      else
      begin
        X := (Button.Width - ImageList.Width) div 2;
        if Button.Style = tbsDropDown then
          Dec(X, cDropDownWidth div 2);
      end;
      if (List and not AllowTextButtons) or
         (AllowTextButtons and (Button.Style = tbsTextButton)) then
        Y := (Button.Height - ImageList.Height) div 2
      else
        Y := cInset;

      ImageList.Draw(FBitmap.Canvas, X, Y, Button.ImageIndex,
        dsTransparent, itImage, Button.Enabled or (csDesigning in ComponentState) or
        (not Button.Enabled and (ImageList = DisabledImages)));
    end;

    if (Button.Style = tbsDropDown) then
    begin
      X := Button.Width - ((cDropDownWidth div 2) + (cDropDownWidth div 4));
      Y := Button.Height div 2;

      FBitmap.Canvas.Pen.Color := Button.Font.Color;
      if not Button.Enabled then
        FBitmap.Canvas.Pen.Color := clGrayText;

      FBitmap.Canvas.Brush.Style := bsSolid;
      DrawArrow(FBitmap.Canvas, sdDown, Point(X, Y), cDropDownWidth div 4);
    end;

    if (ShowCaptions and not AllowTextButtons) or
       (AllowTextButtons and (Button.Style = tbsTextButton)) then
    begin
      FBitmap.Canvas.Brush.Style := bsClear;
      if (ImageList <> nil) and List and ((Button.Style <> tbsTextButton) or
         ((Button.Style = tbsTextButton) and (Button.ImageIndex <> -1))) then
        R.Left := ImageList.Width
      else
        R.Left := 0;
      R.Right := Button.Width;

      Str := Button.Caption;

      if Button.Style = tbsDropDown then
        Dec(R.Right, cDropDownWidth - (FBitmap.Canvas.Pen.Width div 2));
      if (not List) and (ImageList <> nil) then
        R.Top := ImageList.Height + cInset
      else
        R.Top := (Button.Height div 2) - (FBitmap.Canvas.TextHeight(Str) div 2);
      R.Bottom := R.Top + FBitmap.Canvas.TextHeight(Str);

      FBitmap.Canvas.Font.Color := Button.Font.Color;
      if not Button.Enabled then
        FBitmap.Canvas.Font.Color := clGrayText;

      DrawText(FBitmap.Canvas.Handle, Str, Length(Str), R,
        DT_END_ELLIPSIS or DT_NOCLIP or DT_VCENTER or DT_CENTER);
    end;
  end;

  Canvas.Draw(Button.Left, Button.Top, FBitmap);
end;

function TMyToolBar.GradientDrawToolBar(const ARect: TRect): Boolean;
begin
  Result := True;
  if gdoGradient in GradientDrawingOptions then
    GradientFillCanvas(Canvas, FGradientStartColor, FGradientEndColor,
      ARect, GradientDirection);
end;

procedure TMyToolBar.SetGradientDrawingOptions(Value: TTBGradientDrawingOptions);
begin
  if Value <> FGradientDrawingOptions then
  begin
    FGradientDrawingOptions := Value;
    if HandleAllocated then
    Repaint;
  end;
end;

procedure TMyToolBar.SetDrawingStyle(Value: TTBDrawingStyle);
begin
  if Value <> FDrawingStyle then
  begin
    FDrawingStyle := Value;
    if HandleAllocated then
    Repaint;
  end;
end;

procedure TMyToolBar.SetGradientEndColor(Value: TColor);
begin
  if Value <> FGradientEndColor then
  begin
    FGradientEndColor := Value;
    if HandleAllocated then
    Repaint;
  end;
end;

procedure TMyToolBar.SetGradientStartColor(Value: TColor);
begin
  if Value <> FGradientStartColor then
  begin
    FGradientStartColor := Value;
    if HandleAllocated then
    Repaint;
  end;
end;

function TMyToolBar.Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TTBButton): LRESULT;
begin
{$IFDEF CLR}
  Result := inherited Perform(Msg, WParam, LParam);
{$ELSE}
  Result := inherited Perform(Msg, WParam, Winapi.Windows.LPARAM(@LParam));
{$ENDIF}
end;

function TMyToolBar.IsGradientEndColorStored: Boolean;
begin
  Result := FGradientEndColor <> GetShadowColor(clBtnFace, -25);
end;

{ Toolbar menu support }

{$IFDEF CLR}
function ToolMenuGetMsgHook(Code: Integer; WParam: WPARAM;
  lParam: LPARAM): LRESULT; forward;
{$ENDIF}

var
  ToolMenuHook: HHOOK;
  InitDone: Boolean = False;
  MenuToolBar, MenuToolBar2: TMyToolBar;
  MenuButtonIndex: Integer;
  LastMenuItem: TMenuItem;
  LastMousePos: TPoint;
  StillModal: Boolean;
{$IFDEF CLR}
  ToolMenuHookProc: TFNMsgHookProc = ToolMenuGetMsgHook;
{$ENDIF}

{$IFDEF CLR}
function ToolMenuGetMsgHook(Code: Integer; WParam: WPARAM;
  lParam: LPARAM): LRESULT;
{$ELSE}
function ToolMenuGetMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
{$ENDIF}
const
  RightArrowKey: array[Boolean] of Word = (VK_LEFT, VK_RIGHT);
  LeftArrowKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
var
  P: TPoint;
  Target: TControl;
  Item: Integer;
  FindKind: TFindItemKind;
  ParentMenu: TMenu;
{$IFDEF CLR}
  Msg: TMsg;
{$ENDIF}

  function FindButton(Forward: Boolean): TMyToolButton;
  var
    ToolBar: TMyToolBar;
    I, J, Count: Integer;
  begin
    ToolBar := MenuToolBar;
    if ToolBar <> nil then
    begin
      J := MenuButtonIndex;
      I := J;
      Count := ToolBar.ButtonCount;
      if Forward then
        repeat
          if I = Count - 1 then
            I := 0
          else
            Inc(I);
          Result := ToolBar.Buttons[I];
          if Result.Visible and Result.Enabled and Result.Grouped then
            Exit;
        until I = J
      else
        repeat
          if I = 0 then
            I := Count - 1
          else
            Dec(I);
          Result := ToolBar.Buttons[I];
          if Result.Visible and Result.Enabled and Result.Grouped then
            Exit;
        until I = J;
    end;
    Result := nil;
  end;

begin
{$IFDEF CLR}
  Msg := TMsg(Marshal.PtrToStructure(lParam, TypeOf(TMsg)));
{$ENDIF}
  if LastMenuItem <> nil then
  begin
    ParentMenu := LastMenuItem.GetParentMenu;
    if ParentMenu <> nil then
    begin
      if ParentMenu.IsRightToLeft then
{$IFDEF CLR}
        if Msg.WParam = VK_LEFT then
        begin
          Msg.WParam := VK_RIGHT;
          with Marshal do
            WriteInt32(lParam, Longint(OffsetOf(TypeOf(TMsg), 'wParam')), Msg.WParam);
        end
        else
          if Msg.WParam = VK_RIGHT then
          begin
            Msg.WParam := VK_LEFT;
            with Marshal do
              WriteInt32(lParam, Longint(OffsetOf(TypeOf(TMsg), 'wParam')), Msg.WParam);
          end;
{$ELSE}
        if Msg.WParam = VK_LEFT then
          Msg.WParam := VK_RIGHT
        else
          if Msg.WParam = VK_RIGHT then
          Msg.WParam := VK_LEFT;
{$ENDIF}
    end;
  end;
{$IFDEF CLR}
  Result := CallNextHookEx(ToolMenuHook, Code, WParam, lParam);
{$ELSE}
  Result := CallNextHookEx(ToolMenuHook, Code, WParam, LPARAM(@Msg));
{$ENDIF}
  if Result <> 0 then
    Exit;
  if (Code = MSGF_MENU) then
  begin
    Target := nil;
    if not InitDone then
    begin
      InitDone := True;
      PostMessage(Msg.Hwnd, WM_KEYDOWN, VK_DOWN, 0);
    end;
    case Msg.Message of
      WM_MENUSELECT:
        begin
          if (HiWord(Msg.WParam) = $FFFF) and (Msg.LParam = 0) then
          begin
            if not StillModal then
              MenuToolBar.CancelMenu;
            Exit;
          end
          else
            StillModal := False;
          FindKind := fkCommand;
          if HiWord(Msg.WParam) and MF_POPUP <> 0 then
            FindKind := fkHandle;
          if FindKind = fkHandle then
            Item := GetSubMenu(Msg.LParam, LoWord(Msg.WParam))
          else
            Item := LoWord(Msg.WParam);
          LastMenuItem := MenuToolBar.FTempMenu.FindItem(Item, FindKind);
        end;
      WM_SYSKEYDOWN:
        if Msg.WParam = VK_MENU then
        begin
          MenuToolBar.CancelMenu;
          Exit;
        end;
      WM_KEYDOWN:
        if Msg.WParam = VK_RETURN then
          MenuToolBar.FMenuResult := True
        else
          if Msg.WParam = VK_ESCAPE then
            StillModal := True
          else
            if LastMenuItem <> nil then
            begin
              if (Msg.WParam = VK_RIGHT) and (LastMenuItem.Count = 0) then
                Target := FindButton(True)
              else
                if (Msg.WParam = VK_LEFT) and (LastMenuItem.GetParentComponent is TPopupMenu) then
                  Target := FindButton(False)
                else
                  Target := nil;
              if Target <> nil then
                P := Target.ClientToScreen(Point(0, 0));
            end;
      WM_MOUSEMOVE:
        begin
          P := Msg.pt;
          if (P.X <> LastMousePos.X) or (P.Y <> LastMousePos.Y) then
          begin
            Target := FindDragTarget(P, False);
            LastMousePos := P;
          end;
        end;
    end;
    if (Target <> nil) and (Target is TMyToolButton) then
    begin
      with TMyToolButton(Target) do
        if (Index <> MenuButtonIndex) and Grouped and (Parent <> nil) and
          Parent.HandleAllocated then
        begin
          StillModal := True;
          MenuToolBar.FCaptureChangeCancels := False;
          MenuToolBar.ClickButton(TMyToolButton(Target));
          MenuToolBar.ClickButton(TMyToolButton(Target));
        end;
    end;
  end;
end;

procedure InitToolMenuHooks;
begin
  StillModal := False;
  GetCursorPos(LastMousePos);
  if ToolMenuHook = 0 then
{$IFDEF CLR}
    ToolMenuHook := SetWindowsHookEx(WH_MSGFILTER, ToolMenuHookProc, 0,
      GetCurrentThreadID);
{$ELSE}
    ToolMenuHook := SetWindowsHookEx(WH_MSGFILTER, @ToolMenuGetMsgHook, 0,
      GetCurrentThreadID);
{$ENDIF}
end;

procedure ReleaseToolMenuHooks;
begin
  if ToolMenuHook <> 0 then
    UnhookWindowsHookEx(ToolMenuHook);
  ToolMenuHook := 0;
  LastMenuItem := nil;
  MenuToolBar := nil;
  MenuButtonIndex := -1;
  InitDone := False;
end;

procedure ReleaseToolMenuKeyHooks; forward;
{$IFDEF CLR}
function ToolMenuKeyMsgHook(Code: Integer; WParam: WPARAM;
  lParam: LPARAM): LRESULT; forward;
{$ENDIF}

var
  ToolMenuKeyHook: HHOOK;
{$IFDEF CLR}
  ToolMenuKeyHookProc: TFNMsgHookProc = ToolMenuKeyMsgHook;
{$ENDIF}

{$IFNDEF CLR}
function ToolMenuKeyMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
{$ELSE}
function ToolMenuKeyMsgHook(Code: Integer; WParam: WPARAM;
  lParam: LPARAM): LRESULT;
var
  Msg: TMsg;
{$ENDIF}
begin
{$IFDEF CLR}
  Msg := TMsg(Marshal.PtrToStructure(lParam, TypeOf(TMsg)));
{$ENDIF}
  if (Code = HC_ACTION) then
  begin
    if Msg.Message = CM_DEACTIVATE then
      MenuToolBar2.CancelMenu
    else
      if Msg.message = WM_COMMAND then
        ReleaseToolMenuKeyHooks
      else
      begin
        if (ToolMenuHook = 0) and ((Msg.Message = WM_CHAR) or
        (Msg.Message = WM_KEYDOWN) or (Msg.Message = WM_KEYUP) or
        (Msg.Message = WM_SYSKEYDOWN) or (Msg.Message = WM_SYSKEYUP)) then
          Msg.hwnd := MenuToolBar2.Handle;
{$IFDEF CLR}
        Marshal.StructureToPtr(TObject(Msg), lParam, False);
{$ENDIF}
      end;
  end;
{$IFDEF CLR}
  Result := CallNextHookEx(ToolMenuKeyHook, Code, WParam, lParam)
{$ELSE}
  Result := CallNextHookEx(ToolMenuKeyHook, Code, WParam, LPARAM(@Msg))
{$ENDIF}
end;

procedure InitToolMenuKeyHooks;
begin
  if ToolMenuKeyHook = 0 then
{$IFDEF CLR}
    ToolMenuKeyHook := SetWindowsHookEx(WH_GETMESSAGE, ToolMenuKeyHookProc, 0,
      GetCurrentThreadID);
{$ELSE}
    ToolMenuKeyHook := SetWindowsHookEx(WH_GETMESSAGE, @ToolMenuKeyMsgHook, 0,
      GetCurrentThreadID);
{$ENDIF}
end;

procedure ReleaseToolMenuKeyHooks;
begin
  if ToolMenuKeyHook <> 0 then
    UnhookWindowsHookEx(ToolMenuKeyHook);
  ToolMenuKeyHook := 0;
  MenuToolBar2 := nil;
end;

procedure TMyToolBar.ClearTempMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  if (FButtonMenu <> nil) and (FMenuButton <> nil) and
    (FMenuButton.MenuItem <> nil) and (FTempMenu <> nil) then
  begin
    for I := FTempMenu.Items.Count - 1 downto 0 do
    begin
      Item := FTempMenu.Items[I];
      FTempMenu.Items.Delete(I);
      FButtonMenu.Insert(0, Item);
    end;
    FTempMenu.Free;
    FTempMenu := nil;
    FMenuButton := nil;
    FButtonMenu := nil;
  end;
end;

function TMyToolBar.CheckMenuDropdown(Button: TMyToolButton): Boolean;
var
  Hook: Boolean;
  Menu: TMenu;
  Item: TMenuItem;
  I: Integer;
  ParentMenu: TMenu;
  APoint: TPoint;
  LMonitor: TMonitor;
begin
  Result := False;
  if Button = nil then
    Exit;
  FCaptureChangeCancels := False;
  try
    if Button.DropdownMenu <> nil then
      FTempMenu := Button.DropdownMenu
    else if Button.MenuItem <> nil then
    begin
      Button.MenuItem.Click;
      ClearTempMenu;
      FTempMenu := TPopupMenu.Create(Self);
      ParentMenu := Button.MenuItem.GetParentMenu;
      if ParentMenu <> nil then
        FTempMenu.BiDiMode := ParentMenu.BiDiMode;
      FTempMenu.HelpContext := Button.MenuItem.HelpContext;
      FTempMenu.TrackButton := tbLeftButton;
      Menu := Button.MenuItem.GetParentMenu;
      if Menu <> nil then
        FTempMenu.Images := Menu.Images;
      FButtonMenu := Button.MenuItem;
      for I := FButtonMenu.Count - 1 downto 0 do
      begin
        Item := FButtonMenu.Items[I];
        FButtonMenu.Delete(I);
        FTempMenu.Items.Insert(0, Item);
      end;
    end
    else
      Exit;
    SendCancelMode(nil);
    FTempMenu.PopupComponent := Self;
    Hook := Button.Grouped or (Button.MenuItem <> nil);
    if Hook then
    begin
      MenuButtonIndex := Button.Index;
      MenuToolBar := Self;
      InitToolMenuHooks;
    end;
    Perform(TB_SETHOTITEM, WPARAM(-1), 0);
    try
      APoint := Button.ClientToScreen(Point(0, Button.ClientHeight));
      if FTempMenu.IsRightToLeft then
        Inc(APoint.X, Button.Width);
      FMenuDropped := True;
      LMonitor := Screen.MonitorFromPoint(APoint);
      if (LMonitor <> nil) and
         ((GetSystemMetrics(SM_CYMENU) * FTempMenu.Items.Count) + APoint.Y > LMonitor.Height) then
        Dec(APoint.Y, Button.Height);
      if GetComCtlVersion = ComCtlVersionIE5 then
        Button.Invalidate;
      FTempMenu.Popup(APoint.X, APoint.Y);
    finally
      if Hook then ReleaseToolMenuHooks;
    end;
    FMenuButton := Button;
    if StillModal then
      Perform(TB_SETHOTITEM, Button.Index, 0);
    Result := True;
  finally
    PostMessage(Handle, CN_DROPDOWNCLOSED, 0, 0);
  end;
end;

procedure TMyToolBar.WMSysCommand(var Message: TWMSysCommand);

  function IsMenuBar: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FButtons.Count - 1 do
      if (TControl(FButtons[I]) is TMyToolButton)
      and Assigned(TMyToolButton(FButtons[I]).MenuItem) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  Button: TMyToolButton;
begin
  { Enter menu loop if only the Alt key is pressed -- ignore Alt-Space and let
    the default processing show the system menu. }
  if not FInMenuLoop and Enabled and Showing and (ShowCaptions or AllowTextButtons) and IsMenuBar then
    with Message do
      if (CmdType and $FFF0 = SC_KEYMENU) and (Key <> VK_SPACE) and
        (Key <> Word('-')) and (GetCapture = 0) then
      begin
        if Key = 0 then
          Button := nil else
          Button := FindButtonFromAccel(Key);
        if (Key = 0) or ((Button <> nil) and (not AllowTextButtons or (Button.ImageIndex > -1))) then
        begin
          TrackMenu(Button);
          Result := 1;
          Exit;
        end;
      end;
end;

procedure TMyToolBar.ClickButton(Button: TMyToolButton);
var
  P: TPoint;
  SmallPt: TSmallPoint;
begin
  FCaptureChangeCancels := False;
  P := Button.ClientToScreen(Point(0, 0));
  SmallPt := PointToSmallPoint(ScreenToClient(P));
  with SmallPt do
    PostMessage(Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLong(X, Y));
end;

procedure TMyToolBar.InitMenu(Button: TMyToolButton);
begin
  Perform(TB_SETANCHORHIGHLIGHT, 1, 0);
  MenuToolBar2 := Self;
  MouseCapture := True;
  InitToolMenuKeyHooks;
  if Button <> nil then
  begin
    Perform(TB_SETHOTITEM, Button.Index, 0);
    ClickButton(Button);
  end
  else
    Perform(TB_SETHOTITEM, 0, 0);
  if Button = nil then
    FCaptureChangeCancels := True;
end;

procedure TMyToolBar.CancelMenu;
begin
  if FInMenuLoop then
  begin
    ReleaseToolMenuKeyHooks;
    MouseCapture := False;
    Perform(TB_SETANCHORHIGHLIGHT, 0, 0);
  end;
  FInMenuLoop := False;
  FCaptureChangeCancels := False;
  Perform(TB_SETHOTITEM, WPARAM(-1), 0);
end;

function TMyToolBar.TrackMenu(Button: TMyToolButton): Boolean;
begin
  { Already in menu loop - click button to drop-down menu }
  if FInMenuLoop then
  begin
    if Button <> nil then
    begin
      ClickButton(Button);
      Result := True;
    end
    else
      Result := False;
    Exit;
  end;

  InitMenu(Button);
  try
    FInMenuLoop := True;
    repeat
      Application.HandleMessage;
      if Application.Terminated then
        FInMenuLoop := False;
    until not FInMenuLoop;

  finally
    CancelMenu;
  end;
  Result := FMenuResult;
end;

procedure TMyToolBar.CMFontChanged(var Message: TMessage);
begin
  if HandleAllocated and (FShowCaptions or FAllowTextButtons) then
  begin
    Perform(WM_SETFONT, Font.Handle, 0);
    if not (csLoading in ComponentState) then
      RecreateWnd;
  end;
  NotifyControls(CM_PARENTFONTCHANGED);
end;

procedure TMyToolBar.SetCustomizable(const Value: Boolean);
begin
  if Value <> FCustomizable then
  begin
    FCustomizable := Value;
    RecreateWnd;
  end;
end;

function TMyToolBar.DoGetButton(var NMToolbar: TNMToolbar): Boolean;
const
  MaxLen = 128;
var
  NewButton: TControl;
  NewToolButton: TMyToolButton;
  Title: string;
{$IFDEF CLR}
  Buffer: TBytes;
{$ELSE}
  Buffer: array[0..MaxLen] of Char;
{$ENDIF}
begin
  NewButton := nil;
  if (NMToolbar.iItem >= FButtons.Count) then
  begin
    Result := Assigned(FOnCustomizeNewButton);
    if Result then
    begin
      NewToolButton := nil;
      FOnCustomizeNewButton(Self,
        NMToolbar.iItem - FButtons.Count + FSeparators, NewToolButton);
      NewButton := TControl(NewToolButton);
      Result := NewButton <> nil;
      if Result then
      begin
        NewToolButton.FToolbar := Self;
        if FButtons.IndexOf(NewToolButton) = -1 then
        begin
          FButtons.Insert(NMToolbar.iItem, NewToolButton);
          if Assigned(FOnCustomizeAdded) then
            FOnCustomizeAdded(Self, NewToolButton);
        end;
      end;
    end;
  end
  else
  begin
    NewButton := TControl(FButtons[NMToolbar.iItem]);
    Result := NewButton is TMyToolButton;
  end;

  if Result then
    with NMToolbar, NewButton as TMyToolButton do
    begin
{$IFDEF CLR}
      if Style in [tbsSeparator, tbsDivider] then
        Title := SSeparator + #0#0
      else
        Title := Caption + #0#0;

      Buffer := PlatformBytesOf(Title);
      if Length(Buffer) > MaxLen * Marshal.SystemDefaultCharSize then
        SetLength(Buffer, MaxLen * Marshal.SystemDefaultCharSize);
      Marshal.Copy(Buffer, 0, pszText, Length(Buffer));
      cchText := Length(Buffer) div Marshal.SystemDefaultCharSize;
      if ShowCaptions or (AllowTextButtons and (FStyle = tbsTextButton)) then
        tbButton.iString := Self.Perform(_TB_ADDSTRING, 0, Title)
      else
        tbButton.iString := -1;
      tbButton.idCommand := Index;
      tbButton.iBitmap := ImageIndex;
      tbButton.fsStyle := ButtonStyles[Style];
      tbButton.fsState := GetButtonState;
      tbButton.dwData := NewButton.GetHashCode;
      // Add to hashtable to allow access to the actual Button
      // object from the TBButton struct
      if not FButtonHashTable.ContainsValue(NewButton) then
        FButtonHashTable.Add(TObject(tbButton.dwData), NewButton);
{$ELSE}
      if Style in [tbsSeparator, tbsDivider] then
        Title := SSeparator
      else
        Title := Caption;
      StrLCopy(pszText, PChar(Title), MaxLen);
      cchText := StrLen(pszText);
      StrLCopy(Buffer, PChar(Title), MaxLen);
      Buffer[Length(Title) + 1] := #0;
      if ShowCaptions or (AllowTextButtons and (FStyle = tbsTextButton)) then
        tbButton.iString := Self.Perform(TB_ADDSTRING, 0, LPARAM(@Buffer))
      else
        tbButton.iString := -1;
      tbButton.idCommand := Index;
      tbButton.iBitmap := ImageIndex;
      tbButton.fsStyle := ButtonStyles[Style];
      tbButton.fsState := GetButtonState;
      tbButton.dwData := UIntPtr(NewButton);
{$ENDIF}
      // If more than 2^16 strings are TB_ADDSTRING-ed to the tool bar's string
      // pool, the Windows API assumes iString is a pointer to a null terminated
      // string, not an index in the string pool.  Therefore we have to recreate
      // the toolbar to reset the string pool so the strings display propperly.
      if tbButton.iString >= 65536 then
        RecreateWnd;
   end;
end;

function TMyToolBar.DoQueryDelete(Index: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomizeCanDelete) then
    FOnCustomizeCanDelete(Self, Index, Result);
end;

function TMyToolBar.DoQueryInsert(Index: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomizeCanInsert) then
    FOnCustomizeCanInsert(Self, Index, Result);
end;

procedure TMyToolBar.SaveButtons(Save: Boolean);
var
  SP: TTBSaveParams;
begin
  SP.hkr := THandle(HKEY_CURRENT_USER);
{$IFDEF CLR}
  SP.pszSubKey := FCustomizeKeyName;
  SP.pszValueName := FCustomizeValueName;
{$ELSE}
  SP.pszSubKey := PChar(FCustomizeKeyName);
  SP.pszValueName := PChar(FCustomizeValueName);
{$ENDIF}
  SendStructMessage(Handle, _TB_SAVERESTORE, WPARAM(Save), SP)
end;

procedure TMyToolBar.SetHideClippedButtons(const Value: Boolean);
begin
  if FHideClippedButtons <> Value then
  begin
    FHideClippedButtons := Value;
    RecreateWnd;
  end;
end;

procedure TMyToolBar.SetGradientDirection(Value: TGradientDirection);
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    if HandleAllocated then
      Repaint;
  end;
end;

procedure TMyToolBar.Resize;
begin
  inherited Resize;
  if (gdoGradient in GradientDrawingOptions) and HandleAllocated then
    Repaint;
end;

procedure TMyToolBar.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if (gdoGradient in GradientDrawingOptions) and HandleAllocated then
    Repaint;
end;

{ TMyToolButtonActionLink }

procedure TMyToolButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TMyToolButton;
end;

function TMyToolButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Down = TCustomAction(Action).Checked);
end;

function TMyToolButtonActionLink.IsDropdownMenuLinked: Boolean;
begin
  Result := inherited IsDropdownMenuLinked and
    (FClient.DropdownMenu = TCustomControlAction(Action).DropdownMenu);
end;

function TMyToolButtonActionLink.IsEnableDropdownLinked: Boolean;
begin
  Result := inherited IsEnableDropdownLinked and
    (FClient.EnableDropdown = TCustomControlAction(Action).EnableDropdown);
end;

function TMyToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = TCustomAction(Action).ImageIndex);
end;

procedure TMyToolButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Down := Value;
end;

procedure TMyToolButtonActionLink.SetDropdownMenu(Value: TPopupMenu);
begin
  if IsDropdownMenuLinked then FClient.DropdownMenu := Value;
end;

procedure TMyToolButtonActionLink.SetEnableDropdown(Value: Boolean);
begin
  if IsEnableDropdownLinked then FClient.EnableDropdown := Value;
end;

procedure TMyToolButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;


procedure Register;
begin
  RegisterComponents('My', [TMyToolBar]);
  RegisterClasses([TMyToolButton]);
end;

end.
