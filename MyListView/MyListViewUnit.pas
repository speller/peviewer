unit MyListViewUnit;

interface
uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, Winapi.CommCtrl, System.Classes, Vcl.Forms, Vcl.Controls, Vcl.Menus,
  Vcl.Graphics, Vcl.StdCtrls, Winapi.RichEdit, Vcl.ToolWin, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ListActns,
  Winapi.ShlObj, Vcl.Themes, Vcl.GraphUtil, System.UITypes, Vcl.ComCtrls;


type

  TMyListColumns = class;
  TMyListColumn = class;
  TMyListGroups = class;
  TMyListItems = class;
  TMyListItem = class;
  TMyListItemClass = class of TMyListItem;
  TMyCustomListView = class;
  TMyIconOptions = class;

  TMySortOrder = (soNone, soASC, soDESC);


  TMyLVDeletedEvent = procedure(Sender: TObject; Item: TMyListItem) of object;
  TMyLVEditingEvent = procedure(Sender: TObject; Item: TMyListItem;
    var AllowEdit: Boolean) of object;
  TMyLVEditedEvent = procedure(Sender: TObject; Item: TMyListItem; var S: string) of object;
  TMyLVChangeEvent = procedure(Sender: TObject; Item: TMyListItem;
    Change: TItemChange) of object;
  TMyLVChangingEvent = procedure(Sender: TObject; Item: TMyListItem;
    Change: TItemChange; var AllowChange: Boolean) of object;
  TMyLVColumnClickEvent = procedure(Sender: TObject; Column: TMyListColumn) of object;
  TMyLVColumnRClickEvent = procedure(Sender: TObject; Column: TMyListColumn;
    Point: TPoint) of object;
{$IFDEF CLR}
  TMyLVCompareEvent = procedure(Sender: TObject; Item1, Item2: TMyListItem;
    Data: TTag; var Compare: Integer) of object;
{$ELSE}
  TMyLVCompareEvent = procedure(Sender: TObject; Item1, Item2: TMyListItem;
    Data: Integer; var Compare: Integer) of object;
{$ENDIF}
  TMyLVNotifyEvent = procedure(Sender: TObject; Item: TMyListItem) of object;
  TMyLVSelectItemEvent = procedure(Sender: TObject; Item: TMyListItem;
    Selected: Boolean) of object;
  TMyLVCheckedItemEvent = procedure(Sender: TObject; Item: TMyListItem) of object;
  TMyLVDrawItemEvent = procedure(Sender: TMyCustomListView; Item: TMyListItem;
    Rect: TRect; State: TOwnerDrawState) of object;
  TMyLVCustomDrawEvent = procedure(Sender: TMyCustomListView; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TMyLVCustomDrawItemEvent = procedure(Sender: TMyCustomListView; Item: TMyListItem;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TMyLVCustomDrawSubItemEvent = procedure(Sender: TMyCustomListView; Item: TMyListItem;
    SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TMyLVAdvancedCustomDrawEvent = procedure(Sender: TMyCustomListView; const ARect: TRect;
    Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TMyLVAdvancedCustomDrawItemEvent = procedure(Sender: TMyCustomListView; Item: TMyListItem;
    State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TMyLVAdvancedCustomDrawSubItemEvent = procedure(Sender: TMyCustomListView; Item: TMyListItem;
    SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage;
    var DefaultDraw: Boolean) of object;
  TMyLVOwnerDataEvent = procedure(Sender: TObject; Item: TMyListItem) of object;
  TMyLVOwnerDataFindEvent = procedure(Sender: TObject; Find: TItemFind;
    const FindString: string; const FindPosition: TPoint; FindData: TCustomData;
    StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
    var Index: Integer) of object;
  TMyLVOwnerDataHintEvent = procedure(Sender: TObject; StartIndex, EndIndex: Integer) of object;
  TMyLVOwnerDataStateChangeEvent = procedure(Sender: TObject; StartIndex,
    EndIndex: Integer; OldState, NewState: TItemStates) of object;
  TMyLVSubItemImageEvent = procedure(Sender: TObject; Item: TMyListItem; SubItem: Integer;
    var ImageIndex: Integer) of object;
  TMyLVInfoTipEvent = procedure(Sender: TObject; Item: TMyListItem; var InfoTip: string) of object;
  TMyLVCreateItemClassEvent = procedure(Sender: TMyCustomListView; var ItemClass: TMyListItemClass) of object;


{ TMyCustomListView }

  TMyCustomListView = class(TCustomMultiSelectListControl)
  private
    FCanvas: TCanvas;
    FBorderStyle: TBorderStyle;
    FViewStyle: TViewStyle;
    FReadOnly: Boolean;
    FLargeImages: TCustomImageList;
    FSaveSelectedIndex: Integer;
    FSmallImages: TCustomImageList;
    FStateImages: TCustomImageList;
    FGroupHeaderImages: TCustomImageList;
    FDragImage: TDragImageList;
    FMultiSelect: Boolean;
    FSortType: TSortType;
    FColumnClick: Boolean;
    FShowColumnHeaders: Boolean;
    FListItems: TMyListItems;
    FClicked: Boolean;
    FRClicked: Boolean;
    FIconOptions: TMyIconOptions;
    FHideSelection: Boolean;
    FListColumns: TMyListColumns;
    FMemStream: TMemoryStream;
    FOwnerData: Boolean;
    FOwnerDraw: Boolean;
    FColStream: TMemoryStream;
    FCheckStream: TMemoryStream;
    FDefEditProc: TWindowProcPtr;
    FDefHeaderProc: TWindowProcPtr;
    FEditHandle: HWND;
    FHeaderHandle: HWND;
    FAllocBy: Integer;
    FDragIndex: Integer;
    FLastDropTarget: TMyListItem;
    FCheckboxes: Boolean;
    FFlatScrollBars: Boolean;
    FFullDrag: Boolean;
    FGridLines: Boolean;
    FHotTrack: Boolean;
    FHotTrackStyles: TListHotTrackStyles;
    FRowSelect: Boolean;
    FHoverTime: Integer;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FHeaderChangeLink: TChangeLink;
    FStateChangeLink: TChangeLink;
    FSavedSort: TSortType;
    FReading: Boolean;
    FCanvasChanged: Boolean;
    FTempItem: TMyListItem;
    FWorkAreas: TWorkAreas;
    FShowWorkAreas: Boolean;
    FUpdatingColumnOrder: Boolean;
    FOurFont: Integer;
    FStockFont: Integer;
    FInBufferedPrintClient: Boolean;
    FOwnerDataCount: Integer;
    FPanPoint: TPoint;
    FOnAdvancedCustomDraw: TMyLVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TMyLVAdvancedCustomDrawItemEvent;
    FOnAdvancedCustomDrawSubItem: TMyLVAdvancedCustomDrawSubItemEvent;
    FOnChange: TMyLVChangeEvent;
    FOnChanging: TMyLVChangingEvent;
    FOnColumnClick: TMyLVColumnClickEvent;
    FOnColumnDragged: TNotifyEvent;
    FOnColumnRightClick: TMyLVColumnRClickEvent;
    FOnCompare: TMyLVCompareEvent;
    FOnCustomDraw: TMyLVCustomDrawEvent;
    FOnCustomDrawItem: TMyLVCustomDrawItemEvent;
    FOnCustomDrawSubItem: TMyLVCustomDrawSubItemEvent;
    FOnData: TMyLVOwnerDataEvent;
    FOnDataFind: TMyLVOwnerDataFindEvent;
    FOnDataHint: TMyLVOwnerDataHintEvent;
    FOnDataStateChange: TMyLVOwnerDataStateChangeEvent;
    FOnDeletion: TMyLVDeletedEvent;
    FOnDrawItem: TMyLVDrawItemEvent;
    FOnEdited: TMyLVEditedEvent;
    FOnEditing: TMyLVEditingEvent;
    FOnGetImageIndex: TMyLVNotifyEvent;
    FOnGetSubItemImage: TMyLVSubItemImageEvent;
    FOnInfoTip: TMyLVInfoTipEvent;
    FOnInsert: TMyLVDeletedEvent;
    FOnSelectItem: TMyLVSelectItemEvent;
    FOnItemChecked: TMyLVCheckedItemEvent;
    FOnCreateItemClass: TMyLVCreateItemClassEvent;
    FListGroups: TMyListGroups;
    FGroupView: Boolean;
{$IFDEF CLR}
    FLVInstances: TMyLVInstances;
{$ELSE}
    FEditInstance: Pointer;
    FHeaderInstance: Pointer;
{$ENDIF}
    FSavedIndents: array of Integer;
    FDeletingAllItems: Boolean;
    FHeaderImgList: TCustomImageList;
    class constructor Create;
    procedure SaveIndents;
    procedure RestoreIndents;
    function AreItemsStored: Boolean;
    procedure CanvasChanged(Sender: TObject);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotifyLV); message CN_NOTIFY;
//    procedure DoAutoSize;
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    procedure DrawWorkAreas;
    procedure EditWndProc(var Message: TMessage);
    function GetBoundingRect: TRect;
    function GetColumnFromIndex(Index: Integer): TMyListColumn;
    function GetColumnFromTag(Tag: Integer): TMyListColumn;
    function GetDropTarget: TMyListItem;
    function GetFocused: TMyListItem;
    procedure GetImageIndex(Item: TMyListItem);
    procedure GetSubItemImage(Item: TMyListItem; SubItem: Integer; var ImageIndex: Integer);
    function GetItem(Value: TLVItem): TMyListItem;
    function GetSelected: TMyListItem;
    function GetTopItem: TMyListItem;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;
    function GetHoverTime: Integer;
    procedure HeaderWndProc(var Message: TMessage);
    procedure ImageListChange(Sender: TObject);
    procedure RestoreChecks;
    procedure SaveChecks;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnClick(Value: Boolean);
    procedure SetColumnHeaders(Value: Boolean);
    procedure SetDropTarget(Value: TMyListItem);
    procedure SetFocused(Value: TMyListItem);
    procedure SetHideSelection(Value: Boolean);
    procedure SeTMyIconOptions(Value: TMyIconOptions);
    procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetGroupHeaderImages(Value: TCustomImageList);
    procedure SetAllocBy(Value: Integer);
    procedure SetItems(Value: TMyListItems);
    procedure SetListColumns(Value: TMyListColumns);
    procedure SetListGroups(Value: TMyListGroups);
    procedure SetOwnerData(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowWorkAreas(const Value: Boolean);
    procedure SetSmallImages(Value: TCustomImageList);
    procedure SetSortType(Value: TSortType);
    procedure SetSelected(Value: TMyListItem);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetTextBkColor(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFlatScrollBars(Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetGridLines(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TListHotTrackStyles);
    procedure SetRowSelect(Value: Boolean);
    procedure SetHoverTime(Value: Integer);
    procedure SetGroupView(Value: Boolean);
    procedure ResetExStyles;
    function ValidHeaderHandle: Boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCtlColorEdit(var Message: TMessage); message WM_CTLCOLOREDIT;
{$IFDEF CLR}
    function CustomListViewSort(AnItem1, AnItem2: Longint; lParam: Integer): Integer;
    function DefaultListViewSort(Item1, Item2: TMyListItem; lParam: TTag): Integer;
{$ENDIF}
    function StoreGroups: Boolean;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function CanChange(Item: TMyListItem; Change: Integer): Boolean; dynamic;
    function CanEdit(Item: TMyListItem): Boolean; dynamic;
    procedure Change(Item: TMyListItem; Change: Integer); dynamic;
    procedure ChangeScale(M, D: Integer); override;
    procedure ColClick(Column: TMyListColumn); dynamic;
    procedure ColRightClick(Column: TMyListColumn; Point: TPoint); dynamic;
    function ColumnsShowing: Boolean;
    function CreateListItem: TMyListItem; virtual;
    function CreateListItems: TMyListItems; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Item: TMyListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawSubItem(Item: TMyListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Delete(Item: TMyListItem); dynamic;
    procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoInfoTip(Item: TMyListItem; var InfoTip: string); virtual;
    procedure DrawItem(Item: TMyListItem; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure Edit(const Item: TLVItem); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function OwnerDataFetch(Item: TMyListItem; Request: TItemRequest): Boolean; virtual;
    function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: TCustomData; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; virtual;
    function OwnerDataHint(StartIndex, EndIndex: Integer): Boolean; virtual;
    function OwnerDataStateChange(StartIndex, EndIndex: Integer; OldState,
      NewState: TItemStates): Boolean; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetItemIndex(Value: TMyListItem): Integer; reintroduce; overload;
    function GetItemIndex: Integer; reintroduce; overload; override;
    function GetSelCount: Integer; override;
    procedure InsertItem(Item: TMyListItem); dynamic;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetMultiSelect(Value: Boolean); override;
    procedure SetViewStyle(Value: TViewStyle); virtual;
    procedure UpdateColumn(AnIndex: Integer);
    procedure UpdateColumns;
    procedure UpdateGroup(AnIndex: Integer);
    procedure UpdateGroups;
    procedure WndProc(var Message: TMessage); override;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Groups: TMyListGroups read FListGroups write SetListGroups stored StoreGroups;
    property Columns: TMyListColumns read FListColumns write SetListColumns;
    property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property IconOptions: TMyIconOptions read FIconOptions write SeTMyIconOptions;
    property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
    property GroupView: Boolean read FGroupView write SetGroupView default False;
    property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property GroupHeaderImages: TCustomImageList read FGroupHeaderImages write SetGroupHeaderImages;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property OnAdvancedCustomDraw: TMyLVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TMyLVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem: TMyLVAdvancedCustomDrawSubItemEvent read FOnAdvancedCustomDrawSubItem write FOnAdvancedCustomDrawSubItem;
    property OnChange: TMyLVChangeEvent read FOnChange write FOnChange;
    property OnChanging: TMyLVChangingEvent read FOnChanging write FOnChanging;
    property OnColumnClick: TMyLVColumnClickEvent read FOnColumnClick
      write FOnColumnClick;
    property OnColumnDragged: TNotifyEvent read FOnColumnDragged write FOnColumnDragged;
    property OnColumnRightClick: TMyLVColumnRClickEvent read FOnColumnRightClick
      write FOnColumnRightClick;
    property OnCompare: TMyLVCompareEvent read FOnCompare write FOnCompare;
    property OnCreateItemClass: TMyLVCreateItemClassEvent read FOnCreateItemClass write FOnCreateItemClass;
    property OnCustomDraw: TMyLVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TMyLVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnCustomDrawSubItem: TMyLVCustomDrawSubItemEvent read FOnCustomDrawSubItem write FOnCustomDrawSubItem;
    property OnData: TMyLVOwnerDataEvent read FOnData write FOnData;
    property OnDataFind: TMyLVOwnerDataFindEvent read FOnDataFind write FOnDataFind;
    property OnDataHint: TMyLVOwnerDataHintEvent read FOnDataHint write FOnDataHint;
    property OnDataStateChange: TMyLVOwnerDataStateChangeEvent read FOnDataStateChange write FOnDataStateChange;
    property OnDeletion: TMyLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnDrawItem: TMyLVDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEdited: TMyLVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TMyLVEditingEvent read FOnEditing write FOnEditing;
    property OnInfoTip: TMyLVInfoTipEvent read FOnInfoTip write FOnInfoTip;
    property OnInsert: TMyLVDeletedEvent read FOnInsert write FOnInsert;
    property OnGetImageIndex: TMyLVNotifyEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSubItemImage: TMyLVSubItemImageEvent read FOnGetSubItemImage write FOnGetSubItemImage;
    property OnSelectItem: TMyLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnItemChecked: TMyLVCheckedItemEvent read FOnItemChecked write FOnItemChecked;
    property Reading: Boolean read FReading;
    property ShowColumnHeaders: Boolean read FShowColumnHeaders write
      SetColumnHeaders default True;
    property ShowWorkAreas: Boolean read FShowWorkAreas write SetShowWorkAreas default False;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsIcon;
    property HeaderImages: TCustomImageList read FHeaderImgList write FHeaderImgList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(Item: String; AObject: TObject); override;
    function AlphaSort: Boolean;
    procedure Arrange(Code: TListArrangement);
    procedure Clear; override;
    procedure ClearSelection; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure DeleteSelected; override;
    function FindCaption(StartIndex: Integer; Value: string;
      Partial, Inclusive, Wrap: Boolean): TMyListItem;
    function FindData(StartIndex: Integer; Value: TCustomData;
      Inclusive, Wrap: Boolean): TMyListItem;
    function GetCount: Integer; override;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetItemAt(X, Y: Integer): TMyListItem;
    function GetNearestItem(Point: TPoint;
      Direction: TSearchDirection): TMyListItem;
    function GetNextItem(StartItem: TMyListItem;
      Direction: TSearchDirection; States: TItemStates): TMyListItem;
    function GetSearchString: string;
    function IsEditing: Boolean;
    procedure SelectAll; override;
    procedure Scroll(DX, DY: Integer);
    procedure ResortItems(AProc: TLVCompare = nil);
    procedure UpdateHeaderState;
{$IFDEF CLR}
    function CustomSort(SortProc: TLVCompareProc; Data: TTag): Boolean;
{$ELSE}
    function CustomSort(SortProc: TLVCompare; lParam: LPARAM): Boolean;
{$ENDIF}
    property Canvas: TCanvas read FCanvas;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes default False;
    property Column[Index: Integer]: TMyListColumn read GetColumnFromIndex;
    property DropTarget: TMyListItem read GetDropTarget write SetDropTarget;
    property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False;
    property FullDrag: Boolean read FFullDrag write SetFullDrag default False;
    property GridLines: Boolean read FGridLines write SetGridLines default False;
    function GetDragImages: TDragImageList; override;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HotTrackStyles: TListHotTrackStyles read FHotTrackStyles write SetHotTrackStyles default [];
    property ItemFocused: TMyListItem read GetFocused write SetFocused;
    property Items: TMyListItems read FListItems write SetItems stored AreItemsStored;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property SelCount: Integer read GetSelCount;
    property Selected: TMyListItem read GetSelected write SetSelected;
    function StringWidth(S: string): Integer;
    procedure UpdateItems(FirstIndex, LastIndex: Integer);
    property TopItem: TMyListItem read GetTopItem;
    property ViewOrigin: TPoint read GetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property BoundingRect: TRect read GetBoundingRect;
    property WorkAreas: TWorkAreas read FWorkAreas;
    procedure DoAutoSize;
    procedure FixColumnsWidth;
  end;

{ TMyListView }

  TMyListView = class(TMyCustomListView)
  strict private
    class constructor Create;
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property Groups;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property GroupHeaderImages;
    property GroupView default False;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnCreateItemClass;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnItemChecked;
    property OnStartDock;
    property OnStartDrag;
    property HeaderImages;
  end;


  { TMyListItem }

  TMyListItem = class(TPersistent)
  private
    FOwner: TMyListItems;
    FSubItems: TStrings;
    FImageIndex: TImageIndex;
    FIndent: Integer;
    FIndex: Integer;
    FOverlayIndex: TImageIndex;
    FStateIndex: TImageIndex;
    FCaption: string;
    FDeleting: Boolean;
    FProcessedDeleting: Boolean;
    FChecked: Boolean;
    FData: TCustomData;
    FGroupID: Integer;
    function GetChecked: Boolean;
    function GetHandle: HWND;
    function GetIndex: Integer;
    function GetListView: TMyCustomListView;
    function GetLeft: Integer;
    function GetState(Index: Integer): Boolean;
    function GetTop: Integer;
    function IsEqual(Item: TMyListItem): Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetData(Value: TCustomData);
    procedure SetImage(Index: Integer; Value: TImageIndex);
    procedure SetIndent(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetState(Index: Integer; State: Boolean);
    procedure SetSubItems(Value: TStrings);
    procedure SetTop(Value: Integer);
    function GetSubItemImage(Index: Integer): Integer;
    procedure SetSubItemImage(Index: Integer; const Value: Integer);
    procedure SetGroupID(Value: Integer);
  public
    constructor Create(AOwner: TMyListItems); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CancelEdit;
    procedure Delete;
    function DisplayRect(Code: TDisplayCode): TRect;
    function EditCaption: Boolean;
    function GetPosition: TPoint;
    procedure MakeVisible(PartialOK: Boolean);
    procedure Update;
    procedure SetPosition(const Value: TPoint);
    function WorkArea: Integer;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Cut: Boolean index 0 read GetState write SetState;
    property Data: TCustomData read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property DropTarget: Boolean index 1 read GetState write SetState;
    property Focused: Boolean index 2 read GetState write SetState;
    property GroupID: Integer read FGroupID write SetGroupID default -1;
    property Handle: HWND read GetHandle;
    property ImageIndex: TImageIndex index 0 read FImageIndex write SetImage;
    property Indent: Integer read FIndent write SetIndent default 0;
    property Index: Integer read GetIndex;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TMyCustomListView read GetListView;
    property Owner: TMyListItems read FOwner;
    property OverlayIndex: TImageIndex index 1 read FOverlayIndex write SetImage;
    property Position: TPoint read GetPosition write SetPosition;
    property Selected: Boolean index 3 read GetState write SetState;
    property StateIndex: TImageIndex index 2 read FStateIndex write SetImage;
    property SubItems: TStrings read FSubItems write SetSubItems;
    property SubItemImages[Index: Integer]: Integer read GetSubItemImage write SetSubItemImage;
    property Top: Integer read GetTop write SetTop;
  end;

{ TMyListItems }

  TMyListItems = class(TPersistent)
  private
    FOwner: TMyCustomListView;
    FUpdateCount: Integer;
    FNoRedraw: Boolean;
    procedure ReadData(Stream: TStream);
    procedure ReadItemData(Stream: TStream);
    procedure WriteItemData(Stream: TStream);
  protected
{$IFDEF CLR}
    FItemHashTable: HashTable;
{$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    function CreateItem(Index: Integer; ListItem: TMyListItem): TLVItem;
    function GetCount: Integer;
    function GetHandle: HWND;
    function GetItem(Index: Integer): TMyListItem;
    procedure SetCount(Value: Integer);
    procedure SetItem(Index: Integer; Value: TMyListItem);
    procedure SetUpdateState(Updating: Boolean);
  public
    constructor Create(AOwner: TMyCustomListView);
    destructor Destroy; override;
    function Add: TMyListItem;
    function AddItem(Item: TMyListItem; Index: Integer = -1): TMyListItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
//    function GetEnumerator: TMyListItemsEnumerator;
    function IndexOf(Value: TMyListItem): Integer;
    function Insert(Index: Integer): TMyListItem;
    property Count: Integer read GetCount write SetCount;
    property Handle: HWND read GetHandle;
    property Item[Index: Integer]: TMyListItem read GetItem write SetItem; default;
    property Owner: TMyCustomListView read FOwner;
  end;

  TMyListGroup = class(TCollectionItem)
  private
    FHeader: string;
    FFooter: string;
    FGroupID: Integer;
    FState: TListGroupstateSet;
    FHeaderAlign: TAlignment;
    FFooterAlign: TAlignment;
    FSubtitle: string;
    FDescriptionTop: string;
    FDescriptionBottom: string;
    FTitleImage: TImageIndex;
    procedure SetHeader(Value: string);
    procedure SetFooter(Value: string);
    procedure SetGroupID(Value: Integer);
    procedure SetState(Value: TListGroupstateSet);
    function GetState: TListGroupstateSet;
    procedure SetHeaderAlign(Value: TAlignment);
    procedure SetFooterAlign(Value: TAlignment);
    procedure SetSubtitle(Value: string);
    procedure SetTitleImage(Value: TImageIndex);
    procedure ReadDescriptionTop(Reader: TReader);
    procedure ReadDescriptionBottom(Reader: TReader);
    procedure IgnoreInt(Reader: TReader);
    procedure IgnoreString(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Header: string read FHeader write SetHeader;
    property Footer: string read FFooter write SetFooter;
    property GroupID: Integer read FGroupID write SetGroupID;
    property State: TListGroupstateSet read GetState write SetState;
    property HeaderAlign: TAlignment read FHeaderAlign write SetHeaderAlign;
    property FooterAlign: TAlignment read FFooterAlign write SetFooterAlign;
    property Subtitle: string read FSubtitle write SetSubtitle;

    property TitleImage: TImageIndex read FTitleImage write SetTitleImage;
  end;

  TMyListGroups = class(TCollection)
  private
    FOwner: TMyCustomListView;
    function GetItem(Index: Integer): TMyListGroup;
    procedure SetItem(Index: Integer; Value: TMyListGroup);
    procedure UpdateGroups;
    function GetNextGroupID: Integer;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TMyCustomListView);
    function Add: TMyListGroup;
    function Owner: TMyCustomListView;
    property Items[Index: Integer]: TMyListGroup read GetItem write SetItem; default;
    property NextGroupID: Integer read GetNextGroupID;
  end;

  TMyListColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: string;
    FMaxWidth: TWidth;
    FMinWidth: TWidth;
    FImageIndex: TImageIndex;
    FPrivateWidth: TWidth;
    FWidth: TWidth;
    FOrderTag,
    FTag: Integer;

    FSortOrder: TMySortOrder;
    FListView: TMyCustomListView;
    FArrowOnRight: Boolean;
//    FImgList: TCustomImageList;

    procedure DoChange;
    function GetWidth: TWidth;
    function IsWidthStored: Boolean;
    procedure ReadData(Reader: TReader);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetMaxWidth(Value: TWidth);
    procedure SetMinWidth(Value: TWidth);
    procedure SetWidth(Value: TWidth);
    procedure WriteData(Writer: TWriter);
    procedure SetSortOrder(const Value: TMySortOrder);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure SetCollection(Value: TCollection); override;
    procedure UpdateHeaderSortState;
    procedure UpdateItemsSort;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property WidthType: TWidth read FWidth;
    property ArrowOnRight: Boolean read FArrowOnRight write FArrowOnRight;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read FMinWidth write SetMinWidth default 0;
    property Tag: Integer read FTag write FTag default 0;
    property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
    property SortOrder: TMySortOrder read FSortOrder write SetSortOrder;
  end;

  TMyListColumns = class(TCollection)
  private
    FOwner: TMyCustomListView;
    function GetItem(Index: Integer): TMyListColumn;
    procedure SetItem(Index: Integer; Value: TMyListColumn);
    procedure UpdateCols;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TMyCustomListView);
    function Add: TMyListColumn;
    function Owner: TMyCustomListView;
    property Items[Index: Integer]: TMyListColumn read GetItem write SetItem; default;
  end;

{ TMyIconOptions }

  TIconArrangement = (iaTop, iaLeft);

  TMyIconOptions = class(TPersistent)
  private
    FListView: TMyCustomListView;
    FArrangement: TIconArrangement;
    FAutoArrange: Boolean;
    FWrapText: Boolean;
    procedure SetArrangement(Value: TIconArrangement);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetWrapText(Value: Boolean);
  public
    constructor Create(AOwner: TMyCustomListView);
  published
    property Arrangement: TIconArrangement read FArrangement write SetArrangement default iaTop;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property WrapText: Boolean read FWrapText write SetWrapText default True;
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


{ TSubItems }

type
  TSubItems = class(TStringList)
  private
    FOwner: TMyListItem;
    FImageIndices: TList;
    procedure SetColumnWidth(Index: Integer);
    procedure RefreshItem(Index: Integer);
    function GetImageIndex(Index: Integer): TImageIndex;
    procedure SetImageIndex(Index: Integer; const Value: TImageIndex);
  protected
    function GetHandle: HWND;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AOwner: TMyListItem);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    property Handle: HWND read GetHandle;
    property Owner: TMyListItem read FOwner;
    property ImageIndex[Index: Integer]: TImageIndex read GetImageIndex write SetImageIndex;
  end;

constructor TSubItems.Create(AOwner: TMyListItem);
begin
  inherited Create;
  FOwner := AOwner;
  FImageIndices := TList.Create;
end;

destructor TSubItems.Destroy;
begin
  FImageIndices.Free;
  inherited;
end;

function TSubItems.Add(const S: string): Integer;
begin
  Result := inherited Add(S);
{$IFDEF CLR}
  FImageIndices.Add(TObject(TImageIndex(-1)));
{$ELSE}
  FImageIndices.Add(Pointer(-1));
{$ENDIF}
  RefreshItem(Result + 1);
end;

function TSubItems.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
{$IFDEF CLR}
  FImageIndices.Add(TObject(TImageIndex(-1)));
{$ELSE}
  FImageIndices.Add(Pointer(-1));
{$ENDIF}
  RefreshItem(Result + 1);
end;

procedure TSubItems.Clear;
begin
  inherited;
  FImageIndices.Clear;
end;

procedure TSubItems.Delete(Index: Integer);
begin
  inherited;
  FImageIndices.Delete(Index);
  Owner.Update;
end;

function TSubItems.GetHandle: HWND;
begin
  Result := Owner.Owner.Handle;
end;

procedure TSubItems.SetColumnWidth(Index: Integer);
var
  ListView: TMyCustomListView;
begin
  ListView := Owner.ListView;
  if ListView.ColumnsShowing and
    (ListView.Columns.Count > Index) and
    (ListView.Column[Index].WidthType = ColumnTextWidth) then
    ListView.UpdateColumn(Index);
end;

procedure TSubItems.Insert(Index: Integer; const S: string);
var
  i: Integer;
begin
  inherited Insert(Index, S);
{$IFDEF CLR}
  FImageIndices.Insert(Index, TObject(TImageIndex(-1)));
{$ELSE}
  FImageIndices.Insert(Index, Pointer(-1));
{$ENDIF}
  for i := Index + 1 to Count do
    RefreshItem(i);
end;

procedure TSubItems.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  RefreshItem(Index + 1);
end;

procedure TSubItems.RefreshItem(Index: Integer);
begin
{$IFDEF CLR}
  ListView_SetItemText(Handle, Owner.Index, Index, IntPtr(LPSTR_TEXTCALLBACK));
{$ELSE}
  ListView_SetItemText(Handle, Owner.Index, Index, LPSTR_TEXTCALLBACK);
{$ENDIF}
  SetColumnWidth(Index);
end;

procedure TSubItems.SetUpdateState(Updating: Boolean);
begin
  Owner.Owner.SetUpdateState(Updating);
end;

function TSubItems.GetImageIndex(Index: Integer): TImageIndex;
begin
  Result := TImageIndex(FImageIndices[Index]);
end;

procedure TSubItems.SetImageIndex(Index: Integer; const Value: TImageIndex);
begin
{$IFDEF CLR}
  FImageIndices[Index] := TObject(Value);
{$ELSE}
  FImageIndices[Index] := Pointer(Value);
{$ENDIF}
end;


{ TMyCustomListView }

{$IFNDEF CLR}
function DefaultListViewSort(Item1, Item2: TMyListItem;
  lParam: Integer): Integer; stdcall;
begin
  with Item1 do
    if Assigned(ListView.OnCompare) then
      ListView.OnCompare(ListView, Item1, Item2, lParam, Result)
    else Result := lstrcmp(PChar(Item1.Caption), PChar(Item2.Caption));
end;
{$ENDIF}

class constructor TMyCustomListView.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TMyCustomListView, TListViewStyleHook);
end;

constructor TMyCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse] +
    [csDisplayDragImage, csReflector, csPannable];
  Width := 250;
  Height := 150;
  BorderStyle := bsSingle;
  ViewStyle := vsIcon;
  ParentColor := False;
  TabStop := True;
  HideSelection := True;
  ShowColumnHeaders := True;
  ColumnClick := True;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FDragIndex := -1;
  FListColumns := TMyListColumns.Create(Self);
  FListGroups := TMyListGroups.Create(Self);
  FListItems := CreateListItems;
  FTempItem := CreateListItem;
  FGroupView := False;
  FIconOptions := TMyIconOptions.Create(Self);
  FWorkAreas := TWorkAreas.Create(Self, TWorkArea);
  FShowWorkAreas := False;
  FUpdatingColumnOrder := False;
  FOwnerDataCount := 0;
  FHoverTime := -1;
  FDragImage := TDragImageList.CreateSize(32, 32);
{$IFDEF CLR}
  FLVInstances := TLVInstances.Create;
  FLVInstances.FEditInstance := MakeObjectInstance(EditWndProc);
  FLVInstances.FHeaderIstance := MakeObjectInstance(HeaderWndProc);
{$ELSE}
  FEditInstance := MakeObjectInstance(EditWndProc);
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
{$ENDIF}
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := ImageListChange;
  FHeaderChangeLink := TChangeLink.Create;
  FHeaderChangeLink.OnChange := ImageListChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := ImageListChange;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := ImageListChange;
  FSaveSelectedIndex := -1;
  FInBufferedPrintClient := False;
end;

destructor TMyCustomListView.Destroy;
begin
  if HandleAllocated then DestroyWindowHandle;
  FDragImage.Free;
  FListColumns.Free;
  FTempItem.Free;
  FListGroups.Free;
  FListItems.Free;
  FIconOptions.Free;
  FMemStream.Free;
  FColStream.Free;
  FCheckStream.Free;
  FWorkAreas.Free;
{$IFDEF CLR}
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, THandle(FDefHeaderProc));
  if Assigned(FLVInstances) then
  begin
    if Assigned(FLVInstances.FEditInstance) then
    begin
      FreeObjectInstance(FLVInstances.FEditInstance);
      FLVInstances.FEditInstance := nil;
    end;
    if Assigned(FLVInstances.FHeaderInstance) then
    begin
      FreeObjectInstance(FLVInstances.FHeaderInstance);
      FLVInstances.FHeaderInstance := nil;
    end;
    System.GC.SuppressFinalize(FLVInstances);
    FreeAndNil(FLVInstances);
  end;
{$ELSE}
  FreeObjectInstance(FEditInstance);
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, LPARAM(FDefHeaderProc));
  FreeObjectInstance(FHeaderInstance);
{$ENDIF}
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FStateChangeLink.Free;
  FHeaderChangeLink.Free;
  FCanvas.Free;
  FCanvas := nil;
  inherited Destroy;
end;

procedure TMyCustomListView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  EditStyles: array[Boolean] of DWORD = (LVS_EDITLABELS, 0);
  MultiSelections: array[Boolean] of DWORD = (LVS_SINGLESEL, 0);
  HideSelections: array[Boolean] of DWORD = (LVS_SHOWSELALWAYS, 0);
  Arrangements: array[TIconArrangement] of DWORD = (LVS_ALIGNTOP,
    LVS_ALIGNLEFT);
  AutoArrange: array[Boolean] of DWORD = (0, LVS_AUTOARRANGE);
  WrapText: array[Boolean] of DWORD = (LVS_NOLABELWRAP, 0);
  ViewStyles: array[TViewStyle] of DWORD = (LVS_ICON, LVS_SMALLICON,
    LVS_LIST, LVS_REPORT);
  ShowColumns: array[Boolean] of DWORD = (LVS_NOCOLUMNHEADER, 0);
  ColumnClicks: array[Boolean] of DWORD = (LVS_NOSORTHEADER, 0);
begin
  InitCommonControl(ICC_LISTVIEW_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_LISTVIEW);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or ViewStyles[ViewStyle] or
      BorderStyles[BorderStyle] or Arrangements[IconOptions.Arrangement] or
      EditStyles[ReadOnly] or MultiSelections[MultiSelect] or
      HideSelections[HideSelection] or
      AutoArrange[IconOptions.AutoArrange] or
      WrapText[IconOptions.WrapText] or
      ShowColumns[ShowColumnHeaders] or
      ColumnClicks[ColumnClick] or
      LVS_SHAREIMAGELISTS;
    if FOwnerData then Style := Style or LVS_OWNERDATA;
    if FOwnerDraw then Style := Style or LVS_OWNERDRAWFIXED;
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TMyCustomListView.CreateWnd;

  procedure ReadCols;
  var
    Reader: TReader;
  begin
    if FColStream = nil then Exit;
    Columns.Clear;
    Reader := TReader.Create(FColStream, 1024);
    try
      Reader.ReadValue;
      Reader.ReadCollection(Columns);
    finally
      Reader.Free;
    end;
{$IFNDEF CLR}
    FColStream.Destroy;
{$ENDIF}
    FColStream := nil;
  end;

begin
  inherited CreateWnd;
  ResetExStyles;
  SetTextBKColor(Color);
  SetTextColor(Font.Color);
  SetAllocBy(AllocBy);
  HoverTime := FHoverTime;
  if FMemStream <> nil then
  begin
    Items.BeginUpdate;
    FReading := True;
    try
      Columns.Clear;
      FMemStream.Position := 0;
      FMemStream.ReadComponent(Self);
{$IFNDEF CLR}
      FMemStream.Destroy;
{$ENDIF}
      FMemStream := nil;
      if OwnerData then
        Items.Count := FOwnerDataCount;
      if FCheckboxes then
        RestoreChecks;
      ReadCols;
      Font := Font;
      if FSaveSelectedIndex <> -1 then
      begin
        Selected := Items[FSaveSelectedIndex];
        if Selected <> nil then
          Selected.MakeVisible(False);
      end;
      RestoreIndents;
    finally
      Items.EndUpdate;
      FReading := False;
    end;
  end;
  Columns.UpdateCols;

  ListView_EnableGroupView(Handle, FGroupView);
  UpdateGroups;

  if (LargeImages <> nil) and LargeImages.HandleAllocated then
    SetImageList(LargeImages.Handle, LVSIL_NORMAL);
  if (SmallImages <> nil) and SmallImages.HandleAllocated then
    SetImageList(SmallImages.Handle, LVSIL_SMALL);
  if (StateImages <> nil) and StateImages.HandleAllocated then
    SetImageList(StateImages.Handle, LVSIL_STATE);
  if (GroupHeaderImages <> nil) and GroupHeaderImages.HandleAllocated then
    SetImageList(GroupHeaderImages.Handle, LVSIL_GROUPHEADER);
  DoAutoSize;
  if StyleServices.Enabled and TOSVersion.Check(6) and StyleServices.IsSystemStyle then
    SetWindowTheme(Handle, 'explorer', nil); // do not localize
end;

procedure TMyCustomListView.DestroyWnd;
begin
  if (csRecreating in ControlState) then
  begin
    if FMemStream = nil then
      FMemStream := TMemoryStream.Create
    else
      FMemStream.Size := 0;
    if OwnerData then FOwnerDataCount := Items.Count;
    FMemStream.WriteComponent(Self);
    FMemStream.Position := 0;
    if FCheckboxes then
      SaveChecks;
    if Assigned(Selected) then
      FSaveSelectedIndex := Selected.Index
    else
      FSaveSelectedIndex := -1;
    SaveIndents;
  end;
  inherited DestroyWnd;
end;

procedure TMyCustomListView.SetImageList(Value: HImageList; Flags: Integer);
begin
  if HandleAllocated then ListView_SetImageList(Handle, Value, Flags);
end;

function TMyCustomListView.StoreGroups: Boolean;
begin
  Result := FListGroups.Count > 0;
end;

procedure TMyCustomListView.ImageListChange(Sender: TObject);
var
  ImageHandle: HImageList;
begin
  if HandleAllocated then
  begin
    if TCustomImageList(Sender).HandleAllocated then
      ImageHandle := TCustomImageList(Sender).Handle
    else
      ImageHandle := 0;
    if Sender = LargeImages then SetImageList(ImageHandle, LVSIL_NORMAL)
    else if Sender = SmallImages then SetImageList(ImageHandle, LVSIL_SMALL)
    else if Sender = StateImages then SetImageList(ImageHandle, LVSIL_STATE)
    else if Sender = StateImages then SetImageList(ImageHandle, LVSIL_GROUPHEADER);
  end;
end;

procedure TMyCustomListView.Loaded;
begin
  inherited;
  if HandleAllocated then
  begin
    ListView_EnableGroupView(Handle, FGroupView);
    UpdateGroups;
  end;
end;

procedure TMyCustomListView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = LargeImages then LargeImages := nil;
    if AComponent = SmallImages then SmallImages := nil;
    if AComponent = StateImages then StateImages := nil;
    if AComponent = GroupHeaderImages then GroupHeaderImages := nil;
  end;
end;

procedure TMyCustomListView.HeaderWndProc(var Message: TMessage);

  procedure UpdateColumnOrder;
  var
    I: Integer;
    ColumnOrder: TIntegerDynArray;
  begin
    SetLength(ColumnOrder, Columns.Count);
{$IFDEF CLR}
    ListView_GetColumnOrderArray(Handle, Columns.Count, ColumnOrder);
{$ELSE}
    ListView_GetColumnOrderArray(Handle, Columns.Count, PInteger(ColumnOrder));
{$ENDIF}
    FListColumns.BeginUpdate;
    try
      for I := 0 to FListColumns.Count - 1 do
        GetColumnFromTag(ColumnOrder[I]).Index := I;
      if Assigned(FOnColumnDragged) then FOnColumnDragged(Self);
    finally
      FListColumns.EndUpdate;
      FUpdatingColumnOrder := False;
    end;
  end;

  procedure DoBufferedPaint(DC: HDC; Rect: TRect);
  var
    MemDC: HDC;
    PaintBuffer: HPAINTBUFFER;
  begin
    PaintBuffer := BeginBufferedPaint(DC, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      SendMessage(FHeaderHandle, WM_PRINTCLIENT, MemDC, PRF_CLIENT);
      BufferedPaintMakeOpaque(PaintBuffer, Rect);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end;

var
  DC: HDC;
  PS: TPaintStruct;
  Form: TCustomForm;
begin
  try
    with Message do
    begin
      case Msg of
        WM_CAPTURECHANGED:
          if FUpdatingColumnOrder then UpdateColumnOrder;
        WM_NCHITTEST:
          with TWMNCHitTest(Message) do
            if csDesigning in ComponentState then
            begin
              Result := Winapi.Windows.HTTRANSPARENT;
              Exit;
            end;
        WM_NCDESTROY:
          begin
            Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg, WParam, LParam);
            FHeaderHandle := 0;
            FDefHeaderProc := nil;
            Exit;
          end;
        WM_PAINT:
          begin
            if DoubleBuffered and DwmCompositionEnabled then
            begin
              Form := GetParentForm(Self);
              if (Form <> nil) and Form.GlassFrame.FrameExtended and
                Form.GlassFrame.IntersectsControl(Self) then
              begin
                DC := BeginPaint(FHeaderHandle, PS);
                try
                  DoBufferedPaint(DC, PS.rcPaint);
                finally
                  EndPaint(FHeaderHandle, PS);
                end;
                Exit;
              end;
            end;
          end;
      end;
      Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TMyCustomListView.EditWndProc(var Message: TMessage);
var
  LWindowRect: TRect;
{$IFDEF CLR}
  KeyMsg: TWMKey;
{$ENDIF}
begin
  try
    with Message do
    begin
      case Msg of
        WM_KEYDOWN,
        WM_SYSKEYDOWN:
          begin
{$IFDEF CLR}
            KeyMsg := TWMKey.Create(Message);
            if DoKeyDown(KeyMsg) then
{$ELSE}
            if DoKeyDown(TWMKey(Message)) then
{$ENDIF}
              Exit;
          end;
        WM_CHAR:
          begin
{$IFDEF CLR}
            KeyMsg := TWMKey.Create(Message);
            if DoKeyPress(KeyMsg) then
{$ELSE}
            if DoKeyPress(TWMKey(Message)) then
{$ENDIF}
              Exit;
          end;
        WM_KEYUP,
        WM_SYSKEYUP:
          begin
{$IFDEF CLR}
            KeyMsg := TWMKey.Create(Message);
            if DoKeyUp(KeyMsg) then
{$ELSE}
            if DoKeyUp(TWMKey(Message)) then
{$ENDIF}
              Exit;
          end;
        CN_KEYDOWN,
        CN_CHAR, CN_SYSKEYDOWN,
        CN_SYSCHAR:
          begin
            WndProc(Message);
            Exit;
          end;
        CM_BUFFEREDPRINTCLIENT:
          if FInBufferedPrintClient then
          begin
            GetWindowRect(FEditHandle, LWindowRect);
            MapWindowPoints(0, FEditHandle, LWindowRect, 2);
            PerformBufferedPrintClient(FEditHandle, LWindowRect);
            FInBufferedPrintClient := False;
            Exit;
          end;
      end;
      Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TMyCustomListView.UpdateItems(FirstIndex, LastIndex: Integer);
begin
  ListView_RedrawItems(Handle, FirstIndex, LastIndex);
end;

procedure TMyCustomListView.ResetExStyles;
var
  Styles: DWORD;
  TempImages: TCustomImageList;
begin
  if HandleAllocated then
  begin
    TempImages := nil;
    if StateImages <> nil then
    begin
      TempImages := StateImages;
      StateImages := nil;
    end;
    Styles := LVS_EX_SUBITEMIMAGES or LVS_EX_INFOTIP;
    if FCheckboxes then Styles := Styles or LVS_EX_CHECKBOXES;
    if FGridLines then Styles := Styles or LVS_EX_GRIDLINES;
    if FHotTrack then Styles := Styles or LVS_EX_TRACKSELECT;
    if FRowSelect then Styles := Styles or LVS_EX_FULLROWSELECT;
    if FFlatScrollBars then Styles := Styles or LVS_EX_FLATSB;
    if FFullDrag then Styles := Styles or LVS_EX_HEADERDRAGDROP;
    if FShowWorkAreas then Styles := Styles or LVS_EX_MULTIWORKAREAS;
    if htHandPoint in FHotTrackStyles then
      Styles := Styles or LVS_EX_ONECLICKACTIVATE
    else if FHotTrackStyles * [htUnderlineHot, htUnderlineCold] <> [] then
      Styles := Styles or LVS_EX_TWOCLICKACTIVATE;
    if htUnderlineHot in FHotTrackStyles then
      Styles := Styles or LVS_EX_UNDERLINEHOT;
    if htUnderlineCold in FHotTrackStyles then
      Styles := Styles or LVS_EX_UNDERLINECOLD;
    ListView_SetExtendedListViewStyle(Handle, Styles);
    if TempImages <> nil then
      StateImages := TempImages;
  end;
end;

procedure TMyCustomListView.ResortItems(AProc: TLVCompare);
begin
  Items.BeginUpdate;
  try
    CustomSort(AProc, 0);
  finally
    Items.EndUpdate;
  end;
end;

procedure TMyCustomListView.RestoreChecks;
var
  i: Integer;
  Value: Byte;
begin
  if not OwnerData then
    for i := 0 to Items.Count - 1 do
    begin
      if FCheckStream <> nil then
      begin
        FCheckStream.ReadBuffer(Value, SizeOf(Value));
        Items[i].Checked := Boolean(Value);
      end
      else
        Items[i].Checked := Items[i].FChecked;
    end;
  FCheckStream.Free;
  FCheckStream := nil;
end;

procedure TMyCustomListView.SaveChecks;
var
  i: Integer;
  Value: Boolean;
begin
 if OwnerData then Exit;
 if FCheckStream = nil then
    FCheckStream := TMemoryStream.Create
  else
    FCheckStream.Size := 0;
  for i := 0 to Items.Count - 1 do
  begin
    Value := Items[i].Checked;
    FCheckStream.WriteBuffer(Value, SizeOf(Value));
  end;
  FCheckStream.Position := 0;
end;

procedure TMyCustomListView.SetCheckboxes(Value: Boolean);
var
  I: Integer;
begin
  if FCheckboxes <> Value then
  begin
    if Value then
      SetStateImages(nil); //Cant have state images and checkboxes = true

    FCheckboxes := Value;
    ResetExStyles;
    if FCheckboxes then
      RestoreChecks
    else
      if not OwnerData then
        for I := 0 to Items.Count - 1 do
          Items[I].FChecked := (ListView_GetCheckState(Handle, Items[I].Index) <> 0)
  end;
end;

procedure TMyCustomListView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetHotTrackStyles(Value: TListHotTrackStyles);
begin
  if FHotTrackStyles <> Value then
  begin
    FHotTrackStyles := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetOwnerData(Value: Boolean);
begin
  if FOwnerData <> Value then
  begin
    Items.Clear;
    FOwnerData := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetFlatScrollBars(Value: Boolean);
begin
  if FFlatScrollBars <> Value then
  begin
    FFlatScrollBars := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetFullDrag(Value: Boolean);
begin
  if FFullDrag <> Value then
  begin
    FFullDrag := Value;
    ResetExStyles;
  end;
end;

procedure TMyCustomListView.SetBorderStyle(Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetColumnClick(Value: Boolean);
begin
  if ColumnClick <> Value then
  begin
    FColumnClick := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetItemIndex(const Value: Integer);
begin
  if Value < 0 then
  begin
    if Selected <> nil then
      Selected.Selected := False
  end
  else
  begin
    Items[Value].Selected := True;
    ItemFocused := Items[Value];
  end;
end;

procedure TMyCustomListView.SetMultiSelect(Value: Boolean);
begin
  if Value <> MultiSelect then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetColumnHeaders(Value: Boolean);
begin
  if Value <> ShowColumnHeaders then
  begin
    FShowColumnHeaders := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetTextColor(Value: TColor);
begin
  ListView_SetTextColor(Handle, ColorToRGB(Font.Color));
end;

procedure TMyCustomListView.SetTextBkColor(Value: TColor);
begin
  ListView_SetTextBkColor(Handle, ColorToRGB(Color));
  ListView_SetBkColor(Handle, ColorToRGB(Color));
end;

procedure TMyCustomListView.SetAllocBy(Value: Integer);
begin
  if AllocBy <> Value then
  begin
    FAllocBy := Value;
    if HandleAllocated then ListView_SetItemCount(Handle, Value);
  end;
end;

procedure TMyCustomListView.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetTextBkColor(Color);
end;

procedure TMyCustomListView.CMCtl3DChanged(var Message: TMessage);
begin
  if FBorderStyle = bsSingle then RecreateWnd;
  inherited;
end;

procedure TMyCustomListView.WMNotify(var Message: TWMNotify);
var
  Col: TMyListColumn;
  P: TPoint;
  hChildWnd: HWND;
  hdhti: THDHitTestInfo;
{$IFDEF CLR}
  WndClass: StringBuilder;
  HDItem: THDItem;
  Hdr: TNMHdr;
  Notify: THDNotify;
  LMessage: TWMNotifyLV;
{$ELSE}
  WndClass: string;
{$ENDIF}
begin
  inherited;
{$IFDEF CLR}
  Hdr := Message.NMHdr;
  with Hdr do
{$ELSE}
  with Message.NMHdr^ do
{$ENDIF}
  begin
    if ValidHeaderHandle and (hWndFrom = FHeaderHandle) then
      case code of
        HDN_ENDTRACKA, HDN_ENDTRACKW:
          begin
{$IFDEF CLR}
            LMessage := TWMNotifyLV.Create(Message.OriginalMessage);
            Notify := LMessage.HDNotify;
            HDItem := THDItem(Marshal.PtrToStructure(Notify.PItem, TypeOf(THDItem)));
            with HDItem, Notify do
            begin
{$ELSE}
            with PHDNotify(Pointer(Message.NMHdr))^, PItem^ do
{$ENDIF}
              if (Mask and HDI_WIDTH) <> 0 then
                begin
                  Col := GetColumnFromTag(Item);
                  if Col.MinWidth >= cxy then
                    cxy := Col.MinWidth
                  else
                    if (Col.MaxWidth > 0) and (Col.MaxWidth <= cxy) then
                      cxy := Col.MaxWidth;
                  Col.Width := cxy;
                end;
{$IFDEF CLR}
              Marshal.StructureToPtr(TObject(HDItem), PItem, False);
            end;
            LMessage.HDNotify := Notify;
            Message := TWMNotify.Create(LMessage.OriginalMessage);
{$ENDIF}
          end;
        HDN_ENDDRAG:
          FUpdatingColumnOrder := True;
        HDN_DIVIDERDBLCLICKA, HDN_DIVIDERDBLCLICKW:
{$IFDEF CLR}
          with Hdr, TWMNotifyLV.Create(Message.OriginalMessage).HDNotify do
{$ELSE}
          with PHDNotify(Pointer(Message.NMHdr))^ do
{$ENDIF}
          begin
            Col := GetColumnFromTag(Item);
            Col.Width := ListView_GetColumnWidth(Handle, Item);
            if IsCustomDrawn(dtControl, cdPrePaint) then
              Invalidate;
          end;
        NM_RCLICK:
          begin
            P := SmallPoint(GetMessagePos);
            hChildWnd := ChildWindowFromPoint(Handle, ScreenToClient(P));
            if (hChildWnd <> 0) and (hChildWnd <> Handle) then
            begin
{$IFDEF CLR}
              WndClass := StringBuilder.Create(80);
              WndClass.Length := GetClassName(hChildWnd, WndClass, WndClass.Capacity);
              if WndClass.ToString = 'SysHeader32' then
{$ELSE}
              SetLength(WndClass, 80);
              SetLength(WndClass, GetClassName(hChildWnd, PChar(WndClass), Length(WndClass)));
              if WndClass = 'SysHeader32' then
{$ENDIF}
              begin
                hdhti.Point := ScreenToClient(P);
                hdhti.Point.X := hdhti.Point.X + GetScrollPos(Handle, SB_HORZ);
                if SendStructMessage(hChildWnd, HDM_HITTEST, 1, hdhti) >= 0 then
                  ColRightClick(GetColumnFromTag(hdhti.Item), hdhti.Point);
              end;
            end;
          end;
      end;
  end;
end;

function TMyCustomListView.ColumnsShowing: Boolean;
begin
  Result := (ViewStyle = vsReport);
end;

function TMyCustomListView.ValidHeaderHandle: Boolean;
begin
  Result := FHeaderHandle <> 0;
end;

procedure TMyCustomListView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    SetTextColor(Font.Color);
    if ValidHeaderHandle then
      InvalidateRect(FHeaderHandle, nil, True);
  end;
end;

procedure TMyCustomListView.SetHideSelection(Value: Boolean);
begin
  if Value <> HideSelection then
  begin
    FHideSelection := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    FReadOnly := Value;
    RecreateWnd;
  end;
end;

procedure TMyCustomListView.SeTMyIconOptions(Value: TMyIconOptions);
begin
  with FIconOptions do
  begin
    Arrangement := Value.Arrangement;
    AutoArrange := Value.AutoArrange;
    WrapText := Value.WrapText;
  end;
end;

procedure TMyCustomListView.SetViewStyle(Value: TViewStyle);
const
  ViewStyles: array[TViewStyle] of Integer = (LVS_ICON, LVS_SMALLICON,
    LVS_LIST, LVS_REPORT);
var
  Style: Longint;
begin
  if Value <> FViewStyle then
  begin
    FViewStyle := Value;
    if HandleAllocated then
    begin
      Style := GetWindowLong(Handle, GWL_STYLE);
      Style := Style and (not LVS_TYPEMASK);
      Style := Style or Integer(ViewStyles[FViewStyle]);
      SetWindowLong(Handle, GWL_STYLE, Style);
      UpdateColumns;
      case ViewStyle of
        vsIcon,
        vsSmallIcon:
          if IconOptions.Arrangement = iaTop then
            Arrange(arAlignTop) else
            Arrange(arAlignLeft);
      end;
      if not (csLoading in ComponentState) then
        RecreateWnd;
    end;
  end;
end;

procedure TMyCustomListView.WMParentNotify(var Message: TWMParentNotify);
begin
  with Message do
    if (Event = WM_CREATE) and (FHeaderHandle = 0) then
    begin
      FHeaderHandle := ChildWnd;
      FDefHeaderProc := TWindowProcPtr(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
{$IFDEF CLR}
      SetWindowLong(FHeaderHandle, GWL_WNDPROC, FLVInstances.FHeaderInstance);
{$ELSE}
      SetWindowLong(FHeaderHandle, GWL_WNDPROC, Winapi.Windows.LPARAM(FHeaderInstance));
{$ENDIF}
      UpdateHeaderState;
    end;
  inherited;
end;

function TMyCustomListView.GetItemIndex(Value: TMyListItem): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do if Items[I] = Value then Break;
  if I < Items.Count then Result := I;
end;

function TMyCustomListView.GetItemIndex: Integer;
begin
  Result := -1;
  if Selected <> nil then
    Result := Selected.Index;
end;

function TMyCustomListView.OwnerDataFetch(Item: TMyListItem; Request: TItemRequest): Boolean;
begin
  if Assigned(FOnData) then
  begin
    FOnData(Self, Item);
    Result := True;
  end
  else Result := False;
end;

function TMyCustomListView.OwnerDataFind(Find: TItemFind; const FindString: string;
  const FindPosition: TPoint; FindData: TCustomData; StartIndex: Integer;
  Direction: TSearchDirection; Wrap: Boolean): Integer;
begin
  Result := -1;
  if Assigned(FOnDataFind) then
    FOnDataFind(Self, Find, FindString, FindPosition, FindData, StartIndex,
      Direction, Wrap, Result);
end;

function TMyCustomListView.OwnerDataHint(StartIndex, EndIndex: Integer): Boolean;
begin
  if Assigned(FOnDataHint) then
  begin
    FOnDataHint(Self, StartIndex, EndIndex);
    Result := True;
  end
  else Result := False;
end;

function TMyCustomListView.OwnerDataStateChange(StartIndex, EndIndex: Integer;
  OldState, NewState: TItemStates): Boolean;
begin
  if Assigned(FOnDataStateChange) then
  begin
    FOnDataStateChange(Self, StartIndex, EndIndex, OldState, NewState);
    Result := True;
  end
  else Result := False;
end;

function TMyCustomListView.CreateListItem: TMyListItem;
var
  LClass: TMyListItemClass;
begin
  LClass := TMyListItem;
  if Assigned(FOnCreateItemClass) then
    FOnCreateItemClass(Self, LClass);
  Result := LClass.Create(Items);
end;

function TMyCustomListView.CreateListItems: TMyListItems;
begin
  Result := TMyListItems.Create(Self);
end;

function TMyCustomListView.GetItem(Value: TLVItem): TMyListItem;
var
  S: string;
  Request: TItemRequest;

  function ConvertMask(Mask: Longint): TItemRequest;
  begin
    Result := [];
    if Mask and LVIF_TEXT <> 0 then
      Include(Result, irText);
    if Mask and LVIF_IMAGE <> 0 then
      Include(Result, irImage);
    if Mask and LVIF_PARAM <> 0 then
      Include(Result, irParam);
    if Mask and LVIF_STATE <> 0 then
      Include(Result, irState);
    if Mask and LVIF_INDENT <> 0 then
      Include(Result, irIndent);
  end;

begin
  with Value do
    if (mask and LVIF_PARAM) <> 0 then
{$IFDEF CLR}
      Result := TMyListItem(FListItems.FItemHashTable.Item[TObject(Integer(lParam))])
{$ELSE}
      Result := TMyListItem(lParam)
{$ENDIF}
    else
    begin
      if OwnerData then
      begin
        if iItem < 0 then
          Result := nil
        else
          if iSubItem = 0 then
          begin
            Request := ConvertMask(mask);
            FTempItem.FIndex := iItem;
{$IFDEF CLR}
            if lParam <> 0 then
              FTempItem.FData := TMyListItem(FListItems.FItemHashTable.Item[TObject(Integer(lParam))])
            else
              FTempItem.FData := nil;
            FTempItem.FSubItems.Clear;
            if (irText in Request) and (pszText <> nil) then
              S := Marshal.PtrToStringAuto(pszText)
{$ELSE}
            FTempItem.FData := Pointer(lParam);
            FTempItem.FSubItems.Clear;
            if (irText in Request) and (pszText <> nil) then
              S := StrPas(pszText)
{$ENDIF}
            else
              S := '';
            FTempItem.FCaption := S;
            if irImage in Request then
              FTempItem.FImageIndex := iImage;
            if irIndent in Request then
              FTempItem.FIndent := iIndent;
            OwnerDataFetch(FTempItem, Request);
            Result := FTempItem;
          end
          else
            Result := FTempItem;
      end
      else
        Result := Items[IItem];
    end;
end;

function TMyCustomListView.GetSelCount: Integer;
begin
  Result := ListView_GetSelectedCount(Handle);
end;

procedure TMyCustomListView.CNNotify(var Message: TWMNotifyLV);
var
  Item: TMyListItem;
  I: Integer;
  R: TRect;
  DefaultDraw: Boolean;
  ItemFind: TItemFind;
  FindString: string;
  FindPos: TPoint;
  FindData: TCustomData;
  LSubItem: Integer;
  SearchDir: TSearchDirection;
  TmpItem: TLVItem;
  SubItem: Boolean;
  SubItemImage: Integer;
  LogFont: TLogFont;
{$IFDEF CLR}
  Notify: THDNotify;
  HDItem: THDItem;
  LVCustomDraw: TNMLVCustomDraw;
  DispInfo: TMyLVDispInfo;
  Buffer: TBytes;
{$ELSE}
  LVCustomDraw: PNMLVCustomDraw;
  DispInfo: PLVDispInfo;
{$ENDIF}

  function ConvertFlags(Flags: Integer): TItemFind;
  begin
    if Flags and LVFI_PARAM <> 0 then
      Result := ifData
    else if Flags and LVFI_PARTIAL <> 0 then
      Result := ifPartialString
    else if Flags and LVFI_STRING <> 0 then
      Result := ifExactString
    else if Flags and LVFI_NEARESTXY <> 0 then
      Result := ifNearest
    else
      Result := ifData; // Fall-back value
  end;

  function ConvertStates(State: Integer): TItemStates;
  begin
    Result := [];
    if State and LVIS_ACTIVATING <> 0 then
      Include(Result, isActivating);
    if State and LVIS_CUT <> 0 then
      Include(Result, isCut);
    if State and LVIS_DROPHILITED <> 0 then
      Include(Result, isDropHilited);
    if State and LVIS_FOCUSED <> 0 then
      Include(Result, isFocused);
    if State and LVIS_SELECTED <> 0 then
      Include(Result, isSelected);
  end;

begin
  with Message do
    case NMHdr.code of
      HDN_TRACKA, HDN_TRACKW:
        begin
{$IFDEF CLR}
          Notify := Message.HDNotify;
          HDItem := THDItem(Marshal.PtrToStructure(Notify.PItem, TypeOf(THDItem)));
          with Notify, HDItem do
{$ELSE}
          with PHDNotify(Pointer(Message.NMHdr))^, PItem^ do
{$ENDIF}
            if ((Mask and HDI_WIDTH) <> 0) then
            begin
              if Column[Item].MinWidth >= cxy then
                Column[Item].Width := Column[Item].MinWidth
              else
                if Column[Item].MaxWidth <= cxy then
                  Column[Item].Width := Column[Item].MaxWidth;
            end;
          end;
      NM_CUSTOMDRAW:
        if Assigned(FCanvas) then
          with NMCustomDraw{$IFNDEF CLR}^{$ENDIF} do
          try
            FCanvas.Lock;
            Result := CDRF_DODEFAULT;

            if (dwDrawStage and CDDS_ITEM) = 0 then
            begin
              R := ClientRect;
              case dwDrawStage of
                CDDS_PREPAINT:
                begin
                  if IsCustomDrawn(dtControl, cdPrePaint) then
                  begin
                    try
                      FCanvas.Handle := hdc;
                      FCanvas.Font := Font;
                      FCanvas.Brush := Brush;
                      DefaultDraw := CustomDraw(R, cdPrePaint);
                    finally
                      FCanvas.Handle := 0;
                    end;
                    if not DefaultDraw then
                    begin
                      Result := CDRF_SKIPDEFAULT;
                      Exit;
                    end;
                  end;
                  if IsCustomDrawn(dtItem, cdPrePaint) or IsCustomDrawn(dtItem, cdPreErase) then
                    Result := CDRF_NOTIFYITEMDRAW;
                  if IsCustomDrawn(dtItem, cdPostPaint) then
                    Result := Result or CDRF_NOTIFYPOSTPAINT;
                  if IsCustomDrawn(dtItem, cdPostErase) then
                    Result := Result or CDRF_NOTIFYPOSTERASE;
                  if IsCustomDrawn(dtSubItem, cdPrePaint) or IsCustomDrawn(dtSubItem, cdPostPaint) then
                    Result := Result or CDRF_NOTIFYSUBITEMDRAW;
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
            end else
            begin
              LVCustomDraw := NMLVCustomDraw;
              SubItem := dwDrawStage and CDDS_SUBITEM <> 0;
              { Don't call CustomDrawSubItem for the 0th subitem since
                CustomDrawItem draws that item. }
              if SubItem and (LVCustomDraw.iSubItem = 0) then Exit;
              LSubItem := LVCustomDraw.iSubItem;
{$IFNDEF CLR}
              FillChar(TmpItem, SizeOf(TmpItem), 0);
{$ENDIF}
              TmpItem.iItem := dwItemSpec;

              if (dwDrawStage and CDDS_ITEMPREPAINT) = CDDS_ITEMPREPAINT then
              begin
                try
                  FCanvas.Handle := hdc;
                  FCanvas.Font := Font;
                  FCanvas.Brush := Brush;
                  FCanvas.Font.OnChange := CanvasChanged;
                  FCanvas.Brush.OnChange := CanvasChanged;
                  FCanvasChanged := False;
                  if SubItem then
                    DefaultDraw := CustomDrawSubItem(GetItem(TmpItem),
                      LSubItem, TCustomDrawState(Word(uItemState)), cdPrePaint)
                  else
                    DefaultDraw := CustomDrawItem(GetItem(TmpItem),
                      TCustomDrawState(Word(uItemState)), cdPrePaint);
                  if not DefaultDraw then
                  begin
                    Result := Result or CDRF_SKIPDEFAULT;
                    Exit;
                  end
                  else if FCanvasChanged then
                  begin
                    FCanvasChanged := False;
                    FCanvas.Font.OnChange := nil;
                    FCanvas.Brush.OnChange := nil;
                    with LVCustomDraw{$IFNDEF CLR}^{$ENDIF} do
                    begin
                      clrText := ColorToRGB(FCanvas.Font.Color);
                      clrTextBk := ColorToRGB(FCanvas.Brush.Color);
{$IFDEF CLR}
                      if GetObject(FCanvas.Font.Handle, Marshal.SizeOf(LogFont), LogFont) <> 0 then
{$ELSE}
                      if GetObject(FCanvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
{$ENDIF}
                      begin
                        FCanvas.Handle := 0;  // disconnect from hdc
                        // release the font if it's been changed when painting
                        // the listview item, or a previous subitem
                        if SubItem and (FOurFont <> 0) and (FStockFont <> 0) then
                        begin
                          SelectObject(hdc, FStockFont);
                          DeleteObject(FOurFont);
                          FOurFont := 0;
                          FStockFont := 0;
                        end;
                        // don't delete the stock font
                        FOurFont := CreateFontIndirect(LogFont);
                        FStockFont := SelectObject(hdc, FOurFont);
                        Result := Result or CDRF_NEWFONT;
                      end;
                    end;
{$IFDEF CLR}
                    NMLVCustomDraw := LVCustomDraw;
{$ENDIF}
                  end;
                finally
                  FCanvas.Handle := 0;
                end;
                if not SubItem then
                begin
                  if IsCustomDrawn(dtSubItem, cdPrePaint) then
                    Result := Result or CDRF_NOTIFYSUBITEMDRAW;
                  if IsCustomDrawn(dtItem, cdPostPaint) then
                    Result := Result or CDRF_NOTIFYPOSTPAINT;
                  if IsCustomDrawn(dtItem, cdPostErase) then
                    Result := Result or CDRF_NOTIFYPOSTERASE;
                end else
                begin
                  if IsCustomDrawn(dtSubItem, cdPostPaint) then
                    Result := Result or CDRF_NOTIFYPOSTPAINT;
                  if IsCustomDrawn(dtSubItem, cdPostErase) then
                    Result := Result or CDRF_NOTIFYPOSTERASE;
                end;
              end
              else if (dwDrawStage and CDDS_ITEMPOSTPAINT) = CDDS_ITEMPOSTPAINT then
              begin
                try
                  FCanvas.Handle := hdc;
                  FCanvas.Font := Font;
                  FCanvas.Brush := Brush;
                  if SubItem then
                  CustomDrawSubItem(GetItem(TmpItem),
                    LSubItem, TCustomDrawState(Word(uItemState)), cdPostPaint)
                else
                  CustomDrawItem(GetItem(TmpItem),
                    TCustomDrawState(Word(uItemState)), cdPostPaint);
                finally
                  FCanvas.Handle := 0;
                end;

                //release the font we may have loaned during item drawing.
                if (FOurFont <> 0) and (FStockFont <> 0) then
                begin
                  SelectObject(hdc, FStockFont);
                  DeleteObject(FOurFont);
                  FOurFont := 0;
                  FStockFont := 0;
                end;
              end
              else if (dwDrawStage and CDDS_ITEMPREERASE) = CDDS_ITEMPREERASE then
              begin
                if SubItem then
                  CustomDrawSubItem(GetItem(TmpItem),
                    LSubItem, TCustomDrawState(Word(uItemState)), cdPreErase)
                else
                  CustomDrawItem(GetItem(TmpItem),
                    TCustomDrawState(Word(uItemState)), cdPreErase);
              end
              else if (dwDrawStage and CDDS_ITEMPOSTERASE) = CDDS_ITEMPOSTERASE then
              begin
                if SubItem then
                  CustomDrawSubItem(GetItem(TmpItem),
                    LSubItem, TCustomDrawState(Word(uItemState)), cdPostErase)
                else
                  CustomDrawItem(GetItem(TmpItem),
                    TCustomDrawState(Word(uItemState)), cdPostErase);
              end;
            end;
          finally
            FCanvas.Unlock;
          end;
      LVN_BEGINDRAG:
        FDragIndex := NMListView.iItem;
      LVN_DELETEITEM:
        begin
{$IFDEF CLR}
          Delete(TMyListItem(FListItems.FItemHashTable.Item[TObject(Integer(NMListView.lParam))]));
{$ELSE}
          Delete(TMyListItem(PNMListView(NMHdr)^.lParam));
{$ENDIF}
          Result := 1;
        end;
      LVN_DELETEALLITEMS:
        begin
          FDeletingAllItems := True;
          try
            for I := Items.Count - 1 downto 0 do
              Delete(Items[I]);
          finally
            FDeletingAllItems := False;
          end;
          Result := 1;
        end;
      LVN_GETDISPINFOA, LVN_GETDISPINFOW:
        begin
          DispInfo := LVDispInfo;
          Item := GetItem(DispInfo.item);
          with DispInfo.item do
          begin
            if (mask and LVIF_TEXT) <> 0 then
              if iSubItem = 0 then
              begin
{$IFDEF CLR}
                Buffer := PlatformBytesOf(Copy(Item.Caption, 1, cchTextMax - 1) + #0);
                Marshal.Copy(Buffer, 0, pszText, Length(Buffer));
{$ELSE}
                StrPLCopy(pszText, Item.Caption, cchTextMax - 1);
{$ENDIF}
              end
              else
                with Item.SubItems do
                  if iSubItem <= Count then
{$IFDEF CLR}
                  begin
                    Buffer := PlatformBytesOf(Copy(Strings[iSubItem - 1], 1, cchTextMax - 1) + #0);
                    Marshal.Copy(Buffer, 0, pszText, Length(Buffer));
                  end
                  else
                    Marshal.WriteInt16(pszText, 0);
{$ELSE}
                    StrPLCopy(pszText, Strings[iSubItem - 1], cchTextMax - 1)
                  else pszText[0] := #0;
{$ENDIF}
            if (mask and LVIF_IMAGE) <> 0 then
            begin
              if iSubItem = 0 then
              begin
                GetImageIndex(Item);
                iImage := Item.ImageIndex;
                if Assigned(FStateImages) then
                begin
                  state := IndexToStateImageMask(Item.StateIndex + 1);
                  stateMask := $F000;
                  mask := mask or LVIF_STATE;
                end;
              end
              else
                if (iSubItem-1 >= 0) and (iSubItem-1 < Item.FSubItems.Count) then
                begin
                  SubItemImage := Item.SubItemImages[iSubItem-1];
                  GetSubItemImage(Item, iSubItem-1, SubItemImage);
                  iImage := SubItemImage;
                end;
{$IFDEF CLR}
              LVDispInfo := DispInfo;
{$ENDIF}
            end;
            if (mask and LVIF_INDENT) <> 0 then
            begin
              iIndent := Item.Indent;
{$IFDEF CLR}
              LVDispInfo := DispInfo;
{$ENDIF}
            end;
          end;
        end;

      LVN_ODCACHEHINT:
        with NMLVCacheHint{$IFNDEF CLR}^{$ENDIF} do
          OwnerDataHint(iFrom, iTo);
      LVN_ODFINDITEMA, LVN_ODFINDITEMW:
        with NMLVFindItem{$IFNDEF CLR}^{$ENDIF} do
        begin
          ItemFind := ConvertFlags(lvfi.flags);
          FindData := nil;
          FindString := '';
          FindPos := Point(0,0);
          SearchDir := sdAll;
          case ItemFind of
            ifData: FindData := TCustomData(lvfi.lParam);
            ifPartialString, ifExactString:
              if lvfi.psz <> nil then
{$IFDEF CLR}
                FindString := Marshal.PtrToStringAuto(lvfi.psz) else
{$ELSE}
                FindString := StrPas(lvfi.psz) else
{$ENDIF}
                FindString := '';
            ifNearest:
              begin
                FindPos := lvfi.pt;
                case lvfi.vkDirection of
                  VK_LEFT: SearchDir := sdLeft;
                  VK_UP: SearchDir := sdAbove;
                  VK_RIGHT: SearchDir := sdRight;
                  VK_DOWN: SearchDir := sdBelow;
                end;
              end;
          end;
          Result := OwnerDataFind(ConvertFlags(lvfi.flags), FindString, FindPos,
            FindData, iStart, SearchDir, lvfi.flags and LVFI_WRAP <> 0);
        end;
      LVN_ODSTATECHANGED:
        with NMLVODStateChange{$IFNDEF CLR}^{$ENDIF} do
          OwnerDataStateChange(iFrom, iTo, ConvertStates(uNewState),
            ConvertStates(uOldState));

      LVN_BEGINLABELEDITA, LVN_BEGINLABELEDITW:
        begin
          Item := GetItem(LVDispInfo.item);
          if not CanEdit(Item) then Result := 1;
          if Result = 0 then
          begin
            FEditHandle := ListView_GetEditControl(Handle);
            FDefEditProc := TWindowProcPtr(GetWindowLong(FEditHandle, GWL_WNDPROC));
{$IFDEF CLR}
            SetWindowLong(FEditHandle, GWL_WNDPROC, FLVInstances.FEditInstance);
{$ELSE}
            SetWindowLong(FEditHandle, GWL_WNDPROC, Winapi.Windows.LPARAM(FEditInstance));
{$ENDIF}
          end;
        end;
      LVN_ENDLABELEDITA, LVN_ENDLABELEDITW:
        with LVDispInfo{$IFNDEF CLR}^{$ENDIF} do
          if (item.pszText <> nil) and (item.IItem <> -1) then
            Edit(item);
      LVN_COLUMNCLICK:
        ColClick(GetColumnFromTag(NMListView.iSubItem));
      LVN_INSERTITEM:
        InsertItem(Items[NMListView.iItem]);
      LVN_ITEMCHANGING:
        with NMListView{$IFNDEF CLR}^{$ENDIF} do
          if not CanChange(Items[iItem], uChanged) then Result := 1;
      LVN_ITEMCHANGED:
        with NMListView{$IFNDEF CLR}^{$ENDIF} do
        begin
          if Reading then
            Exit;
          Item := Items[iItem];
          Change(Item, uChanged);
          if Assigned(FOnSelectItem) and (uChanged = LVIF_STATE) then
          begin
            if (uOldState and LVIS_SELECTED <> 0) and
              (uNewState and LVIS_SELECTED = 0) then
              FOnSelectItem(Self, Item, False)
            else if (uOldState and LVIS_SELECTED = 0) and
              (uNewState and LVIS_SELECTED <> 0) then
              FOnSelectItem(Self, Item, True);
          end;
          if (Action <> nil) and not (csDesigning in ComponentState) and
             (ActionLink <> nil) then
            ActionLink.Execute(Self);
          if Assigned(FOnItemChecked) and (uChanged = LVIF_STATE) and
             (((uOldState and LVIS_STATEIMAGEMASK) shr 12) <>
             ((uNewState and LVIS_STATEIMAGEMASK) shr 12)) then
          begin
            FOnItemChecked(Self, Item);
          end;
        end;
      LVN_GETINFOTIPA, LVN_GETINFOTIPW:
        if Assigned(FOnInfoTip) then
          Application.ActivateHint(Mouse.CursorPos);
      NM_CLICK:
        FClicked := True;
      NM_RCLICK:
        FRClicked := True;
    end;
end;

{$IFDEF CLR}
type
  TMyLVCompareWrapper = record
    SortProc: TMyLVCompareProc;
    Data: TTag;
  end;
{$ENDIF}

{$IFDEF CLR}
function TMyCustomListView.CustomListViewSort(AnItem1, AnItem2: Longint;
  lParam: Integer): Integer;
var
  Item1, Item2: TMyListItem;
  CompareWrapper: TMyLVCompareWrapper;
begin
  if AnItem1 <> 0 then
    Item1 := TMyListItem(FListItems.FItemHashTable.Item[TObject(Integer(AnItem1))])
  else
    Item1 := nil;
  if AnItem2 <> 0 then
    Item2 := TMyListItem(FListItems.FItemHashTable.Item[TObject(Integer(AnItem2))])
  else
    Item2 := nil;

  // Extract SortProc, Data from wrapper structure and call SortProc
  CompareWrapper := TLVCompareWrapper(GCHandle(IntPtr(lParam)).Target);
  Result := CompareWrapper.SortProc(Item1, Item2, CompareWrapper.Data);
end;
{$ENDIF}

{$IFDEF CLR}
function TMyCustomListView.DefaultListViewSort(Item1, Item2: TMyListItem;
  lParam: TTag): Integer;
begin
  if Assigned(Item1) then
    with Item1 do
      if Assigned(OnCompare) then
        OnCompare(ListView, Item1, Item2, lParam, Result)
      else if Assigned(Item2) then
        Result := CompareText(Item1.Caption, Item2.Caption)
      else
        Result := 1
  else
    Result := -1;
end;
{$ENDIF}

procedure TMyCustomListView.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  if sfWidth in ScalingFlags then
    for I := 0 to Columns.Count-1 do
      if (Columns[I].Width <> LVSCW_AUTOSIZE) and
        (Columns[I].Width <> LVSCW_AUTOSIZE_USEHEADER) then
        Columns[I].Width := MulDiv(Columns[I].Width, M, D);
  inherited ChangeScale(M,D);
end;

procedure TMyCustomListView.ColClick(Column: TMyListColumn);
  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  end;
begin
  if (ssCtrl in GetShiftState) then
    Column.SortOrder := soNone
  else
    case Column.SortOrder of
      soNone, soDESC:
        Column.SortOrder := soASC;
      soASC:
        Column.SortOrder := soDESC;
    end;
  if Assigned(FOnColumnClick) then FOnColumnClick(Self, Column);
end;

procedure TMyCustomListView.ColRightClick(Column: TMyListColumn; Point: TPoint);
begin
  if Assigned(FOnColumnRightClick) then FOnColumnRightClick(Self, Column, Point);
end;

procedure TMyCustomListView.InsertItem(Item: TMyListItem);
begin
  if not Reading and Assigned(FOnInsert) then
    FOnInsert(Self, Item);
end;

procedure TMyCustomListView.AddItem(Item: string; AObject: TObject);
begin
  with Items.Add do
  begin
    Caption := Item;
    Data := AObject;
  end;
end;

function TMyCustomListView.CanChange(Item: TMyListItem; Change: Integer): Boolean;
var
  ItemChange: TItemChange;
begin
  Result := True;
  case Change of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if not Reading and Assigned(FOnChanging) then
    FOnChanging(Self, Item, ItemChange, Result);
end;

procedure TMyCustomListView.Change(Item: TMyListItem; Change: Integer);
var
  ItemChange: TItemChange;
begin
  case Change of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if not Reading and Assigned(FOnChange) then
    FOnChange(Self, Item, ItemChange);
end;

procedure TMyCustomListView.Delete(Item: TMyListItem);
begin
  if (Item <> nil) and not Item.FProcessedDeleting then
  begin
    if not (csRecreating in ControlState) and Assigned(FOnDeletion) then
      FOnDeletion(Self, Item);
    Item.FProcessedDeleting := True;
    Item.Delete;
  end;
end;

function TMyCustomListView.CanEdit(Item: TMyListItem): Boolean;
begin
  Result := True;
  if Assigned(FOnEditing) then FOnEditing(Self, Item, Result);
end;

procedure TMyCustomListView.Edit(const Item: TLVItem);
var
  S: string;
  EditItem: TMyListItem;
begin
  with Item do
  begin
{$IFDEF CLR}
    S := Marshal.PtrToStringAuto(pszText);
{$ELSE}
    S := pszText;
{$ENDIF}
    EditItem := GetItem(Item);
    if Assigned(FOnEdited) then
      FOnEdited(Self, EditItem, S);
    if EditItem <> nil then
      EditItem.Caption := S;
  end;
end;

function TMyCustomListView.IsEditing: Boolean;
var
  ControlHand: HWnd;
begin
  ControlHand := ListView_GetEditControl(Handle);
  Result := (ControlHand <> 0) and IsWindowVisible(ControlHand);
end;

function TMyCustomListView.GetDragImages: TDragImageList;
begin
  if SelCount = 1 then
    Result := FDragImage else
    Result := nil;
end;

procedure TMyCustomListView.WndProc(var Message: TMessage);
{$IFDEF CLR}
var
  MouseMsg: TWMMouse;
{$ENDIF}
begin
  if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
    (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging and (DragMode = dmAutomatic) then
  begin
{$IFDEF CLR}
    MouseMsg := TWMMouse.Create(Message);
    if not IsControlMouseMsg(MouseMsg) then
{$ELSE}
    if not IsControlMouseMsg(TWMMouse(Message)) then
{$ENDIF}
    begin
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);
    end;
  end
  else
    if (Message.Msg = CM_STYLECHANGED) and not (csLoading in ComponentState) then
      RecreateWnd
    else if (Message.Msg = CM_SYSCOLORCHANGE) and not (csLoading in ComponentState) then
      RecreateWnd
    else if not (((Message.Msg = WM_PAINT) or (Message.Msg = WM_ERASEBKGND)) and
      Items.FNoRedraw) then
        inherited WndProc(Message);
end;

procedure TMyCustomListView.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiPan then
  begin
    Handled := True;
    if gfBegin in EventInfo.Flags then
      FPanPoint := EventInfo.Location
    else if not (gfEnd in EventInfo.Flags) then
    begin
      ListView_Scroll(Handle, -(EventInfo.Location.X - FPanPoint.X),
        -(EventInfo.Location.Y - FPanPoint.Y));
      FPanPoint := EventInfo.Location

    end;
  end;
end;

procedure TMyCustomListView.DoStartDrag(var DragObject: TDragObject);
var
  P, P1: TPoint;
  ImageHandle: HImageList;
  DragItem: TMyListItem;
begin
  inherited DoStartDrag(DragObject);
  FLastDropTarget := nil;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if FDragIndex <> -1 then
    DragItem := Items[FDragIndex]
    else DragItem := nil;
  FDragIndex := -1;
  if DragItem = nil then
    with P do DragItem := GetItemAt(X, Y);
  if DragItem <> nil then
  begin
    ImageHandle := ListView_CreateDragImage(Handle, DragItem.Index, P1);
    if ImageHandle <> 0 then
      with FDragImage do
      begin
        Handle := ImageHandle;
        with P, DragItem.DisplayRect(drBounds) do
          SetDragImage(0, X - Left , Y - Top);
      end;
  end;
end;

procedure TMyCustomListView.DoEndDrag(Target: TObject; X, Y: Integer);

begin
  inherited DoEndDrag(Target, X, Y);
  FDragImage.EndDrag;
  FLastDropTarget := nil;
end;

procedure TMyCustomListView.CMDrag(var Message: TCMDrag);
var
  I: Integer;
  Item: TMyListItem;
begin
  inherited;
  with Message, DragRec{$IFNDEF CLR}^{$ENDIF} do
    case DragMessage of
      dmDragMove: with ScreenToClient(Pos) do DoDragOver(Source, X, Y, Message.Result <> 0);
      dmDragLeave:
        begin
          TDragObject(Source).HideDragImage;
          FLastDropTarget := DropTarget;
          DropTarget := nil;
          Update;
          TDragObject(Source).ShowDragImage;
        end;
      dmDragDrop:
        begin
          FLastDropTarget := nil;
          { ListView_GetNextItem always returns nil for OwnerData = True and
            LVNI_ALL and LVNI_DROPHIGHLITED, so it is necessary to find the
            DropTarget and reset it by iterating through all items, starting
            with the first one that's visible }
          if OwnerData then
          begin
            if ViewStyle in [vsIcon, vsSmallIcon] then
              Item := GetNearestItem(Point(0, 0), sdAll)
            else
              Item := TopItem;
            if Item <> nil then
            for I := Item.Index to Items.Count - 1 do
              if Items[I].DropTarget then
              begin
                Items[I].DropTarget := False;
                Exit;
              end;
            end;
        end;
    end
end;

procedure TMyCustomListView.DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
var
  Item: TMyListItem;
  Target: TMyListItem;
begin
  Item := GetItemAt(X, Y);
  if Item <> nil then
  begin
    Target := DropTarget;
    if (Item <> Target) or (Item = FLastDropTarget) then
    begin
      FLastDropTarget := nil;
      TDragObject(Source).HideDragImage;
      Update;
      if Target <> nil then
        Target.DropTarget := False;
      Item.DropTarget := CanDrop;
      Update;
      TDragObject(Source).ShowDragImage;
    end;
  end;
end;

procedure TMyCustomListView.SetItems(Value: TMyListItems);
begin
  FListItems.Assign(Value);
end;

procedure TMyCustomListView.SetListColumns(Value: TMyListColumns);
begin
  FListColumns.Assign(Value);
end;

procedure TMyCustomListView.SetListGroups(Value: TMyListGroups);
begin
  FListGroups.Assign(Value);
end;

{$IFDEF CLR}
function TMyCustomListView.CustomSort(SortProc: TMyLVCompareProc; Data: TTag): Boolean;
var
  lParam: LongInt;
  CompareWrapper: TMyLVCompareWrapper;
begin
  Result := False;
  if HandleAllocated then
  begin
    if not Assigned(SortProc) then
      CompareWrapper.SortProc := DefaultListViewSort
    else
      CompareWrapper.SortProc := SortProc;
    CompareWrapper.Data := Data;

    lParam := THandle(IntPtr(GCHandle.Alloc(CompareWrapper)));
    try
      Result := ListView_SortItems(Handle, CustomListViewSort, lParam);
    finally
      GCHandle(IntPtr(lParam)).Free;
    end;
  end;
end;
{$ELSE}
function TMyCustomListView.CustomSort(SortProc: TLVCompare; lParam: LPARAM): Boolean;
begin
  Result := False;
  if HandleAllocated then
  begin
    if not Assigned(SortProc) then SortProc := @DefaultListViewSort;
    Result := ListView_SortItems(Handle, SortProc, lParam);
  end;
end;
{$ENDIF}

function TMyCustomListView.AlphaSort: Boolean;
begin
  if HandleAllocated then
{$IFDEF CLR}
    Result := CustomSort(nil, nil)
{$ELSE}
    Result := ListView_SortItems(Handle, @DefaultListViewSort, 0)
{$ENDIF}
  else
    Result := False;
end;

procedure TMyCustomListView.SetSortType(Value: TSortType);
begin
  if SortType <> Value then
  begin
    FSortType := Value;
    if ((SortType in [stData, stBoth]) and Assigned(OnCompare)) or
      (SortType in [stText, stBoth]) then
      AlphaSort;
  end;
end;

function TMyCustomListView.GetVisibleRowCount: Integer;
begin
  if ViewStyle in [vsReport, vsList] then
    Result := ListView_GetCountPerPage(Handle)
  else Result := 0;
end;

function TMyCustomListView.GetViewOrigin: TPoint;
begin
  ListView_GetOrigin(Handle, Result);
end;

function TMyCustomListView.GetTopItem: TMyListItem;
var
  Index: Integer;
begin
  Result := nil;
  if not (ViewStyle in [vsSmallIcon, vsIcon]) then
  begin
    Index := ListView_GetTopIndex(Handle);
    if Index <> -1 then Result := Items[Index];
  end;
end;

function TMyCustomListView.GetBoundingRect: TRect;
begin
  ListView_GetViewRect(Handle, Result);
end;

procedure TMyCustomListView.Scroll(DX, DY: Integer);
begin
  ListView_Scroll(Handle, DX, DY);
end;

procedure TMyCustomListView.SetGroupHeaderImages(Value: TCustomImageList);
begin
  if GroupHeaderImages <> Value then
  begin
    if GroupHeaderImages <> nil then
      GroupHeaderImages.UnRegisterChanges(FHeaderChangeLink);
    FGroupHeaderImages := Value;
    if GroupHeaderImages <> nil then
    begin
      GroupHeaderImages.RegisterChanges(FHeaderChangeLink);
      GroupHeaderImages.FreeNotification(Self);
      SetImageList(GroupHeaderImages.Handle, LVSIL_GROUPHEADER)
    end
    else SetImageList(0, LVSIL_GROUPHEADER);
    Invalidate;
  end;
end;

procedure TMyCustomListView.SetLargeImages(Value: TCustomImageList);
begin
  if LargeImages <> Value then
  begin
    if LargeImages <> nil then
      LargeImages.UnRegisterChanges(FLargeChangeLink);
    FLargeImages := Value;
    if LargeImages <> nil then
    begin
      LargeImages.RegisterChanges(FLargeChangeLink);
      LargeImages.FreeNotification(Self);
      SetImageList(LargeImages.Handle, LVSIL_NORMAL)
    end
    else SetImageList(0, LVSIL_NORMAL);
    Invalidate;
  end;
end;

procedure TMyCustomListView.SetSmallImages(Value: TCustomImageList);
begin
  if Value <> SmallImages then
  begin
    if SmallImages <> nil then
      SmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if SmallImages <> nil then
    begin
      SmallImages.RegisterChanges(FSmallChangeLink);
      SmallImages.FreeNotification(Self);
      SetImageList(SmallImages.Handle, LVSIL_SMALL)
    end
    else SetImageList(0, LVSIL_SMALL);
    Invalidate;
  end;
end;

procedure TMyCustomListView.SetStateImages(Value: TCustomImageList);
begin
  if StateImages <> Value then
  begin
    if StateImages <> nil then
      StateImages.UnRegisterChanges(FStateChangeLink);
    FStateImages := Value;
    if StateImages <> nil then
    begin
      SetCheckboxes(False);
      StateImages.RegisterChanges(FStateChangeLink);
      StateImages.FreeNotification(Self);
      SetImageList(StateImages.Handle, LVSIL_STATE);
    end
    else
    begin
      SetImageList(0, LVSIL_STATE);
      if CheckBoxes then
      begin
        CheckBoxes := False;
        CheckBoxes := True;
      end;
    end;
    Invalidate;
  end;
end;

function TMyCustomListView.GetColumnFromIndex(Index: Integer): TMyListColumn;
begin
  Result := FListColumns[Index];
end;

function TMyCustomListView.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean): TMyListItem;
const
  FullString: array[Boolean] of Integer = (0, LVFI_PARTIAL);
  Wraps: array[Boolean] of Integer = (0, LVFI_WRAP);
var
  Info: TLVFindInfo;
  Index: Integer;
begin
  with Info do
  begin
    flags := LVFI_STRING or FullString[Partial] or Wraps[Wrap];
{$IFDEF CLR}
    psz := Marshal.StringToHGlobalAuto(Value);
{$ELSE}
    psz := PChar(Value);
{$ENDIF}
  end;
  try
    if Inclusive then
      Dec(StartIndex);
    Index := ListView_FindItem(Handle, StartIndex, Info);
    if Index <> -1 then
      Result := Items[Index]
    else
      Result := nil;
  finally
{$IFDEF CLR}
    Marshal.FreeHGlobal(Info.psz);
{$ENDIF}
  end;
end;

function TMyCustomListView.FindData(StartIndex: Integer; Value: TCustomData;
  Inclusive, Wrap: Boolean): TMyListItem;
var
  I: Integer;
  Item: TMyListItem;
begin
  Result := nil;
  if Inclusive then Dec(StartIndex);
  for I := StartIndex + 1 to Items.Count - 1 do
  begin
    Item := Items[I];
    if (Item <> nil) and (Item.Data = Value) then
    begin
      Result := Item;
      Exit;
    end;
  end;
  if Wrap then
  begin
    if Inclusive then
      Inc(StartIndex);
    for I := 0 to StartIndex - 1 do
    begin
      Item := Items[I];
      if (Item <> nil) and (Item.Data = Value) then
      begin
        Result := Item;
        Exit;
      end;
    end;
  end;
end;

procedure TMyCustomListView.FixColumnsWidth;
begin
  Perform(WM_SETREDRAW, 0, 0);
  ShowScrollBar(Handle, SB_VERT, True);
  DoAutoSize;
  Perform(WM_SETREDRAW, 1, 0);
end;

function TMyCustomListView.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TLVHitTestInfo;
begin
  Result := [];
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    ListView_HitTest(Handle, HitTest);

    //! WINBUG: LVHT_ABOVE and LVHT_ONITEMSTATEICON have the same value!
    //! We can determine whether a LVHT_ABOVE ocurred ourselves by checking
    //! whether the Y is below 0, and whether a LVHT_ONITEMSTATEICON ocurred
    //! by
    if ((flags and LVHT_ABOVE) <> 0) and (Y < 0) then Include(Result, htAbove);
    if (flags and LVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and LVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and LVHT_ONITEM) = LVHT_ONITEM then
      Include(Result, htOnItem)
    else
    begin
      if (flags and LVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
      if (flags and LVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
      if (flags and LVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    end;
    if (flags and LVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and LVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;
end;

function TMyCustomListView.GetSelected: TMyListItem;
begin
  Result := GetNextItem(nil, sdAll, [isSelected]);
end;

procedure TMyCustomListView.SetSelected(Value: TMyListItem);
var
  I: Integer;
begin
  if Value <> nil then Value.Selected := True
  else begin
    Value := Selected;
    for I := 0 to SelCount - 1 do
      if Value <> nil then
      begin
        Value.Selected := False;
        Value := GetNextItem(Value, sdAll, [isSelected]);
      end;
  end;
end;

function TMyCustomListView.GetDropTarget: TMyListItem;
begin
  Result := GetNextItem(nil, sdAll, [isDropHilited]);
  if Result = nil then
    Result := FLastDropTarget;
end;

procedure TMyCustomListView.SetDropTarget(Value: TMyListItem);
begin
  if HandleAllocated then
    if Value <> nil then Value.DropTarget := True
    else begin
      Value := DropTarget;
      if Value <> nil then Value.DropTarget := False;
    end;
end;

function TMyCustomListView.GetFocused: TMyListItem;
begin
  Result := GetNextItem(nil, sdAll, [isFocused]);
end;

procedure TMyCustomListView.SetFocused(Value: TMyListItem);
begin
  if HandleAllocated then
    if Value <> nil then Value.Focused := True
    else begin
      Value := ItemFocused;
      if Value <> nil then Value.Focused := False;
    end;
end;

procedure TMyCustomListView.GetImageIndex(Item: TMyListItem);
begin
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, Item);
end;

function TMyCustomListView.GetNextItem(StartItem: TMyListItem;
  Direction: TSearchDirection; States: TItemStates): TMyListItem;
var
  Flags, Index: Integer;
begin
  Result := nil;
  if HandleAllocated then
  begin
    Flags := 0;
    case Direction of
      sdAbove: Flags := LVNI_ABOVE;
      sdBelow: Flags := LVNI_BELOW;
      sdLeft: Flags := LVNI_TOLEFT;
      sdRight: Flags := LVNI_TORIGHT;
      sdAll: Flags := LVNI_ALL;
    end;
    if StartItem <> nil then Index := StartItem.Index
    else Index := -1;
    if isCut in States then Flags := Flags or LVNI_CUT;
    if isDropHilited in States then Flags := Flags or LVNI_DROPHILITED;
    if isFocused in States then Flags := Flags or LVNI_FOCUSED;
    if isSelected in States then Flags := Flags or LVNI_SELECTED;
    Index := ListView_GetNextItem(Handle, Index, Flags);
    if Index <> -1 then Result := Items[Index];
  end;
end;

function TMyCustomListView.GetNearestItem(Point: TPoint;
  Direction: TSearchDirection): TMyListItem;
const
  Directions: array[TSearchDirection] of Integer = (VK_LEFT, VK_RIGHT,
    VK_UP, VK_DOWN, 0);
var
  Info: TLVFindInfo;
  Index: Integer;
begin
  with Info do
  begin
    flags := LVFI_NEARESTXY;
    pt := Point;
    vkDirection := Directions[Direction];
  end;
  Index := ListView_FindItem(Handle, -1, Info);
  if Index <> -1 then Result := Items[Index]
  else Result := nil;
end;

function TMyCustomListView.GetItemAt(X, Y: Integer): TMyListItem;
var
  Info: TLVHitTestInfo;
var
  Index: Integer;
begin
  Result := nil;
  if HandleAllocated then
  begin
    Info.pt := Point(X, Y);
    Index := ListView_HitTest(Handle, Info);
    if Index <> -1 then Result := Items[Index];
  end;
end;

procedure TMyCustomListView.Arrange(Code: TListArrangement);
const
  Codes: array[TListArrangement] of Longint = (LVA_ALIGNBOTTOM,
    LVA_ALIGNLEFT, LVA_ALIGNRIGHT, LVA_ALIGNTOP, LVA_DEFAULT, LVA_SNAPTOGRID);
begin
  ListView_Arrange(Handle, Codes[Code]);
end;

function TMyCustomListView.StringWidth(S: string): Integer;
begin
{$IFDEF CLR}
  Result := ListView_GetStringWidth(Handle, S);
{$ELSE}
  Result := ListView_GetStringWidth(Handle, PChar(S));
{$ENDIF}
end;

procedure TMyCustomListView.UpdateColumns;
var
  I: Integer;
begin
  if HandleAllocated and not FUpdatingColumnOrder then
    for I := 0 to Columns.Count - 1 do UpdateColumn(I);
end;

procedure TMyCustomListView.UpdateGroups;
var
  I: Integer;
  GroupIDs: array of Integer;
begin
  for I := 0 to Groups.Count -1 do
  begin
    if Groups[I].FDescriptionTop <> '' then
    begin
      if (Groups[I].FTitleImage > -1) and (Trim(Groups[I].FHeader) = '') then
        Groups[I].FHeader := Groups[I].FDescriptionTop;
      Groups[I].FDescriptionTop := '';
    end;
    if Groups[I].FDescriptionBottom <> '' then
    begin
      if (Groups[I].FTitleImage > -1) and (Trim(Groups[I].FSubtitle) = '') then
        Groups[I].FSubtitle := Groups[I].FDescriptionBottom;
      Groups[I].FDescriptionBottom := '';
    end;
  end;
  if HandleAllocated then
  begin
    if not OwnerData then
    begin
      SetLength(GroupIDs, Items.Count);

      for I := 0 to Items.Count - 1 do
      begin
        GroupIDs[I] := Items[I].GroupID;
        Items[I].GroupID := -1;
      end;

      for I := 0 to Groups.Count - 1 do
        UpdateGroup(I);

      for I := 0 to Items.Count - 1 do
        Items[I].GroupID := GroupIDs[I];
    end
    else
      for I := 0 to Groups.Count - 1 do
        UpdateGroup(I);
  end;
end;

procedure TMyCustomListView.UpdateHeaderState;
var
  i: Integer;
begin
  Items.BeginUpdate;
  try
    for i := 0 to Columns.Count - 1 do
      Columns[i].UpdateHeaderSortState;
  finally
    Items.EndUpdate;
  end;
end;

procedure TMyCustomListView.UpdateGroup(AnIndex: Integer);
var
  Group: TLVGroup;
begin
  if HandleAllocated then
  begin
{$IFNDEF CLR}
    ZeroMemory(@Group, SizeOf(Group));
{$ENDIF}
    with Group, Groups.Items[AnIndex] do
    begin
      ListView_RemoveGroup(Handle, Groups.Items[AnIndex].GroupID);

      mask := LVGF_HEADER or LVGF_STATE or LVGF_ALIGN;

      if Footer <> '' then
        mask := mask or LVGF_FOOTER;
      uAlign := 0;
{$IFDEF CLR}
      cbSize := Marshal.SizeOf(TypeOf(Group));
      if (FHeaderAlign = taCenter) and (FTitleImage > -1) then
      begin
        pszHeader := Marshal.StringToHGLobalUni('');
        pszSubtitle := Marshal.StringToHGLobalUni('');
        pszDescriptionTop := Marshal.StringToHGLobalUni(Header);
        pszDescriptionBottom := Marshal.StringToHGLobalUni(Subtitle);
      end
      else
      begin
        pszHeader := Marshal.StringToHGLobalUni(Header);
        pszSubtitle := Marshal.StringToHGLobalUni(Subtitle);
        pszDescriptionTop := Marshal.StringToHGLobalUni('');
        pszDescriptionBottom := Marshal.StringToHGLobalUni('');
      end;
      pszFooter := Marshal.StringToHGlobalUni(Footer);
      try
{$ELSE}
      cbSize := SizeOf(Group);
      if (FHeaderAlign = taCenter) and (FTitleImage > -1) then
      begin
        pszHeader := PWideChar('');
        pszSubtitle := PWideChar('');
        pszDescriptionTop := PWideChar(UnicodeString(Header));
        pszDescriptionBottom := PWideChar(UnicodeString(Subtitle));
      end
      else
      begin
        pszHeader := PWideChar(UnicodeString(Header));
        pszSubtitle := PWideChar(UnicodeString(Subtitle));
        pszDescriptionTop := PWideChar('');
        pszDescriptionBottom := PWideChar('');
      end;

      pszFooter := PWideChar(UnicodeString(Footer));
{$ENDIF}

      Group.stateMask := 0;
      Group.state := 0;
      if lgsNormal in Groups.Items[AnIndex].FState then
        Group.state := Group.state or LVGS_NORMAL;
      if lgsHidden in Groups.Items[AnIndex].FState then
        Group.state := Group.state or LVGS_HIDDEN;

      if CheckWin32Version(6) then
      begin
        if lgsNoHeader in Groups.Items[AnIndex].FState then
          Group.state := Group.state or LVGS_NOHEADER;
        if lgsCollapsible in Groups.Items[AnIndex].FState then
          Group.state := Group.state or LVGS_COLLAPSIBLE;
        if lgsCollapsed in Groups.Items[AnIndex].FState then
          Group.state := Group.state or LVGS_COLLAPSED;
        if lgsFocused in Groups.Items[AnIndex].FState then
          Group.state := Group.state or LVGS_FOCUSED;
        if lgsSelected in Groups.Items[AnIndex].FState then
          Group.state := Group.state or LVGS_SELECTED;

        if FTitleImage > -1 then
          mask := mask or LVGF_TITLEIMAGE;
        iTitleImage := TitleImage;

        if (FHeaderAlign = taCenter) and (FTitleImage > -1) then
          mask := mask or LVGF_DESCRIPTIONTOP;

        if Subtitle <> '' then
          if (FHeaderAlign = taCenter) and (FTitleImage > -1) then
            mask := mask or LVGF_DESCRIPTIONBOTTOM or LVGF_SUBTITLE
          else
            mask := mask or LVGF_SUBTITLE;

        if FooterAlign = taLeftJustify then
          uAlign := uAlign or LVGA_FOOTER_LEFT
        else if FooterAlign = taRightJustify then
          uAlign := uAlign or LVGA_FOOTER_RIGHT
        else if FooterAlign = taCenter then
          uAlign := uAlign or LVGA_FOOTER_CENTER;
      end;

      if HeaderAlign = taLeftJustify then
        uAlign := uAlign or LVGA_HEADER_LEFT
      else if HeaderAlign = taRightJustify then
        uAlign := uAlign or LVGA_HEADER_RIGHT
      else if HeaderAlign = taCenter then
        uAlign := uAlign or LVGA_HEADER_CENTER;


      mask := mask or LVGF_GROUPID;
      iGroupId := Groups[AnIndex].GroupID;
      ListView_InsertGroup(Handle, AnIndex, Group);

{$IFDEF CLR}
      finally
        Marshal.FreeHGlobal(pszHeader);
        Marshal.FreeHGlobal(pszFooter);
        Marshal.FreeHGlobal(pszSubtitle);
        Marshal.FreeHGlobal(pszDescriptionTop);
        Marshal.FreeHGlobal(pszDescriptionBottom);
      end;
{$ENDIF}
    end;
  end;
end;

procedure TMyCustomListView.UpdateColumn(AnIndex: Integer);
const
  IAlignment: array[Boolean, TAlignment] of LongInt =
    ((LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_CENTER),
    (LVCFMT_RIGHT, LVCFMT_LEFT, LVCFMT_CENTER));
var
  Column: TLVColumn;
  AAlignment: TAlignment;
begin
  if HandleAllocated then
    with Column, Columns.Items[AnIndex] do
    begin
      mask := LVCF_TEXT or LVCF_FMT or LVCF_IMAGE;
      iImage := FImageIndex;
{$IFDEF CLR}
      pszText := Marshal.StringToHGLobalAuto(Caption);
      try
{$ELSE}
      pszText := PChar(Caption);
{$ENDIF}
        AAlignment := Alignment;
        if Index <> 0 then
          fmt := IAlignment[UseRightToLeftAlignment, AAlignment]
        else
          fmt := LVCFMT_LEFT;
        if FImageIndex <> -1 then
          fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES
        else
          mask := mask and not LVCF_IMAGE;
        if WidthType > ColumnTextWidth then
        begin
          mask := mask or LVCF_WIDTH;
          cx := FWidth;
          ListView_SetColumn(Handle, Columns[AnIndex].FOrderTag, Column);
        end
        else
        begin
          ListView_SetColumn(Handle, Columns[AnIndex].FOrderTag, Column);
          if ViewStyle = vsList then
            ListView_SetColumnWidth(Handle, -1, WidthType)
          else
            if (ViewStyle = vsReport) and not OwnerData then
              ListView_SetColumnWidth(Handle, Columns[AnIndex].FOrderTag, WidthType);
        end;
{$IFDEF CLR}
      finally
        Marshal.FreeHGlobal(pszText);
      end;
{$ENDIF}
    end;
end;

procedure TMyCustomListView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Item: TMyListItem;
  MousePos: TPoint;
  ShiftState: TShiftState;
begin
  SetFocus;
  ShiftState := KeysToShiftState(Message.Keys);
  FClicked := False;
  FDragIndex := -1;
  inherited;
  if (DragMode = dmAutomatic) and MultiSelect then
  begin
    if not (ssShift in ShiftState) and not (ssCtrl in ShiftState) then
    begin
      if not FClicked then
      begin
        Item := GetItemAt(Message.XPos, Message.YPos);
        if (Item <> nil) and Item.Selected then
        begin
          BeginDrag(False);
          Exit;
        end;
      end;
    end;
  end;
  if FClicked then
  begin
    GetCursorPos(MousePos);
    with PointToSmallPoint(ScreenToClient(MousePos)) do
      if not Dragging then
      begin
        Perform(WM_LBUTTONUP, 0, MakeLong(X, Y));
        FClicked := False;
      end
      else SendMessage(GetCapture, WM_LBUTTONUP, 0, MakeLong(X, Y));
  end
  else if (DragMode = dmAutomatic) and not (MultiSelect and
    ((ssShift in ShiftState) or (ssCtrl in ShiftState))) then
  begin
    Item := GetItemAt(Message.XPos, Message.YPos);
    if (Item <> nil) and Item.Selected then
      BeginDrag(False);
  end;
end;

procedure TMyCustomListView.DoAutoSize;
var
  I, Count, WorkWidth, TmpWidth, Remain: Integer;
  List: TList;
  Column: TMyListColumn;
begin
  { Try to fit all sections within client width }
  List := TList.Create;
  try
    WorkWidth := ClientWidth;
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      if Column.AutoSize then
        List.Add(Column)
      else
        Dec(WorkWidth, Column.Width);
    end;
    if List.Count > 0 then
    begin
      Columns.BeginUpdate;
      try
        repeat
          Count := List.Count;
          Remain := WorkWidth mod Count;
          { Try to redistribute sizes to those sections which can take it }
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TMyListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            Column.Width := TmpWidth;
          end;

          { Verify new sizes don't conflict with min/max section widths and
            adjust if necessary. }
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TMyListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            if Column.Width <> TmpWidth then
            begin
              List.Delete(I);
              Dec(WorkWidth, Column.Width);
            end;
          end;
        until (List.Count = 0) or (List.Count = Count);
      finally
        Columns.EndUpdate;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TMyCustomListView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if not (csReading in ComponentState) and
     (Message.WindowPos.flags and SWP_NOSIZE = 0) and HandleAllocated then
    DoAutoSize;
  inherited;
end;

function TMyCustomListView.GetSearchString: string;
{$IFDEF CLR}
var
  Buffer: string;
begin
  Result := '';
  if HandleAllocated and ListView_GetISearchString(Handle, Buffer) then
    Result := Buffer;
{$ELSE}
var
  Len: Integer;
begin
  Result := '';
  if HandleAllocated then
  begin
    Len := SendMessage(Handle, LVM_GETISEARCHSTRING, 0, 0);
    if Len > 0 then
    begin
      SetLength(Result, Len);
      ListView_GetISearchString(Handle, PChar(Result));
    end;
  end;
{$ENDIF}
end;

procedure TMyCustomListView.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  SaveIndex: Integer;
begin
  if Assigned(FCanvas) then
  begin
    with Message.DrawItemStruct{$IFNDEF CLR}^{$ENDIF} do
    begin
      State := TOwnerDrawState(LoWord(itemState));
      SaveIndex := SaveDC(hDC);
      FCanvas.Lock;
      try
        FCanvas.Handle := hDC;
        FCanvas.Font := Font;
        FCanvas.Brush := Brush;
        if itemID = DWORD(-1) then
          FCanvas.FillRect(rcItem)
        else
          DrawItem(Items[itemID], rcItem, State);
      finally
        FCanvas.Handle := 0;
        FCanvas.Unlock;
        RestoreDC(hDC, SaveIndex);
      end;
    end;
    Message.Result := 1;
  end;
end;

{ CustomDraw support }

procedure TMyCustomListView.CanvasChanged;
begin
  FCanvasChanged := True;
end;

function TMyCustomListView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  { List view doesn't support erase notifications }
  if Stage = cdPrePaint then
  begin
    if Target = dtSubItem then
      Result := Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem)
    else if Target = dtItem then
      Result := Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem)
    else
      Result := Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) or
        Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem);
  end
  else
  begin
    if Target = dtSubItem then
      Result := Assigned(FOnAdvancedCustomDrawSubItem) or Assigned(FOnCustomDrawSubItem)
    else if Target = dtItem then
      Result := Assigned(FOnAdvancedCustomDrawItem) or Assigned(FOnCustomDrawItem)
    else
      Result := Assigned(FOnAdvancedCustomDraw) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnAdvancedCustomDrawSubItem);
  end;
end;

function TMyCustomListView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDraw) then FOnCustomDraw(Self, ARect, Result);
  if Assigned(FOnAdvancedCustomDraw) then FOnAdvancedCustomDraw(Self, ARect, Stage, Result)
end;

function TMyCustomListView.CustomDrawItem(Item: TMyListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawItem) then FOnCustomDrawItem(Self, Item, State, Result);
  if Assigned(FOnAdvancedCustomDrawItem) then FOnAdvancedCustomDrawItem(Self, Item, State, Stage, Result);
end;

function TMyCustomListView.CustomDrawSubItem(Item: TMyListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawSubItem) then
    FOnCustomDrawSubItem(Self, Item, SubItem, State, Result);
  if Assigned(FOnAdvancedCustomDrawSubItem) then
    FOnAdvancedCustomDrawSubItem(Self, Item, SubItem, State, Stage, Result);
end;

procedure TMyCustomListView.DrawItem(Item: TMyListItem; Rect: TRect;
  State: TOwnerDrawState);
begin
  TControlCanvas(FCanvas).UpdateTextFlags;
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Item, Rect, State)
  else
  begin
    FCanvas.FillRect(Rect);
    FCanvas.TextOut(Rect.Left + 2, Rect.Top, Item.Caption);
  end;
end;

procedure TMyCustomListView.GetSubItemImage(Item: TMyListItem;
  SubItem: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnGetSubItemImage) and (SubItem < Item.SubItems.Count) and (SubItem >= 0) then
    FOnGetSubItemImage(Self, Item, SubItem, ImageIndex);
end;

procedure TMyCustomListView.DrawWorkAreas;
var
  I, dX, dY: Integer;
  R: TRect;
begin
  with FCanvas do
  begin
    Brush.Style := bsClear;
    for I := 0 to WorkAreas.Count-1 do
    begin
      Pen.Color := WorkAreas[I].Color;
      Pen.Style := psDot;
      dX := -GetViewOrigin.X;
      dY := -GetViewOrigin.Y;
      R := WorkAreas[I].Rect;
      OffsetRect(R, dX, dY);
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      if WorkAreas[I].DisplayName <> '' then
      begin
        Pen.Style := psSolid;
        Font.Color := WorkAreas[I].Color;
        TextOut(R.Left, R.Bottom, WorkAreas[I].DisplayName);
      end;
    end;
  end;
end;

procedure TMyCustomListView.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (ViewStyle in [vsIcon, vsSmallIcon]) and FShowWorkAreas then
    DrawWorkAreas;
end;

procedure TMyCustomListView.SetShowWorkAreas(const Value: Boolean);
begin
  FShowWorkAreas := Value;
  Invalidate;
end;

{ InfoTip support }

procedure TMyCustomListView.CMHintShow(var Message: TCMHintShow);
var
  Item: TMyListItem;
  ItemRect: TRect;
  InfoTip: string;
{$IFDEF CLR}
  Info: THintInfo;
{$ELSE}
  Info: PHintInfo;
{$ENDIF}
begin
  if Assigned(FOnInfoTip) then
    with Message do
    begin
      Item := GetItemAt(HintInfo.CursorPos.X, HintInfo.CursorPos.Y);
      if Item <> nil then
      begin
        Info := HintInfo;
        InfoTip := Item.Caption;
        DoInfoTip(Item, InfoTip);
        ItemRect := Item.DisplayRect(drBounds);
        ItemRect.TopLeft := ClientToScreen(ItemRect.TopLeft);
        ItemRect.BottomRight := ClientToScreen(ItemRect.BottomRight);
        with Info{$IFNDEF CLR}^{$ENDIF} do
        begin
          CursorRect := ItemRect;
          HintStr := InfoTip;
          HintPos.Y := CursorRect.Top + GetSystemMetrics(SM_CYCURSOR);
          HintPos.X := CursorRect.Left + GetSystemMetrics(SM_CXCURSOR);
          HintMaxWidth := ClientWidth;
          Message.Result := 0;
        end;
{$IFDEF CLR}
        HintInfo := Info;
{$ENDIF}
      end;
    end
  else
    inherited;
end;

procedure TMyCustomListView.DoInfoTip(Item: TMyListItem; var InfoTip: string);
begin
  if Assigned(FOnInfoTip) then FOnInfoTip(Self, Item, InfoTip);
end;

procedure TMyCustomListView.SetHoverTime(Value: Integer);
begin
  if Value <> HoverTime then
  begin
    if HandleAllocated then
      ListView_SetHoverTime(Handle, Value)
    else
      FHoverTime := Value;
  end;
end;

procedure TMyCustomListView.SetGroupView(Value: Boolean);
begin
  if Value <> FGroupView then
  begin
    FGroupView := Value;
    if HandleAllocated then
    begin
      ListView_EnableGroupView(Handle, FGroupView);
    end;
  end;
end;

function TMyCustomListView.GetHoverTime: Integer;
begin
  if HandleAllocated then
    Result := ListView_GetHoverTime(Handle)
  else
    Result := FHoverTime;
end;

procedure TMyCustomListView.SaveIndents;
var
  I: Integer;
begin
  if OwnerData then Exit;
  SetLength(FSavedIndents, Items.Count);
  for I := 0 to Items.Count - 1 do
    FSavedIndents[I] := Items[I].Indent;
end;

procedure TMyCustomListView.RestoreIndents;
var
  I: Integer;
begin
  if OwnerData then Exit;
  if Length(FSavedIndents) = Items.Count then
  begin
    for I := 0 to Items.Count - 1 do
      Items[I].Indent := FSavedIndents[I];
  end;
  SetLength(FSavedIndents, 0);
end;

function TMyCustomListView.AreItemsStored: Boolean;
begin
  if Assigned(Action) then
  begin
    if Action is TCustomListAction then
      Result := False
    else
      Result := True;
  end
  else
    Result := not OwnerData;
end;

procedure TMyCustomListView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (GetItemAt(X, Y) <> nil) or not FClicked then
    inherited;
end;

function TMyCustomListView.GetColumnFromTag(Tag: Integer): TMyListColumn;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    Result := Columns[I];
    if Result.FOrderTag = Tag then Exit;
  end;
  Result := nil;
end;

procedure TMyCustomListView.WMContextMenu(var Message: TWMContextMenu);
begin
  if not (csDesigning in ComponentState) then
    SetFocus;
  if InvalidPoint(Message.Pos) and (Selected <> nil) then
    Message.Pos := PointToSmallPoint(ClientToScreen(CenterPoint(Selected.DisplayRect(drSelectBounds))));
  inherited;
end;

procedure TMyCustomListView.WMCtlColorEdit(var Message: TMessage);
begin
  if (csGlassPaint in ControlState) and not FInBufferedPrintClient then
  begin
    FInBufferedPrintClient := True;
    PostMessage(FEditHandle, CM_BUFFEREDPRINTCLIENT, 0, 0);
  end
  else
    with Message do
      Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TMyCustomListView.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := False;
end;

procedure TMyCustomListView.CopySelection(Destination: TCustomListControl);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Items[I].Selected then
{$IFDEF CLR}
      Destination.AddItem(Items[I].Caption, TObject(Items[I].Data));
{$ELSE}
      Destination.AddItem(Items[I].Caption, Items[I].Data);
{$ENDIF}
end;

procedure TMyCustomListView.DeleteSelected;
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    for I := Items.Count - 1 downto 0 do
      if Items[I].Selected then
        Delete(Items[I]);
  finally
    Items.EndUpdate;
  end;
end;

function TMyCustomListView.GetCount: Integer;
begin
  Result := Items.Count;
end;

procedure TMyCustomListView.SelectAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := True;
end;

procedure TMyCustomListView.Clear;
begin
  FListItems.BeginUpdate;
  try
    FListItems.Clear;
  finally
    FListItems.EndUpdate;
  end;
end;

function TMyCustomListView.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TListViewActionLink;
end;

procedure TMyCustomListView.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TStaticListAction then
    with TStaticListAction(Sender) do
    begin
      if not CheckDefaults or (Self.SmallImages = nil) then
        Self.SmallImages := Images;
      if not CheckDefaults or (Self.ItemIndex <> -1) then
        Self.ItemIndex := ItemIndex;
    end;
end;

procedure TMyCustomListView.WMVScroll(var Message: TWMVScroll);
var
  Before,
  After: Integer;
begin
  if StyleServices.Enabled then
  begin
    Before := GetScrollPos(Handle, SB_VERT);
    inherited;
    After := GetScrollPos(Handle, SB_VERT);
    if (Before <> After) then
      InvalidateRect(Handle, nil, True);
  end
  else
    inherited;
end;

{ TListView }

class constructor TMyListView.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TMyListView, TListViewStyleHook);
end;

{ TMyListItem }

constructor TMyListItem.Create(AOwner: TMyListItems);
begin
  inherited Create;
  FOwner := AOwner;
  FSubItems := TSubItems.Create(Self);
  FOverlayIndex := -1;
  FStateIndex := -1;
  FGroupID := -1;
{$IFDEF CLR}
  // Add to hashtable to prevent premature garbage collection, and allow
  // access to the actual ListItem object from the LVItem struct
  FOwner.FItemHashTable.Add(TObject(GetHashCode), Self);
{$ENDIF}
end;

destructor TMyListItem.Destroy;
begin
  FDeleting := True;
  if Owner.Owner.FLastDropTarget = Self then
    Owner.Owner.FLastDropTarget := nil;
  if not ListView.FDeletingAllItems and ListView.HandleAllocated then
    ListView_DeleteItem(Handle, Index);
{$IFDEF CLR}
  FOwner.FItemHashTable.Remove(TObject(GetHashCode));
{$ENDIF}
  FSubItems.Free;
  inherited Destroy;
end;

function TMyListItem.GetListView: TMyCustomListView;
begin
  Result := Owner.Owner;
end;

procedure TMyListItem.Delete;
begin
  if not FDeleting and (Self <> ListView.FTempItem) then
    Free;
end;

function TMyListItem.GetHandle: HWND;
begin
  Result := ListView.Handle;
end;

procedure TMyListItem.MakeVisible(PartialOK: Boolean);
begin
  ListView_EnsureVisible(Handle, Index, PartialOK);
end;

function TMyListItem.GetChecked: Boolean;
begin
  with Owner.Owner do
    if not OwnerData and HandleAllocated then
      Result := (ListView_GetCheckState(Handle, Index) <> 0)
    else
      Result := FChecked;
end;

procedure TMyListItem.SetChecked(Value: Boolean);
var
  LV: TMyCustomListView;
begin
  if Value <> Checked then
  begin
    FChecked := Value;
    LV := Owner.Owner;
    if not LV.OwnerData and LV.HandleAllocated then
      ListView_SetCheckState(LV.Handle, Index, Value);
  end;
end;

procedure TMyListItem.SetGroupID(Value: Integer);
var
  Item: TLVItem;
begin
  if Value <> FGroupID then
  begin
    {$IFNDEF CLR}
    ZeroMemory(@Item, SizeOf(Item));
    {$ENDIF}
    with Item do
    begin
      mask := LVIF_GROUPID;
      iItem := Self.Index;
      iGroupID := Value;
    end;
    ListView_SetItem(Handle, Item);
    FGroupID := Value;
  end;
end;

function TMyListItem.GetLeft: Integer;
begin
  Result := GetPosition.X;
end;

procedure TMyListItem.SetLeft(Value: Integer);
begin
  SetPosition(Point(Value, Top));
end;

function TMyListItem.GetTop: Integer;
begin
  Result := GetPosition.Y;
end;

procedure TMyListItem.SetTop(Value: Integer);
begin
  SetPosition(Point(Left, Value));
end;

procedure TMyListItem.Update;
begin
  ListView_Update(Handle, Index);
end;

procedure TMyListItem.SetCaption(const Value: string);
begin
  if Value <> Caption then
  begin
    FCaption := Value;
    if not Owner.Owner.OwnerData then
{$IFDEF CLR}
      ListView_SetItemText(Handle, Index, 0, IntPtr(Integer(LPSTR_TEXTCALLBACK)));
{$ELSE}
      ListView_SetItemText(Handle, Index, 0, LPSTR_TEXTCALLBACK);
{$ENDIF}
    if ListView.ColumnsShowing and
      (ListView.Columns.Count > 0) and
      (ListView.Column[0].WidthType <= ColumnTextWidth) then
      ListView.UpdateColumns;
    if ListView.SortType in [stBoth, stText] then
      ListView.AlphaSort;
  end;
end;

procedure TMyListItem.SetData(Value: TCustomData);
begin
  if Value <> Data then
  begin
    FData := Value;
    if ListView.SortType in [stBoth, stData] then
      ListView.AlphaSort;
  end;
end;

function TMyListItem.EditCaption: Boolean;
begin
  ListView.SetFocus;
  Result := ListView_EditLabel(Handle, Index) <> 0;
end;

procedure TMyListItem.CancelEdit;
begin
  ListView_EditLabel(Handle, -1);
end;

function TMyListItem.GetState(Index: Integer): Boolean;
var
  Mask: Integer;
begin
  case Index of
    0: Mask := LVIS_CUT;
    1: Mask := LVIS_DROPHILITED;
    2: Mask := LVIS_FOCUSED;
    3: Mask := LVIS_SELECTED;
    4: Mask := LVIS_ACTIVATING;
  else
    Mask := 0;
  end;
  Result := ListView_GetItemState(Handle, Self.Index, Mask) and Mask <> 0;
end;

procedure TMyListItem.SetState(Index: Integer; State: Boolean);
var
  Mask: Integer;
  Data: Integer;
begin
  case Index of
    0: Mask := LVIS_CUT;
    1: Mask := LVIS_DROPHILITED;
    2: Mask := LVIS_FOCUSED;
    3: Mask := LVIS_SELECTED;
    4: Mask := LVIS_ACTIVATING;
  else
    Mask := 0;
  end;
  if State then
    Data := Mask
  else
    Data := 0;
  ListView_SetItemState(Handle, Self.Index, Data, Mask);
end;

procedure TMyListItem.SetImage(Index: Integer; Value: TImageIndex);
var
  Item: TLVItem;
  LChanged: Boolean;
begin
  LChanged := False;
  case Index of
    0:  if Value <> FImageIndex then
        begin
          LChanged := True;
          FImageIndex := Value;
          if not Owner.Owner.OwnerData then
          begin
            with Item do
            begin
              mask := LVIF_IMAGE;
              iImage := I_IMAGECALLBACK;
              iItem := Self.Index;
              iSubItem := 0;
            end;
            ListView_SetItem(Handle, Item);
          end;
        end;
    1:  if Value <> FOverlayIndex then
        begin
          LChanged := True;
          FOverlayIndex := Value;
          if not Owner.Owner.OwnerData then
            ListView_SetItemState(Handle, Self.Index,
              IndexToOverlayMask(OverlayIndex + 1), LVIS_OVERLAYMASK);
        end;
    2:  if Value <> FStateIndex then
        begin
          LChanged := True;
          FStateIndex := Value;
          if Owner.Owner.CheckBoxes and (Value = -1) then
            Value := 0;
          if not Owner.Owner.OwnerData then
            ListView_SetItemState(Handle, Self.Index,
              IndexToStateImageMask(Value + 1), LVIS_STATEIMAGEMASK);
        end;
  end;
  if LChanged and not Owner.Owner.OwnerData then
    ListView.UpdateItems(Self.Index, Self.Index);
end;

procedure TMyListItem.SetIndent(Value: Integer);
var
  Item: TLVItem;
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    if not Owner.Owner.OwnerData then
    begin
      with Item do
      begin
        mask := LVIF_INDENT;
        iIndent := Value;
        iItem := Self.Index;
        iSubItem := 0;
      end;
      ListView_SetItem(Handle, Item);
      ListView.UpdateItems(Self.Index, Self.Index);
    end;
  end;
end;

procedure TMyListItem.Assign(Source: TPersistent);
begin
  if Source is TMyListItem then
    with Source as TMyListItem do
    begin
      Self.Caption := Caption;
      Self.Data := Data;
      Self.ImageIndex := ImageIndex;
      Self.Indent := Indent;
      Self.OverlayIndex := OverlayIndex;
      Self.StateIndex := StateIndex;
      Self.SubItems := SubItems;
      Self.Checked := Checked;
      Self.GroupID := GroupID;
    end
  else inherited Assign(Source);
end;

function TMyListItem.IsEqual(Item: TMyListItem): Boolean;
begin
  Result := (Caption = Item.Caption) and (Data = Item.Data);
end;

procedure TMyListItem.SetSubItems(Value: TStrings);
begin
  if Value <> nil then
    FSubItems.Assign(Value);
end;

function TMyListItem.GetIndex: Integer;
begin
  if Owner.Owner.OwnerData then
    Result := FIndex else
    Result := Owner.IndexOf(Self);
end;

function TMyListItem.GetPosition: TPoint;
begin
  ListView_GetItemPosition(Handle, Index, Result);
end;

procedure TMyListItem.SetPosition(const Value: TPoint);
var
  LAt: TPoint;
begin
  LAt := Position;
  if (LAt.X <> Value.X) or (LAt.Y <> Value.Y) then
    if ListView.ViewStyle in [vsSmallIcon, vsIcon] then
      ListView_SetItemPosition32(Handle, Index, Value.X, Value.Y);
end;

function TMyListItem.DisplayRect(Code: TDisplayCode): TRect;
const
  Codes: array[TDisplayCode] of Longint = (LVIR_BOUNDS, LVIR_ICON, LVIR_LABEL,
    LVIR_SELECTBOUNDS);
begin
  ListView_GetItemRect(Handle, Index, Result, Codes[Code]);
end;

function TMyListItem.GetSubItemImage(Index: Integer): Integer;
begin
  Result := TSubItems(FSubItems).ImageIndex[Index];
end;

procedure TMyListItem.SetSubItemImage(Index: Integer; const Value: Integer);
var
  item: TLVItem;
begin
  {Storage of sub-item image indices cannot be provided by the control because
   all display-related content requires a callback}
  if TSubItems(FSubItems).ImageIndex[Index] <> Value then
  begin
    TSubItems(FSubItems).ImageIndex[Index] := Value;
    if not Owner.Owner.OwnerData then
    begin
      with item do
      begin
        mask := LVIF_IMAGE;
        iImage := I_IMAGECALLBACK;
        iItem := Self.Index;
        iSubItem := Index+1;
      end;
      ListView_SetItem(Handle, item);
    end;
  end;
end;

function TMyListItem.WorkArea: Integer;
begin
  with Owner.Owner.WorkAreas do
  begin
    Result := Count-1;
    while (Result >= 0) and not PtInRect(Items[Result].Rect, GetPosition) do
      Dec(Result);
  end;
end;

{ TMyListItems }

type
{$IFNDEF CLR}
  PItemHeader = ^TItemHeader;
{$ENDIF}
  TItemHeader = record
    Size, Count: Integer;
{$IFDEF CLR}
    Items: THandle;
{$ELSE}
    Items: record end;
{$ENDIF}
  end;

{$IFNDEF CLR}
  PItemInfo = ^TItemInfo;
{$ENDIF}
  TItemInfo = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Integer;
    {Caption: string[255];}  // .NET: No longer used in structure
{$IFNDEF CLR}
    Caption: string[255];
{$ENDIF}
  end;

{$ALIGN 1}
{$IFNDEF CLR}
  PItemInfo2 = ^TItemInfo2;
{$ENDIF}
  TItemInfo2 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: TCustomData;
    {Caption: string[255];}  // .NET: No longer used in structure
{$IFNDEF CLR}
    Caption: string[255];
{$ENDIF}
  end;

{$IFNDEF CLR}
  TItemDataInfo = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Pointer;
    CaptionLen: Byte;
    // WideString Caption of CaptionLen chars follows
  end;

  TItemDataInfoX86 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Integer;  // must be integer
    CaptionLen: Byte;
    // WideString Caption of CaptionLen chars follows
  end;

  TItemDataInfo2 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Pointer;
    CaptionLen: Byte;
    // WideString Caption of CaptionLen chars follows
  end;

  // Identical to TItemDataInfo2 except Data field is explicitly
  // 32-bits, used for reading 32-bit streams (ensuring compatibility
  // in 64-bit applications).
  TItemDataInfo2x86 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Integer; // must be Integer
    CaptionLen: Byte;
    // WideString Caption of CaptionLen chars follows
  end;

  // Identical to TItemDataInfo2 except Data field is explicitly
  // 64-bits, used for reading 64-bit streams (ensuring compatibility
  // in 32-bit applications).
  TItemDataInfo2x64 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Int64; // must be Int64
    CaptionLen: Byte;
    // WideString Caption of CaptionLen chars follows
  end;
{$ALIGN ON}

  ShortStr = string[255];
  PShortStr = ^ShortStr;
{$ENDIF}

constructor TMyListItems.Create(AOwner: TMyCustomListView);
begin
  inherited Create;
  FOwner := AOwner;
{$IFDEF CLR}
  FItemHashTable := HashTable.Create;
{$ENDIF}
end;

destructor TMyListItems.Destroy;
begin
  Clear;
{$IFDEF CLR}
  FItemHashTable.Free;
{$ENDIF}
  inherited Destroy;
end;

function TMyListItems.Add: TMyListItem;
begin
  Result := AddItem(nil, -1);
end;

function TMyListItems.Insert(Index: Integer): TMyListItem;
begin
  Result := AddItem(nil, Index);
end;

function TMyListItems.AddItem(Item: TMyListItem; Index: Integer): TMyListItem;
begin
  if Item = nil then
    Result := Owner.CreateListItem
  else
    Result := Item;
  if Index < 0 then
    Index := Count;
  ListView_InsertItem(Handle, CreateItem(Index, Result));
end;

function TMyListItems.GetCount: Integer;
begin
  if Owner.HandleAllocated then
    Result := ListView_GetItemCount(Handle)
  else
    Result := 0;
end;

//function TMyListItems.GetEnumerator: TMyListItemsEnumerator;
//begin
//  Result := TMyListItemsEnumerator.Create(Self);
//end;

function TMyListItems.GetHandle: HWND;
begin
  Result := Owner.Handle;
end;

function TMyListItems.GetItem(Index: Integer): TMyListItem;
var
  Item: TLVItem;
begin
  Result := nil;
  if Owner.Handle <> 0 then
  begin
    if Owner.OwnerData then
    begin
{$IFNDEF CLR}
      FillChar(Item, SizeOf(Item), 0);
{$ENDIF}
      with Item do
      begin
        mask := 0;
        iItem := Index;
        iSubItem := 0;
      end;
      Result := Owner.GetItem(Item);
    end
    else
    begin
      with Item do
      begin
        mask := LVIF_PARAM;
        iItem := Index;
        iSubItem := 0;
      end;
      if ListView_GetItem(Handle, Item) then
{$IFDEF CLR}
        Result := TMyListItem(FItemHashTable.Item[TObject(Integer(Item.lParam))]);
{$ELSE}
        Result := TMyListItem(Item.lParam);
{$ENDIF}
    end;
  end;
end;

function TMyListItems.IndexOf(Value: TMyListItem): Integer;
var
  Info: TLVFindInfo;
begin
  with Info do
  begin
    flags := LVFI_PARAM;
{$IFDEF CLR}
    lParam := Value.GetHashCode;
{$ELSE}
    lParam := Winapi.Windows.LPARAM(Value);
{$ENDIF}
    Result := ListView_FindItem(Handle, -1, Info);
  end;
end;

procedure TMyListItems.SetCount(Value: Integer);
begin
  if Value <> 0 then
    ListView_SetItemCountEx(Handle, Value, LVSICF_NOINVALIDATEALL)
  else
    ListView_SetItemCountEx(Handle, Value, 0);
end;

procedure TMyListItems.SetItem(Index: Integer; Value: TMyListItem);
begin
  Item[Index].Assign(Value);
end;

procedure TMyListItems.Clear;
begin
  if Owner.HandleAllocated then ListView_DeleteAllItems(Handle);
end;

procedure TMyListItems.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TMyListItems.SetUpdateState(Updating: Boolean);
var
  i: Integer;
begin
  if Updating then
  begin
    with Owner do
    begin
      FSavedSort := SortType;
      SortType := stNone;
    end;
    for i := 0 to Owner.Columns.Count - 1 do
    begin
      with Owner.Columns[i] as TMyListColumn do
        if WidthType < 0 then
        begin
          FPrivateWidth := WidthType;
          FWidth := Width;
          DoChange;
        end;
    end;
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    if Owner.ColumnsShowing and Owner.ValidHeaderHandle then
      SendMessage(Owner.FHeaderHandle, WM_SETREDRAW, 0, 0);
  end
  else if FUpdateCount = 0 then
  begin
    Owner.SortType := Owner.FSavedSort;
    for i := 0 to Owner.Columns.Count - 1 do
    begin
      with Owner.Columns[i] as TMyListColumn do
        if FPrivateWidth < 0 then
        begin
          Width := FPrivateWidth;
          FPrivateWidth := 0;
        end;
    end;
    FNoRedraw := True;
    try
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      Owner.Invalidate;
    finally
      FNoRedraw := False;
    end;
    if Owner.ColumnsShowing and Owner.ValidHeaderHandle then
      SendMessage(Owner.FHeaderHandle, WM_SETREDRAW, 1, 0);
  end;
end;

procedure TMyListItems.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

procedure TMyListItems.Assign(Source: TPersistent);
var
  Items: TMyListItems;
  I: Integer;
begin
  if Source is TMyListItems then
  begin
    Clear;
    Items := TMyListItems(Source);
    for I := 0 to Items.Count - 1 do Add.Assign(Items[I]);
  end
  else inherited Assign(Source);
end;

procedure TMyListItems.DefineProperties(Filer: TFiler);

  function WriteItems: Boolean;
  var
    I: Integer;
    Items: TMyListItems;
  begin
    Items := TMyListItems(Filer.Ancestor);
    if (Items = nil) then
      Result := (Count > 0) or ((Owner.FMemStream <> nil) and not Owner.HandleAllocated)
    else if (Items.Count <> Count) then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Item[I].IsEqual(Items[I]);
        if Result then Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  // Data property is platform specific (Ansi)
  // ItemData property stores data in Unicode
  Filer.DefineBinaryProperty('Data', ReadData, nil, False);
  Filer.DefineBinaryProperty('ItemData', ReadItemData, WriteItemData, WriteItems);
end;

{$REGION CLR_LEGACY}
{$IFDEF CLR}
{$ALIGN 1}
type
  // TItemInfo32 is equivalent in size to TItemInfo on a 32-bit platform.
  // This record is used by 64-bit applications loading TMyListItems created
  // by 32-bit applications, to avoid size errors caused by TItemInfo.Data.
  TItemInfo32 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Integer; { TObject }
    {Caption: string[255];}  // No longer used in structure
  end;

  TItemInfo232 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Integer; { TObject }
    {Caption: string[255];}  // No longer used in structure
  end;

  // TItemInfo64 is equivalent in size to TItemInfo on a 64-bit platform.
  // This record is used by 32-bit applications loading TMyListItems created
  // by 64-bit applications, to avoid size errors caused by TItemInfo.Data.
  TItemInfo64 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Int64; { TObject }
    {Caption: string[255];}  // No longer used in structure
  end;

  TItemInfo264 = record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Int64; { TObject }
    {Caption: string[255];}  // No longer used in structure
  end;

  TItemDataType = (idtDefault, idt2Default, idt32bit, idt64bit, idt232bit, idt264bit);
{$ALIGN ON}
{$ENDIF}
{$ENDREGION CLR_LEGACY}

procedure TMyListItems.ReadData(Stream: TStream);
{$IFDEF CLR}
{$REGION CLR_LEGACY}
var
  Buffer, ItemInfoBuf: TBytes;
  I, J, Size, ItemCount, L, Len: Integer;
  ItemInfo: TItemInfo32;
begin
  Clear;
  Stream.ReadBuffer(Size, SizeOf(Integer));
  Stream.ReadBuffer(ItemCount, SizeOf(Integer));
  L := Marshal.SizeOf(TypeOf(TItemInfo32));
  SetLength(ItemInfoBuf, L);

  for I := 0 to ItemCount - 1 do
  begin
    Stream.ReadBuffer(ItemInfoBuf, L);
    with Add do
    begin
      // Read TItemInfo structure
      ItemInfo := TItemInfo32(BytesToStructure(ItemInfoBuf, TypeOf(TItemInfo32)));
      ImageIndex := ItemInfo.ImageIndex;
      OverlayIndex := ItemInfo.OverlayIndex;
      StateIndex := ItemInfo.StateIndex;
      if ItemInfo.Data <> 0 then
        Data := TObject(ItemInfo.Data);

      // Read length of Caption, followed by actual text
      Stream.ReadBuffer(Len, SizeOf(Byte));
      SetLength(Buffer, Len);
      Stream.ReadBuffer(Buffer, Len);
      Caption := StringOf(Buffer);

      // Read length of each SubItem, followed by actual text
      for J := 0 to ItemInfo.SubItemCount - 1 do
      begin
        Stream.ReadBuffer(Len, SizeOf(Byte));
        SetLength(Buffer, Len);
        Stream.ReadBuffer(Buffer, Len);
        SubItems.Add(StringOf(Buffer));
      end;
    end;
  end;

  // Read subitem images, if present
  for I := 0 to Count - 1 do
    with Item[I] do
      for J := 0 to SubItems.Count - 1 do
      begin
        Stream.ReadBuffer(Len, SizeOf(SmallInt));
        SubItemImages[J] := Len;
      end;
{$ENDREGION CLR_LEGACY}
{$ELSE}
var
  I, J, Size, L, Len: Integer;
  ItemHeader: PItemHeader;
  ItemInfo: PItemInfo;
  PStr: PShortStr;
  PInt: PSmallInt;
begin
  Clear;
  Stream.ReadBuffer(Size, SizeOf(Integer));
  ItemHeader := AllocMem(Size);
  try
    Stream.ReadBuffer(ItemHeader^.Count, Size - SizeOf(Integer));
    ItemInfo := @ItemHeader^.Items;
    PStr := nil;
    for I := 0 to ItemHeader^.Count - 1 do
    begin
      with Add do
      begin
        Caption := string(ItemInfo^.Caption);
        ImageIndex := ItemInfo^.ImageIndex;
        OverlayIndex := ItemInfo^.OverlayIndex;
        StateIndex := ItemInfo^.StateIndex;
        Data := Pointer(ItemInfo^.Data);
        PStr := @ItemInfo^.Caption;
        Inc(PByte(PStr), Length(PStr^) + 1);
        Len := 0;
        for J := 0 to ItemInfo^.SubItemCount - 1 do
        begin
          SubItems.Add(string(PStr^));
          L := Length(PStr^);
          Inc(Len, L + 1);
          Inc(PByte(PStr), L + 1);
        end;
      end;
      Inc(PByte(ItemInfo), SizeOf(TItemInfo) - 255 +
        Length(ItemInfo.Caption) + Len);
    end;
    //read subitem images, if present.
    if PChar(PStr) - PChar(ItemHeader) < Size then
    begin
      PInt := Pointer(PStr);
      for I := 0 to Count - 1 do
        with Item[I] do
          for J := 0 to SubItems.Count - 1 do
          begin
            SubItemImages[J] := PInt^;
            Inc(PInt);
          end;
    end;
  finally
    FreeMem(ItemHeader, Size);
  end;
{$ENDIF}
end;

const
  ListItemStreamVersion1    = $01; // 32-bit struct size version 1
  ListItemStreamVersion1x64 = $02; // 64-bit struct size version 1
  ListItemStreamVersion2    = $03; // 32-bit struct size version 2
  ListItemStreamVersion2x64 = $04; // 64-bit struct size version 2
  ListItemStreamVersion3    = $05; // 32-bit struct size version 3
  ListItemStreamVersion3x64 = $06; // 64-bit struct size version 3

procedure TMyListItems.ReadItemData(Stream: TStream);
{$IFDEF CLR}
{$REGION CLR_LEGACY}
var
  StreamVersion: Byte;
  Buffer, ItemInfoBuf: TBytes;
  I, J, Size, ItemCount, L, Len, LSubItemCount: Integer;
  LImageIndex: SmallInt;
  ItemDataType: TItemDataType;
  LItemType: System.&Type;
  LItemObject: TObject;
  ItemInfo: TItemInfo;
  ItemInfo32: TItemInfo32;
  ItemInfo64: TItemInfo64;
  ItemInfo2: TItemInfo2;
  ItemInfo232: TItemInfo232;
  ItemInfo264: TItemInfo264;
begin
  Clear;
  Stream.ReadBuffer(StreamVersion, SizeOf(StreamVersion));

  // Determine which structure to use when loading items
  ItemDataType := idtDefault;
  LItemType := TypeOf(TItemInfo);
  case StreamVersion of
    ListItemStreamVersion1:
      if IntPtr.Size = 8 then
      begin
        ItemDataType := idt32bit;
        LItemType := TypeOf(TItemInfo32);
      end;
    ListItemStreamVersion1x64:
      if IntPtr.Size = 4 then
      begin
        ItemDataType := idt64bit;
        LItemType := TypeOf(TItemInfo64);
      end;
    ListItemStreamVersion2:
      if IntPtr.Size = 8 then
      begin
        ItemDataType := idt232bit;
        LItemType := TypeOf(TItemInfo232);
      end
      else
      begin
        ItemDataType := idt2Default;
        LItemType := TypeOf(TItemInfo2);
      end;
    ListItemStreamVersion2x64:
      if IntPtr.Size = 4 then
      begin
        ItemDataType := idt264bit;
        LItemType := TypeOf(TItemInfo264);
      end
      else
      begin
        ItemDataType := idt2Default;
        LItemType := TypeOf(TItemInfo2);
      end;
  else
    Exit;
  end;

  Stream.ReadBuffer(Size, SizeOf(Integer));
  Stream.ReadBuffer(ItemCount, SizeOf(Integer));
  L := Marshal.SizeOf(LItemType);
  SetLength(ItemInfoBuf, L);

  for I := 0 to ItemCount - 1 do
  begin
    Stream.ReadBuffer(ItemInfoBuf, L);
    with Add do
    begin
      // Read TItemInfo structure
      LItemObject := BytesToStructure(ItemInfoBuf, LItemType);
      case ItemDataType of
        idt32bit:
          begin
            ItemInfo32 := TItemInfo32(LItemObject);
            ImageIndex := ItemInfo32.ImageIndex;
            OverlayIndex := ItemInfo32.OverlayIndex;
            StateIndex := ItemInfo32.StateIndex;
            GroupID := -1;
            if ItemInfo32.Data <> 0 then
              Data := TObject(ItemInfo32.Data);
            LSubItemCount := ItemInfo32.SubItemCount;
          end;
        idt64bit:
          begin
            ItemInfo64 := TItemInfo64(LItemObject);
            ImageIndex := ItemInfo64.ImageIndex;
            OverlayIndex := ItemInfo64.OverlayIndex;
            StateIndex := ItemInfo64.StateIndex;
            GroupID := -1;
            if ItemInfo64.Data <> 0 then
              Data := TObject(ItemInfo64.Data);
            LSubItemCount := ItemInfo64.SubItemCount;
          end;
        idt232bit:
          begin
            ItemInfo232 := TItemInfo232(LItemObject);
            ImageIndex := ItemInfo232.ImageIndex;
            OverlayIndex := ItemInfo232.OverlayIndex;
            StateIndex := ItemInfo232.StateIndex;
            GroupID := ItemInfo232.GroupID;
            if ItemInfo232.Data <> 0 then
              Data := TObject(ItemInfo232.Data);
            LSubItemCount := ItemInfo232.SubItemCount;
          end;
        idt264bit:
          begin
            ItemInfo264 := TItemInfo264(LItemObject);
            ImageIndex := ItemInfo264.ImageIndex;
            OverlayIndex := ItemInfo264.OverlayIndex;
            StateIndex := ItemInfo264.StateIndex;
            GroupID := ItemInfo264.GroupID;
            if ItemInfo264.Data <> 0 then
              Data := TObject(ItemInfo264.Data);
            LSubItemCount := ItemInfo264.SubItemCount;
          end;
        idt2Default:
          begin
            ItemInfo2 := TItemInfo2(LItemObject);
            ImageIndex := ItemInfo2.ImageIndex;
            OverlayIndex := ItemInfo2.OverlayIndex;
            StateIndex := ItemInfo2.StateIndex;
            GroupID := ItemInfo2.GroupID;
            Data := ItemInfo2.Data;
            LSubItemCount := ItemInfo2.SubItemCount;
          end;
      else // idtDefault
        begin
          ItemInfo := TItemInfo(LItemObject);
          ImageIndex := ItemInfo.ImageIndex;
          OverlayIndex := ItemInfo.OverlayIndex;
          StateIndex := ItemInfo.StateIndex;
          GroupID := -1;
          Data := ItemInfo.Data;
          LSubItemCount := ItemInfo.SubItemCount;
        end;
      end;

      // Read length of Caption in chars, followed by actual text
      Stream.ReadBuffer(Len, SizeOf(Byte));
      SetLength(Buffer, Len * SizeOf(WideChar));
      Stream.ReadBuffer(Buffer, Length(Buffer));
      Caption := WideStringOf(Buffer);

      // Read length of each SubItem in chars, followed by actual text
      for J := 0 to LSubItemCount - 1 do
      begin
        Stream.ReadBuffer(Len, SizeOf(Byte));
        SetLength(Buffer, Len * SizeOf(WideChar));
        Stream.ReadBuffer(Buffer, Length(Buffer));
        SubItems.Add(WideStringOf(Buffer));
      end;
    end;
  end;

  // Read subitem images, if present
  for I := 0 to Count - 1 do
    with Item[I] do
      for J := 0 to SubItems.Count - 1 do
      begin
        Stream.ReadBuffer(LImageIndex, SizeOf(SmallInt));
        SubItemImages[J] := LImageIndex;
      end;
{$ENDREGION CLR_LEGACY}
{$ELSE}
var
  I, J, Size, ItemCount: Integer;
  LImageIndex: SmallInt;
  StreamVersion, Len: Byte;
  ItemInfo: TItemDataInfoX86;
  ItemInfo2x86: TItemDataInfo2x86;
  ItemInfo2x64: TItemDataInfo2x64;
  LWideStr: UnicodeString;
  LSubItemData: Pointer;
begin
  Clear;
  if Stream.Size = 0 then
    Exit;

  Stream.ReadBuffer(StreamVersion, SizeOf(StreamVersion));
  case StreamVersion of
    ListItemStreamVersion1:
      begin
        Stream.ReadBuffer(Size, SizeOf(Integer));
        Stream.ReadBuffer(ItemCount, SizeOf(Integer));

        for I := 0 to ItemCount - 1 do
        begin
          Stream.ReadBuffer(ItemInfo, SizeOf(TItemDataInfoX86));
          with Add do
          begin
            ImageIndex := ItemInfo.ImageIndex;
            OverlayIndex := ItemInfo.OverlayIndex;
            StateIndex := ItemInfo.StateIndex;
            Data := Pointer(ItemInfo.Data);
            GroupID := -1;

            // Read Caption
            SetLength(LWideStr, ItemInfo.CaptionLen);
            Stream.ReadBuffer(LWideStr[1], ItemInfo.CaptionLen * SizeOf(WideChar));
            Caption := LWideStr;

            // Read length of each SubItem in chars, followed by actual text
            for J := 0 to ItemInfo.SubItemCount - 1 do
            begin
              Stream.ReadBuffer(Len, SizeOf(Byte));
              SetLength(LWideStr, Len);
              Stream.ReadBuffer(LWideStr[1], Len * SizeOf(WideChar));
              SubItems.Add(LWideStr);
            end;
          end;
        end;
      end;
    ListItemStreamVersion2,
    ListItemStreamVersion3:
      begin
        Stream.ReadBuffer(Size, SizeOf(Integer));
        Stream.ReadBuffer(ItemCount, SizeOf(Integer));

        for I := 0 to ItemCount - 1 do
        begin
          Stream.ReadBuffer(ItemInfo2x86, SizeOf(TItemDataInfo2x86));
          with Add do
          begin
            ImageIndex := ItemInfo2x86.ImageIndex;
            OverlayIndex := ItemInfo2x86.OverlayIndex;
            StateIndex := ItemInfo2x86.StateIndex;
            Data := Pointer(ItemInfo2x86.Data);
            GroupID := ItemInfo2x86.GroupID;

            // Read Caption
            SetLength(LWideStr, ItemInfo2x86.CaptionLen);
            Stream.ReadBuffer(LWideStr[1], ItemInfo2x86.CaptionLen * SizeOf(WideChar));
            Caption := LWideStr;

            // Read length of each SubItem in chars, followed by actual text
            for J := 0 to ItemInfo2x86.SubItemCount - 1 do
            begin
              Stream.ReadBuffer(Len, SizeOf(Byte));
              SetLength(LWideStr, Len);
              Stream.ReadBuffer(LWideStr[1], Len * SizeOf(WideChar));
              LSubItemData := nil;
              if StreamVersion = ListItemStreamVersion3 then
                Stream.ReadBuffer(LSubItemData, SizeOf(Integer));
              SubItems.AddObject(LWideStr, Pointer(LSubItemData));
            end;
          end;
        end;
      end;
    ListItemStreamVersion3x64:
      begin
        Stream.ReadBuffer(Size, SizeOf(Integer));
        Stream.ReadBuffer(ItemCount, SizeOf(Integer));

        for I := 0 to ItemCount - 1 do
        begin
          Stream.ReadBuffer(ItemInfo2x64, SizeOf(TItemDataInfo2x64));
          with Add do
          begin
            ImageIndex := ItemInfo2x64.ImageIndex;
            OverlayIndex := ItemInfo2x64.OverlayIndex;
            StateIndex := ItemInfo2x64.StateIndex;
            Data := Pointer(ItemInfo2x64.Data);
            GroupID := ItemInfo2x64.GroupID;

            // Read Caption
            SetLength(LWideStr, ItemInfo2x64.CaptionLen);
            Stream.ReadBuffer(LWideStr[1], ItemInfo2x64.CaptionLen * SizeOf(WideChar));
            Caption := LWideStr;

            // Read length of each SubItem in chars, followed by actual text
            for J := 0 to ItemInfo2x64.SubItemCount - 1 do
            begin
              Stream.ReadBuffer(Len, SizeOf(Byte));
              SetLength(LWideStr, Len);
              Stream.ReadBuffer(LWideStr[1], Len * SizeOf(WideChar));
              LSubItemData := nil;
              Stream.ReadBuffer(LSubItemData, SizeOf(Int64));
              SubItems.AddObject(LWideStr, Pointer(LSubItemData));
            end;
          end;
        end;
      end
  else

    Exit;
  end;

  // Read subitem images, if present
  for I := 0 to Count - 1 do
    with Item[I] do
      for J := 0 to SubItems.Count - 1 do
      begin
        Stream.ReadBuffer(LImageIndex, SizeOf(SmallInt));
        SubItemImages[J] := LImageIndex;
      end;
{$ENDIF}
end;

procedure TMyListItems.WriteItemData(Stream: TStream);
{$IFDEF CLR}
{$REGION CLR_LEGACY}
var
  Buffer, LCaption: TBytes;
  I, J, Size, L, LCaptionLen: Integer;
  ItemInfo: TItemInfo2;

  function GetByteLength(const S: string): Integer;
  begin
    Result := ByteLength(S);
    if Result > 255 * SizeOf(Char) then
      Result := 255 * SizeOf(Char); // Max length for an item is 255 chars
  end;

begin
  Size := 0;
  for I := 0 to Count - 1 do
  begin
    L := GetByteLength(Item[I].Caption) + SizeOf(Byte); // Add length byte
    for J := 0 to Item[I].SubItems.Count - 1 do
    begin
      Inc(L, GetByteLength(Item[I].SubItems[J]) + SizeOf(Byte)); // Add length byte
      Inc(L, SizeOf(SmallInt)); // SubItem images
    end;
    Inc(Size, Marshal.SizeOf(TypeOf(TItemInfo)) + L);
  end;

  // Check which platform we're running under and write the
  // appropriate stream version. TItemInfo.Data has a different
  // size when running as a 64-bit application.
  if IntPtr.Size = 4 then
    Stream.WriteBuffer(ListItemStreamVersion2, SizeOf(Byte))
  else
    Stream.WriteBuffer(ListItemStreamVersion2x64, SizeOf(Byte));
  Stream.WriteBuffer(Size, SizeOf(Integer));
  Stream.WriteBuffer(Count, SizeOf(Integer));
  for I := 0 to Count - 1 do
  begin
    with Item[I] do
    begin
      // Write TItemInfo structure
      ItemInfo.ImageIndex := ImageIndex;
      ItemInfo.OverlayIndex := OverlayIndex;
      ItemInfo.StateIndex := StateIndex;
      ItemInfo.Data := Data;
      ItemInfo.GroupID := GroupID;
      ItemInfo.SubItemCount := SubItems.Count;
      Buffer := StructureToBytes(TObject(ItemInfo));
      Stream.WriteBuffer(Buffer, Length(Buffer));

      // Write length of Caption in chars followed by Caption
      LCaption := WideBytesOf(Caption);
      LCaptionLen := Length(Caption);
      if LCaptionLen > 255 then
        LCaptionLen := 255;
      Stream.WriteBuffer(LCaptionLen, SizeOf(Byte));
      Stream.WriteBuffer(LCaption, LCaptionLen * SizeOf(WideChar));

      // Write length of each SubItem in chars followed by the SubItem
      for J := 0 to SubItems.Count - 1 do
      begin
        LCaption := WideBytesOf(SubItems[J]);
        LCaptionLen := Length(SubItems[J]);
        if LCaptionLen > 255 then
          LCaptionLen := 255;
        Stream.WriteBuffer(LCaptionLen, SizeOf(Byte));
        Stream.WriteBuffer(LCaption, LCaptionLen * SizeOf(WideChar));
      end;
    end;
  end;

  // Write SubItem images
  for I := 0 to Count - 1 do
    with Item[I] do
      for J := 0 to SubItems.Count - 1 do
        Stream.WriteBuffer(SmallInt(SubItemImages[J]), SizeOf(SmallInt));
{$ENDREGION CLR_LEGACY}
{$ELSE}
var
  ItemInfo: TItemDataInfo2;
  LCaption: UnicodeString;
  I, J, Size, L: Integer;
  LImageIndex: Smallint;
  LStreamVersion: Byte;
  LItemCount, LCaptionLen: Integer;

  function GetByteLength(const S: string): Integer;
  begin
    Result := ByteLength(S);
    if Result > 255 * SizeOf(Char) then
      Result := 255 * SizeOf(Char); // Max length for an item is 255 chars
  end;

  function GeTMyListItemsFromStream(Stream: TMemoryStream): TBytes;
  const
    ItemsPropName = 'Items.ItemData'; // do not localize
  var
    LReader: TReader;
    LFlags: TFilerFlags;
    LPos, LSize: Integer;
  begin
    SetLength(Result, 0);
    Stream.Position := 0;
    LReader := TReader.Create(Stream, 1024);
    try
      LReader.ReadSignature;
      LReader.ReadPrefix(LFlags, LPos);
      LReader.ReadStr; // Class name
      LReader.ReadStr; // Name property
      LReader.ReadStr; // 1st property name
      while LReader.NextValue <> vaNull do
      begin
        LReader.SkipValue;
        if LReader.ReadStr = ItemsPropName then
        begin
          LReader.CheckValue(vaBinary);
          LReader.Read(LSize, SizeOf(LSize));
          SetLength(Result, LSize);
          LReader.Read(Result[0], LSize);
          Break;
        end;
      end;
    finally
      LReader.Free;
    end;
  end;

var
  LBuffer: TBytes;
  LSubItemData: Pointer;
begin
  if (Owner.FMemStream <> nil) and not Owner.HandleAllocated then
  begin
    // If the handle isn't allocated we need to extract the items
    // from the cached memory stream and write them to Stream
    LBuffer := GeTMyListItemsFromStream(Owner.FMemStream);
    Stream.Write(LBuffer[0], Length(LBuffer));
    Exit;
  end;

  Size := 0;
  for I := 0 to Count - 1 do
  begin
    L := GetByteLength(Item[I].Caption) + SizeOf(Byte); // Add length byte
    for J := 0 to Item[I].SubItems.Count - 1 do
    begin
      Inc(L, GetByteLength(Item[I].SubItems[J]) + SizeOf(Byte)); // Add length byte
      Inc(L, SizeOf(LSubItemData)); // Add size of SubItem's Object
      Inc(L, SizeOf(SmallInt)); // SubItem images
    end;
    Inc(Size, SizeOf(TItemDataInfo2) + L);
  end;

  LItemCount := Count;
{$IFDEF CPUX86}
  LStreamVersion := ListItemStreamVersion3;
{$ELSE}
  LStreamVersion := ListItemStreamVersion3x64;
{$ENDIF}
  Stream.WriteBuffer(LStreamVersion, SizeOf(Byte));
  Stream.WriteBuffer(Size, SizeOf(Integer));
  Stream.WriteBuffer(LItemCount, SizeOf(Integer));
  for I := 0 to Count - 1 do
  begin
    with Item[I] do
    begin
      // Write TItemDataInfo structure
      ItemInfo.ImageIndex := ImageIndex;
      ItemInfo.OverlayIndex := OverlayIndex;
      ItemInfo.StateIndex := StateIndex;
      ItemInfo.Data := Data;
      ItemInfo.GroupID := GroupID;
      ItemInfo.SubItemCount := SubItems.Count;
      LCaption := Caption;
      LCaptionLen := Length(LCaption);
      if LCaptionLen > 255 then
        LCaptionLen := 255;
      ItemInfo.CaptionLen := LCaptionLen;
      Stream.WriteBuffer(ItemInfo, SizeOf(TItemDataInfo2));

      // Write Caption
      Stream.WriteBuffer(LCaption[1], ItemInfo.CaptionLen * SizeOf(WideChar));

      // Write length of each SubItem in chars followed by the SubItem
      for J := 0 to SubItems.Count - 1 do
      begin
        LCaption := SubItems[J];
        LCaptionLen := Length(LCaption);
        if LCaptionLen > 255 then
          LCaptionLen := 255;
        Stream.WriteBuffer(LCaptionLen, SizeOf(Byte));
        Stream.WriteBuffer(LCaption[1], LCaptionLen * SizeOf(WideChar));
        LSubItemData := SubItems.Objects[J];
        Stream.WriteBuffer(LSubItemData, SizeOf(LSubItemData));
      end;
    end;
  end;

  // Write SubItem images
  for I := 0 to Count - 1 do
    with Item[I] do
      for J := 0 to SubItems.Count - 1 do
      begin
        LImageIndex := SubItemImages[J];
        Stream.WriteBuffer(LImageIndex, SizeOf(SmallInt));
      end;
{$ENDIF}
end;

procedure TMyListItems.Delete(Index: Integer);
begin
  Item[Index].Delete;
end;

function TMyListItems.CreateItem(Index: Integer;
  ListItem: TMyListItem): TLVItem;
begin
  with Result do
  begin
    mask := LVIF_PARAM or LVIF_IMAGE or LVIF_GROUPID;
    iItem := Index;
    iSubItem := 0;
    iImage := I_IMAGECALLBACK;
    iGroupId := -1;
{$IFDEF CLR}
    lParam := ListItem.GetHashCode;
{$ELSE}
    lParam := Winapi.Windows.LPARAM(ListItem);
{$ENDIF}
  end;
end;

{ TMyListGroup }

constructor TMyListGroup.Create(Collection: TCollection);
var
  Group: TLVGroup;
begin
  FGroupID := TMyListGroups(Collection).NextGroupID;
  inherited Create(Collection);
  FState := [lgsNormal];
  FHeaderAlign := taLeftJustify;
  FFooterAlign := taLeftJustify;
  FTitleImage := -1;

  with Group do
  begin
    cbSize := SizeOf(Group);
    mask := LVGF_STATE or LVGF_GROUPID;
    iGroupID := FGroupID;
    state := LVGS_NORMAL;
  end;

  if TMyListGroups(Collection).Owner.HandleAllocated then
    ListView_InsertGroup(TMyListGroups(Collection).Owner.Handle, Index, Group);
end;

destructor TMyListGroup.Destroy;
var
  Groups: TMyListGroups;
  Items: TMyListItems;
  I: Integer;
begin
  Groups := TMyListGroups(Collection);
  if Groups.Owner.HandleAllocated then
    ListView_RemoveGroup(Groups.Owner.Handle, GroupID);

  Items := Groups.Owner.Items;
  if not Groups.Owner.OwnerData then
    for I := 0 to Items.Count - 1 do
      if Items[I].GroupID = GroupID then
        Items[I].GroupID := -1;

  inherited Destroy;
  Groups.UpdateGroups;
end;

procedure TMyListGroup.SetHeader(Value: string);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetFooter(Value: string);
begin
  if FFooter <> Value then
  begin
    FFooter := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetGroupID(Value: Integer);
var
  I: Integer;
  GroupItems: array of Integer;
begin
  SetLength(GroupItems, 0);

  if Value <> FGroupID then
  begin
    for I := 0 to TMyListGroups(Collection).Count - 1 do
    begin
      if TMyListGroups(Collection)[I].GroupID = Value then
        Exit;
    end;

    for I := 0 to TMyListGroups(Collection).Owner.Items.Count - 1 do
    begin
      if TMyListGroups(Collection).Owner.Items[I].GroupID = FGroupID then
      begin
        SetLength(GroupItems, Length(GroupItems) + 1);
        GroupItems[Length(GroupItems) - 1] := I;
      end;
    end;

    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      ListView_RemoveGroup(TMyListGroups(Collection).Owner.Handle, FGroupID);
    FGroupID := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;

    for I := 0 to Length(GroupItems) - 1 do
    begin
      TMyListGroups(Collection).Owner.Items[GroupItems[I]].GroupID := FGroupID;
    end;
  end;
end;

function TMyListGroup.GetState: TListGroupstateSet;
var
  State: Cardinal;
begin
  Result := FState;
  if TMyListGroups(Collection).Owner.HandleAllocated then
  begin
    State := ListView_GetGroupState(TMyListGroups(Collection).Owner.Handle, GroupID, $FFFFFFFF);
    Result := Result - [lgsHidden, lgsCollapsed, lgsFocused, lgsSelected, lgsSubseted, lgsSubSetLinkFocused];

    if State and LVGS_HIDDEN <> 0 then
      Include(Result, lgsHidden);
    if State and LVGS_COLLAPSED <> 0 then
      Include(Result, lgsCollapsed);
    if State and LVGS_FOCUSED <> 0 then
      Include(Result, lgsFocused);
    if State and LVGS_SELECTED <> 0 then
      Include(Result, lgsSelected);
    if State and LVGS_SUBSETED <> 0 then
      Include(Result, lgsSubseted);
    if State and LVGS_SUBSETLINKFOCUSED <> 0 then
      Include(Result, lgsSubSetLinkFocused);

    FState := Result;
  end;
end;

procedure TMyListGroup.SetState(Value: TListGroupstateSet);
begin
  if FState <> Value then
  begin
    FState := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetHeaderAlign(Value: TAlignment);
begin
  if FHeaderAlign <> Value then
  begin
    FHeaderAlign := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetFooterAlign(Value: TAlignment);
begin
  if FFooterAlign <> Value then
  begin
    FFooterAlign := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetSubtitle(Value: string);
begin
  if FSubtitle <> Value then
  begin
    FSubtitle := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.SetTitleImage(Value: TImageIndex);
begin
  if FTitleImage <> Value then
  begin
    FTitleImage := Value;
    if not (csLoading in TMyListGroups(Collection).Owner.ComponentState) then
      TMyListGroups(Collection).Owner.UpdateGroups;
  end;
end;

procedure TMyListGroup.ReadDescriptionTop(Reader: TReader);
begin
  FDescriptionTop := Reader.ReadString;
end;

procedure TMyListGroup.ReadDescriptionBottom(Reader: TReader);
begin
  FDescriptionBottom := Reader.ReadString;
end;

procedure TMyListGroup.IgnoreInt(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TMyListGroup.IgnoreString(Reader: TReader);
begin
  Reader.ReadString;
end;

procedure TMyListGroup.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('ExtendedImage', IgnoreInt, nil, False);
  Filer.DefineProperty('SubsetTitle', IgnoreString, nil, False);

  Filer.DefineProperty('TopDescription', ReadDescriptionTop, nil, False);
  Filer.DefineProperty('BottomDescription', ReadDescriptionBottom, nil, False);
end;

function TMyListGroup.GetDisplayName: string;
begin
  Result := Header;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TMyListGroup.SetIndex(Value: Integer);
begin
  inherited;
end;

procedure TMyListGroup.Assign(Source: TPersistent);
var
  Group: TMyListGroup;
begin
  if Source is TMyListGroup then
  begin
    Group := TMyListGroup(Source);

    Header := Group.Header;
    Footer := Group.Footer;
    State := Group.State;
    HeaderAlign := Group.HeaderAlign;
    FooterAlign := Group.FooterAlign;
    Subtitle := Group.Subtitle;
    {TopDescription := Group.DescriptionTop;
    BottomDesription := Group.DescriptionBottom;
    TitleImage := Group.TitleImage;
    ExtendedImage := Group.ExtendedImage;
    SubsetTitle := Group.SubsetTitle;}
  end
  else
    inherited Assign(Source);
end;

{ TMyListGroups }

function TMyListGroups.GetItem(Index: Integer): TMyListGroup;
begin
  Result := TMyListGroup(inherited GetItem(Index));
end;

procedure TMyListGroups.SetItem(Index: Integer; Value: TMyListGroup);
begin
  inherited SetItem(Index, Value);
end;

function TMyListGroups.GetNextGroupID: Integer;
var
  I: Integer;
begin
  Result := 0;

  if Count = 0 then
    Exit;

  while True do
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].GroupID = Result then
      begin
        Inc(Result);
        break;
      end
      else if I = Count - 1 then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TMyListGroups.UpdateGroups;
begin
  if not Owner.HandleAllocated then
    Exit;

  BeginUpdate;
  try
    Owner.UpdateGroups;
  finally
    EndUpdate;
  end;
end;

function TMyListGroups.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMyListGroups.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Owner.UpdateGroup(Item.Index)
  else
    Owner.UpdateGroups;
end;

constructor TMyListGroups.Create(AOwner: TMyCustomListView);
begin
  inherited Create(TMyListGroup);
  FOwner := AOwner;
end;

function TMyListGroups.Add: TMyListGroup;
begin
  Result := TMyListGroup(inherited Add);
  UpdateGroups;
end;

function TMyListGroups.Owner: TMyCustomListView;
begin
  Result := FOwner;
end;


{ TMyListColumn }

constructor TMyListColumn.Create(Collection: TCollection);
var
  Column: TLVColumn;
begin
  FOrderTag := Collection.Count;
  inherited Create(Collection);
  FWidth := 50;
  FAlignment := taLeftJustify;
  FImageIndex := -1;
  with Column do
  begin
    mask := LVCF_FMT or LVCF_WIDTH;
    fmt := LVCFMT_LEFT;
    cx := FWidth;
    iImage := FImageIndex;
  end;
  if TMyListColumns(Collection).Owner.HandleAllocated then
    ListView_InsertColumn(TMyListColumns(Collection).Owner.Handle, Index, Column);

  FArrowOnRight := True;
end;

destructor TMyListColumn.Destroy;
var
  Columns: TMyListColumns;
begin
  Columns := TMyListColumns(Collection);
  if TMyListColumns(Collection).Owner.HandleAllocated then
    ListView_DeleteColumn(TMyListColumns(Collection).Owner.Handle, Index);
  inherited Destroy;
  Columns.UpdateCols;
end;

procedure TMyListColumn.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('WidthType', ReadData, WriteData,
    WidthType <= ColumnTextWidth);
end;

procedure TMyListColumn.ReadData(Reader: TReader);
begin
  with Reader do
  begin
    ReadListBegin;
    Width := TWidth(ReadInteger);
    ReadListEnd;
  end;
end;

procedure TMyListColumn.WriteData(Writer: TWriter);
begin
  with Writer do
  begin
    WriteListBegin;
    WriteInteger(Ord(WidthType));
    WriteListEnd;
  end;
end;

procedure TMyListColumn.DoChange;

  procedure WriteCols;
  var
    Writer: TWriter;
    LV: TMyCustomListView;
  begin
    LV := TMyListColumns(Collection).Owner;
    if LV.HandleAllocated or ([csLoading, csReading] * LV.ComponentState <> []) or
      LV.FReading then Exit;
    if LV.FColStream = nil then LV.FColStream := TMemoryStream.Create
    else LV.FColStream.Size := 0;
    Writer := TWriter.Create(LV.FColStream, 1024);
    try
      Writer.WriteCollection(Collection);
    finally
      Writer.Free;
      LV.FColStream.Position := 0;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to Collection.Count - 1 do
    if TMyListColumn(Collection.Items[I]).WidthType <= ColumnTextWidth then Break;
  Changed(I <> Collection.Count);
  WriteCols;
end;

procedure TMyListColumn.SetIndex(Value: Integer);
var
  ColumnOrder: array of Integer;
  I: Integer;
begin
  inherited SetIndex(Value);
  SetLength(ColumnOrder, Collection.Count);
  for I := 0 to Collection.Count - 1 do
    ColumnOrder[I] := TMyListColumn(Collection.Items[I]).FOrderTag;
{$IFDEF CLR}
  ListView_SetColumnOrderArray(TMyListColumns(Collection).Owner.Handle,
    Collection.Count, ColumnOrder);
{$ELSE}
  ListView_SetColumnOrderArray(TMyListColumns(Collection).Owner.Handle,
    Collection.Count, PInteger(ColumnOrder));
{$ENDIF}
end;

procedure TMyListColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    DoChange;
  end;
end;

procedure TMyListColumn.SetCollection(Value: TCollection);
begin
  inherited;
  if (Value <> nil) then
  begin
    FListView := TMyListColumns(Value).Owner;
//    FImgList := FListView.HeaderImages;
  end;
end;

function TMyListColumn.GetWidth: TWidth;
var
  IsStreaming: Boolean;
  LOwner: TMyCustomListView;
begin
  LOwner := TMyListColumns(Collection).Owner;
  IsStreaming := [csReading, csWriting, csLoading] * LOwner.ComponentState <> [];

  if ((FWidth = 0) and (LOwner.HandleAllocated or not IsStreaming)) or
     ((not AutoSize) and LOwner.HandleAllocated and (LOwner.ViewStyle = vsReport) and
     (FWidth <> LVSCW_AUTOSIZE) and (LOwner.ValidHeaderHandle)) then
    FWidth := ListView_GetColumnWidth(LOwner.Handle, FOrderTag);
  Result := FWidth;
end;

function TMyListColumn.IsWidthStored: Boolean;
begin
  Result := not FAutoSize;
end;

procedure TMyListColumn.SetWidth(Value: TWidth);
begin
  if FWidth <> Value then
  begin
    if ((Value < MinWidth) and (Value >= 0)) then Value := MinWidth
    else if ((MaxWidth > 0) and (Value > MaxWidth)) then Value := MaxWidth;
    FWidth := Value;
    DoChange;
  end;
end;

procedure TMyListColumn.UpdateHeaderSortState;
const
  sortMap: array[TMySortOrder] of Integer = (0, HDF_SORTDOWN, HDF_SORTUP);
  sortImgIdx: array[TMySortOrder] of Integer = (-1, 0, 1);
var
  item: THDItem;
begin
  if (FListView.ValidHeaderHandle) and (Index < Header_GetItemCount(FListView.FHeaderHandle)) then
  begin
    if StyleServices.Enabled then
    begin
      // При включенных темах будем рисовать вот так:
      Header_SetImageList(FListView.FHeaderHandle, 0);
      item.Mask := HDI_FORMAT;
      Header_GetItem(FListView.FHeaderHandle, Index, item);
      item.fmt :=
        (item.fmt and not HDF_SORTUP and not HDF_SORTDOWN) or
        sortMap[FSortOrder] and not HDF_IMAGE and not HDF_BITMAP;
      Header_SetItem(FListView.FHeaderHandle, Index, item);
    end
    else if (FListView.HeaderImages <> nil) then
    begin
      // При выключенных темах, рисуем картинками из imagelist:
      Header_SetImageList(FListView.FHeaderHandle, FListView.HeaderImages.Handle);
      item.Mask := HDI_FORMAT or HDI_IMAGE;
      Header_GetItem(FListView.FHeaderHandle, Index, item);
      if (FSortOrder <> soNone) then
      begin
        if (ArrowOnRight) then
          item.fmt := item.fmt or HDF_BITMAP_ON_RIGHT
        else
          item.fmt := item.fmt and not HDF_BITMAP_ON_RIGHT;
        item.fmt := item.fmt or HDF_IMAGE;
        item.iImage := sortImgIdx[FSortOrder];
        Header_SetItem(FListView.FHeaderHandle, Index, item);
      end
      else
      begin
        item.fmt := item.fmt and not HDF_IMAGE;
        item.Mask := item.Mask and not HDI_IMAGE;
        item.iImage := 0;
        Header_SetItem(FListView.FHeaderHandle, Index, item);
      end;
    end;
  end;
end;

procedure TMyListColumn.UpdateItemsSort;
begin
  FListView.ResortItems(nil);
end;

procedure TMyListColumn.SetAlignment(Value: TAlignment);
begin
  if (Alignment <> Value) and (Index <> 0) then
  begin
    FAlignment := Value;
    Changed(False);
    TMyListColumns(Collection).Owner.Repaint;
  end;
end;

procedure TMyListColumn.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if TMyListColumns(Collection).Owner <> nil then
      TMyListColumns(Collection).Owner.AdjustSize;
    DoChange;
  end;
end;

procedure TMyListColumn.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

procedure TMyListColumn.SetMaxWidth(Value: TWidth);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    Changed(False);
  end;
end;

procedure TMyListColumn.SetMinWidth(Value: TWidth);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    Changed(False);
  end;
end;

procedure TMyListColumn.SetSortOrder(const Value: TMySortOrder);
var
  i: Integer;
begin
  if (Value <> FSortOrder) then
  begin
    FSortOrder := Value;
    for i := 0 to Collection.Count - 1 do
    begin
      if (i <> Index) then
      begin
        TMyListColumn(Collection.Items[i]).FSortOrder := soNone;
      end;
      TMyListColumn(Collection.Items[i]).UpdateHeaderSortState;
    end;
    UpdateItemsSort;
  end;
end;

procedure TMyListColumn.Assign(Source: TPersistent);
var
  Column: TMyListColumn;
begin
  if Source is TMyListColumn then
  begin
    Column := TMyListColumn(Source);
    Alignment := Column.Alignment;
    AutoSize := Column.AutoSize;
    Caption := Column.Caption;
    ImageIndex := Column.ImageIndex;
    MaxWidth := Column.MaxWidth;
    MinWidth := Column.MinWidth;
    Width := Column.Width;
  end
  else inherited Assign(Source);
end;

function TMyListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

{ TMyListColumns }

constructor TMyListColumns.Create(AOwner: TMyCustomListView);
begin
  inherited Create(TMyListColumn);
  FOwner := AOwner;
end;

function TMyListColumns.GetItem(Index: Integer): TMyListColumn;
begin
  Result := TMyListColumn(inherited GetItem(Index));
end;

procedure TMyListColumns.SetItem(Index: Integer; Value: TMyListColumn);
begin
  inherited SetItem(Index, Value);
end;

function TMyListColumns.Add: TMyListColumn;
begin
  Result := TMyListColumn(inherited Add);
//  Result.FImgList := Owner.HeaderImages;
  Result.FListView := Owner;
  UpdateCols;
end;

function TMyListColumns.Owner: TMyCustomListView;
begin
  Result := FOwner;
end;

function TMyListColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMyListColumns.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Owner.UpdateColumn(Item.Index) else
    Owner.UpdateColumns;
end;

procedure TMyListColumns.UpdateCols;
var
  I: Integer;
  LVColumn: TLVColumn;
  ColumnOrder: TIntegerDynArray;
begin
  if not Owner.HandleAllocated then Exit;
  BeginUpdate;
  try
    SetLength(ColumnOrder, Count);
    for I := Count - 1 downto 0 do
    begin
      ColumnOrder[I] := Items[I].FOrderTag;
      ListView_DeleteColumn(Owner.Handle, I);
    end;

    for I := 0 to Count - 1 do
    begin
      with LVColumn do
      begin
        mask := LVCF_FMT or LVCF_WIDTH;
        fmt := LVCFMT_LEFT;
        cx := Items[I].FWidth;
      end;
      ListView_InsertColumn(Owner.Handle, I, LVColumn);
    end;
    ListView_SetColumnOrderArray(Owner.Handle, Count, PInteger(ColumnOrder));
    Owner.UpdateColumns;
  finally
    EndUpdate;
  end;
end;



{ TMyIconOptions }

constructor TMyIconOptions.Create(AOwner: TMyCustomListView);
begin
  inherited Create;
  if AOwner = nil then
    raise Exception.Create(sInvalidOwner);
  FListView := AOwner;
  Arrangement := iaTop;
  AutoArrange := False;
  WrapText := True;
end;

procedure TMyIconOptions.SetArrangement(Value: TIconArrangement);
begin
  if Value <> Arrangement then
  begin;
    FArrangement := Value;
    FListView.RecreateWnd;
  end;
end;

procedure TMyIconOptions.SetAutoArrange(Value: Boolean);
begin
  if Value <> AutoArrange then
  begin
    FAutoArrange := Value;
    FListView.RecreateWnd;
  end;
end;

procedure TMyIconOptions.SetWrapText(Value: Boolean);
begin
  if Value <> WrapText then
  begin
    FWrapText := Value;
    FListView.RecreateWnd;
  end;
end;

{$IFDEF CLR}
procedure TLVInstances.Finalize;
begin
  if Assigned(FEditInstance) then
  begin
    FreeObjectInstance(FEditInstance);
    FEditInstance := nil;
  end;
  if Assigned(FHeaderInstance) then
  begin
    FreeObjectInstance(FHeaderInstance);
    FHeaderInstance := nil;
  end;
  inherited;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('My', [TMyListView]);
end;

end.
