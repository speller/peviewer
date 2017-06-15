unit PEViewerFrameUnit;

interface
uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, CommonUtilities, PEUtils, PETypes, ImgList, ComCtrls, Winapi.CommCtrl, Themes,
  Vcl.ActnList, Vcl.Menus, Clipbrd, Strutils, MyListViewUnit;


const
  WM_MYUPDATELISTVIEWHEADERS = WM_USER + 1100;


type

  TPEViewerFrameClass = class of TPEViewerFrame;
  TPEViewerFrame = class(TFrame)
    ilListView: TImageList;
    alActions: TActionList;
    pmListView: TPopupMenu;
    aListViewSelectAll: TAction;
    aListViewCopy: TAction;
    miListViewCopy: TMenuItem;
    miListViewSelectAll: TMenuItem;
    aListViewCopyValue: TAction;
    miListViewCopyValue: TMenuItem;
    procedure aListViewSelectAllExecute(Sender: TObject);
    procedure aListViewCopyExecute(Sender: TObject);
    procedure pmListViewPopup(Sender: TObject);
    procedure aListViewCopyValueExecute(Sender: TObject);
  private
    class var
      FBasePropCount: Integer;
  private
    FActiveControl: TWinControl;
    FPlugin: TPersistent;
    FFileName: string;
    FStringsPrepared: Boolean;
    FMainForm: TForm;
    FPageErrorMsg: string;
    procedure WMMyUpdateListViewHeaders(var AMsg: TMessage); message WM_MYUPDATELISTVIEWHEADERS;
  protected
    FRequiredProperties: TStringArray;

    property StringsPrepared: Boolean read FStringsPrepared;

    procedure GetPropertiesList(AList: TStringList); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PrepareListViews;
    procedure CreateWnd; override;
    function CheckListViewAction(ASender: TObject): Boolean;
    function GetActionListView(AAction: TAction): TMyListView;
    procedure PrepareStrings; virtual;
    procedure ReadProperties(AList: TStringList);
    procedure LoadData(const AFileName: string); virtual;
    procedure LoadConfig; virtual;
    function GetCopyText(
      AAction: TBasicAction; AListView: TMyListView; AValueOnly: Boolean): string; virtual;
    procedure ListViewCompare(
      ASender: TObject; AItem1, AItem2: TMyListItem; AData: Integer;
      var ACompare: Integer);
    function CustomListItemsCompare(
      var AProcessed: Boolean; AItem1, AItem2: TMyListItem;
      AColIdx: Integer): Integer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActiveControl: TWinControl read FActiveControl write FActiveControl;
    property PluginObject: TPersistent read FPlugin write FPlugin;
    property FileName: string read FFileName;
    property MainForm: TForm read FMainForm;
    property PageErrorMsg: string read FPageErrorMsg write FPageErrorMsg;

    procedure DoLoadData(const AFileName: string);
    procedure Reload;
    procedure DoLoadConfig;
    procedure SaveConfig;
    procedure ApplyLocalization; virtual;
    procedure DoOnShow; virtual;
    function FindFirstFocusableControl: TWinControl;
  end;




implementation
uses
  PEViewerClass;


{$R *.dfm}


{ TPEViewerFrame }


procedure TPEViewerFrame.aListViewCopyExecute(Sender: TObject);
begin
  if (CheckListViewAction(Sender)) then
  begin
    Clipboard.AsText :=
      GetCopyText(
        TBasicAction(Sender), GetActionListView(TAction(Sender)), False);
  end;
end;

procedure TPEViewerFrame.aListViewCopyValueExecute(Sender: TObject);
begin
  if (CheckListViewAction(Sender)) then
  begin
    Clipboard.AsText :=
      GetCopyText(
        TBasicAction(Sender), GetActionListView(TAction(Sender)), True);
  end;
end;

procedure TPEViewerFrame.aListViewSelectAllExecute(Sender: TObject);
var
  lv: TMyListView;
  i: Integer;
  item: TMyListItem;
begin
  if (CheckListViewAction(Sender)) then
  begin
    lv := GetActionListView(TAction(Sender));
    lv.Items.BeginUpdate;
    try
      for i := 0 to lv.Items.Count - 1 do
      begin
        item := lv.Items[i];
        item.Selected := True;
      end;
    finally
      lv.Items.EndUpdate;
    end;
  end;
end;

procedure TPEViewerFrame.ApplyLocalization;
var
  plugin: TPEViewer;
begin
  HandleNeeded;
  try
    plugin := TPEViewer(PluginObject);
    if (plugin <> nil) then
      ReadProperties(plugin.PublicLocalization);
  except
    on E: Exception do
      TPEViewer(PluginObject).MainForm.SetErrorMsg(E.Message);
  end;
end;

function TPEViewerFrame.CheckListViewAction(ASender: TObject): Boolean;
var
  mi: TMenuItem;
begin
  Result := (ASender is TAction) and (TAction(ASender).ActionComponent is TMyListView);
  if (not Result) and (ASender is TAction) and (TAction(ASender).ActionComponent is TMenuItem) then
  begin
    mi := TMenuItem(TAction(ASender).ActionComponent);
    while (mi.Parent <> nil) do
      mi := mi.Parent;
    Result :=
      (mi.Owner <> nil) and (mi.Owner is TPopupMenu) and
      (TPopupMenu(mi.Owner).PopupComponent is TMyListView);
  end;
  if (not Result) then
    Result := ActiveControl is TMyListView;
  if (not Result) and (MainForm <> nil) then
    Result := MainForm.ActiveControl is TMyListView;
end;

constructor TPEViewerFrame.Create(AOwner: TComponent);
var
  p: TComponent;
begin
  inherited;
  // Calculate base class published properties count
  if (FBasePropCount = 0) then
  begin
    FBasePropCount := GetTypeData(PTypeInfo(TPEViewerFrame.ClassInfo))^.PropCount;
  end;
  Visible := False;
  p := Owner;
  while (not (p is TForm)) and (p <> nil) do
    p := p.Owner;
  if (p <> nil) then
    FMainForm := TForm(p);
end;

procedure TPEViewerFrame.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Hack for successful manual frame creation (Parent property not assigned yet and this may cause exception)
  if (Owner <> nil) and (Owner is TWinControl) then
    Params.WndParent := TWinControl(Owner).Handle;
end;

procedure TPEViewerFrame.CreateWnd;
begin
  inherited;
  if (not (csReading in ComponentState)) then
    PrepareListViews;
end;

function TPEViewerFrame.CustomListItemsCompare(
  var AProcessed: Boolean; AItem1, AItem2: TMyListItem; AColIdx: Integer): Integer;
begin
  Result := 0;
end;

destructor TPEViewerFrame.Destroy;
begin
  SetLength(FRequiredProperties, 0);
  inherited;
end;

procedure TPEViewerFrame.DoLoadConfig;
var
  plugin: TPEViewer;
begin
  HandleNeeded;
  try
    plugin := TPEViewer(PluginObject);
    if (plugin <> nil) then
    begin
      aListViewCopy.Caption := plugin.Strings.Copy_Line;
      aListViewCopyValue.Caption := plugin.Strings.Copy_Value;
      aListViewSelectAll.Caption := plugin.Strings.Select_All;
      ReadProperties(plugin.PublicConfig);
    end;
    LoadConfig;
  except
  end;
end;

procedure TPEViewerFrame.DoLoadData(const AFileName: string);
begin
  FFileName := AFileName;
  try
    if (not StringsPrepared) then
      PrepareStrings;
    if (TPEViewer(PluginObject).PEImage <> nil) then
      LoadData(AFileName);
    // Update list views headers
    PostMessage(Handle, WM_MYUPDATELISTVIEWHEADERS, 0, 0);
  except
    on E: Exception do
      TPEViewer(PluginObject).MainForm.SetErrorMsg(E.Message);
  end;
end;

procedure TPEViewerFrame.DoOnShow;
begin
end;

function TPEViewerFrame.FindFirstFocusableControl: TWinControl;
begin
  Result := FindNextControl(nil, True, True, True);
end;

function TPEViewerFrame.GetActionListView(AAction: TAction): TMyListView;
var
  mi: TMenuItem;
begin
  Result := nil;
  if (AAction.ActionComponent is TMyListView) then
    Result := TMyListView(AAction.ActionComponent)
  else if (AAction.ActionComponent is TMenuItem) then
  begin
    mi := TMenuItem(AAction.ActionComponent);
    while (mi.Parent <> nil) do
      mi := mi.Parent;
    if
      (mi.Owner <> nil) and (mi.Owner is TPopupMenu) and
      (TPopupMenu(mi.Owner).PopupComponent is TMyListView)
    then
      Result := TMyListView(TPopupMenu(mi.Owner).PopupComponent);
  end;
  if (Result = nil) and (ActiveControl is TMyListView) then
    Result := TMyListView(ActiveControl);
  if (Result = nil) and (MainForm <> nil) and (MainForm.ActiveControl is TMyListView) then
    Result := TMyListView(MainForm.ActiveControl);
end;

function TPEViewerFrame.GetCopyText(
  AAction: TBasicAction; AListView: TMyListView; AValueOnly: Boolean): string;
var
  s, s1, s2: string;
  i: Integer;
  item: TMyListItem;
begin
  s := '';
  if (AListView.SelCount > 0) then
  begin
    for i := 0 to AListView.Items.Count - 1 do
    begin
      item := AListView.Items[i];
      if (item.Selected) then
      begin
        if (AValueOnly) then
        begin
          s1 := Trim(item.SubItems.DelimitedText);
          s := s + s1 + #13#10;
        end
        else
        begin
          s1 := item.Caption;
          s2 := Trim(item.SubItems.DelimitedText);
          if (s1 <> '') and (s2 <> '') then
            s := s + s1 + ': ' + s2;
          s := s + #13#10;
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to AListView.Items.Count - 1 do
    begin
      item := AListView.Items[i];
      if (AValueOnly) then
      begin
        s1 := Trim(item.SubItems.DelimitedText);
        s := s + s1 + #13#10;
      end
      else
      begin
        s1 := item.Caption;
        s2 := Trim(item.SubItems.DelimitedText);
        if (s1 <> '') and (s2 <> '') then
          s := s + s1 + ': ' + s2;
        s := s + #13#10;
      end;
    end;
  end;
  Result := s;
end;

procedure TPEViewerFrame.GetPropertiesList(AList: TStringList);
var
  pl: PPropList;
  i, count: Integer;
  pi: PPropInfo;
begin
  // Obtain this class published props only
  count := GetPropList(Self, pl);
  try
    for i := FBasePropCount to count - 1 do
    begin
      pi := pl^[i];
      if (pi.PropType^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkString, tkUString]) then
        AList.Add(string(pi .Name));
    end;
  finally
    FreeMem(pl);
  end;
end;

procedure TPEViewerFrame.ListViewCompare(
  ASender: TObject; AItem1, AItem2: TMyListItem; AData: Integer;
  var ACompare: Integer);
const
  sortMap: array[TMySortOrder] of Integer = (1, 1, -1);
var
  colIdx: Integer;
  order: TMySortOrder;
//  col: TMyListColumn;
  s1, s2: string;
  i, n1, n2: Integer;
  lv: TMyListView;
  customCompared: Boolean;
begin
  lv := TMyListView(AItem1.ListView);
  colIdx := 0;
  order := soNone;
  for i := 0 to lv.Columns.Count - 1 do
  begin
    if (lv.Columns[i].SortOrder <> soNone) then
    begin
      order := lv.Columns[i].SortOrder;
      colIdx := i;
      Break;
    end;
  end;
  if (order <> soNone) then
  begin
    customCompared := False;
    ACompare :=
      CustomListItemsCompare(customCompared, AItem1, AItem2, colIdx) * sortMap[order];

    if (not customCompared) then
    begin
      if (colIdx = 0) then
      begin
        s1 := AItem1.Caption;
        s2 := AItem2.Caption;
      end
      else
      begin
        s1 := AItem1.SubItems[colIdx - 1];
        s2 := AItem2.SubItems[colIdx - 1];
      end;
      if (s1 = '') then
        ACompare := 1
      else if (s2 = '') then
        ACompare := -1
      else
        ACompare := AnsiCompareText(s1, s2) * sortMap[order];
    end;
  end
  else
  begin
    if (AItem1.Data <> nil) and (TObject(AItem1.Data) is TBasePEItem) then
    begin
      if
        (TBasePEItem(AItem1.Data).Collection.Owner <> nil) and
        (TBasePEItem(AItem1.Data).Collection.Owner is TBasePEItem)
      then
      begin
        n1 := (TBasePEItem(AItem1.Data).Collection.Owner as TBasePEItem).Index;
        n2 := (TBasePEItem(AItem2.Data).Collection.Owner as TBasePEItem).Index;
        if (n1 > n2) then
          ACompare := 1
        else if (n1 < n2) then
          ACompare := -1
        else
          ACompare := 0;
        ACompare := ACompare * sortMap[order];
      end
      else
      begin
        ACompare := 0;
      end;

      if (ACompare = 0) then
      begin
        n1 := TBasePEItem(AItem1.Data).Index;
        n2 := TBasePEItem(AItem2.Data).Index;
        if (n1 > n2) then
          ACompare := 1
        else if (n1 < n2) then
          ACompare := -1
        else
          ACompare := 0;
        ACompare := ACompare * sortMap[order];
      end;
//      AItem1.Caption := TBasePEItem(AItem1.Data).Name + ' ' + IntToStr(TBasePEItem(AItem1.Data).Index);
//      AItem2.Caption := TBasePEItem(AItem2.Data).Name + ' ' + IntToStr(TBasePEItem(AItem2.Data).Index);
    end
    else
      ACompare := 0;
  end;
end;

procedure TPEViewerFrame.LoadConfig;
begin
end;

procedure TPEViewerFrame.LoadData(const AFileName: string);
begin
end;

procedure TPEViewerFrame.pmListViewPopup(Sender: TObject);
var
  lvActnEnabled: Boolean;
begin
  lvActnEnabled := TPopupMenu(Sender).PopupComponent is TMyListView;
  aListViewSelectAll.Enabled := lvActnEnabled;
  aListViewCopy.Enabled := lvActnEnabled;
end;

procedure TPEViewerFrame.PrepareListViews;
var
  i: Integer;
  lv: TMyListView;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if (Components[i] is TMyListView) then
    begin
      lv := TMyListView(Components[i]);
      lv.PopupMenu := pmListView;
      lv.OnCompare := ListViewCompare;
      lv.HeaderImages := ilListView;
    end;
  end;
end;

procedure TPEViewerFrame.PrepareStrings;
begin
end;

procedure TPEViewerFrame.ReadProperties(AList: TStringList);
var
  i: Integer;
  propName, propValue, prefix: string;
  pi: PPropInfo;
begin
  prefix := Name + '.';
  for i := 0 to AList.Count - 1 do
  begin
    propName := Trim(AList.Names[i]);
    if (AnsiSameText(prefix, Copy(propName, 1, Length(prefix)))) then
    begin
      propName := Copy(propName, Length(prefix) + 1, MaxInt);
      propValue := Trim(AList.ValueFromIndex[i]);
      if (not RecurseSetProperty(Self, propName, propValue)) then
      begin
        pi := GetPropInfo(Self, propName);
        if
          (pi <> nil) and (pi.SetProc <> nil) and
          (pi.PropType^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkString, tkUString])
        then
          SetOrdProp(Self, pi, pi.Default);
      end;
    end;
  end;
end;

procedure TPEViewerFrame.Reload;
begin
  LoadData(FileName);
end;

procedure TPEViewerFrame.SaveConfig;
var
  plugin: TPEViewer;
  i: Integer;
  propName, propValue, prefix: string;
  properties: TStringList;
begin
  plugin := TPEViewer(PluginObject);
  if (plugin <> nil) then
  begin
    properties := TStringList.Create;
    try
      prefix := Name + '.';
      GetPropertiesList(properties);
      for i := 0 to properties.Count - 1 do
      begin
        propName := properties[i];
        propValue := RecurseGetProperty(Self, propName);
        plugin.PublicConfig.Values[prefix + propName] := propValue;
      end;
    finally
      properties.Free;
    end;
  end;
end;

procedure TPEViewerFrame.WMMyUpdateListViewHeaders(var AMsg: TMessage);
var
  i: Integer;
  lv: TMyListView;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if (Components[i] is TMyListView) then
    begin
      lv := TMyListView(Components[i]);
      lv.UpdateHeaderState;
    end;
  end;
end;

end.


