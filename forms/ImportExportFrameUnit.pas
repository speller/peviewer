unit ImportExportFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PEViewerFrameUnit, ExtCtrls, ComCtrls, StdCtrls, CommonUtilities, ImgList, Vcl.Menus,
  Vcl.ActnList, MyListViewUnit;

const
  WM_MYSELECTION = WM_USER + 999;
  WM_MYVALIDATE = WM_USER + 1000;

type


  TImportExportFrame = class(TPEViewerFrame)
    lvExportFunctions: TMyListView;
    Splitter1: TSplitter;
    pImport: TPanel;
    pImportModules: TPanel;
    lvImportModules: TMyListView;
    Splitter2: TSplitter;
    pImportModulesTitle: TPanel;
    lblImportModulesCount: TLabel;
    pImportFunctions: TPanel;
    lblImportFunctionsCount: TLabel;
    lvImportFunctions: TMyListView;
    pExport: TPanel;
    lblExportFunctionsCount: TLabel;
    aUndecorateCPPNames: TAction;
    N1: TMenuItem;
    miUndecorateCPPNames: TMenuItem;
    ilValidity: TImageList;
    tValidationCheckDelay: TTimer;
    procedure lvImportModulesSelectItem(Sender: TObject; Item: TMyListItem; Selected: Boolean);
    procedure lblImportFunctionsCountClick(Sender: TObject);
    procedure aUndecorateCPPNamesExecute(Sender: TObject);
    procedure lvImportModulesCustomDrawItem(Sender: TMyCustomListView; Item: TMyListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvImportFunctionsCustomDrawItem(Sender: TMyCustomListView; Item: TMyListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure pImportModulesTitleResize(Sender: TObject);
    procedure tValidationCheckDelayTimer(Sender: TObject);
  private
    FLoaded: Boolean;
    FUndecorateCPPNames: Boolean;
    function GetSplitter1Pos: Integer;
    procedure SetSplitter1Pos(Value: Integer);
    function GetSplitter2Pos: Integer;
    procedure SetSplitter2Pos(Value: Integer);
    procedure SetUndecorateCPPNames(Value: Boolean);
  protected
    FLastSelectedModules: array of Boolean;
    SExportFunctionsCount: string;
    SImportModulesCount: string;
    SImportFunctionsCount: string;


    procedure PrepareStrings; override;
    procedure GetPropertiesList(AList: TStringList); override;
    procedure UpdateImportFunctionsList;
    procedure WMMySelection(var AMsg: TMessage); message WM_MYSELECTION;
    procedure WMMyValidate(var AMsg: TMessage); message WM_MYVALIDATE;
    function GetUndecoratedText(const AText: string): string;
    procedure RemoveMessageFromQueue(AMsg: Integer);
    procedure ValidateDependencies(AModuleItem: TMyListItem);
    function CustomListItemsCompare(
      var AProcessed: Boolean; AItem1, AItem2: TMyListItem;
      AColIdx: Integer): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadData(const AFileName: string); override;
  published
    property Splitter1Pos: Integer read GetSplitter1Pos write SetSplitter1Pos;
    property Splitter2Pos: Integer read GetSplitter2Pos write SetSplitter2Pos;
    property UndecorateCPPNames: Boolean read FUndecorateCPPNames write SetUndecorateCPPNames default True;
  end;



implementation
uses
  PEViewerClass, PEUtils;


{$R *.dfm}


{ TImportExportFrame }


procedure TImportExportFrame.aUndecorateCPPNamesExecute(Sender: TObject);
begin
  UndecorateCPPNames := not UndecorateCPPNames;
end;

constructor TImportExportFrame.Create(AOwner: TComponent);
begin
  inherited;
  FUndecorateCPPNames := True;
end;

function TImportExportFrame.CustomListItemsCompare(
  var AProcessed: Boolean; AItem1, AItem2: TMyListItem; AColIdx: Integer): Integer;
var
  v1, v2: SysInt;
  exp1, exp2: TExportItem;
  imp1, imp2: TImportItem;
begin
  v1 := 0;
  v2 := 0;
  if (AItem1.ListView = lvExportFunctions) then
  begin
    if (AColIdx >= 1) or (AColIdx <= 3) then
    begin
      AProcessed := True;
      exp1 := AItem1.Data;
      exp2 := AItem2.Data;
      case AColIdx of
        1:
          begin
            v1 := exp1.Ordinal;
            v2 := exp2.Ordinal;
          end;

        2:
          begin
            v1 := exp1.Hint;
            v2 := exp2.Hint;
          end;

        3:
          begin
            v1 := exp1.Address;
            v2 := exp2.Address;
          end;

        4:
          begin
            v1 := exp1.RawOffset;
            v2 := exp2.RawOffset;
          end;
      end;
    end;
  end
  else if (AItem1.ListView = lvImportFunctions) then
  begin
    if (AColIdx = 1) then
    begin
      AProcessed := True;
      imp1 := AItem1.Data;
      imp2 := AItem2.Data;
      case AColIdx of
        1:
          begin
            v1 := imp1.Hint;
            v2 := imp2.Hint;
          end;
      end;
    end;
  end;
  if (v1 > v2) then
    Result := 1
  else if (v1 = v2) then
    Result := 0
  else
    Result := -1;
end;

procedure TImportExportFrame.GetPropertiesList(AList: TStringList);
begin
  inherited;
  AList.Add('lvExportFunctions.Columns[0].SortOrder');
  AList.Add('lvExportFunctions.Columns[1].Width');
  AList.Add('lvExportFunctions.Columns[1].SortOrder');
  AList.Add('lvExportFunctions.Columns[2].Width');
  AList.Add('lvExportFunctions.Columns[2].SortOrder');
  AList.Add('lvExportFunctions.Columns[3].Width');
  AList.Add('lvExportFunctions.Columns[3].SortOrder');
  AList.Add('lvExportFunctions.Columns[4].Width');
  AList.Add('lvExportFunctions.Columns[4].SortOrder');
  AList.Add('lvImportModules.Columns[0].SortOrder');
  AList.Add('lvImportModules.Columns[1].Width');
  AList.Add('lvImportModules.Columns[1].SortOrder');
  AList.Add('lvImportFunctions.Columns[0].SortOrder');
  AList.Add('lvImportFunctions.Columns[1].Width');
  AList.Add('lvImportFunctions.Columns[1].SortOrder');
  AList.Add('lvImportFunctions.Columns[2].Width');
  AList.Add('lvImportFunctions.Columns[2].SortOrder');
//  AList.Add('lvImportFunctions.Columns[3].Width');
//  AList.Add('lvImportFunctions.Columns[3].SortOrder');
end;

function TImportExportFrame.GetSplitter1Pos: Integer;
begin
  Result := lvExportFunctions.Height;
end;

function TImportExportFrame.GetSplitter2Pos: Integer;
begin
  Result := pImportModules.Width;
end;

function TImportExportFrame.GetUndecoratedText(const AText: string): string;
begin
  if UndecorateCPPNames and (AText <> '') then
    Result := UndecorateCPPName(AText)
  else
    Result := AText;
end;

procedure TImportExportFrame.lblImportFunctionsCountClick(Sender: TObject);
var
  si: TScrollInfo;
begin
  inherited;
  FillChar(si, SizeOf(si), 0);
  si.cbSize := SizeOf(si);
  si.fMask := SIF_DISABLENOSCROLL;
  SetScrollInfo(lvImportFunctions.Handle, SB_CTL, si, False);
end;

procedure TImportExportFrame.LoadData(const AFileName: string);
var
  plugin: TPEViewer;
  i: Integer;
  image: TPEImage;
  exportItem: TExportItem;
  importLib: TImportLib;
  li: TMyListItem;
//  s: string;
begin
  inherited;
  // Cancel validation, if any
  RemoveMessageFromQueue(WM_MYVALIDATE);

  // Load data
  plugin := Pointer(PluginObject);
  image := plugin.PEImage;
  image.LoadImage;
  lvImportFunctions.Clear;
  try
    image.LoadImExData;
    lvExportFunctions.Items.BeginUpdate;
    try
      lvExportFunctions.Clear;
      for i := 0 to image.ExportList.Count - 1 do
      begin
        exportItem := TExportItem(image.ExportList.Items[i]);
        li := lvExportFunctions.Items.Add;
        li.Caption := GetUndecoratedText(exportItem.Name);
        li.SubItems.Add(IntToStr(exportItem.Ordinal));
        li.SubItems.Add(IntToStr(exportItem.Hint));
        li.SubItems.Add(MyIntToHex(exportItem.Address));
        li.SubItems.Add(MyIntToHex(exportItem.RawOffset));
        li.Data := exportItem;
      end;

      lvExportFunctions.FixColumnsWidth;
      lvExportFunctions.UpdateHeaderState;
      lvExportFunctions.ResortItems();
    finally
      lvExportFunctions.Items.EndUpdate;
    end;
    lblExportFunctionsCount.Caption :=
      Format(SExportFunctionsCount, [lvExportFunctions.Items.Count, 0]);

    lvImportModules.Items.BeginUpdate;
    try
      lvImportModules.Clear;
      for i := 0 to image.ImportList.Count - 1 do
      begin
        importLib := TImportLib(image.ImportList.Items[i]);
        li := lvImportModules.Items.Add;
        li.Caption := importLib.Name;
        li.SubItems.Add('');
        if (importLib.ImportKind = ikDelayImport) then
          li.SubItemImages[0] := 2;
        li.Data := importLib;
        li.ImageIndex := -1;
      end;
      SetLength(FLastSelectedModules, image.ImportList.Count);
      FillChar(FLastSelectedModules[0], Length(FLastSelectedModules) * SizeOf(Boolean), 0);
      lvImportModules.FixColumnsWidth;
      lvImportModules.UpdateHeaderState;
      lvImportModules.ResortItems();
    finally
      lvImportModules.Items.EndUpdate;
    end;

    lblImportModulesCount.Caption :=
      Format(SImportModulesCount, [lvImportModules.Items.Count, 0]);

    if (image.Corrupted) then
      plugin.MainForm.SetErrorMsg(plugin.MainForm.Strings.ImageCorrupted);
  finally
    image.UnloadImage;
  end;

  if (lvImportModules.Items.Count > 0) then
  begin
    lvImportModules.Items[0].Selected := True;
  end;
  FLoaded := True;
  tValidationCheckDelay.Enabled := True;
end;

procedure TImportExportFrame.lvImportFunctionsCustomDrawItem(
  Sender: TMyCustomListView; Item: TMyListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  node: TImportItem;
begin
  inherited;
  node := Item.Data;
  if (node <> nil) and (node.ValidationState <> vsUnknown) then
  begin
    if (node.ValidationState = vsInvalid) then
      Sender.Canvas.Font.Color := clRed
    else if (node.ValidationState = vsValid) and (Item.ImageIndex <> 0) then
      Item.ImageIndex := 0;
  end;
end;

procedure TImportExportFrame.lvImportModulesCustomDrawItem(
  Sender: TMyCustomListView; Item: TMyListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  node: TImportLib;
begin
  inherited;
  node := Item.Data;
  if (node <> nil) and (node.ValidationState <> vsUnknown) then
  begin
    if (node.ValidationState = vsInvalid) then
      Sender.Canvas.Font.Color := clRed
    else if (node.ValidationState = vsValid) and (Item.ImageIndex <> 0) then
      Item.ImageIndex := 0
    else if (node.ValidationState = vsPartialValid) and (Item.ImageIndex <> 1) then
      Item.ImageIndex := 1;
  end;
end;

procedure TImportExportFrame.lvImportModulesSelectItem(Sender: TObject; Item: TMyListItem;
  Selected: Boolean);
var
  msg: TMsg;
begin
  while PeekMessage(msg, Handle, WM_MYSELECTION, WM_MYSELECTION, PM_REMOVE) do;
  PostMessage(Self.Handle, WM_MYSELECTION, WM_MYSELECTION, WM_MYSELECTION);
end;

procedure TImportExportFrame.pImportModulesTitleResize(Sender: TObject);
begin
  lblImportModulesCount.Left := pImportModulesTitle.ClientWidth - lblImportModulesCount.Width;
end;

procedure TImportExportFrame.PrepareStrings;
begin
  SExportFunctionsCount := lblExportFunctionsCount.Caption;
  SImportModulesCount := lblImportModulesCount.Caption;
  SImportFunctionsCount := lblImportFunctionsCount.Caption;
end;

procedure TImportExportFrame.RemoveMessageFromQueue(AMsg: Integer);
var
  msg: TMsg;
begin
  while PeekMessage(msg, Handle, AMsg, AMsg, PM_REMOVE) do;
end;

procedure TImportExportFrame.SetSplitter1Pos(Value: Integer);
begin
  if (Value > (Splitter1.Parent.ClientHeight - Splitter1.Height - Splitter1.MinSize)) then
    Value := Splitter1.Parent.ClientHeight - Splitter1.Height - Splitter1.MinSize;
  if (Value < Splitter1.MinSize) then
    Value := Splitter1.MinSize;
  lvExportFunctions.Height := Value;
end;

procedure TImportExportFrame.SetSplitter2Pos(Value: Integer);
begin
  if (Value > (Splitter2.Parent.ClientWidth - Splitter2.Width - Splitter2.MinSize)) then
    Value := Splitter2.Parent.ClientWidth - Splitter2.Width - Splitter2.MinSize;
  if (Value < Splitter2.MinSize) then
    Value := Splitter2.MinSize;
  pImportModules.Width := Value;
end;

procedure TImportExportFrame.SetUndecorateCPPNames(Value: Boolean);
begin
  FUndecorateCPPNames := Value;
  aUndecorateCPPNames.Checked := Value;
  if (FLoaded) then
    Reload();
end;

procedure TImportExportFrame.tValidationCheckDelayTimer(Sender: TObject);
begin
  tValidationCheckDelay.Enabled := False;
  if (lvImportModules.Items.Count > 0) then
    ValidateDependencies(lvImportModules.Items[0]);
end;

procedure TImportExportFrame.UpdateImportFunctionsList;

  function isSelectionModified: Boolean;
  var
    i: Integer;
  begin
    for i := 0 to lvImportModules.Items.Count - 1 do
    begin
      if (lvImportModules.Items[i].Selected <> FLastSelectedModules[i]) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

  procedure updateLastSelection;
  var
    i: Integer;
  begin
    for i := 0 to lvImportModules.Items.Count - 1 do
    begin
      FLastSelectedModules[i] := lvImportModules.Items[i].Selected;
    end;
  end;

var
  j, i, missedCount: Integer;
  importListItem, moduleListItem: TMyListItem;
  importLib: TImportLib;
  importItem: TImportItem;
begin
  if (isSelectionModified) then
  begin
    lvImportFunctions.Items.BeginUpdate;
    missedCount := 0;
    try
      for i := 0 to lvImportModules.Items.Count - 1 do
      begin
        moduleListItem := lvImportModules.Items[i];
        importLib := moduleListItem.Data;
        if (moduleListItem.Selected) then
        begin
          for j := 0 to importLib.Functions.Count - 1 do
          begin
            importItem := TImportItem(importLib.Functions.Items[j]);
            importListItem := lvImportFunctions.Items.Add;
            importListItem.Caption := GetUndecoratedText(importItem.DisplayName);
//            importListItem.SubItems.Add(MyOrdinal(importItem.Ordinal));
            importListItem.SubItems.Add(IntToStr(importItem.Hint));
            importListItem.SubItems.Add(TImportLib(importItem.Collection.Owner).Name);
            importListItem.Data := importItem;
            importListItem.ImageIndex := -1;
            if (not (importItem.ValidationState in [vsUnknown, vsValid])) then
              Inc(missedCount);
          end;
        end
        else
        begin
          for j := lvImportFunctions.Items.Count - 1 downto 0 do
          begin
            importListItem := lvImportFunctions.Items[j];
            importItem := importListItem.Data;
            if (importItem.Collection.Owner = importLib) then
              lvImportFunctions.Items.Delete(j);
          end;
        end;
      end;
      lvImportFunctions.FixColumnsWidth;
      lvImportFunctions.UpdateHeaderState;
      lvImportFunctions.ResortItems();
      lblImportFunctionsCount.Caption :=
        Format(SImportFunctionsCount, [lvImportFunctions.Items.Count, missedCount]);
    finally
      updateLastSelection;
      lvImportFunctions.Items.EndUpdate;
      // Нужно для ресайза
//      SetWindowPos(lvImportFunctions.Handle, 0, lvImportFunctions.Left, lvImportFunctions.Top, lvImportFunctions.Width, lvImportFunctions.Height + 1, 0);
    end;
  end;
end;

procedure TImportExportFrame.ValidateDependencies(AModuleItem: TMyListItem);
var
  i, missedCount: Integer;
  importLib: TImportLib;
  importFunc: TImportItem;
  tmpImg: TPEImage;
  allValid: Boolean;
begin
  importLib := AModuleItem.Data;
  if (importLib <> nil) and (importLib.Name <> '') then
  begin
    tmpImg := PEImageLoadLibrary(importLib.Name, TPEViewer(PluginObject).PEImage.MachineType = mt64);
    if (tmpImg = nil) then
    begin
      importLib.ValidationState := vsInvalid;
      for i := 0 to importLib.Functions.Count - 1 do
        TImportItem(importLib.Functions.Items[i]).ValidationState := vsInvalid;
    end
    else
    begin
      allValid := True;
      try
        try
          tmpImg.LoadImage;
          try
            tmpImg.LoadSimpleExportInfo;
            for i := 0 to importLib.Functions.Count - 1 do
            begin
              importFunc := TImportItem(importLib.Functions.Items[i]);
              if (importFunc.Ordinal <> 0) then
              begin
                if (tmpImg.SimpleExportList.IndexOfObject(Pointer(importFunc.Ordinal)) >= 0) then
                  importFunc.ValidationState := vsValid;
              end
              else
              begin
                if (tmpImg.SimpleExportList.IndexOf(importFunc.Name) >= 0) then
                  importFunc.ValidationState := vsValid;
              end;
              if (importFunc.ValidationState = vsUnknown) then
                importFunc.ValidationState := vsInvalid;
              allValid := allValid and (importFunc.ValidationState = vsValid);
            end;
            if allValid then
              importLib.ValidationState := vsValid
            else
              importLib.ValidationState := vsPartialValid;
          finally
            tmpImg.UnloadImage;
          end;
        finally
          tmpImg.Free;
        end;
      except
        importLib.ValidationState := vsPartialValid;
      end;
    end;
    lvImportModules.Invalidate;
    if (AModuleItem.Selected) then
      lvImportFunctions.Invalidate;
    i := AModuleItem.Index + 1;
    if (i < lvImportModules.Items.Count) then
    begin
      PostMessage(Handle, WM_MYVALIDATE, i, 0);
    end
    else
    begin
      // Update missed count on validation completion
      missedCount := 0;
      for i := 0 to lvImportModules.Items.Count - 1 do
      begin
        if (TImportLib(lvImportModules.Items[i].Data).ValidationState <> vsValid) then
          Inc(missedCount);
      end;
      lblImportModulesCount.Caption :=
        Format(SImportModulesCount, [lvImportModules.Items.Count, missedCount]);

      missedCount := 0;
      for i := 0 to lvImportFunctions.Items.Count - 1 do
      begin
        if (TImportItem(lvImportFunctions.Items[i].Data).ValidationState <> vsValid) then
          Inc(missedCount);
      end;
      lblImportFunctionsCount.Caption :=
        Format(SImportFunctionsCount, [lvImportFunctions.Items.Count, missedCount]);
    end;
  end;
end;

procedure TImportExportFrame.WMMySelection(var AMsg: TMessage);
begin
  RemoveMessageFromQueue(WM_MYSELECTION);
  UpdateImportFunctionsList;
end;

procedure TImportExportFrame.WMMyValidate(var AMsg: TMessage);
begin
  if (Integer(AMsg.WParam) >= 0) and  (Integer(AMsg.WParam) < lvImportModules.Items.Count) then
    ValidateDependencies(lvImportModules.Items[AMsg.WParam]);
end;

end.
