unit HeadersFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PEViewerFrameUnit, ExtCtrls, ComCtrls, StdCtrls, ImgList, CommonUtilities, PEHdrUtils,
  Vcl.Menus, Vcl.ActnList, PEUtils, MyListViewUnit;

type
  THeadersFrame = class(TPEViewerFrame)
    lblSections: TLabel;
    lvSections: TMyListView;
    Splitter1: TSplitter;
    pHeaders: TPanel;
    tvHeaders: TTreeView;
    lblHeaders: TLabel;
    pDataView: TPanel;
    ilEP: TImageList;
  private
    FHeadersCopy32: TImageHeaders32;
    FHeadersCopy64: TImageHeaders64;
    procedure ClearHeadersCopy;
    procedure MakeHeadersCopy(AImage: TPEImage);
    function GetSplitter1Pos: Integer;
    procedure SetSplitter1Pos(Value: Integer);
  protected
    procedure GetPropertiesList(AList: TStringList); override;
    function CustomListItemsCompare(
      var AProcessed: Boolean; AItem1, AItem2: TMyListItem;
      AColIdx: Integer): Integer; override;
  public
    destructor Destroy; override;

    procedure LoadData(const AFileName: string); override;
  published
    property Splitter1Pos: Integer read GetSplitter1Pos write SetSplitter1Pos;
  end;



implementation
uses
  PEViewerClass;


{$R *.dfm}


{ THeadersFrame }


procedure THeadersFrame.ClearHeadersCopy;
var
  i: Integer;
begin
  if (FHeadersCopy32.RawSections <> nil) then
  begin
    for I := FHeadersCopy32.RawSections.Count - 1 downto 0 do
      FreeMem(FHeadersCopy32.RawSections[I]);
    FHeadersCopy32.RawSections.Clear;
  end;
  if (FHeadersCopy64.RawSections <> nil) then
  begin
    for I := FHeadersCopy64.RawSections.Count - 1 downto 0 do
      FreeMem(FHeadersCopy64.RawSections[I]);
    FHeadersCopy64.RawSections.Clear;
  end;
end;

function THeadersFrame.CustomListItemsCompare(
  var AProcessed: Boolean; AItem1, AItem2: TMyListItem; AColIdx: Integer): Integer;
var
  v1, v2: SysInt;
  s1, s2: TSectionItem;
begin
  Result := 0;
  if (AColIdx >= 1) and (AColIdx <= 5) then
  begin
    AProcessed := True;
    s1 := AItem1.Data;
    s2 := AItem2.Data;
    case AColIdx of
      1:
        begin
          v1 := s1.VirtualAddress;
          v2 := s2.VirtualAddress;
        end;

      2:
        begin
          v1 := s1.RawOffset;
          v2 := s2.RawOffset;
        end;

      3:
        begin
          v1 := s1.VirtualSize;
          v2 := s2.VirtualSize;
        end;

      4:
        begin
          v1 := s1.RawSize;
          v2 := s2.RawSize;
        end;

      5:
        begin
          v1 := s1.Flags;
          v2 := s2.Flags;
        end;

      else
        begin
          v1 := 0;
          v2 := 0;
        end;
    end;
    if (v1 > v2) then
      Result := 1
    else if (v1 = v2) then
      Result := 0
    else
      Result := -1;
  end;
end;

destructor THeadersFrame.Destroy;
begin
  ClearHeadersCopy;
  inherited;
end;

procedure THeadersFrame.GetPropertiesList(AList: TStringList);
begin
  inherited;
  AList.Add('lvSections.Columns[0].SortOrder');
  AList.Add('lvSections.Columns[1].Width');
  AList.Add('lvSections.Columns[1].SortOrder');
  AList.Add('lvSections.Columns[2].Width');
  AList.Add('lvSections.Columns[2].SortOrder');
  AList.Add('lvSections.Columns[3].Width');
  AList.Add('lvSections.Columns[3].SortOrder');
  AList.Add('lvSections.Columns[4].Width');
  AList.Add('lvSections.Columns[4].SortOrder');
  AList.Add('lvSections.Columns[5].Width');
  AList.Add('lvSections.Columns[5].SortOrder');
  AList.Add('lvSections.Columns[6].Width');
  AList.Add('lvSections.Columns[6].SortOrder');
end;

function THeadersFrame.GetSplitter1Pos: Integer;
begin
  Result := lvSections.Height;
end;

procedure THeadersFrame.LoadData(const AFileName: string);
var
  plugin: TPEViewer;
  i: Integer;
  image: TPEImage;
  sectionItem: TSectionItem;
  li: TMyListItem;
  epSecIdx: Integer;
begin
  plugin := Pointer(PluginObject);
  image := plugin.PEImage;
  image.LoadImage;
  try
    image.LoadSectionData;
    image.GetEntryPointRAW(epSecIdx);

    lvSections.Items.BeginUpdate;
    try
      lvSections.Clear;
      for i := 0 to image.SectionList.Count - 1 do
      begin
        sectionItem := TSectionItem(image.SectionList.Items[i]);
        li := lvSections.Items.Add;
        li.Caption := sectionItem.Name;
        li.SubItems.Add(MyIntToHex(sectionItem.VirtualAddress));
        li.SubItems.Add(MyIntToHex(sectionItem.RawOffset));
        li.SubItems.Add(MyIntToHex(sectionItem.VirtualSize));
        li.SubItems.Add(MyIntToHex(sectionItem.RawSize));
        li.SubItems.Add(MyIntToHex(sectionItem.Flags));
        li.SubItems.Add(SecFlags2Str(sectionItem.Flags));
        li.Data := sectionItem;
        if (i = epSecIdx) then
          li.ImageIndex := 0
        else
          li.ImageIndex := -1;
      end;
      lvSections.FixColumnsWidth;
      lvSections.UpdateHeaderState;
      lvSections.ResortItems();
    finally
      lvSections.Items.EndUpdate;
    end;


    tvHeaders.Items.BeginUpdate;
    try
      tvHeaders.Items.Clear;
      ClearHeadersCopy;
      MakeHeadersCopy(image);
      if (image is TPEImage64) then
      begin
        FHeadersCopy64.Dos := PImageDosHeader(image.Mapper.Map)^;
        FHeadersCopy64.NT := PImageNtHeaders64(GetNTHeaders(image.Mapper.Map))^;
        FillTreeView64(tvHeaders, @FHeadersCopy64);
      end
      else
      begin
        FHeadersCopy32.Dos := PImageDosHeader(image.Mapper.Map)^;
        FHeadersCopy32.NT := GetNTHeaders(image.Mapper.Map)^;
        FillTreeView32(tvHeaders, @FHeadersCopy32);
      end;
    finally
      tvHeaders.Items.EndUpdate;
    end;

  finally
    image.UnloadImage;
  end;
end;

procedure THeadersFrame.MakeHeadersCopy(AImage: TPEImage);
var
  i: Integer;
  si: PSectionCopy;
begin
  if (AImage is TPEImage64) then
  begin
    if (FHeadersCopy64.RawSections = nil) then
      FHeadersCopy64.RawSections := Tlist.Create
    else
      ClearHeadersCopy;
    for i := 0 to AImage.RawSectionCount - 1 do
    begin
      GetMem(si, SizeOf(si^));
      si.RawHeaderStruct := PImageSectionHeader(AImage.RawSections[i])^;
      si.OrigPtr := AImage.RawSections[i];
      FHeadersCopy64.RawSections.Add(si);
    end;
  end
  else
  begin
    if (FHeadersCopy32.RawSections = nil) then
      FHeadersCopy32.RawSections := Tlist.Create
    else
      ClearHeadersCopy;
    for i := 0 to AImage.RawSectionCount - 1 do
    begin
      GetMem(si, SizeOf(si^));
      si.RawHeaderStruct := PImageSectionHeader(AImage.RawSections[i])^;
      si.OrigPtr := AImage.RawSections[i];
      FHeadersCopy32.RawSections.Add(si);
    end;
  end;
end;

procedure THeadersFrame.SetSplitter1Pos(Value: Integer);
begin
  if (Value > (Splitter1.Parent.ClientHeight - Splitter1.Height - Splitter1.MinSize)) then
    Value := Splitter1.Parent.ClientHeight - Splitter1.Height - Splitter1.MinSize;
  if (Value < Splitter1.MinSize) then
    Value := Splitter1.MinSize;
  lvSections.Height := Value;
end;

end.
