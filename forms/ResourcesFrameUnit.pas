unit ResourcesFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PEViewerFrameUnit, ExtCtrls, ComCtrls, StdCtrls, CommonUtilities,
  ImgList, PEUtils, DataViewUnit, KOLClasses, Types, Vcl.Menus, Vcl.ActnList, Clipbrd, Math,
  Vcl.Imaging.PngImage;

type


  TSingleIconData = record
    Width,
    Height: Integer;
    ColorDepth: Byte;
    IsIcon: ByteBool;
    ID: Word;
    HotSpot: TSmallPoint;
  end;

  TBitmapData = record
    Width,
    Height: SysInt;
    ColorDepth: Byte;
    ID: Word;
  end;

  TViewMode = (vmDefault, vmBin, vmText, vmHex, vmProcessed);

  TResViewType = (rvImage, rvDataView);

  TCopyMode = (cmData, cmDisplay);
  TCopyDisplayItem = (Offset, Hex, Data);
  TCopyDisplayItems = set of TCopyDisplayItem;

  {$M+}
  TResourcesStringStore = class(TStringStore)
  private
    FImageCaption: string;
    FHotspot: string;
    FStatusFormat: string;
    FName: string;
  public
    constructor Create; override;
  published
    property ImageCaptionFormat: string read FImageCaption write FImageCaption;
    property Hotspot: string read FHotspot write FHotspot;
    property StatusFormat: string read FStatusFormat write FStatusFormat;
    property Name: string read FName write FName;
//'Single Cursor',
//'Bitmap',
//'Single Icon',
//'Menu',
//'Dialog',
//'String',
//'Font',
//'Single Font',
//'Accelerator',
//'RCData',
//'Message Table',
//'Cursor',
//'Icon',
//'Version',
//'Dialog Include',
//'Plug&Play',
//'VXD',
//'AniCursor',
//'AniIcon',
//'HTML',
//'Manifest',
//'User Defined'

  end;
  {$M-}


  TResourcesFrame = class(TPEViewerFrame)
    tvTree: TTreeView;
    Splitter1: TSplitter;
    edStatus: TEdit;
    pResText: TPanel;
    pResImg: TPanel;
    lbImgGroup: TListBox;
    ilResources: TImageList;
    lblImgInfo: TLabel;
    iImage: TImage;
    pmData: TPopupMenu;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Processed1: TMenuItem;
    ext1: TMenuItem;
    BinaryText1: TMenuItem;
    Hex1: TMenuItem;
    aDataCopy: TAction;
    aDataSelectAll: TAction;
    aDataSaveAs: TAction;
    aDataViewProcessed: TAction;
    aDataViewText: TAction;
    aDataViewBinaryText: TAction;
    aDataViewHex: TAction;
    sdSaveAs: TSaveDialog;
    pmTree: TPopupMenu;
    miTreeSaveAs: TMenuItem;
    lblError: TLabel;
    procedure tvTreeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvTreeExpanded(Sender: TObject; Node: TTreeNode);
    procedure lbImgGroupClick(Sender: TObject);
    procedure tvTreeChange(Sender: TObject; Node: TTreeNode);
    procedure aDataSelectAllExecute(Sender: TObject);
    procedure aDataViewProcessedExecute(Sender: TObject);
    procedure aDataSaveAsExecute(Sender: TObject);
    procedure tvTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvTreeClick(Sender: TObject);
    procedure aDataCopyExecute(Sender: TObject);
    procedure pmDataPopup(Sender: TObject);
  private
    FResTreeRoot: TResNode;
    FCurNode: TResNode;
    FCurResType: Integer;
    FDataStream: TMemoryStream;
    FSingleIconData: TSingleIconData;
    FBitmapData: TBitmapData;
    FCurPage: TResViewType;
    FDataView: TDataViewer;
    function GetSplitter1Pos: Integer;
    procedure SetSplitter1Pos(Value: Integer);
    procedure SetCurPage(const Value: TResViewType);
    procedure SetResourceError(const AMsg: string);
  protected
    property ResTreeRoot: TResNode read FResTreeRoot write FResTreeRoot;
    property CurNode: TResNode read FCurNode write FCurNode;
    property CurResType: Integer read FCurResType write FCurResType;
    property SingleIconData: TSingleIconData read FSingleIconData write FSingleIconData;
    property BitmapData: TBitmapData read FBitmapData write FBitmapData;
    property DataStream: TMemoryStream read FDataStream write FDataStream;
    property CurPage: TResViewType read FCurPage write SetCurPage;


    procedure UnloadResources;
    procedure SetCurResNode(AItem: TTreeNode; AMode: TViewMode);
    procedure ShowNodeData(ANode: TResNode; AMode: TViewMode; ACheckText: Boolean);
    procedure LoadBinaryData(ANode: TResNode; AStream: TStream; AMode: TViewerMode; ACheckText: Boolean);
    procedure LoadIconGroup(ANode: TResNode; AStream: TStream; AIsIcon: Boolean);
    procedure LoadSingleIcon(ANode: TResNode; AStream: TStream; AIsIcon, AShowLabel: Boolean);
    procedure LoadSingleIconData(
      ANode: TResNode; ASourceStream: TStream; AIsIcon: Boolean; var ARslt: TSingleIconData);
    procedure LoadBitmap(ANode: TResNode; AStream: TStream);
    procedure LoadBitmapData(ANode: TResNode; ASourceStream: TStream; var Rslt: TBitmapData);
    procedure LoadStrings(ANode: TResNode; AStream: TStream);
    procedure LoadStringsData(ANode: TResNode; ASourceStream: TStream; AStrList: TStringList);
    procedure LoadVersionInfo(ANode: TResNode; AStream: TStream);
    procedure LoadVersionInfoData(ANode: TResNode; ASrcStream: TStream; AStrList: TStringList);
    procedure SafeClearImgGroups;
    procedure SearchMaxDepthAndSize;
    procedure SetCurGroupItem(AItem: Integer);
    procedure AssignDataStream(ASoure: TStream; ASize: Integer);
    procedure SetImgInfo(const AMsg: string = '');
    procedure UpdateViewMenuState(AMode: TViewMode);
    function GetImageCaption(Width, Height: Integer; ColorDepth: Byte; ID: Word): string;
    procedure SaveIcon(ANode: TResNode; AStream: TStream; AIsIcon: Boolean);
    function SearchNode(AID: Word; AResType, ALang: SysUInt; ARoot: TResNode): TResNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadData(const AFileName: string); override;
    procedure ApplyLocalization; override;
  published
    Strings: TResourcesStringStore;
    property Splitter1Pos: Integer read GetSplitter1Pos write SetSplitter1Pos;
    property DataView: TDataViewer read FDataView;
  end;

var
  ResourcesFrame: TResourcesFrame;

implementation
uses
  PEViewerClass, PETypes;


{$R *.dfm}

type
  TPngMagic = array[0..7] of Byte;

const
  PNGMagic: TPngMagic = (137, 80, 78, 71, 13, 10, 26, 10);
  // recognized and handled chunk types
  IHDR = 'IHDR';
  IDAT = 'IDAT';
  IEND = 'IEND';
  PLTE = 'PLTE';
  gAMA = 'gAMA';
  tRNS = 'tRNS';
  bKGD = 'bKGD';


type

  TPNGChunkHeader = packed record
    Length: DWORD;  // size of data (entire chunk excluding itself, CRC and type)
    ChunkType: TChunkName;
  end;

  // chunk type: 'IHDR'
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width,
    Height: DWORD;
    BitDepth,          // bits per sample (allowed are 1,2,4,8 and 16)
    ColorType,         // combination of:
                       //   1 - palette used
                       //   2 - colors used
                       //   4 - alpha channel used
                       // allowed values are:
                       //   0 - gray scale (allowed bit depths are: 1,2,4,8,16)
                       //   2 - RGB (8,16)
                       //   3 - palette (1,2,4,8)
                       //   4 - gray scale with alpha (8,16)
                       //   6 - RGB with alpha (8,16)
    Compression,       // 0 - LZ77, others are not yet defined
    Filter,            // filter mode 0 is the only one currently defined
    Interlaced: byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  end;



function SwapLong(Value: cardinal): DWORD; overload;
// swaps high and low bytes of the given 32 bit value
asm
  BSWAP EAX
end;

procedure SwapLong(P: PInteger; Count: DWORD); overload;
// swaps high and low bytes of 32 bit values
// EAX contains P, EDX contains Count
asm
@@Loop:
        MOV   ECX,[EAX]
        BSWAP ECX
        MOV   [EAX],ECX
        ADD   EAX,4
        DEC   EDX
        JNZ   @@Loop
end;

function LoadAndSwapHeader(AStream: TStream; var APngHeader: TPNGChunkHeader): DWORD;
// read next chunk header and swap fields to little endian,
// returns the intial CRC value for following checks
begin
  AStream.Read(APngHeader, sizeof(APngHeader));
  Result := CRC32(0, @APngHeader.ChunkType, 4);
  APngHeader.Length := SwapLong(APngHeader.Length);
end;

function IsChunk(ChunkType: TChunkName; const APngHeader: TPNGChunkHeader): boolean;
// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type
const
  Mask = not $20202020;
begin
  Result:=(Cardinal(APngHeader.ChunkType) and Mask)=(Cardinal(ChunkType) and Mask);
end;

procedure ReadDataAndCheckCRC(
  AStream: TStream; APngBuffer: Pointer; var APngHeader: TPNGChunkHeader; var ACurrentCrc: DWORD);
// Allocates memory in FRawBuffer and reads the next Header.Length bytes from Stream.
// Furthermore, the CRC value following the data is read as well and compared with
// the CRC value which is calculated here.
var
  FileCRC: DWORD;
begin
//    ReallocMem(FRawBuffer, FHeader.Length);
  AStream.Read(APngBuffer^, APngHeader.Length);
  AStream.Read(FileCRC, sizeof(FileCRC));
  FileCRC := SwapLong(FileCRC);
  // The type field of a chunk is included in the CRC, this serves as initial value
  // for the calculation here and is determined in LoadAndSwapHeader.
  ACurrentCrc := CRC32(ACurrentCrc, APngBuffer, APngHeader.Length);
  if (ACurrentCrc <> FileCRC) then
    raise Exception.Create('Corrupted PNG icon data');
end;



function obtainRealIconSize(
  ASingleItemNode: TResNode; AStream: TStream; ATempBuffer: Pointer;
  ATempBufferSz: Integer; AIsIcon: Boolean; var APngDescription: TIHDRChunk): TPoint;
var
  bih: PBitmapInfoHeader;
  pngCurrentCrc: DWORD;
  pngHeader: TPNGChunkHeader;
  pngBuffer: Pointer;
begin
  Int64(Result) := 0;
  if (ASingleItemNode <> nil) then
  begin
    AStream.Position := ASingleItemNode.DataRaw;
    AStream.Read(ATempBuffer^, ATempBufferSz);
    // Check if image is PNG
    if CompareMem(ATempBuffer, @PNGMagic, SizeOf(PNGMagic)) then
    begin
      AStream.Position := ASingleItemNode.DataRaw + SizeOf(PNGMagic);
      // first chunk must be an IHDR chunk
      pngCurrentCrc := LoadAndSwapHeader(AStream, pngHeader);
      if IsChunk(IHDR, pngHeader) then
      begin
        // read IHDR chunk
        GetMem(pngBuffer, pngHeader.Length);
        try
          ReadDataAndCheckCRC(AStream, pngBuffer, pngHeader, pngCurrentCrc);
          Move(pngBuffer^, APngDescription, SizeOf(APngDescription));
          SwapLong(@APngDescription, 2);
          if (APngDescription.Width = 0) or (APngDescription.Height = 0) then
          begin
            Int64(Result) := 0;
          end
          else
          begin
            Result.X := APngDescription.Width;
            Result.Y := APngDescription.Height;
          end;
        finally
          FreeMem(pngBuffer);
        end;
      end;
    end
    else
    begin
      if AIsIcon then
      begin
        bih := ATempBuffer;
      end else
      begin
        bih := Pointer(SysUInt(ATempBuffer) + 4);
      end;
      Result.X := bih.biWidth;
      Result.Y := bih.biHeight;
      if (Result.Y mod Result.X) = 0 then
        Result.Y := Result.X;
    end;
  end;
end;


function ResType2ImgIdx(ResType: Integer): Integer; // ResType = -1 - closed folder; -2 - opened folder
begin
  case ResType of
    -1: Result := 1;
    -2: Result := 2;
    RT_BITMAP: Result := 4;
    RT_ICON, RT_GROUP_ICON: Result := 5;
    RT_CURSOR, RT_GROUP_CURSOR: Result := 6;
    RT_STRING, RT_MESSAGETABLE, RT_MANIFEST, RT_HTML: Result := 7;
    PETypes.RT_RCDATA, RT_PLUGPLAY, RT_VXD: Result := 8;

    else
      Result := 3;
  end;
end;


function ResType2Mode(ResType: Integer): TViewMode;
begin
  case ResType of
    RT_ICON, RT_CURSOR, RT_BITMAP, RT_STRING, RT_GROUP_CURSOR, RT_GROUP_ICON,
    RT_VERSION:
      Result := vmProcessed;

    RT_MANIFEST: Result := vmText;
    RT_RCDATA: Result := vmBin;
  else
    Result := vmBin;
  end;
end;


procedure ResType2FileExt(AItem: TResNode; var AExtension, AFileDescription: string);
var
  resType: Integer;
  parent: TResNode;
begin
  resType := AItem.DataType;
  case resType of
    RT_BITMAP: AExtension := 'bmp';
    RT_ICON, RT_GROUP_ICON: AExtension := 'ico';
    RT_CURSOR, RT_GROUP_CURSOR: AExtension := 'cur';
    RT_STRING, RT_MESSAGETABLE: AExtension := 'txt';
    RT_MANIFEST: AExtension := 'xml';
    RT_HTML: AExtension := 'html';
    else
      AExtension := 'bin';
  end;
  if (resType >= Low(ResTypes)) and (resType <= High(ResTypes)) then
    AFileDescription := ResTypes[resType]
  else
  begin
    parent := AItem.Parent;
    if (parent <> nil) then
      AFileDescription := parent.DisplayName
    else
      AFileDescription := 'Binary File';
  end;
end;

function CheckForPlainText(Ptr: Pointer; Size: SysUInt): Boolean;
var
  I: Integer;
  N: Byte;
begin
  for I := 0 to Size - 1 do begin
    N := PByteArray(Ptr)[I];
    if (N < 32) then
      if not (N in [10, 13, 9]) then begin
        Result := False;
        Exit;
      end;
  end;
  Result := True;
end;



{ TResourcesFrame }


procedure TResourcesFrame.aDataCopyExecute(Sender: TObject);
var
  s: AnsiString;
  l, ss, se: Integer;
begin
  if (not DataView.Empty) then
  begin
    if (DataView.SelStart > DataView.SelEnd) then
    begin
      ss := DataView.SelEnd;
      se := DataView.SelStart;
    end
    else
    begin
      ss := DataView.SelStart;
      se := DataView.SelEnd;
    end;
    l := se - ss;
    if (l > 0) then
    begin
      SetLength(s, l);
      DataView.Stream.Read(s[1], l);
      Clipboard.AsText := string(s);
    end;
  end;
end;

procedure TResourcesFrame.aDataSaveAsExecute(Sender: TObject);
var
  fileNameSample, fileDesc, fileExt: string;
  stmSrc, stmDest: TFileStream;
begin
  if (CurNode <> nil) then
  begin
    ResType2FileExt(CurNode, fileExt, fileDesc);
    if (CurNode.Name = '') and (CurNode.ID = 0) then
      fileNameSample := Format('%d - %s', [CurNode.Parent.ID, CurNode.DisplayName])
    else
      fileNameSample := CurNode.DisplayName;
    fileNameSample := fileNameSample + '.' + fileExt;
    sdSaveAs.Filter := Format('%s (*.%s)|*.%s|All Files|*.*', [fileDesc, fileExt, fileExt]);
    sdSaveAs.FileName := fileNameSample;
    sdSaveAs.DefaultExt := fileExt;
    if (sdSaveAs.Execute(ParentWindow)) then
    begin
      stmDest := TFileStream.Create(sdSaveAs.FileName, fmCreate or fmOpenWrite);
      try
        case CurNode.DataType of
          RT_ICON,
          RT_GROUP_ICON:
            begin
              SaveIcon(CurNode, stmDest, True);
            end;
          RT_CURSOR,
          RT_GROUP_CURSOR:
            begin
              SaveIcon(CurNode, stmDest, False);
            end;
          else
            begin
              stmSrc := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
              try
                stmSrc.Position := CurNode.DataRaw;
                stmDest.CopyFrom(stmSrc, CurNode.DataSize);
              finally
                stmSrc.Free;
              end;
            end;
        end;
      finally
        stmDest.Free;
      end;
    end;
  end;
end;

procedure TResourcesFrame.aDataSelectAllExecute(Sender: TObject);
begin
  DataView.SelectAll;
end;

procedure TResourcesFrame.aDataViewProcessedExecute(Sender: TObject);
var
  m: TViewMode;
begin
  case (Sender as TCustomAction).Tag of
    1: m := vmProcessed;
    2: m := vmText;
    3: m := vmBin;
    4: m := vmHex;
    else
      m := vmDefault;
  end;

  SetCurResNode(tvTree.Selected, m);
end;

procedure TResourcesFrame.ApplyLocalization;
begin
  DataView.FontSize := 1;
  inherited;
  if (DataView.FontSize = 1) then
    DataView.FontSize := pResText.Font.Size;
end;

procedure TResourcesFrame.AssignDataStream(ASoure: TStream; ASize: Integer);
var
  pos: Int64;
begin
  if (FDataStream = nil) then
    FDataStream := TMemoryStream.Create;
  FDataStream.Size := ASize;
  if (ASoure <> nil) then
  begin
    pos := ASoure.Position;
    ASoure.Read(FDataStream.Memory^, ASize);
    ASoure.Position := pos;
  end;
end;

constructor TResourcesFrame.Create(AOwner: TComponent);
begin
  inherited;
  Strings := TResourcesStringStore.Create;
  pResText.Visible := False;
  pResImg.Visible := False;
  lbImgGroup.Visible := False;
  FDataView := TDataViewer.Create(pResText);
  FDataView.Align := alClient;
  FDataView.Name := 'DataView';
  pResText.Align := alClient;
  lbImgGroup.Align := alLeft;
  pResImg.Align := alClient;
  lblError.Visible := False;
  lblError.Align := alClient;
end;

destructor TResourcesFrame.Destroy;
begin
  FDataStream.Free;
  Strings.Free;
  inherited;
end;

function TResourcesFrame.GetImageCaption(
  Width, Height: Integer; ColorDepth: Byte; ID: Word): string;
begin
  Result := Format(Strings.ImageCaptionFormat, [Width, Height, ColorDepth, ID]);
end;

function TResourcesFrame.GetSplitter1Pos: Integer;
begin
  Result := tvTree.Width;
end;

procedure TResourcesFrame.lbImgGroupClick(Sender: TObject);
begin
  SetCurGroupItem(lbImgGroup.ItemIndex);
end;

procedure TResourcesFrame.LoadBinaryData(ANode: TResNode; AStream: TStream; AMode: TViewerMode; ACheckText: Boolean);
var
  p: Pointer;
begin
  FDataView.Freeze;
  try
    AssignDataStream(AStream, ANode.DataSize);
    p := DataStream.Memory;
    if ACheckText then
      if CheckForPlainText(P, ANode.DataSize) then
        AMode := hmTextFree;
    FDataView.SetStream(DataStream, 0, ANode.DataSize, AMode);
  finally
    FDataView.UnFreeze;
  end;
end;

procedure TResourcesFrame.LoadBitmap(ANode: TResNode; AStream: TStream);
begin
  LoadBitmapData(ANode, AStream, FBitmapData);
  SetImgInfo(
    GetImageCaption(
      BitmapData.Width, BitmapData.Height, BitmapData.ColorDepth,
      BitmapData.ID));
end;

procedure TResourcesFrame.LoadBitmapData(
  ANode: TResNode; ASourceStream: TStream; var Rslt: TBitmapData);
var
  bmp: TKOLBitmap;
begin
  Bmp := TKOLBitmap.Create(0, 0);
  try
    ASourceStream.Seek(ANode.DataRaw, soFromBeginning);
    Bmp.LoadFromStreamEx(ASourceStream);

    Rslt.Width := Bmp.Width;
    Rslt.Height := Bmp.Height;
    Rslt.ColorDepth := Bmp.BitsPerPixel;
    Rslt.ID := ANode.ID;

    iImage.Picture.Bitmap.SetSize(bmp.Width, bmp.Height);
    Bmp.DrawTransparent(iImage.Picture.Bitmap.Canvas.Handle, 0, 0, clBtnFace);
    iImage.Picture.Bitmap.OnChange(iImage.Picture.Bitmap);
  finally
    Bmp.Free;
  end;
end;

procedure TResourcesFrame.LoadData(const AFileName: string);

  procedure FillTreeView(Tree: TResNode; Root: TTreeNode);
  var
    i: Integer;
    s: string;
    itm: TTreeNode;
  begin
    if not Tree.IsRoot then
    begin
      S := Tree.DisplayName;
      Itm := tvTree.Items.AddChild(Root, S);
      Itm.Data := Tree;
      if Tree.IsDirectory then
        Itm.StateIndex := ResType2ImgIdx(-1)
      else
        Itm.StateIndex := ResType2ImgIdx(Tree.DataType);
    end
    else
      Itm := nil;
    for I := 0 to Tree.SubDirs.Count - 1 do
      FillTreeView(Tree.SubDirs.Items[I] as TResNode, Itm);
  end;

var
  plugin: TPEViewer;
  image: TPEImage;
begin
  FDataView.Parent := pResText; // to delay window creation
  plugin := Pointer(PluginObject);
  image := plugin.PEImage;
  image.LoadImage;
  try
    UnloadResources;
    image.LoadResourceTree;
    ResTreeRoot := image.ResourceTree;
    if (ResTreeRoot <> nil) then
      FillTreeView(ResTreeRoot, nil);
    CurNode := nil;
  finally
    image.UnloadImage;
  end;
end;

procedure TResourcesFrame.LoadIconGroup(ANode: TResNode; AStream: TStream; AIsIcon: Boolean);
var
  tempBuffer: Pointer;
  tempBufferSz: Integer;
  gid: PGrpIconDir;
  ide: TGrpIconDirEntry;
  buf: Pointer;
  i, n: Integer;
  itemNode: TResNode;
  realIconSize: TPoint;
  pngDescr: TIHDRChunk;
begin
  GetMem(buf, ANode.DataSize);
  tempBuffer := nil;
  try
    tempBufferSz := SizeOf(TBitmapInfoHeader) + 4;
    GetMem(tempBuffer, tempBufferSz);
    SafeClearImgGroups;
    AStream.Read(buf^, ANode.DataSize);
    gid := buf;

    for i := 0 to GID.Count - 1 do
    begin
      ide := GID.Entries[i];
      if AIsIcon then
        itemNode := SearchNode(ide.ID, SysUInt(RT_ICON), ANode.Lang, ResTreeRoot)
      else
        itemNode := SearchNode(ide.ID, SysUInt(RT_CURSOR), ANode.Lang, ResTreeRoot);

      if (itemNode = nil) then
        raise Exception.Create('Error searching corresponding resource');
      realIconSize :=
        obtainRealIconSize(itemNode, AStream, tempBuffer, tempBufferSz, AIsIcon, pngDescr);
      N := lbImgGroup.Items.Add(GetImageCaption(realIconSize.X, realIconSize.Y, ide.BitCount, ide.ID));

      if AIsIcon then
      begin
        lbImgGroup.Items.Objects[N] := Pointer(ItemNode);
      end
      else
      begin
        lbImgGroup.Items.Objects[N] := Pointer(ItemNode);
      end;
      ItemNode.ImgX := realIconSize.X;
      ItemNode.ImgY := realIconSize.Y;
      ItemNode.ImgColorDepth := IDE.BitCount;
    end;
    SearchMaxDepthAndSize;
    lbImgGroupClick(nil);
  finally
    FreeMem(tempBuffer);
    FreeMem(buf);
  end;
end;

procedure TResourcesFrame.LoadSingleIcon(
  ANode: TResNode; AStream: TStream; AIsIcon, AShowLabel: Boolean);
var
  s: string;
begin
  LoadSingleIconData(ANode, AStream, AIsIcon, FSingleIconData);
  s :=
    GetImageCaption(
      SingleIconData.Width, SingleIconData.Height, SingleIconData.ColorDepth, SingleIconData.ID);
  if not AIsIcon then
    s :=
      Format(
        '%s; %s: %d x %d',
        [s, Strings.Hotspot, SingleIconData.HotSpot.x, SingleIconData.HotSpot.y]);
  SetImgInfo(s);
end;

procedure TResourcesFrame.LoadSingleIconData(
  ANode: TResNode; ASourceStream: TStream; AIsIcon: Boolean; var ARslt: TSingleIconData);

  function ColorBytesPerPixel(const ColorType, BitDepth: Byte): Integer;
  begin
    case ColorType of
      {Palette and grayscale contains a single value, for palette}
      {an value of size 2^bitdepth pointing to the palette index}
      {and grayscale the value from 0 to 2^bitdepth with color intesity}
      COLOR_GRAYSCALE, COLOR_PALETTE:
        Result := (1 * BitDepth + 7) div 8;
      {RGB contains 3 values R, G, B with size 2^bitdepth each}
      COLOR_RGB:
        Result := (1 * BitDepth * 3) div 8;
      {Contains one value followed by alpha value booth size 2^bitdepth}
      COLOR_GRAYSCALEALPHA:
        Result := (1 * BitDepth * 1) div 8;
      {Contains four values size 2^bitdepth, Red, Green, Blue and alpha}
      COLOR_RGBALPHA:
        Result := (1 * BitDepth * 3) div 8;
      else
        Result := 0;
    end {case ColorType}
  end;

var
  buf: Pointer;
  bih: PBitmapInfoHeader;
  icon: TKOLIcon;
  bmp: TKOLBitmap;
  png: TPngImage;
begin
  GetMem(buf, ANode.DataSize);
  try
    ASourceStream.Position := ANode.DataRaw;
    ASourceStream.Read(buf^, ANode.DataSize);
    if CompareMem(buf, @PNGMagic, SizeOf(PNGMagic)) then
    begin
      ASourceStream.Position := ANode.DataRaw;
      png := TPngImage.Create;
      try
        png.LoadFromStream(ASourceStream);
        ARslt.Width := png.Width;
        ARslt.Height := png.Height;
        ARslt.ColorDepth :=
          png.Header.BitDepth * ColorBytesPerPixel(png.Header.ColorType, png.Header.BitDepth);
        ARslt.IsIcon := True;
        ARslt.ID := ANode.ID;
        DWORD(ARslt.HotSpot) := 0;
        iImage.Picture.Bitmap.SetSize(ARslt.Width, ARslt.Height);
        iImage.Picture.Bitmap.Canvas.Brush.Color := clBtnFace;
        iImage.Picture.Bitmap.Canvas.FillRect(MakeRect(0, 0, ARslt.Width, ARslt.Height));
        png.DrawUsingPixelInformation(iImage.Picture.Bitmap.Canvas, MakePoint(0, 0));
        iImage.Picture.Bitmap.OnChange(iImage.Picture.Bitmap);
      finally
        png.Free;
      end;
    end
    else
    begin
      if AIsIcon then
      begin
        bih := Buf;
        Cardinal(ARslt.HotSpot) := 0;
      end else
      begin
        bih := Pointer(SysUInt(Buf) + 4);
        ARslt.HotSpot := PSmallPoint(Buf)^;
      end;
      ARslt.Width := BIH.biWidth;
      ARslt.Height := BIH.biWidth;
      ARslt.ColorDepth := BIH.biBitCount;
      icon := TKOLIcon.Create;
      ASourceStream.Seek(ANode.DataRaw, soFromBeginning);
      if not AIsIcon then
        ASourceStream.Seek(4, soFromCurrent);
      icon.LoadFromStream(ASourceStream);
      bmp := TKOLBitmap.Create(icon.Width, icon.Height);
      bmp.Handle := icon.Convert2Bitmap(clBtnFace);
      iImage.Picture.Bitmap.SetSize(icon.Width, icon.Height);
      bmp.Draw(iImage.Picture.Bitmap.Canvas.Handle, 0, 0);
      iImage.Picture.Bitmap.OnChange(iImage.Picture.Bitmap);
      bmp.Free;
      icon.Free;
      ARslt.ID := ANode.ID;
    end;
  finally
    FreeMem(Buf);
  end;
end;

procedure TResourcesFrame.LoadStrings(ANode: TResNode; AStream: TStream);
var
  sl: TStringList;
  buf: Pointer;
  tmp: string;
  cnt, l, id: Integer;
  ptr: PWORD;
begin
  sl := TStringList.Create;
  try
    GetMem(buf, ANode.DataSize);
    try
      AStream.Read(buf^, ANode.DataSize);

      cnt := 0;
      ptr := buf;
      while (cnt < 16) and (SysUInt(ptr) < (SysUInt(buf) + ANode.DataSize - 1)) do
      begin
        l := ptr^;
        Inc(ptr);
        if (l > 0) then
        begin
          SetLength(tmp, l);
          Move(ptr^, tmp[1], l * SizeOf(WideChar));
          id := ((ANode.ID - 1) shl 4) + cnt;
          sl.Add(IntToStr(id) + ': ' + tmp);
          Inc(ptr, l);
        end;
        Inc(cnt);
      end;

      AssignDataStream(nil, 0);
      sl.SaveToStream(DataStream);
      DataView.SetStream(DataStream, 0, DataStream.Size, hmTextFree);
    finally
      FreeMem(buf);
    end;
  finally
    sl.Free;
  end;
end;

procedure TResourcesFrame.LoadStringsData(
  ANode: TResNode; ASourceStream: TStream; AStrList: TStringList);
begin

end;

procedure TResourcesFrame.LoadVersionInfo(ANode: TResNode; AStream: TStream);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    LoadVersionInfoData(ANode, AStream, SL);
    AssignDataStream(nil, 0);
    SL.SaveToStream(DataStream);
    DataStream.Position := 0;
    FDataView.SetStream(DataStream, 0, DataStream.Size, hmTextFree);
  finally
    sl.Free;
  end;
end;

procedure TResourcesFrame.LoadVersionInfoData(
  ANode: TResNode; ASrcStream: TStream; AStrList: TStringList);

  function Round232(Val: Pointer): Pointer;
  begin
    SysUInt(Result) := SysUInt(Val) mod 4;
    if (Result = nil) then
      Result := Val
    else
      Result := Pointer(SysUInt(Val) - SysUInt(Result) + 4);
  end;

  function GetOSText(Val: SysUInt): string;
  begin
    Result := '';
    if (Val <> 0) then begin
      if (Val and VOS_DOS = VOS_DOS) then
        Result := Result + 'MS-DOS, ';
      if (Val and VOS_NT = VOS_NT) then
        Result := Result + 'Windows NT, ';
      if (Val and VOS__WINDOWS16 = VOS__WINDOWS16) then
        Result := Result + '16-bit Windows, ';
      if (Val and VOS__WINDOWS32 = VOS__WINDOWS32) then
        Result := Result + '32-bit Windows, ';
      if (Val and VOS_OS216 = VOS_OS216) then
        Result := Result + '16-bit OS/2, ';
      if (Val and VOS_OS232 = VOS_OS232) then
        Result := Result + '32-bit OS/2, ';
      if (Val and VOS__PM16 = VOS__PM16) then
        Result := Result + '16-bit Presentation Manager, ';
      if (Val and VOS__PM32 = VOS__PM32) then
        Result := Result + '32-bit Presentation Manager, ';

      if
        (Val and not (VOS_DOS or VOS_NT or VOS__WINDOWS16 or VOS__WINDOWS32 or
        VOS_OS216 or VOS_OS232 or VOS__PM16 or VOS__PM32) <> 0)
      then
        Result := Result + 'Unknown, ';
    end
    else
      Result := 'Undefined, ';

    Delete(Result, Length(Result) - 1, 2);
  end;

  function GetFileFlagsText(Val: SysUInt): string;
  const
    Txts: array[0..5] of string = (
      'Debug', 'PreRelease', 'Patched', 'Private Build', 'Info Inferred',
      'Special Build');
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to 5 do
      if ((Val shr I) and $1 = 1) then
        Result := Result + Txts[I] + ', ';
    if (Result <> '') then
      Delete(Result, Length(Result) - 1, 2);
  end;

  function GetFileTypeText(Val: SysUInt): string;
  const
    Txt: array[0..7] of string = (
      'Undefined', 'Application', 'DLL', 'Driver', 'Font', 'VXD', 'Unknown',
      'Static Link Library');
  begin
    if (Val > 7) then
      Val := 6;
    Result := Txt[Val];
  end;

  function GetFileSubTypeText(Val, FType: SysUInt): string;
  const
    Txt1: array[0..11] of string = (
      'Undefined', 'Printer driver', 'Keyboard driver', 'Language driver',
      'Display driver', 'Mouse driver', 'Network driver', 'System driver',
      'Installable driver', 'Sound driver', 'Communications driver',
      'Versioned printer driver');
    Txt2: array[1..3] of string = ('Raster font', 'Vector font', 'TrueType font');
  begin
    if (FType = VFT_DRV) then
      if (Val <= 11) then
        Result := Txt1[Val]
      else
        Result := 'Unknown'
    else if (FType = VFT_FONT) then
      if (Val <= 3) and (Val >= 1) then
        Result := Txt2[Val]
      else
        Result := 'Unknown'
    else if (Val = 0) then
      Result := ''
    else
      Result := 'Unknown';
  end;

type
  TStructHdr = packed record
    Len: Word;
    ValueLen: Word;
    wType: Word;
    FirstChar: Word;
  end;
  PStructHdr = ^TStructHdr;

  TFileVersion = packed record
    Minor: Word;
    Major: Word;
    Build: Word;
    Release: Word;
  end;
  PFileVersion = ^TFileVersion;

  procedure ReadStringFileInfo(Ptr: Pointer);

    procedure ReadString(Ptr: Pointer);
    var
      S: string;
      WS: string;
      PC: PWord;
      Hdr: PStructHdr;
    begin
      Hdr := Ptr;
      WS := '';
      PC := @Hdr.FirstChar;
      while (PC^ <> 0) do begin
        WS := WS + WideChar(PC^);
        Inc(PC);
      end;
      S := '    ' + string(WS) + ': ';
      Inc(PC);
      PC := Round232(PC);
      WS := '';
      while (PC^ <> 0) do begin
        WS := WS + WideChar(PC^);
        Inc(PC);
      end;
      S := S + TrimRight(WS);
      AStrList.Add(S);
    end;

    procedure ReadStringTable(Ptr: Pointer);
    var
      S: string;
      WS: string;
      Hdr, Hdr2: PStructHdr;
    begin
      Hdr := Ptr;
      SetLength(WS, 8);
      Move(Hdr.FirstChar, WS[1], 8*2);
      S := WS;

      AStrList.Add(
        '  StringTable 1 (0x' + Copy(S, 1, 4) + ', 0x' + Copy(S, 5, 4) +
        ') {'#13#10);
      Hdr2 := Pointer(SysUInt(Hdr) + 6 + 9*2);
      Hdr2 := Round232(Hdr2);
      while (Hdr2.ValueLen <> 0) do begin
        ReadString(Hdr2);
        Hdr2 := Round232(Pointer(SysUInt(Hdr2) + Hdr2.Len));
      end;
      AStrList.Add(#13#10'  }');
    end;

  begin
    AStrList.Add(#13#10'StringFileInfo {'#13#10);
    Inc(SysUInt(Ptr), 6 + 15*2);
    Ptr := Round232(Ptr);
    ReadStringTable(Ptr);
    AStrList.Add(#13#10'}');
  end;

  procedure ReadVarFileInfo(Ptr: Pointer);
  type
    TTranslation = packed record
      Len: Word;
      ValueLen: Word;
      wType: Word;
      Key: array[0..10] of WideChar;
      _Term, _Padding: Word;
      Lang: Word;
      CodePage: Word;
    end;
    PTranslation = ^TTranslation;

    procedure ReadTranslation(Ptr: Pointer);
    var
      T: PTranslation;
    begin
      T := Ptr;
      AStrList.Add('    0x' + IntToHex(T.Lang, 4) + ', 0x' + IntToHex(T.CodePage, 4));
    end;

  var
    Hdr: PStructHdr;
    I: SysUInt;
  begin
    Hdr := Ptr;
    AStrList.Add(#13#10'VarFileInfo {'#13#10);
    Inc(SysUInt(Ptr), 6 + 12*2);
    for I := 0 to (Hdr.Len - 6 - 12*2) div SizeOf(TTranslation) - 1 do
      Ptr := Pointer(SysUInt(Round232(Ptr)) + I * SizeOf(TTranslation));
    ReadTranslation(Ptr);
    AStrList.Add(#13#10'}');
  end;

var
  Buf, Ptr: Pointer;
  HDR: PStructHdr;
  FFI: PVSFixedFileInfo;
  WS: WideString;
  S: string;
  FV: PFileVersion;
begin
  GetMem(Buf, ANode.DataSize);
  try
    ASrcStream.Read(Buf^, ANode.DataSize);
    Ptr := Buf;
    Hdr := Ptr;
    if (Hdr.Len > ANode.DataSize) then
      raise Exception.Create('Invalid resource');
    SetLength(WS, 15);
    Move(Hdr.FirstChar, WS[1], 15 * Sizeof(WideChar));
    S := WS;
    if (S <> 'VS_VERSION_INFO') then
      raise Exception.Create('Invalid resource');
    Inc(SysUInt(Ptr), 6 + 16*2 {+ 4}); // (TStructHdr - 2) + WS#0 + {32 Align}
    Ptr := Round232(Ptr);
    FFI := Ptr;
    if (FFI.dwSignature <> $FEEF04BD) then
      raise Exception.Create('Invalid resource');
    //if (FFI.dwStrucVersion <> 0
    FV := @FFI.dwFileVersionMS;
    AStrList.Add('VS_FIXEDFILEINFO {'#13#10);
    AStrList.Add(
      '  File Version: ' + IntToStr(FV.Major) + '.' + IntToStr(FV.Minor) + '.' +
      IntToStr(FV.Release) + '.' + IntToStr(FV.Build));
    FV := @FFI.dwProductVersionMS;
    AStrList.Add(
      '  Product Version: ' + IntToStr(FV.Major) + '.' + IntToStr(FV.Minor) + '.' +
      IntToStr(FV.Release) + '.' + IntToStr(FV.Build));
    AStrList.Add('  File OS: ' + GetOSText(FFI.dwFileOS));
    AStrList.Add('  File Flags: ' + GetFileFlagsText(FFI.dwFileFlags and FFI.dwFileFlagsMask));
    AStrList.Add('  File Type: ' + GetFileTypeText(FFI.dwFileType));
    AStrList.Add('  File Subtype: ' + GetFileSubTypeText(FFI.dwFileSubtype, FFI.dwFileType));
    // Add FFI.datetime stamp
    AStrList.Add(#13#10'}');
    Ptr := Round232(Pointer(SysUInt(Ptr) + Hdr.ValueLen));

    Hdr := Ptr;
    SetLength(WS, 15);
    Move(Hdr.FirstChar, WS[1], 15*2);
    S := TrimRight(WS);

    if (S = 'StringFileInfo') then begin
      ReadStringFileInfo(Ptr);
      Inc(SysUInt(Ptr), Hdr.Len);
      Ptr := Round232(Ptr);
      Hdr := Ptr;
      SetLength(WS, 12);
      Move(Hdr.FirstChar, WS[1], 12*2);
      S := TrimRight(WS);
    end;
    if (S = 'VarFileInfo') then begin
      ReadVarFileInfo(Ptr);
    end;
  finally
    FreeMem(Buf);
  end;
end;

procedure TResourcesFrame.SafeClearImgGroups;
var
  i: Integer;
begin
  for i := 0 to lbImgGroup.Count - 1 do
    lbImgGroup.Items.Objects[i] := nil;
  lbImgGroup.Clear;
end;

procedure TResourcesFrame.SaveIcon(ANode: TResNode; AStream: TStream; AIsIcon: Boolean);

  type
    PIconDirEntry = ^TIconDirEntry;
    TIconDirEntry = packed record
      Width: Byte;
      Height: Byte;
      Colors: Byte;
      Reserved: Byte;
      Planes: Word;
      Bpp: Word;
      Size: DWORD;
      Offset: DWORD;
    end;

  TSourceHeader = packed record
    Hotspot: TSmallPoint;
    Header: TBitmapInfoHeader;
  end;

var
  srcStream: TFileStream;

  function readSourceHeader(ASingleItemNode: TResNode): TSourceHeader;
  begin
    if (ASingleItemNode <> nil) then
    begin
      srcStream.Position := ASingleItemNode.DataRaw;
      if (AIsIcon) then
      begin
        Cardinal(Result.Hotspot) := 0;
        srcStream.Read(Result.Header, SizeOf(Result.Header));
      end
      else
      begin
        srcStream.Read(Result, SizeOf(Result));
      end;

      if ((Result.Header.biHeight mod Result.Header.biWidth) = 0) then
        Result.Header.biHeight := Result.Header.biWidth;
    end;
  end;

  function checkIsPNG(ASingleItemNode: TResNode): Boolean;
  var
    test: TPngMagic;
  begin
    srcStream.Position := ASingleItemNode.DataRaw;
    srcStream.Read(test, SizeOf(test));
    Result := CompareMem(@test, @PNGMagic, SizeOf(PNGMagic));
  end;


var
  hdr: TGrpIconDir;
  dirs: array of TIconDirEntry;
  sources: array of TResNode;
  i, curDestOffset: Integer;
  srcHeader: TSourceHeader;
  buffer: Pointer;
  source: TResNode;
  gid: PGrpIconDir;
  ide: TGrpIconDirEntry;
  hotspotAdd: Cardinal;
  pngHeader: TIHDRChunk;
  sz: TPoint;
begin
  srcStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if (ANode.DataType in [RT_GROUP_ICON, RT_GROUP_CURSOR]) then
    begin
      GetMem(buffer, ANode.DataSize);
      try
        srcStream.Position := ANode.DataRaw;
        srcStream.Read(buffer^, ANode.DataSize);
        gid := buffer;
        SetLength(sources, gid.Count);
        SetLength(dirs, Length(sources));
        for i := 0 to gid.Count - 1 do
        begin
          ide := GID.Entries[i];
          source := SearchNode(ide.ID, ANode.DataType - DIFFERENCE, ANode.Lang, ResTreeRoot);
          if (source = nil) then
            raise Exception.Create('Error in icon saving');
          sources[i] := source;
        end;
      finally
        FreeMem(buffer);
      end;
    end
    else
    begin
      SetLength(sources, 1);
      sources[0] := ANode;
      SetLength(dirs, Length(sources));
    end;

    FillChar(hdr, SizeOf(hdr), 0);
    if (AIsIcon) then
      hdr.iType := 1
    else
      hdr.iType := 2;
    hdr.Count := Length(sources);
    AStream.Write(hdr, SizeOf(hdr) - SizeOf(TGrpIconDirEntry));

    curDestOffset := SizeOf(hdr) - SizeOf(TGrpIconDirEntry) + SizeOf(dirs[0]) * Length(dirs);
    if (AIsIcon) then
      hotspotAdd := 0
    else
      hotspotAdd := 4;

    for i := 0 to Length(sources) - 1 do
    begin
      source := sources[i];
      if (AIsIcon and checkIsPNG(source)) then
      begin
        GetMem(buffer, SizeOf(PNGMagic));
        try
          sz := obtainRealIconSize(source, srcStream, buffer, SizeOf(PNGMagic), True, pngHeader);
          with dirs[i] do
          begin
            if (pngHeader.Width < 256) then
              Width := pngHeader.Width
            else
              Width := 0;
            if (pngHeader.Height < 256) then
              Height := pngHeader.Height
            else
              Height := 0;
            if (pngHeader.BitDepth < 8) then
              Colors := 2 shl (pngHeader.BitDepth - 1) div 2
            else
              Colors := 0;
            Planes := 0; //srcHeader.biPlanes;
            Bpp := 0; //srcHeader.biBitCount;
            Size := source.DataSize - hotspotAdd;
            Offset := curDestOffset;
            Inc(curDestOffset, source.DataSize - hotspotAdd);
          end;
        finally
          FreeMem(buffer);
        end;
      end
      else
      begin
        srcHeader := readSourceHeader(sources[i]);
        with dirs[i] do
        begin
          if (srcHeader.Header.biWidth < 256) then
            Width := srcHeader.Header.biWidth
          else
            Width := 0;
          if (srcHeader.Header.biHeight < 256) then
            Height := srcHeader.Header.biHeight
          else
            Height := 0;
          if (AIsIcon) then
          begin
            if (srcHeader.Header.biBitCount < 8) then
              Colors := 2 shl (srcHeader.Header.biBitCount - 1) div 2
            else
              Colors := 0;
            Planes := 0; //srcHeader.biPlanes;
            Bpp := 0; //srcHeader.biBitCount;
          end
          else
          begin
            Colors := 0;
            Planes := srcHeader.Hotspot.x;
            Bpp := srcHeader.Hotspot.y;
          end;
          Size := source.DataSize - hotspotAdd;
          Offset := curDestOffset;
          Inc(curDestOffset, source.DataSize - hotspotAdd);
        end;
      end;
    end;

    AStream.Write(dirs[0], SizeOf(dirs[0]) * Length(dirs));

    for i := 0 to Length(sources) - 1 do
    begin
      source := sources[i];
      srcStream.Position := source.DataRaw + hotspotAdd;
      GetMem(buffer, source.DataSize - hotspotAdd);
      try
        srcStream.Read(buffer^, source.DataSize - hotspotAdd);
        AStream.Write(buffer^, source.DataSize - hotspotAdd);
      finally
        FreeMem(buffer);
      end;
    end;
  finally
    srcStream.Free;
  end;
end;

procedure TResourcesFrame.SearchMaxDepthAndSize;
var
  n: TResNode;
  i: Integer;
  mx, my, md, ix, id: Integer;
begin
  MX := 0;
  MY := 0;
  MD := 0;
  IX := -1;
  ID := 0;
  for I := 0 to lbImgGroup.Count - 1 do begin
    N := TResNode(lbImgGroup.Items.Objects[I]);
    if (N <> nil) then begin
      if (N.ImgX > MX) then begin
        MX := N.ImgX;
        MY := N.ImgY;
        IX := I;
      end;
    end;
  end;
  if (IX >= 0) then
    for I := 0 to lbImgGroup.Count - 1 do begin
      N := TResNode(lbImgGroup.Items.Objects[I]);
      if (N <> nil) then begin
        if (N.ImgX = MX) and (N.ImgY = MY) and (N.ImgColorDepth > MD) then begin
          MD := N.ImgColorDepth;
          ID := I;
        end;
      end;
    end;
  lbImgGroup.ItemIndex := ID;
end;

function TResourcesFrame.SearchNode(AID: Word; AResType, ALang: SysUInt; ARoot: TResNode): TResNode;
var
  i: Integer;
  subItem, resGroup, idGroup: TResNode;
begin
  Result := nil;
  resGroup := nil;
  idGroup := nil;
  // Search res type
  for i := 0 to ARoot.ChildrenCount - 1 do
  begin
    subItem := ARoot.Children[i];
    if (subItem.DataType = AResType) then
    begin
      resGroup := subItem;
      Break;
    end;
  end;

  if (resGroup <> nil) then
  begin
    // Search id node group
    for i := 0 to resGroup.ChildrenCount - 1 do
    begin
      subItem := resGroup.Children[i];
      if (subItem.ID = AID) then
      begin
        idGroup := subItem;
        Break;
      end;
    end;
  end;

  if (idGroup <> nil) then
  begin
    // Search language item
    if (idGroup.IsDirectory) then
    begin
      for i := 0 to idGroup.ChildrenCount - 1 do
      begin
        subItem := idGroup.Children[i];
        if (subItem.Lang = ALang) then
        begin
          Result := subItem;
          Break;
        end;
      end;
    end
    else
    begin
      Result := idGroup;
    end;
  end;
end;

procedure TResourcesFrame.SetCurGroupItem(AItem: Integer);
var
  node: TResNode;
  stm: TStream;
begin
  Node := Pointer(lbImgGroup.Items.Objects[AItem]);
  if Node = nil then
    Exit;

  stm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadSingleIcon(Node, stm, SingleIconData.IsIcon, not Boolean(SingleIconData.IsIcon));
  finally
    stm.Free;
  end;
end;

procedure TResourcesFrame.SetCurPage(const Value: TResViewType);
begin
  FCurPage := Value;
  lblError.Visible := False;
  case Value of
    rvImage:
    begin
      DataView.SetStream(nil, 0, 0, FDataView.Mode);
      pResText.Visible := False;
//      lbImgGroup.Visible := True;
      pResImg.Visible := True;
    end;

    rvDataView:
    begin
      DataView.SetStream(nil, 0, 0, FDataView.Mode);
      pResImg.Visible := False;
      lbImgGroup.Visible := False;
      pResText.Visible := True;
    end;
  end;
end;

procedure TResourcesFrame.SetCurResNode(AItem: TTreeNode; AMode: TViewMode);
var
  tree: TResNode;
  m: TViewMode;
  s, sLang, sCp, sId, sOffset, sSize, sIdName: string;
begin
  if (AItem = nil) then
    Exit;
  tree := AItem.Data;
  if not Tree.IsDirectory then
  begin
    CurResType := Tree.DataType;

    if not tree.Corrupted then
    begin
      try
        if (AMode = vmDefault) then
        begin
          m := ResType2Mode(CurResType);
          ShowNodeData(Tree, M, (M = vmBin));
        end
        else
        begin
          ShowNodeData(Tree, AMode, False);
          m := AMode;
        end;
        UpdateViewMenuState(m);

        aDataViewProcessed.Enabled := (ResType2Mode(CurResType) = vmProcessed);
        aDataCopy.Enabled := True;
        aDataSelectAll.Enabled := True;
        aDataSaveAs.Enabled := True;
        aDataViewText.Enabled := True;
        aDataViewBinaryText.Enabled := True;
        aDataViewHex.Enabled := True;
      except
        on e: Exception do
        begin
          tree.Corrupted := True;
          tree.CorruptedReason := e.Message;
          SetResourceError(e.Message);
        end;
      end;
    end
    else
    begin
      SetResourceError(tree.CorruptedReason);
    end;

    CurNode := Tree;
  end
  else
  begin
    pResText.Visible := False;
    pResImg.Visible := False;
    lbImgGroup.Visible := False;
  end;

  if (tree.IsDirectory) then
  begin
    s := '%s: %s';
    if (Tree.ID = 0) then
    begin
      sIdName := Strings.Name;
      sId := Tree.Name;
    end
    else
    begin
      sIdName := 'ID';
      sId := IntToStr(Tree.ID);
    end;
    edStatus.Text := Format(s, [sIdName, sId]);

    aDataCopy.Enabled := False;
    aDataSelectAll.Enabled := False;
    aDataSaveAs.Enabled := False;
    aDataViewProcessed.Enabled := False;
    aDataViewText.Enabled := False;
    aDataViewBinaryText.Enabled := False;
    aDataViewHex.Enabled := False;
  end
  else
  begin
    s := Strings.StatusFormat;
    if Tree.Lang <> 0 then
      sLang := GetLangString(Tree.Lang)
    else
      sLang := '0 (' + LangDesc(Tree.Lang) + ')';

    sOffset := MyIntToHex(Tree.DataRaw);
    sSize := IntToStr(Tree.DataSize);
    sCp := IntToStr(Tree.CodePage);
    if (Tree.ID = 0) then
    begin
      sIdName := Strings.Name;
      sId := Tree.Name;
    end
    else
    begin
      sIdName := 'ID';
      sId := IntToStr(Tree.ID);
    end;
    edStatus.Text := Format(s, [sLang, sCp, sIdName, sId, sOffset, sSize]);
  end;
end;

procedure TResourcesFrame.SetImgInfo(const AMsg: string);
begin
  lblImgInfo.Caption := AMsg;
end;

procedure TResourcesFrame.SetResourceError(const AMsg: string);
begin
  DataView.SetStream(nil, 0, 0, DataView.Mode);
  pResText.Visible := False;
  pResImg.Visible := False;
  lblError.Caption := AMsg;
  lblError.Font.Color := clRed;
  lblError.Visible := True;

  aDataCopy.Enabled := False;
  aDataSelectAll.Enabled := False;
  aDataSaveAs.Enabled := False;
  aDataViewProcessed.Enabled := False;
  aDataViewText.Enabled := False;
  aDataViewBinaryText.Enabled := False;
  aDataViewHex.Enabled := False;
end;

procedure TResourcesFrame.SetSplitter1Pos(Value: Integer);
begin
  if (Value > (Splitter1.Parent.ClientWidth - Splitter1.Width - Splitter1.MinSize)) then
    Value := Splitter1.Parent.ClientWidth - Splitter1.Width - Splitter1.MinSize;
  if (Value < Splitter1.MinSize) then
    Value := Splitter1.MinSize;
  tvTree.Width := Value;
end;

procedure TResourcesFrame.ShowNodeData(ANode: TResNode; AMode: TViewMode; ACheckText: Boolean);
var
  isIcon,
  isGroup,
  lbIsVisible: Boolean;
  stm: TStream;
begin
  if not ANode.Corrupted then
  begin
    try
      stm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      stm.Seek(ANode.DataRaw, soFromBeginning);
      try
        case AMode of
          vmBin:
          begin
            CurPage := rvDataView;
            LoadBinaryData(ANode, stm, hmTextBinary, ACheckText);
            //FFrm.EnabledMenuItems := Def_MIDataOnly;
          end;

          vmText:
          begin
            CurPage := rvDataView;
            LoadBinaryData(ANode, stm, hmTextFree, ACheckText);
            //FFrm.EnabledMenuItems := Def_MIDataOnly;
          end;

          vmHex:
          begin
            CurPage := rvDataView;
            LoadBinaryData(ANode, stm, hmHex, ACheckText);
            //FFrm.EnabledMenuItems := Def_MIDataOnly;
          end;

          vmProcessed:
          begin
            lbIsVisible := False;
            //FFrm.lstImgGroup.Visible := False;
            SafeClearImgGroups;
            case ANode.DataType of
              RT_ICON, RT_GROUP_ICON, RT_CURSOR, RT_GROUP_CURSOR:
              begin
                CurPage := rvImage;
                IsIcon := ANode.DataType in [ RT_ICON, RT_GROUP_ICON ];
                IsGroup := ANode.DataType in [ RT_GROUP_ICON, RT_GROUP_CURSOR ];
                FSingleIconData.IsIcon := IsIcon;
                lbIsVisible := lbIsVisible or IsGroup;
    //            lbImgGroup.Visible := lbIsVisible;
                lbImgGroup.Clear;
                if IsGroup then
                  LoadIconGroup(ANode, stm, IsIcon)
                else
                  LoadSingleIcon(ANode, stm, IsIcon, True);
              end;

              RT_BITMAP:
              begin
                CurPage := rvImage;
                LoadBitmap(ANode, stm);
              end;

              RT_STRING:
              begin
                CurPage := rvDataView;
                LoadStrings(ANode, stm);
              end;

              RT_VERSION:
              begin
                CurPage := rvDataView;
                LoadVersionInfo(ANode, stm);
              end;

              RT_MANIFEST:
              begin
                CurPage := rvDataView;
                LoadStrings(ANode, stm);
              end;
            end;
            lbImgGroup.Visible := lbIsVisible;
      //      CheckedMenuItem := miProcessed;
            //FFrm.EnabledMenuItems := Def_MIProc;
          end;
        end;
      finally
        stm.Free;
      end;
    except
      on e: Exception do
      begin
        ANode.Corrupted := True;
        ANode.CorruptedReason := e.Message;
        SetResourceError(e.Message);
      end;
    end;
  end
  else
    SetResourceError(ANode.CorruptedReason);
end;

procedure TResourcesFrame.tvTreeChange(Sender: TObject; Node: TTreeNode);
begin
  SetCurResNode(Node, vmDefault);
end;

procedure TResourcesFrame.tvTreeClick(Sender: TObject);
var
  node: TTreeNode;
  p: TPoint;
  ht: THitTests;
begin
  p := tvTree.ScreenToClient(Mouse.CursorPos);
  node := tvTree.GetNodeAt(p.X, p.Y);
  ht := tvTree.GetHitTestInfoAt(p.X, p.Y);
  if (node <> nil) and (node.HasChildren) and ((htOnItem in ht) or (htOnIcon in ht)) then
  begin
    if (node.Expanded) then
      node.Collapse(False)
    else
      node.Expand(False);
  end;
end;

procedure TResourcesFrame.tvTreeCollapsed(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  Node.StateIndex := 1;
end;

procedure TResourcesFrame.tvTreeContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  node: TTreeNode;
begin
  node := tvTree.GetNodeAt(MousePos.X, MousePos.Y);
  if (node <> nil) then
    tvTree.Selected := node;
end;

procedure TResourcesFrame.tvTreeExpanded(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  Node.StateIndex := 2;
end;

procedure TResourcesFrame.UnloadResources;
begin
  tvTree.Items.Clear;
  SafeClearImgGroups;
  FreeResTree(ResTreeRoot);
  ResTreeRoot := nil;
  CurNode := nil;
end;

procedure TResourcesFrame.UpdateViewMenuState(AMode: TViewMode);
var
  map: array[TViewMode] of TCustomAction;
begin
  map[vmDefault] := nil;
  map[vmBin] := aDataViewBinaryText;
  map[vmText] := aDataViewText;
  map[vmHex] := aDataViewHex;
  map[vmProcessed] := aDataViewProcessed;
  if (map[AMode] <> nil) then
    map[AMode].Checked := True;
end;

procedure TResourcesFrame.pmDataPopup(Sender: TObject);
var
  flag: Boolean;
begin
  flag := not DataView.Empty;
  aDataCopy.Enabled := flag;
  aDataSelectAll.Enabled := flag;
end;


{ TResourcesStringStore }


constructor TResourcesStringStore.Create;
begin
  inherited;
  ImageCaptionFormat := '%d x %d - %d-bit colors (%d)';
  StatusFormat := 'Lang: %s, Code Page: %s, %s: %s, Offset: %s, Size: %s';
end;

end.


