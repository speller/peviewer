unit PEUtils;

interface
uses
  Winapi.Windows, Classes, SysUtils, PETypes, CommonUtilities, System.Win.Registry, PEHdrUtils,
  System.Generics.Collections, Winapi.ShlObj, WinApiWorks;


type


  TPeImportKind = (ikImport, ikDelayImport{, ikBoundImport});

  TValidState = (vsUnknown, vsValid, vsPartialValid, vsInvalid);

  TMachineType = (mtUnknown, mt32, mt64);



  EImageNotSupported = class(Exception);


  TFileMapper = class
  private
    FFileHandle : THandle;
    FMapHandle: THandle;
    FFileSize: UInt64;
    FMap: Pointer;
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    property Map: Pointer read fMap;
    property FileSize: UInt64 read FFileSize;
    property FileName: string read fFileName;
    procedure LoadFile(const AFileName: string);
    procedure UnloadFile;
  end;



  TBasePEItem = class(TCollectionItem)
  private
    FName: string;
    FTag: Integer;
  public
    constructor Create(const AName: string; ACollection: TCollection); reintroduce; virtual;
    property Name: string read FName write FName;
    property Tag: Integer read FTag write FTag;
  end;


  TBasePECollection = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItemByName(const AName: string): TBasePEItem;
  protected
    function GetOwner: TPersistent; override;
  public
    property ItemByName[const AName: string]: TBasePEItem read GetItemByName; default;

    procedure AddItems(ACollection: TCollection);
    function IndexOfName(const AName: string): Integer;
  end;



  TExportItem = class(TBasePEItem)
  private
    FHint: SysUInt;
    FAddr: SysUInt;
    FOrdinal: SysUInt;
    FRawOffset: SysUInt;
  public
    property Ordinal: SysUInt read FOrdinal write FOrdinal;
    property Hint: SysUInt read FHint write FHint;
    property Address: SysUInt read FAddr write FAddr;
    property RawOffset: SysUInt read FRawOffset write FRawOffset;
  end;


  TImportLib = class;


  TImportItem = class(TBasePEItem)
  private
    FHint: SysUInt;
    FModule: string;
    FLib: TImportLib;
    FValid: TValidState;
    FOrdinal: SysUInt;
    FDisplayName: string;
  public
//    constructor Create(const AName: string; ACollection: TCollection); override;
    property Module: string read FModule write FModule;
    property Hint: SysUInt read FHint write FHint;
    property Ordinal: SysUInt read FOrdinal write FOrdinal;
    property Lib: TImportLib read FLib write FLib;
    property ValidationState: TValidState read FValid write FValid;
    property DisplayName: string read FDisplayName write FDisplayName;

    procedure Assign(Source: TPersistent); override;
  end;


  TPEImage = class;


  TImportLib = class(TBasePEItem)
  private
    FThunk: Pointer;
    FImage: TPEImage;
    FSec: PImageSectionHeader;
    FImportKind: TPEImportKind;
    FValid: TValidState;
    FImportDescriptor: Pointer;
    FFunctions: TBasePECollection;
    procedure LoadData;
  public
    constructor Create(const AName: string; ACollection: TCollection); override;
    destructor Destroy; override;

    property ImportDescriptor: Pointer read FImportDescriptor write FImportDescriptor;
    property ImportKind: TPEImportKind read FImportKind write FImportKind;
    property Functions: TBasePECollection read FFunctions write FFunctions;
    property ValidationState: TValidState read FValid write FValid;
    property Image: TPEImage read FImage write FImage;
  end;



  TSectionItem = class(TBasePEItem)
  private
    FHasEP: Boolean;
    FRawSize: SysUInt;
    FVirtualSize: SysUInt;
    FVirtualAddress: SysUInt;
    FRawOffset: SysUInt;
    FFlags: SysUInt;
  public
    property VirtualAddress: SysUInt read FVirtualAddress write FVirtualAddress;
    property RawOffset: SysUInt read FRawOffset write FRawOffset;
    property VirtualSize: SysUInt read FVirtualSize write FVirtualSize;
    property RawSize: SysUInt read FRawSize write FRawSize;
    property Flags: SysUInt read FFlags write FFlags;
    property HasEP: Boolean read FHasEP write FHasEP;
  end;



  TResNode = class(TBasePEItem)
  private
    FSubDirs: TBasePECollection;
    FIsRoot: Boolean;
    FIsDirectory: Boolean;
    FDataRaw: SysUInt;
    FDataType: SysUInt;
    FID: Word;
    FDataSize: SysUInt;
    FLang: SysUInt;
    FParent: TResNode;
    FImgColorDepth: Byte;
    FImgY: Integer;
    FImgX: Integer;
    FDisplayName: string;
    fCodePage: SysUInt;
    FCorrupted: Boolean;
    FCorruptedReason: string;
    function GetChildren(Index: Integer): TResNode;
    function GetChildrenCount: Integer;
  public
    constructor Create(const AName: string; AParent: TResNode); reintroduce;
    destructor Destroy; override;
  public
    property Parent: TResNode read fParent write fParent;
    property IsRoot: Boolean read fIsRoot;
    property IsDirectory: Boolean read fIsDirectory write fIsDirectory;
    property SubDirs: TBasePECollection read FSubDirs;
    property DataRaw: SysUInt read FDataRaw write FDataRaw;
    property DataSize: SysUInt read FDataSize write FDataSize;
    property DataType: SysUInt read FDataType write FDataType;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Lang: SysUInt read FLang write Flang;
    property CodePage: SysUInt read FCodePage write fCodePage;
    property ID: Word read FID write FID;
    property ImgX: Integer read FImgX write FImgX;
    property ImgY: Integer read FImgY write FImgY;
    property ImgColorDepth: Byte read FImgColorDepth write FImgColorDepth;
    property Children[Index: Integer]: TResNode read GetChildren;
    property ChildrenCount: Integer read GetChildrenCount;
    property Corrupted: Boolean read FCorrupted write FCorrupted;
    property CorruptedReason: string read FCorruptedReason write FCorruptedReason;
  end;



  TPEImageHeaderDirectoryList = packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  PPEImageHeaderDirectoryList = ^TPEImageHeaderDirectoryList;


  TPEImage = class
  private
    FMapper: TFileMapper;
    FImList: TBasePECollection; // import libraries
    FExList: TBasePECollection;
    FRawSections: TList<UInt64>; // raw section headers
    FSimpleExportList: TStringList; // simple list of exports for validity check
    FCorrupted: Boolean;
    FSections: TBasePECollection;
    FImageLoadCount: Integer;
    FEPExists: Boolean;
    FResourceTree: TResNode;
    FVersionResource: TResNode;
    FImageType: Integer;
    FMachineType: TMachineType;
    function GetSect(Idx: Integer): PImageSectionHeader;
    function GetImageTypeText: string;
    function GetImageTypeSupported: Boolean;
    function GetRawSectionCount: Integer;
    function GetImageType: Integer;
    function GetMachineType: TMachineType;
  protected
    function GetImageNTHeader: Pointer; virtual; abstract;
    procedure SetImageNTHeader(AHeader: Pointer); virtual; abstract;
    function GetSizeOfNTHeader: Integer; virtual; abstract;

    function GetNTOptionalHeaderDataDirectory: PPEImageHeaderDirectoryList; virtual; abstract;
    function GetNTFileHeaderNumberOfSections: UInt; virtual; abstract;
    function GetNTOptionalHeaderAddressOfEntryPoint: UInt64; virtual; abstract;
    procedure CheckImageType;
    procedure SafeClearSimpleExportList;
  public
    class function CreateImage(const AFileName: string): TPEImage;
    constructor Create(const AFileName: string); reintroduce;
    destructor Destroy; override;

    property ExportList: TBasePECollection read fExList;
    property ImportList: TBasePECollection read fImList;
    property SectionList: TBasePECollection read FSections;
    property ResourceTree: TResNode read fResourceTree;
    property VersionResource: TResNode read FVersionResource;
    property SimpleExportList: TStringList read FSimpleExportList;
    property ImageLoadCount: Integer read fImageLoadCount;
    property Mapper: TFileMapper read fMapper;
    property RawSections[Idx: Integer]: PImageSectionHeader read GetSect;
    property RawSectionCount: Integer read GetRawSectionCount;
    property ImageTypeText: string read GetImageTypeText;
    property ImageTypeSupported: Boolean read GetImageTypeSupported;
    property ImageType: Integer read GetImageType;
    property MachineType: TMachineType read GetMachineType;
    property Corrupted: Boolean read fCorrupted;
    property ImageNTHeader: Pointer read GetImageNTHeader write SetImageNTHeader;

    procedure LoadImExData;
    procedure LoadSectionData;
    procedure LoadSimpleExportInfo;
    procedure LoadResourceTree;
    procedure LoadVersionInfoResource;
    function GetEntryPointRAW(var SecIdx: Integer): UInt64; virtual; abstract;
    property EntryPointExists: Boolean read fEPExists;
    procedure LoadImage; virtual;
    procedure UnloadImage; virtual;
    function VA2Raw(VirtualAddress: UInt64; ASecList: TList<UInt64>; var SecIdx: Integer): UInt64;
    function RVA2Raw(VirtualAddress: UInt64; SecHdr: PImageSectionHeader ): UInt64;
    procedure LoadCommonData;

    function FormatHex(AValue: UInt64): string; virtual; abstract;
    function RVA2RawEx(VirtualAddress: UInt64; SecHdr: PImageSectionHeader): UInt64; virtual; abstract;
  end;


  TPEImage32 = class(TPEImage)
  private
    FImageNTHeader: PImageNTHeaders32;
  protected
    function GetImageNTHeader: Pointer; override;
    procedure SetImageNTHeader(AHeader: Pointer); override;
    function GetSizeOfNTHeader: Integer; override;

    function GetNTOptionalHeaderAddressOfEntryPoint: UInt64; override;
    function GetNTOptionalHeaderDataDirectory: PPEImageHeaderDirectoryList; override;
    function GetNTFileHeaderNumberOfSections: UInt; override;
  public
    property ImageNTHeader: PImageNTHeaders32 read FImageNTHeader;

    function RVA2RawEx(VirtualAddress: UInt64; SecHdr: PImageSectionHeader): UInt64; override;
    function GetEntryPointRAW(var SecIdx: Integer): UInt64; override;
    function FormatHex(AValue: UInt64): string; override;
  end;


  TPEImage64 = class(TPEImage)
  private
    FImageNTHeader: PImageNTHeaders64;
    {$IFNDEF WIN64}
    FWow64Temp: SysUInt;
    {$ENDIF}
  protected
    function GetImageNTHeader: Pointer; override;
    procedure SetImageNTHeader(AHeader: Pointer); override;
    function GetSizeOfNTHeader: Integer; override;

    function GetNTOptionalHeaderDataDirectory: PPEImageHeaderDirectoryList; override;
    function GetNTFileHeaderNumberOfSections: UInt; override;
    function GetNTOptionalHeaderAddressOfEntryPoint: UInt64; override;
  public
    property ImageNTHeader: PImageNTHeaders64 read FImageNTHeader;

    procedure LoadImage; override;
    procedure UnloadImage; override;

    function RVA2RawEx(VirtualAddress: UInt64; SecHdr: PImageSectionHeader): UInt64; override;
    function GetEntryPointRAW(var SecIdx: Integer): UInt64; override;
    function FormatHex(AValue: UInt64): string; override;
  end;


  TDWORDArray = array[ 0..8191 ] of DWORD;
  PDWORDArray = ^TDWORDArray;

  TWORDArray = array[ 0..15883 ] of WORD;
  PWORDArray = ^TWORDArray;

  TByteArray = array[ 0..15883 ] of Byte;
  PByteArray = ^TByteArray;


function PEImageLoadLibrary(const AFileName: string; AIs64: Boolean): TPEImage;

function GetFileType(Base: Pointer; FileSize: UInt64): UInt64;
function GetNTHeaders( Base: Pointer ): PImageNtHeaders32;
function ImageType2String(IT: SysUInt): string;
procedure FreeResTree(Root: TResNode);



implementation

function GetFileSizeEx(hFile: THandle; var lpFileSize: UInt64): BOOL; stdcall; external kernel32;



const
  EXE_TYPE_DOS16 = $100;
  ENV_BUFFER_SIZE = 10000;
  NATIVE64 = {$IFDEF WIN64}True{$ELSE}False{$ENDIF};


function GetFileType(Base: Pointer; FileSize: UInt64): UInt64;
var
  NTSigPtr: ^SysUInt;
  DosHdr: PImageDosHeader;
begin
  Result := 0;
  DosHdr := Base;
  if PWORD( Base )^ <> $5A4D then Exit;
  if {(DosHdr._lfanew <= $40) or}
    (SysUInt(DosHdr._lfanew) > FileSize - 2) then
  begin
    Result := EXE_TYPE_DOS16;
    Exit;
  end;
  NTSigPtr := Pointer(Integer(Base) + DosHdr._lfanew);


  case (NTSigPtr^ and $0000FFFF) of
    IMAGE_DOS_SIGNATURE: Result := IMAGE_DOS_SIGNATURE;
    IMAGE_OS2_SIGNATURE: Result := IMAGE_OS2_SIGNATURE;
    IMAGE_OS2_SIGNATURE_LE: Result := IMAGE_OS2_SIGNATURE_LE;
    IMAGE_NT_SIGNATURE: Result := IMAGE_NT_SIGNATURE;
  end;
end;


function GetNTHeaders(Base: Pointer): PImageNtHeaders32;
begin
  Inc(PByte(Base), PImageDosHeader(Base)._lfanew);
  Result := Base;
end;


function ImageType2String( IT: SysUInt ): string;
begin
  case IT of
    IMAGE_DOS_SIGNATURE: Result := 'MZ';
    IMAGE_OS2_SIGNATURE: Result := 'NE';
    IMAGE_OS2_SIGNATURE_LE: Result := 'LE';
    IMAGE_NT_SIGNATURE: Result := 'PE';
    EXE_TYPE_DOS16: Result := '16-bit DOS';
    else
      Result := 'UNKNOWN';
  end;
end;


procedure FreeResTree( Root: TResNode );
var
  I: Integer;
begin
  if Root <> nil then
  begin
    if Root.SubDirs.Count = 0 then
      Root.Free
    else
      for I := 0 to Root.SubDirs.Count - 1 do
        TResNode(Root.SubDirs.Items[ I ]).Free;
  end;
end;

{$WARNINGS OFF}
function Wow64DisableWow64FsRedirection(out OldValue: SysUInt): BOOL; stdcall; external 'kernel32.dll' delayed;
function Wow64RevertWow64FsRedirection(const OldValue: SysUInt): BOOL; stdcall; external 'kernel32.dll' delayed;
{$WARNINGS ON}

// Load specified library and search the location of it
// like windows does (LoadLibrary), but we do not need entry point
// calls and other preparations.
// Warning: NT only 16-bit system directory is not included.

function PEImageLoadLibrary(const AFileName: string; AIs64: Boolean): TPEImage;
const
  imgTypes: array[Boolean] of TMachineType = (mt32, mt64);


  function getSystemDir: string;
  begin
    Result := '';
    {if (NATIVE64 and AIs64) or (not NATIVE64 and not AIs64) then
    begin
      SetLength(Result, MAX_PATH);
      SetLength(Result, GetCurrentDirectory(MAX_PATH, @Result[1]));
    end
    else if (not NATIVE64 and AIs64) then
    begin
      SetLength(Result, MAX_PATH);
      SetLength(Result, GetCurrentDirectory(MAX_PATH, @Result[1]));
    end
    else} if (NATIVE64 and not AIs64) then
    begin
      SetLength(Result, MAX_PATH);
      if (SHGetSpecialFolderPath(0, @Result[1], $29, False)) then
        SetLength(Result, StrLen(PWideChar(@Result[1])))
      else
        Result := '';
    end;
    if (Result = '') then
    begin
      SetLength(Result, MAX_PATH);
      SetLength(Result, GetSystemDirectory(@Result[1], MAX_PATH));
    end;
  end;

  function checkImage(const AFN: string; var AResult: TPEImage): Boolean;
  var
    img: TPEImage;
    {$IFNDEF WIN64}
    tmp: SysUInt;
    {$ENDIF}
  begin
    Result := False;
    {$IFNDEF WIN64}
    try
      if AIs64 and IsOnWow then
        Wow64DisableWow64FsRedirection(tmp);
    {$ENDIF}
      if FileExists(AFN) then
      begin
        try
          img := TPEImage.CreateImage(AFN);
          try
            img.LoadImage;
            try
              if (img.MachineType = imgTypes[AIs64]) then
                AResult := img;
            finally
              img.UnloadImage;
            end;
          finally
            if (AResult = nil) then
              img.Free;
          end;
          Result := (AResult <> nil);
        except
        end;
      end;
    {$IFNDEF WIN64}
    finally
      if AIs64 and IsOnWow then
        Wow64RevertWow64FsRedirection(tmp);
    end;
    {$ENDIF}
  end;

var
  s, winDir, curDir, envDir, envDirs, curLib: string;
  len, i: Integer;
  vl: TStringList;
  r: TRegistry;
begin
  Result := nil;
  {$IFNDEF WIN64}
  // если смотрим 64-битный бинарник, но процесс 32 бита и работает на 32-битной системе, то ничего не загружаем
  if AIs64 and not IsOnWow then
    Exit;
  {$ENDIF}

  // 1-st step
  if checkImage(AFileName, Result) then
    Exit;

  if Pos( '\', AFileName ) > 0 then // FileName contains a path - stop searching
  begin
    Result := nil;
    Exit;
  end;

  // 2-nd step
  SetLength(curDir, MAX_PATH);
  SetLength(curDir, GetCurrentDirectory(MAX_PATH, @curDir[1]));
  if checkImage(GluePath([curDir, AFileName]), Result) then
    Exit;

  // 3-rd step
  if checkImage(GluePath([getSystemDir, AFileName]), Result) then
    Exit;


  // 4-th step
  // 16-bit system directory not included

  // 5-th step
  SetLength( WinDir, MAX_PATH + 1 );
  Len := GetWindowsDirectory( @WinDir[ 1 ], MAX_PATH + 1 );
  SetLength( WinDir, Len );
  WinDir := IncludeTrailingPathDelimiter( WinDir );
  S := WinDir + AFileName;
  if checkImage(s, Result) then
    Exit;

  // 6-th step
  SetLength(envDirs, ENV_BUFFER_SIZE);
  SetLength(envDirs, GetEnvironmentVariable('PATH', @EnvDirs[1], ENV_BUFFER_SIZE));
  envDir := Parse(envDirs, ';');
  while (envDirs <> '') do
  begin
    s := GluePath([envDir, AFileName]);
    if checkImage(s, Result) then
      Exit;
    envDir := Parse(envDirs, ';');
  end;

  // Loader checks this registry entry too. "7" step
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    if (r.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\SharedDLLs')) then
    begin
      VL := TStringList.Create;
      r.GetValueNames(VL);
      for I := 0 to VL.Count - 1 do
      begin
        CurLib := VL[ I ];
        S := ExtractFileName( CurLib );
        if AnsiSameText(s, AFileName) then
        begin
          if checkImage(curLib, Result) then
            Exit;
        end;
      end;
      VL.Free;
    end;
  finally
    r.Free;
  end;

  Result := nil;
end;


{ TFileMapper }


constructor TFileMapper.Create(const AFileName: string);
begin
  FFileName := AFileName;
end;

destructor TFileMapper.Destroy;
begin
  UnloadFile;
  inherited;
end;


procedure TFileMapper.LoadFile(const AFileName: string);
begin
  if (FMap = nil) then
  begin
    FFileHandle := CreateFile(
      PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if (FFileHandle = INVALID_HANDLE_VALUE) then
      RaiseLastOSError;
    try
      GetFileSizeEx(FFileHandle, FFileSize);
      FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, 0, 0, nil);
      if (FMapHandle = 0) then
        RaiseLastOSError;
      try
        FMap := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, 0);
        if (FMap = nil) then
          RaiseLastOSError;

        FFileName := AFileName;
      except
        CloseHandle(FMapHandle);
        FMapHandle := 0;
        raise;
      end;
    except
      CloseHandle(FFileHandle);
      FFileHandle := 0;
      raise;
    end;
  end;
end;


procedure TFileMapper.UnloadFile;
begin
  if (FMap <> nil) then
  begin
    UnmapViewOfFile(FMap);
    CloseHandle(FMapHandle);
    CloseHandle(FFileHandle);
    FMap := nil;
    FFileHandle := 0;
    FMapHandle := 0;
  end;
end;


{ TPEImage }


function TPEImage.GetImageTypeText: string;
var
  hdr: PImageNtHeaders32;
  ft: Word;
begin
  if (Mapper.Map <> nil) then
  begin
    ft := GetFileType(Mapper.Map, Mapper.FileSize);
    Result := ImageType2String(ft);
    if (ft = IMAGE_NT_SIGNATURE) then
    begin
      hdr := ImageNTHeader;
      Result := Result + ', ' + GetImageMachineType(hdr.FileHeader.Machine);
    end;
  end
  else
    Result := '';
end;


function TPEImage.GetMachineType: TMachineType;
var
  it: Integer;
begin
  if (FMachineType = mtUnknown) then
  begin
    Result := mtUnknown;
    it := GetImageType;
    if (it = IMAGE_FILE_MACHINE_I386) then
      Result := mt32
    else if (it = IMAGE_FILE_MACHINE_AMD64) then
      Result := mt64;
    FMachineType := Result;
  end
  else
    Result := FMachineType;
end;

function TPEImage.VA2Raw(VirtualAddress: UInt64; ASecList: TList<UInt64>; var SecIdx: Integer): UInt64;
var
  I: Integer;
  Pos: SysUInt;
  CurSect: PImageSectionHeader;
begin
  Result := 0;
  SecIdx := -1;
  for I := 0 to ASecList.Count - 1 do
  begin
    CurSect := Pointer(ASecList.Items[I]);
    if
      (CurSect.VirtualAddress <= VirtualAddress) and
//      (VirtualAddress < CurSect.VirtualAddress + CurSect.SizeOfRawData)
      (VirtualAddress < CurSect.VirtualAddress + CurSect.Misc.VirtualSize)
    then
    begin
      Pos := VirtualAddress - CurSect.VirtualAddress;
      Result := CurSect.PointerToRawData + Pos;
      SecIdx := I;
      Break;
    end;
  end;
end;


function TPEImage.RVA2Raw(VirtualAddress: UInt64; SecHdr: PImageSectionHeader ): UInt64;
begin
  Result := VirtualAddress - SecHdr.VirtualAddress + SecHdr.PointerToRawData;
end;

procedure TPEImage.SafeClearSimpleExportList;
var
  i: Integer;
begin
  if (SimpleExportList <> nil) then
  begin
    for i := 0 to SimpleExportList.Count - 1 do
      SimpleExportList.Objects[i] := nil;
    SimpleExportList.Clear;
  end;
end;

function TPEImage.GetImageType: Integer;
var
  hdr: PImageNtHeaders32;
begin
  if (FMapper.Map <> nil) then
  begin
  Result := -1;
    if (GetFileType(Mapper.Map, Mapper.FileSize) = IMAGE_NT_SIGNATURE) then
    begin
      hdr := ImageNTHeader;
      Result := hdr.FileHeader.Machine;
      FImageType := Result;
    end;
  end
  else
    Result := FImageType;
end;

function TPEImage.GetImageTypeSupported: Boolean;
var
  hdr: PImageNtHeaders32;
begin
  if (FMapper.Map <> nil) then
  begin
    Result := (GetFileType(Mapper.Map, Mapper.FileSize) = IMAGE_NT_SIGNATURE);
    if (Result) then
    begin
      hdr := ImageNTHeader;
      Result :=
        (hdr.FileHeader.Machine = IMAGE_FILE_MACHINE_I386) or
        (hdr.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64);
    end;
  end
  else
    Result := False;
end;


function TPEImage.GetRawSectionCount: Integer;
begin
  Result := FRawSections.Count;
end;

function TPEImage.GetSect(Idx: Integer): PImageSectionHeader;
begin
  Result := Pointer(FRawSections.Items[Idx]);
end;

procedure TPEImage.LoadCommonData;
var
  i: Integer;
  hdr: Pointer;
begin
  hdr := ImageNTHeader;
  if (FRawSections = nil) then
    FRawSections := TList<UInt64>.Create
  else
    FRawSections.Clear;

  for I := 0 to GetNTFileHeaderNumberOfSections - 1 do
  begin
    FRawSections.Add(UInt64(hdr) + GetSizeOfNTHeader + I * SizeOf(TImageSectionHeader));
  end;

  FEPExists := (GetNTOptionalHeaderAddressOfEntryPoint > 0);
end;


procedure TPEImage.CheckImageType;
begin
  if (not ImageTypeSupported) then
    raise EImageNotSupported.Create(ImageTypeText + ': unsupported');
end;

constructor TPEImage.Create(const AFileName: string);
begin
  inherited Create;
  FMapper := TFileMapper.Create(AFileName);
  FImageType := -1;
end;

class function TPEImage.CreateImage(const AFileName: string): TPEImage;
var
  stm: TFileStream;
  buffer: Pointer;
  sz: Integer;
  hdr: PImageNtHeaders32;
begin
  Result := nil;
  // Preload image to determine its type and create object of approriate class
  stm := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    sz := SizeOf(TImageDosHeader) + SizeOf(TImageNtHeaders64) + 2048;
    GetMem(buffer, sz);
    try
      stm.Read(buffer^, sz);
      if (GetFileType(buffer, sz) <> IMAGE_NT_SIGNATURE) then
        raise Exception.Create('Image is not PE image');

      hdr := GetNTHeaders(buffer);
      if (hdr.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64) then
        Result := TPEImage64.Create(AFileName)
      else
        Result := TPEImage32.Create(AFileName);
    finally
      FreeMem(buffer);
    end;
  finally
    stm.Free;
  end;
  if (Result = nil) then
    raise Exception.Create('WTF???');
end;

destructor TPEImage.Destroy;
begin
  FImList.Free;
  FExList.Free;
  FSections.Free;
  SafeClearSimpleExportList;
  SimpleExportList.Free;
  FRawSections.Free;
  FMapper.Free;
  FResourceTree.Free;
  FVersionResource.Free;
  inherited;
end;


procedure TPEImage.LoadImage;
begin
  Mapper.LoadFile(Mapper.FileName);
  Inc(FImageLoadCount);
  ImageNTHeader := GetNTHeaders(Mapper.Map);
  GetMachineType;
  GetImageType;
end;


procedure TPEImage.LoadImExData;
var
  i: Integer;
  dataDir: TImageDataDirectory;
  expDir: PImageExportDirectory;
  imgBase: SysUInt;
  j: SysUInt;
  sec: PImageSectionHeader;
  names: PDWORDArray;
  addresses: PDWORDArray;
  ordinals: PWORDArray;
  rawPtr: SysUInt;
  secIdx: Integer;
  importDesc: PImageImportDescriptor;
  delayImportDesc: PImgDelayDescr;
  nam: PAnsiChar;
  libItem: TImportLib;
  libI, libJ: TImportLib;
  exportItem: TExportItem;

  funcVA, funcRaw: Integer;
  is64: Boolean;
begin
  CheckImageType;
  LoadCommonData;

  ImgBase := SysUInt(Mapper.Map);

  fCorrupted := False;
  if fExList = nil then
    fExList := TBasePECollection.Create(TExportItem);
  if fImList = nil then
    fImList := TBasePECollection.Create(TImportLib);

  is64 := Self is TPEImage64;

  // ---- Export ----

  ExportList.Clear;

  try
    DataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];

    if DataDir.VirtualAddress <> 0 then
    begin
      RawPtr := VA2Raw( DataDir.VirtualAddress, FRawSections, SecIdx );
      if RawPtr <> 0 then
      begin
        Sec := Pointer(FRawSections.Items[SecIdx]);
        ExpDir := Pointer( ImgBase + RawPtr );
        Names := Pointer( RVA2Raw( ExpDir.AddressOfNames, Sec ) +
            ImgBase );
        Addresses := Pointer( RVA2Raw( ExpDir.AddressOfFunctions, Sec ) +
            ImgBase );
        Ordinals := Pointer( RVA2Raw( ExpDir.AddressOfNameOrdinals, Sec ) +
            ImgBase );
        for I := 0 to ExpDir.NumberOfNames - 1 do
        begin
          nam := PAnsiChar(RVA2Raw(Names[I], Sec ) + ImgBase);
          FuncVA := Addresses[ Ordinals[ I ] ];
          FuncRaw := RVA2Raw( FuncVA, Sec );
//          FuncRaw := VA2Raw( FuncVA, fSList, SecIdx );
          exportItem := TExportItem(FExList.Add);
          exportItem.Name := string(nam);
          exportItem.Ordinal := Ordinals[ I ] + ExpDir.Base;
          exportItem.Hint := I;
          exportItem.Address := FuncVA;
          exportItem.RawOffset := FuncRaw;
        end;
      end;
    end;
  except
    fCorrupted := True;
  end;

  // ---- Import ----

  ImportList.Clear;

  try
    DataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
    if DataDir.VirtualAddress <> 0 then
    begin
      RawPtr := VA2Raw( DataDir.VirtualAddress, FRawSections, SecIdx );
      if RawPtr <> 0 then
      begin
        Sec := Pointer(FRawSections.Items[SecIdx]);
        ImportDesc := Pointer( ImgBase + RawPtr );
        while ImportDesc.Name <> 0 do
        begin
          nam := PAnsiChar( RVA2Raw( ImportDesc.Name, Sec ) + ImgBase );
          libItem := TImportLib.Create(string(nam), FImList);
          libItem.ImportKind := ikImport;
          libItem.ImportDescriptor := ImportDesc;
          libItem.Image := Self;

          if (importDesc.Characteristics = 0) then
            LibItem.FThunk := Pointer(RVA2Raw(importDesc.FirstThunk, Sec) + ImgBase)
          else
            LibItem.FThunk := Pointer(RVA2Raw(importDesc.Characteristics, Sec) + ImgBase);
          LibItem.FSec := Sec;
          LibItem.LoadData;
          Inc(importDesc);
        end;
      end;
    end;
  except
    FCorrupted := True;
  end;

  try
    dataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT];
    if dataDir.VirtualAddress <> 0 then
    begin
      rawPtr := VA2Raw(dataDir.VirtualAddress, FRawSections, secIdx);
      if (rawPtr <> 0) then
      begin
        sec := Pointer(FRawSections.Items[SecIdx]);
        delayImportDesc := Pointer(ImgBase + RawPtr);
        while delayImportDesc.szName <> 0 do
        begin
          nam := PAnsiChar(RVA2Raw(delayImportDesc.szName, Sec) + ImgBase);
          LibItem := TImportLib.Create(string(nam), FImList);
          libItem.ImportKind := ikDelayImport;
          libItem.ImportDescriptor := delayImportDesc;
          libItem.Image := Self;
          if (is64) then
            LibItem.FThunk := Pointer(RVA2Raw(delayImportDesc^.pINT, Sec) + ImgBase)
          else
            LibItem.FThunk := Pointer(VA2Raw(delayImportDesc^.pINT, FRawSections, secIdx) + ImgBase);
          LibItem.fSec := Sec;
          LibItem.LoadData;
          Inc(delayImportDesc);
        end;
      end;
    end;
  except
    FCorrupted := True;
  end;

  // ----  Compacting  ----
  // Модули могут встречаться в импорте несколько раз, для избежания этого - собираем функции одинаковых модулей вместе
  for i := ImportList.Count - 1 downto 1 do
  begin
    libI := TImportLib(ImportList.Items[i]);
    for j := i - 1 downto 0 do
    begin
      libJ := TImportLib(ImportList.Items[j]);
      if (SameText(libI.Name, libJ.Name) and (libI.ImportKind = libJ.ImportKind)) then
      begin
        libJ.Functions.AddItems(libI.Functions);
        libI.Free;
        Break;
      end;
    end;
  end;
end;

procedure TPEImage.LoadResourceTree;

var
  secIdx: Integer;
  imgBase: SysUInt;
  rawPtr: SysUInt;
//  rsrcSection: PImageSectionHeader;

  procedure MakeTree(ARoot: TResNode; ADir: PImageResourceDirectory; ALevel: Integer = 0);
  var
    n, i, resSecIdx: Integer;
    de: PImageResuorceDirectoryEntry;
    node: TResNode;
    offset: SysUInt;
    name: string;
    data: PImageResourceDataEntry;
    len: Word;
  begin

    N := ADir.NumOfNamedEntries + ADir.NumOfIdEntries;
    for I := 0 to N - 1 do
    begin
      Node := TResNode.Create('', ARoot);
      DE := Pointer(SysUInt(ADir) + SizeOf(TImageResourceDirectory) +
        SizeOf(TImageResuorceDirectoryEntry) * SysUInt(I));
      if DE.Name and $80000000 = 0 then // ID
        Node.ID := DE.Name
      else
      begin // string name
        offset := (DE.Name and not $80000000) + RawPtr + ImgBase;
        Len := PWord(offset)^;
        Inc(offset, 2);
        SetLength( Name, Len );
        Move(Pointer(offset)^, name[1], Len * 2);
        Node.Name := Name;
      end;
      offset := (DE.DataOffset and not $80000000) + RawPtr + ImgBase;
      if DE.DataOffset and $80000000 <> 0 then
      begin
        Node.IsDirectory := True;
      end
      else
      begin
        Data := Pointer(offset);
//        Node.DataRaw := RVA2Raw(Data.OffsetToData, rsrcSection);
//        if
//          (node.DataRaw > (rsrcSection.PointerToRawData + rsrcSection.Misc.PhysicalAddress))
//        then
        node.DataRaw := VA2Raw(Data.OffsetToData, FRawSections, resSecIdx);
        node.DataSize := data.Size;
        node.CodePage := data.CodePage;
        node.Lang := node.ID;
        node.ID := 0;
        // check validity for compressed binaries
        if (node.DataRaw + node.DataSize >= Mapper.FileSize) then
        begin
          node.DataRaw := 0;
          node.Corrupted := True;
          node.CorruptedReason := 'Corruped or compressed resource';
        end;
      end;
      case ALevel of
        0:
          begin
            if (node.Name = '') then
            begin
              if (node.ID > 0) and (node.ID < ResTypesCount) then
                node.DisplayName := ResTypes[node.ID]
              else
                node.DisplayName := IntToStr(node.ID);
            end
            else
              node.DisplayName := node.Name;
            node.DataType := node.ID;
          end;

        1:
          begin
            if (node.Name = '') then
              node.DisplayName := IntToStr(node.ID)
            else
              node.DisplayName := node.Name;
            node.DataType := ARoot.ID;
          end;

        2:
          begin
            node.DisplayName := LangDesc(node.Lang);
            node.DataType := ARoot.DataType;
          end;
      end;
      if (node.IsDirectory) then
        MakeTree(node, Pointer(offset), ALevel + 1);
    end;
  end;

  procedure CompactTree(ARoot: TResNode);
  var
    i, j: Integer;
    subItem, resGroup, idGroup: TResNode;
  begin
    for i := 0 to ARoot.SubDirs.Count - 1 do
    begin
      resGroup := ARoot.Children[i];
      for j := 0 to resGroup.ChildrenCount - 1 do
      begin
        idGroup := resGroup.Children[j];
        if (idGroup.ChildrenCount = 1) then
        begin
          subItem := idGroup.Children[0];
          idGroup.DataRaw := subItem.DataRaw;
          idGroup.DataSize := subItem.DataSize;
          idGroup.CodePage := subItem.CodePage;
          idGroup.IsDirectory := False;
          idGroup.Corrupted := subItem.Corrupted;
          idGroup.CorruptedReason := subItem.CorruptedReason;

          subItem.Free;
        end;
      end;
    end;
  end;

var
  ImgResDir: PImageResourceDirectory;
  DataDir: TImageDataDirectory;
begin
  CheckImageType;
  LoadCommonData;

  ImgBase := SysUInt( fMapper.Map );
  fCorrupted := False;

  if FResourceTree <> nil then
    FreeAndNil(FResourceTree);

  try
    DataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE];

    if DataDir.VirtualAddress <> 0 then
    begin
      RawPtr := VA2Raw(DataDir.VirtualAddress, FRawSections, SecIdx);
      if RawPtr <> 0 then
      begin
//        rsrcSection := Pointer(FRawSections[secIdx]);
        ImgResDir := Pointer(ImgBase + RawPtr);
        fResourceTree := TResNode.Create('', nil);
        MakeTree( fResourceTree, ImgResDir );
      end;
    end;

    if (ResourceTree <> nil) then
      CompactTree(ResourceTree);
  except
    FCorrupted := True;
  end;
end;

procedure TPEImage.LoadSectionData;
var
  I: Integer;
  Hdr: PImageSectionHeader;
  SBuf: array [ 0..8 ] of AnsiChar;
  sbi: Int64 absolute SBuf;
  si: TSectionItem;
  SecIdx: Integer;
begin
  CheckImageType;
  LoadCommonData;

  fCorrupted := False;
  if (FSections = nil) then
    FSections := TBasePECollection.Create(TSectionItem);
  if EntryPointExists then
    GetEntryPointRAW( SecIdx );

  try
    SBuf[ 8 ] := #0;
    FSections.Clear;
    for I := 0 to FRawSections.Count - 1 do
    begin
      Hdr := Pointer(FRawSections.Items[I]);
      sbi := 0;
      Move( Hdr.Name[ 0 ], SBuf[ 0 ], 8 );
      si := TSectionItem.Create(string(SBuf), FSections);
      si.VirtualAddress := Hdr.VirtualAddress;
      si.RawOffset := Hdr.PointerToRawData;
      si.VirtualSize := Hdr.Misc.VirtualSize;
      si.RawSize := Hdr.SizeOfRawData;
      si.Flags := Hdr.Characteristics;

      if EntryPointExists then
        SI.HasEP := (I = SecIdx);
    end;
  except
    fCorrupted := True;
  end;
end;

procedure TPEImage.LoadSimpleExportInfo;
var
  imgBase: SysUInt;
  dataDir: TImageDataDirectory;
  rawPtr: SysUInt;
  secIdx: Integer;
  sec: PImageSectionHeader;
  expDir: PImageExportDirectory;
  names: PDWORDArray;
  ordinals: PWORDArray;
  i: Integer;
  nam: PAnsiChar;
begin
  CheckImageType;
  LoadCommonData;

  imgBase := SysUInt(Mapper.Map);

  fCorrupted := False;
  if (SimpleExportList = nil) then
    FSimpleExportList := TStringList.Create;

  SafeClearSimpleExportList;

  try
    dataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];

    if DataDir.VirtualAddress <> 0 then
    begin
      RawPtr := VA2Raw(dataDir.VirtualAddress, FRawSections, SecIdx);
      if (RawPtr <> 0) then
      begin
        Sec := Pointer(FRawSections.Items[SecIdx]);
        ExpDir := Pointer( ImgBase + RawPtr );
        Names := Pointer( RVA2Raw( ExpDir.AddressOfNames, Sec ) +
            ImgBase );
        Ordinals := Pointer( RVA2Raw( ExpDir.AddressOfNameOrdinals, Sec ) +
            ImgBase );
        for I := 0 to ExpDir.NumberOfNames - 1 do
        begin
          Nam := PAnsiChar( RVA2Raw( Names[ I ], Sec ) + ImgBase );
          SimpleExportList.AddObject(string(Nam), Pointer(Ordinals[ I ]) );
        end;
      end;
    end;
  except
    FCorrupted := True;
  end;
end;

procedure TPEImage.LoadVersionInfoResource;
var
  secIdx: Integer;
  imgBase: SysUInt;
  rawPtr: SysUInt;
  imgResDir: PImageResourceDirectory;
  dataDir: TImageDataDirectory;

  function findResource(
    ADir: PImageResourceDirectory; AIdToFind: Integer; var AIdFound: Integer): Integer;
  var
    i: Integer;
    de: PImageResuorceDirectoryEntry;
  begin
    Result := -1;
    for I := 0 to ADir.NumOfIdEntries + ADir.NumOfNamedEntries - 1 do
    begin
      de := Pointer(SysUInt(ADir) + SizeOf(TImageResourceDirectory) +
        SizeOf(TImageResuorceDirectoryEntry) * SysUInt(I));
      if (de.Name and $80000000 = 0) and ((de.Name = SysUInt(AIdToFind)) or (AIdToFind = -1)) then
      begin
        AIdFound := de.Name;
        Result := (de.DataOffset and not $80000000) + rawPtr + imgBase;
        Break;
      end;
    end;
  end;

  procedure findVersionResource(ADir: PImageResourceDirectory);
  var
    off, off2, lang, tmp, resSecIdx: Integer;
    data: PImageResourceDataEntry;
  begin
    off := findResource(ADir, RT_VERSION, tmp);
    if (off >= 0) then
    begin
      // Find first subdirectory
      off := findResource(Pointer(off), -1, tmp);
      if (off >= 0) then
      begin
        // find version resource
        off2 := findResource(Pointer(off), -1, lang);
        if (off2 >= 0) then
        begin
          data := Pointer(off2);
          FVersionResource := TResNode.Create(ResTypes[RT_VERSION], nil);
//          FVersionResource.DataRaw := RVA2Raw(data.OffsetToData, RawSections[secIdx]);
          FVersionResource.DataRaw := VA2Raw(data.OffsetToData, FRawSections, resSecIdx);
          FVersionResource.DataSize := data.Size;
          FVersionResource.CodePage := data.CodePage;
          FVersionResource.Lang := lang;
        end;
      end;
    end;
  end;

begin
  CheckImageType;
  LoadCommonData;

  imgBase := SysUInt(FMapper.Map);
  FCorrupted := False;

  if (FVersionResource <> nil) then
    FreeAndNil(FVersionResource);

  try
    dataDir := GetNTOptionalHeaderDataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE];
    if (dataDir.VirtualAddress <> 0) then
    begin
      rawPtr := VA2Raw(DataDir.VirtualAddress, FRawSections, secIdx);
      if (rawPtr <> 0) then
      begin
        imgResDir := Pointer(imgBase + rawPtr);
        findVersionResource(imgResDir);
      end;
    end;
  except
    FCorrupted := True;
  end;
end;

procedure TPEImage.UnloadImage;
begin
  Dec(FImageLoadCount);
  if (FImageLoadCount <= 0) then
    Mapper.UnloadFile;
end;


{ TImportLib }


constructor TImportLib.Create(const AName: string; ACollection: TCollection);
begin
  inherited;
  FFunctions := TBasePECollection.Create(TImportItem);
  FFunctions.FOwner := Self;
end;

destructor TImportLib.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

procedure TImportLib.LoadData;
var
  funcItem: TImportItem;
  ordName: PImageImportByName;
  is64: Boolean;

  function checkThunk(thunk: Pointer; is64: Boolean): Boolean; inline;
  begin
    if is64 then
      Result := (PUInt64(thunk)^ <> 0)
    else
      Result := (PDWORD(thunk)^ <> 0);
  end;

  procedure incThunk(var thunk: Pointer; is64: Boolean); inline;
  begin
    if is64 then
      Inc(PUInt64(thunk))
    else
      Inc(PDWORD(thunk));
  end;

  function isOrdinalImport(thunk: Pointer; is64: Boolean): Boolean; //inline;
  begin
    if is64 then
      Result := (PUInt64(thunk)^ and IMAGE_ORDINAL_FLAG64 <> 0)
    else
      Result := (PDWORD(thunk)^ and IMAGE_ORDINAL_FLAG32 <> 0);
  end;


begin
  if (FThunk = nil) then
    Exit;

  is64 := (Image is TPEImage64);

  Functions.Clear;

  while checkThunk(FThunk, is64) do
  begin
    funcItem := TImportItem.Create('', Functions);
    funcItem.Module := Name;
    FuncItem.Lib := Self;
    if isOrdinalImport(FThunk, is64) then
    begin
      funcItem.Ordinal := IMAGE_ORDINAL(PDWORD(FThunk)^);
      funcItem.Name := '';
      funcItem.DisplayName := Format('ordinal %d', [funcItem.Ordinal]);
    end else
    begin
      ordName := nil;
      case fImportKind of
        ikImport:
          ordName :=
            PImageImportByName(FImage.RVA2Raw(PDWORD(FThunk)^, fSec) +
            SysUInt(Image.Mapper.Map));
        ikDelayImport:
          begin
            if (is64) then
              ordName :=
                PImageImportByName(FImage.RVA2RawEx(PUInt64(FThunk)^, fSec) +
                SysUInt(Image.Mapper.Map))
            else
              ordName :=
                PImageImportByName(FImage.RVA2RawEx(PDWORD(FThunk)^, fSec) +
                SysUInt(Image.Mapper.Map));
          end;
      end;
      funcItem.Hint := ordName.Hint;
      funcItem.Name := string(PAnsiChar(@OrdName.Name));
      funcItem.DisplayName := funcItem.Name;
    end;
    incThunk(FThunk, is64);
  end;

end;



{ TImportItem }



procedure TImportItem.Assign(Source: TPersistent);
begin
  if (Source is TImportItem) then
  begin
    FName := TImportItem(Source).Name;
    FModule := TImportItem(Source).Module;
    FHint := TImportItem(Source).Hint;
    FOrdinal := TImportItem(Source).Ordinal;
  end
  else
    inherited;
end;


{ TResNode }


constructor TResNode.Create(const AName: string; AParent: TResNode);
begin
  if (AParent = nil) then
  begin
    inherited Create(AName, nil);
    FIsRoot := True;
    FIsDirectory := True;
  end
  else
  begin
    inherited Create(AName, AParent.SubDirs);
    FParent := AParent;
  end;
  FSubDirs := TBasePECollection.Create(TResNode);
end;


destructor TResNode.Destroy;
begin
  FSubDirs.Free;
  inherited;
end;

function TResNode.GetChildren(Index: Integer): TResNode;
begin
  Result := TResNode(SubDirs.Items[Index]);
end;

function TResNode.GetChildrenCount: Integer;
begin
  Result := SubDirs.Count;
end;


{ TBasePECollection }


procedure TBasePECollection.AddItems(ACollection: TCollection);
var
  i: Integer;
begin
  if (ItemClass <> ACollection.ItemClass) and not ACollection.ItemClass.InheritsFrom(ItemClass) then
    raise Exception.Create('Item class must be equal');
  for i := ACollection.Count - 1 downto 0 do
    ACollection.Items[i].Collection := Self;
end;

function TBasePECollection.GetItemByName(const AName: string): TBasePEItem;
var
  i: Integer;
begin
  i := IndexOfName(AName);
  if (i >= 0) then
    Result := TBasePEItem(Items[i])
  else
    Result := nil;
end;

function TBasePECollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBasePECollection.IndexOfName(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if (TBasePEItem(Items[Result]).Name = AName) then
      Exit;
  end;
  Result := -1;
end;


{ TBasePEItem }


constructor TBasePEItem.Create(const AName: string; ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := AName;
end;


{ TPEImage32 }


function TPEImage32.FormatHex(AValue: UInt64): string;
begin
  Result := MyIntToHex(AValue);
end;

function TPEImage32.GetEntryPointRAW(var SecIdx: Integer): UInt64;
begin
  Result := VA2Raw(ImageNTHeader.OptionalHeader.AddressOfEntryPoint, FRawSections, SecIdx);
end;

function TPEImage32.GetImageNTHeader: Pointer;
begin
  Result := FImageNTHeader;
end;

function TPEImage32.GetNTFileHeaderNumberOfSections: UInt;
begin
  Result := FImageNTHeader^.FileHeader.NumberOfSections;
end;

function TPEImage32.GetNTOptionalHeaderAddressOfEntryPoint: UInt64;
begin
  Result := FImageNTHeader^.OptionalHeader.AddressOfEntryPoint;
end;

function TPEImage32.GetNTOptionalHeaderDataDirectory: PPEImageHeaderDirectoryList;
begin
  Result := @FImageNTHeader^.OptionalHeader.DataDirectory;
end;

function TPEImage32.GetSizeOfNTHeader: Integer;
begin
  Result := SizeOf(FImageNTHeader^);
end;

function TPEImage32.RVA2RawEx(VirtualAddress: UInt64; SecHdr: PImageSectionHeader): UInt64;
begin
  if (VirtualAddress > Mapper.FileSize) and (VirtualAddress > ImageNTHeader.OptionalHeader.ImageBase) then
    Dec(VirtualAddress, ImageNTHeader.OptionalHeader.ImageBase);
  Result := RVA2Raw( VirtualAddress, SecHdr );
end;

procedure TPEImage32.SetImageNTHeader(AHeader: Pointer);
begin
  FImageNTHeader := AHeader;
end;


{ TPEImage64 }


function TPEImage64.FormatHex(AValue: UInt64): string;
begin
  Result := MyIntToHex64(AValue);
end;

function TPEImage64.GetEntryPointRAW(var SecIdx: Integer): UInt64;
begin
  Result := VA2Raw(ImageNTHeader.OptionalHeader.AddressOfEntryPoint, FRawSections, SecIdx);
end;

function TPEImage64.GetImageNTHeader: Pointer;
begin
  Result := FImageNTHeader;
end;

function TPEImage64.GetNTFileHeaderNumberOfSections: UInt;
begin
  Result := FImageNTHeader^.FileHeader.NumberOfSections;
end;

function TPEImage64.GetNTOptionalHeaderAddressOfEntryPoint: UInt64;
begin
  Result := FImageNTHeader^.OptionalHeader.AddressOfEntryPoint;
end;

function TPEImage64.GetNTOptionalHeaderDataDirectory: PPEImageHeaderDirectoryList;
begin
  Result := @FImageNTHeader^.OptionalHeader.DataDirectory;
end;

function TPEImage64.GetSizeOfNTHeader: Integer;
begin
  Result := SizeOf(FImageNTHeader^);
end;

procedure TPEImage64.LoadImage;
begin
  {$IFNDEF WIN64}
  if IsOnWow then
    Wow64DisableWow64FsRedirection(FWow64Temp);
  {$ENDIF}
  inherited;
end;

function TPEImage64.RVA2RawEx(VirtualAddress: UInt64; SecHdr: PImageSectionHeader): UInt64;
begin
  if (VirtualAddress > Mapper.FileSize) and (VirtualAddress > ImageNTHeader.OptionalHeader.ImageBase) then
    Dec(VirtualAddress, ImageNTHeader.OptionalHeader.ImageBase);
  Result := RVA2Raw( VirtualAddress, SecHdr );
end;

procedure TPEImage64.SetImageNTHeader(AHeader: Pointer);
begin
  FImageNTHeader := AHeader;
end;

procedure TPEImage64.UnloadImage;
begin
  inherited;
  {$IFNDEF WIN64}
  if IsOnWow then
    Wow64RevertWow64FsRedirection(FWow64Temp);
  {$ENDIF}
end;

end.
