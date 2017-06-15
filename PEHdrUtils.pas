unit PEHdrUtils;

interface
uses
  Winapi.Windows, Classes, SysUtils, CommonUtilities, ComCtrls;

type

  PSectionCopy = ^TSectionCopy;
  TSectionCopy = record
    RawHeaderStruct: TImageSectionHeader;
    OrigPtr: Pointer;
  end;

  TNodeData = packed record
    case Integer of
    1: (
      Fmt,
      Count,
      ID,
      Idx: Byte;
    );
    2: (AsDWORD: DWORD;);
    3: (AsPointer: Pointer)
  end;


  PImageHeaders32 = ^TImageHeaders32;
  TImageHeaders32 = record
    Dos: TImageDosHeader;
    NT: TImageNtHeaders32;
    RawSections: TList;
  end;

  PImageHeaders64 = ^TImageHeaders64;
  TImageHeaders64 = record
    Dos: TImageDosHeader;
    NT: TImageNtHeaders64;
    RawSections: TList;
  end;


  TByteArray = array[ 0..31 ] of Byte;
  PByteArray = ^TByteArray;


const

  // Reserved Image types
  IMAGE_SCN_TYPE_DSECT             =   $00000001;
  IMAGE_SCN_TYPE_NOLOAD            =   $00000002;
  IMAGE_SCN_TYPE_GROUP             =   $00000004;
  IMAGE_SCN_TYPE_COPY              =   $00000010;
  IMAGE_SCN_TYPE_OVER              =   $00000400;
  IMAGE_SCN_ALIGN_128BYTES     =   $00800000; 
  IMAGE_SCN_ALIGN_256BYTES     =   $00900000;
  IMAGE_SCN_ALIGN_512BYTES     =   $00A00000; 
  IMAGE_SCN_ALIGN_1024BYTES    =   $00B00000;
  IMAGE_SCN_ALIGN_2048BYTES    =   $00C00000; 
  IMAGE_SCN_ALIGN_4096BYTES    =   $00D00000;
  IMAGE_SCN_ALIGN_8192BYTES    =   $00E00000;


  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT  = 13;   // Delay Load Import Descriptors
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14;   // COM Runtime descriptor


  IMAGE_FILE_MACHINE_UNKNOWN      =    $0;
  IMAGE_FILE_MACHINE_ALPHA        =    $184;
  IMAGE_FILE_MACHINE_ARM          =    $1c0;
  IMAGE_FILE_MACHINE_ALPHA64      =    $284;
  IMAGE_FILE_MACHINE_I386         =    $14c;
  IMAGE_FILE_MACHINE_IA64         =    $200;
  IMAGE_FILE_MACHINE_M68K         =    $268;
  IMAGE_FILE_MACHINE_MIPS16       =    $266;
  IMAGE_FILE_MACHINE_MIPSFPU      =    $366;
  IMAGE_FILE_MACHINE_MIPSFPU16    =    $466;
  IMAGE_FILE_MACHINE_POWERPC      =    $1f0;
  IMAGE_FILE_MACHINE_R3000        =    $162;
  IMAGE_FILE_MACHINE_R4000        =    $166;
  IMAGE_FILE_MACHINE_R10000       =    $168;
  IMAGE_FILE_MACHINE_WCEMIPSV2    =    $169;
  IMAGE_FILE_MACHINE_SH3          =    $1a2;
  IMAGE_FILE_MACHINE_SH3DSP       =    $1a3;
  IMAGE_FILE_MACHINE_SH3E         =    $1a4;
  IMAGE_FILE_MACHINE_SH4          =    $1a6;
  IMAGE_FILE_MACHINE_SH5          =    $1a8;
  IMAGE_FILE_MACHINE_THUMB        =    $1c2;
  IMAGE_FILE_MACHINE_AM33         =    $1d3;
  IMAGE_FILE_MACHINE_POWERPCFP    =    $1f1;
  IMAGE_FILE_MACHINE_TRICORE      =    $520;
  IMAGE_FILE_MACHINE_CEF          =    $cef;
  IMAGE_FILE_MACHINE_EBC          =    $ebc;
  IMAGE_FILE_MACHINE_AMD64        =    $8664;
  IMAGE_FILE_MACHINE_M32R         =    $9041;
  IMAGE_FILE_MACHINE_CEE          =    $c0ee;


  IMAGE_FILE_RELOCS_STRIPPED         = $0001;
  IMAGE_FILE_EXECUTABLE_IMAGE        = $0002;
  IMAGE_FILE_LINE_NUMS_STRIPPED      = $0004;
  IMAGE_FILE_LOCAL_SYMS_STRIPPED     = $0008;
  IMAGE_FILE_AGGRESSIVE_WS_TRIM      = $0010;
  IMAGE_FILE_LARGE_ADDRESS_AWARE     = $0020;
  IMAGE_FILE_16BIT_MACHINE           = $0040;
  IMAGE_FILE_BYTES_REVERSED_LO       = $0080;
  IMAGE_FILE_32BIT_MACHINE           = $0100;
  IMAGE_FILE_DEBUG_STRIPPED          = $0200;
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400;
  IMAGE_FILE_SYSTEM                  = $1000;
  IMAGE_FILE_DLL                     = $2000;
  IMAGE_FILE_UP_SYSTEM_ONLY          = $4000;
  IMAGE_FILE_BYTES_REVERSED_HI       = $8000;


  IMAGE_SUBSYSTEM_UNKNOWN         = 0;   // Unknown subsystem.
  IMAGE_SUBSYSTEM_NATIVE          = 1;   // Image doesn't require a subsystem.
  IMAGE_SUBSYSTEM_WINDOWS_GUI     = 2;   // Image runs in the Windows GUI subsystem.
  IMAGE_SUBSYSTEM_WINDOWS_CUI     = 3;   // Image runs in the Windows character subsystem.
  IMAGE_SUBSYSTEM_OS2_CUI         = 5;   // image runs in the OS/2 character subsystem.
  IMAGE_SUBSYSTEM_POSIX_CUI       = 7;   // image runs in the Posix character subsystem.
  IMAGE_SUBSYSTEM_NATIVE_WINDOWS  = 8;   // image is a native Win9x driver.
  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI  = 9;   // Image runs in the Windows CE subsystem.
  IMAGE_SUBSYSTEM_EFI_APPLICATION         = 10;   // Image is an EFI application.
  IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER = 11;   // Image is an EFI driver that provides
                                                // boot services.
  IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER      = 12;   // Image is an EFI driver that provides
                                                // runtime services.
  IMAGE_SUBSYSTEM_EFI_ROM                 = 13;   // Image is an EFI ROM application.
  IMAGE_SUBSYSTEM_XBOX                    = 14;   // Image runs in the XBOX subsystem.


  IMAGE_DLLCHARACTERISTICS_NO_ISOLATION = $0200;    // Image understands
                                                    // isolation and doesn't
                                                    // want it
  IMAGE_DLLCHARACTERISTICS_NO_SEH = $0400;     // Image does not use SEH.
                                               // No SE handler may reside
                                               // in this image
  IMAGE_DLLCHARACTERISTICS_NO_BIND = $0800;     // Do not bind this image.
  IMAGE_DLLCHARACTERISTICS_WDM_DRIVER = $2000;     // Driver uses WDM model
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;
  IMAGE_DLLCHARACTERISTICS_X86_THUNK = $1000; // Image is a Wx86 Thunk DLL


// Format spec:
// 33         2         1
// 10 8 6 4 2 0 8 6 4 2 0 8 6 4 2 0
// 00000000000000000000000000000000b
//
// bits   meaning
// 0-1    Size of each item in bytes (TVI_S2, TVI_S4, Default - 1 byte)
// 2      Signed/unsigned (TVI_SIGNED, Default - unsigned)
// 3-5    Display format (TVI_DEC, TVI_CHAR, TVI_DATE, TVI_TIME, TVI_DATETIME, Default - Hex)
// 6-7    Reserved
// 8-15   Items count in array
// 16-23  Item identifier
// 24-31  Index of the item


  TVI_FMT_S1 = $00;
  TVI_FMT_S2 = $01;
  TVI_FMT_S4 = $02;
  TVI_FMT_S8 = $03;
  TVI_FMT_SZMASK = $03;

  TVI_FMT_UNSIGNED = $00;
  TVI_FMT_SIGNED = $04;
  TVI_FMT_SIGNMASK = $04;

  TVI_FMT_HEX = $00;
  TVI_FMT_DEC = $08;
  TVI_FMT_CHAR = $18;
  TVI_FMT_DATETIME = $20;
  TVI_FMT_TYPEMASK = $38;


// Item identifiers:


  ID_IDH_ROOT          =    1;
  ID_IDH_E_MAGIC       =    2;
  ID_IDH_E_CBLP        =    3;
  ID_IDH_E_CP          =    4;
  ID_IDH_E_CRLC        =    5;
  ID_IDH_E_CPARHDR     =    6;
  ID_IDH_E_MINALLOC    =    7;
  ID_IDH_E_MAXALLOC    =    8;
  ID_IDH_E_SS          =    9;
  ID_IDH_E_SP          =    10;
  ID_IDH_E_CSUM        =    11;
  ID_IDH_E_IP          =    12;
  ID_IDH_E_CS          =    13;
  ID_IDH_E_LFARLC      =    14;
  ID_IDH_E_OVNO        =    15;
  ID_IDH_E_RES         =    16;
  ID_IDH_E_OEMID       =    17;
  ID_IDH_E_OEMINFO     =    18;
  ID_IDH_E_RES2        =    19;
  ID_IDH_E_LFANEW      =    20;

  ID_IFH_ROOT                  = 21;
  ID_IFH_MACHINE               = 22;
  ID_IFH_NUMBEROFSECTIONS      = 23;
  ID_IFH_TIMEDATESTAMP         = 24;
  ID_IFH_POINTERTOSYMBOLTABLE  = 25;
  ID_IFH_NUMBEROFSYMBOLS       = 26;
  ID_IFH_SIZEOFOPTIONALHEADER  = 27;
  ID_IFH_CHARACTERISTICS       = 28;

  ID_IOH_ROOT                        = 31;
  ID_IOH_MAGIC                       = 32;
  ID_IOH_MAJORLINKERVERSION          = 33;
  ID_IOH_MINORLINKERVERSION          = 34;
  ID_IOH_SIZEOFCODE                  = 35;
  ID_IOH_SIZEOFINITIALIZEDDATA       = 36;
  ID_IOH_SIZEOFUNINITIALIZEDDATA     = 37;
  ID_IOH_ADDRESSOFENTRYPOINT         = 38;
  ID_IOH_BASEOFCODE                  = 39;
  ID_IOH_BASEOFDATA                  = 40;
  ID_IOH_IMAGEBASE                   = 41;
  ID_IOH_SECTIONALIGNMENT            = 42;
  ID_IOH_FILEALIGNMENT               = 43;
  ID_IOH_MAJOROPERATINGSYSTEMVERSION = 44;
  ID_IOH_MINOROPERATINGSYSTEMVERSION = 45;
  ID_IOH_MAJORIMAGEVERSION           = 46;
  ID_IOH_MINORIMAGEVERSION           = 47;
  ID_IOH_MAJORSUBSYSTEMVERSION       = 48;
  ID_IOH_MINORSUBSYSTEMVERSION       = 49;
  ID_IOH_WIN32VERSIONVALUE           = 50;
  ID_IOH_SIZEOFIMAGE                 = 51;
  ID_IOH_SIZEOFHEADERS               = 52;
  ID_IOH_CHECKSUM                    = 53;
  ID_IOH_SUBSYSTEM                   = 54;
  ID_IOH_DLLCHARACTERISTICS          = 55;
  ID_IOH_SIZEOFSTACKRESERVE          = 56;
  ID_IOH_SIZEOFSTACKCOMMIT           = 57;
  ID_IOH_SIZEOFHEAPRESERVE           = 58;
  ID_IOH_SIZEOFHEAPCOMMIT            = 59;
  ID_IOH_LOADERFLAGS                 = 60;
  ID_IOH_NUMBEROFRVAANDSIZES         = 61;
  ID_IOH_DATADIRECTORY               = 62;

  // Image NT Headers

  ID_INH_ROOT             = 66;
  ID_INH_SIGNATURE        = 67;
//  ID_INH_FILEHEADER       = 68;
//  ID_INH_OPTIONALHEADER   = 69;

  // Image Sections

  ID_ISH_ROOTCAPTION          = 70;
  ID_ISH_ROOT                 = 71;
  ID_ISH_NAME                 = 72;
  ID_ISH_MISC                 = 73;
  ID_ISH_VIRTUALADDRESS       = 74;
  ID_ISH_SIZEOFRAWDATA        = 75;
  ID_ISH_POINTERTORAWDATA     = 76;
  ID_ISH_POINTERTORELOCATIONS = 77;
  ID_ISH_POINTERTOLINENUMBERS = 78;
  ID_ISH_NUMBEROFRELOCATIONS  = 79;
  ID_ISH_NUMBEROFLINENUMBERS  = 80;
  ID_ISH_CHARACTERISTICS      = 81;

  ID_IDD_VIRTUALADDRESS = 83;
  ID_IDD_SIZE           = 84;

  ID_IDD_EXPORT         = 85;
  ID_IDD_IMPORT         = 86;
  ID_IDD_RESOURCE       = 87;
  ID_IDD_EXCEPTION      = 88;
  ID_IDD_SECURITY       = 89;
  ID_IDD_BASERELOC      = 90;
  ID_IDD_DEBUG          = 91;
  ID_IDD_COPYRIGHT      = 92;
  ID_IDD_GLOBALPTR      = 93;
  ID_IDD_TLS            = 94;
  ID_IDD_LOAD_CONFIG    = 95;
  ID_IDD_BOUND_IMPORT   = 96;
  ID_IDD_IAT            = 97;
  ID_IDD_DELAY_IMPORT   = 98;
  ID_IDD_COM_DESCRIPTOR = 99;
  ID_IDD_RESERVED       = 100;


  HdrCaptions: array[ 1..100 ] of string = (
{5}   'IMAGE_DOS_HEADER', 'e_magic', 'e_cblp', 'e_cp', 'e_crlc',
{10}  'e_cparhdr', 'e_minalloc', 'e_maxalloc', 'e_ss', 'e_sp',
{15}  'e_csum', 'e_ip', 'e_cs', 'e_lfarlc', 'e_ovno',
{20}  'e_res', 'e_oemid', 'e_oeminfo', 'e_res2', 'e_lfanew',
{25}  'IMAGE_FILE_HEADER', 'Machine', 'NumberOfSections', 'TimeDateStamp', 'PointerToSymbolTable',
{30}  'NumberOfSymbols', 'SizeOfOptionalHeader', 'Characteristics', '', '',
{35}  'IMAGE_OPTIONAL_HEADER', 'Magic', 'MajorLinkerVersion', 'MinorLinkerVersion', 'SizeOfCode',
{40}  'SizeOfInitializedData', 'SizeOfUninitializedData', 'AddressOfEntryPoint', 'BaseOfCode', 'BaseOfData',
{45}  'ImageBase', 'SectionAlignment', 'FileAlignment', 'MajorOperatingSystemVersion', 'MinorOperatingSystemVersion',
{50}  'MajorImageVersion', 'MinorImageVersion', 'MajorSubsystemVersion', 'MinorSubsystemVersion', 'Win32VersionValue',
{55}  'SizeOfImage', 'SizeOfHeaders', 'CheckSum', 'Subsystem', 'DllCharacteristics',
{60}  'SizeOfStackReserve', 'SizeOfStackCommit', 'SizeOfHeapReserve', 'SizeOfHeapCommit', 'LoaderFlags',
{65}  'NumberOfRvaAndSizes', 'Data Directories', '', '', '',
{70}  'IMAGE_NT_HEADERS', 'Signature', 'FileHeader', 'OptionalHeader', 'Section Table',
{75}  'IMAGE_SECTION_HEADER', 'Name', 'VirtualSize', 'VirtualAddress', 'SizeOfRawData',
{80}  'PointerToRawData', 'PointerToRelocations', 'PointerToLinenumbers', 'NumberOfRelocations', 'NumberOfLinenumbers',
{85}  'Characteristics', '', 'VirtualAddress', 'Size', 'IMAGE_DIRECTORY_ENTRY_EXPORT',
{90}  'IMAGE_DIRECTORY_ENTRY_IMPORT', 'IMAGE_DIRECTORY_ENTRY_RESOURCE', 'IMAGE_DIRECTORY_ENTRY_EXCEPTION', 'IMAGE_DIRECTORY_ENTRY_SECURITY', 'IMAGE_DIRECTORY_ENTRY_BASERELOC',
{95}  'IMAGE_DIRECTORY_ENTRY_DEBUG', 'IMAGE_DIRECTORY_ENTRY_COPYRIGHT', 'IMAGE_DIRECTORY_ENTRY_GLOBALPTR', 'IMAGE_DIRECTORY_ENTRY_TLS', 'IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG',
{100} 'IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT', 'IMAGE_DIRECTORY_ENTRY_IAT', 'IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT', 'IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR', 'Reserved' );



function SecFlags2Str( Chars: Cardinal ): string;
procedure FillTreeView32(ATree: TTreeView; AHeaders: PImageHeaders32);
procedure FillTreeView64(ATree: TTreeView; AHeaders: PImageHeaders64);
function GetFormattedData(
  Data: Pointer; Format: TNodeData; Hdrs: Pointer; StdData: Boolean): string;
function GetDataFromID32(Hdrs: PImageHeaders32; ID, Idx: Byte): Pointer;
function GetDataFromID64(Hdrs: PImageHeaders64; ID, Idx: Byte): Pointer;
function GetImageMachineType( Value: Word ): string;
function ImageCharFlags2Str( Value: Word ): string;
function GetImageSybsystem( Value: Word ): string;
function ImageDllChars2Str( Value: Word ): string;
function DateTimeStamp2DateTime( Value: DWORD ): TDateTime;
//function GetOffsetFromID32(ID, Idx: Byte; Hdrs: PImageHeaders32): Cardinal;
//function GetOffsetFromID64(ID, Idx: Byte; Hdrs: PImageHeaders64): Cardinal;


const
  Sz2Val: array[0..3] of Byte = (1, 2, 4, 8);


implementation



function SecFlags2Str( Chars: Cardinal ): string;
begin
  Result := '';
  if (Chars and IMAGE_SCN_TYPE_NO_PAD) = IMAGE_SCN_TYPE_NO_PAD then
    Result := Result + 'No Pad, ';
  if (Chars and IMAGE_SCN_CNT_CODE) = IMAGE_SCN_CNT_CODE then
    Result := Result + 'Code, ';
  if (Chars and IMAGE_SCN_CNT_INITIALIZED_DATA) = IMAGE_SCN_CNT_INITIALIZED_DATA then
    Result := Result + 'Initialized Data, ';
  if Chars and IMAGE_SCN_CNT_UNINITIALIZED_DATA = IMAGE_SCN_CNT_UNINITIALIZED_DATA then
    Result := Result + 'Uninitialized Data, ';
  if Chars and IMAGE_SCN_LNK_INFO = IMAGE_SCN_LNK_INFO then
    Result := Result + 'Info, ';
  if Chars and IMAGE_SCN_LNK_REMOVE = IMAGE_SCN_LNK_REMOVE then
    Result := Result + 'Remove, ';
  if Chars and IMAGE_SCN_LNK_COMDAT = IMAGE_SCN_LNK_COMDAT then
    Result := Result + 'Comdat, ';
  if Chars and IMAGE_SCN_ALIGN_1BYTES = IMAGE_SCN_ALIGN_1BYTES then
    Result := Result + '1-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_2BYTES = IMAGE_SCN_ALIGN_2BYTES then
    Result := Result + '2-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_4BYTES = IMAGE_SCN_ALIGN_4BYTES then
    Result := Result + '4-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_8BYTES = IMAGE_SCN_ALIGN_8BYTES then
    Result := Result + '8-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_16BYTES = IMAGE_SCN_ALIGN_16BYTES then
    Result := Result + '16-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_32BYTES = IMAGE_SCN_ALIGN_32BYTES then
    Result := Result + '32-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_64BYTES = IMAGE_SCN_ALIGN_64BYTES then
    Result := Result + '64-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_128BYTES = IMAGE_SCN_ALIGN_128BYTES then
    Result := Result + '128-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_256BYTES = IMAGE_SCN_ALIGN_256BYTES then
    Result := Result + '256-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_512BYTES = IMAGE_SCN_ALIGN_512BYTES then
    Result := Result + '512-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_1024BYTES = IMAGE_SCN_ALIGN_1024BYTES then
    Result := Result + '1024-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_2048BYTES = IMAGE_SCN_ALIGN_2048BYTES then
    Result := Result + '2048-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_4096BYTES = IMAGE_SCN_ALIGN_4096BYTES then
    Result := Result + '4096-byte Align, ';
  if Chars and IMAGE_SCN_ALIGN_8192BYTES = IMAGE_SCN_ALIGN_8192BYTES then
    Result := Result + '8192-byte Align, ';
  if Chars and IMAGE_SCN_LNK_NRELOC_OVFL = IMAGE_SCN_LNK_NRELOC_OVFL then
    Result := Result + 'Ext. Relocations, ';
  if Chars and IMAGE_SCN_MEM_DISCARDABLE = IMAGE_SCN_MEM_DISCARDABLE then
    Result := Result + 'Discardable, ';
  if Chars and IMAGE_SCN_MEM_NOT_CACHED = IMAGE_SCN_MEM_NOT_CACHED then
    Result := Result + 'Cached, ';
  if Chars and IMAGE_SCN_MEM_NOT_PAGED = IMAGE_SCN_MEM_NOT_PAGED then
    Result := Result + 'Not Pageable, ';
  if Chars and IMAGE_SCN_MEM_SHARED = IMAGE_SCN_MEM_SHARED then
    Result := Result + 'Shared, ';
  if Chars and IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE then
    Result := Result + 'Executable, ';
  if Chars and IMAGE_SCN_MEM_READ = IMAGE_SCN_MEM_READ then
    Result := Result + 'Read, ';
  if Chars and IMAGE_SCN_MEM_WRITE = IMAGE_SCN_MEM_WRITE then
    Result := Result + 'Write, ';
  if Chars and IMAGE_SCN_TYPE_DSECT = IMAGE_SCN_TYPE_DSECT then
    Result := Result + 'SCN_TYPE_DSECT, ';
  if Chars and IMAGE_SCN_TYPE_NOLOAD = IMAGE_SCN_TYPE_NOLOAD then
    Result := Result + 'SCN_TYPE_NOLOAD, ';
  if Chars and IMAGE_SCN_TYPE_GROUP = IMAGE_SCN_TYPE_GROUP then
    Result := Result + 'SCN_TYPE_GROUP, ';
  if Chars and IMAGE_SCN_TYPE_COPY = IMAGE_SCN_TYPE_COPY then
    Result := Result + 'SCN_TYPE_COPY, ';
  if Chars and IMAGE_SCN_LNK_OTHER = IMAGE_SCN_LNK_OTHER then
    Result := Result + 'SCN_LNK_OTHER, ';
  if Chars and IMAGE_SCN_TYPE_OVER = IMAGE_SCN_TYPE_OVER then
    Result := Result + 'SCN_TYPE_OVER, ';
  if Chars and IMAGE_SCN_MEM_FARDATA = IMAGE_SCN_MEM_FARDATA then
    Result := Result + 'SCN_MEM_FARDATA, ';
  if Chars and IMAGE_SCN_MEM_PURGEABLE = IMAGE_SCN_MEM_PURGEABLE then
    Result := Result + 'SCN_MEM_PURGEABLE, ';
  if Chars and IMAGE_SCN_MEM_16BIT = IMAGE_SCN_MEM_16BIT then
    Result := Result + 'SCN_MEM_16BIT, ';
  if Chars and IMAGE_SCN_MEM_LOCKED = IMAGE_SCN_MEM_LOCKED then
    Result := Result + 'SCN_MEM_LOCKED, ';
  if Chars and IMAGE_SCN_MEM_LOCKED = IMAGE_SCN_MEM_LOCKED then
    Result := Result + 'SCN_MEM_LOCKED, ';
  if Chars and IMAGE_SCN_MEM_PRELOAD = IMAGE_SCN_MEM_PRELOAD then
    Result := Result + 'SCN_MEM_PRELOAD, ';
  if Chars and IMAGE_SCN_MEM_LOCKED = IMAGE_SCN_MEM_LOCKED then
    Result := Result + 'SCN_MEM_LOCKED, ';
  if Chars and IMAGE_SCN_MEM_LOCKED = IMAGE_SCN_MEM_LOCKED then
    Result := Result + 'SCN_MEM_LOCKED, ';
  if Result = '' then
    Result := '<No Characteristics>'
  else
    Delete( Result, Length( Result ) - 1, 2 );
end;



function GetImageMachineType(Value: Word): string;
begin
  case Value of
    IMAGE_FILE_MACHINE_UNKNOWN   :  Result := '<Not specified>';
    IMAGE_FILE_MACHINE_ALPHA     :  Result := 'Alpha AXP™';
    IMAGE_FILE_MACHINE_ARM       :  Result := 'ARM Little-endian';
    IMAGE_FILE_MACHINE_ALPHA64   :  Result := 'Alpha 64';
    IMAGE_FILE_MACHINE_I386      :  Result := 'Intel 386 (or compatible)';
    IMAGE_FILE_MACHINE_IA64      :  Result := 'Intel IA64';
    IMAGE_FILE_MACHINE_M68K      :  Result := 'Motorola 68000';
    IMAGE_FILE_MACHINE_MIPS16    :  Result := 'MIPS16';
    IMAGE_FILE_MACHINE_MIPSFPU   :  Result := 'MIPSFPU';
    IMAGE_FILE_MACHINE_MIPSFPU16 :  Result := 'MIPSFPU16';
    IMAGE_FILE_MACHINE_POWERPC   :  Result := 'Power PC, little endian';
    IMAGE_FILE_MACHINE_R3000     :  Result := 'MIPS little-endian (R3000)';
    IMAGE_FILE_MACHINE_R4000     :  Result := 'MIPS little endian (R4000)';
    IMAGE_FILE_MACHINE_R10000    :  Result := 'MIPS little endian (R10000)';
    IMAGE_FILE_MACHINE_WCEMIPSV2 :  Result := 'MIPS little-endian WCE v2';
    IMAGE_FILE_MACHINE_SH3       :  Result := 'Hitachi SH3';
    IMAGE_FILE_MACHINE_SH3DSP    :  Result := 'Hitachi SH3DSP';
    IMAGE_FILE_MACHINE_SH3E      :  Result := 'Hitachi SH3E';
    IMAGE_FILE_MACHINE_SH4       :  Result := 'Hitachi SH4';
    IMAGE_FILE_MACHINE_SH5       :  Result := 'Hitachi SH5';
    IMAGE_FILE_MACHINE_THUMB     :  Result := 'THUMB';
    IMAGE_FILE_MACHINE_AM33      :  Result := 'AM33';
    IMAGE_FILE_MACHINE_POWERPCFP :  Result := 'IBM PowerPC FP';
    IMAGE_FILE_MACHINE_TRICORE   :  Result := 'TRICORE';
    IMAGE_FILE_MACHINE_CEF       :  Result := 'CEF';
    IMAGE_FILE_MACHINE_EBC       :  Result := 'EFI Byte Code';
    IMAGE_FILE_MACHINE_AMD64     :  Result := 'AMD64';
    IMAGE_FILE_MACHINE_M32R      :  Result := 'M32R Little-endian';
    IMAGE_FILE_MACHINE_CEE       :  Result := 'CEE';
    else
      Result := '<Unknown Machine>';
  end;
end;



function ImageCharFlags2Str( Value: Word ): string;
begin
  Result := '';
  if (Value and IMAGE_FILE_RELOCS_STRIPPED) = IMAGE_FILE_RELOCS_STRIPPED then
    Result := Result + 'Relocs stripped, ';
  if (Value and IMAGE_FILE_EXECUTABLE_IMAGE) = IMAGE_FILE_EXECUTABLE_IMAGE then
    Result := Result + 'Executable, ';
  if (Value and IMAGE_FILE_LINE_NUMS_STRIPPED) = IMAGE_FILE_LINE_NUMS_STRIPPED then
    Result := Result + 'Line nums stripped, ';
  if (Value and IMAGE_FILE_LOCAL_SYMS_STRIPPED) = IMAGE_FILE_LOCAL_SYMS_STRIPPED then
    Result := Result + 'Symbol table stripped, ';
  if (Value and IMAGE_FILE_AGGRESSIVE_WS_TRIM) = IMAGE_FILE_AGGRESSIVE_WS_TRIM then
    Result := Result + 'Aggressive trim w.s., ';
  if (Value and IMAGE_FILE_LARGE_ADDRESS_AWARE) = IMAGE_FILE_LARGE_ADDRESS_AWARE then
    Result := Result + 'Large address, ';
  if (Value and IMAGE_FILE_16BIT_MACHINE) = IMAGE_FILE_16BIT_MACHINE then
    Result := Result + '16 bit, ';
  if (Value and IMAGE_FILE_BYTES_REVERSED_LO) = IMAGE_FILE_BYTES_REVERSED_LO then
    Result := Result + 'Little endian, ';
  if (Value and IMAGE_FILE_BYTES_REVERSED_HI) = IMAGE_FILE_BYTES_REVERSED_HI then
    Result := Result + 'Big endian, ';
  if (Value and IMAGE_FILE_32BIT_MACHINE) = IMAGE_FILE_32BIT_MACHINE then
    Result := Result + '32 bit, ';
  if (Value and IMAGE_FILE_DEBUG_STRIPPED) = IMAGE_FILE_DEBUG_STRIPPED then
    Result := Result + 'Debug Stripped, ';
  if (Value and IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP) = IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP then
    Result := Result + 'Run from swap, ';
  if (Value and IMAGE_FILE_SYSTEM) = IMAGE_FILE_SYSTEM then
    Result := Result + 'System, ';
  if (Value and IMAGE_FILE_DLL) = IMAGE_FILE_DLL then
    Result := Result + 'DLL, ';
  if (Value and IMAGE_FILE_UP_SYSTEM_ONLY) = IMAGE_FILE_UP_SYSTEM_ONLY then
    Result := Result + 'UP machine, ';
  if Result = '' then
    Result := '<No Characteristics>'
  else
    Delete( Result, Length( Result ) - 1, 2 );
end;



function GetImageSybsystem( Value: Word ): string;
begin
  case Value of
    IMAGE_SUBSYSTEM_UNKNOWN         : Result := 'Unknown';
    IMAGE_SUBSYSTEM_NATIVE          : Result := 'Native';
    IMAGE_SUBSYSTEM_WINDOWS_GUI     : Result := 'Windows GUI';
    IMAGE_SUBSYSTEM_WINDOWS_CUI     : Result := 'Windows Console';
    IMAGE_SUBSYSTEM_OS2_CUI         : Result := 'OS/2 Console';
    IMAGE_SUBSYSTEM_POSIX_CUI       : Result := 'Posix Console';
    IMAGE_SUBSYSTEM_NATIVE_WINDOWS  : Result := 'Win 9x Driver';
    IMAGE_SUBSYSTEM_WINDOWS_CE_GUI  : Result := 'Windows CE GUI';
    IMAGE_SUBSYSTEM_EFI_APPLICATION         : Result := 'EFI Application';
    IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER : Result := 'EFI Boot-Service Driver';
    IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER      : Result := 'EFI Runtime Driver';
    IMAGE_SUBSYSTEM_EFI_ROM                 : Result := 'EFI ROM application';
    IMAGE_SUBSYSTEM_XBOX                    : Result := 'XBOX';
    else
      Result := '<Unknown Subsystem>';
  end;
end;



function ImageDllChars2Str( Value: Word ): string;
begin
  Result := '';
  if (Value and IMAGE_DLLCHARACTERISTICS_NO_BIND) = IMAGE_DLLCHARACTERISTICS_NO_BIND then
    Result := Result + 'No bind, ';
  if (Value and IMAGE_DLLCHARACTERISTICS_NO_ISOLATION) = IMAGE_DLLCHARACTERISTICS_NO_ISOLATION then
    Result := Result + 'No isolation, ';
  if (Value and IMAGE_DLLCHARACTERISTICS_NO_SEH) = IMAGE_DLLCHARACTERISTICS_NO_SEH then
    Result := Result + 'No SEH, ';
  if (Value and IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE) = IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE then
    Result := Result + 'Terminal server aware, ';
  if (Value and IMAGE_DLLCHARACTERISTICS_WDM_DRIVER) = IMAGE_DLLCHARACTERISTICS_WDM_DRIVER then
    Result := Result + 'WDM driver, ';
  if (Value and IMAGE_DLLCHARACTERISTICS_X86_THUNK) = IMAGE_DLLCHARACTERISTICS_X86_THUNK then
    Result := Result + 'x86 thunk, ';
  if Result = '' then
    Result := '<No Characteristics>'
  else
    Delete( Result, Length( Result ) - 1, 2 );
end;


function DateTimeStamp2DateTime( Value: DWORD ): TDateTime;
begin
  Result := 719162.666666666 + Value / 86400;
end;


procedure FillTreeView32(ATree: TTreeView; AHeaders: PImageHeaders32);

  function GetItemCap( D: TNodeData ): string;
  var
    S: string;
  begin
    Result := HdrCaptions[ D.ID ];
    S := GetFormattedData(GetDataFromID32(AHeaders, D.ID, D.Idx), D, AHeaders, False);
    if S <> '' then
      Result := Result + ': ' + S;
  end;

  function SN2Str(SN: PByteArray): string;
  var
    s: AnsiString;
  begin
    SetLength(s, 8);
    Move(SN^, s[1], 8);
    Result := TrimRight(string(s));
  end;

var
  csr, SR, DH_RT, itm, NH_RT, IH_RT, OH_RT, DD_RT, DD_RT2: TTreeNode;
  nd: TNodeData;
  I: Integer;
  SH: PSectionCopy;
  SL: TList;
begin
  nd.AsDWORD := 0;

  // Image Dos Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IDH_ROOT;
  DH_RT := ATree.Items.Insert(nil, GetItemCap( nd ) );
  DH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MAGIC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CBLP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CRLC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CPARHDR;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MINALLOC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MAXALLOC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_SS;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_SP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CSUM;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_IP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CS;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_LFARLC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OVNO;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 4;
  nd.ID := ID_IDH_E_RES;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OEMID;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OEMINFO;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 10;
  nd.ID := ID_IDH_E_RES2;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IDH_E_LFANEW;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image NT Headers

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_INH_ROOT;
  NH_RT := ATree.Items.Add( DH_RT, GetItemCap( nd ) );
  NH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_INH_SIGNATURE;
  itm := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image File Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IFH_ROOT;
  IH_RT := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  IH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_MACHINE;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IFH_NUMBEROFSECTIONS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DATETIME or TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_TIMEDATESTAMP;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_POINTERTOSYMBOLTABLE;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_NUMBEROFSYMBOLS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_SIZEOFOPTIONALHEADER;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_CHARACTERISTICS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image Optional Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IOH_ROOT;
  OH_RT := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  OH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAGIC;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJORLINKERVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_MINORLINKERVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFCODE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFINITIALIZEDDATA;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFUNINITIALIZEDDATA;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_ADDRESSOFENTRYPOINT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_BASEOFCODE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_BASEOFDATA;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_IMAGEBASE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SECTIONALIGNMENT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_FILEALIGNMENT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJOROPERATINGSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MINOROPERATINGSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJORIMAGEVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MINORSUBSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_WIN32VERSIONVALUE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFIMAGE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEADERS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_CHECKSUM;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_SUBSYSTEM;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_DLLCHARACTERISTICS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFSTACKRESERVE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFSTACKCOMMIT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEAPRESERVE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEAPCOMMIT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_LOADERFLAGS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4 or TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_NUMBEROFRVAANDSIZES;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IOH_DATADIRECTORY;
  DD_RT := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  DD_RT.Data := nd.AsPointer;

  // Data Directories

  for I := 0 to 15 do
  begin
    nd.Fmt := 0;
    nd.Count := 0;
    nd.ID := I + ID_IDD_EXPORT;
    DD_RT2 := ATree.Items.AddChild( DD_RT, GetItemCap( nd ) );
    DD_RT2.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4 or TVI_FMT_HEX;
    nd.Count := 1;
    nd.ID := ID_IDD_VIRTUALADDRESS;
    nd.Idx := I;
    itm := ATree.Items.AddChild( DD_RT2, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4 or TVI_FMT_HEX;
    nd.Count := 1;
    nd.ID := ID_IDD_SIZE;
    nd.Idx := I;
    itm := ATree.Items.AddChild( DD_RT2, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;
  end;

  NH_RT.Expanded := True;

  // Sections

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_ISH_ROOTCAPTION;
  nd.Idx := 0;
  SR := ATree.Items.Add(DH_RT, GetItemCap(nd));
  SR.Data := nd.AsPointer;

  SL := AHeaders.RawSections;
  for I := 0 to SL.Count - 1 do
  begin
    SH := SL.Items[ I ];

    nd.Fmt := TVI_FMT_CHAR;
    nd.Count := 8;
    nd.ID := ID_ISH_NAME;
    nd.Idx := I;
    csr := ATree.Items.AddChild(SR, SN2Str(@SH.RawHeaderStruct.Name));
    csr.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_MISC;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_VIRTUALADDRESS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_SIZEOFRAWDATA;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTORAWDATA;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTORELOCATIONS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTOLINENUMBERS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
    nd.Count := 1;
    nd.ID := ID_ISH_NUMBEROFRELOCATIONS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
    nd.Count := 1;
    nd.ID := ID_ISH_NUMBEROFLINENUMBERS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_CHARACTERISTICS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;
  end;
  sr.Expanded := True;

end;

procedure FillTreeView64(ATree: TTreeView; AHeaders: PImageHeaders64);

  function GetItemCap( D: TNodeData ): string;
  var
    S: string;
  begin
    Result := HdrCaptions[ D.ID ];
    S := GetFormattedData(GetDataFromID64(AHeaders, D.ID, D.Idx ), D, AHeaders, False);
    if S <> '' then
      Result := Result + ': ' + S;
  end;

  function SN2Str(SN: PByteArray): string;
  var
    s: AnsiString;
  begin
    SetLength(s, 8);
    Move(SN^, s[1], 8);
    Result := TrimRight(string(s));
  end;

var
  csr, SR, DH_RT, itm, NH_RT, IH_RT, OH_RT, DD_RT, DD_RT2: TTreeNode;
  nd: TNodeData;
  I: Integer;
  SH: PSectionCopy;
  SL: TList;
begin
  nd.AsDWORD := 0;

  // Image Dos Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IDH_ROOT;
  DH_RT := ATree.Items.Insert(nil, GetItemCap( nd ) );
  DH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MAGIC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CBLP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CRLC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CPARHDR;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MINALLOC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_MAXALLOC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_SS;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_SP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CSUM;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_IP;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_CS;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_LFARLC;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OVNO;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 4;
  nd.ID := ID_IDH_E_RES;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OEMID;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IDH_E_OEMINFO;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 10;
  nd.ID := ID_IDH_E_RES2;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IDH_E_LFANEW;
  itm := ATree.Items.AddChild( DH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image NT Headers

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_INH_ROOT;
  NH_RT := ATree.Items.Add( DH_RT, GetItemCap( nd ) );
  NH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_INH_SIGNATURE;
  itm := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image File Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IFH_ROOT;
  IH_RT := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  IH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_MACHINE;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IFH_NUMBEROFSECTIONS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DATETIME or TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_TIMEDATESTAMP;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_POINTERTOSYMBOLTABLE;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IFH_NUMBEROFSYMBOLS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_SIZEOFOPTIONALHEADER;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IFH_CHARACTERISTICS;
  itm := ATree.Items.AddChild( IH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  // Image Optional Header

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IOH_ROOT;
  OH_RT := ATree.Items.AddChild( NH_RT, GetItemCap( nd ) );
  OH_RT.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAGIC;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJORLINKERVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_MINORLINKERVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFCODE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFINITIALIZEDDATA;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFUNINITIALIZEDDATA;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_ADDRESSOFENTRYPOINT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_BASEOFCODE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

//  nd.Fmt := TVI_FMT_S4;
//  nd.Count := 1;
//  nd.ID := ID_IOH_BASEOFDATA;
//  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
//  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S8;
  nd.Count := 1;
  nd.ID := ID_IOH_IMAGEBASE;
  itm := ATree.Items.AddChild(OH_RT, GetItemCap(nd));
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SECTIONALIGNMENT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_FILEALIGNMENT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJOROPERATINGSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MINOROPERATINGSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MAJORIMAGEVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_MINORSUBSYSTEMVERSION;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_WIN32VERSIONVALUE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFIMAGE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEADERS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_CHECKSUM;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_SUBSYSTEM;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S2;
  nd.Count := 1;
  nd.ID := ID_IOH_DLLCHARACTERISTICS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S8;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFSTACKRESERVE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S8;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFSTACKCOMMIT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S8;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEAPRESERVE;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S8;
  nd.Count := 1;
  nd.ID := ID_IOH_SIZEOFHEAPCOMMIT;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4;
  nd.Count := 1;
  nd.ID := ID_IOH_LOADERFLAGS;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := TVI_FMT_S4 or TVI_FMT_DEC;
  nd.Count := 1;
  nd.ID := ID_IOH_NUMBEROFRVAANDSIZES;
  itm := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  itm.Data := nd.AsPointer;

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_IOH_DATADIRECTORY;
  DD_RT := ATree.Items.AddChild( OH_RT, GetItemCap( nd ) );
  DD_RT.Data := nd.AsPointer;

  // Data Directories

  for I := 0 to 15 do
  begin
    nd.Fmt := 0;
    nd.Count := 0;
    nd.ID := I + ID_IDD_EXPORT;
    DD_RT2 := ATree.Items.AddChild( DD_RT, GetItemCap( nd ) );
    DD_RT2.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4 or TVI_FMT_HEX;
    nd.Count := 1;
    nd.ID := ID_IDD_VIRTUALADDRESS;
    nd.Idx := I;
    itm := ATree.Items.AddChild( DD_RT2, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4 or TVI_FMT_HEX;
    nd.Count := 1;
    nd.ID := ID_IDD_SIZE;
    nd.Idx := I;
    itm := ATree.Items.AddChild( DD_RT2, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;
  end;

  NH_RT.Expanded := True;

  // Sections

  nd.Fmt := 0;
  nd.Count := 0;
  nd.ID := ID_ISH_ROOTCAPTION;
  nd.Idx := 0;
  SR := ATree.Items.Add(DH_RT, GetItemCap(nd));
  SR.Data := nd.AsPointer;

  SL := AHeaders.RawSections;
  for I := 0 to SL.Count - 1 do
  begin
    SH := SL.Items[ I ];

    nd.Fmt := TVI_FMT_CHAR;
    nd.Count := 8;
    nd.ID := ID_ISH_NAME;
    nd.Idx := I;
    csr := ATree.Items.AddChild(SR, SN2Str(@SH.RawHeaderStruct.Name));
    csr.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_MISC;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_VIRTUALADDRESS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_SIZEOFRAWDATA;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTORAWDATA;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTORELOCATIONS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_POINTERTOLINENUMBERS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
    nd.Count := 1;
    nd.ID := ID_ISH_NUMBEROFRELOCATIONS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S2 or TVI_FMT_DEC;
    nd.Count := 1;
    nd.ID := ID_ISH_NUMBEROFLINENUMBERS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;

    nd.Fmt := TVI_FMT_S4;
    nd.Count := 1;
    nd.ID := ID_ISH_CHARACTERISTICS;
    itm := ATree.Items.AddChild( csr, GetItemCap( nd ) );
    itm.Data := nd.AsPointer;
  end;

  sr.Expanded := True;
end;



function GetDataFromID32(Hdrs: PImageHeaders32; ID, Idx: Byte): Pointer;
begin
  case ID of
    ID_IDH_E_MAGIC: Result := @Hdrs.Dos.e_magic;
    ID_IDH_E_CBLP: Result := @Hdrs.Dos.e_cblp;
    ID_IDH_E_CP: Result := @Hdrs.Dos.e_cp;
    ID_IDH_E_CRLC: Result := @Hdrs.Dos.e_crlc;
    ID_IDH_E_CPARHDR: Result := @Hdrs.Dos.e_cparhdr;
    ID_IDH_E_MINALLOC: Result := @Hdrs.Dos.e_minalloc;
    ID_IDH_E_MAXALLOC: Result := @Hdrs.Dos.e_maxalloc;
    ID_IDH_E_SS: Result := @Hdrs.Dos.e_ss;
    ID_IDH_E_SP: Result := @Hdrs.Dos.e_sp;
    ID_IDH_E_CSUM: Result := @Hdrs.Dos.e_csum;
    ID_IDH_E_IP: Result := @Hdrs.Dos.e_ip;
    ID_IDH_E_CS: Result := @Hdrs.Dos.e_cs;
    ID_IDH_E_LFARLC: Result := @Hdrs.Dos.e_lfarlc;
    ID_IDH_E_OVNO: Result := @Hdrs.Dos.e_ovno;
    ID_IDH_E_RES: Result := @Hdrs.Dos.e_res;
    ID_IDH_E_OEMID: Result := @Hdrs.Dos.e_oemid;
    ID_IDH_E_OEMINFO: Result := @Hdrs.Dos.e_oeminfo;
    ID_IDH_E_RES2: Result := @Hdrs.Dos.e_res2;
    ID_IDH_E_LFANEW: Result := @Hdrs.Dos._lfanew;

    ID_IFH_MACHINE: Result := @Hdrs.NT.FileHeader.Machine;
    ID_IFH_NUMBEROFSECTIONS: Result := @Hdrs.NT.FileHeader.NumberOfSections;
    ID_IFH_TIMEDATESTAMP: Result := @Hdrs.NT.FileHeader.TimeDateStamp;
    ID_IFH_POINTERTOSYMBOLTABLE: Result := @Hdrs.NT.FileHeader.PointerToSymbolTable;
    ID_IFH_NUMBEROFSYMBOLS: Result := @Hdrs.NT.FileHeader.NumberOfSymbols;
    ID_IFH_SIZEOFOPTIONALHEADER: Result := @Hdrs.NT.FileHeader.SizeOfOptionalHeader;
    ID_IFH_CHARACTERISTICS: Result := @Hdrs.NT.FileHeader.Characteristics;

    ID_IOH_MAGIC: Result := @Hdrs.NT.OptionalHeader.Magic;
    ID_IOH_MAJORLINKERVERSION: Result := @Hdrs.NT.OptionalHeader.MajorLinkerVersion;
    ID_IOH_MINORLINKERVERSION: Result := @Hdrs.NT.OptionalHeader.MinorLinkerVersion;
    ID_IOH_SIZEOFCODE: Result := @Hdrs.NT.OptionalHeader.SizeOfCode;
    ID_IOH_SIZEOFINITIALIZEDDATA: Result := @Hdrs.NT.OptionalHeader.SizeOfInitializedData;
    ID_IOH_SIZEOFUNINITIALIZEDDATA: Result := @Hdrs.NT.OptionalHeader.SizeOfUninitializedData;
    ID_IOH_ADDRESSOFENTRYPOINT: Result := @Hdrs.NT.OptionalHeader.AddressOfEntryPoint;
    ID_IOH_BASEOFCODE: Result := @Hdrs.NT.OptionalHeader.BaseOfCode;
    ID_IOH_BASEOFDATA: Result := @Hdrs.NT.OptionalHeader.BaseOfData;
    ID_IOH_IMAGEBASE: Result := @Hdrs.NT.OptionalHeader.ImageBase;
    ID_IOH_SECTIONALIGNMENT: Result := @Hdrs.NT.OptionalHeader.SectionAlignment;
    ID_IOH_FILEALIGNMENT: Result := @Hdrs.NT.OptionalHeader.FileAlignment;
    ID_IOH_MAJOROPERATINGSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MajorOperatingSystemVersion;
    ID_IOH_MINOROPERATINGSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MinorOperatingSystemVersion;
    ID_IOH_MAJORIMAGEVERSION: Result := @Hdrs.NT.OptionalHeader.MajorImageVersion;
    ID_IOH_MINORIMAGEVERSION: Result := @Hdrs.NT.OptionalHeader.MinorImageVersion;
    ID_IOH_MAJORSUBSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MajorSubsystemVersion;
    ID_IOH_MINORSUBSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MinorSubsystemVersion;
    ID_IOH_WIN32VERSIONVALUE: Result := @Hdrs.NT.OptionalHeader.Win32VersionValue;
    ID_IOH_SIZEOFIMAGE: Result := @Hdrs.NT.OptionalHeader.SizeOfImage;
    ID_IOH_SIZEOFHEADERS: Result := @Hdrs.NT.OptionalHeader.SizeOfHeaders;
    ID_IOH_CHECKSUM: Result := @Hdrs.NT.OptionalHeader.CheckSum;
    ID_IOH_SUBSYSTEM: Result := @Hdrs.NT.OptionalHeader.Subsystem;
    ID_IOH_DLLCHARACTERISTICS: Result := @Hdrs.NT.OptionalHeader.DllCharacteristics;
    ID_IOH_SIZEOFSTACKRESERVE: Result := @Hdrs.NT.OptionalHeader.SizeOfStackReserve;
    ID_IOH_SIZEOFSTACKCOMMIT: Result := @Hdrs.NT.OptionalHeader.SizeOfStackCommit;
    ID_IOH_SIZEOFHEAPRESERVE: Result := @Hdrs.NT.OptionalHeader.SizeOfHeapReserve;
    ID_IOH_SIZEOFHEAPCOMMIT: Result := @Hdrs.NT.OptionalHeader.SizeOfHeapCommit;
    ID_IOH_LOADERFLAGS: Result := @Hdrs.NT.OptionalHeader.LoaderFlags;
    ID_IOH_NUMBEROFRVAANDSIZES: Result := @Hdrs.NT.OptionalHeader.NumberOfRvaAndSizes;
//    ID_IOH_DATADIRECTORY: Result := @Hdrs.NT.OptionalHeader.DataDirectory;



    ID_INH_SIGNATURE: Result := @Hdrs.NT.Signature;

    ID_ISH_NAME: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).Name;
    ID_ISH_MISC: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).Misc;
    ID_ISH_VIRTUALADDRESS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).VirtualAddress;
    ID_ISH_SIZEOFRAWDATA: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).SizeOfRawData;
    ID_ISH_POINTERTORAWDATA: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).PointerToRawData;
    ID_ISH_POINTERTORELOCATIONS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).PointerToRelocations;
    ID_ISH_POINTERTOLINENUMBERS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).PointerToLinenumbers;
    ID_ISH_NUMBEROFRELOCATIONS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).NumberOfRelocations;
    ID_ISH_NUMBEROFLINENUMBERS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).NumberOfLinenumbers;
    ID_ISH_CHARACTERISTICS: Result := @PImageSectionHeader(Hdrs.RawSections[Idx]).Characteristics;

    ID_IDD_VIRTUALADDRESS: Result := @Hdrs.NT.OptionalHeader.DataDirectory[Idx].VirtualAddress;
    ID_IDD_SIZE: Result := @Hdrs.NT.OptionalHeader.DataDirectory[Idx].SIZE;
  else
    Result := nil;
  end;
end;

function GetDataFromID64(Hdrs: PImageHeaders64; ID, Idx: Byte): Pointer;
begin
  case ID of
    ID_IDH_E_MAGIC: Result := @Hdrs.Dos.e_magic;
    ID_IDH_E_CBLP: Result := @Hdrs.Dos.e_cblp;
    ID_IDH_E_CP: Result := @Hdrs.Dos.e_cp;
    ID_IDH_E_CRLC: Result := @Hdrs.Dos.e_crlc;
    ID_IDH_E_CPARHDR: Result := @Hdrs.Dos.e_cparhdr;
    ID_IDH_E_MINALLOC: Result := @Hdrs.Dos.e_minalloc;
    ID_IDH_E_MAXALLOC: Result := @Hdrs.Dos.e_maxalloc;
    ID_IDH_E_SS: Result := @Hdrs.Dos.e_ss;
    ID_IDH_E_SP: Result := @Hdrs.Dos.e_sp;
    ID_IDH_E_CSUM: Result := @Hdrs.Dos.e_csum;
    ID_IDH_E_IP: Result := @Hdrs.Dos.e_ip;
    ID_IDH_E_CS: Result := @Hdrs.Dos.e_cs;
    ID_IDH_E_LFARLC: Result := @Hdrs.Dos.e_lfarlc;
    ID_IDH_E_OVNO: Result := @Hdrs.Dos.e_ovno;
    ID_IDH_E_RES: Result := @Hdrs.Dos.e_res;
    ID_IDH_E_OEMID: Result := @Hdrs.Dos.e_oemid;
    ID_IDH_E_OEMINFO: Result := @Hdrs.Dos.e_oeminfo;
    ID_IDH_E_RES2: Result := @Hdrs.Dos.e_res2;
    ID_IDH_E_LFANEW: Result := @Hdrs.Dos._lfanew;

    ID_IFH_MACHINE: Result := @Hdrs.NT.FileHeader.Machine;
    ID_IFH_NUMBEROFSECTIONS: Result := @Hdrs.NT.FileHeader.NumberOfSections;
    ID_IFH_TIMEDATESTAMP: Result := @Hdrs.NT.FileHeader.TimeDateStamp;
    ID_IFH_POINTERTOSYMBOLTABLE: Result := @Hdrs.NT.FileHeader.PointerToSymbolTable;
    ID_IFH_NUMBEROFSYMBOLS: Result := @Hdrs.NT.FileHeader.NumberOfSymbols;
    ID_IFH_SIZEOFOPTIONALHEADER: Result := @Hdrs.NT.FileHeader.SizeOfOptionalHeader;
    ID_IFH_CHARACTERISTICS: Result := @Hdrs.NT.FileHeader.Characteristics;

    ID_IOH_MAGIC: Result := @Hdrs.NT.OptionalHeader.Magic;
    ID_IOH_MAJORLINKERVERSION: Result := @Hdrs.NT.OptionalHeader.MajorLinkerVersion;
    ID_IOH_MINORLINKERVERSION: Result := @Hdrs.NT.OptionalHeader.MinorLinkerVersion;
    ID_IOH_SIZEOFCODE: Result := @Hdrs.NT.OptionalHeader.SizeOfCode;
    ID_IOH_SIZEOFINITIALIZEDDATA: Result := @Hdrs.NT.OptionalHeader.SizeOfInitializedData;
    ID_IOH_SIZEOFUNINITIALIZEDDATA: Result := @Hdrs.NT.OptionalHeader.SizeOfUninitializedData;
    ID_IOH_ADDRESSOFENTRYPOINT: Result := @Hdrs.NT.OptionalHeader.AddressOfEntryPoint;
    ID_IOH_BASEOFCODE: Result := @Hdrs.NT.OptionalHeader.BaseOfCode;
//    ID_IOH_BASEOFDATA: Result := @Hdrs.NT.OptionalHeader.BaseOfData;
    ID_IOH_IMAGEBASE: Result := @Hdrs.NT.OptionalHeader.ImageBase;
    ID_IOH_SECTIONALIGNMENT: Result := @Hdrs.NT.OptionalHeader.SectionAlignment;
    ID_IOH_FILEALIGNMENT: Result := @Hdrs.NT.OptionalHeader.FileAlignment;
    ID_IOH_MAJOROPERATINGSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MajorOperatingSystemVersion;
    ID_IOH_MINOROPERATINGSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MinorOperatingSystemVersion;
    ID_IOH_MAJORIMAGEVERSION: Result := @Hdrs.NT.OptionalHeader.MajorImageVersion;
    ID_IOH_MINORIMAGEVERSION: Result := @Hdrs.NT.OptionalHeader.MinorImageVersion;
    ID_IOH_MAJORSUBSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MajorSubsystemVersion;
    ID_IOH_MINORSUBSYSTEMVERSION: Result := @Hdrs.NT.OptionalHeader.MinorSubsystemVersion;
    ID_IOH_WIN32VERSIONVALUE: Result := @Hdrs.NT.OptionalHeader.Win32VersionValue;
    ID_IOH_SIZEOFIMAGE: Result := @Hdrs.NT.OptionalHeader.SizeOfImage;
    ID_IOH_SIZEOFHEADERS: Result := @Hdrs.NT.OptionalHeader.SizeOfHeaders;
    ID_IOH_CHECKSUM: Result := @Hdrs.NT.OptionalHeader.CheckSum;
    ID_IOH_SUBSYSTEM: Result := @Hdrs.NT.OptionalHeader.Subsystem;
    ID_IOH_DLLCHARACTERISTICS: Result := @Hdrs.NT.OptionalHeader.DllCharacteristics;
    ID_IOH_SIZEOFSTACKRESERVE: Result := @Hdrs.NT.OptionalHeader.SizeOfStackReserve;
    ID_IOH_SIZEOFSTACKCOMMIT: Result := @Hdrs.NT.OptionalHeader.SizeOfStackCommit;
    ID_IOH_SIZEOFHEAPRESERVE: Result := @Hdrs.NT.OptionalHeader.SizeOfHeapReserve;
    ID_IOH_SIZEOFHEAPCOMMIT: Result := @Hdrs.NT.OptionalHeader.SizeOfHeapCommit;
    ID_IOH_LOADERFLAGS: Result := @Hdrs.NT.OptionalHeader.LoaderFlags;
    ID_IOH_NUMBEROFRVAANDSIZES: Result := @Hdrs.NT.OptionalHeader.NumberOfRvaAndSizes;
//    ID_IOH_DATADIRECTORY: Result := @Hdrs.NT.OptionalHeader.DataDirectory;



    ID_INH_SIGNATURE: Result := @Hdrs.NT.Signature;

    ID_ISH_NAME: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.Name;
    ID_ISH_MISC: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.Misc;
    ID_ISH_VIRTUALADDRESS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.VirtualAddress;
    ID_ISH_SIZEOFRAWDATA: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.SizeOfRawData;
    ID_ISH_POINTERTORAWDATA: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.PointerToRawData;
    ID_ISH_POINTERTORELOCATIONS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.PointerToRelocations;
    ID_ISH_POINTERTOLINENUMBERS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.PointerToLinenumbers;
    ID_ISH_NUMBEROFRELOCATIONS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.NumberOfRelocations;
    ID_ISH_NUMBEROFLINENUMBERS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.NumberOfLinenumbers;
    ID_ISH_CHARACTERISTICS: Result := @PSectionCopy(Hdrs.RawSections[Idx]).RawHeaderStruct.Characteristics;

    ID_IDD_VIRTUALADDRESS: Result := @Hdrs.NT.OptionalHeader.DataDirectory[ Idx ].VirtualAddress;
    ID_IDD_SIZE: Result := @Hdrs.NT.OptionalHeader.DataDirectory[ Idx ].SIZE;
  else
    Result := nil;
  end;
end;



{function GetOffsetFromID(ID, Idx: Byte; Hdrs: PImageHeaders): Cardinal;

  function idho( Ptr: Pointer ): Cardinal;
  begin
    Result := Cardinal(Ptr) - Cardinal(@Hdrs^.Dos);
  end;

  function inho( Ptr: Pointer ): Cardinal;
  begin
    Result := Cardinal(Ptr) - Cardinal(@Hdrs^.NT) + Cardinal(Hdrs^.Dos._lfanew);
  end;

  function isho( lID, Idx: Byte ): Cardinal;
  var
    tms: TImageSectionHeader;
    tmp: Pointer;
  begin
    case lID of
      ID_ISH_NAME: tmp := @tms.Name;
      ID_ISH_MISC: tmp := @tms.Misc;
      ID_ISH_VIRTUALADDRESS: tmp := @tms.VirtualAddress;
      ID_ISH_SIZEOFRAWDATA: tmp := @tms.SizeOfRawData;
      ID_ISH_POINTERTORAWDATA: tmp := @tms.PointerToRawData;
      ID_ISH_POINTERTORELOCATIONS: tmp := @tms.PointerToRelocations;
      ID_ISH_POINTERTOLINENUMBERS: tmp := @tms.PointerToLinenumbers;
      ID_ISH_NUMBEROFRELOCATIONS: tmp := @tms.NumberOfRelocations;
      ID_ISH_NUMBEROFLINENUMBERS: tmp := @tms.NumberOfLinenumbers;
      ID_ISH_CHARACTERISTICS: tmp := @tms.Characteristics;
      else
        tmp := nil;
    end;
    Result := Cardinal(Hdrs^.Dos._lfanew) + SizeOf( TImageNTHeaders ) +
      SizeOf( TImageSectionHeader ) * Idx + (Cardinal(tmp) - Cardinal(@tms));
  end;

  function iddo( Ptr: Pointer ): Cardinal;
  begin
    Result := Cardinal(Ptr) - Cardinal(@Hdrs^.NT) + Cardinal(Hdrs^.Dos._lfanew);
  end;

begin
  case ID of
    ID_IDH_E_MAGIC: Result := idho( @Hdrs.Dos.e_magic );
    ID_IDH_E_CBLP: Result := idho( @Hdrs.Dos.e_cblp );
    ID_IDH_E_CP: Result := idho( @Hdrs.Dos.e_cp );
    ID_IDH_E_CRLC: Result := idho( @Hdrs.Dos.e_crlc );
    ID_IDH_E_CPARHDR: Result := idho( @Hdrs.Dos.e_cparhdr );
    ID_IDH_E_MINALLOC: Result := idho( @Hdrs.Dos.e_minalloc );
    ID_IDH_E_MAXALLOC: Result := idho( @Hdrs.Dos.e_maxalloc );
    ID_IDH_E_SS: Result := idho( @Hdrs.Dos.e_ss );
    ID_IDH_E_SP: Result := idho( @Hdrs.Dos.e_sp );
    ID_IDH_E_CSUM: Result := idho( @Hdrs.Dos.e_csum );
    ID_IDH_E_IP: Result := idho( @Hdrs.Dos.e_ip );
    ID_IDH_E_CS: Result := idho( @Hdrs.Dos.e_cs );
    ID_IDH_E_LFARLC: Result := idho( @Hdrs.Dos.e_lfarlc );
    ID_IDH_E_OVNO: Result := idho( @Hdrs.Dos.e_ovno );
    ID_IDH_E_RES: Result := idho( @Hdrs.Dos.e_res );
    ID_IDH_E_OEMID: Result := idho( @Hdrs.Dos.e_oemid );
    ID_IDH_E_OEMINFO: Result := idho( @Hdrs.Dos.e_oeminfo );
    ID_IDH_E_RES2: Result := idho( @Hdrs.Dos.e_res2 );
    ID_IDH_E_LFANEW: Result := idho( @Hdrs.Dos._lfanew );

    ID_IFH_MACHINE: Result := inho( @Hdrs.NT.FileHeader.Machine );
    ID_IFH_NUMBEROFSECTIONS: Result := inho( @Hdrs.NT.FileHeader.NumberOfSections );
    ID_IFH_TIMEDATESTAMP: Result := inho( @Hdrs.NT.FileHeader.TimeDateStamp );
    ID_IFH_POINTERTOSYMBOLTABLE: Result := inho( @Hdrs.NT.FileHeader.PointerToSymbolTable );
    ID_IFH_NUMBEROFSYMBOLS: Result := inho( @Hdrs.NT.FileHeader.NumberOfSymbols );
    ID_IFH_SIZEOFOPTIONALHEADER: Result := inho( @Hdrs.NT.FileHeader.SizeOfOptionalHeader );
    ID_IFH_CHARACTERISTICS: Result := inho( @Hdrs.NT.FileHeader.Characteristics );

    ID_IOH_MAGIC: Result := inho( @Hdrs.NT.OptionalHeader.Magic );
    ID_IOH_MAJORLINKERVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MajorLinkerVersion );
    ID_IOH_MINORLINKERVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MinorLinkerVersion );
    ID_IOH_SIZEOFCODE: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfCode );
    ID_IOH_SIZEOFINITIALIZEDDATA: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfInitializedData );
    ID_IOH_SIZEOFUNINITIALIZEDDATA: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfUninitializedData );
    ID_IOH_ADDRESSOFENTRYPOINT: Result := inho( @Hdrs.NT.OptionalHeader.AddressOfEntryPoint );
    ID_IOH_BASEOFCODE: Result := inho( @Hdrs.NT.OptionalHeader.BaseOfCode );
    ID_IOH_BASEOFDATA: Result := inho( @Hdrs.NT.OptionalHeader.BaseOfData );
    ID_IOH_IMAGEBASE: Result := inho( @Hdrs.NT.OptionalHeader.ImageBase );
    ID_IOH_SECTIONALIGNMENT: Result := inho( @Hdrs.NT.OptionalHeader.SectionAlignment );
    ID_IOH_FILEALIGNMENT: Result := inho( @Hdrs.NT.OptionalHeader.FileAlignment );
    ID_IOH_MAJOROPERATINGSYSTEMVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MajorOperatingSystemVersion );
    ID_IOH_MINOROPERATINGSYSTEMVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MinorOperatingSystemVersion );
    ID_IOH_MAJORIMAGEVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MajorImageVersion );
    ID_IOH_MINORIMAGEVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MinorImageVersion );
    ID_IOH_MAJORSUBSYSTEMVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MajorSubsystemVersion );
    ID_IOH_MINORSUBSYSTEMVERSION: Result := inho( @Hdrs.NT.OptionalHeader.MinorSubsystemVersion );
    ID_IOH_WIN32VERSIONVALUE: Result := inho( @Hdrs.NT.OptionalHeader.Win32VersionValue );
    ID_IOH_SIZEOFIMAGE: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfImage );
    ID_IOH_SIZEOFHEADERS: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfHeaders );
    ID_IOH_CHECKSUM: Result := inho( @Hdrs.NT.OptionalHeader.CheckSum );
    ID_IOH_SUBSYSTEM: Result := inho( @Hdrs.NT.OptionalHeader.Subsystem );
    ID_IOH_DLLCHARACTERISTICS: Result := inho( @Hdrs.NT.OptionalHeader.DllCharacteristics );
    ID_IOH_SIZEOFSTACKRESERVE: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfStackReserve );
    ID_IOH_SIZEOFSTACKCOMMIT: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfStackCommit );
    ID_IOH_SIZEOFHEAPRESERVE: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfHeapReserve );
    ID_IOH_SIZEOFHEAPCOMMIT: Result := inho( @Hdrs.NT.OptionalHeader.SizeOfHeapCommit );
    ID_IOH_LOADERFLAGS: Result := inho( @Hdrs.NT.OptionalHeader.LoaderFlags );
    ID_IOH_NUMBEROFRVAANDSIZES: Result := inho( @Hdrs.NT.OptionalHeader.NumberOfRvaAndSizes );
//    ID_IOH_DATADIRECTORY: Result := inho( @Hdrs.NT.OptionalHeader.DataDirectory );



    ID_INH_SIGNATURE: Result := inho( @Hdrs.NT.Signature );

    ID_ISH_NAME: Result := isho( ID, Idx );
    ID_ISH_MISC: Result := isho( ID, Idx );
    ID_ISH_VIRTUALADDRESS: Result := isho( ID, Idx );
    ID_ISH_SIZEOFRAWDATA: Result := isho( ID, Idx );
    ID_ISH_POINTERTORAWDATA: Result := isho( ID, Idx );
    ID_ISH_POINTERTORELOCATIONS: Result := isho( ID, Idx );
    ID_ISH_POINTERTOLINENUMBERS: Result := isho( ID, Idx );
    ID_ISH_NUMBEROFRELOCATIONS: Result := isho( ID, Idx );
    ID_ISH_NUMBEROFLINENUMBERS: Result := isho( ID, Idx );
    ID_ISH_CHARACTERISTICS: Result := isho( ID, Idx );

    ID_IDD_VIRTUALADDRESS: Result := iddo( @Hdrs.NT.OptionalHeader.DataDirectory[ Idx ].VirtualAddress );
    ID_IDD_SIZE: Result := iddo( @Hdrs.NT.OptionalHeader.DataDirectory[ Idx ].Size );
  else
    Result := 0;
  end;
end;}



function GetFormattedData(
  Data: Pointer; Format: TNodeData; Hdrs: Pointer; StdData: Boolean): string;

  function Item2Str( Ptr: Pointer; Sz, Fmt: Byte; Sgn: Boolean ): string;
  var
    tm: UInt64;
    I: Integer;
    S: string;
  begin
    Result := '';
    if Ptr = nil then Exit;
    case Fmt of
      TVI_FMT_HEX:
      begin
        case Sz of
          TVI_FMT_S1: tm := PByte(Ptr)^;
          TVI_FMT_S2: tm := PWORD(Ptr)^;
          TVI_FMT_S4: tm := PDWORD(Ptr)^;
          TVI_FMT_S8: tm := PUInt64(Ptr)^;
          else
            tm := 0;
        end;
        if Sgn then
        begin
          case Sz of
            TVI_FMT_S1: Int64(tm) := -PShortInt(Ptr)^;
            TVI_FMT_S2: Int64(tm) := -PSmallInt(Ptr)^;
            TVI_FMT_S4: Int64(tm) := -PLongInt(Ptr)^;
            TVI_FMT_S8: Int64(tm) := -PInt64(Ptr)^;
          end;
        end;
        Result := IntToHex( tm, Sz2Val[ Sz ] * 2 );
        if Sgn then Result := '-' + Result;
      end;

      TVI_FMT_DEC:
      begin
        if Sgn then
          case Sz of
            TVI_FMT_S1: Result := IntToStr( PShortInt(Ptr)^ );
            TVI_FMT_S2: Result := IntToStr( PSmallInt(Ptr)^ );
            TVI_FMT_S4: Result := IntToStr( PLongInt(Ptr)^ );
            TVI_FMT_S8: Result := IntToStr(PInt64(Ptr)^);
          end
        else
          case Sz of
            TVI_FMT_S1: Result := IntToStr( PByte(Ptr)^ );
            TVI_FMT_S2: Result := IntToStr( PWORD(Ptr)^ );
            TVI_FMT_S4: Result := IntToStr( PDWORD(Ptr)^ );
            TVI_FMT_S8: Result := IntToStr(PUINT64(Ptr)^);
          end;
      end;

      TVI_FMT_CHAR:
      begin
        for I := 0 to Sz2Val[ Sz ] - 1 do
        begin
          if (PByte(Ptr)^ < 32) then
            S := '#' + IntToStr( PByte(Ptr)^ )
          else
            S := PChar(Ptr)^;
          Result := Result + S;
          Inc(PChar(Ptr));
        end;
      end;

      TVI_FMT_DATETIME:
        Result := DateTimeToStr( DateTimeStamp2DateTime( PCardinal(Ptr)^ ) );
    end;
  end;

  function GetStdData: string;
  var
    Sz, Fmt: Byte;
    Sgn: Boolean;
    I, IM: Integer;
    S: string;
  begin
    Sz := Format.AsDWORD and TVI_FMT_SZMASK;
    Sgn := Boolean(CBool( Format.AsDWORD and TVI_FMT_SIGNMASK ));
    Fmt := Format.AsDWORD and TVI_FMT_TYPEMASK;

    Result := '';
    IM := Format.Count - 1;
    for I := 0 to IM do
    begin
      S := Item2Str( Data, Sz, Fmt, Sgn );
      case Fmt of
        TVI_FMT_HEX:
          if I > 0 then
            Result := Result + ' ' + S
          else
            if Format.Count = 1 then
              Result := '0x' + S
            else
              Result := '0x ' + S;
        TVI_FMT_DEC:
          if I > 0 then
            Result := Result + ', ' + S
          else
            Result := S;
        TVI_FMT_CHAR:
          begin
            if (I = 0) {or (I = IM)} then begin
              if (I = 0) and (PByte(Data)^ >= 32) then
                Result := '''' + S;
            end
            else begin
              if (PByte(Data)^ < 32) and (PByte(Cardinal(Data) - Sz2Val[Sz])^ >= 32) then
                Result := Result + '''' + S
              else if (PByte(Data)^ >= 32) and (PByte(Cardinal(Data) - Sz2Val[Sz])^ < 32) then
                Result := Result + '''' + S
              else
                Result := Result + S;
            end;
            if (I = IM) and (PByte(Data)^ >= 32) then
              Result := Result + S + '''';
          {if (I = 0) and (PByte(Data)^ >= 32) then
            Result := '''' + S
          else
            if (I = IM) and (PByte(Data)^ >= 32) then
              Result := Result + S + ''''
            else
              Result := Result + S;}
          end;
        TVI_FMT_DATETIME:
          if I > 0 then
            Result := Result + '; ' + S
          else
            Result := S;

        else
          if I > 0 then
            Result := Result + ', ' + S
          else
            Result := S;
      end;
      Inc(PByte(Data), Sz2Val[ Sz ]);
    end;
  end;
begin
  if StdData then
    Result := GetStdData
  else
  begin
//    value := getDataFromId(Hdrs, Format.ID, Format.Idx);
    case Format.ID of
      ID_IFH_MACHINE:
        Result := GetImageMachineType(PWord(Data)^);
      ID_IFH_CHARACTERISTICS:
        Result := ImageCharFlags2Str(PWord(Data)^);
      ID_IOH_SUBSYSTEM:
        Result := GetImageSybsystem(PWord(Data)^);
      ID_IOH_DLLCHARACTERISTICS:
        Result := ImageDllChars2Str(PWord(Data)^);
      ID_ISH_CHARACTERISTICS:
        Result := SecFlags2Str(PDWord(Data)^);
      else
        Result := GetStdData;
    end;
  end;
end;



end.
