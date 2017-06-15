unit PETypes;

interface
uses
  Winapi.Windows;

type


  PImageExportDirectory = ^TImageExportDirectory;
  TImageExportDirectory = packed record
      Characteristics: DWord;
      TimeDateStamp: DWord;
      MajorVersion: Word;
      MinorVersion: Word;
      Name: DWord;
      Base: DWord;
      NumberOfFunctions: DWord;
      NumberOfNames: DWord;
      AddressOfFunctions: DWORD;
      AddressOfNames: DWORD;
      AddressOfNameOrdinals: DWORD;
  end;

// -------------  Code from Jedi Code Library (JclWin32) ----------------

  PImageThunkData64 = ^TImageThunkData64;
  _IMAGE_THUNK_DATA64 = packed record
    case Integer of
      0: (ForwarderString: DWORD;);      // PBYTE
      1: (Function_: DWORD;);            // PDWORD
      2: (Ordinal: DWORD;);
      3: (AddressOfData: DWORD;);        // PIMAGE_IMPORT_BY_NAME
  end;
  TImageThunkData64 = _IMAGE_THUNK_DATA64;
  IMAGE_THUNK_DATA64 = _IMAGE_THUNK_DATA64;

  PImageThunkData32 = ^TImageThunkData32;
  _IMAGE_THUNK_DATA32 = packed record
    case Integer of
      0: (ForwarderString: UInt64;);      // PBYTE
      1: (Function_: UInt64;);            // PDWORD
      2: (Ordinal: UInt64;);
      3: (AddressOfData: UInt64;);        // PIMAGE_IMPORT_BY_NAME
  end;
  TImageThunkData32 = _IMAGE_THUNK_DATA32;
  IMAGE_THUNK_DATA32 = _IMAGE_THUNK_DATA32;


  PImageImportDescriptor = ^TImageImportDescriptor;
  _IMAGE_IMPORT_DESCRIPTOR = record
    Characteristics: DWORD;  // 0 for terminating null import descriptor
                             // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
    TimeDateStamp: DWORD;    // 0 if not bound,
                             // -1 if bound, and real date\time stamp
                             //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                             // O.W. date/time stamp of DLL bound to (Old BIND)
    ForwarderChain: DWORD;   // -1 if no forwarders
    Name: DWORD;
    FirstThunk: DWORD;       // RVA to IAT (if bound this IAT has actual addresses)
  end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;

  PImgDelayDescr = ^TImgDelayDescr;
  ImgDelayDescr = packed record
    grAttrs: DWORD;                 // attributes
    szName: DWORD;                  // pointer to dll name
    phmod: DWORD;                  // address of module handle
    pIAT: DWORD;          // address of the IAT
    pINT: DWORD;          // address of the INT
    pBoundIAT: DWORD;     // address of the optional bound IAT
    pUnloadIAT: DWORD;    // address of optional copy of original IAT
    dwTimeStamp: DWORD;             // 0 if not bound,
                                    // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  TImgDelayDescr = ImgDelayDescr;

  PImageBoundImportDescriptor = ^TImageBoundImportDescriptor;
  _IMAGE_BOUND_IMPORT_DESCRIPTOR = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    NumberOfModuleForwarderRefs: Word;
    // Array of zero or more IMAGE_BOUND_FORWARDER_REF follows
  end;
  TImageBoundImportDescriptor = _IMAGE_BOUND_IMPORT_DESCRIPTOR;
  IMAGE_BOUND_IMPORT_DESCRIPTOR = _IMAGE_BOUND_IMPORT_DESCRIPTOR;

  PImageImportByName = ^TImageImportByName;
  _IMAGE_IMPORT_BY_NAME = packed record
    Hint: Word;
    Name: array [0..0] of Char;
  end;
  TImageImportByName = _IMAGE_IMPORT_BY_NAME;
  IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;

  PPImageSectionHeader = ^PImageSectionHeader;


//-----------  End of JCL code ---------------------------


  PImageResourceDirectory = ^TImageResourceDirectory;
  TImageResourceDirectory = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    NumOfNamedEntries: WORD;
    NumOfIdEntries: WORD;
  end;

  PImageResuorceDirectoryEntry = ^TImageResuorceDirectoryEntry;
  TImageResuorceDirectoryEntry = packed record
    Name: DWORD;
    DataOffset: DWORD;
  end;


  PImageResourceDataEntry = ^TImageResourceDataEntry;
  TImageResourceDataEntry = packed record
    OffsetToData    : DWORD;
    Size            : DWORD;
    CodePage        : DWORD;
    Reserved        : DWORD;
  end;


  PGrpIconDirEntry = ^TGrpIconDirEntry;
  TGrpIconDirEntry = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
    Planes: Word;
    BitCount: Word;
    BytesInRes: DWord;
    ID: Word;
  end;


  TGrpCursorDirEntry = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
    XHotspot: Word;
    YHotspot: Word;
    BytesInRes: DWord;
    ID: Word;
  end;


  PGrpIconDir = ^TGrpIconDir;
  TGrpIconDir = packed record
    Reserved: Word;
    iType: Word; // 1 for icons, 2 for cursors
    Count: Word;
    Entries: array[ 0..0 ] of TGrpIconDirEntry;
  end;


  // THis structure is common starting for all entries
  TVerDataEntry = packed record
    wLength,
    wValueLength,
    wType: Word;
  end;



const

  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT  = 13;   // Delay Load Import Descriptors


  RT_CURSOR       = 1;
  RT_BITMAP       = 2;
  RT_ICON         = 3;
  RT_MENU         = 4;
  RT_DIALOG       = 5;
  RT_STRING       = 6;
  RT_FONTDIR      = 7;
  RT_FONT         = 8;
  RT_ACCELERATOR  = 9;
  RT_RCDATA       = 10;
  RT_MESSAGETABLE = 11;
  DIFFERENCE = 11;
  RT_GROUP_CURSOR = RT_CURSOR + DIFFERENCE;
  RT_GROUP_ICON   = RT_ICON + DIFFERENCE;
  RT_VERSION      = 16;
  RT_DLGINCLUDE   = 17;
  RT_PLUGPLAY     = 19;
  RT_VXD          = 20;
  RT_ANICURSOR    = 21;
  RT_ANIICON      = 22;
  RT_HTML         = 23;
  RT_MANIFEST     = 24;
  RT_USER         = 25;


  ResTypesCount = 25;
  ResTypes: array[ 1..25 ] of string = (
    'Single Cursor', 'Bitmap', 'Single Icon', 'Menu', 'Dialog',
    'String', 'Font', 'Single Font', 'Accelerator', 'RCData',
    'Message Table', 'Cursor', '', 'Icon', '',
    'Version', 'Dialog Include', '', 'Plug&Play', 'VXD',
    'AniCursor', 'AniIcon', 'HTML', 'Manifest', 'User Defined'
    );


function IMAGE_ORDINAL(Ordinal: DWORD): Word;




function ImageRvaToVa(NtHeaders: PImageNtHeaders; Base: Pointer;
    Rva: ULONG; LastRvaSection: PImageSectionHeader): Pointer; stdcall;
    external 'imagehlp.dll' name 'ImageRvaToVa';


implementation


function IMAGE_ORDINAL(Ordinal: DWORD): Word;
begin
  Result := Ordinal and $FFFF;
end;


end.
