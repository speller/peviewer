unit KOLClasses;

interface
uses
  Winapi.Windows, Classes, Graphics, SysUtils, CommonUtilities, Dialogs, Math, Types;

{$DEFINE PAS_VERSION}
{$DEFINE ICON_DIFF_WH}
{$DEFINE LOADEX}

type

  TKOLBitmap = class
  {* Bitmap incapsulation object. }
  protected
    fHeight: Integer;
    fWidth: Integer;
    fHandle: HBitmap;
    fCanvas: TCanvas;
    fScanLineSize: Integer;
    fBkColor: TColor;
    fApplyBkColor2Canvas: procedure( Sender: TKOLBitmap );
    fDetachCanvas: procedure( Sender: TKOLBitmap );
    fCanvasAttached : Integer;
    fHandleType: TBitmapHandleType;
    fDIBHeader: PBitmapInfo;
    fDIBBits: Pointer;
    fDIBSize: Integer;
    fNewPixelFormat: TPixelFormat;
    fFillWithBkColor: procedure( BmpObj: TKOLBitmap; DC: HDC; oldW, oldH: Integer );
                        //stdcall;
    fTransMaskBmp: TKOlBitmap;
    fTransColor: TColor;
    fGetDIBPixels: function( Bmp: TKOLBitmap; X, Y: Integer ): TColor;
    fSetDIBPixels: procedure( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
    fScanLine0: PByte;
    fScanLineDelta: Integer;
    fPixelMask: DWORD;
    fPixelsPerByteMask: Integer;
    fBytesPerPixel: Integer;
    fDIBAutoFree: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetEmpty: Boolean;
    function GetHandle: HBitmap;
    function GetHandleAllocated: Boolean;
    procedure SetHandle(const Value: HBitmap);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure FormatChanged;
    function GetCanvas: TCanvas;
    procedure CanvasChanged( Sender: TObject );
    function GetScanLine(Y: Integer): Pointer;
    function GetScanLineSize: Integer;
    procedure ClearData;
    procedure ClearTransImage;
    procedure SetBkColor(const Value: TColor);
    function GetDIBPalEntries(Idx: Integer): TColor;
    function GetDIBPalEntryCount: Integer;
    procedure SetDIBPalEntries(Idx: Integer; const Value: TColor);
    procedure SetHandleType(const Value: TBitmapHandleType);
    function GetPixelFormat: TPixelFormat;
    function GetPixels(X, Y: Integer): TColor;
    procedure SetPixels(X, Y: Integer; const Value: TColor);
    function GetDIBPixels(X, Y: Integer): TColor;
    procedure SetDIBPixels(X, Y: Integer; const Value: TColor);
    function GetBoundsRect: TRect;
  public
    constructor Create(AWidth, AHeight: Integer);
    constructor CreateDIB(AWidth, AHeight: Integer; APixelFormat: TPixelFormat);
    destructor Destroy; override;
  public
    property Width: Integer read fWidth write SetWidth;
    {* Width of bitmap. To make code smaller, avoid changing Width or Height
       after bitmap is created (using NewBitmap) or after it is loaded from
       file, stream of resource. }
    property Height: Integer read fHeight write SetHeight;
    {* Height of bitmap. To make code smaller, avoid changing Width or Height
       after bitmap is created (using NewBitmap) or after it is loaded from
       file, stream of resource. }
    property BoundsRect: TRect read GetBoundsRect;
    {* Returns rectangle (0,0,Width,Height). }
    property Empty: Boolean read GetEmpty;
    {* Returns True if Width or Height is 0. }
    procedure Clear;
    {* Makes bitmap empty, setting its Width and Height to 0. }
    procedure LoadFromFile( const Filename: string );
    {* Loads bitmap from file (LoadFromStream used). }
    function LoadFromFileEx( const Filename: string ): Boolean;
    {* Loads bitmap from a file. If necessary, bitmap is RLE-decoded. Code given
       by Vyacheslav A. Gavrik. }
    procedure SaveToFile( const Filename: string );
    {* Stores bitmap to file (SaveToStream used). }
    procedure CoreSaveToFile( const Filename: string );
    {* Stores bitmap to file (CoreSaveToStream used). }
    procedure RLESaveToFile( const Filename: string );
    {* Stores bitmap to file (CoreSaveToStream used). }
    procedure LoadFromStream( Strm: TStream );
    {* Loads bitmap from stream. Follow loading, bitmap has DIB format (without
       handle allocated). It is possible to draw DIB bitmap without creating
       handle for it, which can economy GDI resources. }
    function LoadFromStreamEx( Strm: TStream ): Boolean;
    {* Loads bitmap from a stream. Difference is that RLE decoding supported.
       Code given by Vyacheslav A. Gavrik. }
    procedure SaveToStream( Strm: TStream );
    {* Saves bitmap to stream. If bitmap is not DIB, it is converted to DIB
       before saving. }
    procedure CoreSaveToStream( Strm: TStream );
    {* Saves bitmap to stream using CORE format with RGBTRIPLE palette and
       with BITMAPCOREHEADER as a header.
       If bitmap is not DIB, it is converted to DIB before saving. }
    procedure RLESaveToStream( Strm: TStream );
    {* Saves bitmap to stream using CORE format with RGBTRIPLE palette and
       with BITMAPCOREHEADER as a header.
       If bitmap is not DIB, it is converted to DIB before saving. }
    procedure LoadFromResourceID( Inst: DWORD; ResID: Integer );
    {* Loads bitmap from resource using integer ID of resource. To load by name,
       use LoadFromResurceName. To load resource of application itself, pass
       hInstance as first parameter. This method also can be used to load system
       predefined bitmaps, if 0 is passed as Inst parameter:
       |<pre>
       OBM_BTNCORNERS	OBM_REDUCE
       OBM_BTSIZE       OBM_REDUCED
       OBM_CHECK        OBM_RESTORE
       OBM_CHECKBOXES   OBM_RESTORED
       OBM_CLOSE        OBM_RGARROW
       OBM_COMBO        OBM_RGARROWD
       OBM_DNARROW      OBM_RGARROWI
       OBM_DNARROWD     OBM_SIZE
       OBM_DNARROWI     OBM_UPARROW
       OBM_LFARROW      OBM_UPARROWD
       OBM_LFARROWD     OBM_UPARROWI
       OBM_LFARROWI     OBM_ZOOM
       OBM_MNARROW      OBM_ZOOMD
       |</pre>        }
    procedure LoadFromResourceName( Inst: DWORD; ResName: PChar );
    {* Loads bitmap from resurce (using passed name of bitmap resource. }
    function Assign( SrcBmp: TKOLBitmap ): Boolean;
    {* Assigns bitmap from another. Returns False if not success.
       Note: remember, that Canvas is not assigned - only bitmap image
       is copied. And for DIB, handle is not allocating due this process. }
    property Handle: HBitmap read GetHandle write SetHandle;
    {* Handle of bitmap. Created whenever property accessed. To check if handle
       is allocated (without allocating it), use HandleAllocated property. }
    property HandleAllocated: Boolean read GetHandleAllocated;
    {* Returns True, if Handle already allocated. }
    function ReleaseHandle: HBitmap;
    {* Returns Handle and releases it, so bitmap no more know about handle.
       This method does not destroy bitmap image, but converts it into DIB.
       Returned Handle actually is a handle of copy of original bitmap. If
       You need not in keping it up, use Dormant method instead. }
    procedure Dormant;
    {* Releases handle from bitmap and destroys it. But image is not destroyed
       and its data are preserved in DIB format. Please note, that in KOL, DIB
       bitmaps can be drawn onto given device context without allocating of
       handle. So, it is very useful to call Dormant preparing it using
       Canvas drawing operations - to economy GDI resources. }
    property HandleType: TBitmapHandleType read fHandleType write SetHandleType;
    {* bmDIB, if DIB part of image data is filled and stored internally in
       TBitmap object. DIB image therefore can have Handle allocated, which
       require resources. Use HandleAllocated funtion to determine if handle
       is allocated and Dormant method to remove it, if You want to economy
       GDI resources. (Actually Handle needed for DIB bitmap only in case
       when Canvas is used to draw on bitmap surface). Please note also, that
       before saving bitmap to file or stream, it is converted to DIB. }
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    {* Current pixel format. If format of bitmap is unknown, or bitmap is DDB,
       value is pfDevice. Setting PixelFormat to any other format converts
       bitmap to DIB, back to pfDevice converts bitmap to DDB again. Avoid
       such conversations for large bitmaps or for numerous bitmaps in your
       application to keep good performance. }
    function BitsPerPixel: Integer;
    {* Returns bits per pixel if possible. }
    procedure Draw( DC: HDC; X, Y: Integer );
    {* Draws bitmap to given device context. If bitmap is DIB, it is always
       drawing using SetDIBitsToDevice API call, which does not require bitmap
       handle (so, it is very sensible to call Dormant method to free correspondent
       GDI resources). }
    procedure StretchDraw( DC: HDC; const Rect: TRect );
    {* Draws bitmap onto DC, stretching it to fit given rectangle Rect. }
    procedure DrawTransparent( DC: HDC; X, Y: Integer; TranspColor: TColor );
    {* Draws bitmap onto DC transparently, using TranspColor as transparent.
       See function DesktopPixelFormat also. }
    procedure StretchDrawTransparent( DC: HDC; const Rect: TRect; TranspColor: TColor );
    {* Draws bitmap onto given rectangle of destination DC (with stretching it
       to fit Rect) - transparently, using TranspColor as transparent.
       See function DesktopPixelFormat also. }
    procedure DrawMasked( DC: HDC; X, Y: Integer; Mask: HBitmap );
    {* Draws bitmap to destination DC transparently by mask. It is possible
       to pass as a mask handle of another TBitmap, previously converted to
       monochrome mask using Convert2Mask method. }
    procedure StretchDrawMasked( DC: HDC; const Rect: TRect; Mask: HBitmap );
    {* Like DrawMasked, but with stretching image onto given rectangle. }
    procedure Convert2Mask( TranspColor: TColor );
    {* Converts bitmap to monochrome (mask) bitmap with TranspColor replaced
       to clBlack and all other ones to clWhite. Such mask bitmap can be used
       to draw original bitmap transparently, with given TranspColor as
       transparent. (To preserve original bitmap, create new instance of
       TBitmap and assign original bitmap to it). See also DrawTransparent and
       StretchDrawTransparent methods. }
    procedure Invert;
    {* Obvious. }
    property Canvas: TCanvas read GetCanvas;
    {* Canvas can be used to draw onto bitmap. Whenever it is accessed, handle
       is allocated for bitmap, if it is not yet (to make it possible
       to select bitmap to display compatible device context). }
    procedure RemoveCanvas;
    {* Call this method to destroy Canvas and free GDI resources. }
    property BkColor: TColor read fBkColor write SetBkColor;
    {* Used to fill background for Bitmap, when its width or height is increased.
       Although this value always synchronized with Canvas.Brush.Color, use it
       instead if You do not use Canvas for drawing on bitmap surface. }
    property Pixels[ X, Y: Integer ]: TColor read GetPixels write SetPixels;
    {* Allows to obtain or change certain pixels of a bitmap. This method is
       both for DIB and DDB bitmaps, and leads to allocate handle anyway. For
       DIB bitmaps, it is possible to use property DIBPixels[ ] instead,
       which is much faster and does not require in Handle. }
    property ScanLineSize: Integer read GetScanLineSize;
    {* Returns size of scan line in bytes. Use it to measure size of a single
       ScanLine. To calculate increment value from first byte of ScanLine to
       first byte of next ScanLine, use difference
       !  Integer(ScanLine[1]-ScanLine[0])
       (this is because bitmap can be oriented from bottom to top, so
       step can be negative). }
    property ScanLine[ Y: Integer ]: Pointer read GetScanLine;
    {* Use ScanLine to access DIB bitmap pixels in memory to direct access it
       fast. Take in attention, that for different pixel formats, different
       bit counts are used to represent bitmap pixels. Also do not forget, that
       for formats pf4bit and pf8bit, pixels actually are indices to palette
       entries, and for formats pf16bit, pf24bit and pf32bit are actually
       RGB values (for pf16bit B:5-G:6-R:5, for pf15bit B:5-G:5-R:5 (high order
       bit not used), for pf24bit B:8-G:8-R:8, and for pf32bit high order byte
       of TRGBQuad structure is not used). }
    property DIBPixels[ X, Y: Integer ]: TColor read GetDIBPixels write SetDIBPixels;
    {* Allows direct access to pixels of DIB bitmap, faster then Pixels[ ]
       property. Access to read is slower for pf15bit, pf16bit formats (because
       some conversation needed to translate packed RGB color to TColor). And
       for write, operation performed most slower for pf4bit, pf8bit (searching
       nearest color required) and fastest for pf24bit, pf32bit and pf1bit. }
    property DIBPalEntryCount: Integer read GetDIBPalEntryCount;
    {* Returns palette entries count for DIB image. Always returns 2 for pf1bit,
       16 for pf4bit, 256 for pf8bit and 0 for other pixel formats. }
    property DIBPalEntries[ Idx: Integer ]: TColor read GetDIBPalEntries write
             SetDIBPalEntries;
    {* Provides direct access to DIB palette. }
    function DIBPalNearestEntry( Color: TColor ): Integer;
    {* Returns index of entry in DIB palette with color nearest (or matching)
       to given one. }
    property DIBBits: Pointer read fDIBBits;
    {* This property is mainly for internal use. }
    property DIBSize: Integer read fDIBSize;
    {* Size of DIBBits array. }
    property DIBHeader: PBitmapInfo read fDIBHeader;
    {* This property is mainly for internal use. }
    procedure DIBDrawRect( DC: HDC; X, Y: Integer; const R: TRect );
    {* This procedure copies given rectangle to the target device context,
       but only for DIB bitmap (using SetDIBBitsToDevice API call). }
    procedure RotateRight;
    {* Rotates bitmap right (90 degree). Bitmap must be DIB. If You definitevely
       know format of a bitmap, use instead one of methods RotateRightMono,
       RotateRight4bit, RotateRight8bit, RotateRight16bit or RotateRightTrueColor
       - this will economy code. But if for most of formats such methods are
       called, this can be more economy just to call always universal method
       RotateRight. }
    procedure RotateLeft;
    {* Rotates bitmap left (90 degree). Bitmap must be DIB. If You definitevely
       know format of a bitmap, use instead one of methods RotateLeftMono,
       RotateLeft4bit, RotateLeft8bit, RotateLeft16bit or RotateLeftTrueColor
       - this will economy code. But if for most of formats such methods are
       called, this can be more economy just to call always universal method
       RotateLeft. }
    procedure RotateRightMono;
    {* Rotates bitmat right, but only if bitmap is monochrome (pf1bit). }
    procedure RotateLeftMono;
    {* Rotates bitmap left, but only if bitmap is monochrome (pf1bit). }
    procedure RotateRight4bit;
    {* Rotates bitmap right, but only if PixelFormat is pf4bit. }
    procedure RotateLeft4bit;
    {* Rotates bitmap left, but only if PixelFormat is pf4bit. }
    procedure RotateRight8bit;
    {* Rotates bitmap right, but only if PixelFormat is pf8bit. }
    procedure RotateLeft8bit;
    {* Rotates bitmap left, but only if PixelFormat is pf8bit. }
    procedure RotateRight16bit;
    {* Rotates bitmap right, but only if PixelFormat is pf16bit. }
    procedure RotateLeft16bit;
    {* Rotates bitmap left, but only if PixelFormat is pf16bit. }
    procedure RotateRightTrueColor;
    {* Rotates bitmap right, but only if PixelFormat is pf24bit or pf32bit. }
    procedure RotateLeftTrueColor;
    {* Rotates bitmap left, but only if PixelFormat is pf24bit or pf32bit. }
    procedure FlipVertical;
    {* Flips bitmap vertically }
    procedure FlipHorizontal;
    {* Flips bitmap horizontally }
    procedure CopyRect( const DstRect : TRect; SrcBmp : TKOLBitmap; const SrcRect : TRect );
    {* It is possible to use Canvas.CopyRect for such purpose, but if You
       do not want use TCanvas, it is possible to copy rectangle from one
       bitmap to another using this function. }
    function CopyToClipboard: Boolean;
    {* Copies bitmap to clipboard. }
    function PasteFromClipboard: Boolean;
    {* Takes CF_DIB format bitmap from clipboard and assigns it to the
       TBitmap object. }
  end;

function Bits2PixelFormat( BitsPerPixel: Integer ): TPixelFormat;

function CalcScanLineSize( Header: PBitmapInfoHeader ): Integer;
{* May be will be useful. }

var
  DefaultPixelFormat: TPixelFormat = pf32bit; //pf16bit;



type

  TKOLIcon = class
  {* Object type to incapsulate icon or cursor image. }
  protected
    {$IFDEF ICON_DIFF_WH}
    FWidth: Integer;
    FHeight: Integer;
    {$ELSE}
    FSize : Integer;
    {$ENDIF}
    FHandle: HIcon;
    FShareIcon: Boolean;
    procedure SetSize(const Value: Integer);
    {$IFDEF ICON_DIFF_WH}
    function GeTKOLIconSize: Integer;
    {$ENDIF}
    procedure SetHandle(const Value: HIcon);
    function GetHotSpot: TPoint;
    function GetEmpty: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    {$IFDEF ICONLOAD_PRESERVEBMPS}
    ImgBmp, MskBmp : PBitmap;
    Only_Bmp: Boolean;
    {$ENDIF ICONLOAD_PRESERVEBMPS}
    property Size : Integer read
      {$IFDEF ICON_DIFF_WH}
      GeTKOLIconSize
      {$ELSE}
      FSize
      {$ENDIF}
    write SetSize;
    {* Icon dimension (width and/or height, which are equal to each other always). }
    {$IFDEF ICON_DIFF_WH}
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    {$ENDIF}
    property Handle : HIcon read FHandle write SetHandle;
    {* Windows icon object handle. }
    procedure SetHandleEx( NewHandle: HIcon );
    {* Set Handle without changing Size (Width/Height). }
    procedure Clear;
    {* Clears icon, freeing image and allocated GDI resource (Handle). }
    property Empty: Boolean read GetEmpty;
    {* Returns True if icon is Empty. }
    property ShareIcon : Boolean read FShareIcon write FShareIcon;
    {* True, if icon object is shared and can not be deleted when TKOLIcon object
       is destroyed (set this flag is to True, if an icon is obtained from another
       TKOLIcon object, for example). }
    property HotSpot : TPoint read GetHotSpot;
    {* Hot spot point - for cursors. }
    procedure Draw( DC : HDC; X, Y : Integer );
    {* Draws icon onto given device context. Icon always is drawn transparently
       using its transparency mask (stored internally in icon object). }
    procedure StretchDraw( DC : HDC; Dest : TRect );
    {* Draws icon onto given device context with stretching it to fit destination
       rectangle. See also Draw. }
    procedure LoadFromStream( Strm : TStream );
    {* Loads icon from stream. If stream contains several icons (of
       different dimentions), icon with the most appropriate size is loading. }
    procedure LoadFromFile( const FileName : string );
    {* Load icon from file. If file contains several icons (of
       different dimensions), icon with the most appropriate size is loading. }
    procedure LoadFromResourceID( Inst: Integer; ResID: Integer; DesiredSize: Integer );
    {* Loads icon from resource. To load system default icon, pass 0 as Inst and
       one of followin values as ResID:
       |<pre>
       IDI_APPLICATION  Default application icon.
       IDI_ASTERISK     Asterisk (used in informative messages).
       IDI_EXCLAMATION  Exclamation point (used in warning messages).
       IDI_HAND         Hand-shaped icon (used in serious warning messages).
       IDI_QUESTION     Question mark (used in prompting messages).
       IDI_WINLOGO      Windows logo.
       |</pre> It is also possible to load icon from resources of another module,
       if pass instance handle of loaded module as Inst parameter. }
    procedure LoadFromResourceName( Inst: Integer; ResName: PChar; DesiredSize: Integer );
    {* Loads icon from resource. To load own application resource, pass
       hInstance as Inst parameter. It is possible to load resource from
       another module, if pass its instance handle as Inst. }
    procedure LoadFromExecutable( const FileName: string; IconIdx: Integer );
    {* Loads icon from executable (exe or dll file). Always default sized icon
       is loaded. It is possible also to get know how much icons are contained
       in executable using gloabl function GetFileIconCount. To obtain icon of
       another size, try to load given executable and use LoadFromResourceID
       method. }
    procedure SaveToStream( Strm : TStream );
    {* Saves single icon to stream. To save icons with several different
       dimensions, use global procedure SaveIcons2Stream. }
    procedure SaveToFile( const FileName : string );
    {* Saves single icon to file. To save icons with several different
       dimensions, use global procedure SaveIcons2File. }
    function Convert2Bitmap( TranColor: TColor ): HBitmap;
    {* Converts icon to bitmap, returning Windows GDI bitmap resource as
       a result. It is possible later to assign returned bitmap handle to
       Handle property of TBitmap object to use features of TBitmap.
       Pass TranColor to replace transparent area of icon with given color. }
  end;






implementation




type
  TWindowsVersion = ( wv31, wv95, wv98, wvME, wvNT, wvY2K, wvXP, wvServer2003,
                  wvVista, wvSeven );

function WriteVal(Stm: TStream; Value, Count: DWORD): DWORD; inline;
begin
  Result := Stm.Write( Value, Count );
end;

var SaveWinVer: Byte = $FF;

function WinVer : TWindowsVersion;
var MajorVersion, MinorVersion: Byte;
    dwVersion: Integer;
begin
  if SaveWinVer <> $FF then Result := TWindowsVersion( SaveWinVer )
  else
  begin
    dwVersion := GetVersion;
    MajorVersion := LoByte( dwVersion );
    MinorVersion := HiByte( LoWord( dwVersion ) );
    if dwVersion >= 0 then
    begin
      Result := wvNT;
      if (MajorVersion >= 6) then begin
        if (MinorVersion >= 1) then
          Result := wvSeven
        else
          Result := wvVista;
      end else begin
             if MajorVersion >= 5 then
                if MinorVersion >= 1 then
                begin
                     Result := wvXP;
                     if MinorVersion >= 2 then
                       Result := wvServer2003;
                end
                else Result := wvY2K;
           end;
    end
      else
    begin
      Result := wv95;
      if (MajorVersion > 4) or
         (MajorVersion = 4) and (MinorVersion >= 10)  then
      begin
        Result := wv98;
        if (MajorVersion = 4) and (MinorVersion >= $5A) then
          Result := wvME;
      end
        else
      if MajorVersion <= 3 then
        Result := wv31;
    end;
    SaveWinVer := Ord( Result );
  end;
end;

function ColorToRGBQuad( Color: TColor ): TRGBQuad;
var C: Integer;
begin
  C := ColorToRGB( Color );
  C := ((C shr 16) and $FF)
    or ((C shl 16) and $FF0000)
    or (C and $FF00);
  Result := TRGBQuad( C );
end;

function ColorToColor15( Color: TColor ): WORD;
begin
  Color := ColorToRGB( Color );
  Result := (Color shr 19) and $1F or
            (Color shr 6) and $3E0 or
            (Color shl 7) and $7C00;
end;

function ColorToColor16( Color: TColor ): WORD;
begin
  Color := ColorToRGB( Color );
  Result := (Color shr 19) and $1F or
            (Color shr 5) and $7E0 or
            (Color shl 8) and $F800;
end;


////////////////////////////////////////////////////////////////////////
//                         t  B  I  T  M  A  P
///////////////////////////////////////////////////////////////////////

{ -- bitmap -- }

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function PrepareBitmapHeader( W, H, BitsPerPixel: Integer ): PBitmapInfo;
begin
  {$IFDEF KOL_ASSERTIONS}
  Assert( W > 0, 'Width must be >0' );
  Assert( H > 0, 'Height must be >0' );
  {$ENDIF KOL_ASSERTIONS}
  Result := AllocMem( 256*Sizeof(TRGBQuad)+Sizeof(TBitmapInfoHeader) );
  {$IFDEF KOL_ASSERTIONS}
  Assert( Result <> nil, 'No memory' );
  {$ENDIF KOL_ASSERTIONS}
  Result.bmiHeader.biSize := Sizeof( TBitmapInfoHeader );
  Result.bmiHeader.biWidth := W;
  Result.bmiHeader.biHeight := H; // may be, -H ?
  Result.bmiHeader.biPlanes := 1;
  Result.bmiHeader.biBitCount := BitsPerPixel;
end;
{$ENDIF PAS_VERSION}

const
  BitsPerPixel_By_PixelFormat: array[ TPixelFormat ] of Byte =
                               ( 0, 1, 4, 8, 16, 16, 24, 32, 0 );

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function Bits2PixelFormat( BitsPerPixel: Integer ): TPixelFormat;
var I: TPixelFormat;
begin
  for I := High(I) downto Low(I) do
    if BitsPerPixel = BitsPerPixel_By_PixelFormat[ I ] then
    begin
      Result := I; Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    end;
  Result := pfDevice;
end;
{$ENDIF PAS_VERSION}

procedure DummyDetachCanvas( Sender: TKOLBitmap );
begin
end;

const InitColors: array[ 0..17 ] of DWORD = ( $F800, $7E0, $1F, 0, $800000, $8000,
      $808000, $80, $800080, $8080, $808080, $C0C0C0, $FF0000, $FF00, $FFFF00, $FF,
      $FF00FF, $FFFF );
{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure PreparePF16bit( DIBHeader: PBitmapInfo );
begin
        DIBHeader.bmiHeader.biCompression := BI_BITFIELDS;
        Move( InitColors[ 0 ], DIBHeader.bmiColors[ 0 ], 19*Sizeof(TRGBQUAD) );
end;
{$ENDIF PAS_VERSION}


{ TKOLBitmap }


procedure TKOLBitmap.ClearData;
begin
  fDetachCanvas( Self );
  if fHandle <> 0 then
  begin
    DeleteObject( fHandle );
    fHandle := 0;
    fDIBBits := nil;
  end;
  if fDIBBits <> nil then
  begin
    if not fDIBAutoFree then
      GlobalFree( THandle( fDIBBits ) );
    fDIBBits := nil;
  end;
  if fDIBHeader <> nil then
  begin
    FreeMem( fDIBHeader );
    fDIBHeader := nil;
  end;
  fScanLineSize := 0;
  fGetDIBPixels := nil;
  fSetDIBPixels := nil;
  ClearTransImage;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.Clear;
begin
  RemoveCanvas;
  ClearData;
  fWidth := 0;
  fHeight := 0;
  fDIBAutoFree := FALSE;
end;
{$ENDIF PAS_VERSION}

constructor TKOLBitmap.Create(AWidth, AHeight: Integer);
var
  DC: HDC;
begin
  inherited Create;
  fHandleType := bmDDB;
  fDetachCanvas := DummyDetachCanvas;
  fWidth := AWidth;
  fHeight := AHeight;
  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    DC := GetDC( 0 );
    fHandle := CreateCompatibleBitmap( DC, AWidth, AHeight );
    ReleaseDC( 0, DC );
  end;
end;

constructor TKOLBitmap.CreateDIB(AWidth, AHeight: Integer; APixelFormat: TPixelFormat);
const
  BitsPerPixel: array[ TPixelFormat ] of Byte = ( 0, 1, 4, 8, 16, 16, 24, 32, 0 );
var
  BitsPixel: Integer;
begin
  fDetachCanvas := DummyDetachCanvas;
  fWidth := AWidth;
  fHeight := AHeight;
  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    BitsPixel := BitsPerPixel[ PixelFormat ];
    if BitsPixel = 0 then
    begin
       fNewPixelFormat := DefaultPixelFormat;
       BitsPixel := BitsPerPixel[DefaultPixelFormat];
    end
    else
       fNewPixelFormat := PixelFormat;
    fDIBHeader := PrepareBitmapHeader( AWidth, AHeight, BitsPixel );
    if PixelFormat = pf16bit then
    begin
      PreparePF16bit( fDIBHeader );
    end;

    fDIBSize := ScanLineSize * AHeight;
    fDIBBits :=
      Pointer( GlobalAlloc( GMEM_FIXED or GMEM_ZEROINIT, fDIBSize + 16 ) );
  end;
end;

function TKOLBitmap.GetBoundsRect: TRect;
begin
  Result := MakeRect( 0, 0, Width, Height );
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
destructor TKOLBitmap.Destroy;
begin
  Clear;
  inherited;
end;
{$ENDIF PAS_VERSION}

function TKOLBitmap.BitsPerPixel: Integer;
var B: tagBitmap;
begin
  CASE PixelFormat OF
  pf1bit: Result := 1;
  pf4bit: Result := 4;
  pf8bit: Result := 8;
  pf15bit: Result := 15;
  pf16bit: Result := 16;
  pf24bit: Result := 24;
  pf32bit: Result := 32;
  else begin
         Result := 0;
         if fHandle <> 0 then
         if GetObject( fHandle, Sizeof( B ), @B ) > 0 then
           Result := B.bmBitsPixel * B.bmPlanes;
       end;
  END;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.Draw(DC: HDC; X, Y: Integer);
var
    DCfrom, DC0: HDC;
    oldBmp: HBitmap;
    oldHeight: Integer;
    B: tagBitmap;
label
    TRYAgain;
begin
TRYAgain:
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if fHandle <> 0 then
  begin
    fDetachCanvas( Self );
    oldHeight := fHeight;
    if GetObject( fHandle, sizeof( B ), @B ) <> 0 then
       oldHeight := B.bmHeight;
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( oldHeight > 0, 'oldHeight must be > 0' );
    {$ENDIF KOL_ASSERTIONS}

    DC0 := GetDC( 0 );
    DCfrom := CreateCompatibleDC( DC0 );
    ReleaseDC( 0, DC0 );

    oldBmp := SelectObject( DCfrom, fHandle );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( oldBmp <> 0, 'Can not select bitmap to DC' );
    {$ENDIF KOL_ASSERTIONS}

    BitBlt( DC, X, Y, fWidth, oldHeight, DCfrom, 0, 0, SRCCOPY );
    {$IFDEF CHK_BITBLT} Chk_BitBlt; {$ENDIF}

    SelectObject( DCfrom, oldBmp );
    DeleteDC( DCfrom );
  end
     else
  if fDIBBits <> nil then
  begin
    oldHeight := Abs(fDIBHeader.bmiHeader.biHeight);
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( oldHeight > 0, 'oldHeight must be > 0' );
    ASSERT( fWidth > 0, 'Width must be > 0' );
    {$ENDIF KOL_ASSERTIONS}
    if StretchDIBits( DC, X, Y, fWidth, oldHeight, 0, 0, fWidth, oldHeight,
                   fDIBBits, fDIBHeader^, DIB_RGB_COLORS, SRCCOPY ) = 0 then
    begin
      if GetHandle <> 0 then
        goto TRYAgain;
    end;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.StretchDraw(DC: HDC; const Rect: TRect);
var DCfrom: HDC;
    oldBmp: HBitmap;
label DrawHandle;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
DrawHandle:
  if fHandle <> 0 then
  begin
    fDetachCanvas( Self );
    DCfrom := CreateCompatibleDC( 0 );
    oldBmp := SelectObject( DCfrom, fHandle );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( oldBmp <> 0, 'Can not select bitmap to DC' );
    {$ENDIF KOL_ASSERTIONS}
    StretchBlt( DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left,
                Rect.Bottom - Rect.Top, DCfrom, 0, 0, fWidth, fHeight,
                SRCCOPY );
    SelectObject( DCfrom, oldBmp );
    DeleteDC( DCfrom );
  end
     else
  if fDIBBits <> nil then
  begin
    if StretchDIBits( DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left,
                Rect.Bottom - Rect.Top, 0, 0, fWidth, fHeight,
                fDIBBits, fDIBHeader^, DIB_RGB_COLORS, SRCCOPY )<=0 then
    begin
      if GetHandle <> 0 then
        goto DrawHandle;
    end;
  end;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.DrawMasked(DC: HDC; X, Y: Integer; Mask: HBitmap);
begin
  StretchDrawMasked( DC, MakeRect( X, Y, X + fWidth, Y + fHeight ), Mask );
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.DrawTransparent(DC: HDC; X, Y: Integer; TranspColor: TColor);
begin
  if TranspColor = clNone then
    Draw( DC, X, Y )
  else
    StretchDrawTransparent( DC, MakeRect( X, Y, X + fWidth, Y + fHeight ),
                            TranspColor );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.StretchDrawTransparent(DC: HDC; const Rect: TRect; TranspColor: TColor);
begin
  if TranspColor = clNone then
     StretchDraw( DC, Rect )
  else
  begin
    if GetHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    TranspColor := ColorToRGB( TranspColor );
    if (fTransMaskBmp = nil) or (fTransColor <> TranspColor) then
    begin
      if fTransMaskBmp = nil then
        fTransMaskBmp := TKOLBitmap.Create( 0, 0 {fWidth, fHeight} );
      fTransColor := TranspColor;
      // Create here mask bitmap:
      fTransMaskBmp.Assign( Self );
      fTransMaskBmp.Convert2Mask( TranspColor );
    end;
    StretchDrawMasked( DC, Rect, fTransMaskBmp.Handle );
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF DEBUG_DRAWTRANSPARENT}
procedure DebugDrawTransparent( DC: HDC; X, Y, W, H: Integer; PF: TPixelFormat;
  const Note: AnsiString );
const PixelFormatAsStr: array[ TPixelFormat ] of String = ( 'pfDevice', 'pf1bit',
      'pf4bit', 'pf8bit', 'pf15bit', 'pf16bit', 'pf24bit', 'pf32bit', 'pfCustom' );
var Bmp: TKOLBitmap;
begin
  Bmp := NewDibBitmap( W, H, pf32bit );
  BitBlt( Bmp.Canvas.Handle, 0, 0, W, H, DC, X, Y, SrcCopy );
  Bmp.SaveToFile( GetStartDir + PixelFormatAsStr[ PF ] + Note );
  Bmp.Free;
end;
{$ENDIF DEBUG_DRAWTRANSPARENT}

const
  ROP_DstCopy = $00AA0029;
{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.StretchDrawMasked(DC: HDC; const Rect: TRect; Mask: HBitmap);
var
  DCfrom, MemDC, MaskDC: HDC;
  MemBmp: HBITMAP;
  //Save4From,
  Save4Mem, Save4Mask: THandle;
  crText, crBack: TColorRef;
  {$IFDEF FIX_TRANSPBMPPALETTE}
  FixBmp: TKOLBitmap;
  {$ENDIF FIX_TRANSPBMPPALETTE}
begin
  {$IFDEF FIX_TRANSPBMPPALETTE}
  if PixelFormat in [ pf4bit, pf8bit ] then
  begin
    FixBmp := NewBitmap( 0, 0 );
    FixBmp.Assign( @ Self );
    FixBmp.PixelFormat := pf32bit;
    FixBmp.StretchDrawMasked( DC, Rect, Mask );
    FixBmp.Free; Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  end;
  {$ENDIF FIX_TRANSPBMPPALETTE}
  if GetHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  DCFrom := Canvas.Handle;
  MaskDC := CreateCompatibleDC( 0 );
  Save4Mask := SelectObject( MaskDC, Mask );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( Save4Mask <> 0, 'Can not select mask bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  MemDC := CreateCompatibleDC( 0 );
  MemBmp := CreateCompatibleBitmap( DCfrom, fWidth, fHeight );
  Save4Mem := SelectObject( MemDC, MemBmp ); if Save4Mem <> 0 then;
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( Save4Mem <> 0, 'Can not select memory bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  StretchBlt( MemDC, 0, 0, fWidth, fHeight, MaskDC, 0, 0, fWidth, fHeight, SrcCopy);
  {$IFDEF DEBUG_DRAWTRANSPARENT}
  DebugDrawTransparent( MemDC, 0, 0, fWidth, fWidth, PixelFormat, '1SrcCopy.bmp' );
  {$ENDIF}
  StretchBlt( MemDC, 0, 0, fWidth, fHeight, DCfrom, 0, 0, fWidth, fHeight, SrcErase);
  {$IFDEF DEBUG_DRAWTRANSPARENT}
  DebugDrawTransparent( MemDC, 0, 0, fWidth, fWidth, PixelFormat, '2SrcErase.bmp' );
  {$ENDIF}
  crText := SetTextColor(DC, $0);
  crBack := Winapi.Windows.SetBkColor(DC, $FFFFFF);
  StretchBlt( DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
              MaskDC, 0, 0, fWidth, fHeight, SrcAnd);
  {$IFDEF DEBUG_DRAWTRANSPARENT}
  DebugDrawTransparent( DC, Rect.Left, Rect.Top, fWidth, fHeight, PixelFormat, '3SrcAnd.bmp' );
  {$ENDIF}
  StretchBlt( DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
              MemDC, 0, 0, fWidth, fHeight, SrcInvert);
  {$IFDEF DEBUG_DRAWTRANSPARENT}
  DebugDrawTransparent( DC, Rect.Left, Rect.Top, fWidth, fHeight, PixelFormat, '4SrcInvert.bmp' );
  {$ENDIF}
  Winapi.Windows.SetBkColor( DC, crBack);
  SetTextColor( DC, crText);
  DeleteObject(MemBmp);
  DeleteDC(MemDC);
  SelectObject( MaskDC, Save4Mask );
  DeleteDC( MaskDC );
end;
{$ENDIF PAS_VERSION}

procedure ApplyBitmapBkColor2Canvas( Sender: TKOLBitmap );
begin
  if Sender.fCanvas = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Sender.fCanvas.Brush.Color := Sender.BkColor;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure DetachBitmapFromCanvas( Sender: TKOLBitmap );
begin
  if Sender.fCanvasAttached = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  SelectObject( Sender.fCanvas.Handle, Sender.fCanvasAttached );
  Sender.fCanvasAttached := 0;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetCanvas: TCanvas;
var DC: HDC;
begin
  Result := nil;
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if GetHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if fCanvas = nil then
  begin
    fApplyBkColor2Canvas := ApplyBitmapBkColor2Canvas;
    fCanvas := TCanvas.Create; //NewCanvas( 0 );
    DC := CreateCompatibleDC( 0 );
    fCanvas.Handle := DC;
    fCanvasAttached := 0;
    fCanvas.Handle := DC;
    fCanvas.OnChange := CanvasChanged;
    if fBkColor <> 0 then
      fCanvas.Brush.Color := fBkColor;
  end;
  Result := fCanvas;

  if fCanvas.Handle = 0 then
  begin
    DC := CreateCompatibleDC( 0 );
    fCanvas.Handle := DC;
    fCanvasAttached := 0;
  end;

  if fCanvasAttached = 0 then
  begin
    fCanvasAttached := SelectObject( fCanvas.Handle, fHandle );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( fCanvasAttached <> 0, 'Can not select bitmap to DC of Canvas' );
    {$ENDIF KOL_ASSERTIONS}
  end;
  fDetachCanvas := DetachBitmapFromCanvas;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetEmpty: Boolean;
begin
  Result := (fWidth = 0) or (fHeight = 0);
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( (fWidth >= 0) and (fHeight >= 0), 'Bitmap dimensions can be negative' );
  {$ENDIF KOL_ASSERTIONS}
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_noVERSION}
function TKOLBitmap.GetHandle: HBitmap;
asm
        PUSH     EBX
        MOV      EBX, EAX
        CALL     GetEmpty
        JZ       @@exit
          MOV      EAX, EBX
          CALL     [EAX].fDetachCanvas
        MOV      ECX, [EBX].fHandle
        INC      ECX
        LOOP     @@exit
          MOV      ECX, [EBX].fDIBBits
          JECXZ    @@exit
        PUSH     ECX
        PUSH     0
        CALL     GetDC
        PUSH     EAX
        PUSH     0
        PUSH     0
        LEA      EDX, [EBX].fDIBBits
        PUSH     EDX
        PUSH     DIB_RGB_COLORS
        PUSH     [EBX].fDIBHeader
        PUSH     EAX
        CALL     CreateDIBSection
        MOV      [EBX].fHandle, EAX
        PUSH     0
        CALL     ReleaseDC
        POP      EAX
        PUSH     EAX
        MOV      EDX, [EBX].fDIBBits
        MOV      ECX, [EBX].fDIBSize
        CALL     System.Move
        POP      EAX
        CMP      [EBX].fDIBAutoFree, 0
        JNZ      @@freed
        PUSH     EAX
        CALL     GlobalFree
@@freed:MOV      [EBX].fDIBAutoFree, 1
        XOR      EAX, EAX
        MOV      [EBX].fGetDIBPixels, EAX
        MOV      [EBX].fSetDIBPixels, EAX
@@exit:   MOV      EAX, [EBX].fHandle
          POP      EBX
end;
{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetHandle: HBitmap;
var OldBits: Pointer;
    DC0: HDC;
begin
  Result := 0;
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fDetachCanvas( @ Self );
  if fHandle = 0 then
  begin
    if fDIBBits <> nil then
    begin
      OldBits := fDIBBits;
      DC0 := GetDC( 0 );
      fDIBBits := nil;
      fHandle := CreateDIBSection( DC0, fDIBHeader^, DIB_RGB_COLORS,
                    fDIBBits, 0, 0 );
      {$IFDEF DEBUG}
      if fHandle = 0 then
        ShowMessage( 'Can not create DIB section, error: ' + IntToStr( GetLastError ) +
        ', ' + SysErrorMessage( GetLastError ) );
      {$ELSE}
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fHandle <> 0, 'Can not create DIB section, error: ' + IntToStr( GetLastError ) +
      ', ' + SysErrorMessage( GetLastError ) );
      {$ENDIF KOL_ASSERTIONS}
      {$ENDIF}
      ReleaseDC( 0, DC0 );
      if fHandle <> 0 then
      begin
        Move( OldBits^, fDIBBits^, fDIBSize );
        if not fDIBAutoFree then
          GlobalFree( THandle( OldBits ) );
        fDIBAutoFree := TRUE;

        fGetDIBPixels := nil;
        fSetDIBPixels := nil;
      end
        else
        fDIBBits := OldBits;
    end;
  end;
  Result := fHandle;
end;
{$ENDIF PAS_VERSION}

function TKOLBitmap.GetHandleAllocated: Boolean;
begin
  Result := fHandle <> 0;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.LoadFromFile(const Filename: string);
var Strm: TStream;
begin
  Strm := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  LoadFromStream( Strm );
  Strm.Free;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.LoadFromResourceID(Inst: DWORD; ResID: Integer);
begin
  LoadFromResourceName( Inst, MAKEINTRESOURCE( ResID ) );
end;

{$IFDEF ASM_UNICODE}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.LoadFromResourceName(Inst: DWORD; ResName: PChar);
var ResHandle: HBitmap;
    Flg: DWORD;
begin
  Clear;
  Flg := 0;
  if fHandleType = bmDIB then
    Flg := LR_CREATEDIBSECTION;
  ResHandle := LoadImage( Inst, ResName, IMAGE_BITMAP, 0, 0, LR_DEFAULTSIZE or Flg );
  if ResHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Handle := ResHandle;
end;
{$ENDIF PAS_VERSION}

{$IFDEF F_P}
type
  PBitmapFileHeader = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;
{$ENDIF}

procedure TKOLBitmap.LoadFromStream(Strm: TStream);
type
  TColorsArray = array[ 0..15 ] of TColor;
  PColorsArray = ^TColorsArray;
  PColor = ^TColor;
var Pos : DWORD;
    BFH : TBitmapFileHeader;

    function ReadBitmap : Boolean;
    var Size, Size1: Integer;
        BCH: TBitmapCoreHeader;
        RGBSize: DWORD;
        C: PColor;
        Off, HdSz, ColorCount: DWORD;
    begin
      fHandleType := bmDIB;
      Result := False;
      if Strm.Read( BFH, Sizeof( BFH ) ) <> Sizeof( BFH ) then Exit; {>>>>>>>>>}
      Off := 0; Size := 0;
      if BFH.bfType <> $4D42 then
         Strm.Seek( Pos, soFromBeginning )
      else
      begin
         Off := BFH.bfOffBits - Sizeof( BFH );
         Size := BFH.bfSize; // don't matter, just <> 0 is good
      end;
      RGBSize := 4;
      HdSz := Sizeof( TBitmapInfoHeader );
      fDIBHeader := AllocMem( 256*sizeof(TRGBQuad) + HdSz );
      if Strm.Read( fDIBHeader.bmiHeader.biSize, Sizeof( DWORD ) ) <> Sizeof( DWORD ) then
         Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      if fDIBHeader.bmiHeader.biSize = HdSz then
      begin
        if Strm.Read( fDIBHeader.bmiHeader.biWidth, HdSz - Sizeof( DWORD ) ) <>
           Integer(HdSz - Sizeof( DWORD )) then
           Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      end
        else
      if fDIBHeader.bmiHeader.biSize = Sizeof( TBitmapCoreHeader ) then
      begin
        RGBSize := 3;
        HdSz := Sizeof( TBitmapCoreHeader );
        if Strm.Read( BCH.bcWidth, HdSz - Sizeof( DWORD ) ) <>
           Integer(HdSz - Sizeof( DWORD )) then
           Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
        fDIBHeader.bmiHeader.biSize := Sizeof( TBitmapInfoHeader );
        fDIBHeader.bmiHeader.biWidth := BCH.bcWidth;
        fDIBHeader.bmiHeader.biHeight := BCH.bcHeight;
        fDIBHeader.bmiHeader.biPlanes := BCH.bcPlanes;
        fDIBHeader.bmiHeader.biBitCount := BCH.bcBitCount;
      end else Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      fNewPixelFormat := Bits2PixelFormat( fDIBHeader.bmiHeader.biBitCount
                         * fDIBHeader.bmiHeader.biPlanes );
      {$IFDEF KOL_ASSERTIONS}
      if (fNewPixelFormat = pf15bit) and (fDIBHeader.bmiHeader.biCompression <> BI_RGB) then
      begin
        ASSERT( fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS, 'Unsupported bitmap format' );
      end;
      {$ENDIF KOL_ASSERTIONS}
      fWidth := fDIBHeader.bmiHeader.biWidth;
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fWidth > 0, 'Bitmap width must be > 0' );
      {$ENDIF KOL_ASSERTIONS}
      fHeight := Abs(fDIBHeader.bmiHeader.biHeight);
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fHeight > 0, 'Bitmap height must be > 0' );
      {$ENDIF KOL_ASSERTIONS}

      fDIBSize := ScanLineSize * fHeight;
      fDIBBits :=
        Pointer( GlobalAlloc( GMEM_FIXED or GMEM_ZEROINIT, fDIBSize ) );
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fDIBBits <> nil, 'No memory' );
      {$ENDIF KOL_ASSERTIONS}

      ColorCount := 0;
      if fDIBHeader.bmiHeader.biBitCount <= 8 then
      begin
        if fDIBHeader.bmiHeader.biClrUsed > 0 then
        ColorCount := fDIBHeader.bmiHeader.biClrUsed * Sizeof( TRGBQuad )
        else
        ColorCount := (1 shl fDIBHeader.bmiHeader.biBitCount) * Sizeof( TRGBQuad )
      end
      else if (fNewPixelFormat in [ pf16bit ]) or
              (fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS) then
             ColorCount := 12;

      if Off > 0 then
      begin
         Off := Off - HdSz;
         if (Off <> ColorCount) then
         if not(fNewPixelFormat in [pf15bit,pf16bit])
         or (Off = 0) //+++ to fix loading 15- and 16-bit bmps with mask omitted
         then
            ColorCount := Min( 1024, Off );
      end;
      if ColorCount <> 0 then
      begin
         if Off >= ColorCount then
           Off := Off - ColorCount;
         if RGBSize = 4 then
         begin
           if Strm.Read( fDIBheader.bmiColors[ 0 ], ColorCount )
              <> Integer(ColorCount) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
         end
           else
         begin
           C := @ fDIBHeader.bmiColors[ 0 ];
           while ColorCount > 0 do
           begin
             if Strm.Read( C^, RGBSize ) <> Integer(RGBSize) then Exit; {>>>>>>>>>>>>>>>}
             Dec( ColorCount, RGBSize );
             Inc( C );
           end;
         end;
      end;
      if Off > 0 then
        Strm.Seek( Off, soFromCurrent );
      if (Size = 0) or (Strm.Size <= 0) then
         Size := fDIBSize
      else
         Size := Min( fDIBSize, Strm.Size - Strm.Position );
      Size1 := Min( Size, fDIBSize );

      if (Size1 < fDIBSize)
         and (DWORD( fDIBSize - Size1 ) <= Strm.Position) then
      begin
        Strm.Seek( Size1 - fDIBSize, soFromCurrent );
        Size1 := fDIBSize;
      end;
      if Size1 > fDIBSize then Size1 := fDIBSize;
      // +++++++++++++++++++ to fix some "incorrect" bitmaps while loading
      if Strm.Read( fDIBBits^, Size1 ) <> Size1 then Exit; {>>>>>>>>>>}
      if Size > Size1 then
        Strm.Seek( Size - Size1, soFromCurrent );
      Result := True;
    end;
begin
  Clear;
  Pos := Strm.Position;
  if not ReadBitmap then
  begin
     Strm.Seek( Pos, soFromBeginning );
     Clear;
  end;
end;

////////////////// bitmap RLE-decoding and loading - by Vyacheslav A. Gavrik

// by Vyacheslav A. Gavrik
procedure DecodeRLE4(Bmp:TKOLBitmap;Data:Pointer; MaxSize: DWORD);
  procedure OddMove(Src,Dst:PByte;Size:Integer);
  begin
    if Size=0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    repeat
      Dst^:=(Dst^ and $F0)or(Src^ shr 4);
      Inc(Dst);
      Dst^:=(Dst^ and $0F)or(Src^ shl 4);
      Inc(Src);
      Dec(Size);
    until Size=0;
  end;
  procedure OddFill(Mem:PByte;Size,Value:Integer);
  begin
    Value:=(Value shr 4)or(Value shl 4);
    Mem^:=(Mem^ and $F0)or(Value and $0F);
    Inc(Mem);
    if Size>1 then FillChar(Mem^,Size,Char( Value ))
    else Mem^:=(Mem^ and $0F)or(Value and $F0);
  end;
var
  pb: PByte;
  x,y,z,i: Integer;
begin
  pb:=Data; x:=0; y:=0;
  if Bmp.fScanLineSize = 0 then
     Bmp.ScanLineSize;
  while (y<Bmp.Height) and (DWORD(pb) - DWORD(Data) < MaxSize) do
  begin
    if pb^=0 then
    begin
      Inc(pb);
      z:=pb^;
      case pb^ of
        0: begin
             Inc(y);
             x:=0;
           end;
        1: Break;
        2: begin
             Inc(pb); Inc(x,pb^);
             Inc(pb); Inc(y,pb^);
           end;
        else
        begin
          Inc(pb);
          i:=(z+1)shr 1;
          if i and 1 = 1 then Inc( i );
          if x + z <= bmp.Width then
          if x and 1 =1 then
            OddMove(pb,@PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x shr 1],(z+1)shr 1)
          else
            Move(pb^,PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x shr 1],(z+1)shr 1);
          Inc(pb,i-1);
          Inc(x,z);
        end;
      end;
    end else
    begin
      z:=pb^;
      Inc(pb);
      if x + z <= Bmp.Width then
        if x and 1 = 1 then
          OddFill(@PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x shr 1],(z+1) shr 1,pb^)
        else
          FillChar( PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x shr 1],
                    (z+1) shr 1, AnsiChar( pb^ ));
      Inc(x,z);
    end;
    Inc(pb);
  end;
end;

// by Vyacheslav A. Gavrik
procedure DecodeRLE8(Bmp:TKOLBitmap;Data:Pointer; MaxSize: DWORD);
var
  pb: PByte;
  x,y,z,i: Integer;
begin
  pb:=Data; y:=0; x:=0;
  if Bmp.fScanLineSize = 0 then
     Bmp.ScanLineSize;

  while (y<Bmp.Height) and (DWORD(pb) - DWORD(Data) < MaxSize) do
  begin
    if pb^=0 then
    begin
      Inc(pb);
      case pb^ of
        0: begin
             Inc(y);
             x:=0;
           end;
        1: Break;
        2: begin
             Inc(pb); Inc(x,pb^);
             Inc(pb); Inc(y,pb^);
           end;
        else
        begin
          i:=pb^;
          z:=(i+1)and(not 1);
          Inc(pb);
          Move(pb^,PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x],i);
          Inc(pb,z-1);
          Inc(x,i);
        end;
      end;
    end else
    begin
      i:=pb^; Inc(pb);
      if x + i <= Bmp.Width then
        FillChar( PByteArray(Integer( Bmp.fDIBBits ) + Bmp.fScanLineSize * y)[x],
                  i, AnsiChar( pb^ ));
      Inc(x,i);
    end;
    Inc(pb);
  end;
end;

function TKOLBitmap.LoadFromFileEx(const Filename: string): Boolean; // by Vyacheslav A. Gavrik
var Strm: TStream;
begin
  Strm := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  Result := LoadFromStreamEx(Strm);
  Strm.Free;
end;

function TKOLBitmap.LoadFromStreamEx(Strm: TStream): Boolean; // by Vyacheslav A. Gavrik
var Pos : DWORD;
    i: Integer;

    function ReadBitmap : Boolean;
    var Off, Size, ColorCount: Integer;
        BFH : TBitmapFileHeader;
        BCH: TBitmapCoreHeader;
        BFHValid: Boolean;
        Buffer: Pointer;
        L: DWORD;
        ColorTriples: Boolean;
        PColr: PDWORD;
        FinalPos: DWORD;
        ZI: DWORD;
    begin
      fHandleType := bmDIB;
      Result := False;
      BFHValid := FALSE;
      if Strm.Read( BFH, Sizeof( BFH ) ) <> Sizeof( BFH ) then Exit; {>>>>>>>>>}
      Off := 0; Size := 0;
      ColorTriples := FALSE;
      if BFH.bfType <> $4D42 then
      begin
         Strm.Seek( Pos, soFromBeginning );
         BFH.bfOffBits := 0;
         BFH.bfSize := 0;
      end
      else
      begin
         BFHValid := TRUE;
         Off := BFH.bfOffBits;
         Size := BFH.bfSize;
      end;
      fDIBHeader := AllocMem( 256*sizeof(TRGBQuad) + sizeof(TBitmapInfoHeader) );
      if Strm.Read( fDIBHeader.bmiHeader.biSize, Sizeof( fDIBHeader.bmiHeader.biSize ) ) <>
        Sizeof( fDIBHeader.bmiHeader.biSize ) then Exit; {>>>>>>>>>>>>>>>>>>>>>}
      if (fDIBHeader.bmiHeader.biSize <> Sizeof( TBitmapCoreHeader )) and
         (fDIBHeader.bmiHeader.biSize <> Sizeof( TBitmapInfoHeader )) then
         Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      L := fDIBHeader.bmiHeader.biSize - Sizeof( fDIBHeader.bmiHeader.biSize );
      if (fDIBHeader.bmiHeader.biSize = Sizeof( TBitmapCoreHeader )) then
      begin
        if Strm.Read( BCH.bcWidth, L ) <> Integer(L) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>}
        fDIBHeader.bmiHeader.biSize := Sizeof( TBitmapInfoHeader );
        fDIBHeader.bmiHeader.biWidth := BCH.bcWidth;
        fDIBHeader.bmiHeader.biHeight := BCH.bcHeight;
        fDIBHeader.bmiHeader.biPlanes := BCH.bcPlanes;
        fDIBHeader.bmiHeader.biBitCount := BCH.bcBitCount;
        ColorTriples := TRUE;
      end
        else
      begin
        if Strm.Read( fDIBHeader.bmiHeader.biWidth, L) <> Integer(L) then
           Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      end;
      fNewPixelFormat := Bits2PixelFormat( fDIBHeader.bmiHeader.biBitCount
                         * fDIBHeader.bmiHeader.biPlanes );
      fWidth := fDIBHeader.bmiHeader.biWidth;
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fWidth > 0, 'Bitmap width must be > 0' );
      {$ENDIF KOL_ASSERTIONS}
      fHeight := Abs(fDIBHeader.bmiHeader.biHeight);
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fHeight > 0, 'Bitmap height must be > 0' );
      {$ENDIF KOL_ASSERTIONS}

      fDIBSize := ScanLineSize * fHeight;
      ZI := 0;
      if (fDIBHeader.bmiHeader.biCompression = BI_RLE8) or
         (fDIBHeader.bmiHeader.biCompression = BI_RLE4) then
         ZI := GMEM_ZEROINIT;
      fDIBBits := Pointer( GlobalAlloc( GMEM_FIXED or ZI, fDIBSize + 4 ) );
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( fDIBBits <> nil, 'No memory' );
      ASSERT( (fDIBHeader.bmiHeader.biCompression and
              (BI_RLE8 or BI_RLE4 or BI_RLE8 or BI_BITFIELDS) <> 0) or
              (fDIBHeader.bmiHeader.biCompression = BI_RGB),
              'Unknown compression algorithm');
      {$ENDIF KOL_ASSERTIONS}

      ColorCount := 0;
      if fDIBHeader.bmiHeader.biBitCount <= 8 then
      begin
        if fDIBHeader.bmiHeader.biClrUsed > 0 then
        ColorCount := fDIBHeader.bmiHeader.biClrUsed * Sizeof( TRGBQuad )
        else
        ColorCount := (1 shl fDIBHeader.bmiHeader.biBitCount) * Sizeof( TRGBQuad )
      end
      else if (fNewPixelFormat in [ pf15bit, pf16bit ]) or
              (fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS) then
      begin
        if (Strm.Size = 0) or (Strm.Size - Strm.Position - DWORD( Size ) >= 12) then
          ColorCount := 12;
      end;

      if ColorTriples then
        ColorCount := ColorCount div 4 * 3;

      if Off > 0 then
      begin
         if   ColorTriples then
              Off := Off - SizeOf( TBitmapFileHeader ) - Sizeof( TBitmapCoreHeader )
         else Off := Off - SizeOf( TBitmapFileHeader ) - Sizeof( TBitmapInfoHeader );
         if  (Off <> ColorCount) and (fNewPixelFormat <= pf8bit) then
             if ColorTriples then
                ColorCount := min( Off, 3 * 256 )
             else
                ColorCount := min( Off, 4 * 256 );
      end;
      if  (fNewPixelFormat in [ pf15bit, pf16bit ]) then
      if  (fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS) then
      begin
          PDWORD( DWORD( @ fDIBHeader.bmiColors[ 0 ] ) + 8 )^ := ( $00001F );
          PDWORD( DWORD( @ fDIBHeader.bmiColors[ 0 ] ) + 4 )^ := ( $0007E0 );
          TColor( fDIBHeader.bmiColors[ 0 ] ) := ( $00F800 );
      end else
          ColorCount := 0;

      if ColorCount <> 0 then
        if ColorTriples then
        begin
          PColr := @ fDIBheader.bmiColors[ 0 ];
          while ColorCount >= 3 do
          begin
            if strm.Read( PColr^, 3 ) <> 3 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>}
            Inc( PColr );
            Dec( ColorCount, 3 );
          end;
        end else
        begin
          if (Integer( Strm.Size - Strm.Position ) > fDIBSize) or
             (fDIBHeader.bmiHeader.biCompression = BI_RLE8) or
             (fDIBHeader.bmiHeader.biCompression = BI_RLE4) then
          begin
            if Strm.Read( fDIBheader.bmiColors[ 0 ], ColorCount )
               <> ColorCount  then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
            if Off - ColorCount > 0 then
              Strm.Position := Integer( Strm.Position ) + Off - ColorCount;
          end;
        end;

      if not BFHValid then
        Size := fDIBSize
      else
      if (fDIBHeader.bmiHeader.biCompression = BI_RLE8) or
         (fDIBHeader.bmiHeader.biCompression = BI_RLE4) then
         begin
           //if BFHValid then //-- already TRUE here
              Size := BFH.bfSize - BFH.bfOffBits;
         end
         else
         begin
           if (Strm.Size = 0) or
              (Integer( Strm.Size - BFH.bfOffBits - Pos ) > Integer(Size)) then
             Size := fDIBSize
           else
             Size := Strm.Size - BFH.bfOffBits - DWORD( Pos );
           if Size > fDIBSize then Size := fDIBSize
           else if (Size < fDIBSize) and (fDIBheader.bmiHeader.biClrUsed <> 0) then
           begin
             BFHValid := FALSE;
             Strm.Position := Strm.Position + fDIBheader.bmiHeader.biClrUsed * 4;
             Size := Strm.Size - Strm.Position;
           end;
         end;

      if (fDIBHeader.bmiHeader.biCompression = BI_RGB) or
         (fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS) then
      begin
        if BFHValid and
           ( (Strm.Size > 0) and
             (Integer( Strm.Size - BFH.bfOffBits - Pos) > Integer(Size))
             or
             (Strm.Size = 0) and
             (Off > 0)
            ) then
        if Integer( Strm.Position - Pos ) <= Integer( BFH.bfOffbits ) then
          Strm.Position := Pos + BFH.bfOffbits;
        i := Strm.Read( fDIBBits^, Size );
        if i <> Size then
        begin
          {$IFDEF FILL_BROKEN_BITMAP}
          ZeroMemory( Pointer( Integer( fDIBBits ) + i ), Size - i );
          {$ENDIF FILL_BROKEN_BITMAP}
        end;
      end
        else
      begin
          if (Integer( fDIBHeader.bmiHeader.biSizeImage ) > 0) and
             (Integer( fDIBHeader.bmiHeader.biSizeImage ) < Size) then
             Size := Integer( fDIBHeader.bmiHeader.biSizeImage ); // - ColorCount;
          // it is possible that bitmap "compressed" with RLE has size
          // greater then non-compressed one:
          FinalPos := Strm.Position + DWORD( Size );
          L := Strm.Size - Strm.Position;
          if  L > DWORD( Size ) then
              L := Size;
          Buffer := AllocMem( Size * 3 );
          if Strm.Read(Buffer^,L) <> Integer(L) then ;
          if fDIBHeader.bmiHeader.biCompression=BI_RLE8 then
             DecodeRLE8(Self,Buffer,Size * 3)
          else
             DecodeRLE4(Self,Buffer,Size * 3);
          Strm.Position := FinalPos;
          fDIBHeader.bmiHeader.biCompression := BI_RGB;
          FreeMem(Buffer);
      end;

      Result := True;
    end;
begin
  Clear;
  Pos := Strm.Position;
  result := ReadBitmap;
  if not result then
  begin
     Strm.Seek( Pos, soFromBeginning );
     Clear;
  end;
  if not Result then
    raise Exception.Create('Invalid image format');
end;

///////////////////////////

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.ReleaseHandle: HBitmap;
var OldBits: Pointer;
begin
  HandleType := bmDIB;
  Result := GetHandle;
  if Result = 0 then Exit; // only when bitmap is empty {>>>>>>>>>>>>>>>>>>>>>>}
  if fDIBAutoFree then
  begin
    OldBits := fDIBBits;
    fDIBBits := Pointer( GlobalAlloc( GMEM_FIXED {or GMEM_ZEROINIT}, fDIBSize ) );
    Move( OldBits^, fDIBBits^, fDIBSize );
    fDIBAutoFree := FALSE;
  end;
  fHandle := 0;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SaveToFile(const Filename: string);
var Strm: TStream;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Strm := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite);
  SaveToStream( Strm );
  Strm.Free;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.CoreSaveToFile(const Filename: string);
var Strm: TStream;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Strm := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite);
  CoreSaveToStream( Strm );
  Strm.Free;
end;

procedure TKOLBitmap.RLESaveToFile(const Filename: string);
var Strm: TStream;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Strm := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite);
  RLESaveToStream( Strm );
  Strm.Free;
end;

procedure TKOLBitmap.SaveToStream(Strm: TStream);
var BFH : PBitmapFileHeader;
    Pos : Integer;
   function WriteBitmap : Boolean;
   var ColorsSize, BitsSize, Size : Integer;
   begin
      Result := False;
      if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      HandleType := bmDIB; // convert to DIB if DDB
      ZeroMemory( @BFH, Sizeof( BFH ) );
      ColorsSize := 0;
      with fDIBHeader.bmiHeader do
           if biBitCount <= 8 then
              ColorsSize := (1 shl biBitCount) * Sizeof( TRGBQuad );
      BFH.bfOffBits := Sizeof( BFH ) + Sizeof( TBitmapInfoHeader ) + ColorsSize;
      BitsSize := fDIBSize; //ScanLineSize * fHeight;
      BFH.bfSize := BFH.bfOffBits + DWord( BitsSize );
      BFH.bfType := $4D42; // 'BM';
      if fDIBHeader.bmiHeader.biCompression <> 0 then
      begin
         ColorsSize := 12 + 16*sizeof(TRGBQuad);
         Inc( BFH.bfOffBits, ColorsSize );
      end;
      if Strm.Write( BFH, Sizeof( BFH ) ) <> Sizeof( BFH ) then Exit; {>>>>>>>>}
      Size := Sizeof( TBitmapInfoHeader ) + ColorsSize;
      if Strm.Write( fDIBHeader^, Size ) <> Size then Exit; {>>>>>>>>>>>}
      if Strm.Write( fDIBBits^, BitsSize ) <> BitsSize then Exit; {>>>}
      Result := True;
   end;
begin
  Pos := Strm.Position;
  if not WriteBitmap then
     Strm.Seek( Pos, soFromBeginning );
end;

procedure TKOLBitmap.CoreSaveToStream(Strm: TStream);
type TRGBTriple = packed record
         bRed, bGreen, bBlue: Byte;
     end;
var BFH : TBitmapFileHeader;
    Pos : Integer;
   function WriteCoreBitmap : Boolean;
   var ColorsSize, ColorsCount, BitsSize, i: Integer;
       CH: TBitmapCoreHeader;
   begin
      Result := False;
      if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      HandleType := bmDIB; // convert to DIB if DDB
      ZeroMemory( @BFH, Sizeof( BFH ) );
      ColorsSize := 0;
      ColorsCount := 1 shl fDIBHeader.bmiHeader.biBitCount;
      with fDIBHeader.bmiHeader do
           if biBitCount <= 8 then
              ColorsSize := ColorsCount * Sizeof( TRGBTriple );
      BFH.bfOffBits := Sizeof( BFH ) + Sizeof( CH ) + ColorsSize;
      BitsSize := fDIBSize; //ScanLineSize * fHeight;
      BFH.bfSize := BFH.bfOffBits + DWord( BitsSize );
      BFH.bfType := $4D42; // 'BM';

      if Strm.Write( BFH, Sizeof( BFH ) ) <> Sizeof( BFH ) then Exit; {>>>>>>>>}
      CH.bcSize := Sizeof( CH );
      CH.bcWidth := Width;
      CH.bcHeight := Height;
      CH.bcPlanes := 1;
      CH.bcBitCount := fDIBHeader.bmiHeader.biBitCount;
      if Strm.Write( CH, Sizeof( CH ) ) <> Sizeof(CH) then Exit; {>>>>>>>>>>>>>}
      for i := 0 to ColorsCount-1 do
      begin
          if  Strm.Write( fDIBHeader.bmiColors[i], 3 ) <> 3 then Exit; {>>>>>>>}
      end;
      if Strm.Write( fDIBBits^, BitsSize ) <> BitsSize then Exit; {>>>}
      Result := True;
   end;
begin
  if  (fDIBHeader.bmiHeader.biBitCount > 8)
  or  (fDIBHeader.bmiHeader.biCompression <> 0) then
  begin
      SaveToStream( Strm ); Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  end;
  Pos := Strm.Position;
  if not WriteCoreBitmap then
     Strm.Seek( Pos, soFromBeginning );
end;

procedure TKOLBitmap.RLESaveToStream(Strm: TStream);
var BFH : PBitmapFileHeader;
    Pos : Integer;
    MS: TMemoryStream;
   function CountZeroes( P: PByte; maxBytes: Integer ): Integer;
   begin
       Result := 0;
       while (P^ = 0) and (Result < maxBytes) do
       begin
           inc( P );
           inc( Result );
       end;
   end;
   function CountSame( P: PByte; maxBytes: Integer ): Integer;
   var B: Byte;
   begin
       Result := 1;
       B := P^;
       while maxBytes > 1 do
       begin
           inc(P);
           if  P^ <> B then break;
           inc(Result);
           dec(maxBytes);
       end;
   end;
   function CountSame2( P: PByteArray; maxPixels: Integer ): Integer;
   var B1, B2: Byte;
       i: Integer;
   begin
       Result := 2;
       B1 := P[0];
       B2 := P[1];
       i := 0;
       dec( maxPixels, 2 );
       while maxPixels > 0 do
       begin
           inc(i, 2);
           if  P[i] <> B1 then break;
           inc(Result);
           dec(maxPixels);
           if  maxPixels = 0 then break;
           if  P[i+1] <> B2 then break;
           inc(Result);
           dec(maxPixels);
       end;
   end;
   function CountDiff( P: PByte; maxBytes: Integer; minSame: Integer ): Integer;
   var Cnt: Integer;
   begin
       Result := 1;
       while (maxBytes > 1) do
       begin
           inc(P);
           dec(maxBytes);
           Cnt := CountSame( P, maxBytes );
           if  Cnt >= minSame then
               break;
           inc( Result );
       end;
   end;
   function CountDiff2( P: PByte; maxPixels: Integer; minSame: Integer ): Integer;
   var Cnt: Integer;
   begin
       Result := 1;
       while (maxPixels > 1) do
       begin
           inc(P);
           dec(maxPixels);
           Cnt := CountSame2( Pointer( P ), maxPixels );
           if  Cnt >= minSame then
               break;
           inc( Result );
       end;
   end;
   procedure WriteOffset( dx, dy: Integer );
   var b: Byte;
   begin
       while (dx > 0) or (dy > 0) do
       begin
           WriteVal(Strm, 0, 1 );
           WriteVal(Strm, 2, 1 );
           b := min( dx, 255 );
           WriteVal(Strm, b, 1 );
           dec( dx, b );
           b := min( dy, 255 );
           WriteVal(Strm, b, 1 );
           dec( dy, b );
       end;
   end;
   procedure WriteRep( cnt: Integer; Value: Byte );
   var n: Integer;
   begin
       while cnt > 0 do
       begin
           n := min( cnt, 255 );
           dec( cnt, n );
           while (cnt > 0) and (cnt < 3) do
           begin
               inc( cnt );
               dec( n );
           end;
           WriteVal(Strm, n, 1 );
           WriteVal(Strm, Value, 1 );
       end;
   end;
   procedure WriteRun( P: PByte; cnt: Integer );
   var n: Integer;
   begin
       while cnt > 0 do
       begin
           n := min( cnt, 255 );
           dec( cnt, n );
           if  (cnt < 3) and (n = 255) then
           begin
               inc( cnt, 2 );
               dec( n, 2 );
           end;
           if  n > 2 then
           begin
               WriteVal(Strm, 00, 1 );
               WriteVal(Strm, n, 1 );
               Strm.Write( P^, n );
               inc( P, n );
               if  n and 1 <> 0 then
                   WriteVal(Strm, 00, 1 );
           end else
           while n > 0 do
           begin
               WriteVal(Strm, 01, 1 );
               Strm.Write( P^, 1 );
               inc( P );
               dec( n );
           end;
       end;
   end;
   procedure WriteRun2( P: PByteArray; cnt: Integer );
   var n, i, L: Integer;
   begin
       i := 0;
       while cnt > 0 do
       begin
           n := min( cnt, 252 );
           dec( cnt, n );
           if  (cnt < 3) and (n = 252) then
           begin
               inc( n, cnt );
               cnt := 0;
           end;
           if  n > 2 then
           begin
               WriteVal(Strm, 00, 1 );
               WriteVal(Strm, n, 1 );
               L := 0;
               while n > 0 do
               begin
                   WriteVal(Strm, P[i] shl 4 or P[i+1], 1 );
                   inc( i, 2 );
                   dec( n, 2 );
                   inc( L );
               end;
               if  L and 1 <> 0 then
                   WriteVal(Strm, 0, 1 );
           end else
           while n > 0 do
           begin
               if  n = 1 then
                   WriteVal(Strm, 01, 1 )
               else
                   WriteVal(Strm, 02, 1 );
               WriteVal(Strm, P[i] shl 4 or P[i+1], 1 );
               inc( i, 2 );
               dec( n, 2 );
           end;
       end;
   end;
   function WriteRLE4: Boolean;
   var line_len_left, y, cnt: Integer;
       P, Pnext: PByte;
 //       PnextLine: PByte;
       offX, offY: Integer;
   begin
       y := 0;
       P := MS.Memory;
       while y < Height do
       begin
           line_len_left := Width;
//           PnextLine := P;
//           inc( PnextLine, line_len_left );
           while line_len_left > 0 do
           begin
               if  P^ = 0 then
               begin
                   cnt := CountZeroes( P, line_len_left + (Height-y-1)*Width );
                   if  cnt > 3 then
                   begin // generate offset
                       offY := cnt div Width;
                       offX := cnt - offY * Width;
                       if  (offX < 0)
                       or (offY = 0) and (offX >= line_len_left)
                       or (line_len_left < offX) then
                       begin
                           inc( P, line_len_left );
                           break;
                       end;
                       if  offY > 0 then
                       begin
                           WriteOffset( offX, offY );
                           inc( P, cnt );
                           dec( line_len_left, offX );
                           inc( Y, offY );
                           continue;
                       end;
                   end;
               end;
               cnt := CountSame2( Pointer( P ), line_len_left );
               if  cnt >= 3 then
               begin
                   Pnext := P; inc( Pnext );
                   WriteRep( cnt, (P^ shl 4) or (Pnext^) );
                   inc( P, cnt );
                   dec( line_len_left, cnt );
               end else
               begin
                   cnt := CountDiff2( P, line_len_left, 3 );
                   WriteRun2( Pointer( P ), cnt );
                   inc( P, cnt );
                   dec( line_len_left, cnt );
               end;
           end;
           WriteVal(Strm, 0, 1 );
           if   y < Height-1 then
                WriteVal(Strm, 0, 1 )  // EOL
           else WriteVal(Strm, 1, 1 ); // EOB
           inc(y);
//           if  ( Integer( P ) - Integer( PnextLine ) ) mod Width <> 0 then
//           asm
//             nop
//           end;
       end;
       Result := TRUE;
   end;
   function WriteRLE8: Boolean;
   var line_len_left, y, cnt: Integer;
       P: PByte;
       //Pnext: PByte;
       offX, offY: Integer;
   begin
       y := 0;
       P := MS.Memory;
       while y < Height do
       begin
           line_len_left := Width;
           //Pnext := P; inc( Pnext, line_len_left );
           while line_len_left > 0 do
           begin
               if  P^ = 0 then
               begin
                   cnt := CountZeroes( P, line_len_left + (Height-y-1)*Width );
                   if  cnt > 3 then
                   begin // generate offset
                       offY := cnt div Width;
                       offX := cnt - offY * Width;
                       if  (offX < 0)
                       or (offY = 0) and (offX >= line_len_left)
                       or (line_len_left < offX) then
                       begin
                           inc( P, line_len_left );
                           break;
                       end;
                       if  offY > 0 then
                       begin
                           WriteOffset( offX, offY );
                           inc( P, cnt );
                           dec( line_len_left, offX );
                           inc( Y, offY );
                           continue;
                       end;
                   end;
               end;
               cnt := CountSame( P, line_len_left );
               if  cnt >= 2 then
               begin
                   WriteRep( cnt, P^ );
                   inc( P, cnt );
                   dec( line_len_left, cnt );
               end else
               begin
                   cnt := CountDiff( P, line_len_left, 2 );
                   WriteRun( P, cnt );
                   inc( P, cnt );
                   dec( line_len_left, cnt );
               end;
           end;
           WriteVal(Strm, 00, 1 );
           if   y < Height-1 then
                WriteVal(Strm, 00, 1 )  // EOL
           else WriteVal(Strm, 01, 1 ); // EOB
           inc(y);
           {if  P <> Pnext then
           asm
             nop
           end;}
       end;
       Result := TRUE;
   end;
   function WriteBitmap : Boolean;
   var ColorsSize, BitsSize : Integer;
       BIH: TBitmapInfoHeader;
       x, y: Integer;
       Line: PByte;
       Buffer: PByteArray;
   begin
      Result := False;
      if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      HandleType := bmDIB; // convert to DIB if DDB
      ZeroMemory( @BFH, Sizeof( BFH ) );
      ColorsSize := 0;
      with fDIBHeader.bmiHeader do
           if biBitCount <= 8 then
              ColorsSize := (1 shl biBitCount) * Sizeof( TRGBQuad );
      BFH.bfOffBits := Sizeof( BFH ) + Sizeof( TBitmapInfoHeader ) + ColorsSize;
      BitsSize := fDIBSize; //ScanLineSize * fHeight;
      BFH.bfSize := BFH.bfOffBits + DWord( BitsSize );
      BFH.bfType := $4D42; // 'BM';
      if fDIBHeader.bmiHeader.biCompression <> 0 then
      begin
         ColorsSize := 12 + 16*sizeof(TRGBQuad);
         Inc( BFH.bfOffBits, ColorsSize );
      end;
      if Strm.Write( BFH, Sizeof( BFH ) ) <> Sizeof( BFH ) then Exit; {>>>>>>>>}
      BIH := fDIBHeader.bmiHeader;
      MS := TMemoryStream.Create;
      if  fDIBHeader.bmiHeader.biBitCount = 8 then
      begin
          for y := Height-1 downto 0 do
          begin
              Line := ScanLine[y];
              MS.Write( Line^, Width );
          end;
      end else
      begin
          Buffer := AllocMem( Width );
          for y := Height-1 downto 0 do
          begin
              Line := ScanLine[y];
              x := 0;
              while x < Width do
              begin
                  Buffer[x] := Line^ shr 4;
                  inc( x );
                  if  x >= Width then break;
                  Buffer[x] := Line^ and 15;
                  inc( x );
                  inc( Line );
              end;
              MS.Write( Buffer^, Width );
          end;
          WriteVal(MS, 0, 2 );
      end;
      if   fDIBHeader.bmiHeader.biBitCount = 8 then
           BIH.biCompression := BI_RLE8
      else BIH.biCompression := BI_RLE4;
      if  Strm.Write( BIH, Sizeof( BIH ) ) <> Sizeof( BIH ) then Exit; {>>>>>>>}
      if  Strm.Write( fDIBHeader.bmiColors, ColorsSize ) <> ColorsSize then
          Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      if   fDIBHeader.bmiHeader.biBitCount = 8 then
           Result := WriteRLE8
      else Result := WriteRLE4;
      MS.Free;
   end;
begin
  Pos := Strm.Position;
  if  (fDIBHeader.bmiHeader.biBitCount <> 4)
  and (fDIBHeader.bmiHeader.biBitCount <> 8) then
  begin
      SaveToStream( Strm ); Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  end;
  if not WriteBitmap then
     Strm.Seek( Pos, soFromBeginning );
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetHandle(const Value: HBitmap);
var B: tagBitmap;
    Dib: TDIBSection;
begin
  Clear;
  if Value = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if (WinVer >= wvNT) and
     (GetObject( Value, Sizeof( Dib ), @ Dib ) = Sizeof( Dib ))
     and (Dib.dsBmih.biBitCount > 8) then
  begin
    fHandle := Value;
    fHandleType := bmDIB;
    fDIBHeader := PrepareBitmapHeader( Dib.dsBm.bmWidth, Dib.dsBm.bmHeight,
    Dib.dsBm.bmBitsPixel );
    Move( Dib.dsBitfields, fDIBHeader.bmiColors, 3 * 4 );
    fWidth := Dib.dsBm.bmWidth;
    fHeight := Dib.dsBm.bmHeight;
    fDIBBits := Dib.dsBm.bmBits;
    fDIBSize := Dib.dsBmih.biSizeImage;
    fDIBAutoFree := true;
  end
  else
  begin
    if GetObject( Value, Sizeof( B ), @B ) = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>}
    fHandle := Value;
    fWidth := B.bmWidth;
    fHeight := B.bmHeight;
    fHandleType := bmDDB;
  end;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.SetWidth(const Value: Integer);
begin
  if fWidth = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fWidth := Value;
  FormatChanged;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetHeight(const Value: Integer);
{$IFNDEF SMALLER_CODE}
var
 pf : TPixelFormat;
{$ENDIF SMALLER_CODE}
begin
  if fHeight = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
{$IFNDEF SMALLER_CODE}
  pf := PixelFormat;
{$ENDIF SMALLER_CODE}
  HandleType := bmDDB;
  // Not too good, but provides correct changing of height
  // preserving previous image
  fHeight := Value;
  FormatChanged;
{$IFNDEF SMALLER_CODE}
  PixelFormat := pf;
{$ENDIF SMALLER_CODE}
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetPixelFormat(Value: TPixelFormat);
begin
  if PixelFormat = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if Value = pfDevice then
    HandleType := bmDDB
  else
  begin
    fNewPixelFormat := Value;
    HandleType := bmDIB;
    FormatChanged;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function CalcScanLineSize( Header: PBitmapInfoHeader ): Integer;
begin
  Result := ((Header.biBitCount * Header.biWidth + 31) shr 3) and $FFFFFFFC;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure FillBmpWithBkColor( Bmp: TKOLBitmap; DC2: HDC; oldWidth, oldHeight: Integer );
var oldBmp: HBitmap;
    R: TRect;
    Br: HBrush;
begin
  with Bmp do
  if ColorToRGB( fBkColor ) <> 0 then
  if (oldWidth < fWidth) or (oldHeight < fHeight) then
    if GetHandle <> 0 then
    begin
      oldBmp := SelectObject( DC2, fHandle );
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( oldBmp <> 0, 'Can not select bitmap to DC' );
      {$ENDIF KOL_ASSERTIONS}
      Br := CreateSolidBrush( ColorToRGB( fBkColor ) );
      R := MakeRect( oldWidth, oldHeight, fWidth, fHeight );
      if  oldWidth = fWidth then
          R.Left := 0;
      if  oldHeight = fHeight then
          R.Top := 0;
      Winapi.Windows.FillRect( DC2, R, Br );
      DeleteObject( Br );
      SelectObject( DC2, oldBmp );
    end;
end;
{$ENDIF PAS_VERSION}

const BitCounts: array[ TPixelFormat ] of Byte = ( 0, 1, 4, 8, 16, 16, 24, 32, 0 );
{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.FormatChanged;
// This method is used whenever Width, Height, PixelFormat or HandleType
// properties are changed.
// Old image will be drawn here to a new one (excluding cases when
// old width or height was 0, and / or new width or height is 0).
// To avoid inserting this code into executable, try not to change
// properties Width / Height of bitmat after it is created using
// NewBitmap( W, H ) function or after it is loaded from file, stream or resource.
var B: tagBitmap;
    oldBmp, NewHandle: HBitmap;
    DC0, DC2: HDC;
    NewHeader: PBitmapInfo;
    NewBits: Pointer;
    oldHeight, oldWidth, sizeBits, bitsPixel: Integer;
    Br: HBrush;
    N: Integer;
    NewDIBAutoFree: Boolean;
    Hndl: THandle;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  NewDIBAutoFree := FALSE;
  fDetachCanvas( Self );
  fScanLineSize := 0;
  fGetDIBPixels := nil;
  fSetDIBPixels := nil;

    oldWidth := fWidth;
    oldHeight := fHeight;
    if fDIBBits <> nil then
    begin
      oldWidth := fDIBHeader.bmiHeader.biWidth;
      oldHeight := Abs(fDIBHeader.bmiHeader.biHeight);
    end
      else
    if fHandle <> 0 then
    begin
      if GetObject( fHandle, Sizeof( B ), @ B ) <> 0 then
      begin
        oldWidth := B.bmWidth;
        oldHeight := B.bmHeight;
      end;
    end;

  DC2 := CreateCompatibleDC( 0 );

  if fHandleType = bmDDB then
  begin
    // New HandleType is bmDDB: old bitmap can be copied using Draw method
    DC0 := GetDC( 0 );
    NewHandle := CreateCompatibleBitmap( DC0, fWidth, fHeight );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( NewHandle <> 0, 'Can not create DDB' );
    {$ENDIF KOL_ASSERTIONS}
    ReleaseDC( 0, DC0 );

    oldBmp := SelectObject( DC2, NewHandle );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( oldBmp <> 0, 'Can not select bitmap to DC' );
    {$ENDIF KOL_ASSERTIONS}

    Br := CreateSolidBrush( ColorToRGB( fBkColor ) );
    FillRect( DC2, MakeRect( 0, 0, fWidth, fHeight ), Br );
    DeleteObject( Br );

    if fDIBBits <> nil then
    begin
      SelectObject( DC2, oldBmp );
      SetDIBits( DC2, NewHandle, 0, fHeight, fDIBBits, fDIBHeader^, DIB_RGB_COLORS );
    end
       else
    begin
      Draw( DC2, 0, 0 );
      SelectObject( DC2, oldBmp );
    end;

    ClearData; // Image is cleared but fWidth and fHeight are preserved
    fHandle := NewHandle;
  end
     else
  begin
    // New format is DIB. GetDIBits applied to transform old data to new one.
    bitsPixel := BitCounts[ fNewPixelFormat ];
    if bitsPixel = 0 then
    begin
      bitsPixel := BitCounts[DefaultPixelFormat];
    end;

    NewHandle := 0;
    NewHeader := PrepareBitmapHeader( fWidth, fHeight, bitsPixel );
    if fNewPixelFormat = pf16bit then
      PreparePF16bit( NewHeader );

    sizeBits := CalcScanLineSize( @NewHeader.bmiHeader ) * fHeight;

      NewBits := Pointer( GlobalAlloc( GMEM_FIXED, sizeBits ) );
      {$IFDEF KOL_ASSERTIONS}
      ASSERT( NewBits <> nil, 'No memory' );
      {$ENDIF KOL_ASSERTIONS}

      Hndl := GetHandle;
      if Hndl = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
      N :=
      GetDIBits( DC2, Hndl, 0, Min( fHeight, oldHeight ),
                 NewBits, NewHeader^, DIB_RGB_COLORS );
      if N <> Min( fHeight, oldHeight ) then
      begin
        GlobalFree( DWORD( NewBits ) );
        NewBits := nil;
        NewHandle := CreateDIBSection( DC2, NewHeader^, DIB_RGB_COLORS, NewBits, 0, 0 );
        NewDIBAutoFree := TRUE;
        {$IFDEF KOL_ASSERTIONS}
        ASSERT( NewHandle <> 0, 'Can not create DIB secion for pf16bit bitmap' );
        {$ENDIF KOL_ASSERTIONS}
        oldBmp := SelectObject( DC2, NewHandle );
        {$IFDEF KOL_ASSERTIONS}
        ASSERT( oldBmp <> 0, 'Can not select pf16bit to DC' );
        {$ENDIF KOL_ASSERTIONS}
        Draw( DC2, 0, 0 );
        SelectObject( DC2, oldBmp );
      end;

    ClearData;
    fDIBSize := sizeBits;
    fDIBBits := NewBits;
    fDIBHeader := NewHeader;
    fHandle := NewHandle;
    fDIBAutoFree := NewDIBAutoFree;

  end;

  if Assigned( fFillWithBkColor ) then
     fFillWithBkColor( Self, DC2, oldWidth, oldHeight );

  DeleteDC( DC2 );

end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetScanLine(Y: Integer): Pointer;
begin
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( (Y >= 0) {and (Y < fHeight)}, 'ScanLine index out of bounds' );
  ASSERT( fDIBBits <> nil, 'No bits available' );
  {$ENDIF KOL_ASSERTIONS}
  Result := nil;
  if fDIBHeader = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if fDIBHeader.bmiHeader.biHeight > 0 then
     Y := fHeight - 1 - Y;
  if fScanLineSize = 0 then
     ScanLineSize;

  Result := Pointer( Integer( fDIBBits ) + fScanLineSize * Y );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetScanLineSize: Integer;
begin
  Result := 0;
  if fDIBHeader = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  FScanLineSize := CalcScanLineSize( @fDIBHeader.bmiHeader );
  Result := FScanLineSize;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.CanvasChanged( Sender : TObject );
begin
  fBkColor := TCanvas( Sender ).Brush.Color;
  ClearTransImage;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.Dormant;
begin
  RemoveCanvas;
  if fHandle <> 0 then
    DeleteObject( ReleaseHandle );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetBkColor(const Value: TColor);
begin
  if fBkColor = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fBkColor := Value;
  fFillWithBkColor := FillBmpWithBkColor;
  if  Assigned( fApplyBkColor2Canvas ) then
      fApplyBkColor2Canvas( Self );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.Assign(SrcBmp: TKOLBitmap): Boolean;
begin
  Clear;
  Result := False;
  if SrcBmp = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if SrcBmp.Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fWidth := SrcBmp.fWidth;
  fHeight := SrcBmp.fHeight;
  fHandleType := SrcBmp.fHandleType;
  //fNewPixelFormat := SrcBmp.PixelFormat;
  if SrcBmp.fHandleType = bmDDB then
  begin
    fHandle := CopyImage( SrcBmp.fHandle, IMAGE_BITMAP, 0, 0, 0 {LR_COPYRETURNORG} );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( fHandle <> 0, 'Can not copy bitmap image' );
    {$ENDIF KOL_ASSERTIONS}
    Result := fHandle <> 0;
    if not Result then Clear;
  end
     else
  begin
    GetMem( fDIBHeader, Sizeof(TBitmapInfoHeader) + 256*sizeof(TRGBQuad) );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( fDIBHeader <> nil, 'No memory' );
    {$ENDIF KOL_ASSERTIONS}
    Move( SrcBmp.fDIBHeader^, fDIBHeader^, Sizeof(TBitmapInfoHeader) + 256*sizeof(TRGBQuad) );
    fDIBSize := SrcBmp.fDIBSize;
    fDIBBits := Pointer( GlobalAlloc( GMEM_FIXED {or GMEM_ZEROINIT}, fDIBSize ) );
    {$IFDEF KOL_ASSERTIONS}
    ASSERT( fDIBBits <> nil, 'No memory' );
    {$ENDIF KOL_ASSERTIONS}
    Move( SrcBmp.fDIBBits^, fDIBBits^, fDIBSize );
    Result := True;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.RemoveCanvas;
begin
  fDetachCanvas( Self );
  fCanvas.Free;
  fCanvas := nil;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.DIBPalNearestEntry(Color: TColor): Integer;
var I, Diff, D: Integer;
    C : Integer;
begin
  Color := TColor(ColorToRGBQuad( Color ) );
  Result := 0;
  Diff := MaxInt;
  for I := 0 to DIBPalEntryCount - 1 do
  begin
    C := Color xor PInteger( Integer( @fDIBHeader.bmiColors[ 0 ] )
                    + I * Sizeof( TRGBQuad ) )^;
    D := TRGBQuad( C ).rgbBlue + TRGBQuad( C ).rgbGreen + TRGBQuad( C ).rgbRed;
    if D < Diff then
    begin
      Diff := D;
      Result := I;
    end;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetDIBPalEntries(Idx: Integer): TColor;
begin
  Result := TColor(-1);
  if fDIBBits = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( PixelFormat in [pf1bit..pf8bit], 'Format has no DIB palette entries available' );
  ASSERT( (Idx >= 0) and (Idx < (1 shl fDIBHeader.bmiHeader.biBitCount)),
          'DIB palette index out of bounds' );
  {$ENDIF KOL_ASSERTIONS}
  Result := PDWORD( Integer( @fDIBHeader.bmiColors[ 0 ] )
          + Idx * Sizeof( TRGBQuad ) )^;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetDIBPalEntryCount: Integer;
begin
  Result := 0;
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  case PixelFormat of
  pf1bit: Result := 2;
  pf4bit: Result := 16;
  pf8bit: Result := 256;
  else;
  end;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.SetDIBPalEntries(Idx: Integer; const Value: TColor);
begin
  if fDIBBits = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Dormant;
  PDWORD( Integer( @fDIBHeader.bmiColors[ 0 ] )
                    + Idx * Sizeof( TRGBQuad ) )^ := ColorToRGB( Value );
end;

procedure TKOLBitmap.SetHandleType(const Value: TBitmapHandleType);
begin
  if fHandleType = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fHandleType := Value;
  FormatChanged;
end;

function TKOLBitmap.GetPixelFormat: TPixelFormat;
begin
  if (HandleType = bmDDB) or (fDIBBits = nil) then
    Result := pfDevice
  else
  begin
    Result := Bits2PixelFormat( fDIBHeader.bmiHeader.biBitCount );
    if fDIBHeader.bmiHeader.biCompression <> 0 then
    begin
      {$IFDEF KOL_ASSERTIONS}
      Assert( fDIBHeader.bmiHeader.biCompression = BI_BITFIELDS, 'Unsupported bitmap format' );
      {$ENDIF KOL_ASSERTIONS}
      if (TColor( fDIBHeader.bmiColors[ 0 ] ) = $F800) and
         (PInteger( DWORD(@ fDIBHeader.bmiColors[ 0 ])+4 )^ =  $7E0) and
         (PInteger( DWORD(@ fDIBHeader.bmiColors[ 0 ])+8 )^ =   $1F) then
        Result := pf16bit
      else
      if (TColor( fDIBHeader.bmiColors[ 0 ] ) = $7C00) and
         (PInteger( DWORD(@ fDIBHeader.bmiColors[ 0 ])+4 )^ =  $3E0) and
         (PInteger( DWORD(@ fDIBHeader.bmiColors[ 0 ])+8 )^ =   $1F) then
        Result := pf15bit
      else
        Result := pfCustom;
    end;
  end;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.ClearTransImage;
begin
  fTransColor := clNone;
  fTransMaskBmp.Free;
  fTransMaskBmp := nil;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
{$IFDEF USE_OLDCONVERT2MASK}
procedure TKOLBitmap.Convert2Mask(TranspColor: TColor);
var MonoHandle: HBitmap;
    SaveMono, SaveFrom: THandle;
    MonoDC, DCfrom: HDC;
    SaveBkColor: TColorRef;
begin
  if GetHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fDetachCanvas( Self );
  MonoHandle := CreateBitmap( fWidth, fHeight, 1, 1, nil );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( MonoHandle <> 0, 'Can not create monochrome bitmap' );
  {$ENDIF KOL_ASSERTIONS}
  MonoDC := CreateCompatibleDC( 0 );
  SaveMono := SelectObject( MonoDC, MonoHandle );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( SaveMono <> 0, 'Can not select bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  DCfrom := CreateCompatibleDC( 0 );
  SaveFrom := SelectObject( DCfrom, fHandle );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( SaveFrom <> 0, 'Can not select source bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  TranspColor := ColorToRGB( TranspColor );
  SaveBkColor := Windows.SetBkColor( DCfrom, TranspColor );
  BitBlt( MonoDC, 0, 0, fWidth, fHeight, DCfrom, 0, 0, SRCCOPY );
  {$IFDEF CHK_BITBLT} Chk_BitBlt; {$ENDIF}
  Windows.SetBkColor( DCfrom, SaveBkColor );
  SelectObject( DCfrom, SaveFrom );
  DeleteDC( DCfrom );
  SelectObject( MonoDC, SaveMono );
  DeleteDC( MonoDC );
  ///ReleaseDC( 0, DC0 );
  ClearData;
  fHandle := MonoHandle;
  fHandleType := bmDDB;
end;
{$ELSE NOT USE_OLDCONVERT2MASK} //Pascal
procedure TKOLBitmap.Convert2Mask(TranspColor: TColor);
var Y, X, i: Integer;
    Src, Dst: PByte;
    W: Word;
    TmpMsk: TKOLBitmap;
    B, C: Byte;
    TranspColor32: TColor;
begin
  HandleType := bmDIB;
  if PixelFormat < pf4bit then
    PixelFormat := pf4bit;
  if PixelFormat > pf32bit then
    PixelFormat := pf32bit;
  TranspColor := ColorToRGB( TranspColor ) and $FFFFFF;
  TranspColor32 := TColor( ColorToRGBQuad( TranspColor ) );
  TmpMsk := TKOLBitmap.CreateDIB(fWidth, fHeight, pf1bit);
  TmpMsk.DIBPalEntries[ 1 ] := $FFFFFF;
  for Y := 0 to fHeight-1 do
  begin
    Src := ScanLine[ Y ];
    Dst := TmpMsk.ScanLine[ Y ];
    B := 0; C := 8;
    CASE PixelFormat OF
    pf4bit:
      begin
        W := 16;
        for i := 0 to 15 do
          if DIBPalEntries[ i ] = TranspColor32 then
          begin
            W := i; break;
          end;
        for X := 0 to (fWidth div 2)-1 do
        begin
          B := B shl 1;
          if Src^ shr 4 = W then inc( B );
          B := B shl 1;
          if Src^ and $0F = W then inc( B );
          Inc( Src );
          Dec( C, 2 );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    pf8bit:
      begin
        W := 256;
        for i := 0 to 255 do
          if DIBPalEntries[ i ] = TranspColor32 then
          begin
            W := i; break;
          end;
        for X := 0 to fWidth-1 do
        begin
          B := B shl 1;
          if Src^ = W then inc( B );
          Inc( Src );
          Dec( C );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    pf15bit:
      begin
        W := ColorToColor15( TranspColor );
        for X := 0 to fWidth-1 do
        begin
          B := B shl 1;
          if PWord( Src )^ = W then inc( B );
          Inc( Src, 2 );
          Dec( C );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    pf16bit:
      begin
        W := ColorToColor16( TranspColor );
        for X := 0 to fWidth-1 do
        begin
          B := B shl 1;
          if PWord( Src )^ = W then inc( B );
          Inc( Src, 2 );
          Dec( C );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    pf24bit:
      begin
        for X := 0 to fWidth-1 do
        begin
          B := B shl 1;
          if PInteger( Src )^ and $FFFFFF = TranspColor32 then inc( B );
          Inc( Src, 3 );
          Dec( C );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    pf32bit:
      begin
        for X := 0 to fWidth-1 do
        begin
          B := B shl 1;
          if PInteger( Src )^ and $FFFFFF = TranspColor32 then inc( B );
          Inc( Src, 4 );
          Dec( C );
          if C = 0 then
          begin
            Dst^ := B;
            Inc( Dst );
            C := 8;
          end;
        end;
      end;
    END;
    if (C > 0) and (C < 8) then
    begin
      while C > 0 do
      begin
        B := B shl 1;
        dec( C );
      end;
      Dst^ := B;
    end;
  end;
  Assign( TmpMsk );
  TmpMsk.Free;
end;
{$ENDIF USE_OLDCONVERT2MASK} //Pascal
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.Invert;
var R: TRect;
begin
  //BitBlt( Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, DSTINVERT  )
  R := BoundsRect;
  InvertRect(Canvas.Handle, R);
end;

procedure TKOLBitmap.DIBDrawRect( DC: HDC; X, Y: Integer; const R: TRect );
begin
  if fDIBBits = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  StretchDIBits( DC, X, Y, R.Right - R.Left, R.Bottom - R.Top,
                 R.Left, fHeight - R.Bottom, R.Right - R.Left, R.Bottom - R.Top,
                 fDIBBits, fDIBHeader^, DIB_RGB_COLORS, SRCCOPY );
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmapMono( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
var X, Y, Z, Shf, Wbytes, BytesPerDstLine: Integer;
    Src, Dst, Dst1: PByte;
    Tmp: Byte;
begin

  DstBmp := TKOLBitmap.CreateDIB( SrcBmp.fHeight, (SrcBmp.fWidth + 7) and not 7, pf1bit );
  Move( SrcBmp.fDIBHeader.bmiColors[ 0 ], DstBmp.fDIBHeader.bmiColors[ 0 ], 2 * Sizeof( TRGBQuad ) );

  // Calculate ones:
  Dst := DstBmp.ScanLine[ 0 ];
  BytesPerDstLine := Integer( DstBmp.ScanLine[ 1 ]) - Integer( Dst );
  Wbytes := (SrcBmp.fWidth + 7) shr 3;

  Inc( Dst, (DstBmp.fWidth - 1) shr 3 );
  Shf := (DstBmp.fWidth - 1) and 7;

  // Rotating bits:
  for Y := 0 to SrcBmp.fHeight - 1 do
  begin
    Src := SrcBmp.ScanLine[ Y ];
    Dst1 := Dst;
    for X := Wbytes downto 1 do
    begin
      Tmp := Src^;
      Inc( Src );
      for Z := 8 downto 1 do
      begin
        Dst1^ := Dst1^ or ( (Tmp and $80) shr Shf );
        Tmp := Tmp shl 1;
        Inc( Dst1, BytesPerDstLine );
      end;
    end;
    Dec( Shf );
    if Shf < 0 then
    begin
      Shf := 7;
      Dec( Dst );
    end;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmap4bit( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
var X, Y, Shf, Wbytes, BytesPerDstLine: Integer;
    Src, Dst, Dst1: PByte;
    Tmp: Byte;
begin
  DstBmp := TKOLBitmap.CreateDIB( SrcBmp.fHeight, (SrcBmp.fWidth + 1) and not 1, pf4bit );
  Move( SrcBmp.fDIBHeader.bmiColors[ 0 ], DstBmp.fDIBHeader.bmiColors[ 0 ], 16 * Sizeof( TRGBQuad ) );

  // Calculate ones:
  Dst := DstBmp.ScanLine[ 0 ];
  BytesPerDstLine := Integer( DstBmp.ScanLine[ 1 ]) - Integer( Dst );
  Wbytes := (SrcBmp.fWidth + 1) shr 1;
  Inc( Dst, (DstBmp.fWidth - 1) shr 1 );
  Shf := ((DstBmp.fWidth - 1) and 1) shl 2;

  // Rotating bits:
  for Y := 0 to SrcBmp.fHeight - 1 do
  begin
    Src := SrcBmp.ScanLine[ Y ];
    Dst1 := Dst;
    for X := Wbytes downto 1 do
    begin
      Tmp := Src^;
      Inc( Src );
      Dst1^ := Dst1^ or ( (Tmp and $F0) shr Shf );
      Inc( Dst1, BytesPerDstLine );
      Dst1^ := Dst1^ or ( ((Tmp shl 4) and $F0) shr Shf );
      Inc( Dst1, BytesPerDstLine );
    end;
    Dec( Shf, 4 );
    if Shf < 0 then
    begin
      Shf := 4;
      Dec( Dst );
    end;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmap8bit( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
var X, Y, Wbytes, BytesPerDstLine: Integer;
    Src, Dst, Dst1: PByte;
    Tmp: Byte;
begin

  DstBmp := TKOLBitmap.CreateDIB( SrcBmp.fHeight, SrcBmp.fWidth, SrcBmp.PixelFormat );
  Move( SrcBmp.fDIBHeader.bmiColors[ 0 ], DstBmp.fDIBHeader.bmiColors[ 0 ], 256 * Sizeof( TRGBQuad ) );

  // Calculate ones:
  Wbytes := SrcBmp.fWidth;
  Dst := DstBmp.ScanLine[ 0 ];
  BytesPerDstLine := Integer( DstBmp.ScanLine[ 1 ]) - Integer( Dst );

  Inc( Dst, DstBmp.fWidth - 1 );

  // Rotating bits:
  for Y := 0 to SrcBmp.fHeight - 1 do
  begin
    Src := SrcBmp.ScanLine[ Y ];
    Dst1 := Dst;
    for X := Wbytes downto 1 do
    begin
      Tmp := Src^;
      Inc( Src );
      Dst1^ := Tmp;
      Inc( Dst1, BytesPerDstLine );
    end;
    Dec( Dst );
  end;

end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmap16bit( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
var X, Y, Wwords, BytesPerDstLine: Integer;
    Src, Dst, Dst1: PWord;
    Tmp: Word;
begin
  DstBmp := TKOLBitmap.CreateDIB( SrcBmp.fHeight, SrcBmp.fWidth, SrcBmp.PixelFormat );
  Wwords := SrcBmp.fWidth;
  Dst := DstBmp.ScanLine[ 0 ];
  BytesPerDstLine := Integer( DstBmp.ScanLine[ 1 ]) - Integer( Dst );
  Inc( Dst, DstBmp.fWidth - 1 );

  // Rotating bits:
  for Y := 0 to SrcBmp.fHeight - 1 do
  begin
    Src := SrcBmp.ScanLine[ Y ];
    Dst1 := Dst;
    for X := Wwords downto 1 do
    begin
      Tmp := Src^;
      Inc( Src );
      Dst1^ := Tmp;
      Inc( PByte(Dst1), BytesPerDstLine );
    end;
    Dec( Dst );
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmap2432bit( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
var X, Y, Wwords, BytesPerDstLine, IncW: Integer;
    Src, Dst, Dst1: PDWord;
    Tmp: DWord;
begin

  DstBmp := TKOLBitmap.CreateDIB( SrcBmp.fHeight, SrcBmp.fWidth, SrcBmp.PixelFormat );

  // Calculate ones:
  IncW := 4;
  if DstBmp.PixelFormat = pf24bit then
     IncW := 3;
  Wwords := SrcBmp.fWidth;
  Dst := DstBmp.ScanLine[ 0 ];
  BytesPerDstLine := Integer( DstBmp.ScanLine[ 1 ]) - Integer( Dst );

  Inc( PByte(Dst), (DstBmp.fWidth - 1) * IncW );

  // Rotating bits:
  for Y := 0 to SrcBmp.fHeight - 1 do
  begin
    Src := SrcBmp.ScanLine[ Y ];
    Dst1 := Dst;
    for X := Wwords downto 1 do
    begin
      Tmp := Src^ and $FFFFFF;
      Inc( PByte(Src), IncW );
      Dst1^ := Dst1^ or Tmp;
      Inc( PByte(Dst1), BytesPerDstLine );
    end;
    Dec( PByte(Dst), IncW );
  end;

end;
{$ENDIF PAS_VERSION}

type
  TRotateBmpRefs = packed record
    proc_RotateBitmapMono: procedure( var Dst: TKOLBitmap; Src: TKOLBitmap );
    proc_RotateBitmap4bit: procedure( var Dst: TKOLBitmap; Src: TKOLBitmap );
    proc_RotateBitmap8bit: procedure( var Dst: TKOLBitmap; Src: TKOLBitmap );
    proc_RotateBitmap16bit: procedure( var Dst: TKOLBitmap; Src: TKOLBitmap );
    proc_RotateBitmap2432bit: procedure( var Dst: TKOLBitmap; Src: TKOLBitmap );
  end;

var
  RotateProcs: TRotateBmpRefs;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _RotateBitmapRight( SrcBmp: TKOLBitmap );
var DstBmp: TKOLBitmap;
    RotateProc: procedure( var DstBmp: TKOLBitmap; SrcBmp: TKOLBitmap );
begin
  if SrcBmp.fHandleType <> bmDIB then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  case SrcBmp.PixelFormat of
  pf1bit: RotateProc := RotateProcs.proc_RotateBitmapMono;
  pf4bit: RotateProc := RotateProcs.proc_RotateBitmap4bit;
  pf8bit: RotateProc := RotateProcs.proc_RotateBitmap8bit;
  pf15bit, pf16bit: RotateProc := RotateProcs.proc_RotateBitmap16bit;
  else RotateProc := RotateProcs.proc_RotateBitmap2432bit;
  end;
  if not Assigned( RotateProc ) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProc( DstBmp, SrcBmp );
  if DstBmp.fHeight > SrcBmp.fWidth then
  begin
      DstBmp.fDIBSize := DstBmp.fScanLineSize * SrcBmp.fWidth;
      if DstBmp.fDIBHeader.bmiHeader.biHeight > 0 then
        Move( DstBmp.ScanLine[ SrcBmp.fWidth - 1 ]^, DstBmp.ScanLine[ DstBmp.fHeight - 1 ]^,
              DstBmp.fDIBSize );
      DstBmp.fHeight := SrcBmp.fWidth;
      DstBmp.fDIBHeader.bmiHeader.biHeight := DstBmp.fHeight;
  end;

  SrcBmp.ClearData;

  SrcBmp.fDIBHeader := DstBmp.fDIBHeader;
  DstBmp.fDIBHeader := nil;

  SrcBmp.fDIBBits := DstBmp.fDIBBits;
  DstBmp.fDIBBits := nil;
  SrcBmp.fDIBAutoFree := DstBmp.fDIBAutoFree;

  SrcBmp.fDIBSize := DstBmp.fDIBSize;

  SrcBmp.fWidth := DstBmp.fWidth;
  SrcBmp.fHeight := DstBmp.fHeight;
  DstBmp.Free;
end;
{$ENDIF PAS_VERSION}

procedure TKOLBitmap.RotateRight;
const AllRotators: TRotateBmpRefs = (
        proc_RotateBitmapMono: _RotateBitmapMono;
        proc_RotateBitmap4bit: _RotateBitmap4bit;
        proc_RotateBitmap8bit: _RotateBitmap8bit;
        proc_RotateBitmap16bit: _RotateBitmap16bit;
        proc_RotateBitmap2432bit: _RotateBitmap2432bit );
begin
  RotateProcs := AllRotators;
  _RotateBitmapRight( Self );
end;

procedure _RotateBitmapLeft( Src: TKOLBitmap );
begin
  _RotateBitmapRight( Src );
  _RotateBitmapRight( Src );
  _RotateBitmapRight( Src );
end;

procedure TKOLBitmap.RotateLeft;
begin
  RotateRight;
  _RotateBitmapRight( Self );
  _RotateBitmapRight( Self );
end;

procedure TKOLBitmap.RotateLeftMono;
begin
  if PixelFormat <> pf1bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmapMono := _RotateBitmapMono;
  _RotateBitmapRight( Self );
end;

procedure TKOLBitmap.RotateRightMono;
begin
  if PixelFormat <> pf1bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmapMono := _RotateBitmapMono;
  _RotateBitmapLeft( Self );
end;

procedure TKOLBitmap.RotateLeft16bit;
begin
  if PixelFormat <> pf16bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap16bit := _RotateBitmap16bit;
  _RotateBitmapLeft( Self );
end;

procedure TKOLBitmap.RotateLeft4bit;
begin
  if PixelFormat <> pf4bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap4bit := _RotateBitmap4bit;
  _RotateBitmapLeft( Self );
end;

procedure TKOLBitmap.RotateLeft8bit;
begin
  if PixelFormat <> pf8bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap8bit := _RotateBitmap8bit;
  _RotateBitmapLeft( Self );
end;

procedure TKOLBitmap.RotateLeftTrueColor;
begin
  if not (PixelFormat in [ pf24bit, pf32bit ]) then Exit; {>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap2432bit := _RotateBitmap2432bit;
  _RotateBitmapLeft( Self );
end;

procedure TKOLBitmap.RotateRight16bit;
begin
  if PixelFormat <> pf16bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap16bit := _RotateBitmap16bit;
  _RotateBitmapRight( Self );
end;

procedure TKOLBitmap.RotateRight4bit;
begin
  if PixelFormat <> pf4bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap4bit := _RotateBitmap4bit;
  _RotateBitmapRight( Self );
end;

procedure TKOLBitmap.RotateRight8bit;
begin
  if PixelFormat <> pf8bit then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap8bit := _RotateBitmap8bit;
  _RotateBitmapRight( Self );
end;

procedure TKOLBitmap.RotateRightTrueColor;
begin
  if not (PixelFormat in [ pf24bit, pf32bit ]) then Exit; {>>>>>>>>>>>>>>>>>>>>}
  RotateProcs.proc_RotateBitmap2432bit := _RotateBitmap2432bit;
  _RotateBitmapRight( Self );
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetPixels(X, Y: Integer): TColor;
var DC: HDC;
    Save: THandle;
begin
  Result := clNone;
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fDetachCanvas( Self );
  DC := CreateCompatibleDC( 0 );
  Save := SelectObject( DC, GetHandle );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( Save <> 0, 'Can not select bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  Result := Winapi.Windows.GetPixel( DC, X, Y );
  SelectObject( DC, Save );
  DeleteDC( DC );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetPixels(X, Y: Integer; const Value: TColor);
var DC: HDC;
    Save: THandle;
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  fDetachCanvas( Self );
  DC := CreateCompatibleDC( 0 );
  Save := SelectObject( DC, GetHandle );
  {$IFDEF KOL_ASSERTIONS}
  ASSERT( Save <> 0, 'Can not select bitmap to DC' );
  {$ENDIF KOL_ASSERTIONS}
  Winapi.Windows.SetPixel( DC, X, Y, ColorToRGB( Value ) );
  SelectObject( DC, Save );
  DeleteDC( DC );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function _GetDIBPixelsPalIdx( Bmp: TKOLBitmap; X, Y: Integer ): TColor;
var Pixel: Byte;
begin
  Pixel := PByte( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta
             + (X div (Bmp.fPixelsPerByteMask + 1)) )^;
  Pixel := ( Pixel shr ( (Bmp.fPixelsPerByteMask - (X and Bmp.fPixelsPerByteMask))
                       * Bmp.fDIBHeader.bmiHeader.biBitCount ) )
           and Bmp.fPixelMask;
  Result := TColor( ColorToRGBQuad( TColor( PRGBQuad( DWORD(@Bmp.fDIBHeader.bmiColors[ 0 ])
                           + Pixel * Sizeof( TRGBQuad ) )^ ) ) );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function _GetDIBPixels16bit( Bmp: TKOLBitmap; X, Y: Integer ): TColor;
var Pixel: Word;
begin
  Pixel := PWord( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta + X * 2 )^;
  if Bmp.fPixelMask = 15 then
    Result := (Pixel shr 7) and $F8 or (Pixel shl 6) and $F800
           or (Pixel shl 19) and $F80000
  else
    Result := (Pixel shr 8) and $F8 or (Pixel shl 5) and $FC00
           or (Pixel shl 19) and $F80000;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function _GetDIBPixelsTrueColor( Bmp: TKOLBitmap; X, Y: Integer ): TColor;
var Pixel: DWORD;
begin
  Pixel := PDWORD( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta +
                   X * Bmp.fBytesPerPixel )^ and $FFFFFF;
  Result := TColor( ColorToRGBQuad( TColor( Pixel ) ) );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION}
function _GetDIBPixelsTrueColorAlpha( Bmp: TKOLBitmap; X, Y: Integer ): TColor;
var Pixel: DWORD;
    RGB:   TRGBQuad;
    blue, red: Byte;
begin
  Pixel := PDWORD( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta +
                   X * Bmp.fBytesPerPixel )^;
  RGB := TRGBQuad(Pixel);
  blue := RGB.rgbRed;
  red  := RGB.rgbBlue;
  RGB.rgbBlue := blue;
  RGB.rgbRed  := red;
  Result := TColor( RGB );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function TKOLBitmap.GetDIBPixels(X, Y: Integer): TColor;
begin
  if not Assigned( fGetDIBPixels ) then
  begin
    if fHandleType = bmDIB then
    begin
      fScanLine0 := ScanLine[ 0 ];
      fScanLineDelta := Integer(ScanLine[ 1 ]) - Integer(fScanLine0);
      case PixelFormat of
      pf1bit:
        begin
          fPixelMask := $01;
          fPixelsPerByteMask := 7;
          fGetDIBPixels := _GetDIBPixelsPalIdx;
        end;
      pf4bit:
        begin
          fPixelMask := $0F;
          fPixelsPerByteMask := 1;
          fGetDIBPixels := _GetDIBPixelsPalIdx;
        end;
      pf8bit:
        begin
          fPixelMask := $FF;
          fPixelsPerByteMask := 0;
          fGetDIBPixels := _GetDIBPixelsPalIdx;
        end;
      pf15bit:
        begin
          fPixelMask := 15;
          fGetDIBPixels := _GetDIBPixels16bit;
        end;
      pf16bit:
        begin
          fPixelMask := 16;
          fGetDIBPixels := _GetDIBPixels16bit;
        end;
      pf24bit:
        begin
          fPixelsPerByteMask := 0;
          fBytesPerPixel := 3;
          fGetDIBPixels := _GetDIBPixelsTrueColor;
        end;
      pf32bit:
        begin
          fPixelsPerByteMask := 1;
          fBytesPerPixel := 4;
          fGetDIBPixels := {$IFDEF DIBPixels32bitWithAlpha} _GetDIBPixelsTrueColorAlpha
                           {$ELSE} _GetDIBPixelsTrueColor {$ENDIF};
        end;
      else;
      end;
    end;
    if not Assigned( fGetDIBPixels ) then
    begin
      Result := Pixels[ X, Y ]; Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    end;
  end;
  Result := fGetDIBPixels( Self, X, Y );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _SetDIBPixels1bit( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
var Pixel: Byte;
    Pos: PByte;
    Shf: Integer;
begin
  Value := ColorToRGB( Value );
  if ((Value shr 16) and $FF) + ((Value shr 8) and $FF) + (Value and $FF)
     < 255 * 3 div 2 then Pixel := 0 else Pixel := $80;
  Pos := PByte( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta + X div 8 );
  Shf := X and 7;
  Pos^ := Pos^ and ($FF7F shr Shf) or (Pixel shr Shf);
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _SetDIBPixelsPalIdx( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
var Pixel: Byte;
    Pos: PByte;
    Shf: Integer;
begin
  Pixel := Bmp.DIBPalNearestEntry( Value );
  Pos := PByte( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta
                + X div (Bmp.fPixelsPerByteMask + 1) );
  Shf := (Bmp.fPixelsPerByteMask - (X and Bmp.fPixelsPerByteMask))
         * Bmp.fDIBHeader.bmiHeader.biBitCount;
  Pos^ := Pos^ and not (Bmp.fPixelMask shl Shf) or (Pixel shl Shf);
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _SetDIBPixels16bit( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
var RGB16: Word;
    Pos: PWord;
begin
  Value := ColorToRGB( Value );
  if Bmp.fPixelMask = 15 then
    RGB16 := (Value shr 19) and $001F or (Value shr 6) and $03E0
          or (Value shl 7) and $7C00
  else
    RGB16 := (Value shr 19) and $001F or (Value shr 5) and $07E0
          or (Value shl 8) and $F800;
  Pos := PWord( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta + X * 2 );
  Pos^ := RGB16;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure _SetDIBPixelsTrueColor( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
var RGB: TRGBQuad;
    Pos: PDWord;
begin
  RGB := ColorToRGBQuad( Value );
  Pos := PDWORD( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta
                 + X * Bmp.fBytesPerPixel );
  Pos^ := Pos^ and $FF000000 or DWORD(RGB);
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION}
procedure _SetDIBPixelsTrueColorAlpha( Bmp: TKOLBitmap; X, Y: Integer; Value: TColor );
var RGB: TRGBQuad;
    Pos: PDWord;
    blue, red: Byte;
begin
  RGB := TRGBQuad(Value);
  blue := RGB.rgbRed;
  red  := RGB.rgbBlue;
  RGB.rgbBlue := blue;
  RGB.rgbRed  := red;
  Pos := PDWORD( Integer(Bmp.fScanLine0) + Y * Bmp.fScanLineDelta
                 + X * Bmp.fBytesPerPixel );
  Pos^ := Pos^ or DWORD(RGB);
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.SetDIBPixels(X, Y: Integer; const Value: TColor);
begin
  if not Assigned( fSetDIBPixels ) then
  begin
    if fHandleType = bmDIB then
    begin
      fScanLine0 := ScanLine[ 0 ];
      fScanLineDelta := Integer(ScanLine[ 1 ]) - Integer(fScanLine0);
      case PixelFormat of
      pf1bit:
        begin
          //fPixelMask := $01;
          //fPixelsPerByteMask := 7;
          fSetDIBPixels := _SetDIBPixels1bit;
        end;
      pf4bit:
        begin
          fPixelMask := $0F;
          fPixelsPerByteMask := 1;
          fSetDIBPixels := _SetDIBPixelsPalIdx;
        end;
      pf8bit:
        begin
          fPixelMask := $FF;
          fPixelsPerByteMask := 0;
          fSetDIBPixels := _SetDIBPixelsPalIdx;
        end;
      pf15bit:
        begin
          fPixelMask := 15;
          fSetDIBPixels := _SetDIBPixels16bit;
        end;
      pf16bit:
        begin
          fPixelMask := 16;
          fSetDIBPixels := _SetDIBPixels16bit;
        end;
      pf24bit:
        begin
          fPixelsPerByteMask := 0;
          fBytesPerPixel := 3;
          fSetDIBPixels := _SetDIBPixelsTrueColor;
        end;
      pf32bit:
        begin
          fPixelsPerByteMask := 1;
          fBytesPerPixel := 4;
          fSetDIBPixels := {$IFDEF DIBPixels32bitWithAlpha} _SetDIBPixelsTrueColorAlpha
                           {$ELSE} _SetDIBPixelsTrueColor {$ENDIF};
        end;
      else;
      end;
    end;
    if not Assigned( fSetDIBPixels ) then
    begin
      Pixels[ X, Y ] := Value; Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    end;
  end;
  fSetDIBPixels( Self, X, Y, Value );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.FlipVertical;
var DC: HDC;
    Save: THandle;
    TmpScan: PByte;
    Y: Integer;
begin
  if fHandle <> 0 then
  begin
    fDetachCanvas( Self );
    DC := CreateCompatibleDC( 0 );
    Save := SelectObject( DC, fHandle );
    StretchBlt( DC, 0, fHeight - 1, fWidth, -fHeight, DC, 0, 0, fWidth, fHeight, SRCCOPY );
    SelectObject( DC, Save );
    DeleteDC( DC );
  end
     else
  if fDIBBits <> nil then
  begin
    GetMem( TmpScan, ScanLineSize );
    for Y := 0 to fHeight div 2-1 do
    begin
      Move( ScanLine[ Y ]^, TmpScan^, fScanLineSize );
      Move( ScanLine[ fHeight - Y - 1 ]^, ScanLine[ Y ]^, fScanLineSize );
      Move( TmpScan^, ScanLine[ fHeight - Y - 1 ]^, fScanLineSize );
    end;
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.FlipHorizontal;
var DC: HDC;
    Save: THandle;
begin
  if GetHandle <> 0 then
  begin
    fDetachCanvas( Self );
    DC := CreateCompatibleDC( 0 );
    Save := SelectObject( DC, fHandle );
    StretchBlt( DC, fWidth - 1, 0, -fWidth, fHeight, DC, 0, 0, fWidth, fHeight, SRCCOPY );
    SelectObject( DC, Save );
    DeleteDC( DC );
  end;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLBitmap.CopyRect(const DstRect: TRect; SrcBmp: TKOLBitmap;
  const SrcRect: TRect);
var DCsrc, DCdst: HDC;
    SaveSrc, SaveDst: THandle;
begin
  if (GetHandle = 0) or (SrcBmp.GetHandle = 0) then Exit; {>>>>>>>>>>>>>>>>>>>>}
  fDetachCanvas( Self );
  SrcBmp.fDetachCanvas( SrcBmp );
  DCsrc := CreateCompatibleDC( 0 );
  SaveSrc := SelectObject( DCsrc, SrcBmp.fHandle );
  DCdst := DCsrc;
  SaveDst := 0;
  if SrcBmp <> Self then
  begin
    DCdst := CreateCompatibleDC( 0 );
    SaveDst := SelectObject( DCdst, fHandle );
  end;
  StretchBlt( DCdst, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
              DstRect.Bottom - DstRect.Top, DCsrc, SrcRect.Left, SrcRect.Top,
              SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
              SRCCOPY );
  if SrcBmp <> Self then
  begin
    SelectObject( DCdst, SaveDst );
    DeleteDC( DCdst );
  end;
  SelectObject( DCsrc, SaveSrc );
  DeleteDC( DCsrc );
end;
{$ENDIF PAS_VERSION}

function TKOLBitmap.CopyToClipboard: Boolean;
{var DibMem: PAnsiChar;
    HdrSize: Integer;
    Gbl: HGlobal;
    //Mem: TStream;
    //Sz: Integer;
    //Pt: Pointer;
    Restore_Compression: Integer;}
begin
  Result := FALSE;
(*  if Applet = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if not OpenClipboard( Applet.GetWindowHandle ) then Exit; {>>>>>>>>>>>>>>>>>>}
  if EmptyClipboard then
  begin
    HandleType := bmDIB;
    HdrSize := Sizeof( TBitmapInfoHeader );
    Restore_Compression := -1;
    TRY
      if fDIBHeader.bmiHeader.biBitCount <= 8 then
         Inc( HdrSize,
         (1 shl fDIBHeader.bmiHeader.biBitCount) * Sizeof( TRGBQuad ) )
      else
      begin
        if fDIBHeader.bmiHeader.biCompression = BI_RGB then
        begin
          CASE fDIBHeader.bmiHeader.biBitCount OF
          {24,} 32:
            begin
              Restore_Compression := fDIBHeader.bmiHeader.biCompression;
              fDIBHeader.bmiHeader.biCompression := BI_BITFIELDS;
              PDWORD( @ fDIBHeader.bmiColors[ 0 ] )^ := $FF0000;
              PDWORD( Integer( @ fDIBHeader.bmiColors[ 0 ] ) + 4 )^ := $FF00;
              PDWORD( Integer( @ fDIBHeader.bmiColors[ 0 ] ) + 8 )^ := $FF;
              Inc( HdrSize, 12 );
            end;
          END;
        end;
      end;
      Gbl := GlobalAlloc( GMEM_MOVEABLE, HdrSize + fDIBSize );
      DibMem := GlobalLock( Gbl );
      if DibMem <> nil then
      begin
        Move( fDIBHeader^, DibMem^, HdrSize );
        Move( fDIBBits^, Pointer( Integer( DibMem ) + HdrSize )^, fDIBSize );
        if not GlobalUnlock( Gbl ) and (GetLastError = NO_ERROR) then
        begin
          Result := SetClipboardData( CF_DIB, Gbl ) <> 0;
        end;
      end;
    FINALLY
      if Restore_Compression >= 0 then
        fDIBHeader.bmiHeader.biCompression := Restore_Compression;
    END;

  end;
  CloseClipboard; *)
end;

function TKOLBitmap.PasteFromClipboard: Boolean;
//var Gbl: HGlobal;
//    Size {, HdrSize}: Integer;
//    Mem: PAnsiChar;
//    Strm: TStream;
begin
  Result := FALSE;
(*  if Applet = nil then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  if not OpenClipboard( Applet.GetWindowHandle ) then Exit; {>>>>>>>>>>>>>>>>>>}
  TRY
  if IsClipboardFormatAvailable( CF_DIB ) then
  begin
    Gbl := GetClipboardData( CF_DIB );
    if Gbl <> 0 then
    begin
      Size := GlobalSize( Gbl );
      Mem := GlobalLock( Gbl );
      TRY
      if (Size > 0) and (Mem <> nil) then
      begin
        Strm := NewMemoryStream;
        Strm.Write( Mem^, Size );
        Strm.Position := 0;
        LoadFromStreamEx( Strm );
        Strm.Free;
        Result := not Empty;
      end;
      FINALLY
      GlobalUnlock( Gbl );
      END;
    end;
  end;
  FINALLY
  CloseClipboard;
  END; *)
end;



///////////////////////////////////////////////////////////////////////
//                             I  C  O  N
///////////////////////////////////////////////////////////////////////

type
  KOLString = string;
  PKOLChar = PChar;
  PStream = TStream;
  PIcon = TKOLIcon;

  TIconHeader = packed record
    idReserved: Word; (* Always set to 0 *)
    idType: Word;     (* Always set to 1 *)
    idCount: Word;    (* Number of icon images *)
    (* immediately followed by idCount TIconDirEntries *)
  end;

  TIconDirEntry = packed record
    bWidth: Byte;          (* Width *)
    bHeight: Byte;         (* Height *)
    bColorCount: Byte;     (* Nr. of colors used *)
    bReserved: Byte;       (* not used, 0 *)
    wPlanes: Word;         (* not used, 0 *)
    wBitCount: Word;       (* not used, 0 *)
    dwBytesInRes: Longint; (* total number of bytes in images *)
    dwImageOffset: Longint;(* location of image from the beginning of file *)
  end;


const
  spBegin = soFromBeginning;

const PossibleColorBits : array[1..7] of Byte = ( 1, 4, 8, 16, 24, 32, 0 );

function SaveIcons2StreamEx( const BmpHandles: array of HBitmap; Strm: PStream ): Boolean; forward;
procedure SaveIcons2Stream( const Icons : array of PIcon; Strm : PStream ); forward;
procedure SaveIcons2File( const Icons : array of PIcon; const FileName : KOLString ); forward;
function ExtractIcon(hInst: HINST; lpszExeFileName: PKOLChar;
  nIconIndex: UINT): HICON; stdcall;
  external 'shell32.dll' name 'ExtractIconW';


{ TKOLIcon }


procedure TKOLIcon.Clear;
begin
  if fHandle <> 0 then
  begin
    if not FShareIcon then
      DestroyIcon( fHandle );
    fHandle := 0;
  end;
  fShareIcon := False;
end;

{$IFDEF ASM_LOCAL}
  {$UNDEF ASM_LOCAL}
{$ENDIF}

{$IFNDEF ICON_DIFF_WH}
  {$IFDEF ASM_VERSION} {$DEFINE ASM_LOCAL} {$ENDIF}
{$ENDIF}

{$IFDEF ASM_LOCAL}
{$ELSE PAS_VERSION} //Pascal
function TKOLIcon.Convert2Bitmap(TranColor: TColor): HBitmap;
var DC0, DC2: HDC;
    Save: THandle;
    Br: HBrush;
begin
  Result := 0;
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  DC0 := GetDC( 0 );
  DC2 := CreateCompatibleDC( DC0 );
  {$IFDEF ICON_DIFF_WH}
  Result := CreateCompatibleBitmap( DC0, fWidth, fHeight );
  {$ELSE}
  Result := CreateCompatibleBitmap( DC0, fSize, fSize );
  {$ENDIF}
  Save := SelectObject( DC2, Result );
  Br := CreateSolidBrush( ColorToRGB( TranColor ) );
  {$IFDEF ICON_DIFF_WH}
  FillRect( DC2, MakeRect( 0, 0, fWidth, fHeight ), Br );
  {$ELSE}
  FillRect( DC2, MakeRect( 0, 0, fSize, fSize ), Br );
  {$ENDIF}
  DeleteObject( Br );
  Draw( DC2, 0, 0 );
  SelectObject( DC2, Save );
  DeleteDC( DC2 );
  ReleaseDC( 0, DC0 );
end;
constructor TKOLIcon.Create;
begin
  {$IFDEF ICON_DIFF_WH}
  FWidth := 32;
  FHeight := 32;
  {$ELSE}
  FSize := 32;
  {$ENDIF}
end;

{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
destructor TKOLIcon.Destroy;
begin
  Clear;
  inherited;
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLIcon.Draw(DC: HDC; X, Y: Integer);
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  {$IFDEF ICON_DIFF_WH}
  DrawIconEx( DC, X, Y, fHandle, fWidth, fHeight, 0, 0, DI_NORMAL );
  {$ELSE}
  DrawIconEx( DC, X, Y, fHandle, fSize, fSize, 0, 0, DI_NORMAL );
  {$ENDIF}
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLIcon.StretchDraw(DC: HDC; Dest: TRect);
begin
  if Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  DrawIconEx( DC, Dest.Left, Dest.Top, FHandle, Dest.Right - Dest.Left,
              Dest.Bottom - Dest.Top, 0, 0, DI_NORMAL );
end;
{$ENDIF PAS_VERSION}

function TKOLIcon.GetEmpty: Boolean;
begin
  Result := (fHandle = 0)
  {$IFDEF ICONLOAD_PRESERVEBMPS}
          and ((ImgBmp = nil) or ImgBmp.Empty)
  {$ENDIF ICONLOAD_PRESERVEBMPS}
  ;
end;

function TKOLIcon.GetHotSpot: TPoint;
var II : TIconInfo;
begin
  Result := MakePoint( 0, 0 );
  if FHandle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  GetIconInfo( FHandle, II );
  Result.x := II.xHotspot;
  Result.y := II.yHotspot;
  if II.hbmMask <> 0 then
    DeleteObject( II.hbmMask );
  if II.hbmColor <> 0 then
    DeleteObject( II.hbmColor );
end;

procedure TKOLIcon.LoadFromFile(const FileName: KOLString);
var Strm : PStream;
begin
  Strm := TFileStream.Create(fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream( Strm );
  finally
    Strm.Free;
  end;
end;

procedure TKOLIcon.LoadFromStream(Strm: PStream);
var DesiredSize : Integer;
    Pos : DWord;
    Mem : PStream;
    {$IFNDEF ICONLOAD_PRESERVEBMPS}
    ImgBmp, MskBmp : TKOLBitmap;
    {$ENDIF ICONLOAD_PRESERVEBMPS}
    TmpBmp: TKOLBitmap;
  function ReadIcon : Boolean;
  var IH : TIconHeader;
      IDI, FoundIDI : TIconDirEntry;
      I, J, SumSz, FoundSz, D : Integer;
      II : TIconInfo;
      BIH : TBitmapInfoheader;
      SzImg: DWORD;
  begin
     Result := False;
     if Strm.Read( IH, Sizeof( IH ) ) <> Sizeof( IH ) then Exit; {>>>>>>>>>>>>>}
     if (IH.idReserved = Sizeof( TBitmapInfoHeader )) then
     begin
       Strm.Position := Strm.Position - Sizeof( IH );
       {$IFDEF ICON_DIFF_WH} fWidth := 0;
                             fHeight := 0;
       {$ELSE}               fSize := 0;
       {$ENDIF}
       SumSz := 0;
     end
       else
     if (IH.idReserved = 0) and ((IH.idType = 1) or (IH.idType = 2)) and
        (IH.idCount >= 1) then
     begin
       if (IH.idReserved <> 0) or ((IH.idType <> 1) and (IH.idType <> 2)) or
          (IH.idCount < 1) or (IH.idCount >= 1024) then Exit; {>>>>>>>>>>>>>>>>}
       SumSz := Sizeof( IH );
       FoundSz := 1000000;
       for I := 1 to IH.idCount do
       begin
          if Strm.Read( IDI, Sizeof( IDI ) ) <> Sizeof( IDI ) then Exit; {>>>>>}
          Inc( SumSz, IDI.dwBytesInRes + Sizeof( IDI ) );
          D := IDI.bWidth - DesiredSize;
          if D < 0 then D := -D;
          if D < FoundSz then
          begin
             FoundSz := D;
             FoundIDI := IDI;
          end;
       end;
       if FoundSz = 1000000 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
       Strm.Position := Integer( Pos ) + FoundIDI.dwImageOffset;
       {$IFDEF ICON_DIFF_WH} fWidth := FoundIDI.bWidth;
                             fHeight := FoundIDI.bHeight;
       {$ELSE} fSize := FoundIDI.bWidth;
       {$ENDIF}
     end else Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
     if Strm.Read( BIH, Sizeof( BIH ) ) <> Sizeof( BIH ) then Exit; {>>>>>>>>>>}
     {$IFDEF ICON_DIFF_WH}
     fWidth := BIH.biWidth;
     BIH.biHeight := BIH.biHeight div 2; // fSize;
     fHeight := BIH.biHeight;
     {$ELSE}
     fSize := BIH.biWidth;
     BIH.biHeight := BIH.biHeight div 2; // fSize;
     {$ENDIF}
     Mem := TMemoryStream.Create;
     if (FoundIDI.bColorCount >= 2) or (FoundIDI.bReserved = 1) or
        (FoundIDI.bColorCount = 0) then
     begin
       I := 0;
       SzImg := ((BIH.biBitCount * BIH.biWidth + 31) div 32) * 4 * BIH.biHeight;
       if (BIH.biSizeImage > 0) and (SzImg > BIH.biSizeImage) then
         SzImg := BIH.biSizeImage;
       if BIH.biBitCount <= 8 then
       begin
          I := (1 shl BIH.biBitCount) * Sizeof( TRGBQuad );
       end;
       Mem.Write( BIH, Sizeof( BIH ) );
       if I > 0 then
       begin
          if mem.CopyFrom(strm, I) <> I then Exit; {>>>>>>>>>>>>}
       end
       else
         if BIH.biBitCount = 16 then
         begin
             if  BIH.biCompression = BI_BITFIELDS then  // + by mdw - fix for
                 mem.CopyFrom(strm, 12)           // 16 bit per pixels
             else
                 for I := 0 to 2 do
                 begin
                   J := InitColors[ I ];
                   Mem.Write( J, 4 );
                 end;
         end;
       I := mem.CopyFrom(strm, SzImg );
       if I <> Integer( SzImg ) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
       {$IFDEF ICON_DIFF_WH}
       ImgBmp := TKOLBitmap.Create(fWidth, fHeight);
       {$ELSE}
       ImgBmp := NewBitmap( fSize, fSize );
       {$ENDIF}
       {$IFDEF ICONLOAD_PRESERVEBMPS}
       Add2AutoFree( ImgBmp );
       {$ENDIF ICONLOAD_PRESERVEBMPS}
       Mem.Seek( 0, spBegin );
       {$IFDEF LOADEX}
       ImgBmp.LoadFromStreamEx( Mem );
       {$ELSE}
       ImgBmp.LoadFromStream( Mem );
       {$ENDIF}
       if ImgBmp.Empty then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
     end
       else
     begin
       Mem.Write( BIH, Sizeof( BIH ) );
     end;

     BIH.biBitCount := 1;
     BIH.biPlanes := 1;
     BIH.biClrUsed := 0;
     BIH.biCompression := 0;
     Mem.Seek( 0, spBegin );
     BIH.biSizeImage := ((BIH.biWidth + 31) div 32) * 4 * BIH.biHeight;
     Mem.Write( BIH, Sizeof( BIH ) );
     I := 0;
     Mem.Write( I, Sizeof( I ) );
     I := $FFFFFF;
     Mem.Write( I, Sizeof( I ) );
     I := BIH.biSizeImage;
     J := mem.CopyFrom(strm, I );
     while J < I do
     begin
       D := 0;
       Mem.Write( D, 4 );
       Inc( J, 4 );
     end;

       {$IFDEF ICON_DIFF_WH}
       MskBmp := TKOLBitmap.Create(fWidth, fHeight);
       {$ELSE}
       MskBmp := NewBitmap( fSize, fSize );
       {$ENDIF}
       {$IFDEF ICONLOAD_PRESERVEBMPS}
       Add2AutoFree( MskBmp );
       {$ENDIF ICONLOAD_PRESERVEBMPS}
       Mem.Seek( 0, spBegin );
       {$IFDEF LOADEX}
       MskBmp.LoadFromStreamEx( Mem );
       {$ELSE}
       MskBmp.LoadFromStream( Mem );
       {$ENDIF}

    {$IFDEF ICONLOAD_PRESERVEBMPS}
    Result := TRUE;
    if not Only_Bmp then
    {$ENDIF ICONLOAD_PRESERVEBMPS}
    begin
     II.fIcon := True;
     II.xHotspot := 0;
     II.yHotspot := 0;
     II.hbmMask := 0;
     if Assigned( MskBmp ) and not MskBmp.Empty then
       II.hbmMask := MskBmp.Handle;
     II.hbmColor := 0;
     if ImgBmp <> nil then
        II.hbmColor := ImgBmp.Handle;
     fHandle := CreateIconIndirect( II );
     if SumSz > 0 then
       Strm.Seek( Integer( Pos ) + SumSz, spBegin );
     Result := fHandle <> 0;
  end;

  end;
begin
  DesiredSize := Size;
  if DesiredSize = 0 then
     DesiredSize := GetSystemMetrics( SM_CXICON );
  Clear;
  Pos := Strm.Position;

  Mem := nil;
  {$IFDEF ICONLOAD_PRESERVEBMPS}
  if ImgBmp <> nil then
  begin
    RemoveFromAutoFree( ImgBmp );
    RemoveFromAutoFree( MskBmp );
    Free_And_Nil( ImgBmp );
    Free_And_Nil( MskBmp );
  end;
  {$ELSE}
  ImgBmp := nil;
  MskBmp := nil;
  {$ENDIF ICONLOAD_PRESERVEBMPS}
  TmpBmp := nil;

  if not ReadIcon then
  begin
     Clear;
     Strm.Seek( Pos, spBegin );
  end;

  Mem.Free;
  {$IFNDEF ICONLOAD_PRESERVEBMPS}
  ImgBmp.Free;
  MskBmp.Free;
  {$ENDIF ICONLOAD_PRESERVEBMPS}
  TmpBmp.Free;
end;

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLIcon.SaveToFile(const FileName: KOLString);
begin
  SaveIcons2File( [ @Self ], FileName );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
procedure TKOLIcon.SaveToStream(Strm: PStream);
begin
  SaveIcons2Stream( [ @Self ], Strm );
end;
{$ENDIF PAS_VERSION}

{$IFDEF ASM_noVERSION}
procedure TKOLIcon.SetHandle(const Value: HIcon);
const szII = sizeof( TIconInfo );
      szBIH = sizeof(TBitmapInfoHeader);
asm     //cmd    //opd
        CMP      EDX, [EAX].fHandle
        JE       @@exit
        PUSHAD
        PUSH     EDX
        MOV      EBX, EAX
        CALL     Clear
        POP      ECX
        MOV      [EBX].fHandle, ECX
        JECXZ    @@fin
        ADD      ESP, -szBIH
        PUSH     ESP
        PUSH     ECX
        CALL     GeTIconInfo
        MOV      ESI, [ESP].TIconInfo.hbmMask
        MOV      EDI, [ESP].TIconInfo.hbmColor
        PUSH     ESP
        PUSH     szBIH
        PUSH     ESI
        CALL     GetObject
        POP      EAX
        POP      [EBX].fSize
        ADD      ESP, szBIH-8
        TEST     ESI, ESI
        JZ       @@1
        PUSH     ESI
        CALL     DeleteObject
@@1:    TEST     EDI, EDI
        JZ       @@fin
        PUSH     EDI
        CALL     DeleteObject
@@fin:  POPAD
@@exit:
end;
{$ELSE PAS_VERSION} //Pascal
procedure TKOLIcon.SetHandle(const Value: HIcon);
var II : TIconInfo;
    B: TagBitmap;
begin
  if FHandle = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Clear;
  FHandle := Value;
  if Value <> 0 then
  begin
     GetIconInfo( FHandle, II );
     GetObject( II.hbmMask, Sizeof( B ), @B );
     {$IFDEF ICON_DIFF_WH}
     fWidth := B.bmWidth;
     fHeight := B.bmHeight;
     {$ELSE}
     fSize := B.bmWidth;
     {$ENDIF}
     if II.hbmMask <> 0 then
       DeleteObject( II.hbmMask );
     if II.hbmColor <> 0 then
       DeleteObject( II.hbmColor );
  end;
end;
{$ENDIF PAS_VERSION}

procedure TKOLIcon.SetHandleEx(NewHandle: HIcon);
begin
  if FHandle = NewHandle then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  Clear;
  FHandle := NewHandle;
end;

procedure TKOLIcon.SetSize(const Value: Integer);
begin
  {$IFDEF ICON_DIFF_WH}
  if (fWidth = Value) and (fHeight = Value) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>}
  {$ELSE}
  if FSize = Value then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
  {$ENDIF}
  Clear;
  {$IFDEF ICON_DIFF_WH}
  fWidth := Value;
  fHeight := Value;
  {$ELSE}
  FSize := Value;
  {$ENDIF}
end;

{$IFDEF ICON_DIFF_WH}
function TKOLIcon.GeTKOLIconSize: Integer;
begin
  Result := Max( fWidth, fHeight );
end;
{$ENDIF}

{$IFDEF ASM_VERSION}{$ELSE PAS_VERSION} //Pascal
function ColorBits( ColorsCount : Integer ) : Integer;
var I : Integer;
begin
   for I := 1 to 6 do
   begin
      Result := PossibleColorBits[ I ];
      if (1 shl Result) >= ColorsCount then break;
   end;
end;
{$ENDIF PAS_VERSION}

function SaveIcons2StreamEx( const BmpHandles: array of HBitmap; Strm: PStream ): Boolean;
var I, Off : Integer;
   IDI : TIconDirEntry;
   BIH : TBitmapInfoHeader;
   B: TagBitmap;
  function RGBArraySize : Integer;
  begin
     Result := 0;
     if (IDI.bColorCount >= 2) or (IDI.bReserved = 1) then
        Result := (IDI.bColorCount + (IDI.bReserved shl 8)) * Sizeof( TRGBQuad );
  end;
  function ColorDataSize( W, H: Integer ) : Integer;
  var N: Integer;
  begin
     if (IDI.bColorCount >= 2) or (IDI.bReserved = 1) then
       N := (ColorBits( IDI.bColorCount + (IDI.bReserved shl 8) ) )
     else
     begin
       N := IDI.wBitCount;
     end;
     Result := ((N * W + 31) div 32) * 4
                   * H;
  end;
  function MaskDataSize( W, H: Integer ) : Integer;
  begin
     Result := ((W + 31) div 32) * 4 * H;
  end;
var BColor, BMask: HBitmap;
    W, H: Integer;
    ImgBmp, MskBmp: TKOLBitmap;
    IH : TIconHeader;
    Colors : TList;
begin
  {$IFDEF KOL_ASSERTIONS}
  Assert( (High(BmpHandles) >= 0) and (High(BmpHandles) and 1 <> 0),
          'Incorrect parameters count in call to SaveIcons2StreamEx' );
  {$ENDIF KOL_ASSERTIONS}
  Result := False;
  IH.idReserved := 0;
  IH.idType := 1;
  IH.idCount := (High( BmpHandles )+1) div 2;
  if Strm.Write( IH, Sizeof( IH ) ) <> Sizeof( IH ) then Exit; {>>>>>>>>>>>>>>>}
  Off := Sizeof( IH ) + IH.idCount * Sizeof( IDI );
  Colors := TList.Create;
  ImgBmp := TKOLBitmap.Create( 0, 0 );
  MskBmp := TKOLBitmap.Create( 0, 0 );
  TRY

    for I := 0 to High( BmpHandles ) div 2 do
    begin
      BColor := BmpHandles[ I * 2 ];
      BMask  := BmpHandles[ I * 2 + 1 ];
      if (BColor = 0) and (BMask = 0) then break;
      {$IFDEF KOL_ASSERTIONS}
      Assert( BMask <> 0, 'Mask bitmap not provided for saving icons in SaveIcons2StreamEx' );
      {$ENDIF KOL_ASSERTIONS}
      GetObject( BMask, Sizeof( B ), @ B );
      W := B.bmWidth;
      H := B.bmHeight;
      if BColor <> 0 then
      begin
        GetObject( BColor, Sizeof( B ), @B );
        {$IFDEF KOL_ASSERTIONS}
        Assert( (B.bmWidth = W) and (B.bmHeight = H),
                'Mask bitmap size must much color bitmap size in SaveIcons2StreamEx' );
        {$ENDIF KOL_ASSERTIONS}
      end;
      ZeroMemory( @IDI, Sizeof( IDI ) );

      IDI.bWidth := W;
      IDI.bHeight := H;
      if BColor = 0 then
        IDI.bColorCount := 2
      else
      begin
        ImgBmp.Handle := CopyImage( BColor, IMAGE_BITMAP, W, H,
                         LR_CREATEDIBSECTION );
        ZeroMemory( @BIH, Sizeof( BIH ) );
        BIH.biSize := Sizeof( BIH );
        GetObject( ImgBmp.Handle, Sizeof( B ), @B );
        if (B.bmPlanes = 1) and (B.bmBitsPixel >= 15) then
        begin
          IDI.bColorCount := 0;
          IDI.bReserved := 0;
          IDI.wBitCount := B.bmBitsPixel;
        end
          else
        if B.bmPlanes * (1 shl B.bmBitsPixel) < 16 then
        begin
           ImgBmp.PixelFormat := pf1bit;
           IDI.bColorCount := 2;
        end
           else
        if B.bmPlanes * (1 shl B.bmBitsPixel) < 256 then
        begin
           ImgBmp.PixelFormat := pf4bit;
           IDI.bColorCount := 16;
        end
           else
        begin
           ImgBmp.PixelFormat := pf8bit;
           IDI.bColorCount := 0;
           IDI.bReserved := 1;
        end;
      end;
      Colors.Add( Pointer(IDI.bColorCount + (IDI.bReserved shl 8)) );
      IDI.dwBytesInRes := Sizeof( BIH ) +  RGBArraySize +
                          ColorDataSize( W, H ) + MaskDataSize( W, H );
      IDI.dwImageOffset := Off;
      if Strm.Write( IDI, Sizeof( IDI ) ) <> Sizeof( IDI ) then Exit; {>>>>>>>>}
      Inc( Off, IDI.dwBytesInRes );
    end;
    for I := 0 to High( BmpHandles ) div 2 do
    begin
      BColor := BmpHandles[ I * 2 ];
      BMask  := BmpHandles[ I * 2 + 1 ];
      if (BColor = 0) and (BMask = 0) then break;
      GetObject( BMask, Sizeof( B ), @ B );
      W := B.bmWidth;
      H := B.bmHeight;

      ZeroMemory( @BIH, Sizeof( BIH ) );
      BIH.biSize := Sizeof( BIH );
      BIH.biWidth := W;
      BIH.biHeight := H;
      if BColor <> 0 then
        BIH.biHeight := W * 2;
      BIH.biPlanes := 1;
      PWord( @ IDI.bColorCount )^ := DWord( Colors.Items[ I ] );
      if IDI.wBitCount = 0 then
        IDI.wBitCount := ColorBits( PWord( @ IDI.bColorCount )^ );
      BIH.biBitCount := IDI.wBitCount;
      BIH.biSizeImage := Sizeof( BIH ) + ColorDataSize( W, H ) + MaskDataSize( W, H );
      if Strm.Write( BIH, Sizeof( BIH ) ) <> Sizeof( BIH ) then Exit; {>>>>>>>>}
      if BColor <> 0 then
      begin

        ImgBmp.Handle := CopyImage( BColor, IMAGE_BITMAP, W, H, 0 );
        case BIH.biBitCount of
        1 : ImgBmp.PixelFormat := pf1bit;
        4 : ImgBmp.PixelFormat := pf4bit;
        8 : ImgBmp.PixelFormat := pf8bit;
        16: ImgBmp.PixelFormat := pf16bit;
        24: ImgBmp.PixelFormat := pf24bit;
        32: ImgBmp.PixelFormat := pf32bit;
        end;
      end
        else
      begin
        ImgBmp.Handle := CopyImage( BMask, IMAGE_BITMAP, W, H, 0 );
        ImgBmp.PixelFormat := pf1bit;
      end;
      if ImgBmp.FDIBBits <> nil then
      begin
        if Strm.Write( Pointer(Integer(ImgBmp.FDIBHeader) + Sizeof(TBitmapInfoHeader))^,
           PWord( @ IDI.bColorCount )^ * Sizeof( TRGBQuad ) ) <>
           PWord( @ IDI.bColorCount )^ * Sizeof( TRGBQuad ) then Exit; {>>>>>>>}
        if Strm.Write( ImgBmp.FDIBBits^, ColorDataSize( W, H ) ) <>
           ColorDataSize( W, H ) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>}
      end;
    MskBmp.Handle := CopyImage( BMask, IMAGE_BITMAP, W, H, 0 );

    MskBmp.PixelFormat := pf1bit;
    if Strm.Write( MskBmp.FDIBBits^, MaskDataSize( W, H ) ) <>
      MaskDataSize( W, H ) then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
    end;

  FINALLY
    Colors.Free;
    ImgBmp.Free;
    MskBmp.Free;
  END;
  Result := True;
end;

{$IFDEF FPC}
  {$DEFINE _D3orFPC}
{$ENDIF}
{$IFDEF _D2orD3}
  {$DEFINE _D3orFPC}
{$ENDIF}
procedure SaveIcons2Stream( const Icons : array of PIcon; Strm : PStream );
var I, J, Pos : Integer;
    {$IFDEF _D3orFPC}
    Bitmaps: array[ 0..63 ] of HBitmap;
    {$ELSE DELPHI}
    Bitmaps: array of HBitmap;
    {$ENDIF FPC/DELPHI}
    II: TIconInfo;
    Bmp: HBitmap;
begin
  for I := 0 to High( Icons ) do
  begin
     if Icons[ I ].Handle = 0 then Exit; {>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
     for J := I + 1 to High( Icons ) do
        if Icons[ I ].Size = Icons[ J ].Size then Exit; {>>>>>>>>>>>>>>>>>>>>>>}
  end;
  Pos := Strm.Position;

  {$IFDEF _D3orFPC}
  for I := 0 to High( Bitmaps ) do
    Bitmaps[ I ] := 0;
  {$ELSE DELPHI}
  SetLength( Bitmaps, Length( Icons ) * 2 );
  {$ENDIF FPC/DELPHI}
  for I := 0 to High( Icons ) do
  begin
    GeTIconInfo( Icons[ I ].Handle, II );
    Bitmaps[ I * 2 ] := II.hbmColor;
    Bitmaps[ I * 2 + 1 ] := II.hbmMask;
  end;

  if not SaveIcons2StreamEx( Bitmaps, Strm ) then
     Strm.Seek( Pos, spBegin );

  for I := 0 to High( Bitmaps ) do
  begin
    Bmp := Bitmaps[ I ];
    if Bmp <> 0 then
      DeleteObject( Bmp );
  end;
end;

procedure SaveIcons2File( const Icons : array of PIcon; const FileName : KOLString );
var Strm: PStream;
begin
  Strm := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite);
  SaveIcons2Stream( Icons, Strm );
  Strm.Free;
end;

procedure TKOLIcon.LoadFromExecutable(const FileName: KOLString; IconIdx: Integer);
var I: Integer;
begin
  Clear;
  I := ExtractIcon( hInstance, PKOLChar( FileName ), IconIdx );
  if I > 1 then
    Handle := I;
end;

function GetFileIconCount( const FileName: KOLString ): Integer;
begin
  Result := ExtractIcon( hInstance, PKOLChar( FileName ), DWORD(-1) );
end;

procedure TKOLIcon.LoadFromResourceID(Inst, ResID, DesiredSize: Integer);
begin
  LoadFromResourceName( Inst, MAKEINTRESOURCE( ResID ), DesiredSize );
end;

procedure TKOLIcon.LoadFromResourceName(Inst: Integer; ResName: PKOLChar; DesiredSize: Integer);
begin
  Handle := LoadImage( Inst, ResName, IMAGE_ICON, DesiredSize, DesiredSize, $8000 {LR_SHARED} );
  if fHandle <> 0 then FShareIcon := True;
end;

function LoadImgIcon( RsrcName: PKOLChar; Size: Integer ): HIcon;
begin
  Result := LoadImage( hInstance, RsrcName, IMAGE_ICON, Size, Size, $8000 {LR_SHARED} );
end;

end.
