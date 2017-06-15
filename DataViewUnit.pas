unit DataViewUnit;


interface
uses
  Winapi.Windows, CommonUtilities, Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms, Math,
  Winapi.Messages, Themes, Winapi.UxTheme;


type

  TViewerElement = ( hvOffset, hvHexData, hvCharData );
  TViewerElements = set of TViewerElement;
  TViewerMode = ( hmHex, hmTextBinary, hmTextFree{, hmTextWordWrap} );
  TMoveDirection = ( mdLeft, mdTop, mdRight, mdBottom );


  TPos = packed record
    Col, Row: Integer;
  end;
  PPos = ^TPos;



  TDataViewer = class(TPanel)
  private
    FBufferSize: Integer;
    FLineCount: Integer;
    FBorderLeft,
    FBorderRight,
    FPosPerScroll,
    FPageSize: Byte;
    FDataHeight,
    FDataWidth: Integer;
    FCharHeight,
    FCharWidth: Integer;
    FSelecting: Boolean;
    FPrevCaptureWnd: HWND;
    FSL: TStringList;
    FScrollTimer: TTimer;
    FScrollDelta: TSmallPoint;
    FCaretShown: Boolean;
    FFreezeCount: Integer;
    FScrollTimerInterval: Integer;

    FStream: TStream;
    FStreamDataOffset: Integer;
    FVisibleElements: TViewerElements;
    FNonPrintReplacement: AnsiChar;
    FSelStart: Integer;
    FSelEnd: Integer;
    FSelectionElement: TViewerElement;
    FFontColor: TColor;
    FSelBkColor: TColor;
    FSelFontColor: TColor;
    FBkColor: TColor;
    FNonSelectAreaCursor: TCursor;
    FMode: TViewerMode;
    FCurLineWidth: Integer;
    FPrevSelEnd: Integer;
    FSelEndPos: TPos;

    procedure ScrollBoxOnResize(Sender: TObject);
    procedure ScrollBoxOnMouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxOnMouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxOnMouseWheel(
      Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure SetVisibleElements(Value: TViewerElements);
    procedure SetSelEnd(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelectionElement( Value: TViewerElement );
    procedure SetMode( Value: TViewerMode );
    procedure SetCurLineWidth(Value: Integer);
    procedure ScrollTimerOnTimer(Sender: TObject);
    procedure ScrollBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetFreezed: Boolean;
    function GetHScroll: Integer;
    function GetVScroll: Integer;
    procedure SetHScroll(Value: Integer);
    procedure SetVScroll(Value: Integer);
    function GetFullLines: Integer;
    function GetVisibleLines: Integer;
    function GetFullCols: Integer;
    function GetSelEndChanged: Boolean;
    procedure SetSelEndPos( Value: TPos; NewLineIfEOL: Boolean = False );
    function GetEmpty: Boolean;
    function DrawFrame: Boolean;
  private
    fSelInactiveColor: TColor;
    FBinaryTextLineWidth: Integer;
    procedure SetFontColor(Value: TColor);
    function GetFontSize: Integer;
    procedure SetFontSize(Value: Integer);
  protected
    procedure WndProc(var AMessage: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ClearSelection;
    function ClientCursorPos: TPoint;
    procedure DecPointOnScroll(var Pt: TPoint);
    function ElementFromPos(const Pos: Integer): Byte;
    function GetCharPoint(N: Integer; Element: TViewerElement): TPoint;
    function GetCharPos(N: Integer; BlockRecursive: Boolean = False): TPos;
    function GetPointPos(const Point: TPoint;
      Element: TViewerElement): TPos;
    function GetPosChar(Pos: TPos): Integer;
    function GetPosPoint(Pos: TPos; Element: TViewerElement): TPoint;
    procedure HideCaret;
    procedure MakePosVisible(const Pos: TPos);
    function NewNormalCanvas: TCanvas;
    {$WARNINGS OFF}
    procedure Paint(DC: HDC; const UpdateRect: TRect);
    procedure ProcessHotKeys(HK: THotKey);
    {$WARNINGS ON}
    procedure SetHCursor(MouseX: Integer);
    procedure SetIntDefaults;
    procedure SetNormalCanvas(C: TCanvas);
    procedure SetSelectedCanvas(C: TCanvas);
    procedure SetSelInactiveCanvas(C: TCanvas);
    procedure SetVisDefaults;
    procedure ShowCaret;
    procedure UpdateCaretPos(CalcCharPos: Boolean = False; Pos: PPos = nil);
    procedure UpdateMousePos(const Mouse: TPoint);
    procedure UpdateProperties;
    function IsBarVisible(ABar: NativeUInt): Boolean;

    property PrevSelEnd: Integer read fPrevSelEnd write fPrevSelEnd;
    property SelEndChanged: Boolean read GetSelEndChanged;
    property SelEndPos: TPos read fSelEndPos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Mode: TViewerMode read fMode write SetMode;
    property Stream: TStream read FStream;
    property StreamDataOffset: Integer read FStreamDataOffset;
    property StreamDataSize: Integer read FBufferSize;
    property VisibleElements: TViewerElements read fVisibleElements write SetVisibleElements;
    property SelStart: Integer read fSelStart write SetSelStart;
    property SelEnd: Integer read fSelEnd write SetSelEnd;
    property SelectionElement: TViewerElement read fSelectionElement write SetSelectionElement;
    property Selecting: Boolean read fSelecting write fSelecting;
    property LineCount: Integer read fLineCount;
    property CurLineWidth: Integer read FCurLineWidth write SetCurLineWidth;
    property FullLines: Integer read GetFullLines;
    property VisibleLines: Integer read GetVisibleLines;
    property FullCols: Integer read GetFullCols;
    property VScroll: Integer read GetVScroll write SetVScroll;
    property HScroll: Integer read GetHScroll write SetHScroll;
    property Freezed: Boolean read GetFreezed;
    property Empty: Boolean read GetEmpty;

    procedure SetStream(
      AStream: TStream; AStreamDataOffset, AStreamDataSize: Cardinal; AMode: TViewerMode);
    procedure Freeze;
    procedure UnFreeze;
    {$WARNINGS OFF}
    procedure Invalidate;
    procedure ScrollPage( Direction: TMoveDirection );
    {$WARNINGS ON}
    procedure SelectAll;

    procedure SetHandlers;
  published
    property NonPrintReplacement: AnsiChar read fNonPrintReplacement write fNonPrintReplacement;
    property BkColor: TColor read fBkColor write fBkColor;
    property FontColor: TColor read FFontColor write SetFontColor;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property SelBkColor: TColor read fSelBkColor write fSelBkColor;
    property SelFontColor: TColor read fSelFontColor write fSelFontColor;
    property SelInactiveColor: TColor read fSelInactiveColor write fSelInactiveColor;
    property NonSelectAreaCursor: TCursor read FNonSelectAreaCursor write FNonSelectAreaCursor;
    property BinaryTextLineWidth: Integer read FBinaryTextLineWidth write FBinaryTextLineWidth;
  end;



function ConvertNonPrintingChar(Chr, Replace: AnsiChar): AnsiChar;


const
  Def_NPRepl = '.';


implementation


const

  AddressTempl = '00000000';
  AddressDelTempl = ': ';
  HexTempl = '00 00 00 00 00 00 00 00|00 00 00 00 00 00 00 00';
  HexDelTempl = ' | ';
  DataTempl = 'QQQQQQQQQQQQQQQQ';

  Def_Separators = ' ,.;:/|\+=-@^&*()'''#9;


//  Def_NPRepl = #128;
  Def_SelBkColor = clHighlight;
  Def_SelFontColor = clHighlightText;
  Def_BkColor = clWindow;
  Def_FontColor = clWindowText;
  Def_SelInactiveColor = clBtnFace;


function LinesPerHeight( const Rect: TRect; LineHeight: Integer ): Integer;
begin
  Result := (Rect.Bottom - Rect.Top) div LineHeight;
end;


function LinesPerHeight2( const Rect: TRect; LineHeight: Integer ): Integer;
begin
  Result := (Rect.Bottom - Rect.Top) div LineHeight;
  if (Rect.Bottom - Rect.Top) mod LineHeight > 0 then
    Inc( Result );
end;


function ColsPerWidth( const Rect: TRect; ColWidth: Integer ): Integer;
begin
  Result := (Rect.Right - Rect.Left) div ColWidth;
end;


function ConvertNonPrintingChar(Chr, Replace: AnsiChar): AnsiChar;
begin
  Result := Chr;
  if (Byte(Result) < 32) or (Byte(Result) >= $80) and (Byte(Result) < $90) then
    Result := Replace;
end;


procedure ConvertNonPrintingChars(
  Str: PAnsiChar; Len: Integer; Replace: AnsiChar; ReplaceCrLf: Boolean);
var
  I: Integer;
begin
  I := 0;
  while I < Len do
  begin
    if Str[ I ] < #32 then
      if ReplaceCrLf then
        Str[ I ] := Replace
      else
        if (Str[ I ] <> #13) then
          Str[ I ] := Replace
        else
          if (I + 1 < Len) and (Str[ I + 1 ] = #10) then
            Inc( I );
    Inc( I );
  end;
end;


procedure Stream2StrList(AList: TStringList; AStream: TStream);

  procedure SetTextStr(SL: TStringList; Data: Pointer; DataLength: SysUInt);
  var
    P, Start, LB: PAnsiChar;
    S: AnsiString;
    LineBreakLen, lengthMod: Integer;
  begin
    SL.BeginUpdate;
    try
      SL.Clear;
      P := Data;
      if P <> nil then
        if CompareStr(SL.LineBreak, sLineBreak) = 0 then
        begin
          // This is a lot faster than using StrPos/AnsiStrPos when
          // LineBreak is the default (#13#10)
//          while P^ <> #0 do
          while (SysUInt(P) - SysUInt(Data)) < DataLength do
          begin
            Start := P;
            while not (P^ in [#0, #10, #13]) do
              Inc(P);
            SetString(S, Start, P - Start);
            SL.AddObject(string(S), Pointer(Start));
            if P^ = #13 then Inc(P);
            if P^ = #10 then Inc(P);
          end;
        end
        else
        begin
          LineBreakLen := Length(SL.LineBreak);
//          while P^ <> #0 do
          while (SysUInt(P) - SysUInt(Data)) < DataLength do
          begin
            Start := P;
            LB := AnsiStrPos(P, PAnsiChar(AnsiString(SL.LineBreak)));
            while (P^ <> #0) and (P <> LB) do
              Inc(P);
            // удаляем символ 0xD если разделитель строк 0xA и встречаем разделение строк 0xD 0xA
            if (P^ <> #0) and (P <> Start) and (SL.LineBreak = #10) and (P[-1] = #13) then
              lengthMod := -1
            else
              lengthMod := 0;

            SetString(S, Start, P - Start + lengthMod);
            SL.AddObject(string(S), Pointer(Start));
            if P = LB then
              Inc(P, LineBreakLen);
          end;
        end;
    finally
      SL.EndUpdate;
    end;
  end;

var
  i: Integer;
  buffer: Pointer;
begin
  if (AList.Count > 0) then
  begin
    FreeMem(Pointer(AList.Objects[0]));
    for i := 0 to AList.Count - 1 do
      AList.Objects[i] := nil;
    AList.Clear;
  end;

  GetMem(buffer, AStream.Size);
  AStream.Read(buffer^, AStream.Size);
//  PByte(buffer)[AStream.Size] := 0;
//  PByte(buffer)[AStream.Size + 1] := 0;

  Alist.LineBreak := #10;
  SetTextStr(AList, buffer, AStream.Size);
end;


function MyNewCanvas(DC: HDC): TCanvas;
begin
  Result := TCanvas.Create;
  Result.Handle := DC;
end;

type
  TCControl = class(TControl);


{ TDataViewer }


procedure TDataViewer.ClearSelection;
begin
  Freeze;
  SelStart := 0;
  SelEnd := 0;
  UnFreeze;
end;


function TDataViewer.ClientCursorPos: TPoint;
begin
  GetCursorPos(Result);
  Result := ScreenToClient(Result);
end;

constructor TDataViewer.Create(AOwner: TComponent);
begin
  inherited;
  fSL := TStringList.Create;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;

  FBinaryTextLineWidth := 80;

  SetIntDefaults;
  SetVisDefaults;

  SetHandlers;

  FScrollTimer.OnTimer := ScrollTimerOnTimer;
  AutoSize := False;
  TabStop := True;
end;

procedure TDataViewer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  Params.WindowClass.style := Params.WindowClass.style or CS_CLASSDC;
end;

procedure TDataViewer.DecPointOnScroll(var Pt: TPoint);
begin
  Dec( Pt.X, HScroll * fCharWidth );
  Dec( Pt.Y, VScroll * fCharHeight );
end;


destructor TDataViewer.Destroy;
var
  i: Integer;
begin
//  fMenu.Free;

//  fControl.OnResize := nil;
//  fControl.OnMouseDown := nil;
//  fControl.OnMouseUp := nil;
//  fControl.OnMouseWheel := nil;
//  fControl.OnMessage := nil;
//  fControl.OnMouseMove := nil;
//  fControl.OnKeyDown := nil;

//  fFont.Free;
  if (fSL.Count > 0) then
  begin
    FreeMem(Pointer(fSL.Objects[0]));
    for i := 0 to fSL.Count - 1 do
      fSL.Objects[i] := nil;
  end;
  fSL.Free;
//  fControl.Free;
  inherited;
end;


function TDataViewer.DrawFrame: Boolean;
var
  EmptyRect,
  DrawRect, barRectVert, barRectHorz, r: TRect;
  DC: HDC;
  vertBarWidth, vertBarBtnHeight, horzBarHeight, horzBarBtnWidth: Integer;
  ExStyle: Integer;
  Details: TThemedElementDetails;
  drawVertBar, drawHorzBar: Boolean;
begin
  Result := StyleServices.Enabled;
  if (Result) then
  begin
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      vertBarWidth := 0;
      vertBarBtnHeight := 0;
      horzBarHeight := 0;
      horzBarBtnWidth := 0;

      GetWindowRect(Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Handle);
      try
        EmptyRect := DrawRect;
//        AStyle := GetWindowLong(Handle, GWL_STYLE);
//        if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
//        begin
//      Может, сделать определение через стили?
        drawVertBar := IsBarVisible(OBJID_VSCROLL);
        drawHorzBar := IsBarVisible(OBJID_HSCROLL);

        if (drawVertBar) then
        begin
          vertBarWidth := GetSystemMetrics(SM_CXVSCROLL);
          vertBarBtnHeight := GetSystemMetrics(SM_CYVSCROLL);
        end;
        if (drawHorzBar) then
        begin
          horzBarHeight := GetSystemMetrics(SM_CYHSCROLL);
          horzBarBtnWidth := GetSystemMetrics(SM_CXHSCROLL);
        end;

        if (drawVertBar and drawHorzBar) then
        begin
          // draw right bottom rect between scrollbars
          InflateRect(EmptyRect, -2, -2);
          with EmptyRect do
            if UseRightToLeftScrollBar then
              EmptyRect := Rect(Left, Bottom - horzBarHeight, Left + vertBarWidth, Bottom)
            else
              EmptyRect := Rect(Right - vertBarWidth, Bottom - horzBarHeight, Right, Bottom);
          FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
          ExcludeClipRect(DC, EmptyRect.Left, EmptyRect.Top, EmptyRect.Right, EmptyRect.Bottom);
        end;

        if (drawVertBar) then
        begin
          barRectVert.Left := DrawRect.Right - 2 - vertBarWidth;
          barRectVert.Top := 2;
          barRectVert.Width := vertBarWidth;
          barRectVert.Bottom := DrawRect.Bottom - 2 - horzBarHeight;
        end;

        if (drawHorzBar) then
        begin
          barRectHorz.Left := 2;
          barRectHorz.Top := DrawRect.Bottom - 2 - horzBarHeight;
          barRectHorz.Right := DrawRect.Right - 2 - vertBarWidth;
          barRectHorz.Height := horzBarHeight;
        end;
        if (not drawVertBar) and drawHorzBar then
        begin
          Inc(barRectHorz.Right, vertBarWidth);
        end
        else if (not drawHorzBar) and drawVertBar then
        begin
          Inc(barRectVert.Bottom, horzBarHeight);
        end;
        // exclude bar trackbars and thumbnails from drawing - they will be redrawn by system
        if (drawVertBar) then
        begin
          ExcludeClipRect(
            DC, barRectVert.Left, barRectVert.Top + vertBarBtnHeight, barRectVert.Right,
            barRectVert.Bottom - vertBarBtnHeight);
        end;
        if (drawHorzBar) then
        begin
          ExcludeClipRect(
            DC, barRectHorz.Left + horzBarBtnWidth, barRectHorz.Top,
            barRectHorz.Right - horzBarBtnWidth, barRectHorz.Bottom);
        end;

        // remove client area from drawing
        r := DrawRect;
        r.Inflate(-2, -2);
        Dec(r.Bottom, horzBarHeight);
        Dec(r.Right, vertBarWidth);
        ExcludeClipRect(DC, r.Left, r.Top, r.Right, r.Bottom);
        // draw border
        Details := StyleServices.GetElementDetails(tlListviewRoot);
        StyleServices.DrawElement(DC, Details, DrawRect);
        // draw bar arrows
        if drawVertBar then
        begin
          r := barRectVert;
          r.Height := vertBarBtnHeight;
          Details := StyleServices.GetElementDetails(tsArrowBtnUpNormal);
          StyleServices.DrawElement(DC, Details, r);
          r := barRectVert;
          r.Top := r.Bottom - vertBarWidth;
          Details := StyleServices.GetElementDetails(tsArrowBtnDownNormal);
          StyleServices.DrawElement(DC, Details, r);
        end;
        if drawHorzBar then
        begin
          r := barRectHorz;
          r.Width := horzBarBtnWidth;
          Details := StyleServices.GetElementDetails(tsArrowBtnLeftNormal);
          StyleServices.DrawElement(DC, Details, r);
          r := barRectHorz;
          r.Left := r.Right - horzBarBtnWidth;
          Details := StyleServices.GetElementDetails(tsArrowBtnRightNormal);
          StyleServices.DrawElement(DC, Details, r);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  end;
end;

function TDataViewer.ElementFromPos(const Pos: Integer): Byte;
type
  TLineArray = array of TPoint;

  function GetLineIdx( const Lines: TLineArray; Point: Integer ): Integer;
  var
    I: Integer;
  begin
    for I := Low( Lines ) to High( Lines ) do
      if (Point > Lines[ I ].X) and (Point <= Lines[ I ].Y) then
      begin
        Result := I;
        Exit;
      end;
    Result := -1;
  end;

const
  Border = 6;
var
  ShowOffset, ShowHex, ShowChar: Boolean;
  Pt: Integer;
  S: ShortString;
  Width: Integer;
  L: TLineArray;
begin
  ShowOffset := hvOffset in VisibleElements;
  ShowHex := hvHexData in VisibleElements;
  ShowChar := hvCharData in VisibleElements;
  Pt := Pos;

  S := '';
  Dec( Pt, fBorderLeft );

  SetLength( L, 7 );
  FillChar( L[ 0 ], SizeOf( TPoint ) * 7, 0 );

  L[ 0 ].X := -999999;
  L[ 0 ].Y := 0;

  Width := 0;
  Dec( Width, fCharWidth * HScroll );

  if ShowOffset then
  begin
    L[ 1 ].X := Width;
    Inc( Width, Length( AddressTempl ) * fCharWidth );
    L[ 1 ].Y := Width;

    if ShowHex or ShowChar then
    begin
      L[ 2 ].X := Width;
      Inc( Width, Length( AddressDelTempl ) * fCharWidth );
      L[ 2 ].Y := Width - Border;
    end;
  end;
  if ShowHex then
  begin
    L[ 3 ].X := Width - Border;
    Inc( Width, Length( HexTempl ) * fCharWidth );
    L[ 3 ].Y := Width + Border;

    if {ShowHex or} ShowChar then
    begin
      L[ 4 ].X := Width + Border;
      Inc( Width, Length( HexDelTempl ) * fCharWidth );
      L[ 4 ].Y := Width - Border;
    end;
  end;
  if ShowChar then
  begin
    L[ 5 ].X := Width - Border;
    Inc( Width, Length( DataTempl ) * fCharWidth );
    L[ 5 ].Y := Width + Border;
  end;
  L[ 6 ].X := Width + Border;
  L[ 6 ].Y := 999999;

  Result := GetLineIdx( L, Pt );
end;


procedure TDataViewer.Freeze;
begin
  Inc( fFreezeCount );
end;


function TDataViewer.GetCharPoint( N: Integer; Element: TViewerElement ): TPoint;
begin
  Result := GetPosPoint( GetCharPos( N ), Element );
end;


function TDataViewer.GetCharPos(N: Integer; BlockRecursive: Boolean): TPos;
var
  col, row, i: Integer;
  start: Pointer;
begin
  case Mode of
    hmHex:
    begin
      Result.Col := N mod 16;
      Result.Row := N div 16;

      if Result.Row >= fLineCount then
      begin
        Result.Col := FCurLineWidth;
        Result.Row := fLineCount - 1;
      end;
    end;

    hmTextBinary:
    begin
      Result.Col := N mod FCurLineWidth;
      Result.Row := N div FCurLineWidth;
      if Result.Row >= fLineCount then
      begin
        Result.Col := FCurLineWidth;
        Result.Row := fLineCount - 1;
      end;
    end;

    hmTextFree:
    begin
//      Offset := fSL.Objects[ 0 ];
      if (fSL.Count > 0) then
        start := fSL.Objects[0]
      else
        start := nil;
      Col := 0;
      Row := 0;
      for I := fSL.Count - 1 downto 0 do
      begin
        if N >= (SysInt(fSL.Objects[I]) - SysInt(start)) then
        begin
          Col := N - (SysInt(fSL.Objects[I]) - SysInt(start));
          Row := I;
          if Col > Length(fSL[I]) then
          begin
            Col := 0;
            Inc( Row );
            Inc( fSelEnd );
          end;
          Break;
        end;
      end;
      Result.Col := Col;
      Result.Row := Row;
    end;
  end;
end;


function TDataViewer.GetEmpty: Boolean;
begin
  Result := not ((Stream <> nil) or (fSL.Count > 0));
end;


function TDataViewer.GetFontSize: Integer;
begin
  Result := Font.Size;
end;

function TDataViewer.GetFreezed: Boolean;
begin
  Result := fFreezeCount > 0;
end;


function TDataViewer.GetFullCols: Integer;
var
  R: TRect;
begin
  R := ClientRect;
  Inc( R.Left, fBorderLeft );
  Dec( R.Right, fBorderRight );
  Result := ColsPerWidth( R, fCharWidth );
end;


function TDataViewer.GetFullLines: Integer;
begin
  Result := LinesPerHeight(ClientRect, fCharHeight);
end;


function TDataViewer.GetHScroll: Integer;
begin
  Result := GetScrollPos(Handle, SB_HORZ);
end;


function TDataViewer.GetPointPos(const Point: TPoint; Element: TViewerElement): TPos;
var
  {Line, Col,} CL: Integer;
  R: TRect;
  WH: Byte;
  ElemLeft, VP, HP, {I,} L: Integer;
  S: ShortString;
  ShowOffset, ShowHex, ShowChar: Boolean;
  Pt: TPoint;
  St: string;
//  Offset: Cardinal;
begin
  R := ClientRect;
  CL := LinesPerHeight( R, fCharHeight );
  WH := fCharWidth div 2;
  Pt := Point;
  VP := GetScrollPos(Handle, SB_VERT);
  HP := GetScrollPos(Handle, SB_HORZ);
  Int64(Result) := 0;

  case Mode of
    hmHex:
    begin
      ShowOffset := hvOffset in VisibleElements;
      ShowHex := hvHexData in VisibleElements;
      ShowChar := hvCharData in VisibleElements;

      case Element of
        hvCharData:
        begin
          S := '';
          if ShowOffset then
            if ShowHex or ShowChar then
              S := S + AddressTempl + AddressDelTempl
            else
              S := S + AddressTempl;
          if ShowHex then
            if ShowChar then
              S := S + HexTempl + HexDelTempl
            else
              S := S + HexTempl;
          ElemLeft := Length( S ) * fCharWidth;
          Dec( Pt.X, ElemLeft + fBorderLeft - HP * fCharWidth );
          if Pt.Y < 0 then
            Result.Row := 0
          else
          begin
            Result.Row := Pt.Y div fCharHeight;
            if Result.Row > CL then
              Result.Row := CL;
          end;
          if Pt.X < 0 then
            Result.Col := 0
          else
          begin
            Result.Col := (Pt.X + WH) div fCharWidth;
            if Result.Col > 16 then
              Result.Col := 16;
          end;
          //Inc( Col, HP );
          Inc( Result.Row, VP );
{          Result := Line * 16 + Col;

          if Cardinal(Result) > StreamDataSize then
            Result := StreamDataSize;}
        end;

        hvHexData:
        begin
          S := '';
          if ShowOffset then
            if ShowHex or ShowChar then
              S := S + AddressTempl + AddressDelTempl
            else
              S := S + AddressTempl;
          ElemLeft := Length( S ) * fCharWidth;
          Dec( Pt.X, ElemLeft + fBorderLeft - HP * fCharWidth );
          if Pt.Y < 0 then
            Result.Row := 0
          else
          begin
            Result.Row := Pt.Y div fCharHeight;
            if Result.Row > CL then
              Result.Row := CL;
          end;
          if Pt.X < 0 then
            Result.Col := 0
          else
          begin
            Result.Col := ((Pt.X + WH) div fCharWidth + 2) div 3;
            if Result.Col > 16 then
              Result.Col := 16;
          end;
          //Inc( Col, HP div 3 );
          Inc( Result.Row, VP );
{          Result := Line * 16 + Col;

          if Cardinal(Result) > StreamDataSize then
            Result := StreamDataSize;}
        end;
      end;
    end;

    hmTextBinary:
    begin
      Dec( Pt.X, fBorderLeft );
      if Pt.Y < 0 then
        Result.Row := 0
      else
      begin
        Result.Row := Pt.Y div fCharHeight;
        if Result.Row > CL then
          Result.Row := CL;
      end;

      if Pt.X < 0 then
        Result.Col := 0
      else
      begin
        Result.Col := (Pt.X + WH) div fCharWidth;
        if Result.Col > CurLineWidth then
          Result.Col := CurLineWidth;
      end;
      Inc( Result.Row, VP );
      Inc( Result.Col, HP );
{      Result := Line * Integer(LineWidth) + Col;

      if Cardinal(Result) > StreamDataSize then
        Result := StreamDataSize;}
    end;

    hmTextFree:
    begin
      Dec( Pt.X, fBorderLeft );
      if Pt.Y < 0 then
        Result.Row := 0
      else
      begin
        Result.Row := Pt.Y div fCharHeight;
        if Result.Row > CL then
          Result.Row := CL;
      end;
      Inc( Result.Row, VP );

      if Result.Row >= fLineCount then
        Result.Row := fLineCount - 1;
      St := fSL[ Result.Row ];
      L := Length( St );

      if Pt.X < 0 then
        Result.Col := 0
      else
      begin
        Result.Col := (Pt.X + WH) div fCharWidth;
        if Result.Col > L then
          Result.Col := L;
      end;
      Inc( Result.Col, HP );

{      Result := 0;
      Inc( Result, fSL.Objects[ Result.Row ] );
      Inc( Result, Result.Col );

      if Cardinal(Result) > StreamDataSize then
        Result := StreamDataSize;}
    end;
  end;
end;


function TDataViewer.GetPosChar(Pos: TPos): Integer;
var
  L: Integer;
begin
  case Mode of
    hmHex:
    begin
      if Pos.Col > 16 then
        Pos.Col := 16;
      if Pos.Row >= fLineCount then
        Pos.Row := fLineCount - 1;
      if Pos.Row < 0 then
        Pos.Row := 0;
      Result := Pos.Row * 16 + Pos.Col;
    end;

    hmTextBinary:
    begin
      if Pos.Col > CurLineWidth then
      begin
        Pos.Col := CurLineWidth;
        if (Mode = hmTextBinary) then
          Dec( Pos.Col );
      end;
      if Pos.Row >= fLineCount then
        Pos.Row := fLineCount - 1;
      if Pos.Row < 0 then
        Pos.Row := 0;
      Result := Pos.Row * CurLineWidth + Pos.Col;
    end;

    hmTextFree:
    begin
      if Pos.Row >= fLineCount then
        Pos.Row := fLineCount - 1;
      if Pos.Row < 0 then
        Pos.Row := 0;
      L := Length(fSL[ Pos.Row ]);
      if Pos.Col > L then
        Pos.Col := L;
      Result := Integer(fSL.Objects[Pos.Row]) - Integer(fSL.Objects[0]) + Pos.Col;
    end;

    else
      Result := 0;
  end;
end;


function TDataViewer.GetPosPoint(Pos: TPos; Element: TViewerElement): TPoint;
var
  ShowOffset, ShowHex, ShowChar: Boolean;
  ElemLeft, L: Integer;
  S: string;
begin
  case Mode of
    hmHex:
    begin
      ShowOffset := hvOffset in VisibleElements;
      ShowHex := hvHexData in VisibleElements;
      ShowChar := hvCharData in VisibleElements;
      case Element of
        hvCharData:
        begin
          S := '';
          if ShowOffset then
            if ShowHex or ShowChar then
              S := S + AddressTempl + AddressDelTempl
            else
              S := S + AddressTempl;
          if ShowHex then
            if ShowChar then
              S := S + HexTempl + HexDelTempl
            else
              S := S + HexTempl;
          ElemLeft := Length( S ) * fCharWidth;

          if Pos.Col > 16 then
            Pos.Col := 16;

          Result.X := fBorderLeft + ElemLeft + Pos.Col * fCharWidth;
          Result.Y := Pos.Row * fCharHeight;
        end;

        hvHexData:
        begin
          S := '';
          if ShowOffset then
            if ShowHex or ShowChar then
              S := S + AddressTempl + AddressDelTempl
            else
              S := S + AddressTempl;

          ElemLeft := Length( S ) * fCharWidth;

          if Pos.Col > 16 then
            Pos.Col := 16;

          if Pos.Col = 0 then
            Result.X := fBorderLeft + ElemLeft + Pos.Col * fCharWidth * 3
          else
            Result.X := fBorderLeft + ElemLeft + Pos.Col * fCharWidth * 3 - fCharWidth;
          Result.Y := Pos.Row * fCharHeight;
        end;
      end;
    end;

    hmTextBinary:
    begin
      if (Pos.Col > CurLineWidth) then
        Pos.Col := CurLineWidth;

      Result.X := fBorderLeft + Pos.Col * fCharWidth;
      Result.Y := Pos.Row * fCharHeight;
    end;

    hmTextFree:
    begin
      L := Length(fSL[Pos.Row]);
      if Pos.Col > L then
        Pos.Col := L;

      Result.X := fBorderLeft + Pos.Col * fCharWidth;
      Result.Y := Pos.Row * fCharHeight;
    end;
  end;
end;


function TDataViewer.GetSelEndChanged: Boolean;
begin
  Result := PrevSelEnd <> SelEnd;
end;


{function TDataViewer.GetSelEndPos(NewLineIfEOL: Boolean): TPos;
begin
  Result := fSelEndPos;
end;}


{function TDataViewer.GetTmProp(Idx: Boolean): Integer;
begin
  Result := Ord(Idx);
end;}


function TDataViewer.GetVisibleLines: Integer;
begin
  Result := LinesPerHeight2(ClientRect, fCharHeight);
end;


function TDataViewer.GetVScroll: Integer;
begin
  Result := GetScrollPos(Handle, SB_VERT);
end;


procedure TDataViewer.HideCaret;
begin
  Winapi.Windows.HideCaret(Handle);
  DestroyCaret;
  fCaretShown := False;
end;


procedure TDataViewer.Invalidate;
begin
  if not Freezed then
    inherited Invalidate;
end;


function TDataViewer.IsBarVisible(ABar: NativeUInt): Boolean;
var
  sbi: TScrollBarInfo;
begin
  FillChar(sbi, SizeOf(sbi), 0);
  sbi.cbSize := SizeOf(sbi);
  GetScrollBarInfo(Handle, ABar, sbi);
  Result := (sbi.rgstate[0] and STATE_SYSTEM_INVISIBLE) <> STATE_SYSTEM_INVISIBLE;
end;

procedure TDataViewer.MakePosVisible(const Pos: TPos);
var
  VS, HS, HS2, FL, FC: Integer;
  ShowOffset, ShowHex, ShowChar: Boolean;
  S: string;
begin
  VS := VScroll;
  HS := HScroll;
  FL := FullLines - 1;
  FC := FullCols;

  if Pos.Row < VS then
    VScroll := Pos.Row
  else
  if Pos.Row > VS + FL then
    VScroll := Pos.Row - FL;


  case Mode of
    hmHex:
    begin
      ShowOffset := hvOffset in VisibleElements;
      ShowHex := hvHexData in VisibleElements;
      ShowChar := hvCharData in VisibleElements;
      S := '';
      if ShowOffset then
        if ShowHex or ShowChar then
          S := S + AddressTempl + AddressDelTempl
        else
          S := S + AddressTempl;
      if ShowHex and (SelectionElement = hvCharData) then
        if ShowChar then
          S := S + HexTempl + HexDelTempl
        else
          S := S + HexTempl;
      HS2 := Length( S );
      if SelectionElement = hvHexData then
      begin
        if (Pos.Col * 3) < (HS - HS2) then
          HScroll := Pos.Col * 3 + HS2 - 1
        else
          if (Pos.Col * 3 + HS2) > (HS + FC - 2) then
            HScroll := Pos.Col * 3 + HS2 - FC + 3;
      end else
      begin
        if (Pos.Col) < (HS - HS2) then
          HScroll := Pos.Col + HS2 - 1
        else
          if (Pos.Col + HS2) > (HS + FC - 2) then
            HScroll := Pos.Col + HS2 - FC + 1;
      end;
    end;

    hmTextBinary:
    begin
      if Pos.Col < HS then
        HScroll := Pos.Col
      else
        if Pos.Col > HS + FC - 1 then
          HScroll := Pos.Col - FC + 1;
    end;

    hmTextFree:
    begin
      if Pos.Col < HS then
        HScroll := Pos.Col
      else
        if Pos.Col > HS + FC then
          HScroll := Pos.Col - FC;
    end;
  end;
end;


function TDataViewer.NewNormalCanvas: TCanvas;
var
  tmDC: HDC;
begin
  tmDC := CreateCompatibleDC( 0 );
  Result := MyNewCanvas( tmDC );
  SetNormalCanvas( Result );
end;


procedure TDataViewer.Paint( DC: HDC; const UpdateRect: TRect );

  function IsRectIntersect( const R1, R2: TRect ): Boolean;
  var
    RR: TRect;
  begin
    IntersectRect(RR, R1, R2);
    Result := not IsRectEmpty(RR);
  end;

var
  c: TCanvas;
  vp, hp: Integer;
  r{, UR}: TRect;
  tmBmp: TBitmap;

  procedure DrawHexMode;
  var
    s, tm, tm1, tm2: string;
    tr: TRect;
    lc, i, l: Integer;
    curPos, curPos16: Integer;
    j: Integer;
    buf: Byte;
    showOffset, showHex, showChar: Boolean;
    selectedRow: Boolean;
    selSt, selFn, selTextLength: Integer;

    procedure DrawHexData;
    begin
      S := '';
      if ShowOffset then
        if ShowHex or ShowChar then
          S := S + AddressTempl + AddressDelTempl
        else
          S := S + AddressTempl;
      if ShowHex then
      begin
        L := Length( S );
        Inc( L, SelSt * 3 );
        TR.Left := fBorderLeft + L * fCharWidth;
        Dec( TR.Left, fCharWidth * HP );
//        if SelFn < 16 then
//          J := 1
//        else
//          J := 0;
        c.TextFlags := DT_NOPREFIX;
        tr.Width := (selTextLength * 3 - 1) * FCharWidth;
        c.TextRect(tr, tr.Left, tr.Top, Copy(tm1, selSt * 3 + 1, selTextLength * 3 - 1));
//        C.DrawText( Copy( tm1, (SelSt) * 3 + 1, (SelFn - SelSt) * 3 - Integer(J) ), TR, DT_NOPREFIX );
      end;
    end;

    procedure DrawCharData;
    begin
      S := '';
      if ShowOffset then
        if ShowHex or ShowChar then
          S := S + AddressTempl + AddressDelTempl
        else
          S := S + AddressTempl;
      if ShowHex then
        if ShowChar then
          S := S + HexTempl + HexDelTempl
        else
          S := S + HexTempl;
      if ShowChar then
      begin
        L := Length(S);
        Inc(L, SelSt);
        tr.Left := fBorderLeft + L * FCharWidth;
        Dec(tr.Left, FCharWidth * HP );
        tr.Width := selTextLength * FCharWidth;
        c.TextFlags := DT_NOPREFIX;
        c.TextRect(tr, tr.Left, tr.Top, Copy(tm2, SelSt + 1, SelFn - SelSt));
//        C.DrawText( Copy( tm2, SelSt + 1, SelFn - SelSt ), TR, DT_NOPREFIX );
      end;
    end;

  begin
    ShowOffset := hvOffset in VisibleElements;
    ShowHex := hvHexData in VisibleElements;
    ShowChar := hvCharData in VisibleElements;

    LC := LinesPerHeight( R, fCharHeight );
    for I := 0 to LC do
    begin
      SetNormalCanvas(C);
      CurPos16 := (VP + I) * 16;
      if ShowOffset then
        tm := IntToHex( CurPos16, 8 );
      tm1 := '';
      tm2 := '';
      TR.Left := fBorderLeft;
      if HP > 0 then
        Dec( TR.Left, HP * fCharWidth );
      TR.Top := I * fCharHeight;
      TR.Right := R.Right;
      TR.Bottom := TR.Top + fCharHeight;
      if (CurPos16 < fBufferSize) and IsRectIntersect( UpdateRect, TR ) then
      begin
        SelectedRow := False;
        SelFn := 0;
        SelSt := 0;
        for J := 0 to 15 do
        begin
          CurPos := CurPos16 + J;
          if not SelectedRow and (SelStart <> SelEnd) and
            ((Integer(CurPos) >= SelStart) and (Integer(CurPos) < SelEnd) or
             (Integer(CurPos) >= SelEnd) and (Integer(CurPos) < SelStart)) then
          begin
            SelectedRow := True;
            if SelStart < SelEnd then
            begin
              SelSt := SelStart;
              SelFn := SelEnd;
            end else
            begin
              SelSt := SelEnd;
              SelFn := SelStart;
            end;

            if SelSt <= Integer(CurPos) then
              SelSt := J;
            if SelFn >= Integer(CurPos16) + 16 then
              SelFn := 16
            else
              SelFn := SelFn - CurPos16;
          end;
          if CurPos < fBufferSize then
          begin
            if J = 8 then
              tm1 := tm1 + '|'
            else
              if J > 0 then
                tm1 := tm1 + ' ';
            Stream.Seek(CurPos + StreamDataOffset, soFromBeginning);
            Stream.Read(Buf, 1);
            tm1 := tm1 + IntToHex( Buf, 2 );
            tm2 := tm2 + Char(ConvertNonPrintingChar(AnsiChar(Buf), NonPrintReplacement));
          end else
          begin
            if J = 8 then
              tm1 := tm1 + '|'
            else
              if J > 0 then
                tm1 := tm1 + ' ';
            tm1 := tm1 + '  ';
            tm2 := tm2 + ' ';
          end;
        end;
        S := '';
        if ShowOffset then
          if ShowHex or ShowChar then
            S := tm + AddressDelTempl
          else
            S := tm;

        if ShowHex then
          if ShowChar then
            S := S + tm1 + HexDelTempl
          else
            S := S + tm1;
        if ShowChar then
          S := S + tm2;
        c.TextFlags := DT_NOPREFIX;
        c.TextRect(tr, tr.Left, tr.Top, string(s));
//        C.DrawText( S, TR, DT_NOPREFIX );

        if SelectedRow then
        begin
          SetSelectedCanvas(c);
          selTextLength := selFn - selSt;
          case SelectionElement of
            hvCharData:
            begin
              {S := '';
              if ShowOffset then
                if ShowHex or ShowChar then
                  S := S + AddressTempl + AddressDelTempl
                else
                  S := S + AddressTempl;
              if ShowHex then
                if ShowChar then
                  S := S + HexTempl + HexDelTempl
                else
                  S := S + HexTempl;
              if ShowChar then
              begin
                L := Length( S );
                Inc( L, SelSt );
                TR.Left := fBorderLeft + L * fCharWidth;
                Dec( TR.Left, fCharWidth * HP );
                C.DrawText( Copy( tm2, SelSt + 1, SelFn - SelSt ), TR, DT_NOPREFIX );
              end;}
              DrawCharData;
              SetSelInactiveCanvas(C);
              DrawHexData;
            end;

            hvHexData:
            begin
              {S := '';
              if ShowOffset then
                if ShowHex or ShowChar then
                  S := S + AddressTempl + AddressDelTempl
                else
                  S := S + AddressTempl;
              if ShowHex then
              begin
                L := Length( S );
                Inc( L, SelSt * 3 );
                TR.Left := fBorderLeft + L * fCharWidth;
                Dec( TR.Left, fCharWidth * HP );
                if SelFn < 16 then
                  J := 1
                else
                  J := 0;
                C.DrawText( Copy( tm1, (SelSt) * 3 + 1, (SelFn - SelSt) * 3 - Integer(J) ), TR, DT_NOPREFIX );
              end;}
              DrawHexData;
              SetSelInactiveCanvas(C);
              DrawCharData;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure DrawBinaryTextMode;
  var
    rc, i, lc, curPosW, LineR: Integer;
    s: AnsiString;
    tr: TRect;
    buf: PAnsiChar;
    selSt, selFn, selTextLength: Integer;
  begin
    LC := LinesPerHeight( R, fCharHeight );
    GetMem(buf, CurLineWidth + 1);
    for I := 0 to LC do
    begin
      tr.Right := r.Right;
      SetNormalCanvas( C );
      CurPosW := (VP + I) * CurLineWidth;
      if HP > 0 then
        Dec( TR.Left, HP * fCharWidth );
      TR.Left := fBorderLeft - HP * fCharWidth;
      TR.Top := I * fCharHeight;
      TR.Bottom := TR.Top + fCharHeight;
      LineR := CurPosW + CurLineWidth;
      if (LineR <= fBufferSize) then
        RC := CurLineWidth
      else
        RC := {LineWidth -} (fBufferSize mod CurLineWidth);
      if (CurPosW < fBufferSize) and IsRectIntersect( UpdateRect, TR ) then
      begin
        Stream.Seek(CurPosW + Integer(StreamDataOffset), soFromBeginning);
        Stream.Read( Buf^, RC );
        Buf[ RC ] := #0;
        ConvertNonPrintingChars( Buf, RC, NonPrintReplacement, True );

        c.TextFlags := DT_NOPREFIX;
        c.TextRect(tr, tr.Left, tr.Top, string(buf));
//        C.DrawText( Buf, TR, DT_NOPREFIX );
      end;

      if SelStart <> SelEnd then
      begin
        if SelStart < SelEnd then
        begin
          SelSt := SelStart;
          SelFn := SelEnd;
        end else
        begin
          SelSt := SelEnd;
          SelFn := SelStart;
        end;

        if ((SelSt >= CurPosW) and (SelSt < LineR)) or
          ((SelFn >= CurPosW) and (SelFn < LineR)) or
          ((SelSt < CurPosW) and (SelFn >= LineR)) then
        begin
          if SelSt <= CurPosW then
            SelSt := 0
          else
            SelSt := SelSt - CurPosW;
          if SelFn >= LineR then
            SelFn := CurLineWidth
          else
            SelFn := SelFn - CurPosW;

          S := Buf;
          selTextLength := SelFn - SelSt;
          tr.Left := fBorderLeft + (SelSt - HP) * fCharWidth;
          tr.Right := tr.Left + selTextLength * FCharWidth;
          SetSelectedCanvas( C );
          c.TextFlags := DT_NOPREFIX;
          c.TextRect(tr, tr.Left, tr.Top, Copy(string(S), SelSt + 1, selTextLength));
//          C.DrawText( Copy( S, SelSt + 1, SelFn - SelSt ), TR, DT_NOPREFIX );
//          tmBmp.SaveToFile('2.bmp');
        end;
      end;
    end;
    FreeMem( Buf );
  end;

  procedure DrawFreeTextMode;
  var
    i, lc, ln, lineR, lineL, l: Integer;
    s: string;
    tr: TRect;
    selSt, selFn, selTextLength: Integer;
    capLineBreak: Boolean;
    bufStart: SysUInt;
  begin
    LC := LinesPerHeight( R, fCharHeight );
    for I := 0 to LC do
    begin
      tr.Right := r.Right;
      LN := I + VP;
      if LN < fLineCount then
      begin
        S := fSL[LN];
        TR.Left := fBorderLeft - HP * fCharWidth;
        TR.Top := I * fCharHeight;
        TR.Bottom := TR.Top + fCharHeight;
        SetNormalCanvas( C );

        c.TextFlags := DT_NOPREFIX;
        c.TextRect(tr, tr.Left, tr.Top, s);

        if SelStart <> SelEnd then
        begin
          if SelStart < SelEnd then
          begin
            SelSt := SelStart;
            SelFn := SelEnd;
          end else
          begin
            SelSt := SelEnd;
            SelFn := SelStart;
          end;

          L := Length( S );
//          Offset := fSL.Objects[ 0 ];
          bufStart := SysUInt(fSL.Objects[0]);
          LineL := SysUInt(fSL.Objects[LN]) - bufStart;
          LineR := LineL + L;

          if ((SelSt >= LineL) and (SelSt <= LineR)) or
            ((SelFn >= LineL) and (SelFn <= LineR)) or
            ((SelSt < LineL) and (SelFn >= LineR)) then
          begin
            if SelSt <= LineL then
              SelSt := 0
            else
              SelSt := SelSt - LineL;
            CapLineBreak := (SelFn > LineR);
            if CapLineBreak then
              SelFn := L
            else
              SelFn := SelFn - LineL;

            TR.Left := fBorderLeft + (SelSt - HP) * fCharWidth;
            SetSelectedCanvas(c);
            selTextLength := selFn - selSt;
            s := Copy(s, SelSt + 1, selTextLength);
            tr.Right := tr.Left + FCharWidth * selTextLength;
            if CapLineBreak then
            begin
              S := S + ' ';
              Inc(tr.Right, FCharWidth);
            end;
            c.TextFlags := DT_NOPREFIX;
            c.TextRect(tr, tr.Left, tr.Top, s);
//            C.DrawText( S, TR, DT_NOPREFIX );
          end;
        end;
      end;
    end;
  end;

begin
  R := ClientRect;
  if (R.Right - R.Left <= 0) or (R.Bottom - R.Top <= 0) then
    Exit;
  if (UpdateRect.Right - UpdateRect.Left <= 0) or (UpdateRect.Bottom - UpdateRect.Top <= 0) then
    Exit;
//  BeginPaint( Ctl.Handle, PS );
//  UR := PS.rcPaint;
//  GetUpdateRect( Ctl.Handle, UR, False );
  tmBmp := TBitmap.Create;
  tmBmp.Width := R.Right - R.Left;
  tmBmp.Height := R.Bottom - R.Top;
  c := tmBmp.Canvas;
  SetNormalCanvas(C);
  C.FillRect(MakeRect( 0, 0, tmBmp.Width, tmBmp.Height));

  if not Empty then
  begin
    VP := GetScrollPos(Handle, SB_VERT);
    HP := GetScrollPos(Handle, SB_HORZ);
    case Mode of
      hmHex: DrawHexMode;
      hmTextBinary: DrawBinaryTextMode;
      hmTextFree: DrawFreeTextMode;
    end;
  end;

  BitBlt( DC, UpdateRect.Left, UpdateRect.Top, UpdateRect.Right - UpdateRect.Left,
    UpdateRect.Bottom - UpdateRect.Top, C.Handle, UpdateRect.Left, UpdateRect.Top, SRCCOPY );
  tmBmp.Free;
//  EndPaint( Ctl.Handle, PS );
//  ValidateRect( Ctl.Handle, @UR );
end;


{procedure TDataViewer.PopupMenuSelAll( Sender: PMenu; Item: Integer );
begin
  SelectAll;
end;}


procedure TDataViewer.ProcessHotKeys(HK: THotKey);
begin
{  case HK.AsDWORD of
    HK_CA: SelectAll;
  end;}
end;


procedure TDataViewer.ScrollBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function CalcWordEnd(Back: Boolean): TPos;
  var
    S: string;
    CPos: TPos;
    I, J: Integer;
    C: Char;
    Found: Boolean;
  begin
    CPos := SelEndPos;
    S := fSL[CPos.Row];

    if Back then begin
      if (CPos.Col = 0) then begin
        if (CPos.Row <= 0) then begin
          Result := CPos;
          Exit;
        end;
        Dec(CPos.Row);
        S := fSL[CPos.Row];
        CPos.Col := Length(S);
      end;
      for I := CPos.Col - 1 downto 0 do begin
        C := S[I];
        Found := False;
        for J := 1 to Length(Def_Separators) do
          if (C = Def_Separators[J]) then
            Found := True;
        if Found then begin
          Result.Col := I;
          Result.Row := CPos.Row;
          Exit;
        end;
      end;
      Result.Col := 0;
    end
    else begin
      if (Length(S) = CPos.Col) then begin
        if (CPos.Row >= fSL.Count - 1) then begin
          Result := CPos;
          Exit;
        end;
        Inc(CPos.Row);
        CPos.Col := 0;
        S := fSL[CPos.Row];
      end;
      for I := CPos.Col + 1 to Length(S) do begin
        C := S[I];
        Found := False;
        for J := 1 to Length(Def_Separators) do
          if (C = Def_Separators[J]) then
            Found := True;
        if Found then begin
          Result.Col := I;
          Result.Row := CPos.Row;
          Exit;
        end;
      end;
      Result.Col := Length(S);
    end;
    Result.Row := CPos.Row;
  end;

var
  R: TRect;
  Pos: TPos;
begin
  R := ClientRect;
  Freeze;
  case Key of
    VK_RIGHT:
    begin
      Pos := SelEndPos;
      if (ssCtrl in Shift) and (Mode = hmTextFree) then
        Pos := CalcWordEnd(False)
      else
        Inc( Pos.Col );
      SetSelEndPos( Pos, True );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_LEFT:
    begin
      Pos := SelEndPos;
      if (ssCtrl in Shift) and (Mode = hmTextFree) then
        Pos := CalcWordEnd(True)
      else
        Dec( Pos.Col );
      SetSelEndPos( Pos, True );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_UP:
    begin
      Pos := SelEndPos;
      Dec( Pos.Row );
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_DOWN:
    begin
      Pos := SelEndPos;
      Inc( Pos.Row );
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_HOME:
    begin
      Pos := SelEndPos;
      Pos.Col := 0;
      if ssCtrl in Shift then
        Pos.Row := 0;
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_END:
    begin
      Pos := SelEndPos;
      Pos.Col := MaxInt;
      if ssCtrl in Shift then
        Pos.Row := LineCount - 1;
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_NEXT://VK_PAGE_DOWN:
    begin
      Pos := SelEndPos;
      Inc(Pos.Row, Max(FullLines, 6));
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    VK_PRIOR://VK_PAGE_UP:
    begin
      Pos := SelEndPos;
      Dec( Pos.Row, Max( FullLines, 6 ) );
      SetSelEndPos( Pos );
      if not (ssShift in Shift) then
        SelStart := SelEnd;
    end;

    else
    begin
      Dec( fFreezeCount );
      ProcessHotKeys(MakeHotKey(Key, Shift));
      Exit;
    end;
  end;
  UnFreeze;
  Key := 0;
end;


{procedure TDataViewer.ScrollBoxOnKeyUp(Sender: PControl; var Key: Integer;
  Shift: Cardinal);
begin
  Key := 0;
end;}


(*function TDataViewer.ScrollBoxOnMessage(var Msg: tagMSG;
  var Rslt: Integer): Boolean;
var
  PS: TPaintStruct;
//  Stm: PStream;
begin
  Result := False;
  case Msg.message of
    WM_PAINT:
    begin
      if not Freezed then
      begin
        BeginPaint( Ctl.Handle, PS );
        if Msg.wParam = 0 then
          Paint( PS.hdc, PS.rcPaint )
        else
          Paint( Msg.wParam, PS.rcPaint );
        EndPaint( Ctl.Handle, PS );
      end else
        ValidateRect( Ctl.Handle, nil );
      Result := True;
    end;

    WM_SETFOCUS:
      ShowCaret;

    WM_KILLFOCUS:
      HideCaret;

{    WM_ERASEBKGND:
    begin
      Rslt := 1;
      if Stream = nil then
      begin
        Stm := Stream;
        fStream := nil;
        BeginPaint( Ctl.Handle, PS );
        Paint( PS.hdc, PS.rcPaint );
        EndPaint( Ctl.Handle, PS );
        fStream := Stm;
      end;
    end;}

//    WM_KEYFIRST..WM_KEYLAST:
//      Result := True;
  end;
end;*)


procedure TDataViewer.ScrollBoxOnMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  E: Byte;
  EE: TViewerElement;
begin
  SetFocus;
//  Windows.SetFocus(Handle);
  if Button = mbLeft then
    fPrevCaptureWnd := SetCapture(Handle);
  SetHCursor(X);

  if (Button = mbLeft) and not Empty then
  begin
    case Mode of
      hmHex:
      begin
        E := ElementFromPos(X);
        case E of
    //      0, 1: EE := hvOffset;
          {2,} 3: EE := hvHexData;
          5{, 6}: EE := hvCharData;
          else
          begin
            Invalidate;
            fSelecting := False;
            Exit;
          end;
        end;
        Freeze;
        fSelectionElement := EE;
        SetSelEndPos(GetPointPos(MakePoint(X, Y), EE));
        SelStart := SelEnd;
        UnFreeze;
        fSelecting := True;
      end;

      hmTextBinary:
      begin
        Freeze;
        fSelectionElement := hvCharData;
        SetSelEndPos(GetPointPos(MakePoint(X, Y), hvCharData));
        SelStart := SelEnd;
        UnFreeze;
        fSelecting := True;
      end;

      hmTextFree:
      begin
        Freeze;
        fSelectionElement := hvCharData;
        SetSelEndPos(GetPointPos(MakePoint(X, Y), hvCharData));
        SelStart := SelEnd;
        UnFreeze;
        fSelecting := True;
      end;
    end;
    Integer(fScrollDelta) := 0;
    fScrollTimer.Enabled := True;
  end;
end;


procedure TDataViewer.ScrollBoxOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  D: TSmallPoint;
  R: TRect;
  DD, MaxStep: Byte;
begin
  UpdateMousePos(MakePoint(X, Y));

  if Selecting and not Empty then
  begin
    R := ClientRect;
    if X < 0 then
      D.x := X
    else
      if X > R.Right then
        D.x := X - R.Right
      else
        D.x := 0;
    if Y < 0 then
      D.y := Y
    else
      if Y > R.Bottom then
        D.y := Y - R.Bottom
      else
        D.y := 0;
    DD := 9;
    MaxStep := 20;

    fScrollDelta := D;
    if D.y <> 0 then
    begin
      if D.y < 0 then
        D.y := D.y div DD - 1
      else
        D.y := D.y div DD + 1;

      if D.y > MaxStep then
        D.y := MaxStep
      else
        if D.y < -MaxStep then
          D.y := -MaxStep;
      fScrollDelta.y := -D.y;
    end;
    if D.x <> 0 then
    begin
      if D.x < 0 then
        D.x := D.x div DD - 1
      else
        D.x := D.x div DD + 1;
      if D.x > MaxStep then
        D.x := MaxStep
      else
        if D.x < -MaxStep then
          D.x := -MaxStep;
      fScrollDelta.x := -D.x;
    end;
//    Ctl.Parent.Caption := Int2Str( D.X ) + ' ' + Int2Str( D.Y ) + '    ' + Int2Str( Mouse.X ) + ' ' + Int2Str( Mouse.Y );
  end;
end;


procedure TDataViewer.ScrollBoxOnMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetHCursor(X);
  ReleaseCapture;
  if (Button = mbLeft) and not Empty then
  begin
    fScrollTimer.Enabled := False;
    fSelecting := False;
  end;
//  Ctl.Parent.Caption := Int2Str( SelStart ) + '  ' + Int2Str( SelEnd );
end;


procedure TDataViewer.ScrollBoxOnMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  d: Integer;
  delta: Integer;
  vk: Word;
begin
  Delta := 3;
  d := WheelDelta div WHEEL_DELTA * delta;

  if (Shift = [ssCtrl]) or (Shift = [ssCtrl, ssShift]) then
  begin
    if d > 0 then
      VK := VK_UP
    else
      VK := VK_DOWN;
    ScrollBoxOnKeyDown(nil, VK, Shift);
  end
  else if (Shift = [ssShift]) then
  begin
    HScroll := HScroll - D;
  end
  else
  begin
    VScroll := VScroll - D;
  end;


  UpdateMousePos( ClientCursorPos );
  UpdateCaretPos;
  Handled := True;
end;


procedure TDataViewer.ScrollBoxOnResize(Sender: TObject);

  procedure SetMax(Bar, nMax: Integer );
  var
    si: tagSCROLLINFO;
    newMax: Integer;
  begin
    FillChar( SI, SizeOf( SI ), 0 );
    SI.cbSize := SizeOf( SI );
    SI.fMask := SIF_RANGE or SIF_PAGE;
    newMax := nMax + fPageSize - 1;
    GetScrollInfo(Handle, Bar, si);
    SI.nMax := newMax;
    SI.nPage := fPageSize;
    SetScrollInfo(Handle, Bar, SI, True);
  end;

  procedure SetHorizontalSB(Rect: TRect);
  begin
    if (Rect.Width < (fDataWidth + fBorderLeft + fBorderRight)) then
    begin
      Inc( Rect.Left, fBorderLeft + fBorderRight );
      SetMax(SB_HORZ, (CurLineWidth - ColsPerWidth(Rect, fCharWidth)) div fPosPerScroll);
    end
    else
      SetMax( SB_HORZ, 0 );
  end;

  procedure SetVerticalSB(const Rect: TRect);
  begin
    if (Rect.Height < fDataHeight) then
    begin
      SetMax( SB_VERT,
        (fLineCount - LinesPerHeight( Rect, fCharHeight )) div fPosPerScroll );
    end else
      SetMax( SB_VERT, 0 );
  end;

  {function myGetClientRect: TRect;
  begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := Width;
    Result.Bottom := Height;
    Result.Inflate(2, 2);
    if (IsBarVisible(OBJID_VSCROLL)) then
      Result.Width := Result.Width - GetSystemMetrics(SM_CXVSCROLL);
    if (IsBarVisible(OBJID_HSCROLL)) then
      Result.Height := Result.Height - GetSystemMetrics(SM_CYVSCROLL);
  end;}

var
  r, r2: TRect;
begin
  if fPosPerScroll = 0 then
    fPosPerScroll := 10;
  r := ClientRect;
  SetHorizontalSB(R);
  SetVerticalSB(R);
  r2 := ClientRect;
  if not CompareMem(@r, @r2, SizeOf(TRect)) then
    ScrollBoxOnResize(Sender)
  else
    DrawFrame;
end;


procedure TDataViewer.ScrollPage(Direction: TMoveDirection);
var
  Cmd: Integer;
  Msg: Integer;
  P: Int;
begin
  case Direction of
    mdLeft:
    begin
      Cmd := SB_PAGEUP;
      Msg := WM_HSCROLL;
    end;

    mdTop:
    begin
      Cmd := SB_PAGEUP;
      Msg := WM_VSCROLL;
    end;

    mdRight:
    begin
      Cmd := SB_PAGEDOWN;
      Msg := WM_HSCROLL;
    end;

    mdBottom:
    begin
      Cmd := SB_PAGEDOWN;
      Msg := WM_VSCROLL;
    end;

    else
      Exit;
  end;
  P.ULo := Cmd;
  P.UHi := 0;
  Perform(Msg, P.Int, 0);
end;


procedure TDataViewer.ScrollTimerOnTimer(Sender: TObject);
begin
  VScroll := VScroll - fScrollDelta.y;
  HScroll := HScroll - fScrollDelta.x;
  UpdateMousePos( ClientCursorPos );
end;


procedure TDataViewer.SelectAll;
begin
  if (not Empty) then
  begin
    Freeze;
    try
      SelStart := 0;
      SelEnd := fBufferSize;
    finally
      UnFreeze;
    end;
  end;
end;

procedure TDataViewer.SetFontColor(Value: TColor);
begin
  FFontColor := Value;
  Font.Color := Value;
end;

procedure TDataViewer.SetFontSize(Value: Integer);
begin
  Font.Size := Value;
end;

procedure TDataViewer.SetHandlers;
begin
  OnResize := ScrollBoxOnResize;
  OnMouseDown := ScrollBoxOnMouseDown;
  OnMouseUp := ScrollBoxOnMouseUp;
  OnMouseWheel := ScrollBoxOnMouseWheel;
  OnMouseMove := ScrollBoxOnMouseMove;
  OnKeyDown := ScrollBoxOnKeyDown;
end;


procedure TDataViewer.SetHCursor(MouseX: Integer);
var
  c: TCursor;
begin
  c := crIBeam;
  if not Selecting and (Mode = hmHex) then
    case ElementFromPos( MouseX )of
      0, 1, 2, 4, 6:
        c := NonSelectAreaCursor;
    end;
  Cursor := c;
end;


procedure TDataViewer.SetHScroll(Value: Integer);
var
  p: Int;
begin
  if (Value < 0) then
    Value := 0;
  P.ULo := SB_THUMBTRACK;
  P.UHi := Value;
  Perform(WM_HSCROLL, p.Int, 0);
//  Invalidate;
end;


procedure TDataViewer.SetIntDefaults;
begin
  fBufferSize := 0;
  fLineCount := 0;
  fBorderLeft := 5;
  FBorderRight := 0;
  fPosPerScroll := 1;
  fPageSize := 10;
  fDataHeight := 0;
  fDataWidth := 0;
  fStreamDataOffset := 0;
  fSelStart := 0;
  fSelEnd := 0;
  fSL.Clear;
  fScrollTimer.Enabled := False;
  Integer(fScrollDelta) := 0;
  fCaretShown := False;
  //fFreezeCount := 0;
  fScrollTimerInterval := 40;
  fScrollTimer.Interval := fScrollTimerInterval;
  fSelEndPos.Col := 0;
  fSelEndPos.Col := 0;
end;


procedure TDataViewer.SetCurLineWidth(Value: Integer);
begin
  FCurLineWidth := Value;
  UpdateProperties;
end;


procedure TDataViewer.SetMode( Value: TViewerMode );
begin
  fMode := Value;
  UpdateProperties;
end;


procedure TDataViewer.SetNormalCanvas(C: TCanvas);
begin
  C.Font.Assign(Font);
  C.Font.Color := FontColor;
  C.Brush.Color := BkColor;
end;


procedure TDataViewer.SetSelectedCanvas(C: TCanvas);
begin
  C.Font.Assign( Font );
  C.Font.Color := SelFontColor;
  C.Brush.Color := SelBkColor;
end;


procedure TDataViewer.SetSelectionElement( Value: TViewerElement );
begin
  fSelectionElement := Value;
  Invalidate;
end;


procedure TDataViewer.SetSelEnd( Value: Integer );
var
  pos: TPos;
begin
  if Value < 0 then
    Value := 0;
  if Value > Integer(fBufferSize) then
    Value := fBufferSize;
  fPrevSelEnd := fSelEnd;
  fSelEnd := Value;

  Pos := GetCharPos( Value );
  //CorrectPos( Pos, Value );
  fSelEndPos := Pos;

  if HandleAllocated then
  begin
    MakePosVisible(pos);
    UpdateCaretPos(False, @pos);
    Invalidate;
  end;
end;


procedure TDataViewer.SetSelEndPos( Value: TPos; NewLineIfEOL: Boolean );

  procedure CheckValue;
  begin
    if Value.Row < 0 then
      Value.Row := 0;
    if Value.Row >= fLineCount then
      Value.Row := fLineCount - 1;
  end;

var
  ml: Integer;
  se: Integer;
begin
  CheckValue;
  case Mode of
    hmHex: ML := 16;
    hmTextBinary: ml := CurLineWidth;
    hmTextFree: ML := Length(fSL[Value.Row]);
    else
      ML := 0;
  end;

  if Value.Col > ML then
    if NewLineIfEOL then
    begin
      if (Value.Row < (FLineCount - 1)) then
      begin
        Inc(Value.Row);
        Value.Col := 0;
      end;
    end
    else
      Value.Col := ML;

  CheckValue;

  if Value.Col < 0 then
    if NewLineIfEOL and (Value.Row > 0) then
    begin
      if (Value.Row > 0) then
      begin
        Dec(Value.Row);
        case Mode of
          hmHex: Value.Col := 16;
          hmTextBinary: Value.Col := CurLineWidth;
          hmTextFree: Value.Col := Length(FSL[Value.Row]);
          else
            Value.Col := 0;
        end;
      end;
    end
    else
      Value.Col := 0;

  SE := GetPosChar( Value );
  while SE > fBufferSize do
  begin
    Dec(Value.Col);
    if Value.Col < 0 then
    begin
      Dec( Value.Row );
      Value.Col := 15;
    end;
    Dec( SE );
  end;

  MakePosVisible( Value );

  fSelEnd := SE;
  fSelEndPos := Value;

  UpdateCaretPos( False, @Value );
end;


procedure TDataViewer.SetSelInactiveCanvas(C: TCanvas);
begin
  C.Font.Assign(Font);
  C.Font.Color := FontColor;
  C.Brush.Color := SelInactiveColor;
end;


procedure TDataViewer.SetSelStart( Value: Integer );
begin
  if Value < 0 then
    Value := 0;
  if Value > Integer(fBufferSize) then
    Value := fBufferSize;
  fSelStart := Value;
  Invalidate;
end;


procedure TDataViewer.SetStream(
  AStream: TStream; AStreamDataOffset, AStreamDataSize: Cardinal; AMode: TViewerMode );
var
  ss: string;
  si: tagSCROLLINFO;
  c: TCanvas;
  i, ml, l: Integer;
  stm: TMemoryStream;
  ptr: Pointer;
begin
  if AStream <> nil then
  begin
    SetIntDefaults;

    C := NewNormalCanvas;
    fBufferSize := AStreamDataSize;
    fStreamDataOffset := AStreamDataOffset;

    fCharHeight := C.TextHeight( 'qtyipdfhjklbIJYQ' );
    fCharWidth := C.TextWidth( 'O' );

    fSL.Clear;

    case AMode of
      hmHex:
      begin
        ss := AddressTempl + AddressDelTempl + HexTempl + HexDelTempl + DataTempl;
        FCurLineWidth := Length(ss);
        fLineCount := fBufferSize div 16 + Integer(CBool(FBufferSize mod 16));
        fDataWidth := CurLineWidth * fCharWidth;
      end;

      hmTextBinary:
      begin
        FCurLineWidth := BinaryTextLineWidth;
        fLineCount := FBufferSize div CurLineWidth + Integer(CBool(FBufferSize mod CurLineWidth));
        fDataWidth := CurLineWidth * fCharWidth;
      end;

      hmTextFree:
      begin
        Stm := TMemoryStream.Create;
        Stm.Size := fBufferSize;
        Stm.Seek(0, soFromBeginning);
        AStream.Seek(AStreamDataOffset, soFromBeginning);
        ptr := stm.Memory;
        AStream.Read( Ptr^, AStreamDataSize );
        ConvertNonPrintingChars( Ptr, AStreamDataSize, NonPrintReplacement, False );

        Stream2StrList( fSL, Stm );
        Stm.Free;
        ML := 0;
        for I := 0 to fSL.Count - 1 do
        begin
          L := Length(fSL[I]);
          if L > ML then
            ML := L;
        end;
        FLineCount := fSL.Count;
        FCurLineWidth := ML;
        FDataWidth := CurLineWidth * fCharWidth;
      end;

//      hmTextWordWrap: fLineCount := 0;
    end;


    //fDataWidth := C.TextWidth( ss );
    fDataHeight := fCharHeight * fLineCount;
    C.Free;

    FillChar( SI, SizeOf( SI ), 0 );
    SI.cbSize := SizeOf( SI );
    SI.fMask := SIF_ALL;
    SI.nMin := 0;
    SI.nPage := fPageSize;
    SI.nMax := 0;
    SI.nPos := 0;

    fPosPerScroll := 1;

    fStream := AStream;
    fMode := AMode;
    SetSelEnd(0);
    fCaretShown := True;
    SetScrollInfo(Handle, SB_VERT, SI, True);
    SetScrollInfo(Handle, SB_HORZ, SI, True);
    ScrollBoxOnResize(nil);
  end
  else
  begin
    SetIntDefaults;
    fStream := AStream;
    fMode := AMode;
    SetSelEnd(0);
  end;
end;


procedure TDataViewer.SetVisDefaults;
var
  C: TCanvas;
begin
  fVisibleElements := [ hvOffset, hvHexData, hvCharData ];
  fBkColor := Def_BkColor;
  FFontColor := clWindowText;
  fSelBkColor := Def_SelBkColor;
  fSelFontColor := Def_SelFontColor;
  fSelInactiveColor := Def_SelInactiveColor;
  Color := fBkColor;

  Font.Name := 'Lucida Console';
//  Font.Name := 'Fixedsys';
//  Font.Height := -13;
//  Font.Charset := 204;
  Font.Color := fFontColor;

  fNonPrintReplacement := Def_NPRepl;
  fMode := hmHex;
  fSelecting := False;
//  fLineWidth := 80;
  NonSelectAreaCursor := crArrow;
  Cursor := crIBeam;


  //---------------------
  C := NewNormalCanvas;
  fCharHeight := C.TextHeight( 'qtyipdfhjklbIJYQ' );
  fCharWidth := C.TextWidth( 'O' );
  C.Free;
end;


procedure TDataViewer.SetVisibleElements(Value: TViewerElements);
var
  S: string;
  C: TCanvas;
begin
  S := '';
  if hvOffset in Value then
    if (hvHexData in Value) or (hvCharData in Value) then
      S := S + AddressTempl + AddressDelTempl
    else
      S := S + AddressTempl;
  if hvHexData in Value then
    if hvCharData in Value then
      S := S + HexTempl + HexDelTempl
    else
      S := S + HexTempl;
  if hvCharData in Value then
    S := S + DataTempl;

  fVisibleElements := Value;

  FCurLineWidth := Length(S);
  C := NewNormalCanvas;
  fDataWidth := C.TextWidth( S );
  C.Free;
  ScrollBoxOnResize( nil );
end;


procedure TDataViewer.SetVScroll(Value: Integer);
var
  p: Int;
begin
  if (Value < 0) then
    Value := 0;
  P.ULo := SB_THUMBTRACK;
  P.UHi := Value;
  Perform(WM_VSCROLL, P.Int, 0);
//  Invalidate;
end;


procedure TDataViewer.ShowCaret;
begin
  CreateCaret(Handle, 0, 2, fCharHeight);
  fCaretShown := Winapi.Windows.ShowCaret(Handle);
  //if SelEndChanged then
  //  UpdateCaretPos( False, @fPrevCharPos );
  UpdateCaretPos( False, @fSelEndPos );
end;


procedure TDataViewer.UnFreeze;
begin
  Dec( fFreezeCount );
  if fFreezeCount <= 0 then
    inherited Invalidate;
  if fFreezeCount < 0 then
    fFreezeCount := 0;
end;


procedure TDataViewer.UpdateCaretPos(CalcCharPos: Boolean; Pos: PPos);
var
  Pt: TPoint;
begin
//  Exit;
  if fCaretShown then
  begin
    if not Empty then
    begin
      if CalcCharPos then
        Pt := GetCharPoint(SelEnd, SelectionElement)
      else if (Pos <> nil) then
        Pt := GetPosPoint(Pos^, SelectionElement)
      else
        Pt := GetPosPoint(SelEndPos, SelectionElement);
      DecPointOnScroll( Pt );
    end else
    begin
      Pt.X := fBorderLeft;
      Pt.Y := 0;
    end;
    SetCaretPos( Pt.X - 1, Pt.Y - 0 );
  end
end;


procedure TDataViewer.UpdateMousePos(const Mouse: TPoint);
var
  Pos: TPos;
begin
  SetHCursor( Mouse.X );
  if Selecting then
  begin
    Pos := GetPointPos( MakePoint( Mouse.X, Mouse.Y ), fSelectionElement );
    SetSelEndPos( Pos );
    Invalidate;
  end;
end;


procedure TDataViewer.UpdateProperties;
begin
  SetStream(Stream, StreamDataOffset, StreamDataSize, Mode);
end;


procedure TDataViewer.WndProc(var AMessage: TMessage);
var
  defaultProc: Boolean;
  ps: TPaintStruct;
  si: TScrollInfo;
  bar: Integer;
begin
  defaultProc := True;
  case AMessage.Msg of
    WM_VSCROLL,
    WM_HSCROLL:
      begin
        if (AMessage.Msg = WM_VSCROLL) then
          Bar := SB_VERT
        else
          Bar := SB_HORZ;

        SI.cbSize := Sizeof( SI );
        SI.fMask := SIF_RANGE or SIF_POS or SIF_PAGE or
                    {$IFDEF F_P}$10{$ELSE}SIF_TRACKPOS{$ENDIF};
        GetScrollInfo(Handle, Bar, SI);
        SI.fMask := SIF_POS;
        case AMessage.WParamLo of
          SB_BOTTOM:    SI.nPos := SI.nMax;
          SB_TOP:       SI.nPos := SI.nMin;
          SB_LINEDOWN:  Inc( SI.nPos, FPosPerScroll);
          SB_LINEUP:    Dec( SI.nPos, FPosPerScroll);
          SB_PAGEDOWN:  Inc( SI.nPos, Max( SI.nPage, 1 ) );
          SB_PAGEUP:    Dec( SI.nPos, Max( SI.nPage, 1 ) );
          SB_THUMBTRACK:SI.nPos := AMessage.WParamHi; //si.nTrackPos; //AMessage.lParam;
          SB_THUMBPOSITION: si.nPos := AMessage.WParamHi;
        end;
        if SI.nPos > SI.nMax { - Integer( SI.nPage ) } then
          SI.nPos := SI.nMax { - Integer( SI.nPage ) };
        if SI.nPos < SI.nMin then
          SI.nPos := SI.nMin;
        SetScrollInfo(Handle, Bar, SI, True);
        Invalidate;
        UpdateCaretPos;
      end;

    WM_PAINT:
      begin
        if not Freezed then
        begin
          BeginPaint(Handle, PS);
          if (AMessage.wParam = 0) then
            Paint(ps.hdc, ps.rcPaint)
          else
            Paint(AMessage.wParam, ps.rcPaint);
          EndPaint(Handle, PS);
        end
        else
          ValidateRect(Handle, nil);
        AMessage.Result := 0;
        defaultProc := False;
      end;

    WM_NCPAINT,
    WM_NCACTIVATE:
      defaultProc := not DrawFrame;

    WM_SETFOCUS:
      ShowCaret;

    WM_KILLFOCUS:
      HideCaret;

    WM_ERASEBKGND:
      begin
        if not Freezed then
        begin
          BeginPaint(Handle, PS);
          if (AMessage.wParam = 0) then
            Paint(ps.hdc, ps.rcPaint)
          else
            Paint(AMessage.wParam, ps.rcPaint);
          EndPaint(Handle, PS);
        end
        else
          ValidateRect(Handle, nil);
        AMessage.Result := 0;
        defaultProc := False;
      end;
  end;
  if defaultProc then
    inherited;
end;

end.

