unit CommonUtilities;

interface
uses
  Winapi.Windows, Classes, Controls, ComCtrls, SysUtils, TypInfo, Winapi.Messages, Winapi.ImageHlp,
  StrUtils;

type
  SysInt = NativeInt;
  SysUInt = NativeUInt;
  PSysInt = PNativeInt;
  PSysUInt = PNativeUInt;


  THotKey = packed record
    case Integer of
      1:(
        ControlKey: Byte;
        Reserved1: Byte;
        Key: Byte;
        Reserved2: Byte;
      );
      2:(
        AsDWORD: DWord;
      );
  end;

  Int = packed record
    case Integer of
    1: ( ULo, UHi: Word );
    2: ( SLo, SHi: Smallint );
    3: ( Int: LongInt );
    4: ( Dword: DWORD );
  end;

//  TSortOrder = (soNone, soASC, soDESC);


  TStringArray = array of string;


procedure ErrorBox(const Message: string);
procedure MsgBox(const Message: string);
function MakeHotKey(Key: Word; ControlKey: TShiftState): THotKey;
//procedure ListViewStdKeyDownProc(Sender: TObject; var Key: Word; Shift: TShiftState);
function RecurseGetProperty(AObj: TPersistent; const APropName: string): string;
function RecurseSetProperty(AObj: TPersistent; const APropName, APropValue: string): Boolean;
function Parse(var S: string; const Separator: string): string;
function CBool(N: Cardinal): Cardinal; inline;
function GetShiftState: TShiftState;
procedure FreezeControl(AControl: TWinControl);
procedure UnfreezeControl(AControl: TWinControl);
function MyIntToHex(N: Cardinal): string;
function MyIntToHex64(N: UInt64): string;
function MyOrdinal(N: Int64): string;
function UndecorateCPPName(const Name: string): string;
function MakeRect(Left, Top, Right, Bottom: Integer): TRect; inline;
function MakePoint(X, Y: Integer): TPoint; inline;
function LangDesc(Lang: DWORD): string;
procedure CheckLastError;
function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
function GluePath(const Pieces: array of string): string;
function GetFileList(
  const Dir: string; const Mask: string; IncludeSubDirs: Boolean = False): TStringArray;
function GetLangString(ALang: Integer): string;
function GetPluginFileName: string;
function CRC32(CRC: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;
function IIF(ACondition: Boolean; ATrue, AFalse: Pointer): Pointer; inline; overload;
function IIF(ACondition: Boolean; ATrue, AFalse: Int64): Int64; inline; overload;
function IIF(ACondition: Boolean; ATrue, AFalse: UInt64): UInt64; inline; overload;
{$IFNDEF WIN64}
function IIF(ACondition: Boolean; ATrue, AFalse: Integer): Integer; inline; overload;
{$ENDIF}
function GetFileSize(const AFileName: string): UInt64;
procedure ReadBufferFromFile(
  const AFileName: string; ABuffer: Pointer; ASize: Integer);


const
  SMsgBoxTitle = 'PE Viewer';

  MK_ALT = $20;


  HK_C_TAB: THotKey = (ControlKey: MK_CONTROL; Key: VK_TAB);
  HK_CS_TAB: THotKey = (ControlKey: MK_CONTROL or  MK_SHIFT; Key: VK_TAB);
  HK_AC_TAB: THotKey = (ControlKey: MK_ALT or MK_CONTROL; Key: VK_TAB);
  HK_ACS_TAB: THotKey = (ControlKey: MK_ALT or MK_CONTROL or MK_SHIFT; Key: VK_TAB);
  HK_C_C: THotKey = (ControlKey: MK_CONTROL; Key: Ord('C'));
  HK_C_A: THotKey = (ControlKey: MK_CONTROL; Key: Ord('A'));
  HK_TAB: THotKey = (Key: VK_TAB);
  HK_S_TAB: THotKey = (ControlKey: MK_SHIFT; Key: VK_TAB);



type

  {$M+}
  TStringStore = class
  public
    constructor Create; virtual;

    function GetString(const AStringName: string): string;
    function ConvertNameToValue(const AFieldName: string): string;
  end;
  {$M-}



implementation


{ TStringStore }


function TStringStore.ConvertNameToValue(const AFieldName: string): string;
begin
  Result := ReplaceStr(AFieldName, '_', ' ');
end;

constructor TStringStore.Create;
var
  pl: PPropList;
  cnt, i: Integer;
  pi: PPropInfo;
begin
  cnt := GetPropList(Self, pl);
  try
    for i := 0 to cnt - 1 do
    begin
      pi := pl[i];
      if (pi.PropType^.Kind in [tkString, tkUString, tkChar, tkWChar]) then
      begin
        SetPropValue(Self, pi, ConvertNameToValue(GetPropName(pi)));
      end;
    end;
  finally
    FreeMem(pl);
  end;
end;

function TStringStore.GetString(const AStringName: string): string;
var
  pi: PPropInfo;
begin
  Result := ConvertNameToValue(AStringName);
  pi := GetPropInfo(Self, AStringName, [tkString, tkUString]);
  if (pi <> nil) then
    Result := GetPropValue(Self, pi, True);
end;


procedure ErrorBox(const Message: string);
begin
  MessageBox(0, PChar(Message), SMsgBoxTitle, MB_OK or MB_ICONINFORMATION or MB_TASKMODAL);
end;

procedure MsgBox(const Message: string);
begin
  MessageBox(0, PChar(Message), SMsgBoxTitle, MB_OK or MB_ICONINFORMATION or MB_TASKMODAL);
end;

function MakeHotKey(Key: Word; ControlKey: TShiftState): THotKey;
begin
  Result.AsDWORD := 0;
  Result.Key := Key;
  if (ssShift in ControlKey) then
    Result.ControlKey := Result.ControlKey or MK_SHIFT;
  if (ssCtrl in ControlKey) then
    Result.ControlKey := Result.ControlKey or MK_CONTROL;
  if (ssAlt in ControlKey) then
    Result.ControlKey := Result.ControlKey or MK_ALT;
end;

{procedure ListViewStdKeyDownProc(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  hk: THotKey;
  s: string;
  i: Integer;
  item: TListItem;
  lv: TListView;
begin
  Exit;
  lv := Sender as TListView;
  hk := MakeHotKey(Key, Shift);
  if (hk.AsDWORD = HK_C_C.AsDWORD) then
  begin
    s := '';
    if (lv.SelCount > 0) then
    begin
      for i := 0 to lv.Items.Count - 1 do
      begin
        item := lv.Items[i];
        if (item.Selected) then
        begin
          s := s + item.Caption + ': ' + Trim(item.SubItems.DelimitedText) + #13#10;
        end;
      end;
    end
    else
    begin
      for i := 0 to lv.Items.Count - 1 do
      begin
        item := lv.Items[i];
        s := s + item.Caption + ': ' + Trim(item.SubItems.DelimitedText) + #13#10;
      end;
    end;
    Clipboard.AsText := s;
  end
  else if (hk.AsDWORD = HK_C_A.AsDWORD) then
  begin
    for i := 0 to lv.Items.Count - 1 do
    begin
      item := lv.Items[i];
      item.Selected := True;
    end;
  end;
end;}


function RecurseSetProperty(AObj: TPersistent; const APropName, APropValue: string): Boolean;
var
  dp, bp, collectionIndex: Integer;
  curPropName, newPropName: string;
  pi: PPropInfo;
  newObj: TPersistent;
begin
  Result := False;
  dp := Pos('.', APropName);
  try
    if (dp > 0) then
    begin
      curPropName := Copy(APropName, 1, dp - 1);
      newPropName := Copy(APropName, dp + 1, MaxInt);

      bp := Pos('[', curPropName);
      if (bp > 0) then
      begin
        if (curPropName[Length(curPropName)] <> ']') then
        begin
          Result := False;
          Exit;
        end;
        collectionIndex := StrToInt(Copy(curPropName, bp + 1, Length(curPropName) - bp - 1));
        curPropName := Copy(curPropName, 1, bp - 1);
      end
      else
        collectionIndex := -1;

      pi := GetPropInfo(AObj, curPropName);
      if (pi <> nil) then
        newObj := TPersistent(GetObjectProp(AObj, curPropName, TPersistent))
      else
        newObj := TPersistent(AObj.FieldAddress(curPropName)^);
      if (collectionIndex >= 0) then
        newObj := (newObj as TCollection).Items[collectionIndex];
      Result := RecurseSetProperty(newObj, newPropName, APropValue);
    end
    else
    begin
      pi := GetPropInfo(AObj, APropName);
      if (pi <> nil) and not (pi.PropType^.Kind in [tkClass, tkClassRef]) then
      begin
        SetPropValue(AObj, pi, APropValue);
        Result := True;
      end;
    end;
  except
    Result := False;
  end;
end;

function RecurseGetProperty(AObj: TPersistent; const APropName: string): string;
var
  dp, bp, collectionIndex: Integer;
  curPropName, newPropName: string;
  pi: PPropInfo;
  newObj: TPersistent;
begin
  dp := Pos('.', APropName);
  try
    if (dp > 0) then
    begin
      curPropName := Copy(APropName, 1, dp - 1);
      newPropName := Copy(APropName, dp + 1, MaxInt);

      bp := Pos('[', curPropName);
      if (bp > 0) then
      begin
        if (curPropName[Length(curPropName)] <> ']') then
        begin
          Result := '';
          Exit;
        end;
        collectionIndex := StrToInt(Copy(curPropName, bp + 1, Length(curPropName) - bp - 1));
        curPropName := Copy(curPropName, 1, bp - 1);
      end
      else
        collectionIndex := -1;

      pi := GetPropInfo(AObj, curPropName);
      if (pi <> nil) then
        newObj := TPersistent(GetObjectProp(AObj, curPropName, TPersistent))
      else
        newObj := TPersistent(AObj.FieldAddress(curPropName)^);
      if (collectionIndex >= 0) then
        newObj := (newObj as TCollection).Items[collectionIndex];
      Result := RecurseGetProperty(newObj, newPropName);
    end
    else
    begin
      pi := GetPropInfo(AObj, APropName);
      if (pi <> nil) and not (pi.PropType^.Kind in [tkClass, tkClassRef]) then
        Result := GetPropValue(AObj, pi, True)
      else
        Result := '';
    end;
  except
    Result := '';
  end;
end;

function Parse(var S: string; const Separator: string): string;
var
  p: Integer;
begin
  p := Pos(Separator, S);
  if (p <= 0) then
     p := Length(S);
  Result := Copy(S, 1, p - 1);
  Delete(S, 1, p);
end;

function CBool(N: Cardinal): Cardinal;
begin
  if (N = 0) then
    Result := N
  else
    Result := 1;
end;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

procedure FreezeControl(AControl: TWinControl);
begin
  AControl.Perform(WM_SETREDRAW, 0, 0);
end;

procedure UnfreezeControl(AControl: TWinControl);
begin
  AControl.Perform(WM_SETREDRAW, 1, 0);
end;

function MyIntToHex(N: Cardinal): string;
begin
  Result := '0x' + IntToHex(N, 8);
end;

function MyIntToHex64(N: UInt64): string;
begin
  Result := '0x' + IntToHex(N, 16);
end;

function MyOrdinal(N: Int64): string;
begin
  if (N = 0) then
    Result := ''
  else
    Result := IntToStr(N);
end;

function UndecorateCPPName(const Name: string): string;
var
  s: AnsiString;
begin
  SetLength(s, 512);
  SetLength(
    s,
    UnDecorateSymbolName(PAnsiChar(AnsiString(Name)), @s[1], 512, UNDNAME_COMPLETE));
  Result := string(s);
end;

function MakeRect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function MakePoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function LangDesc(Lang: DWORD): string;
begin
  SetLength(Result, 513);
  SetLength(Result, VerLanguageName(Lang, @Result[1], 512));
end;

procedure CheckLastError;
var
  ec: Cardinal;
begin
  ec := GetLastError;
  if (ec <> 0) then
    RaiseLastOSError(ec);
end;

function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
var
  ModifiedTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if (Int64(AFileTime) = 0) then
    Exit;
  try
    FileTimeToLocalFileTime(AFileTime, ModifiedTime);
    FileTimeToSystemTime(ModifiedTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  except
    Result := Now;  // Something to return in case of error
  end;
end;

function GluePath(const Pieces: array of string): string;
var
  i, l: Integer;
  s: string;
begin
  Result := '';
  l := Low(Pieces);
  for i := l to High(Pieces) do
  begin
    if (i > l) then
    begin
      s := Pieces[i];
      if (s <> '') and (s[1] = PathDelim) then
        raise Exception.Create('Invalid path piece');
      if (Result <> '') and not IsPathDelimiter(Result, Length(Result)) then
        Result := Result + PathDelim;
      Result := Result + s;
    end
    else
      Result := Pieces[i];
  end;
end;

procedure DynArrayAppend(var A: Pointer; const Append: Pointer; ItemsTypeInfo: Pointer); overload;
var
  l, la: Integer;
begin
  la := DynArraySize(Append);
  if (la > 0) then
  begin
    l := DynArraySize(A);
    SetLength(TBoundArray(A), l + la);
    if (ItemsTypeInfo <> nil) then
      CopyArray(@TBoundArray(A)[l], @TBoundArray(Append)[0], ItemsTypeInfo, la)
    else
      Move(TBoundArray(Append)[0], TBoundArray(A)[l], la * SizeOf(TBoundArray(A)[0]));
  end;
end;

procedure DynArrayAppend(var A: TStringArray; const Append: TStringArray); overload;
begin
  DynArrayAppend(Pointer(A), Pointer(Append), TypeInfo(string));
end;

procedure DynArrayInsert(var A: Pointer; const Piece: Pointer; Idx: Integer; ItemsTypeInfo: Pointer);
const
    isz = SizeOf(TBoundArray(A)[0]);
var
  l, la: Integer;
begin
  la := Length(TBoundArray(Piece));
  if (la > 0) then
  begin
    if (Idx < 0) or (Idx > (la - 1)) then
      raise Exception.Create('Index %d is out of bounds');
    l := Length(TBoundArray(A));
    if (idx = l) then
    begin
      DynArrayAppend(A, Piece, ItemsTypeInfo);
    end
    else
    begin
      SetLength(TBoundArray(A), l + la);
      if (ItemsTypeInfo <> nil) then
      begin
        CopyArray(@TBoundArray(A)[Idx + la], @TBoundArray(A)[Idx], ItemsTypeInfo, l - idx);
        CopyArray(@TBoundArray(A)[Idx], @TBoundArray(Piece)[0], ItemsTypeInfo, la);
      end
      else
      begin
        Move(TBoundArray(A)[Idx], TBoundArray(A)[Idx + la], (l - idx) * isz);
        Move(TBoundArray(Piece)[0], TBoundArray(A)[Idx], la * isz);
      end;
    end;
  end;
end;

procedure DynArrayAddItem(var A: TStringArray; Item: string);
var
  l: Integer;
begin
  l := Length(TStringArray(A));
  SetLength(TStringArray(A), l + 1);
  TStringArray(A)[l] := Item;
end;

function GetFileList(const Dir: string; const Mask: string; IncludeSubDirs: Boolean): TStringArray;
var
  sr: TSearchRec;
  r, i: Integer;
  s, fn: string;
  subDirItems: TStringArray;
begin
  SetLength(Result, 0);
  if (Dir <> '') then
  begin
    s := IncludeTrailingPathDelimiter(Dir);
    r := FindFirst(s + Mask, faAnyFile and not faDirectory, sr);
    while (r = 0) do
    begin
      if (sr.Attr and faDirectory <> faDirectory) then
      begin
        fn := s + sr.Name;
        DynArrayAddItem(Result, sr.Name);
      end;
      r := FindNext(sr);
    end;
    SysUtils.FindClose(sr);

    if IncludeSubDirs then
    begin
      r := FindFirst(s + '*.*', faDirectory, sr);
      while (r = 0) do
      begin
        if (sr.Attr and faDirectory = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          fn := s + sr.Name;
          subDirItems := GetFileList(fn, Mask, IncludeSubDirs);
          if (Length(subDirItems) > 0) then
          begin
            for i := 0 to Length(subDirItems) - 1 do
              subDirItems[i] := sr.Name + PathDelim + subDirItems[i];
            DynArrayAppend(Result, subDirItems);
          end;
        end;
        r := FindNext(sr);
      end;
      SysUtils.FindClose(sr);
    end;
  end;
end;

function GetLangString(ALang: Integer): string;
begin
  Result := IntToStr(ALang) + ' (0x' + IntToHex(ALang, 4) + ') ' + LangDesc(ALang);
end;

var
  PluginFileName: string;

function GetPluginFileName: string;
begin
  if (PluginFileName = '') then
  begin
    SetLength(PluginFileName, MAX_PATH);
    SetLength(PluginFileName, GetModuleFileName(HInstance, @PluginFilename[1], MAX_PATH));
  end;
  Result := PluginFileName;
end;

var
  // used to calculate the running CRC of a bunch of bytes,
  // this table is dynamically created in order to save space if never needed
  CRCTable: array of Cardinal;

procedure MakeCRCTable;

// creates the CRC table when it is needed the first time

var
  C: Cardinal;
  N, K : Integer;
  Poly: Cardinal; // polynomial exclusive-or pattern

const
 // terms of polynomial defining this CRC (except x^32)
 P: array [0..13] of Byte = (0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22, 23, 26);

begin
  // make exclusive-or pattern from polynomial ($EDB88320)
  SetLength(CRCTable, 256);
  Poly:=0;
  for N:=0 to SizeOf(P) - 1 do
    Poly:=Poly or (1 shl (31 - P[N]));

  for N:=0 to 255 do
  begin
    C:=N;
    for K:=0 to 7 do
    begin
      if (C and 1)<>0 then C:=Poly xor (C shr 1)
                        else C:=C shr 1;
    end;
    CRCTable[N]:=C;
  end;
end;

function CRC32(CRC: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

// Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
// x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.
//
// Polynomials over GF(2) are represented in binary, one bit per coefficient,
// with the lowest powers in the most significant bit.  Then adding polynomials
// is just exclusive-or, and multiplying a polynomial by x is a right shift by
// one.  If we call the above polynomial p, and represent a byte as the
// polynomial q, also with the lowest power in the most significant bit (so the
// byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
// where a mod b means the remainder after dividing a by b.
//
// This calculation is done using the shift-register method of multiplying and
// taking the remainder.  The register is initialized to zero, and for each
// incoming bit, x^32 is added mod p to the register if the bit is a one (where
// x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
// x (which is shifting right by one and adding x^32 mod p if the bit shifted
// out is a one).  We start with the highest power (least significant bit) of
// q and repeat for all eight bits of q.
//
// The table is simply the CRC of all possible eight bit values.  This is all
// the information needed to generate CRC's on data a byte at a time for all
// combinations of CRC register values and incoming bytes.

begin
  if Buffer = nil then Result:=0
                  else
  begin
    if CRCTable = nil then MakeCRCTable;

    CRC:=CRC xor $FFFFFFFF;
    while Len >= 8 do
    begin
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC:=CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);

      Dec(Len, 8);
    end;

    while Len > 0 do
    begin
      CRC:=CRCTable[(CRC xor Buffer^) and $FF] xor (CRC shr 8);
      Inc(Buffer);
      Dec(Len);
    end;
    Result:=CRC xor $FFFFFFFF;
  end;
end;

function IIF(ACondition: Boolean; ATrue, AFalse: Pointer): Pointer; inline;
begin
  if (ACondition) then
    Result := ATrue
  else
    Result := AFalse;
end;

function IIF(ACondition: Boolean; ATrue, AFalse: Int64): Int64; inline;
begin
  if (ACondition) then
    Result := ATrue
  else
    Result := AFalse;
end;

function IIF(ACondition: Boolean; ATrue, AFalse: UInt64): UInt64; inline;
begin
  if (ACondition) then
    Result := ATrue
  else
    Result := AFalse;
end;

{$IFNDEF WIN64}
function IIF(ACondition: Boolean; ATrue, AFalse: Integer): Integer; inline;
begin
  if (ACondition) then
    Result := ATrue
  else
    Result := AFalse;
end;
{$ENDIF}

function GetFileSize(const AFileName: string): UInt64;
var
  f: THandle;
begin
  f :=
    CreateFile(
      PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if (f <> INVALID_HANDLE_VALUE) then
  begin
    try
      Int64Rec(Result).Lo := Winapi.Windows.GetFileSize(f, @Int64Rec(Result).Hi);
    finally
      CloseHandle(f);
    end;
  end
  else
    Result := 0;
end;

procedure ReadBufferFromFile(
  const AFileName: string; ABuffer: Pointer; ASize: Integer);
var
  f: THandle;
  br: Cardinal;
begin
  f :=
    CreateFile(
      PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if (f <> INVALID_HANDLE_VALUE) then
  begin
    try
      ReadFile(f, ABuffer^, ASize, br, nil);
    finally
      CloseHandle(f);
    end;
  end
  else
    RaiseLastOSError;
end;

end.
