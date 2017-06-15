unit InfoFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PEViewerFrameUnit, StdCtrls, ComCtrls, Math, CommonUtilities, Clipbrd, ImgList,
  Vcl.ActnList, PEUtils, Vcl.Menus, Vcl.ExtCtrls, IniFiles, MyListViewUnit;

type

  TInfoStringStore = class(TStringStore)
  private
    FFile_Size: string;
    FFile_SizeFormat: string;
    FFile_Created: string;
    FEntry_Point_Raw_Offset: string;
    FFile_Modified: string;
    FLanguage: string;
    FFile_Subtype: string;
    FFile_Type: string;
    FFile_OS: string;
    FFile_Flags: string;
    FFile_Version: string;
    FProduct_Version: string;
    FFileVersion: string;
    FProductVersion: string;
    FProductName: string;
    FLegalCopyright: string;
    FFileDescription: string;
    FInternalName: string;
    FOriginalFilename: string;
    FCompiler: string;
    FUnknownCompiler: string;
  public
    constructor Create; override;
  published
    property File_Size: string read FFile_Size write FFile_Size;
    property File_SizeFormat: string read FFile_SizeFormat write FFile_SizeFormat;
    property File_Modified: string read FFile_Modified write FFile_Modified;
    property File_Created: string read FFile_Created write FFile_Created;
    property Entry_Point_Raw_Offset: string read FEntry_Point_Raw_Offset write FEntry_Point_Raw_Offset;
    property Language: string read FLanguage write FLanguage;
    property File_Version: string read FFile_Version write FFile_Version;
    property Product_Version: string read FProduct_Version write FProduct_Version;
    property File_OS: string read FFile_OS write FFile_OS;
    property File_Flags: string read FFile_Flags write FFile_Flags;
    property File_Type: string read FFile_Type write FFile_Type;
    property File_Subtype: string read FFile_Subtype write FFile_Subtype;
    property FileDescription: string read FFileDescription write FFileDescription;
    property FileVersion: string read FFileVersion write FFileVersion;
    property InternalName: string read FInternalName write FInternalName;
    property LegalCopyright: string read FLegalCopyright write FLegalCopyright;
    property OriginalFilename: string read FOriginalFilename write FOriginalFilename;
    property ProductName: string read FProductName write FProductName;
    property ProductVersion: string read FProductVersion write FProductVersion;
    property Compiler: string read FCompiler write FCompiler;
    property UnknownCompiler: string read FUnknownCompiler write FUnknownCompiler;
  end;


  TInfoFrame = class(TPEViewerFrame)
    lvInfo: TMyListView;
    tCompilerDeterminationInterval: TTimer;
    aDetermineCompiler: TAction;
    N1: TMenuItem;
    DetermineCompiler1: TMenuItem;
    procedure tCompilerDeterminationIntervalTimer(Sender: TObject);
    procedure aDetermineCompilerExecute(Sender: TObject);
  private
    FCompilerDetermined: Boolean;
  protected
    procedure GetPropertiesList(AList: TStringList); override;
    procedure LoadVersionInfoData(ANode: TResNode; AStream: TStream; AStrList: TStringList);
    function GetCompilerName1(AImage: TPEImage): string;
    function GetCompilerName2(AImage: TPEImage): string;
    procedure DetermineCompiler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadData(const AFileName: string); override;
  published
    Strings: TInfoStringStore;
  end;

var
  InfoFrame: TInfoFrame;

implementation
uses
  PEViewerClass, PETypes;

{$R *.dfm}


const
  SNIF_BUFFER_SIZE = 1024;


{ TInfoFrame }


procedure TInfoFrame.aDetermineCompilerExecute(Sender: TObject);
begin
  DetermineCompiler;
end;

constructor TInfoFrame.Create(AOwner: TComponent);
begin
  inherited;
  Strings := TInfoStringStore.Create;
end;

destructor TInfoFrame.Destroy;
begin
  Strings.Free;
  inherited;
end;

procedure TInfoFrame.DetermineCompiler;
var
  plugin: TPEViewer;
  image: TPEImage;
  p1, p2: string;
  item: TMyListItem;
  i: Integer;
begin
  plugin := Pointer(PluginObject);
  image := plugin.PEImage;
  image.LoadImage;
  try
    image.LoadCommonData;
    if (FCompilerDetermined) then
    begin
      for i := lvInfo.Items.Count - 1 downto lvInfo.Items.Count - 3 do
      begin
        if (i >= 0) then
        begin
          lvInfo.Items[i].Delete;
        end;
      end;
    end;

    try
      p1 := GetCompilerName1(image);
    except
      on e: Exception do
        p1 := e.Message;
    end;
    try
      p2 := GetCompilerName2(image);
    except
      on e: Exception do
        p2 := e.Message;
    end;

    lvInfo.Items.Add;

    item := lvInfo.Items.Add;
    item.Caption := Strings.Compiler;
    item.SubItems.Add(p1);
    item := lvInfo.Items.Add;
    item.Caption := Strings.Compiler + ' (PEiD)';
    item.SubItems.Add(p2);
    FCompilerDetermined := True;
  finally
    image.UnloadImage;
  end;
end;

function TInfoFrame.GetCompilerName1(AImage: TPEImage): string;

var
  entryPoint: Pointer;


  function hex2Byte(TwoChars: PChar): Byte;

    function Powr( Base, Step: Integer ): Integer;
    var
      I: Integer;
    begin
      Result := 1;
      for I := 1 to Step do
        Result := Result * Base;
    end;

  const
    Allowed: set of AnsiChar = ['0'..'9', 'A'..'F', 'a'..'f'];
  var
    I: Integer;
  begin
    Result := 0;
    if not (AnsiChar(TwoChars[0]) in Allowed) or not (AnsiChar(TwoChars[1]) in Allowed) then
      Exit;
    for I := 0 to 1 do
    begin
      case TwoChars[ I ] of
        '0'..'9': Inc( Result, (Byte( TwoChars[ I ] ) - 48 ) * Powr( 16, 1 - I ) );
        'A'..'F': Inc( Result, (Byte( TwoChars[ I ] ) - 55 ) * Powr( 16, 1 - I ) );
        'a'..'f': Inc( Result, (Byte( TwoChars[ I ] ) - 87 ) * Powr( 16, 1 - I ) );
      end;
    end;
  end;

  function checkSignature(const ASignature: string): Single;
  var
    i: Integer;
    found, toFind, sigLen: Integer;
    buf: PByte;
  begin
    sigLen := Length(ASignature);
    found := 0;
    toFind := 0;
    i := 1;
    buf := entryPoint;
    while (i <= sigLen) do
    begin
      if (ASignature[i] <> ':') and (ASignature[i + 1] <> ':') then
      begin
        Inc(toFind);
        if (buf^ = Hex2Byte(@ASignature[i])) then
          Inc(found);
      end;
      Inc(i, 2);
      Inc(buf);
    end;

    if (toFind <> 0) then
      Result := found / toFind * 100
    else
      Result := 0.0;
  end;

var
  signsFn, s: string;
  signs: TStringList;
  i, maxIdx, epSection: Integer;
  percent, maxPercent: Single;
begin
  Result := Strings.UnknownCompiler;

  if (AImage.EntryPointExists) then
  begin
    signsFn := GluePath([ExtractFilePath(GetPluginFileName), 'signs.txt']);
    if (FileExists(signsFn)) then
    begin
      signs := TStringList.Create;
      try
        signs.LoadFromFile(signsFn, TEncoding.ANSI);

        entryPoint := Pointer(SysUInt(AImage.Mapper.Map) + AImage.GetEntryPointRAW(epSection));

        // convert signatures into our needs
        for i := signs.Count - 1 downto 0 do
        begin
          s := signs[i];
          if (s = '') or (s[1] <> '[') or (s[Length(s)] <> ']') then
            signs.Delete(i);
        end;
        maxPercent := -1;
        maxIdx := -1;
        for i := 0 to signs.Count - 1 do
        begin
          s := signs[i];
          s := Copy(s, 2, Length(s) - 2);
          signs[i] := s;
          percent := checkSignature(signs.ValueFromIndex[i]);
          if (percent > maxPercent) then
          begin
            maxPercent := percent;
            maxIdx := i;
            if (maxPercent = 100) then
              Break;
          end;
        end;
        if (maxIdx >= 0) and (maxPercent >= 80) then
        begin
          Result := signs.Names[maxIdx];
          if (maxPercent <= 99) then
            Result := Result + ' ' + FloatToStr(Round(maxPercent)) + '%';
        end;
      finally
        for i := 0 to signs.Count - 1 do
          signs.Objects[i] := nil;
        signs.Free;
      end;
    end;
  end;
end;

function TInfoFrame.GetCompilerName2(AImage: TPEImage): string;

const
  OP_COMPARE = 1;
  OP_SKIP = 2;
  OP_STOP = 0;

  OP_BUFFER_SIZE = 1024 * 1024;

type
  TOpHeader = record
    Op: SysInt;
    ByteCount: SysInt;
  end;
  POpHeader = ^TOpHeader;

var
  entryPoint: Pointer;
  opBuffer: Pointer;


  function hex2Byte(TwoChars: PAnsiChar): Byte;
  const
    Allowed: set of AnsiChar = ['0'..'9', 'A'..'F', 'a'..'f'];
  begin
    Result := 0;
    if not (AnsiChar(TwoChars[0]) in Allowed) or not (AnsiChar(TwoChars[1]) in Allowed) then
      Exit;
    case TwoChars[0] of
      '0'..'9': Inc( Result, (Byte( TwoChars[0] ) - 48 ) * 16);
      'A'..'F': Inc( Result, (Byte( TwoChars[0] ) - 55 ) * 16);
      'a'..'f': Inc( Result, (Byte( TwoChars[0] ) - 87 ) * 16);
    end;
    case TwoChars[1] of
      '0'..'9': Inc( Result, (Byte( TwoChars[1] ) - 48 ));
      'A'..'F': Inc( Result, (Byte( TwoChars[1] ) - 55 ));
      'a'..'f': Inc( Result, (Byte( TwoChars[1] ) - 87 ));
    end;
  end;

var
  signsFn: string;
  tmp: AnsiString;
  eqPos, epSection, dataSize, lineLength, sizeLeft,
  compilerLength, signLength, lineNameLength, lineValueLength, signLengthLeft: Integer;
  data, curLine, nextLine, compilerName, sign, lineName, lineValue,
  signPos: PAnsiChar;
  signRed, compilerRed, epOnlyRed, epOnly, curByteAny, signatureFound: Boolean;
  curByte: Byte;

  function checkSignature: Boolean;
  var
    buf: PByte;
    bufLen, bufLenLeft: Integer;
    curOp: POpHeader;
    opBufferPos: PByte;
  begin
    if epOnly then
    begin
      buf := entryPoint;
      bufLen := AImage.Mapper.FileSize - (SysInt(entryPoint) - SysInt(AImage.Mapper.Map));
      bufLenLeft := bufLen;
      curOp := opBuffer;
      opBufferPos := opBuffer;
      Inc(opBufferPos, SizeOf(curOp^));
      Result := True;
      while (bufLenLeft > 0) and (curOp.Op <> OP_STOP) do
      begin
        case curOp.Op of
          OP_COMPARE:
            begin
              Result := Result and CompareMem(buf, opBufferPos, curOp.ByteCount);
              Inc(buf, curOp.ByteCount);
              Dec(bufLenLeft, curOp.ByteCount);
              Inc(PByte(curOp), SizeOf(curOp^) + curOp.ByteCount);
            end;
          OP_SKIP:
            begin
              Inc(buf, curOp.ByteCount);
              Dec(bufLenLeft, curOp.ByteCount);
              Inc(curOp);
            end
          else
            Result := False;
        end;
        if not Result then
          Break;
      end;
    end
    else
    begin
      Result := False;
    end;
  end;

  function readByte: Boolean;
  begin
    // skip whitespace
    while (signLengthLeft > 0) and CharInSet(signPos^, [#0 .. ' ']) do
    begin
      Inc(signPos);
      Dec(signLengthLeft);
    end;

    if (signLengthLeft >= 2) then
    begin
      curByteAny := (signPos[0] = '?') and (signPos[1] = '?');
      if not curByteAny then
      begin
        curByte := hex2Byte(signPos);
      end;
      Inc(signPos, 2);
      Dec(signLengthLeft, 2);
      Result := True;
    end
    else
      Result := False;
  end;
  
  procedure compileSignature;
  var
    curOp: POpHeader;
    opBufferPos: PByte;
  begin
    curOp := opBuffer;
    curOp.Op := OP_STOP;
    curOp.ByteCount := 0;
    opBufferPos := opBuffer; // do not increment at startup
    signLengthLeft := signLength;
    signPos := sign;
    while readByte do
    begin
      if curByteAny then
      begin
        if (curOp.Op = OP_SKIP) then
        begin
          Inc(curOp.ByteCount);
        end
        else
        begin
          curOp := Pointer(opBufferPos);
          curOp.Op := OP_SKIP;
          curOp.ByteCount := 1;
          Inc(opBufferPos, SizeOf(curOp^));
        end;
      end
      else
      begin
        if (curOp.Op = OP_COMPARE) then
        begin
          Inc(curOp.ByteCount);
          opBufferPos^ := curByte;
          Inc(opBufferPos);
        end
        else
        begin
          curOp := Pointer(opBufferPos);
          curOp.Op := OP_COMPARE;
          curOp.ByteCount := 1;
          Inc(opBufferPos, SizeOf(curOp^));
          opBufferPos^ := curByte;
          Inc(opBufferPos);
        end;
      end;
    end;
    if (opBufferPos <> opBuffer) then
    begin
      curOp := Pointer(opBufferPos);
      curOp.Op := OP_STOP;
      curOp.ByteCount := 0;
    end;
  end;

  function checkCompiler: Boolean;
  begin
    Result := False;
    if compilerRed and signRed then
    begin
      compileSignature;
      signatureFound := checkSignature;
      Result := signatureFound;
    end;
    signRed := False;
    compilerRed := False;
    epOnlyRed := False;
  end;

  function readLine: Boolean;
  var
    p: PAnsiChar;
  begin
    lineLength := 0;
    p := curLine;
    // loop before line end
    while (not CharInSet(p^, [#13, #10])) and (sizeLeft >= 0) do
    begin
      Inc(p);
      Inc(lineLength);
      Dec(sizeLeft);
    end;
    nextLine := p;
    // move next line pointer to next line
    while CharInSet(nextLine^, [#13, #10]) and (sizeLeft >= 0) do
    begin
      Inc(nextLine);
      Dec(sizeLeft);
    end;
    Result := (lineLength > 0);
  end;

  procedure skipWhitespace(var AStr: PAnsiChar; var ALength: Integer);
  var
    p: PAnsiChar;
  begin
    while (ALength > 0) and CharInSet(AStr^, [#0 .. ' ']) do
    begin
      Inc(AStr);
      Dec(ALength);
    end;
    if (ALength > 0) then
    begin
      p := AStr;
      Inc(p, ALength - 1);
      while (ALength > 0) and CharInSet(p^, [#0 .. ' ']) do
      begin
        Dec(p);
        Dec(ALength);
      end;
    end;
  end;

  function findChar(AChar: AnsiChar): Integer;
  var
    p: PAnsiChar;
    l: Integer;
  begin
    p := curLine;
    l := lineLength;
    while (l > 0) and (p^ <> AChar) do
    begin
      Inc(p);
      Dec(l);
    end;
    if (l > 0) then
      Result := lineLength - l
    else
      Result := -1;
  end;

begin
  Result := Strings.UnknownCompiler;

  if (AImage.EntryPointExists) then
  begin
    signsFn := GluePath([ExtractFilePath(GetPluginFileName), 'userdb.txt']);
    if (FileExists(signsFn)) then
    begin
      dataSize := Min(GetFileSize(signsFn), 1024 * 1024 * 4); // Max 4MB of signs
      GetMem(data, dataSize);
      try
        entryPoint := Pointer(SysUInt(AImage.Mapper.Map) + AImage.GetEntryPointRAW(epSection));
        ReadBufferFromFile(signsFn, data, dataSize);
        GetMem(opBuffer, OP_BUFFER_SIZE);
        try
          sizeLeft := dataSize;
          curLine := data;
          signRed := False;
          compilerRed := False;
          epOnlyRed := False;
          compilerName := nil;
          compilerLength := 0;
          while readLine do
          begin
            skipWhitespace(curLine, lineLength);
            if (lineLength > 0) then
            begin
              if (curLine[0] = '[') and (curLine[lineLength - 1] = ']') then
              begin
                if CheckCompiler then
                  Break;
                compilerName := curLine;
                Inc(compilerName);
                compilerLength := lineLength - 2;
                compilerRed := True;
              end
              else
              begin
                eqPos := findChar('=');
                if (eqPos >= 0) then
                begin
                  lineName := curLine;
                  lineNameLength := eqPos;
                  lineValue := curLine;
                  Inc(lineValue, eqPos + 1);
                  lineValueLength := lineLength - eqPos - 1;
                  skipWhitespace(lineName, lineNameLength);
                  if (StrLIComp(lineName, 'ep_only', lineNameLength) = 0) then
                  begin
                    epOnly := not (StrLIComp(lineValue, 'false', lineValueLength) = 0);
                    epOnlyRed := True;
                  end
                  else if (StrLIComp(lineName, 'signature', lineNameLength) = 0) then
                  begin
                    sign := lineValue;
                    signLength := lineValueLength;
                    signRed := True;
                  end;
                end;
              end;
            end;
            curLine := nextLine;
          end;
          CheckCompiler;

          if (signatureFound) then
          begin
            SetLength(tmp, compilerLength);
            StrLCopy(PAnsiChar(tmp), compilerName, compilerLength);
            Result := string(tmp);
          end;
        finally
          FreeMem(opBuffer);
        end;
      finally
        FreeMem(data);
      end;
    end;
  end;
end;

procedure TInfoFrame.GetPropertiesList(AList: TStringList);
begin
  inherited;
  AList.Add('lvInfo.Columns[0].Width');
end;

procedure TInfoFrame.LoadData(const AFileName: string);

  function doubleStub(n: Int64): Double; inline;
  begin
    Result := n;
  end;

  procedure addStrings(AStrings: TStringList);
  var
    i: Integer;
    sName, sValue: string;
  li: TMyListItem;
  begin
    lvInfo.Items.BeginUpdate;
    try
      for i := 0 to AStrings.Count - 1 do
      begin
        sName := AStrings.Names[i];
        sValue := AStrings.ValueFromIndex[i];
        if (sValue <> '') or (sName = '') and (sValue = '') then
        begin
          li := lvInfo.Items.Add;
          li.Caption := sName;
          li.SubItems.Add(sValue);
        end;
      end;
    finally
      lvInfo.Items.EndUpdate;
    end;
  end;

var
  plugin: TPEViewer;
  image: TPEImage;
  stream: TFileStream;
  info: TStringList;
  secIdx: Integer;
  epOffset: SysUInt;
  fileCreated, fileModified: TFileTime;
begin
  info := nil;
  plugin := Pointer(PluginObject);
  image := plugin.PEImage;
  image.LoadImage;
  try
    image.LoadVersionInfoResource;
    lvInfo.Clear;
    stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      info := TStringList.Create;
      if (image.VersionResource <> nil) then
      begin
        stream.Position := image.VersionResource.DataRaw;
        LoadVersionInfoData(image.VersionResource, stream, info);
        info.Add('');
      end;
      GetFileTime(stream.Handle, @fileCreated, nil, @fileModified);
      info.Add(Strings.File_Modified + '=' + DateTimeToStr(FileTimeToDateTime(fileModified)));
      info.Add(Strings.File_Created + '=' + DateTimeToStr(FileTimeToDateTime(fileCreated)));
      info.Add(Strings.File_Size + '=' + Format(Strings.FFile_SizeFormat, [doubleStub(stream.Size)]));
      epOffset := image.GetEntryPointRAW(secIdx);
      if (epOffset > 0) then
      begin
        info.Add('');
        info.Add(Strings.Entry_Point_Raw_Offset + '=' + MyIntToHex(epOffset));
      end;

      tCompilerDeterminationInterval.Enabled := True;

      addStrings(info);
    finally
      stream.Free();
      if (image.Corrupted) then
        plugin.MainForm.SetErrorMsg(plugin.MainForm.Strings.ImageCorrupted);
    end;
  finally
    info.Free;
    image.UnloadImage;
  end;
end;

procedure TInfoFrame.LoadVersionInfoData(ANode: TResNode; AStream: TStream; AStrList: TStringList);
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
      S := Strings.GetString(WS) + '=';
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

      Hdr2 := Pointer(SysUInt(Hdr) + 6 + 9*2);
      Hdr2 := Round232(Hdr2);
      while (Hdr2.ValueLen <> 0) do begin
        ReadString(Hdr2);
        Hdr2 := Round232(Pointer(SysUInt(Hdr2) + Hdr2.Len));
      end;
    end;

  begin
    Inc(SysUInt(Ptr), 6 + 15*2);
    Ptr := Round232(Ptr);
    ReadStringTable(Ptr);
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

  var
    Hdr: PStructHdr;
    I: SysUInt;
  begin
    Hdr := Ptr;
    Inc(SysUInt(Ptr), 6 + 12*2);
    for I := 0 to (Hdr.Len - 6 - 12*2) div SizeOf(TTranslation) - 1 do
      Ptr := Pointer(SysUInt(Round232(Ptr)) + I * SizeOf(TTranslation));
  end;

var
  Buf, Ptr: Pointer;
  HDR: PStructHdr;
  FFI: PVSFixedFileInfo;
  WS: string;
  S: string;
  FV: PFileVersion;
begin
  GetMem(Buf, ANode.DataSize);
  try
    AStream.Read(Buf^, ANode.DataSize);
    Ptr := Buf;
    Hdr := Ptr;
    if (Hdr.Len > ANode.DataSize) then
      raise Exception.Create('Invalid resource');
    SetLength(WS, 15);
    Move(Hdr.FirstChar, WS[1], 15 * SizeOf(WideChar));
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
    AStrList.Add(Strings.Language + '=' + LangDesc(ANode.Lang));
    AStrList.Add(
      Strings.File_Version + '=' + IntToStr(FV.Major) + '.' + IntToStr(FV.Minor) + '.' +
      IntToStr(FV.Release) + '.' + IntToStr(FV.Build));
    FV := @FFI.dwProductVersionMS;
    AStrList.Add(
      Strings.Product_Version + '=' + IntToStr(FV.Major) + '.' + IntToStr(FV.Minor) + '.' +
      IntToStr(FV.Release) + '.' + IntToStr(FV.Build));
    AStrList.Add(Strings.File_OS + '=' + GetOSText(FFI.dwFileOS));
    AStrList.Add(Strings.File_Flags + '=' + GetFileFlagsText(FFI.dwFileFlags and FFI.dwFileFlagsMask));
    AStrList.Add(Strings.File_Type + '=' + GetFileTypeText(FFI.dwFileType));
    AStrList.Add(Strings.File_Subtype + '=' + GetFileSubTypeText(FFI.dwFileSubtype, FFI.dwFileType));
  finally
    FreeMem(Buf);
  end;
end;


{ TInfoStringStore }


constructor TInfoStringStore.Create;
begin
  inherited;
  File_SizeFormat := '%.0n bytes';
  UnknownCompiler := 'Unknown';
end;

procedure TInfoFrame.tCompilerDeterminationIntervalTimer(Sender: TObject);
begin
  tCompilerDeterminationInterval.Enabled := False;
  if TPEViewer(PluginObject).AutoDetermineCompiler then
    aDetermineCompiler.Execute;
end;

end.
