unit PEViewerClass;

interface
uses
  Winapi.Windows, Classes, SysUtils, MainFormUnit, ComCtrls, PEViewerFrameUnit,
  InfoFrameUnit, ImportExportFrameUnit, HeadersFrameUnit, Controls, IniFiles,
  TypInfo, PEUtils, ResourcesFrameUnit, Winapi.Messages, Graphics,
  CommonUtilities, Menus, System.Win.Registry, MyToolBarUnit;


type


  TPEViewerStringStore = class(TStringStore)
  private
    FCopy_Value: string;
    FCopy_Line: string;
    FSelect_All: string;
  published
    property Copy_Line: string read FCopy_Line write FCopy_Line;
    property Copy_Value: string read FCopy_Value write FCopy_Value;
    property Select_All: string read FSelect_All write FSelect_All;
  end;


  TCurrentTab = (ctInfo, ctImportExport, ctHeaders, ctResources);



  TPEViewer = class(TPersistent)
  private
    class var
      FTCPluginIniFileName: string;
      FOwnPluginIniFileName: string;
      FLocalizationDir: string;
  private
    FLoadedFrames: array[TCurrentTab] of TPEViewerFrame;
  private
    FMainForm: TMainForm;
    FCurrentTab: TCurrentTab;
    FFileName: string;
    FDefaultTab: TCurrentTab;
    FSaveDefaultTab: Boolean;
    FPublicConfig: TStringList;
    FPEImage: TPEImage;
    FDontUseTCIni: Boolean;
    FLocalization: string;
    FPublicLocalization: TStringList;
    FDontDetermineCompiler: Boolean;
    procedure SetCurrentTab(const Value: TCurrentTab);
    function GetFrame(AIndex: TCurrentTab): TPEViewerFrame;
    function GetCurrentFrame: TPEViewerFrame;
    function GetIniFileName: string;
    procedure SetDontUseTCIni(Value: Boolean);
    function GetDontUseTCIni: Boolean;
  protected
    procedure OnFormActivate(Sender: TObject);
    procedure OnFormShow(Sender: TObject);
    procedure OnFormMiLocalizationClick(Sender: TObject);
    procedure OnFormMiDontUsePluginIniClick(Sender: TObject);
    procedure OnFormMiAutoDetermineCompilerClick(Sender: TObject);
    procedure OnFormMiSaveLastOpenedTabClick(Sender: TObject);
    procedure UpdateTabToolbar;
    procedure UpdateCurrentTab;
    procedure GetPropertiesList(AList: TStringList);
    procedure LoadImageInfo;
    procedure ReadProperties(AList, APublicList: TStringList);
    procedure DoLoadConfig(const AIniFileName: string; AClearExistingConfig: Boolean);
    function GetRelativeFilePath(const AFileName: string): string;
    procedure CleanupConfig(AConfig: TStringList);
  public
    constructor Create(
      AParentWindow: HWND; const AFileToLoad: string; APluginFlags: Integer); virtual;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property CurrentTab: TCurrentTab read FCurrentTab write SetCurrentTab;
    property Frame[AIndex: TCurrentTab]: TPEViewerFrame read GetFrame;
    property CurrentFrame: TPEViewerFrame read GetCurrentFrame;
    property IniFileName: string read GetIniFileName;
    property PublicConfig: TStringList read FPublicConfig;
    property PublicLocalization: TStringList read FPublicLocalization;
    property PEImage: TPEImage read FPEImage;
    property DontUseTCPluginIni: Boolean read FDontUseTCIni write SetDontUseTCIni;

    procedure LoadConfig;
    procedure SaveConfig;
    procedure ApplyLocalization;
    procedure ApplyConfig;
    procedure UpdateConfig;
    procedure Show;
    procedure ShowNextTab;
    procedure ShowPreviousTab;
    class procedure SetTCIniFileName(const AFileName: string);
    function GetLocalizationDirPath: string;
    procedure InitializeMainForm;
  published
    property DefaultTab: TCurrentTab read FDefaultTab write FDefaultTab default ctInfo;
    property SaveDefaultTab: Boolean read FSaveDefaultTab write FSaveDefaultTab default True;
    property Localization: string read FLocalization write FLocalization;
    property AutoDetermineCompiler: Boolean read FDontDetermineCompiler write FDontDetermineCompiler;

    property MainForm: TMainForm read FMainForm;
  published
    Strings: TPEViewerStringStore;
  end;


const
  FramesMap: array[TCurrentTab] of TPEViewerFrameClass =
    (TInfoFrame, TImportExportFrame, THeadersFrame, TResourcesFrame);



implementation


const
  SPEViewerKey = 'Software\PEViewer';
  SDontUseTCIni = 'DontUseTCIni';
  SLangFileExt = '.lng2';
  SConfigFileName = 'PEViewer_config.ini';
  SPluginIniFileName = 'PEViewer.ini';
  SIniFileSectionName = 'PEViewer';


{ TPEViewer }


procedure TPEViewer.ApplyConfig;
begin
  CurrentTab := DefaultTab;
end;

procedure TPEViewer.ApplyLocalization;
var
  locFileName: string;
  locList: TStringList;
begin
  if (Localization <> '') then
  begin
    try
      locFileName := GluePath([GetLocalizationDirPath, Localization + SLangFileExt]);
      if (FileExists(locFileName)) then
      begin
        locList := TStringList.Create;
        try
          // Remove first line with lang name
          locList.LoadFromFile(locFileName);
          if (locList.Count > 0) then
          begin
            locList.Delete(0);
            PublicLocalization.Clear;
            CleanupConfig(locList);
            ReadProperties(locList, PublicLocalization);
          end;
        finally
          locList.Free;
        end;
      end;
    except
      on E: Exception do
        MainForm.SetErrorMsg(E.Message);
    end;
  end;
  InitializeMainForm;
end;

procedure TPEViewer.CleanupConfig(AConfig: TStringList);
var
  i: Integer;
  s: string;
begin
  for i := AConfig.Count - 1 to 0 do
  begin
    s := Trim(AConfig[i]);
    if (s = '') or (s[1] = '#') or ((Length(s) > 1) and (s[1] = '/') and (s[2] = '/')) then
      AConfig.Delete(i);
  end;
end;

constructor TPEViewer.Create(
  AParentWindow: HWND; const AFileToLoad: string; APluginFlags: Integer);
begin
  Strings := TPEViewerStringStore.Create;
  FPublicConfig := TStringList.Create;
  FPublicLocalization := TStringList.Create;

  FFileName := AFileToLoad;

  if (FOwnPluginIniFileName = '') then
  begin
    FOwnPluginIniFileName := GetRelativeFilePath(SPluginIniFileName);
  end;

  FMainForm := TMainForm.CreateParented(AParentWindow);
  MainForm.Plugin := Self;
  MainForm.tbbImEx.Tag := Ord(ctImportExport);
  MainForm.tbbHdr.Tag := Ord(ctHeaders);
  MainForm.tbbInfo.Tag := Ord(ctInfo);
  MainForm.tbbRes.Tag := Ord(ctResources);

  MainForm.OnActivate := OnFormActivate;

  LoadImageInfo;

  FDontUseTCIni := GetDontUseTCIni;
end;

destructor TPEViewer.Destroy;
begin
  FMainForm.Free;
  FPublicLocalization.Free;
  FPublicConfig.Free;
  Strings.Free;
  inherited;
end;

procedure TPEViewer.DoLoadConfig(const AIniFileName: string; AClearExistingConfig: Boolean);
var
  ini: TIniFile;
  settings, properties: TStringList;
  i: integer;
  pi: PPropInfo;
  propName: string;
begin
  if AClearExistingConfig then
    PublicConfig.Clear;
  try
    ini := TIniFile.Create(AIniFileName);
    try
      settings := TStringList.Create;
      try
        ini.ReadSection(SIniFileSectionName, settings);
        for i := settings.Count - 1 downto 0 do
        begin
          propName := settings[i];
          settings[i] := propName + '=' + ini.ReadString(SIniFileSectionName, propName, '');
        end;
        ReadProperties(settings, PublicConfig);

        // Set default values
        properties := TStringList.Create;
        try
          GetPropertiesList(properties);
          for i := 0 to properties.Count - 1 do
          begin
            if (settings.IndexOfName(properties[i]) < 0) then
            begin
              pi := GetPropInfo(Self, properties[i]);
              if (pi.PropType^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkString, tkUString]) then
                SetOrdProp(Self, pi, pi.Default);
            end;
          end;
        finally
          properties.Free;
        end;
      finally
        settings.Free;
      end;
    finally
      ini.Free;
    end;
  except
    //
  end;
end;

function TPEViewer.GetCurrentFrame: TPEViewerFrame;
begin
  Result := FLoadedFrames[FCurrentTab];
end;

function TPEViewer.GetDontUseTCIni: Boolean;
var
  r: TRegistry;
begin
  Result := False;
  try
    r := TRegistry.Create;
    try
      r.RootKey := HKEY_CURRENT_USER;
      if (r.KeyExists(SPEViewerKey)) then
      begin
        r.OpenKey(SPEViewerKey, False);
        if (r.ValueExists(SDontUseTCIni)) then
          Result := r.ReadBool(SDontUseTCIni);
      end;
    finally
      r.Free;
    end;
  except
  end;
end;

function TPEViewer.GetFrame(AIndex: TCurrentTab): TPEViewerFrame;
begin
  Result := FLoadedFrames[AIndex];
end;

function TPEViewer.GetIniFileName: string;
begin
  if (DontUseTCPluginIni) or (FTCPluginIniFileName = '') then
    Result := FOwnPluginIniFileName
  else
    Result := FTCPluginIniFileName;
end;

function TPEViewer.GetLocalizationDirPath: string;
begin
  Result := GetRelativeFilePath(FLocalizationDir);
end;

procedure TPEViewer.GetPropertiesList(AList: TStringList);
var
  pl: PPropList;
  i, count: Integer;
begin
  count := GetPropList(Self, pl);
  try
    for i := 0 to count - 1 do
    begin
      AList.Add(string(pl[i].Name));
    end;
  finally
    FreeMem(pl);
  end;
end;

function TPEViewer.GetRelativeFilePath(const AFileName: string): string;
begin
  Result := GluePath([ExtractFilePath(GetPluginFileName), AFileName]);
end;

procedure TPEViewer.InitializeMainForm;
var
  dir, fn, s, locName: string;
  i: Integer;
  mi: TMyMenuItem;
  files: TStringArray;
  sl: TStringList;
begin
  try
    MainForm.Initialize;

    MainForm.miDontUseTCPluginIni.Checked := DontUseTCPluginIni;
    MainForm.miDontUseTCPluginIni.OnClick := OnFormMiDontUsePluginIniClick;
    MainForm.miAutoDetermineCompiler.Checked := AutoDetermineCompiler;
    MainForm.miAutoDetermineCompiler.OnClick := OnFormMiAutoDetermineCompilerClick;
    MainForm.miSaveLastOpenedTab.Checked := SaveDefaultTab;
    MainForm.miSaveLastOpenedTab.OnClick := OnFormMiSaveLastOpenedTabClick;

    MainForm.miLocalization.Clear;

    mi := TMyMenuItem.Create(nil);
    mi.Caption := 'Default';
    mi.GroupIndex := 1;
    mi.RadioItem := True;
    mi.AutoCheck := True;
    mi.LocalizationName := '';
    mi.OnClick := OnFormMiLocalizationClick;
    MainForm.miLocalization.Add(mi);
    mi.Checked := True;
    dir := GetLocalizationDirPath;
    files := GetFileList(dir, '*.lng2', False);
    sl := TStringList.Create;
    try
      for i := 0 to Length(files) - 1 do
      begin
        fn := GluePath([dir, files[i]]);
        sl.LoadFromFile(fn);
        if (sl.Count > 0) then
        begin
          s := sl[0];
          locName := ChangeFileExt(files[i], '');
          mi := TMyMenuItem.Create(nil);
          mi.Caption := Trim(s);
          mi.GroupIndex := 1;
          mi.RadioItem := True;
//          mi.AutoCheck := True;
          mi.LocalizationName := locName;
          mi.OnClick := OnFormMiLocalizationClick;
          MainForm.miLocalization.Add(mi);
          if AnsiSameText(locName, Localization) then
            mi.Checked := True;
        end;
      end;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      MainForm.SetErrorMsg(E.Message);
  end;
end;

procedure TPEViewer.LoadConfig;
var
  s: string;
  sl: TStringList;
begin
  if (DontUseTCPluginIni) then
    s := FOwnPluginIniFileName
  else
    s := IniFileName;
  DoLoadConfig(s, True);
  s := GetRelativeFilePath(SConfigFileName);
  if (FileExists(s)) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(s);
      if (sl.Count > 0) then
      begin
        CleanupConfig(sl);
        ReadProperties(sl, PublicConfig);
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TPEViewer.LoadImageInfo;
begin
  try
    FPEImage := TPEImage.CreateImage(FFileName);

    PEImage.LoadImage;
    try
      if PEImage.ImageTypeSupported then
        MainForm.SetImgInfo(PEImage.ImageTypeText, False)
      else
        MainForm.SetImgInfo(PEImage.ImageTypeText, True);
    finally
      PEImage.UnloadImage;
    end;
  except
    on E: Exception do
    begin
      MainForm.SetImgInfo(E.Message, True);
    end;
  end;
end;

procedure TPEViewer.OnFormActivate(Sender: TObject);
var
  f: TPEViewerFrame;
  c: TWinControl;
begin
  f := CurrentFrame;
  if (f <> nil) then
  begin
    if (f.ActiveControl <> nil) then
      f.ActiveControl.SetFocus
    else
    begin
      c := f.FindFirstFocusableControl;
      if (c <> nil) then
        c.SetFocus;
    end;
  end;
end;

procedure TPEViewer.OnFormMiAutoDetermineCompilerClick(Sender: TObject);
begin
  AutoDetermineCompiler := not AutoDetermineCompiler;
  TMenuItem(Sender).Checked := not AutoDetermineCompiler;
end;

procedure TPEViewer.OnFormMiDontUsePluginIniClick(Sender: TObject);
begin
  DontUseTCPluginIni := not DontUseTCPluginIni;
  TMenuItem(Sender).Checked := DontUseTCPluginIni;
end;

procedure TPEViewer.OnFormMiLocalizationClick(Sender: TObject);
begin
  Localization := TMyMenuItem(Sender).LocalizationName;
  TMenuItem(Sender).Checked := True;
//  ApplyLocalization;
end;

procedure TPEViewer.OnFormMiSaveLastOpenedTabClick(Sender: TObject);
begin
  SaveDefaultTab := not SaveDefaultTab;
  TMenuItem(Sender).Checked := SaveDefaultTab;
end;

procedure TPEViewer.OnFormShow(Sender: TObject);
var
  frame: TPEViewerFrame;
begin
  frame := CurrentFrame;
  if (frame <> nil) then
  begin
    frame.DoOnShow;
  end;
end;

procedure TPEViewer.ReadProperties(AList, APublicList: TStringList);
var
  i: Integer;
  pi: PPropInfo;
  propName, propValue, mainFormPrefix, stringsPrefix: string;
begin
  mainFormPrefix := MainForm.Name + '.';
  stringsPrefix := 'Strings.';
  for i := AList.Count - 1 downto 0 do
  begin
    propName := Trim(AList.Names[i]);
    propValue := Trim(AList.ValueFromIndex[i]);
    if (Pos('.', propName) <= 0) then
    begin
      pi := GetPropInfo(Self, propName);
      if
        (pi <> nil) and (pi.SetProc <> nil) and
        (pi.PropType^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkString, tkUString])
      then
      begin
        try
          SetPropValue(Self, pi, propValue);
        except
          // On error delete prop from list and it will be assigned to default later
          AList.Delete(i);
        end;
      end;
    end
    else if AnsiSameText(Copy(propName, 1, Length(mainFormPrefix)), mainFormPrefix) then
    begin
      RecurseSetProperty(Self, propName, propValue);
    end
    else if AnsiSameText(Copy(propName, 1, Length(stringsPrefix)), stringsPrefix) then
    begin
      RecurseSetProperty(Self, propName, propValue);
    end
    else
    begin
      APublicList.Add(Alist[i]);
    end;
  end;
end;

procedure TPEViewer.SaveConfig;
var
  pl: PPropList;
  i, count: Integer;
  settings: TStringList;
  ini: TIniFile;
  pi: PPropInfo;
  s: string;
begin
  count := GetPropList(Self, pl);
  try
    settings := TStringList.Create;
    try
      for i := 0 to count - 1 do
      begin
        pi := pl^[i];
        if
          (pi <> nil) and (pi.GetProc <> nil) and
          (pi.PropType^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkString, tkUString])
        then
        begin
          s := GetPropValue(Self, pi, True);
          // To save empty values in list
          if (s = '') then
            s := #0;
          settings.Values[string(pi.Name)] := s;
        end;
      end;
      settings.AddStrings(PublicConfig);

      ini := TIniFile.Create(IniFileName);
      try
        for i := 0 to settings.Count - 1 do
        begin
          s := settings.ValueFromIndex[i];
          if (s = #0) then
            s := '';
          ini.WriteString(SIniFileSectionName, settings.Names[i], s);
        end;
      finally
        ini.Free;
      end;
    finally
      settings.Free;
    end;
  finally
    FreeMem(pl);
  end;
end;

procedure TPEViewer.SetCurrentTab(const Value: TCurrentTab);
begin
  FCurrentTab := Value;
  UpdateCurrentTab;
  UpdateTabToolbar;
end;

procedure TPEViewer.SetDontUseTCIni(Value: Boolean);
var
  r: TRegistry;
begin
  if (FDontUseTCIni <> Value) then
  begin
    FDontUseTCIni := Value;
    try
      r := TRegistry.Create;
      try
        r.RootKey := HKEY_CURRENT_USER;
        r.OpenKey(SPEViewerKey, True);
        r.WriteBool(SDontUseTCIni, FDontUseTCIni);
        r.CloseKey;
      finally
        r.Free;
      end;
    except
    end;
  end;
end;

class procedure TPEViewer.SetTCIniFileName(const AFileName: string);
begin
  FTCPluginIniFileName := AFileName;
end;

procedure TPEViewer.Show;
begin
  MainForm.Show;
end;

procedure TPEViewer.ShowNextTab;
var
  n: TCurrentTab;
begin
  n := CurrentTab;
  Inc(n);
  if (n > High(n)) then
    n := Low(n);
  CurrentTab := n;
end;

procedure TPEViewer.ShowPreviousTab;
var
  n: TCurrentTab;
begin
  n := CurrentTab;
  if (n = Low(n)) then
    n := High(n)
  else
    Dec(n);
  CurrentTab := n;
end;

procedure TPEViewer.UpdateConfig;
var
  n: TCurrentTab;
begin
  if (SaveDefaultTab) then
    DefaultTab := CurrentTab;
  for n := Low(n) to High(n) do
  begin
    if (FLoadedFrames[n] <> nil) then
      FLoadedFrames[n].SaveConfig;
  end;
end;

procedure TPEViewer.UpdateCurrentTab;
var
  newFrameClass: TPEViewerFrameClass;
  newFrame: TPEViewerFrame;
  n: TCurrentTab;
  c, p: TWinControl;
  isPluginFocused: Boolean;
begin
  // Find control frame
  c := MainForm.ActiveControl;
  if (c <> nil) then
  begin
    p := c;
    repeat
      p := p.Parent;
    until (p = nil) or (p is TPEViewerFrame);
    // Set Active Control to remember last activated control on tab
    if (p <> nil) then
      TPEViewerFrame(p).ActiveControl := c;
  end;

  // Check if our plugin has focus
  c := FindControl(GetFocus);
  p := nil;
  if (c <> nil) then
  begin
    p := c;
    repeat
      p := p.Parent;
    until (p = nil) or (p = MainForm);
  end;
  isPluginFocused := (p <> nil);

  newFrameClass := FramesMap[CurrentTab];
  for n := Low(n) to High(n) do
  begin
    if ((n <> CurrentTab) and (FLoadedFrames[n] <> nil)) then
      FLoadedFrames[n].Hide;
  end;

  if (newFrameClass <> nil) then
  begin
    newFrame := FLoadedFrames[CurrentTab];
    if (newFrame = nil) then
    begin
      newFrame := newFrameClass.Create(MainForm.pParent);
      FLoadedFrames[CurrentTab] := newFrame;
      newFrame.Parent := MainForm.pParent;
      newFrame.PluginObject := Self;
      newFrame.DoLoadConfig;
      newFrame.ApplyLocalization;
      newFrame.DoLoadData(FileName);
      newFrame.Width := MainForm.ClientWidth;
      newFrame.Height := MainForm.ClientHeight;
      newFrame.Realign;
    end;
    newFrame.Show;
  end
  else
    newFrame := nil;

  if (newFrame <> nil) and isPluginFocused then
  begin
    if (newFrame.ActiveControl <> nil) then
      newFrame.ActiveControl.SetFocus
    else
    begin
      c := newFrame.FindFirstFocusableControl;
      if (c <> nil) then
        c.SetFocus;
    end;
  end;
end;

procedure TPEViewer.UpdateTabToolbar;
var
  i: Integer;
  b: TMyToolButton;
begin
  for i := 0 to MainForm.tbMain.ButtonCount - 1 do
  begin
    b := MainForm.tbMain.Buttons[i];
    if (b <> MainForm.tbbSettings) then
      b.Down := (TCurrentTab(b.Tag) = CurrentTab);
  end;
end;

initialization
  TPEViewer.FLocalizationDir := 'Lang';

finalization
  TPEViewer.FTCPluginIniFileName := '';
  TPEViewer.FOwnPluginIniFileName := '';
  TPEViewer.FLocalizationDir := '';
end.

