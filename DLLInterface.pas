unit DLLInterface;

interface
uses
  Winapi.Windows, SysUtils, PEViewerClass, CommonUtilities, Vcl.Forms;

type
  TListDefaultParamStruct = record
    Size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: LongInt;
    DefaultIniName: array[0..MAX_PATH - 1] of AnsiChar;
  end;
  PListDefaultParamStruct = ^TListDefaultParamStruct;

{$IFNDEF WIN64}
function ListLoad(AParentWnd: HWND; AFileToLoad: PAnsiChar; AShowFlags: Integer):HWND; stdcall;
{$ENDIF}
function ListLoadW(AParentWnd: HWND; AFileToLoad: PWideChar; AShowFlags: Integer):HWND; stdcall;
procedure ListCloseWindow(AListWnd: HWND); stdcall;
procedure ListGetDetectString(ADetectString: PAnsiChar; AMaxLen: Integer); stdcall;
procedure ListSetDefaultParams(DPS: PListDefaultParamStruct); stdcall;


const
  SMainWindowRefProp = 'PEVIEWERREF';


implementation


{$IFNDEF WIN64}
function ListLoad(AParentWnd: HWND; AFileToLoad: PAnsiChar; AShowFlags: Integer): HWND;
var
  s: string;
begin
  s := string(AnsiString(AFileToLoad));
  Result := ListLoadW(AParentWnd, PChar(s), AShowFlags);
end;
{$ENDIF}


function ListLoadW(AParentWnd: HWND; AFileToLoad: PWideChar; AShowFlags: Integer): HWND;
var
  plugin: TPEViewer;
begin
  plugin := nil;
  try
    plugin := TPEViewer.Create(AParentWnd, AFileToLoad, AShowFlags);
    plugin.LoadConfig;
    plugin.ApplyLocalization;
    plugin.ApplyConfig;
    plugin.Show;
    Result := Plugin.MainForm.Handle;
    SetProp(Result, SMainWindowRefProp, Cardinal(plugin));
  except
    on E: Exception do begin
      Result := 0;
      ErrorBox('SErrorLoadingPlugin' + #13#10 + E.Message);
      try
        plugin.Free;
      except
      end;
    end;
  end;
end;

procedure ListCloseWindow(AListWnd: HWND); stdcall;
var
  plugin: TPEViewer;
begin
  try
    plugin := Pointer(GetProp(AListWnd, SMainWindowRefProp));
    if (plugin = nil) then
      ErrorBox('SInvalidPluginWindow')
    else
    begin
      RemoveProp(AListWnd, SMainWindowRefProp);
      plugin.UpdateConfig;
      plugin.SaveConfig;
      plugin.Free;
      Screen.FocusedForm := nil;
    end;
  except
    on E: Exception do
      ErrorBox('SErrorClosingPlugin' + #13#10 + E.Message);
  end;
end;


procedure ListGetDetectString(ADetectString: PAnsiChar; AMaxLen: Integer); stdcall;
begin
  StrCopy(
    ADetectString,
    'EXT = "EXE" | EXT = "DLL" | EXT = "OCX" | EXT = "SYS" | EXT = "WLX" | EXT = "WFX" | ' +
    'EXT = "WDX" | EXT = "WCX" | EXT = "WLX64" | EXT = "WFX64" | EXT = "WDX64" | EXT = "WCX64" | ' +
    'EXT = "AX" | EXT = "CPL" | EXT = "SO" | EXT = "SCR" | EXT = "MUI" | EXT = "ACM" | EXT = "BPL"');
end;


procedure ListSetDefaultParams(DPS: PListDefaultParamStruct); stdcall;
var
  s: string;
begin
  s := string(DPS.DefaultIniName);
  if (s <> '') then
  begin
    TPEViewer.SetTCIniFileName(s);
  end;
end;



end.
