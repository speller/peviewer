unit HostAppFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IniFiles, Vcl.Menus;

type
  THostAppForm = class(TForm)
    MainMenu1: TMainMenu;
    Plugin1: TMenuItem;
    ListLoad1: TMenuItem;
    ListUnload1: TMenuItem;
    N1: TMenuItem;
    SetFocus1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListLoad1Click(Sender: TObject);
    procedure ListUnload1Click(Sender: TObject);
    procedure SetFocus1Click(Sender: TObject);
  private
    FPlugin: THandle;
    FPluginWindow: HWND;
    FIniFileName: string;
    FPluginFileName: string;
    FFileName: string;
    procedure CallLoad;
    procedure CallUnload;
    procedure LoadModule;
    procedure UnloadModule;
  public
    { Public declarations }
  end;

var
  HostAppForm: THostAppForm;

implementation

{$R *.dfm}

type
  TListLoad = function( ParentWin: THandle; FileToLoad: PWideChar; ShowFlags: Integer): THandle; stdcall;
  TListClose = procedure( Wnd: HWND ); stdcall;


{ THostAppForm }

procedure THostAppForm.CallLoad;
var
  F: TListLoad;
begin
  @F := GetProcAddress(FPlugin, 'ListLoadW');
  if (FFileName <> '') and (@F <> nil) then
  begin
    HandleNeeded;
    FPluginWindow := F(Handle, PChar(FFileName), 0);
  end;
end;

procedure THostAppForm.CallUnload;
var
  FC: TListClose;
begin
  @FC := GetProcAddress(FPlugin, 'ListCloseWindow');
  if (@FC <> nil) then
  begin
    FC(FPluginWindow);
    FPluginWindow := 0;
  end;
end;

procedure THostAppForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CallUnload;
  UnloadModule;
end;

procedure THostAppForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  curIdx: Integer;
begin
  FIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  ini := TIniFile.Create(FIniFileName);
  try
    FPluginFileName := ini.ReadString('Settings', 'Plugin', '');
    if (FPluginFileName = '') then
      raise Exception.Create('Plugin not specified!');
    curIdx := ini.ReadInteger('Files', 'CurIndex', 0);
    FFileName := ini.ReadString('Files', 'File' + IntToStr(curIdx), '');
    if (FPluginFileName = '') then
      raise Exception.Create('File not specified!');
  finally
    ini.Free;
  end;
  LoadModule;
  CallLoad;
end;

procedure THostAppForm.FormResize(Sender: TObject);
begin
  SetWindowPos(FPluginWindow, 0, 0, 0, ClientWidth, ClientHeight, 0);
end;


procedure THostAppForm.ListLoad1Click(Sender: TObject);
begin
  CallLoad;
end;

procedure THostAppForm.ListUnload1Click(Sender: TObject);
begin
  CallUnload;
end;

procedure THostAppForm.LoadModule;
begin
  FPlugin := LoadLibrary(PChar(FPluginFileName));
  if (FPlugin = 0) then
    RaiseLastOSError;
end;

procedure THostAppForm.SetFocus1Click(Sender: TObject);
begin
  if (FPluginWindow <> 0) then
    SendMessage(FPluginWindow, WM_SETFOCUS, GetFocus, 0);
end;

procedure THostAppForm.UnloadModule;
begin
  FreeLibrary(FPlugin);
  FPlugin := 0;
end;

end.
