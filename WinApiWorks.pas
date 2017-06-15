unit WinApiWorks;

interface
uses
  Winapi.Windows, CommonUtilities;


function IsOnWow: Boolean;


implementation

type
  TIsWow64Process = function (hProcess: THandle; var IsWow64: BOOL): BOOL; stdcall;

var
  IsWow64Process: TIsWow64Process;
  IsWow64: Boolean;


function IsOnWow: Boolean;
begin
  Result := IsWow64;
end;


var
  lib: HMODULE;
  isWow: BOOL;
initialization
  lib := LoadLibrary(kernel32);
  try
    @IsWow64Process := GetProcAddress(lib, 'IsWow64Process');
    if (Assigned(IsWow64Process)) then
      if IsWow64Process(GetCurrentProcess, isWow) then
        IsWow64 := isWow;
  finally
    FreeLibrary(lib);
  end;
end.
