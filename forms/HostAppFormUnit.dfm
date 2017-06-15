object HostAppForm: THostAppForm
  Left = 245
  Top = 94
  Caption = 'HostAppForm'
  ClientHeight = 525
  ClientWidth = 963
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 124
    Top = 60
    object Plugin1: TMenuItem
      Caption = 'Plugin'
      object ListLoad1: TMenuItem
        Caption = 'ListLoad'
        OnClick = ListLoad1Click
      end
      object ListUnload1: TMenuItem
        Caption = 'ListUnload'
        OnClick = ListUnload1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SetFocus1: TMenuItem
        Caption = 'SetFocus'
        OnClick = SetFocus1Click
      end
    end
  end
end
