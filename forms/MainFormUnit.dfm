object MainForm: TMainForm
  Left = 288
  Top = 199
  Caption = 'MainForm'
  ClientHeight = 471
  ClientWidth = 725
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pParent: TPanel
    Left = 0
    Top = 0
    Width = 725
    Height = 471
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object tbMain: TMyToolBar
      Left = 0
      Top = 0
      Width = 725
      Height = 27
      AutoSize = True
      BorderWidth = 1
      ButtonHeight = 19
      ButtonWidth = 98
      DoubleBuffered = True
      EdgeBorders = [ebBottom]
      Flat = False
      List = True
      ParentDoubleBuffered = False
      AllowTextButtons = True
      TabOrder = 0
      Wrapable = False
      object ToolButton1: TMyToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbbInfo: TMyToolButton
        Left = 8
        Top = 0
        AutoSize = True
        Caption = 'Info'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = tbMainButtonClick
      end
      object ToolButton2: TMyToolButton
        Left = 44
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbbImEx: TMyToolButton
        Left = 52
        Top = 0
        AutoSize = True
        Caption = 'Import, export'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = tbMainButtonClick
      end
      object ToolButton3: TMyToolButton
        Left = 139
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbbHdr: TMyToolButton
        Left = 147
        Top = 0
        AutoSize = True
        Caption = 'Headers, sections'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = tbMainButtonClick
      end
      object ToolButton4: TMyToolButton
        Left = 249
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbbRes: TMyToolButton
        Left = 257
        Top = 0
        AutoSize = True
        Caption = 'Resources'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = tbMainButtonClick
      end
      object ToolButton5: TMyToolButton
        Left = 323
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbbSettings: TMyToolButton
        Left = 331
        Top = 0
        Caption = 'Settings  v'
        DropdownMenu = pmSettings
        ImageIndex = 0
        Style = tbsTextButton
      end
      object tbSep7: TMyToolButton
        Left = 398
        Top = 0
        Width = 8
        Caption = 'tbSep7'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object lblImgInfo: TLabel
        Left = 411
        Top = 3
        Width = 48
        Height = 13
        Caption = 'lblImgInfo'
        Transparent = True
      end
      object lblErrorMsg: TLabel
        Left = 505
        Top = 3
        Width = 3
        Height = 13
        Transparent = True
      end
    end
  end
  object pmSettings: TPopupMenu
    Left = 332
    Top = 52
    object miLocalization: TMenuItem
      Caption = 'Localization (needs restart)'
      object TMenuItem
      end
    end
    object miDontUseTCPluginIni: TMenuItem
      Caption = 'Do not use lsplugin.ini'
    end
    object miAutoDetermineCompiler: TMenuItem
      Caption = 'Determine Compiler Automatically'
    end
    object miSaveLastOpenedTab: TMenuItem
      Caption = 'Save last opened tab'
    end
  end
end
