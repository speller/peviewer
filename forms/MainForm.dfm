object frmMain: TfrmMain
  Left = 288
  Top = 199
  Caption = 'frmMain'
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
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object tbMain: TToolBar
    Left = 0
    Top = 0
    Width = 725
    Height = 25
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 19
    ButtonWidth = 98
    Caption = 'tbMain'
    DoubleBuffered = False
    EdgeBorders = [ebBottom]
    List = True
    ParentDoubleBuffered = False
    AllowTextButtons = True
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 20
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbbInfo: TToolButton
      Left = 8
      Top = 0
      AutoSize = True
      Caption = 'Info'
      Grouped = True
      Style = tbsTextButton
      OnClick = tbMainButtonClick
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbbImEx: TToolButton
      Left = 47
      Top = 0
      AutoSize = True
      Caption = 'Import, export'
      Grouped = True
      Style = tbsTextButton
      OnClick = tbMainButtonClick
    end
    object ToolButton3: TToolButton
      Left = 129
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbbHdr: TToolButton
      Left = 137
      Top = 0
      AutoSize = True
      Caption = 'Headers, sections'
      Grouped = True
      Style = tbsTextButton
      OnClick = tbMainButtonClick
    end
    object ToolButton4: TToolButton
      Left = 234
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbbRes: TToolButton
      Left = 242
      Top = 0
      AutoSize = True
      Caption = 'Resources'
      Grouped = True
      Style = tbsTextButton
      OnClick = tbMainButtonClick
    end
    object ToolButton5: TToolButton
      Left = 303
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 0
      Style = tbsSeparator
    end
  end
end
