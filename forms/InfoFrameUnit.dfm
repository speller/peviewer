inherited InfoFrame: TInfoFrame
  object lvInfo: TMyListView [0]
    Left = 0
    Top = 0
    Width = 451
    Height = 304
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Name'
        Width = 150
        SortOrder = soNone
      end
      item
        AutoSize = True
        Caption = 'Value'
        SortOrder = soNone
      end>
    ColumnClick = False
    DoubleBuffered = True
    GridLines = True
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  inherited alActions: TActionList
    object aDetermineCompiler: TAction [2]
      Caption = 'Determine Compiler'
      OnExecute = aDetermineCompilerExecute
    end
  end
  inherited pmListView: TPopupMenu
    object N1: TMenuItem
      Caption = '-'
    end
    object DetermineCompiler1: TMenuItem
      Action = aDetermineCompiler
    end
  end
  object tCompilerDeterminationInterval: TTimer
    Enabled = False
    Interval = 300
    OnTimer = tCompilerDeterminationIntervalTimer
    Left = 152
    Top = 8
  end
end
