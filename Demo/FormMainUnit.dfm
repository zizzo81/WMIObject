object FormMain: TFormMain
  Left = 0
  Top = 0
  ActiveControl = cQuery
  Caption = 'WMIObject Demo'
  ClientHeight = 459
  ClientWidth = 884
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object pQuery: TPanel
    Left = 0
    Top = 0
    Width = 884
    Height = 94
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 4
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    OnResize = pQueryResize
    DesignSize = (
      884
      94)
    object lComputer: TLabel
      Left = 4
      Top = 4
      Width = 51
      Height = 13
      Caption = '&Computer:'
      FocusControl = eComputer
    end
    object lNamespace: TLabel
      Left = 192
      Top = 4
      Width = 59
      Height = 13
      Caption = '&Namespace:'
      FocusControl = cNamespace
    end
    object lUsername: TLabel
      Left = 343
      Top = 4
      Width = 52
      Height = 13
      Caption = '&Username:'
      FocusControl = eUsername
    end
    object lPassword: TLabel
      Left = 470
      Top = 4
      Width = 50
      Height = 13
      Caption = 'Pass&word:'
      FocusControl = ePassword
    end
    object lQuery: TLabel
      Left = 4
      Top = 50
      Width = 34
      Height = 13
      Caption = 'Qu&ery:'
      FocusControl = cQuery
    end
    object bExecute: TButton
      Left = 805
      Top = 4
      Width = 75
      Height = 86
      Anchors = [akTop, akRight]
      Caption = 'E&xecute'
      Default = True
      TabOrder = 5
      OnClick = bExecuteClick
    end
    object cQuery: TComboBox
      Left = 4
      Top = 69
      Width = 795
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      Text = 
        'SELECT Manufacturer, Model, Name, Product, PartNumber, SlotLayou' +
        't, SerialNumber, Version FROM Win32_BaseBoard'
      OnChange = cQueryChange
    end
    object eComputer: TEdit
      Left = 4
      Top = 23
      Width = 165
      Height = 21
      TabOrder = 0
      Text = '.'
    end
    object cNamespace: TComboBox
      Left = 192
      Top = 23
      Width = 145
      Height = 21
      TabOrder = 1
      Items.Strings = (
        'root\CIMV2'
        'root\WMI'
        'root\Virtualization'
        'root\Hardware')
    end
    object eUsername: TEdit
      Left = 343
      Top = 23
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object ePassword: TEdit
      Left = 470
      Top = 23
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object pResults: TPanel
    Left = 0
    Top = 94
    Width = 884
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'No query has been executed.'
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object dgResults: TDrawGrid
      Left = 4
      Top = 4
      Width = 876
      Height = 357
      Align = alClient
      BevelInner = bvNone
      BevelKind = bkFlat
      BorderStyle = bsNone
      DefaultRowHeight = 22
      FixedColor = clWhite
      FixedCols = 2
      FixedRows = 0
      Options = [goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
      TabOrder = 0
      Visible = False
      OnDblClick = dgResultsDblClick
      OnDrawCell = dgResultsDrawCell
      OnSelectCell = dgResultsSelectCell
    end
  end
end
