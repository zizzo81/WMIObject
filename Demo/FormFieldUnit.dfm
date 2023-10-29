object FormField: TFormField
  Left = 0
  Top = 0
  ActiveControl = eValue
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Value detail'
  ClientHeight = 163
  ClientWidth = 413
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 13
  object lField: TLabel
    Left = 0
    Top = 0
    Width = 26
    Height = 13
    Caption = 'Field:'
    FocusControl = eField
  end
  object lIndex: TLabel
    Left = 372
    Top = 0
    Width = 32
    Height = 13
    Caption = 'Index:'
    FocusControl = eIndex
  end
  object lValue: TLabel
    Left = 0
    Top = 92
    Width = 30
    Height = 13
    Caption = 'Value:'
    FocusControl = eValue
  end
  object lType: TLabel
    Left = 0
    Top = 46
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object eField: TEdit
    Left = 0
    Top = 19
    Width = 366
    Height = 21
    TabStop = False
    BevelInner = bvSpace
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    ReadOnly = True
    TabOrder = 0
  end
  object eIndex: TEdit
    Left = 372
    Top = 19
    Width = 41
    Height = 21
    TabStop = False
    Alignment = taCenter
    BevelInner = bvSpace
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    ReadOnly = True
    TabOrder = 1
    Text = '0'
  end
  object eValue: TEdit
    Left = 0
    Top = 111
    Width = 413
    Height = 21
    BevelInner = bvSpace
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    ReadOnly = True
    TabOrder = 2
  end
  object bClose: TButton
    Left = 338
    Top = 138
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'C&l&ose'
    Default = True
    ModalResult = 8
    TabOrder = 3
  end
  object eType: TEdit
    Left = 0
    Top = 65
    Width = 413
    Height = 21
    TabStop = False
    BevelInner = bvSpace
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    ReadOnly = True
    TabOrder = 4
  end
end
