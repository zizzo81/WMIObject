object FormArray: TFormArray
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 4
  Caption = 'Array of %s'
  ClientHeight = 283
  ClientWidth = 550
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object dwArray: TDrawGrid
    Left = 0
    Top = 0
    Width = 550
    Height = 283
    Align = alClient
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    ColCount = 2
    DefaultRowHeight = 22
    FixedColor = clWhite
    Options = [goFixedHorzLine, goHorzLine, goRowSelect, goThumbTracking]
    TabOrder = 0
    OnDblClick = dwArrayDblClick
    OnDrawCell = dwArrayDrawCell
  end
end
