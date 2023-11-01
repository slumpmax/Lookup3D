object FormXML: TFormXML
  Left = 0
  Top = 0
  Caption = 'DAE File Structure'
  ClientHeight = 516
  ClientWidth = 746
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TreeXML: TTreeView
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 209
    Height = 500
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnClick = TreeXMLClick
    ExplicitTop = 29
    ExplicitHeight = 459
  end
  object EditorXML: TValueListEditor
    AlignWithMargins = True
    Left = 225
    Top = 8
    Width = 513
    Height = 500
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 1
    ExplicitTop = 29
    ExplicitHeight = 459
    ColWidths = (
      132
      375)
  end
end
