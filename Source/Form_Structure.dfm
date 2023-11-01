object FormStructure: TFormStructure
  Left = 0
  Top = 0
  Caption = '3DS File Structure'
  ClientHeight = 567
  ClientWidth = 838
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
  DesignSize = (
    838
    567)
  PixelsPerInch = 96
  TextHeight = 13
  object TreeChunks: TTreeView
    Left = 8
    Top = 8
    Width = 313
    Height = 551
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnClick = TreeChunksClick
  end
  object MemoDetail: TMemo
    Left = 327
    Top = 8
    Width = 503
    Height = 551
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
