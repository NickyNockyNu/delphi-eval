object EvalForm: TEvalForm
  Left = 0
  Top = 0
  ActiveControl = ExpressionEdit
  AlphaBlend = True
  AlphaBlendValue = 245
  BorderStyle = bsSizeToolWin
  Caption = 'Evaluator'
  ClientHeight = 230
  ClientWidth = 360
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  GlassFrame.Left = -1
  GlassFrame.Top = -1
  GlassFrame.Right = -1
  GlassFrame.Bottom = -1
  GlassFrame.SheetOfGlass = True
  KeyPreview = True
  OldCreateOrder = False
  ScreenSnap = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    360
    230)
  PixelsPerInch = 96
  TextHeight = 13
  object ExpressionLabel: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = '&Expression:'
    FocusControl = ExpressionEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 58
    Width = 34
    Height = 13
    Caption = '&Result:'
  end
  object ExpressionEdit: TComboBox
    Left = 8
    Top = 27
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Type an expression here'
    ExplicitWidth = 354
  end
  object OutputMemo: TMemo
    Left = 8
    Top = 77
    Width = 353
    Height = 110
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 354
    ExplicitHeight = 118
  end
  object EvalButton: TButton
    Left = 8
    Top = 197
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'E&valuate'
    Default = True
    TabOrder = 2
    OnClick = EvalButtonClick
    ExplicitTop = 178
  end
  object ClearButton: TButton
    Left = 89
    Top = 197
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = ClearButtonClick
    ExplicitTop = 178
  end
end
