object Form1: TForm1
  Left = 192
  Top = 107
  Width = 613
  Height = 417
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 605
    Height = 390
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object OMultiPanel1: TOMultiPanel
        Left = 0
        Top = 0
        Width = 597
        Height = 362
        PanelCollection = <
          item
            Control = OMultiPanel2
            Position = 0.333333333333333300
            Visible = True
            Index = 0
          end
          item
            Control = Memo2
            Position = 1.000000000000000000
            Visible = True
            Index = 1
          end>
        MinPosition = 0.020000000000000000
        Align = alClient
        TabOrder = 0
        DesignSize = (
          597
          362)
        object Memo2: TMemo
          Left = 202
          Top = 0
          Width = 395
          Height = 362
          Anchors = []
          Lines.Strings = (
            'See http://www.kluug.net for my other delphi projects.')
          TabOrder = 1
        end
        object OMultiPanel2: TOMultiPanel
          Left = 0
          Top = 0
          Width = 199
          Height = 362
          PanelType = ptVertical
          PanelCollection = <
            item
              Control = Memo1
              Position = 0.500000000000000000
              Visible = True
              Index = 0
            end
            item
              Control = StringGrid1
              Position = 1.000000000000000000
              Visible = True
              Index = 1
            end>
          MinPosition = 0.020000000000000000
          Anchors = []
          TabOrder = 0
          DesignSize = (
            199
            362)
          object Memo1: TMemo
            Left = 0
            Top = 0
            Width = 199
            Height = 181
            Anchors = []
            Lines.Strings = (
              'Memo1')
            TabOrder = 0
          end
          object StringGrid1: TStringGrid
            Left = 0
            Top = 184
            Width = 199
            Height = 178
            Anchors = []
            TabOrder = 1
          end
        end
      end
    end
  end
end
