object fMain: TfMain
  Left = 171
  Top = 111
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'OXml demo: FileList'
  ClientHeight = 621
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 56
    Top = 400
    Width = 313
    Height = 2
  end
  object Bevel1: TBevel
    Left = 56
    Top = 184
    Width = 313
    Height = 2
  end
  object Label2: TLabel
    Left = 56
    Top = 136
    Width = 171
    Height = 13
    Caption = 'Name of the &file with XML document'
    FocusControl = eFileName
  end
  object Label10: TLabel
    Left = 8
    Top = 176
    Width = 44
    Height = 19
    Caption = 'Save '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 56
    Top = 96
    Width = 94
    Height = 13
    Caption = 'Select &drive to scan'
    FocusControl = cobDrive
  end
  object Label9: TLabel
    Left = 56
    Top = 200
    Width = 125
    Height = 13
    Caption = 'Use &encoding (code page)'
  end
  object Label11: TLabel
    Left = 8
    Top = 392
    Width = 44
    Height = 19
    Caption = 'Load '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 197
    Height = 19
    Caption = 'How to use OXml library'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object bvl1: TBevel
    Left = 56
    Top = 584
    Width = 313
    Height = 2
  end
  object Lbl1: TLabel
    Left = 8
    Top = 576
    Width = 41
    Height = 19
    Caption = 'Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LblTime: TLabel
    Left = 64
    Top = 597
    Width = 37
    Height = 13
    Caption = 'Elapsed'
  end
  object eFileName: TEdit
    Left = 56
    Top = 152
    Width = 241
    Height = 21
    TabOrder = 2
  end
  object cobDrive: TDriveComboBox
    Left = 56
    Top = 112
    Width = 241
    Height = 19
    TabOrder = 1
    OnChange = cobDriveChange
  end
  object cobCodePage: TComboBox
    Left = 56
    Top = 216
    Width = 241
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object bDriveSaveDOM: TButton
    Left = 67
    Top = 324
    Width = 214
    Height = 25
    Caption = '&Scan, create and save document (DOM)'
    TabOrder = 5
    OnClick = bDriveSaveDOMClick
  end
  object bDriveLoadDOM: TButton
    Left = 64
    Top = 440
    Width = 137
    Height = 25
    Caption = '&Load to TreeView (DOM)'
    TabOrder = 7
    OnClick = bDriveLoadDOMClick
  end
  object tvDrive: TTreeView
    Left = 57
    Top = 476
    Width = 240
    Height = 85
    ChangeDelay = 50
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 8
  end
  object mDescription: TMemo
    Left = 8
    Top = 32
    Width = 361
    Height = 57
    TabStop = False
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'This demo shows how to create XML document, scan selected drive ' +
        'and '
      
        'fill XML document with hierarchical list of files. Optionally, y' +
        'ou can also '
      'change encoding of the document.'
      '(The demo has been ported from OmniXML.)')
    ReadOnly = True
    TabOrder = 0
  end
  object rgOutputFormat: TRadioGroup
    Left = 56
    Top = 248
    Width = 241
    Height = 65
    Caption = ' XML style format '
    ItemIndex = 2
    Items.Strings = (
      '&none (no formatting)'
      '&flat (CRLF before new tags)'
      '&indent (create hierarchy document)')
    TabOrder = 4
  end
  object chbPreserveWhiteSpace: TCheckBox
    Left = 72
    Top = 416
    Width = 209
    Height = 17
    Caption = 'preserve white space'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object bDriveSaveDirect: TButton
    Left = 67
    Top = 356
    Width = 214
    Height = 25
    Caption = '&Scan, create and save document (direct)'
    TabOrder = 9
    OnClick = bDriveSaveDirectClick
  end
  object bDriveLoadSeq: TButton
    Left = 208
    Top = 440
    Width = 161
    Height = 25
    Caption = '&Load to TreeView (Sequential)'
    TabOrder = 10
    OnClick = bDriveLoadSeqClick
  end
end
