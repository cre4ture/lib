object Form2: TForm2
  Left = 575
  Top = 202
  Width = 653
  Height = 445
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VST_Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 645
    Height = 344
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnGetText = VST_TreeGetText
    OnGetNodeDataSize = VST_TreeGetNodeDataSize
    Columns = <
      item
        Position = 0
        Width = 400
        WideText = 'Object'
      end
      item
        Position = 1
        Width = 150
        WideText = 'Address'
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 645
    Height = 67
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 60
      Height = 13
      Caption = 'HexAdresse:'
    end
    object Label2: TLabel
      Left = 148
      Top = 8
      Width = 489
      Height = 53
      AutoSize = False
      Caption = 'Label2'
      WordWrap = True
    end
    object Edit1: TEdit
      Left = 4
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object Button1: TButton
      Left = 4
      Top = 44
      Width = 121
      Height = 17
      Caption = 'check'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 140
    Top = 56
  end
  object PopupMenu1: TPopupMenu
    Left = 36
    Top = 68
    object SocketMultiplex1: TMenuItem
      Caption = 'SocketMultiplex'
      object Close1: TMenuItem
        Caption = 'Close;'
        OnClick = Close1Click
      end
      object Free1: TMenuItem
        Caption = 'Free;'
        OnClick = Free1Click
      end
    end
  end
end
