object Form1: TForm1
  Left = 227
  Top = 137
  Width = 729
  Height = 535
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 88
    Top = 8
    Width = 625
    Height = 325
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
  end
  object Label1: TLabel
    Left = 4
    Top = 216
    Width = 25
    Height = 13
    Caption = 'Linie:'
  end
  object PaintBox1: TPaintBox
    Left = 88
    Top = 8
    Width = 625
    Height = 325
    Color = clBtnFace
    ParentColor = False
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 77
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 36
    Width = 77
    Height = 21
    TabOrder = 1
    Text = 'localhost'
  end
  object Edit2: TEdit
    Left = 8
    Top = 60
    Width = 77
    Height = 21
    TabOrder = 2
    Text = '2350'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 84
    Width = 77
    Height = 17
    Caption = 'Server'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object Memo1: TMemo
    Left = 88
    Top = 336
    Width = 625
    Height = 157
    TabOrder = 4
    OnKeyDown = Memo1KeyDown
    OnKeyPress = Memo1KeyPress
    OnMouseMove = Memo1MouseMove
  end
  object ColorBox1: TColorBox
    Left = 4
    Top = 256
    Width = 81
    Height = 22
    ItemHeight = 16
    TabOrder = 5
  end
  object SpinEdit1: TSpinEdit
    Left = 4
    Top = 232
    Width = 81
    Height = 22
    MaxValue = 100
    MinValue = 1
    TabOrder = 6
    Value = 1
  end
  object Button2: TButton
    Left = 8
    Top = 468
    Width = 75
    Height = 25
    Caption = 'Form2'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 4
    Top = 104
    Width = 81
    Height = 105
    BevelOuter = bvNone
    TabOrder = 8
    object SB_Linie: TSpeedButton
      Left = 16
      Top = 32
      Width = 23
      Height = 22
      GroupIndex = 1
      Down = True
      Caption = 'L'
    end
    object SB_Kreis: TSpeedButton
      Left = 40
      Top = 32
      Width = 23
      Height = 22
      GroupIndex = 1
      Caption = 'K'
    end
    object SB_Rechteck: TSpeedButton
      Left = 16
      Top = 56
      Width = 23
      Height = 22
      GroupIndex = 1
      Caption = 'R'
    end
    object SB_Text: TSpeedButton
      Left = 40
      Top = 56
      Width = 23
      Height = 22
      GroupIndex = 1
      Caption = 'T'
    end
  end
  object Button3: TButton
    Left = 8
    Top = 324
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 9
    OnClick = Button3Click
  end
  object XPManifest1: TXPManifest
    Left = 808
    Top = 4
  end
  object SimpleServerComponent1: TSimpleServerComponent
    OnClientConnect = SimpleServerComponent1ClientConnect
    Left = 144
    Top = 80
  end
  object LineMerge: TMergeSocketComponent
    OnNewPacket_ReadThread = LineMergeNewPacket_ReadThread
    Left = 144
    Top = 144
  end
  object TextMerge: TMergeSocketComponent
    OnNewPacket_ReadThread = TextMergeNewPacket_ReadThread
    Left = 144
    Top = 176
  end
  object EllipseMerge: TMergeSocketComponent
    OnNewPacket_ReadThread = EllipseMergeNewPacket_ReadThread
    Left = 176
    Top = 144
  end
  object RechteckMerge: TMergeSocketComponent
    OnNewPacket_ReadThread = RechteckMergeNewPacket_ReadThread
    Left = 176
    Top = 176
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 436
  end
  object SocketMultiplexList1: TSocketMultiplexListComponent
    OnSocketOpenedTunnel = SocketMultiplexList1SocketOpenedTunnel
    Left = 288
    Top = 80
  end
end
