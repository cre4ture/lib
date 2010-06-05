unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ExtCtrls, MergeSocket, MultiplexList, Menus,
  SplitSocket, StdCtrls;

type
  TNodeData = record
    Obj: TObject;
    Name: String;
  end;
  TForm2 = class(TForm)
    VST_Tree: TVirtualStringTree;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    SocketMultiplex1: TMenuItem;
    Close1: TMenuItem;
    Free1: TMenuItem;
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure VST_TreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VST_TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure Timer1Timer(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Free1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    procedure ListTMergeSocket(RNode: PVirtualNode);
    procedure ListTSocketMultiplexList(RNode: PVirtualNode);
    procedure ListTSocketMultiplex(RNode: PVirtualNode);
    { Private-Deklarationen }
  public
    procedure AddObjectToWatch(AObj: TObject; AName: String);
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.AddObjectToWatch(AObj: TObject; AName: String);
begin
  with TNodeData(VST_Tree.GetNodeData(VST_Tree.AddChild(nil))^) do
  begin
    Obj := AObj;
    Name := AName;
  end;
end;

procedure TForm2.VST_TreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := Sizeof(TNodeData);
end;

procedure TForm2.VST_TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  with TNodeData(VST_Tree.GetNodeData(Node)^)do
  begin
    case Column of
    0: CellText := Name;
    1: CellText := '$' + IntToHex(Cardinal(Obj),6);
    else
      CellText := '';
    end;
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var RNode: PVirtualNode;
begin
  VST_Tree.BeginUpdate;
  RNode := VST_Tree.GetFirst;
  while RNode <> nil do
  with TNodeData(VST_Tree.GetNodeData(RNode)^) do
  begin

    if Obj is TMergeSocket then ListTMergeSocket(RNode);
    if Obj is TSocketMultiplexList then ListTSocketMultiplexList(RNode);

    VST_Tree.Expanded[RNode] := True;
    RNode := VST_Tree.GetNextSibling(RNode);
  end;
  VST_Tree.EndUpdate;
end;

procedure TForm2.ListTMergeSocket(RNode: PVirtualNode);
var CNode, dnode: PVirtualNode;
    i: integer;
begin
  with TNodeData(VST_Tree.GetNodeData(RNode)^) do
  if Obj is TMergeSocket then
  with TMergeSocket(Obj) do
  begin
    LockList;
    try
      CNode := VST_Tree.GetFirstChild(RNode);
      i := 0;
      repeat
        if i < Count then
        begin
          if CNode = nil then
            CNode := VST_Tree.AddChild(RNode);

          with TNodeData(VST_Tree.GetNodeData(CNode)^) do
          begin
            Obj := Sockets[i];
            Name := 'SplitSocket[' + IntToStr(i) + '] Host: $' +
                    IntToHex(Cardinal(TSplitSocket(Obj).HostSocket),6) +
                    ' CNR: ' + IntToStr(TSplitSocket(Obj).WorkProcessIndex);
          end;

          CNode := VST_Tree.GetNextSibling(CNode);
        end else if CNode <> nil then
                begin
                  dnode := CNode;
                  CNode := VST_Tree.GetNextSibling(CNode);
                  VST_Tree.DeleteNode(dnode);
                end;

        inc(i);

      until (CNode = nil)and(i >= Count);
    finally
      UnlockList;
    end;
  end;
end;

procedure TForm2.ListTSocketMultiplexList(RNode: PVirtualNode);
var CNode, dnode: PVirtualNode;
    i: integer;
begin
  with TNodeData(VST_Tree.GetNodeData(RNode)^) do
  if Obj is TSocketMultiplexList then
  with TSocketMultiplexList(Obj) do
  begin
    CNode := VST_Tree.GetFirstChild(RNode);
    try
      i := 0;
      repeat
        if i < Count then
        begin
          if CNode = nil then
            CNode := VST_Tree.AddChild(RNode);

          with TNodeData(VST_Tree.GetNodeData(CNode)^) do
          begin
            Obj := Sockets[i];
            Name := 'SocketMultiplex[' + IntToStr(i) + ']';
            ListTSocketMultiplex(CNode);
          end;

          CNode := VST_Tree.GetNextSibling(CNode);
        end else if CNode <> nil then
                begin
                  dnode := CNode;
                  CNode := VST_Tree.GetNextSibling(CNode);
                  VST_Tree.DeleteNode(dnode);
                end;

        inc(i);

      until (CNode = nil)and(i >= Count);
    finally

    end;
  end;
end;

procedure TForm2.Close1Click(Sender: TObject);
begin
  With TNodeData(VST_Tree.GetNodeData(VST_Tree.GetFirstSelected)^) do
  begin
    TSocketMultiplex(Obj).Close;
  end;
end;

procedure TForm2.Free1Click(Sender: TObject);
begin
  With TNodeData(VST_Tree.GetNodeData(VST_Tree.GetFirstSelected)^) do
  begin
    TSocketMultiplex(Obj).Free;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var p: Pointer;
    obj: TObject;
    s: string;
begin
  p := pointer(StrToInt(Edit1.Text));
  obj := TObject(p);
  s := '';
  try
    if obj is TSplitSocket then
      s := s + 'obj is'
    else s := s + 'obj is not';
    s := s + ' a TSplitSocket' + #10 + #13;
    if obj is TSocketMultiplex then
      s := s + 'obj is'
    else s := s + 'obj is not';
    s := s + ' a TSocketMultiplex' + #10 + #13;
    if obj is TObject then
      s := s + 'obj is'
    else s := s + 'obj is not';
    s := s + ' a TObject' + #10 + #13;
  except
    s := s + ' !EXCEPTION!';
  end;
  Label2.Caption := s;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm2.FormHide(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm2.ListTSocketMultiplex(RNode: PVirtualNode);
var CNode, dnode: PVirtualNode;
    i: integer;
begin
  with TNodeData(VST_Tree.GetNodeData(RNode)^) do
  if Obj is TSocketMultiplex then
  with TSocketMultiplex(Obj) do
  begin
    CNode := VST_Tree.GetFirstChild(RNode);
    i := 0;
    with ____SplitSocketList.LockList do
    try
      repeat
        if i < Count then
        begin
          if CNode = nil then
            CNode := VST_Tree.AddChild(RNode);

          with TNodeData(VST_Tree.GetNodeData(CNode)^) do
          begin
            Obj := TObject(Items[i]);
            Name := 'SplitSocket[' + IntToStr(i) + '] CNR: ' +
                    IntToStr(TSplitSocket(Obj).WorkProcessIndex);
          end;

          CNode := VST_Tree.GetNextSibling(CNode);
        end else if CNode <> nil then
                begin
                  dnode := CNode;
                  CNode := VST_Tree.GetNextSibling(CNode);
                  VST_Tree.DeleteNode(dnode);
                end;

        inc(i);

      until (CNode = nil)and(i >= Count);
    finally
      ____SplitSocketList.UnlockList;
    end;
  end;
end;

end.
