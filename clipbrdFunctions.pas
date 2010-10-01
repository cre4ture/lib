unit clipbrdFunctions;

interface

uses
  Classes, clipbrd, windows, SysUtils;

type
  TClipboardEx = class(TClipboard)
  public
    procedure SetBuffer(Format: Word; var Buffer; Size: Integer);
    procedure AddToCurrent(Format: Word; Handle: Cardinal);
  end;
  PClipbrdCopyFormat = ^TClipbrdCopyFormat;
  TClipbrdCopyFormat = record
    ID: Word;
    Name: String;
    Size: Cardinal;
    Data: pointer;
  end;
  TClipbrdCopy = class
  private
    FFormatList: TList;
    FFilename: String;
    function AddFormat(ID: Word; Name: String;
      var Data; Size: Cardinal): Integer;
    procedure DeleteFormat(nr: Integer);
    procedure Warning(Text: String);
  public
    property Filename: string read FFilename;
    function LoadFromStream(AStream: TStream): Boolean;
    procedure SaveToStream(AStream: TStream);
    procedure ReadClipbrd;
    procedure WriteClipbrd;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

procedure SaveClipboardtoFile(Filename, PluginFilename, PlugInName,
  ogame_domain: String; UserUniName: String);
function ReadClipboardHtml: String;
function ReadClipboardHtml_Ex: WideString;
function GetHtmlFormat: Integer;
function GetClipboardText: String;
function ReadClipboardText: string;
function FindClipbrdFormat(name: String): Integer;

implementation

function FindClipbrdFormat(name: String): Integer;
var i: integer;
    s: array[0..255] of Char;
begin
  i := 0;
  Result := 0;
  while Clipboard.Formats[i] <> 0 do
  begin
    GetClipboardFormatName(Clipboard.Formats[i],@s,256);
    if pos(name,s) > 0 then
    begin
      Result := Clipboard.Formats[i];
      Break;
    end;
    inc(i);
  end;
end;

function GetHtmlFormat: Integer;
begin
  Result := FindClipbrdFormat('HTML');
end;

function GetClipboardText: String;
var Data: THandle;
begin
  Data := Clipboard.GetAsHandle(CF_TEXT);
  try
    if Data <> 0 then
      Result := PChar(GlobalLock(Data)) else
      Result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
  end;
end;

function ReadClipboardText: string;
begin
  Result := GetClipboardText;
end;

function ReadClipboardHtmlString(CutFormatHeader: Boolean = False): String;
var
  MyHandle: THandle;
  TextPtr: PChar;
  CF_HTML: Word;
begin
  CF_HTML := GetHtmlFormat;
  Result := '';
  ClipBoard.Open;
  try
    MyHandle := Clipboard.GetAsHandle(CF_HTML);
    TextPtr := GlobalLock(MyHandle);
    Result := Utf8ToAnsi(TextPtr);
    GlobalUnlock(MyHandle);
  finally
    Clipboard.Close;
  end;
end;

function ReadClipboardHtml_Ex: WideString;
var
  MyHandle: THandle;
  ptr: pointer;
  size: integer;
  CF_HTML: Word;
  sig: Word;
  UTF8: UTF8string;
begin
  CF_HTML := GetHtmlFormat;
  ClipBoard.Open;
  try
    MyHandle := Clipboard.GetAsHandle(CF_HTML);
    size := GlobalSize(MyHandle);
    ptr := GlobalLock(MyHandle);
    if size > sizeof(sig) then
      Move(ptr^,sig,sizeof(sig))
    else sig := 0;

    case sig of
    $feff: //WideString
      begin
        SetLength(Result,size);
        CopyMemory(PWideChar(Result),ptr,size);
      end;
    else //UTF8
      SetLength(UTF8,size);
      CopyMemory(PChar(UTF8),ptr,size);
      Result := UTF8Decode(UTF8);
    end;
    GlobalUnlock(MyHandle);
  finally
    Clipboard.Close;
  end;
end;

function ReadClipboardHtmlWideString: Widestring;
var
  MyHandle: THandle;
  ptr: pointer;
  size: integer;
  CF_HTML: Word;
begin
  CF_HTML := GetHtmlFormat;
  ClipBoard.Open;
  try
    MyHandle := Clipboard.GetAsHandle(CF_HTML);
    size := GlobalSize(MyHandle);
    ptr := GlobalLock(MyHandle);
    SetLength(Result,size);
    CopyMemory(PWideChar(Result),ptr,size);
    GlobalUnlock(MyHandle);
  finally
    Clipboard.Close;
  end;
end;

function ReadClipboardHtml: String;
begin
  Result := ReadClipboardHtml_Ex;
  {if widehtml then
    Result := ReadClipboardHtmlWideString()
  else
  begin
    Result := ReadClipboardHtmlString();
    if (length(Result) > 2)and(Result[1] = #$FF)and(Result[2] = #$FE) then
      Result := ReadClipboardHtmlWideString;
  end; }
end;


procedure SaveClipboardtoFile(Filename, PluginFilename, PlugInName, ogame_domain: String;
  UserUniName: String);

  procedure WriteStringToStream(s: string; stream: TStream);
  var i: integer; {4Byte ~ 2^31 Zeichen}
  begin
    i := length(s);
    stream.WriteBuffer(i,sizeof(i));
    stream.WriteBuffer(PChar(s)^,i);
  end;

  procedure WriteClipboardFormatToStream(Format: Word; stream: TStream);
  var MyHandle: THandle;
      size: integer;
  begin
    Clipboard.Open;
    try
      MyHandle := Clipboard.GetAsHandle(Format);
      size := GlobalSize(MyHandle);
      stream.Write(size,sizeof(size));
      stream.Write(GlobalLock(MyHandle)^,size);
      GlobalUnlock(MyHandle);
    finally
      Clipboard.Close;
    end;
  end;

var stream: TFileStream;
    cc: TClipbrdCopy;
begin
  stream := TFileStream.Create(filename,fmCreate);

  WriteStringToStream('cS - clipbrdfile',stream);
  WriteStringToStream(GetClipboardText,stream);
  WriteStringToStream(ReadClipboardHtml,stream);
  WriteStringToStream(PluginFilename,stream);
  WriteStringToStream(PlugInName,stream);
  WriteStringToStream(ogame_domain,stream);
  WriteStringToStream(UserUniName,stream);
  WriteStringToStream('ClipbrdCopy',stream);

  cc := TClipbrdCopy.Create;
  cc.ReadClipbrd;
  cc.SaveToStream(stream);
  cc.Free;

  stream.Free;
end;

function TClipbrdCopy.AddFormat(ID: Word; Name: String;
  var Data; Size: Cardinal): Integer;
var Format: PClipbrdCopyFormat;
begin
  New(Format);
  Format^.ID := ID;
  Format^.Name := Name;
  Format^.Size := Size;
  GetMem(Format^.Data,Size);
  CopyMemory(Format^.Data,@Data,Size);
  Result := FFormatList.Add(Format);
end;

procedure TClipbrdCopy.Clear;
begin
  while FFormatList.Count > 0 do
    DeleteFormat(0);
end;

constructor TClipbrdCopy.Create;
begin
  inherited;
  FFormatList := TList.Create;
end;

procedure TClipbrdCopy.DeleteFormat(nr: Integer);
var Format: PClipbrdCopyFormat;
begin
  Format := FFormatList[nr];
  FreeMem(Format^.Data);
  Dispose(Format);
  FFormatList.Delete(nr);
end;

destructor TClipbrdCopy.Destroy;
begin
  Clear;
  FFormatList.Free;
  inherited;
end;

function TClipbrdCopy.LoadFromStream(AStream: TStream): Boolean;

  function ReadStringFromStream(stream: TStream): String;
  var i: integer;
  begin
    stream.ReadBuffer(i,SizeOf(i));
    SetLength(Result,i);
    stream.ReadBuffer(PChar(Result)^,i);
  end;

var ID: Word;
    Name: String;
    Size: Cardinal;
    Data: pointer;
begin
  Clear;
  while (AStream.Position < AStream.Size) do
  begin
    AStream.ReadBuffer(ID,SizeOf(ID));
    Name := ReadStringFromStream(AStream);
    AStream.ReadBuffer(Size,SizeOf(Size));
    GetMem(Data,Size);
    AStream.ReadBuffer(Data^,Size);
    AddFormat(ID,Name,Data^,Size);
    FreeMem(Data);
  end;
  Result := True;
end;

procedure TClipbrdCopy.ReadClipbrd;
var i, ID, size: integer;
    s: array[byte] of Char;
    hndl: THandle;
    data: pointer;
begin
  Clear;
  ClipBoard.Open;
  try
    for i := 0 to Clipboard.FormatCount-1 do
    begin
      ID := Clipboard.Formats[i];
      FillChar(s,sizeof(s),0);
      GetClipboardFormatName(ID,@s,sizeof(s));

      hndl := Clipboard.GetAsHandle(ID);
      size := GlobalSize(hndl);
      data := GlobalLock(hndl);

      AddFormat(ID,s,data^,size);

      GlobalUnlock(hndl);
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TClipbrdCopy.SaveToStream(AStream: TStream);

  procedure WriteStringToStream(s: string; stream: TStream);
  var i: integer; {4Byte ~ 2^31 Zeichen}
  begin
    i := length(s);
    stream.WriteBuffer(i,sizeof(i));
    stream.WriteBuffer(PChar(s)^,i);
  end;

var
  i: integer;
begin
  for i := 0 to FFormatList.Count-1 do
    with TClipbrdCopyFormat(FFormatList[i]^) do
    begin
      AStream.WriteBuffer(ID,SizeOf(ID));
      WriteStringToStream(Name,AStream);
      AStream.WriteBuffer(Size,SizeOf(Size));
      AStream.WriteBuffer(Data^,Size);
    end;
end;

procedure TClipbrdCopy.Warning(Text: String);
begin

end;

procedure TClipbrdCopy.WriteClipbrd;
var i, nID: integer;
    clpb: TClipboardEx;
begin
  clpb := TClipboardEx.Create;
  clpb.Open;
  try
    clpb.Clear;
    for i := 0 to FFormatList.Count-1 do
    with TClipbrdCopyFormat(FFormatList[i]^) do
    begin
      nID := RegisterClipboardFormat(PChar(Name));
      if (nID <> 0)and(nID <> ID) then
      begin
        Warning('New ID of ' + Name + ': ' + IntToStr(nID) +
          '. The old was: ' + IntToStr(ID) + '.');
        ID := nID;
      end;
      clpb.SetBuffer(ID,Data^,Size);
    end;
  finally
    clpb.Close;
  end;
end;

procedure TClipboardEx.AddToCurrent(Format: Word; Handle: Cardinal);
begin
  Open;
  try
    SetClipboardData(Format,Handle);
  finally
    Close;
  end;
end;

procedure TClipboardEx.SetBuffer(Format: Word; var Buffer; Size: Integer);
begin
  inherited;
end;

end.
