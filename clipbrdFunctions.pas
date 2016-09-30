unit clipbrdFunctions;

interface

uses
  Classes, clipbrd, windows, SysUtils;

type
{$ifndef UNICODE}
  RawByteString = AnsiString;
{$endif}
  TClipboardEx = class(TClipboard)
  public
    procedure SetBuffer(Format: Word; var Buffer; Size: Integer);
    procedure AddToCurrent(Format: Word; Handle: Cardinal);
  end;
  PClipbrdCopyFormat = ^TClipbrdCopyFormat;
  TClipbrdCopyFormat = record
    ID: Word;
    Name: AnsiString;
    Size: Cardinal;
    Data: pointer;
  end;
  TClipbrdCopy = class
  private
    FFormatList: TList;
    FFilename: String;
    function AddFormat(ID: Word; Name: AnsiString;
      var Data; Size: Cardinal): Integer;
    procedure DeleteFormat(nr: Integer);
    procedure Warning(Text: String);
  public
    property Filename: string read FFilename;
    function LoadFromStream(AStream: TStream): Boolean;
    procedure SaveToStream(AStream: TStream);
    procedure ReadClipbrd;
    procedure WriteClipbrd(useStoredIDs: boolean);
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

procedure SaveClipboardtoFile(Filename, Description, PlugInName,
  ogame_domain: String; UserUniName: String);
function ReadClipboardHtml: string;
function ReadClipboardHtmlUTF8: AnsiString;
function GetHtmlFormat: Integer;
function getClipboardTextANSI: AnsiString;
function GetClipboardTextUTF8: RawByteString;
function GetClipboardTextWide: WideString;
function GetClipBoardText: String;
function ReadClipboardText: string;
function FindClipbrdFormat(name: String): Integer;

implementation

function conv_UTF8(const s: AnsiString): string;
begin
{$ifdef UNICODE}
  Result := UTF8ToWideString(s);
{$else}
  Result := s;
{$endif}
end;

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

function GetClipboardText: string;
begin
{$ifdef UNICODE}
  Result := GetClipBoardTextWide;
{$else}
  Result := GetClipBoardTextUTF8;
{$endif}
end;

function getClipboardTextANSI: AnsiString;
var Data: THandle;
begin
  Data := Clipboard.GetAsHandle(CF_TEXT);
  try
    if Data <> 0 then
      Result := PAnsiChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
  end;
end;

function GetClipboardTextUTF8: RawByteString;
begin
  if (Clipboard.HasFormat(CF_UNICODETEXT)) then
  begin
    Result := UTF8Encode(GetClipboardTextWide);
  end
  else
  begin
    Result := UTF8Encode(RawByteString(getClipboardTextANSI));
  end;
end;

function GetClipboardTextWide: WideString;
var Data: THandle;
begin
  Data := Clipboard.GetAsHandle(CF_UNICODETEXT);
  try
    if Data <> 0 then
      Result := PWideChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
  end;
end;

function ReadClipboardText: string;
begin
  Result := GetClipboardText;
end;

function ReadClipboardHtmlString(CutFormatHeader: Boolean = False): AnsiString;
var
  MyHandle: THandle;
  TextPtr: PAnsiChar;
  CF_HTML: Word;
begin
  CF_HTML := GetHtmlFormat;
  Result := '';
  ClipBoard.Open;
  try
    MyHandle := Clipboard.GetAsHandle(CF_HTML);
    TextPtr := GlobalLock(MyHandle);
    Result := TextPtr;
    GlobalUnlock(MyHandle);
  finally
    Clipboard.Close;
  end;
end;

function ReadClipboardHtmlUTF8: AnsiString;
var
  MyHandle: THandle;
  ptr: pointer;
  size: integer;
  CF_HTML: Word;
  sig: Word;
begin
  Result := '';

  CF_HTML := GetHtmlFormat;
  ClipBoard.Open;
  try
    if Clipboard.HasFormat(CF_HTML) then
    begin
      MyHandle := Clipboard.GetAsHandle(CF_HTML);
      size := GlobalSize(MyHandle);
      ptr := GlobalLock(MyHandle);
      if size > sizeof(sig) then
        Move(ptr^,sig,sizeof(sig))
      else sig := 0;

      case sig of
      $feff: //WideString
        begin
          Result := UTF8Encode(PWideChar(ptr));
        end;
      else //UTF8
        Result := PAnsiChar(ptr);
      end;
      GlobalUnlock(MyHandle);
    end;
  finally
    Clipboard.Close;
  end;
end;

function ReadClipboardHtml: String;
begin
{$ifdef UNICODE}
  Result := UTF8ToWideString(ReadClipboardHtmlUTF8);
{$else}
  Result := ReadClipboardHtmlUTF8;
{$endif}
end;

procedure SaveClipboardtoFile(Filename, Description, PlugInName, ogame_domain: String;
  UserUniName: String);

  procedure WriteStringToStream(s: AnsiString; stream: TStream); overload;
  var i: integer; {4Byte ~ 2^31 Zeichen}
  begin
    i := length(s);
    stream.WriteBuffer(i,sizeof(i));
    stream.WriteBuffer(PAnsiChar(s)^,i);
  end;

  procedure WriteStringToStream(s: WideString; stream: TStream); overload;
  begin
    WriteStringToStream(UTF8Encode((s)), stream);
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
  WriteStringToStream(Description,stream);
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

function TClipbrdCopy.AddFormat(ID: Word; Name: AnsiString;
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

  function ReadStringFromStream(stream: TStream): AnsiString;
  var i: integer;
  begin
    stream.ReadBuffer(i,SizeOf(i));
    SetLength(Result,i);
    stream.ReadBuffer(PAnsiChar(Result)^,i);
  end;

var ID: Word;
    Name: AnsiString;
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
{$ifdef UNICODE}
    s: array[byte] of WideChar;
{$else}
    s: array[byte] of AnsiChar;
{$endif}
    hndl: THandle;
    data: pointer;
    format_name: AnsiString;
begin
  Clear;
  ClipBoard.Open;
  try
    for i := 0 to Clipboard.FormatCount-1 do
    begin
      ID := Clipboard.Formats[i];
      FillChar(s,sizeof(s),0);

{$ifdef UNICODE}
      GetClipboardFormatName(ID,@s,sizeof(s));
      format_name := UTF8Encode(s);
{$else}
      GetClipboardFormatName(ID,@s,sizeof(s));
      format_name := s;
{$endif}

      hndl := Clipboard.GetAsHandle(ID);
      size := GlobalSize(hndl);
      data := GlobalLock(hndl);

      AddFormat(ID,format_name,data^,size);

      GlobalUnlock(hndl);
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TClipbrdCopy.SaveToStream(AStream: TStream);

  procedure WriteStringToStream(s: AnsiString; stream: TStream);
  var i: integer; {4Byte ~ 2^31 Zeichen}
  begin
    i := length(s);
    stream.WriteBuffer(i,sizeof(i));
    stream.WriteBuffer(PAnsiChar(s)^,i);
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

procedure TClipbrdCopy.WriteClipbrd(useStoredIDs: boolean);
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
{$ifdef UNICODE}
      nID := RegisterClipboardFormat(PWideChar(UTF8ToWideString(Name)));
{$else}
      nID := RegisterClipboardFormat(PChar(Name));
{$endif}
      if (nID <> 0)and(nID <> ID) then
      begin
        Warning('New ID of ' + conv_UTF8(Name) + ': ' + IntToStr(nID) +
          '. The old was: ' + IntToStr(ID) + '.');

        if not useStoredIDs then
        begin
          ID := nID;
        end;
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
