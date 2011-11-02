unit ClipboardViewerForm;

interface
                                                         
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Clipbrd, clipbrdfunctions;

type
  TClipboardViewer = class(TForm)
  private
    FCVActive: Boolean;
    FNextClipboardViewer: HWND;
    procedure WMChangeCBChain(var Msg : TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg : TWMDrawClipboard); message WM_DRAWCLIPBOARD;
  protected
    procedure SetCVActive(B: Boolean); virtual;
  public
    OnClipboardContentChanged: TNotifyEvent;
    property CVActive: Boolean read FCVActive write SetCVActive;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stopp;
    function GetClipboardText: String;
    function OpenClipboard: Boolean;
    procedure CloseClipboard;
    function GetClipboardHtml: String;
    function GetClipboardHtml_utf8: AnsiString;
    function ClipboardHasText: Boolean;
    function GetClipboardText_utf8: AnsiString;
  end;

implementation

procedure TClipboardViewer.Start;
begin
  if not FCVActive then
  begin
    { Add to clipboard chain }
    FNextClipboardViewer := SetClipboardViewer(Handle);
    FCVActive := True;
  end;
end;

procedure TClipboardViewer.Stopp;
begin
  if FCVActive then
  begin
    { Remove from clipboard chain }
    ChangeClipboardChain(Handle, FNextClipboardViewer);
    FNextClipboardViewer := 0;
    FCVActive := False;
  end;
end;


procedure TClipboardViewer.WMDrawClipboard(var Msg : TWMDrawClipboard);
begin
  inherited;
  { Clipboard content has changed }
  try
    if Assigned(OnClipboardContentChanged) then OnClipboardContentChanged(Self);
  finally
    { Inform the next window in the clipboard viewer chain }
    SendMessage(FNextClipboardViewer, WM_DRAWCLIPBOARD, 0, 0);
  end;
end;

procedure TClipboardViewer.WMChangeCBChain(var Msg : TWMChangeCBChain);
begin
  inherited;
  { mark message as done }
  Msg.Result := 0;
  { the chain has changed }
  if Msg.Remove = FNextClipboardViewer then
    { The next window in the clipboard viewer chain had been removed. We recreate it. }
    FNextClipboardViewer := Msg.Next
  else
    { Inform the next window in the clipboard viewer chain }
    SendMessage(FNextClipboardViewer, WM_CHANGECBCHAIN, Msg.Remove, Msg.Next);
end;

constructor TClipboardViewer.Create(AOwner: TComponent);
begin
  inherited;
  { Initialize variable }
  FCVActive := False;
end;

destructor TClipboardViewer.Destroy;
begin
  if FCVActive then
  begin
    { Remove from clipboard chain }
    Stopp;
  end;
  inherited;
end;

procedure TClipboardViewer.SetCVActive(B: Boolean);
begin
  if B then Start else Stopp;
end;

function TClipboardViewer.GetClipboardText: String;
begin
  Result := clipbrdfunctions.GetClipboardText;
end;

function TClipboardViewer.GetClipboardText_utf8: AnsiString;
begin
  Result := clipbrdfunctions.GetClipboardTextUTF8;
end;

function TClipboardViewer.OpenClipboard: Boolean;
begin
  Result := True;
  try
    Clipboard.Open;
  except
    Result := False;
  end;
end;

procedure TClipboardViewer.CloseClipboard;
begin
  Clipboard.Close;
end;

function TClipboardViewer.GetClipboardHtml: String;
begin
  Result := clipbrdfunctions.ReadClipboardHtml;
end;

function TClipboardViewer.GetClipboardHtml_utf8: AnsiString;
begin
  Result := clipbrdfunctions.ReadClipboardHtmlUTF8;
end;

function TClipboardViewer.ClipboardHasText: Boolean;
begin
  Result := Clipboard.HasFormat(CF_TEXT);
end;

end.
