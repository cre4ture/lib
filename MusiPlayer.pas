unit MusiPlayer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MMSystem, stdctrls;

type
  TMusiPlayer = class(TCustomControl)
  private
    { Private-Deklarationen }
    Sounds : array of Word;
    ii : integer;
    procedure CloseDevice(nr: Integer);
  protected
    { Protected-Deklarationen }
  public
    Silent: Boolean;
    Function PlayFile(Filename : string): Boolean;
    procedure CloseFreeFiles;
    procedure CloseAllFiles;
    // How to awoid this warning?
    constructor Create(AParent: TWinControl); reintroduce;
    destructor Destroy; override;
    { Public-Deklarationen }
  published
    { Published-Deklarationen }
  end;

implementation

procedure TMusiPlayer.CloseAllFiles;
begin
  while length(Sounds) > 0 do
  begin
    CloseDevice(0);
  end;
end;

procedure TMusiPlayer.CloseDevice(nr: Integer);
var GenParm: TMCI_Generic_Parms;
    i : integer;
begin
  GenParm.dwCallback := Application.Handle;
  mciSendCommand(Sounds[nr], mci_Close, 0, Longint(@GenParm));
  for i := nr to length(sounds)-2 do
    Sounds[i] := Sounds[i+1];
  Setlength(Sounds,length(sounds)-1);
end;

procedure TMusiPlayer.CloseFreeFiles;
var i : integer;
    StatusParm: TMCI_Status_Parms;
    Len, Pos : integer;
begin
  i := 0;
  while i < length(Sounds) do
  begin
    StatusParm.dwItem := mci_Status_Length;
    mciSendCommand(Sounds[i], mci_Status, mci_Wait or mci_Status_Item, Longint(@StatusParm));
    Len := StatusParm.dwReturn;
    StatusParm.dwItem := MCI_STATUS_POSITION;
    mciSendCommand(Sounds[i], mci_Status, mci_Wait or mci_Status_Item, Longint(@StatusParm));
    Pos := StatusParm.dwReturn;
    if Pos >= Len then
      CloseDevice(i)
    else
      inc(i);
  end;
end;

constructor TMusiPlayer.Create(AParent: TWinControl);
begin
  inherited Create(AParent);
  Parent := AParent;
  ii := 0;
  Silent := false;
end;

destructor TMusiPlayer.Destroy;
begin
  CloseAllFiles;
  inherited;
end;

Function TMusiPlayer.PlayFile(Filename : string): Boolean;
var DeviceID: Word;
    OpenParm: TMCI_Open_Parms;
    FFlags: Longint;
    PlayParm: TMCI_Play_Parms;
begin
  Result := false;
  if Silent then
    Exit;
  inc(ii);
  if ii = high(ii) then
    ii := 0;
  FillChar(OpenParm, SizeOf(TMCI_Open_Parms), 0);
  OpenParm.dwCallback := 0;
  OpenParm.lpstrDeviceType := '';
  OpenParm.lpstrElementName := PChar(FileName);
  FFlags := MCI_OPEN_ELEMENT{ or MCI_OPEN_SHAREABLE }or MCI_OPEN_ALIAS;
  OpenParm.dwCallback := Handle;
  OpenParm.lpstrAlias := Pchar('MusiPlayerNR' + inttostr(ii) + ExtractFileName(filename));
  if (mciSendCommand(0, mci_Open, FFlags, Longint(@OpenParm)) = 0) then
  begin
    DeviceID := OpenParm.wDeviceID;
    PlayParm.dwCallback := Handle;
    PlayParm.dwFrom := 0;
    PlayParm.dwTo := 0;
    FFlags := 0;
    if mciSendCommand(DeviceID, mci_Play, FFlags, Longint(@PlayParm)) = 0 then
    begin
      Result := true;
      setlength(sounds,length(sounds)+1);
      Sounds[length(Sounds)-1] := DeviceID;
    end;
  end;
end;

end.
 