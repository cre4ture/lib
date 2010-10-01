unit frm_pos_size_ini;

interface

uses
  IniFiles, Forms;

//Visibility geht leider in OnCreate nicht...... :-(
procedure SaveFormSizePos(ini: TIniFile; Section: String; Form: TForm;
  Visibility: Boolean = False); overload;
procedure SaveFormSizePos(inifile: string; Form: TForm;
  Visibility: Boolean = False); overload;
procedure LoadFormSizePos(ini: TIniFile; Section: String; Form: TForm;
  Visibility: Boolean = False); overload;
procedure LoadFormSizePos(inifile: string; Form: TForm;
  Visibility: Boolean = False); overload;

implementation

procedure SaveFormSizePos(inifile: string; Form: TForm;
  Visibility: Boolean = False); overload;
var ini: TIniFile;
begin
  ini := TIniFile.Create(inifile);
  SaveFormSizePos(ini,Form.Name,Form,Visibility);
  ini.UpdateFile;
  ini.Free;
end;

procedure SaveFormSizePos(ini: TIniFile; Section: String; Form: TForm;
  Visibility: Boolean = False); overload;
begin
  ini.WriteInteger(Section,'Left',Form.Left);
  ini.WriteInteger(Section,'Top',Form.Top);
  ini.WriteInteger(Section,'Width',Form.Width);
  ini.WriteInteger(Section,'Height',Form.Height);

  if Visibility then
    ini.WriteBool(Section,'Visible',Form.Visible);
end;

procedure LoadFormSizePos(inifile: string; Form: TForm;
  Visibility: Boolean = False); overload;
var ini: TIniFile;
begin
  ini := TIniFile.Create(inifile);
  LoadFormSizePos(ini,Form.Name,Form,Visibility);
  ini.Free;
end;

procedure LoadFormSizePos(ini: TIniFile; Section: String; Form: TForm;
  Visibility: Boolean = False); overload;
begin
  Form.Left := ini.ReadInteger(Section,'Left',Form.Left);
  Form.Top := ini.ReadInteger(Section,'Top',Form.Top);

  if not(Form.BorderStyle in [bsSingle]) then
  begin
    Form.Width := ini.ReadInteger(Section,'Width',Form.Width);
    Form.Height := ini.ReadInteger(Section,'Height',Form.Height);
  end;

  if Form.Top > Screen.DesktopRect.Bottom - 50 then
    Form.Top := Screen.DesktopRect.Bottom - 100;
  if Form.Top < Screen.DesktopRect.Top - 50 then
    Form.Top := Screen.DesktopRect.Top;
  if Form.Left > Screen.DesktopRect.Right - 50 then
    Form.Left := Screen.DesktopRect.Right - 100;
  if Form.Left + Form.Width < Screen.DesktopRect.Left + 10 then
    Form.Left := Screen.DesktopRect.Left;

  if Visibility then
    Form.Visible := ini.ReadBool(Section,'Visible',False);
end;


end.
