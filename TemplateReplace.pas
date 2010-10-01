unit TemplateReplace;

interface

uses
  Classes, StrUtils;

//function DoTemplateReplace(const Template: String;
//  DataSetRecord: TDataSet): String;

implementation

{$ifdef jshdglksghskdhg}
function DoTemplateReplace(const Template: String;
  DataSetRecord: TDataSet): String;
var
  sl: TStringList;
  str: String;
  i: Integer;
begin

  //Delphi 7 note
  //Make sure your uses clause includes Classes and StrUtils
  //Delphi 8 note
  //Make sure your uses clause includes Borland.Vcl.Classes
  //and Borland.Vcl.StrUtils
  sl := TStringList.Create;

  try
    sl.Text := Template;
    //remove all comment lines
    i := 0;
    while i < sl.Count do
    begin
      if UpperCase(Copy(sl.Strings[i],1,3)) = 'REM' then
      begin
        sl.Delete(i);
        continue;
      end;
      //This line is executed if a comment is NOT removed
      inc(i);
    end;
    str := sl.Text;
  finally
    sl.Free;
  end;

  //we now have the template in a string.
  //Use StringReplace to replace parts of the template
  //with data from our database

  //begin by iterating through the AttendeesDataSource.DataSet Fields
  //Replace {fieldname} tags in str with field contents
  for i := 0 to Pred(DataSetRecord.Fields.Count) do
    str := StringReplace(str, '{'+DataSetRecord.Fields[i].FieldName+'}',
      DataSetRecord.Fields[i].AsString, [rfReplaceAll,rfIgnoreCase]);

  //Check for today's date tag {TODAY}
  if Pos('{TODAY}', UpperCase(str)) > 0 then
  begin
    str := StringReplace(str, '{today}',
      FormatDateTime('mmmm, dd, yyyy',Date),[rfReplaceAll, rfIgnoreCase]);
  end;

  //Check for current time tag {TIME}
  if Pos('{TIME}', UpperCase(str)) > 0 then
  begin
    str := StringReplace(str, '{time}',
      FormatDateTime('tt',Time),[rfReplaceAll, rfIgnoreCase]);
  end;

  //You can create any additional
  //custom tags using this same technique

  Result := str;
end;
{$endif}

end.
