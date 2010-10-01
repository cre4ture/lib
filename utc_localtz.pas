unit utc_localtz;

interface

uses windows, sysutils;

function UtcToLocalDateTime(aUTC : TDateTime) : TDateTime;
function LocalDateTimeToUtc(aLocalTime : TDateTime) : TDateTime;

// This Function is missing in the "windows"-api-header from Delphi
// can be removed when this bug is fixed
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
  var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
{$EXTERNALSYM TzSpecificLocalTimeToSystemTime}

implementation

function TzSpecificLocalTimeToSystemTime; external kernel32 name 'TzSpecificLocalTimeToSystemTime';

function UtcToLocalDateTime(aUTC : TDateTime) : TDateTime;
var
  tzi : TIME_ZONE_INFORMATION;
  utc : TSystemTime;
  localtime : TSystemTime;
begin
  DateTimeToSystemTime(aUTC,utc);
  GetTimeZoneInformation(tzi);
  SystemTimeToTzSpecificLocalTime(@tzi,utc,localtime);
  Result := SystemTimeToDateTime(localtime);
end;

function LocalDateTimeToUtc(aLocalTime : TDateTime) : TDateTime;
var
  tzi : TIME_ZONE_INFORMATION;
  utc : TSystemTime;
  localtime : TSystemTime;
begin
  DateTimeToSystemTime(aLocalTime,localtime);
  GetTimeZoneInformation(tzi);
  TzSpecificLocalTimeToSystemTime(@tzi,localtime,utc);
  Result := SystemTimeToDateTime(utc);
end;

end.
