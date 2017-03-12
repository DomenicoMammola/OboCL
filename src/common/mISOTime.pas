// original source code from jcl/source/common/JclDITs.pas

{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is: DITs.pas.                                                                  }
{ The Initial Developer of the Original Code is Peter J. Haas. Portions created by Peter J. Haas   }
{ are Copyright (C) 2001 Peter J. Haas. All Rights Reserved.                                       }
{                                                                                                  }
{ The Original Code Version 2.0 is: JclDITs.pas.                                                   }
{ The Initial Developer of the Original Code V2.0 is Peter J. Haas. Portions created by            }
{ Peter J. Haas are Copyright (C) 2004 Peter J. Haas. All Rights Reserved.                         }
{ You may retrieve the latest version of the Original Code at the homepage                         }
{ of Peter J. Haas (delphi@pjh2.de) located at http://delphi.pjh2.de/                              }
{                                                                                                  }
{--------------------------------------------------------------------------------------------------}
{                                                                                                  }
{  NOTE: As of 2004-05-15, Peter J. Haas has stopped maintaining code he donated to the JCL.       }
{        He is not to be held responsible for modifications applied after this date.               }
{        Peter J. Haas no longer wants to be associated with Project JEDI.                         }
{                                                                                                  }
{--------------------------------------------------------------------------------------------------}
{                                                                                                  }
{  Contributor(s):                                                                                 }
{    Peter J. Haas (peterjhaas)                                                                    }
{    Robert Rossmair (rrossmair)                                                                   }
{                                                                                                  }
{ Alternatively, the contents of this file may be used under the terms of the GNU Lesser General   }
{ Public License (the  "LGPL License"), in which case the provisions of the LGPL License are       }
{ applicable instead of those above.                                                               }
{                                                                                                  }
{ If you wish to allow use of your version of this file only under the terms of the LGPL License   }
{ and not to allow others to use your version of this file under the MPL, indicate your decision by}
{ deleting the provisions above and replace them with the notice and other provisions required by  }
{ the LGPL License. If you do not delete the provisions above, a recipient may use your version of }
{ this file under either the MPL or the LGPL License.                                              }
{                                                                                                  }
{  For more information about the LGPL:                                                            }
{  http://www.gnu.org/copyleft/lesser.html                                                         }
{                                                                                                  }
{**************************************************************************************************}

unit mISOTime;

interface

type
  TISODateTimeOption = (dtoDate, dtoTime, dtoMilliseconds, dtoBasic);
  TISODateTimeOptions = set of TISODateTimeOption;
  TISODateTimeSeparator = (dtsT, dtsSpace);
  TISOFloatDecimalSeparator = (fdsComma, fdsPoint);


{$IFNDEF FPC}
  TISOTimeChar = AnsiChar;
  TISOTimeString = AnsiString;
{$ELSE}
  TISOTimeChar = Char;
  TISOTimeString = String;
{$ENDIF}

const
  // date time separator
  ISODateTimeSeparatorT = 'T';
  ISODateTimeSeparatorSpace = ' ';
  ISODateTimeSeparators: array[TISODateTimeSeparator] of TISOTimeChar =
    (ISODateTimeSeparatorT, ISODateTimeSeparatorSpace);
  // float decimal separator
  ISOFloatDecimalSeparatorComma = ',';
  ISOFloatDecimalSeparatorPoint = '.';
  ISOFloatDecimalSeparators: array[TISOFloatDecimalSeparator] of TISOTimeChar =
    (ISOFloatDecimalSeparatorComma, ISOFloatDecimalSeparatorPoint);

  RsTextDataInvalidISODate     = '''%s'' is not a valid ISO date.';
  RsTextDataInvalidISOTime     = '''%s'' is not a valid ISO time.';
  RsTextDataInvalidISODateTime = '''%s'' is not a valid ISO date and time.';
  RsTextDataInvalidISOFloat    = '''%s'' is not a valid ISO float value.';



// Convert TDateTime to TISOTimeString
function ISODateTimeToStrCustom(
    const Value: TDateTime;
    Options: TISODateTimeOptions;
    CustomDateTimeSeparator: TISODateTimeSeparator = dtsT;
    CustomDecimalSeparator: TISOFloatDecimalSeparator = fdsComma): TISOTimeString;


// Converts an ISO date TISOTimeString to TDateTime and replaces the date part of Date
// Valid TISOTimeStrings are
//   'YYYY-MM-DD' and 'YYYYMMDD'
function TryISOStrToDate(const Value: TISOTimeString; var Date: TDateTime): Boolean;

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function TryISOStrToDateTime(const Value: TISOTimeString; out DateTime: TDateTime): Boolean;

// Converts an ISO time TISOTimeString to TDateTime and replace the time part of Time
// Valid TISOTimeStrings are
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function TryISOStrToTime(const Value: TISOTimeString; var Time: TDateTime): Boolean;


// Converts an ISO date TISOTimeString to TDateTime
// Valid TISOTimeStrings:
//   'YYYY-MM-DD' and 'YYYYMMDD'
function ISOStrToDate(const Value: TISOTimeString): TDateTime;
function ISOStrToDateDef(const Value: TISOTimeString; const Default: TDateTime): TDateTime;

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function ISOStrToDateTime(const Value: TISOTimeString): TDateTime;
function ISOStrToDateTimeDef(const Value: TISOTimeString; const Default: TDateTime): TDateTime;

// Converts TDateTime to date TISOTimeString 'YYYY-MM-DD'
function ISODateToStr(const Value: TDateTime): TISOTimeString;

// Converts TDateTime to date time TISOTimeString 'YYYY-MM-DDThh:mm:ss'
function ISODateTimeToStr(const Value: TDateTime): TISOTimeString;

// Converts an ISO time string to TDateTime
// Valid strings:
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function ISOStrToTime(const Value: String): TDateTime;
function ISOStrToTimeDef(const Value: String; const Default: TDateTime): TDateTime;

// Converts TDateTime to time string 'hh:mm:ss'
function ISOTimeToStr(const Value: TDateTime): String;


implementation

uses
  SysUtils;

function CheckDateTimeFormat(const Value, DTFormat: TISOTimeString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(Value) <> Length(DTFormat) then
    Exit;
  for i := 1 to Length(Value) do begin
    if DTFormat[i] = '9' then begin  // Digit
{$IFNDEF FPC}
      if not CharInSet(Value[i], ['0'..'9']) then
{$ELSE}
      if not (Value[i] in ['0'..'9']) then
{$ENDIF}
        Exit;
    end
    else begin
      if DTFormat[i] <> Value[i] then
        Exit;
    end;
  end;
  Result := True;
end;


// Converts an ISO date TISOTimeString to TDateTime and replace the date part of Date
function TryISOStrToDate(const Value: TISOTimeString; var Date: TDateTime): Boolean;
var
  Offset: Integer;
  Year, Month, Day: Word;
begin
  Result := False;
  if CheckDateTimeFormat(Value, '9999-99-99') then
    Offset := 1
  else if CheckDateTimeFormat(Value, '99999999') then
    Offset := 0
  else
    Exit;
  Year  := Word(StrToIntDef(Copy(Value, 1, 4), 0));
  Month := Word(StrToIntDef(Copy(Value, 5 + 1 * Offset, 2), 0));
  Day   := Word(StrToIntDef(Copy(Value, 7 + 2 * Offset, 2), 0));
  try
    Date := EncodeDate(Year, Month, Day) + Frac(Date);
    Result := True;
  except
    on EConvertError do;
  end;
end;

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function TryISOStrToDateTime(const Value: TISOTimeString; out DateTime: TDateTime): Boolean;
var
  DatePart, TimePart : TISOTimeString;
  i : Integer;
begin
  Result := False;
  DateTime := 0;
  i := Pos('T', Value);
  if i = 0 then
    i := Pos(' ', Value);
  if i > 0 then begin
    DatePart := Copy(Value, 1, i-1);
    TimePart := Copy(Value, i+1, MaxInt);
    Result := TryISOStrToDate(DatePart, DateTime) and TryISOStrToTime(TimePart, DateTime);
  end;
end;


// Converts an ISO time TISOTimeString to TDateTime and replace the time part of Time
function TryISOStrToTime(const Value: TISOTimeString; var Time: TDateTime): Boolean;
var
  s, ms: TISOTimeString;
  i: Integer;
  Hours, Minutes, Seconds, Milliseconds: Word;
  Offset: Integer;
  WithSeconds: Boolean;
begin
  Result := False;
  // Milliseconds part
  i := Pos(ISOFloatDecimalSeparatorComma, Value);    // ','
  if i = 0 then
    i := Pos(ISOFloatDecimalSeparatorPoint, Value);  // '.'
  if i = 0 then begin
    s := Value;
    ms := '';
  end
  else begin
    s := Copy(Value, 1, i - 1);
    ms := Copy(Value, i + 1, MaxInt);
  end;
  if CheckDateTimeFormat(s, '99:99:99') then begin
    Offset := 1;
    WithSeconds := True;
  end
  else if CheckDateTimeFormat(s, '999999') then begin
    Offset := 0;
    WithSeconds := True;
  end
  else if CheckDateTimeFormat(s, '99:99') then begin
    Offset := 1;
    WithSeconds := False;
  end
  else if CheckDateTimeFormat(s, '9999') then begin
    Offset := 0;
    WithSeconds := False;
  end
  else
    Exit;

  Hours   := Word(StrToIntDef(Copy(Value, 1, 2), 100));
  Minutes := Word(StrToIntDef(Copy(Value, 3 + 1 * Offset, 2), 100));

  if WithSeconds then begin
    Seconds := Word(StrToIntDef(Copy(Value, 5 + 2 * Offset, 2), 100));
  end
  else begin
    Seconds := 0;
    if Length(ms) > 0 then  // Milliseconds without seconds -> error
      Exit;
  end;

  case Length(ms) of
    0: Milliseconds := 0;
    3: Milliseconds := Word(StrToIntDef(ms, 10000));
  else
    Exit;
  end;

  try
    Time := EncodeTime(Hours, Minutes, Seconds, Milliseconds) + Int(Time);
    Result := True;
  except
    on EConvertError do;
  end;
end;


// Converts an ISO date TISOTimeString to TDateTime
// Valid TISOTimeStrings:
//   'CCYY-MM-DD' and 'CCYYMMDD'
function ISOStrToDate(const Value: TISOTimeString): TDateTime;
begin
  Result := 0;
  if not TryISOStrToDate(Value, Result) then
    raise Exception.Create(Format(RsTextDataInvalidISODate, [Value]));
end;

function ISOStrToDateDef(const Value: TISOTimeString; const Default: TDateTime): TDateTime;
begin
  Result := 0;
  if not TryISOStrToDate(Value, Result) then
    Result := Default;
end;

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function ISOStrToDateTime(const Value: TISOTimeString): TDateTime;
begin
  if not TryISOStrToDateTime(Value, Result) then
    raise Exception.Create(Format(RsTextDataInvalidISODateTime, [Value]));
end;

function ISOStrToDateTimeDef(const Value: TISOTimeString; const Default: TDateTime): TDateTime;
begin
  if not TryISOStrToDateTime(Value, Result) then
    Result := Default;
end;

// Converts TDateTime to date TISOTimeString 'YYYY-MM-DD'
function ISODateToStr(const Value: TDateTime): TISOTimeString;
begin
  Result := ISODateTimeToStrCustom(Value, [dtoDate], dtsT);
end;


// Convert TDateTime to TISOTimeString
function ISODateTimeToStrCustom(
    const Value: TDateTime;
    Options: TISODateTimeOptions;
    CustomDateTimeSeparator: TISODateTimeSeparator{$IFDEF SUPPORTS_DEFAULTPARAMS} = dtsT{$ENDIF};
    CustomDecimalSeparator: TISOFloatDecimalSeparator{$IFDEF SUPPORTS_DEFAULTPARAMS} = fdsComma{$ENDIF}): TISOTimeString;
var
  DTFormat: TISOTimeString;
  Year, Month, Day: Word;
  Hours, Minutes, Seconds, Milliseconds: Word;
begin
  // Parameter check
  if Options = [] then
    Options := [dtoDate, dtoTime]
  else if Options = [dtoBasic] then
    Options := [dtoDate, dtoTime, dtoBasic]
  else if dtoMilliseconds in Options then
    Include(Options, dtoTime);
  // convert date part
  if dtoDate in Options then begin
    DecodeDate(Value, Year, Month, Day);
    if dtoBasic in Options then
      DTFormat := '%.4d%.2d%.2d'     // 'YYYYMMDD'
    else
      DTFormat := '%.4d-%.2d-%.2d';  // 'YYYY-MM-DD'
    Result := TISOTimeString(Format(DTFormat, [Year, Month, Day]));
    // date and time part exists
    if dtoTime in Options then
      Result := Result + ISODateTimeSeparators[CustomDateTimeSeparator];
  end
  else
    Result := '';
  // convert time part
  if dtoTime in Options then begin
    DecodeTime(Value, Hours, Minutes, Seconds, Milliseconds);
    if dtoBasic in Options then
      DTFormat := '%s%.2d%.2d%.2d'     // 'hhnnss'
    else
      DTFormat := '%s%.2d:%.2d:%.2d';  // 'hh:nn:ss'
    Result := Format(DTFormat, [Result, Hours, Minutes, Seconds]);
    // Milliseconds
    if dtoMilliseconds in Options then   // ',zzz'
      Result := Format('%s%s%.3d', [Result, ISOFloatDecimalSeparators[CustomDecimalSeparator], Milliseconds]);
  end;
end;

// Converts TDateTime to date time TISOTimeString 'YYYY-MM-DDThh:mm:ss'
function ISODateTimeToStr(const Value: TDateTime): TISOTimeString;
begin
  Result := ISODateTimeToStrCustom(Value, [dtoDate, dtoTime], dtsT);
end;

// Converts an ISO time string to TDateTime
// Valid strings:
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function ISOStrToTime(const Value: String): TDateTime;
begin
  Result := 0;
  if not TryISOStrToTime(Value, Result) then
    raise Exception.Create(Format(RsTextDataInvalidISOTime, [Value]));
end;

function ISOStrToTimeDef(const Value: String; const Default: TDateTime): TDateTime;
begin
  Result := 0;
  if not TryISOStrToTime(Value, Result) then
    Result := Default;
end;

// Converts TDateTime to time string 'hh:mm:ss'
function ISOTimeToStr(const Value: TDateTime): String;
begin
  Result := ISODateTimeToStrCustom(Value, [dtoTime], dtsT);
end;

end.
