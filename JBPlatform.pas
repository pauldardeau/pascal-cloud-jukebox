unit JBPlatform;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, IniReader, JBSysUtils, KeyValuePairs, Process,
  StrUtils, SysUtils, Types;

const
  DOUBLE_DASHES = '--';
  DIR_PATH_SEPARATOR = PathDelim;
  PLATFORM_MAC = 'mac';
  PLATFORM_LINUX = 'linux';
  PLATFORM_WINDOWS = 'windows';
  PLATFORM_UNKNOWN = 'unknown';


function JBMd5ForFile(IniFileName: String; PathToFile: String): String;
function JBGetPlatformConfigValues(IniFileName: String;
                                   Kvp: TKeyValuePairs): Boolean;
function JBGetPlatformIdentifier: String;


//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

function JBMd5ForFile(IniFileName: String; PathToFile: String): String;
var
  Kvp: TKeyValuePairs;
  KeyExe: String;
  KeyFieldNumber: String;
  Md5Exe: String;
  ProgramArgs: TStringList;
  ExitCode: Integer;
  StdOut: String;
  StdErr: String;
  FieldNumber: Integer;
  FieldNumberText: String;
  FileLines: TStringArray;
  FirstLine: String;
  LineFields: TStringArray;
begin
  Kvp := nil;
  ProgramArgs := nil;
  FileLines := nil;
  LineFields := nil;

  if not FileExists(IniFileName) then begin
    writeLn('error (JBMd5ForFile): ini file does not exist ' + IniFileName);
    JBMd5ForFile := '';
    exit;
  end;

  if not JBFileExists(PathToFile) then begin
    writeLn('error (JBMd5ForFile): file does not exist ' + PathToFile);
    JBMd5ForFile := '';
    exit;
  end;

  Kvp := TKeyValuePairs.Create;
  if JBGetPlatformConfigValues(IniFileName, Kvp) then begin
    KeyExe := 'md5_exe_file_name';
    KeyFieldNumber := 'md5_hash_output_field';
    if Kvp.ContainsKey(KeyExe) then begin
      Md5Exe := Kvp.GetValue(KeyExe);
      if not JBFileExists(Md5Exe) then begin
        writeLn('error: md5 executable not found: ' + Md5Exe);
        Kvp.Free;
        JBMd5ForFile := '';
        exit;
      end;

      ProgramArgs := TStringList.Create;
      ProgramArgs.Add(PathToFile);
      ExitCode := 0;
      StdOut := '';
      StdErr := '';

      if JBExecuteProgram(Md5Exe,
                          ProgramArgs,
                          '',
                          ExitCode,
                          StdOut,
                          StdErr) then begin
        if ExitCode = 0 then begin
          if StdOut.Length > 0 then begin
            FieldNumber := 1;
            if Kvp.ContainsKey(KeyFieldNumber) then begin
              FieldNumberText := Kvp.GetValue(KeyFieldNumber);
              if FieldNumberText.Length > 0 then begin
                try
                  FieldNumber := StrToInt(FieldNumberText);
                except
                  writeLn('error: unable to convert value ' + FieldNumberText +
                          ' for ' + KeyFieldNumber + ' to integer');
                  writeLn('will attempt to use first field');
                end;
              end;
            end;
            FileLines := StdOut.Split(LineEnding);
            if Length(FileLines) > 0 then begin
              FirstLine := FileLines[0];
              LineFields := FirstLine.Split(' ');
              if Length(LineFields) > 0 then begin
                Kvp.Free;
                ProgramArgs.Free;
                JBMd5ForFile := LineFields[FieldNumber-1];
                exit;
              end
              else begin
                if FirstLine.Length > 0 then begin
                  Kvp.Free;
                  ProgramArgs.Free;
                  JBMd5ForFile := FirstLine;
                  exit;
                end
                else begin
                  writeLn('error: Md5ForFile - first stdout line is empty');
                end;
              end;
            end
            else begin
              writeLn('error: Md5ForFile - stdout split by lines is empty');
            end;
          end
          else begin
            writeLn('error: Md5ForFile - no content for stdout captured');
          end;
        end
        else begin
          writeLn('error: Md5ForFile - non-zero exit code for md5 utility. value=' + IntToStr(ExitCode));
        end;
      end
      else begin
        writeLn('error: Md5ForFile - unable to execute md5 sum utility ' + Md5Exe);
      end;

      ProgramArgs.Free;
    end
    else begin
      writeLn('error: Md5ForFile - no value present for ' + KeyExe);
    end;
  end
  else begin
    writeLn('error: Md5ForFile - unable to retrieve platform config values');
  end;

  Kvp.Free;

  JBMd5ForFile := '';
end;

//*******************************************************************************

function JBGetPlatformConfigValues(IniFileName: String;
                                   Kvp: TKeyValuePairs): Boolean;
var
  OsIdentifier: String;
  Reader: TIniReader;
begin
  Reader := nil;
  OsIdentifier := JBGetPlatformIdentifier;
  if (OsIdentifier = PLATFORM_UNKNOWN) or (OsIdentifier.Length = 0) then begin
    writeLn('error: unknown platform');
    JBGetPlatformConfigValues := false;
    exit;
  end;

  try
    try
      Reader := TIniReader.Create(IniFileName);
      if not Reader.ReadSection(OsIdentifier, Kvp) then begin
        writeLn('error: no config section present for ' + OsIdentifier);
        JBGetPlatformConfigValues := false;
        exit;
      end
      else begin
        JBGetPlatformConfigValues := true;
        exit;
      end;
    except
      writeLn('error: unable to read ' + IniFileName);
      JBGetPlatformConfigValues := false;
      exit;
    end;
  finally
    if Reader <> nil then begin
      Reader.Free;
    end;
  end;
end;

//*******************************************************************************

function JBGetPlatformIdentifier: String;
var
  PlatformId: String;
begin
  PlatformId := '';

{$IFDEF Darwin}
  PlatformId := PLATFORM_MAC;
{$ENDIF}
{$IFDEF Linux}
  PlatformId := PLATFORM_LINUX;
{$ENDIF}
{$IFDEF Windows}
  PlatformId := PLATFORM_WINDOWS;
{$ENDIF}

  if PlatformId.Length = 0 then begin
    PlatformId := PLATFORM_UNKNOWN;
  end;

  JBGetPlatformIdentifier := PlatformId;
end;

//*******************************************************************************

end.

