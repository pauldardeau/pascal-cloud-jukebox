unit JBSysUtils;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, Process, StrUtils, SysUtils, Types;

const
  DOUBLE_DASHES = '--';
  DIR_PATH_SEPARATOR = PathDelim;

function JBPathJoin(DirPath: String; FileName: String): String;
function JBFileExists(FilePath: String): Boolean;
function JBDirectoryExists(DirPath: String): Boolean;
function JBDeleteFile(FilePath: String): Boolean;
function JBDeleteFileIfExists(FilePath: String): Boolean;
function JBFileReadAllText(FilePath: String): String;
function JBFileReadAllBytes(FilePath: String): TMemoryStream;
function JBFileWriteAllBytes(FilePath: String; Contents: array of Byte): Boolean;
function JBFileWriteAllText(FilePath: String; Contents: String): Boolean;
function JBGetFileSize(FilePath: String): Int64;
function JBFileReadTextLines(FilePath: String): TStringList;
function JBFileCopy(Source: String; Target: String): Boolean;
function JBExecuteProgram(ProgramPath: String;
                          ProgramArgs: TStringList;
                          out ExitCode: Integer;
                          out StdOut: String;
                          out StdErr: String): Boolean;
function JBListFilesInDirectory(DirPath: String): TStringList;
function JBPathSplitExt(FilePath: String;
                        out Root: String;
                        out Ext: String): Boolean;
procedure JBSleepSeconds(Seconds: Integer);
function JBFileAppendAllText(FilePath: String; Contents: String): Boolean;
function JBGetFileExtension(FileName: String): String;
function JBCreateDirectory(DirPath: String): Boolean;
procedure JBDeleteFilesInDirectory(DirPath: String);
function JBGetPid(): Integer;


//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

function JBPathJoin(DirPath: String; FileName: String): String;
begin
  if not DirPath.EndsWith(DIR_PATH_SEPARATOR) then begin
    JBPathJoin := DirPath + DIR_PATH_SEPARATOR + FileName;
  end
  else begin
    JBPathJoin := DirPath + FileName;
  end;
end;

//*******************************************************************************

function JBFileExists(FilePath: String): Boolean;
begin
  JBFileExists := FileExists(FilePath, true);
end;

//*******************************************************************************

function JBDirectoryExists(DirPath: String): Boolean;
begin
  JBDirectoryExists := DirectoryExists(DirPath, true);
end;

//*******************************************************************************

function JBDeleteFile(FilePath: String): Boolean;
begin
  JBDeleteFile := DeleteFile(FilePath);
end;

//*******************************************************************************

function JBDeleteFileIfExists(FilePath: String): Boolean;
begin
  if JBFileExists(FilePath) then begin
    JBDeleteFileIfExists := DeleteFile(FilePath);
  end
  else begin
    JBDeleteFileIfExists := false;
  end;
end;

//*******************************************************************************

function JBFileReadAllText(FilePath: String): String;
var
  FileHandle: TextFile;
  FileLine: String;
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  AssignFile(FileHandle, FilePath);
  try
    Reset(FileHandle);
    while not Eof(FileHandle) do begin
      readln(FileHandle, FileLine);
      Sb.Append(FileLine);
    end;
  except
    On E: EInOutError do begin
      writeln('error: JBFileReadAllText exception. Details: ', E.Message);
    end;
  end;
  CloseFile(FileHandle);

  JBFileReadAllText := Sb.ToString;
  Sb.Free;
end;

//*******************************************************************************

function JBFileReadAllBytes(FilePath: String): TMemoryStream;
var
  MemoryStream: TMemoryStream;
begin
  if JBFileExists(FilePath) then begin
    MemoryStream := TMemoryStream.Create;
    MemoryStream.LoadFromFile(FilePath);
    JBFileReadAllBytes := MemoryStream;
  end
  else begin
    JBFileReadAllBytes := nil;
  end;
end;

//*******************************************************************************

function JBFileWriteAllBytes(FilePath: String; Contents: array of Byte): Boolean;
var
  FileHandle: File of Byte;
begin
  AssignFile(FileHandle, FilePath);
  try
    try
      ReWrite(FileHandle);
      BlockWrite(FileHandle, Contents, Length(Contents));
      JBFileWriteAllBytes := true;
    except
      JBFileWriteAllBytes := false;
    end;
  finally
    CloseFile(FileHandle);
  end;
end;

//*******************************************************************************

function JBFileWriteAllText(FilePath: String; Contents: String): Boolean;
var
  Fs: TFileStream;
  Success: Boolean;
begin
  try
    Fs := TFileStream.Create(FilePath, fmCreate);
    Fs.Write(Contents[1], Length(Contents));
    Success := true;
  except
    on E: Exception do begin
      Success := false;
      writeln('Unable to write text to file. Details: ', E.ClassName, ': ', E.Message);
    end;
  end;

  Fs.Free;

  JBFileWriteAllText := Success;
end;

//*******************************************************************************

function JBGetFileSize(FilePath: String): Int64;
var
  F: File Of byte;
begin
  if JBFileExists(FilePath) then begin
    Assign(F, FilePath);
    try
      Reset(F);
      JBGetFileSize := FileSize(F);
    finally
      Close(F);
    end;
  end
  else begin
    JBGetFileSize := -1;
  end;
end;

//*******************************************************************************

function JBFileReadTextLines(FilePath: String): TStringList;
var
  F: TextFile;
  FileLine: String;
  FileLines: TStringList;
begin
  FileLines := TStringList.Create;
  AssignFile(F, FilePath);
  try
    Reset(F);
    while not EOF(F) do begin
      readLn(F, FileLine);
      FileLines.Append(FileLine);
    end;
  finally
    CloseFile(F);
  end;

  JBFileReadTextLines := FileLines;
end;

//*******************************************************************************

function JBFileCopy(Source: String; Target: String): Boolean;
var
  MemBuffer: TMemoryStream;
  Success: Boolean;
begin
  MemBuffer := TMemoryStream.Create;
  try
    MemBuffer.LoadFromFile(Source);
    MemBuffer.SaveToFile(Target);
    Success := true;
  except
  end;
  MemBuffer.Free;
  JBFileCopy := Success;
end;

//*******************************************************************************

function JBExecuteProgram(ProgramPath: String;
                          ProgramArgs: TStringList;
                          out ExitCode: Integer;
                          out StdOut: String;
                          out StdErr: String): Boolean;
var
  Process: TProcess;
  i: Integer;
  StringList: TStringList;
begin
  StringList := nil;
  ExitCode := -1;
  StdOut := '';
  StdErr := '';

  try
    Process := TProcess.Create(nil);
    Process.Executable:= ProgramPath;

    for i := 0 to ProgramArgs.Count-1 do begin
      Process.Parameters.Add(ProgramArgs[i]);
    end;

    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute;
    ExitCode := Process.ExitCode;

    StringList := TStringList.Create;

    StringList.LoadFromStream(Process.Output);
    for i := 0 to StringList.Count-1 do begin
      StdOut := StdOut + StringList[i];
    end;

    StringList.Clear;

    StringList.LoadFromStream(Process.Stderr);
    for i := 0 to StringList.Count-1 do begin
      StdErr := StdErr + StringList[i];
    end;
  finally
    Process.Free;
    if StringList <> nil then begin
      StringList.Free;
    end;
  end;
  
  JBExecuteProgram := ExitCode = 0;
end;

//*******************************************************************************

function JBListFilesInDirectory(DirPath: String): TStringList;
var
  ListFiles: TStringList;
begin
  ListFiles := TStringList.Create;
  //TODO: implement JBListFilesInDirectory

  JBListFilesInDirectory := ListFiles;
end;

//*******************************************************************************

function JBPathSplitExt(FilePath: String;
                        out Root: String;
                        out Ext: String): Boolean;
var
  Success: Boolean;
  PosLastDot: Integer;
  PrecedingChar: Char;
begin
  // splitext('bar') -> ('bar', '')
  // splitext('foo.bar.exe') -> ('foo.bar', '.exe')
  // splitext('/foo/bar.exe') -> ('/foo/bar', '.exe')
  // splitext('.cshrc') -> ('.cshrc', '')
  // splitext('/foo/....jpg') -> ('/foo/....jpg', '')

  Root := '';
  Ext := '';
  Success := false;

  if FilePath.Length > 0 then begin
    Success := true;
    PosLastDot := FilePath.LastIndexOf('.');
    if PosLastDot = -1 then begin
      // no '.' exists in path (i.e., 'bar')
      Root := FilePath;
    end
    else begin
      // is the last '.' the first character? (i.e., '.cshrc')
      if PosLastDot = 0 then begin
        Root := FilePath;
      end
      else begin
        PrecedingChar := FilePath[PosLastDot-1];
        // is the preceding char also a '.'? (i.e., '/foo/....jpg')
        if PrecedingChar = '.' then begin
          Root := FilePath;
        end
        else begin
          // splitext('foo.bar.exe') -> ('foo.bar', '.exe')
          // splitext('/foo/bar.exe') -> ('/foo/bar', '.exe')
          Root := FilePath.Substring(0, PosLastDot);
          Ext := FilePath.Substring(PosLastDot);
        end;
      end;
    end;
  end;

  JBPathSplitExt := Success;
end;

//*******************************************************************************

procedure JBSleepSeconds(Seconds: Integer);
begin
  Sleep(Seconds * 1000);
end;

//*******************************************************************************

function JBFileAppendAllText(FilePath: String; Contents: String): Boolean;
var
  Success: Boolean;
begin
  Success := false;
  //TODO: implement JBFileAppendAllText
  JBFileAppendAllText := Success;
end;

//*******************************************************************************

function JBGetFileExtension(FileName: String): String;
begin
  JBGetFileExtension := ExtractFileExt(FileName);
end;

//*******************************************************************************

function JBCreateDirectory(DirPath: String): Boolean;
var
  Success: Boolean;
begin
  if JBDirectoryExists(DirPath) then begin
    Success := false;
  end
  else begin
    Success := CreateDir(DirPath);
  end;
  JBCreateDirectory := Success;
end;

//*******************************************************************************

procedure JBDeleteFilesInDirectory(DirPath: String);
var
  listFiles: TStringList;
  i: Integer;
begin
  listFiles := JBListFilesInDirectory(DirPath);
  for i := 0 to listFiles.Count-1 do begin
    JBDeleteFile(JBPathJoin(DirPath, listFiles[i]));
  end;
end;

//*******************************************************************************

function JBGetPid(): Integer;
begin
  JBGetPid := GetProcessId;
end;

//*******************************************************************************

end.
