unit S3ExtStorageSystem;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, JBSysUtils, KeyValuePairs, PropertySet, StorageSystem, SysUtils;

const
  // Http properties
  PROP_CONTENT_LENGTH = 'Content-Length';
  PROP_CONTENT_TYPE = 'Content-Type';
  PROP_CONTENT_MD5 = 'Content-MD5';
  PROP_CONTENT_ENCODING = 'Content-Encoding';

  // script file extensions (suffixes)
  SFX_BATCH_FILE = '.bat';
  SFX_SHELL_SCRIPT = '.sh';

  // system shells
  DEFAULT_POSIX_SHELL = '/bin/sh';

  // file prefixes
  PREFIX_RUN_SCRIPT_NAME = 'exec-';

  // scripts
  SCR_TEMPLATE_LIST_CONTAINERS = 's3-list-containers';
  SCR_TEMPLATE_CREATE_CONTAINER = 's3-create-container';
  SCR_TEMPLATE_DELETE_CONTAINER = 's3-delete-container';
  SCR_TEMPLATE_LIST_CONTAINER_CONTENTS = 's3-list-container-contents';
  SCR_TEMPLATE_HEAD_OBJECT = 's3-head-object';
  SCR_TEMPLATE_PUT_OBJECT_WITH_PROPERTIES = 's3-put-object-props';
  SCR_TEMPLATE_PUT_OBJECT = 's3-put-object';
  SCR_TEMPLATE_DELETE_OBJECT = 's3-delete-object';
  SCR_TEMPLATE_GET_OBJECT = 's3-get-object';

  // script variables
  SCR_VAR_BUCKET_NAME = '%%BUCKET_NAME%%';
  SCR_VAR_METADATA_PROPERTIES = '%%METADATA_PROPERTIES%%';
  SCR_VAR_OBJECT_NAME = '%%OBJECT_NAME%%';
  SCR_VAR_OUTPUT_FILE = '%%OUTPUT_FILE%%';
  SCR_VAR_S3_ENDPOINT_URL = '%%S3_ENDPOINT_URL%%';
  SCR_VAR_S3_REGION = '%%S3_REGION%%';

type
  TS3ExtStorageSystem = Class(TStorageSystem)
  private
    DebugMode: Boolean;
    EndpointUrl: String;
    Region: String;
    Directory: String;
    ScriptDirectory: String;
    ListContainers: TStringList;

  public
    constructor Create(aEndpointUrl: String;
                       aRegion: String;
                       aDirectory: String;
                       aDebugMode: Boolean);
    destructor Destroy; override;

    function Enter(): Boolean; override;
    procedure Leave; override;
    function ListAccountContainers: TStringList; override;
    function GetContainerNames: TStringList; override;
    function HasContainer(ContainerName: String): Boolean; override;
    function CreateContainer(ContainerName: String): Boolean; override;
    function DeleteContainer(ContainerName: String): Boolean; override;
    function ListContainerContents(ContainerName: String): TStringList; override;
    function GetObjectMetadata(ContainerName: String;
                               ObjectName: String;
                               DictProps: TPropertySet): Boolean; override;
    function PutObject(ContainerName: String;
                       ObjectName: String;
                       FileContents: TMemoryStream;
                       Headers: TPropertySet): Boolean; override;
    function PutObjectFromFile(ContainerName: String;
                               ObjectName: String;
                               FilePath: String;
                               Headers: TPropertySet): Boolean; override;
    function DeleteObject(ContainerName: String;
                          ObjectName: String): Boolean; override;
    function GetObject(ContainerName: String;
                       ObjectName: String;
                       LocalFilePath: String): Int64; override;
    function GetScriptSuffix(): String;


  protected
    procedure PopulateCommonVariables(Kvp: TKeyValuePairs);
    procedure PopulateBucket(Kvp: TKeyValuePairs; BucketName: String);
    procedure PopulateObject(Kvp: TKeyValuePairs; ObjectName: String);
    function RunProgram(ProgramPath: String;
                        ListOutputLines: TStringList): Boolean;
    function RunProgram(ProgramPath: String): Boolean;
    function RunProgram(ProgramPath: String; out StdOut: String): Boolean;
    function PrepareRunScript(ScriptTemplate: String;
                              RunScript: String;
                              Kvp: TKeyValuePairs): Boolean;
    function RunScriptNameForTemplate(ScriptTemplate: String): String;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TS3ExtStorageSystem.Create(aEndpointUrl: String;
                                       aRegion: String;
                                       aDirectory: String;
                                       aDebugMode: Boolean);
begin
  inherited Create;
  DebugMode := aDebugMode;
  EndpointUrl := aEndpointUrl;
  Region := aRegion;
  Directory := aDirectory;
  ScriptDirectory := JBPathJoin(Directory, 'scripts');
  ListContainers := nil;
end;

//*******************************************************************************

destructor TS3ExtStorageSystem.Destroy;
begin
  writeLn('TS3ExtStorageSystem.Destroy');
  ListContainers.Free;
  inherited;
end;

//*******************************************************************************

function TS3ExtStorageSystem.Enter: Boolean;
begin
  if DebugMode then begin
    writeLn('TS3ExtStorageSystem.Enter');
  end;

  ListContainers := ListAccountContainers;
  Enter := true;
end;

//*******************************************************************************

procedure TS3ExtStorageSystem.Leave;
begin
  if DebugMode then begin
    writeLn('TS3ExtStorageSystem.Leave');
  end;
end;

//*******************************************************************************

function TS3ExtStorageSystem.ListAccountContainers: TStringList;
var
  ListOfContainers: TStringList;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('entering ListAccountContainers');
  end;

  ListOfContainers := TStringList.Create;
  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);

  ScriptTemplate := SCR_TEMPLATE_LIST_CONTAINERS + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if not RunProgram(RunScript, ListOfContainers) then begin
        ListOfContainers.Clear;
        writeLn('error: unable to run script');
      end;
    end
    else begin
      writeLn('error: unable to prepare script');
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  ListAccountContainers := ListOfContainers;
end;

//*******************************************************************************

function TS3ExtStorageSystem.GetContainerNames: TStringList;
begin
  GetContainerNames := ListContainers;
end;

//*******************************************************************************

function TS3ExtStorageSystem.HasContainer(ContainerName: String): Boolean;
begin
  HasContainer := ListContainers.IndexOf(ContainerName) <> -1;
end;

//*******************************************************************************

function TS3ExtStorageSystem.CreateContainer(ContainerName: String): Boolean;
var
  ContainerCreated: Boolean;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('entering CreateContainer with ContainerName=' + ContainerName);
  end;

  ContainerCreated := false;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);

  ScriptTemplate := SCR_TEMPLATE_CREATE_CONTAINER + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript) then begin
        ContainerCreated := true;
      end
      else begin
        writeLn('error: create container ' +
                ContainerName + ' failed');
      end;
    end
    else begin
      writeLn('error: unable to prepare run script');
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  CreateContainer := ContainerCreated;
end;

//*******************************************************************************

function TS3ExtStorageSystem.DeleteContainer(ContainerName: String): Boolean;
var
  ContainerDeleted: Boolean;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('DeleteContainer: ' + ContainerName);
  end;

  ContainerDeleted := false;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);

  ScriptTemplate := SCR_TEMPLATE_DELETE_CONTAINER + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript) then begin
        ContainerDeleted := true;
      end;
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  DeleteContainer := ContainerDeleted;
end;

//*******************************************************************************

function TS3ExtStorageSystem.ListContainerContents(ContainerName: String): TStringList;
var
  ListObjects: TStringList;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('entering ListContainerContents with ContainerName=' + ContainerName);
  end;

  ListObjects := TStringList.Create;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);

  ScriptTemplate := SCR_TEMPLATE_LIST_CONTAINER_CONTENTS + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if not RunProgram(RunScript, ListObjects) then begin
        ListObjects.Clear;
        writeLn('error: unable to run program');
      end;
    end
    else begin
      writeLn('error: unable to prepare run script');
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  ListContainerContents := ListObjects;
end;

//*******************************************************************************

function TS3ExtStorageSystem.GetObjectMetadata(ContainerName: String;
                                               ObjectName: String;
                                               DictProps: TPropertySet): Boolean;
var
  Success: Boolean;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
  StdOut: String;
begin
  if DebugMode then begin
    writeLn('GetObjectMetadata: Container=' +
            ContainerName +
            ', Object=' +
            ObjectName);
  end;

  Success := false;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);
  PopulateObject(Kvp, ObjectName);

  ScriptTemplate := SCR_TEMPLATE_HEAD_OBJECT + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript, StdOut) then begin
        writeLn(StdOut);
        Success := true;
      end;
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  GetObjectMetadata := Success;
end;

//*******************************************************************************

function TS3ExtStorageSystem.PutObject(ContainerName: String;
                                       ObjectName: String;
                                       FileContents: TMemoryStream;
                                       Headers: TPropertySet): Boolean;
var
  ObjectAdded: Boolean;
  TmpFile: String;
  FileSaveSuccess: Boolean;
begin
  if DebugMode then begin
    writeLn('PutObject: Container=' +
            ContainerName +
            ', Object=' +
            ObjectName +
            ', Length=' +
            IntToStr(FileContents.Size));
  end;

  ObjectAdded := false;

  TmpFile := 'tmp_' + ContainerName + '_' + ObjectName;

  try
    FileContents.SaveToFile(TmpFile);
    FileSaveSuccess := true;
  except
    FileSaveSuccess := false;
  end;

  if FileSaveSuccess then begin
    ObjectAdded := PutObjectFromFile(ContainerName,
                                     ObjectName,
                                     TmpFile,
                                     Headers);
    DeleteFile(TmpFile);
  end
  else begin
    writeLn('error: PutObject not able to write to tmp file');
  end;

  PutObject := ObjectAdded;
end;

//*******************************************************************************

function TS3ExtStorageSystem.PutObjectFromFile(ContainerName: String;
                                               ObjectName: String;
                                               FilePath: String;
                                               Headers: TPropertySet): Boolean;
var
  ObjectAdded: Boolean;
  ContentLength: UInt64;
  ContentType: String;
  ContentMd5: String;
  ContentEncoding: String;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
  SbMetadataProps: TStringBuilder;
  MetadataProps: String;
begin
  if DebugMode then begin
    writeLn('PutObjectFromFile: Container=' +
            ContainerName +
            ', Object=' +
            ObjectName +
            ', FilePath=' +
            FilePath);
  end;

  ObjectAdded := false;
  SbMetadataProps := TStringBuilder.Create;

  if Headers <> nil then begin
    if Headers.Contains(PROP_CONTENT_LENGTH) then begin
      ContentLength :=
           Headers.GetULongValue(PROP_CONTENT_LENGTH);
      SbMetadataProps.Append('contentLength=');
      SbMetadataProps.Append(IntToStr(ContentLength));
      SbMetadataProps.Append(' ');
    end;

    if Headers.Contains(PROP_CONTENT_TYPE) then begin
      ContentType :=
          Headers.GetStringValue(PROP_CONTENT_TYPE);
      // contentType
      if ContentType.Length > 0 then begin
        SbMetadataProps.Append('contentType=');
        SbMetadataProps.Append(ContentType);
        SbMetadataProps.Append(' ');
      end;
    end;

    if Headers.Contains(PROP_CONTENT_MD5) then begin
      ContentMd5 :=
        Headers.GetStringValue(PROP_CONTENT_MD5);
      // md5
      if ContentMd5.Length > 0 then begin
        SbMetadataProps.Append('md5=');
        SbMetadataProps.Append(ContentMd5);
        SbMetadataProps.Append(' ');
      end;
    end;

    if Headers.Contains(PROP_CONTENT_ENCODING) then begin
      ContentEncoding :=
        Headers.GetStringValue(PROP_CONTENT_ENCODING);
      // contentEncoding
      if ContentEncoding.Length > 0 then begin
        SbMetadataProps.Append('contentEncoding=');
        SbMetadataProps.Append(ContentEncoding);
        SbMetadataProps.Append(' ');
      end;
    end;
  end;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);
  PopulateObject(Kvp, ObjectName);

  ScriptTemplate := '';

  MetadataProps := SbMetadataProps.ToString;
  MetadataProps := MetadataProps.Trim;

  SbMetadataProps.Free;
  SbMetadataProps := nil;

  if MetadataProps.Length > 0 then begin
    ScriptTemplate := SCR_TEMPLATE_PUT_OBJECT_WITH_PROPERTIES + GetScriptSuffix;
    Kvp.AddPair(SCR_VAR_METADATA_PROPERTIES, MetadataProps);
  end
  else begin
    ScriptTemplate := SCR_TEMPLATE_PUT_OBJECT + GetScriptSuffix;
  end;

  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript) then begin
        ObjectAdded := true;
      end;
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  PutObjectFromFile := ObjectAdded;
end;

//*******************************************************************************

function TS3ExtStorageSystem.DeleteObject(ContainerName: String;
                                          ObjectName: String): Boolean;
var
  ObjectDeleted: Boolean;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('DeleteObject: Container=' + ContainerName +
            ', Object=' + ObjectName);
  end;

  ObjectDeleted := false;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);
  PopulateObject(Kvp, ObjectName);

  ScriptTemplate := SCR_TEMPLATE_DELETE_OBJECT + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript) then begin
        ObjectDeleted := true;
      end;
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  DeleteObject := ObjectDeleted;
end;

//*******************************************************************************

function TS3ExtStorageSystem.GetObject(ContainerName: String;
                                       ObjectName: String;
                                       LocalFilePath: String): Int64;
var
  Success: Boolean;
  Kvp: TKeyValuePairs;
  ScriptTemplate: String;
  RunScript: String;
begin
  if DebugMode then begin
    writeLn('GetObject: Container=' +
            ContainerName +
            ', Object=' +
            ObjectName +
            ', LocalFilePath=' +
            LocalFilePath);
  end;

  if LocalFilePath.Length = 0 then begin
    writeLn('error: local file path is empty');
    GetObject := 0;
    exit;
  end;

  Success := false;

  Kvp := TKeyValuePairs.Create;
  PopulateCommonVariables(Kvp);
  PopulateBucket(Kvp, ContainerName);
  PopulateObject(Kvp, ObjectName);
  Kvp.AddPair(SCR_VAR_OUTPUT_FILE, LocalFilePath);

  ScriptTemplate := SCR_TEMPLATE_GET_OBJECT + GetScriptSuffix;
  RunScript := JBPathJoin(ScriptDirectory,
                          RunScriptNameForTemplate(ScriptTemplate));

  try
    if PrepareRunScript(ScriptTemplate, RunScript, Kvp) then begin
      if RunProgram(RunScript) then begin
        Success := true;
      end;
    end;
  finally
    JBDeleteFileIfExists(RunScript);
    Kvp.Free;
  end;

  if Success and JBFileExists(LocalFilePath) then begin
    GetObject := JBGetFileSize(LocalFilePath);
  end
  else begin
    GetObject := 0;
  end;
end;

//*******************************************************************************

procedure TS3ExtStorageSystem.PopulateCommonVariables(Kvp: TKeyValuePairs);
begin
  Kvp.AddPair(SCR_VAR_S3_ENDPOINT_URL, EndpointUrl);
  Kvp.AddPair(SCR_VAR_S3_REGION, Region);
end;

//*****************************************************************************

procedure TS3ExtStorageSystem.PopulateBucket(Kvp: TKeyValuePairs;
                                             BucketName: String);
begin
  Kvp.AddPair(SCR_VAR_BUCKET_NAME, BucketName);
end;

//*****************************************************************************

procedure TS3ExtStorageSystem.PopulateObject(Kvp: TKeyValuePairs;
                                             ObjectName: String);
begin
  Kvp.AddPair(SCR_VAR_OBJECT_NAME, ObjectName);
end;

//*****************************************************************************

function TS3ExtStorageSystem.RunProgram(ProgramPath: String;
                                        ListOutputLines: TStringList): Boolean;
var
  StdOut: String;
  StdErr: String;
  Success: Boolean;
  IsShellScript: Boolean;
  ExecutablePath: String;
  FirstLine: String;
  ProgramArgs: TStringList;
  ExitCode: Integer;
  LineLength: Integer;
  FileLines: TStringList;
  OutputLines: TStringArray;
  OutputLine: String;
  i: Integer;
begin
  ProgramArgs := nil;
  FileLines := nil;
  OutputLines := nil;

  StdOut := '';
  StdErr := '';
  Success := false;

  if not JBFileExists(ProgramPath) then begin
    writeLn('RunProgram: error ' +
            ProgramPath +
            ' does not exist');
    RunProgram := false;
    exit;
  end;

  IsShellScript := false;
  ExecutablePath := ProgramPath;

  if ProgramPath.EndsWith(SFX_SHELL_SCRIPT) then begin
    FileLines := JBFileReadTextLines(ProgramPath);
    if FileLines.Count = 0 then begin
      FileLines.Free;
      FileLines := nil;
      writeLn('RunProgram: unable to read file ' +
              ProgramPath);
      RunProgram := false;
      exit;
    end;
    FirstLine := FileLines[0];
    if FirstLine.StartsWith('#!') then begin
      LineLength := FirstLine.Length;
      ExecutablePath := FirstLine.Substring(2, LineLength-2);
    end
    else begin
      ExecutablePath := DEFAULT_POSIX_SHELL;
    end;
    FileLines.Free;
    FileLines := nil;
    IsShellScript := true;
  end;

  ProgramArgs := TStringList.Create;
  ExitCode := 0;

  if IsShellScript then begin
    ProgramArgs.Add(ProgramPath);
  end;

  if JBExecuteProgram(ExecutablePath,
                      ProgramArgs,
                      ExitCode,
                      StdOut,
                      StdErr) then begin

    if DebugMode then begin
      writeLn('ExitCode = ' + IntToStr(ExitCode));
      writeLn('*********** START STDOUT **************');
      writeLn(StdOut);
      writeLn('*********** END STDOUT **************');
    end;

    if ExitCode = 0 then begin
      if StdOut.Length > 0 then begin
        OutputLines := StdOut.Split(LineEnding);
        for i := 0 to Length(OutputLines)-1 do begin
          OutputLine := OutputLines[i];
          if OutputLine.Length > 0 then begin
            ListOutputLines.Add(OutputLine);
          end;
        end;
      end;
      Success := true;
    end;
  end;

  ProgramArgs.Free;
  ProgramArgs := nil;

  RunProgram := Success;
end;

//*****************************************************************************

function TS3ExtStorageSystem.RunProgram(ProgramPath: String;
                                        out StdOut: String): Boolean;
var
  StdErr: String;
  Success: Boolean;
  IsShellScript: Boolean;
  ExecutablePath: String;
  FileLines: TStringList;
  FirstLine: String;
  LineLength: Integer;
  ProgramArgs: TStringList;
  ExitCode: Integer;
begin
  FileLines := nil;
  ProgramArgs := nil;

  StdOut := '';
  StdErr := '';
  Success := false;

  if not JBFileExists(ProgramPath) then begin
    writeLn('RunProgram: error ' +
            ProgramPath +
            ' does not exist');
    RunProgram := false;
    exit;
  end;

  IsShellScript := false;
  ExecutablePath := ProgramPath;

  if ProgramPath.EndsWith(SFX_SHELL_SCRIPT) then begin
    FileLines := JBFileReadTextLines(ProgramPath);
    if FileLines.Count = 0 then begin
      FileLines.Free;
      FileLines := nil;
      writeLn('RunProgram: unable to read file ' +
              ProgramPath);
      RunProgram := false;
      exit;
    end;
    FirstLine := FileLines[0];
    if FirstLine.StartsWith('#!') then begin
      LineLength := FirstLine.Length;
      ExecutablePath := FirstLine.Substring(2, LineLength-2);
    end
    else begin
      ExecutablePath := DEFAULT_POSIX_SHELL;
    end;
    FileLines.Free;
    FileLines := nil;
    IsShellScript := true;
  end;

  ProgramArgs := TStringList.Create;
  ExitCode := 0;

  if IsShellScript then begin
    ProgramArgs.Add(ProgramPath);
  end;

  if JBExecuteProgram(ExecutablePath,
                      ProgramArgs,
                      ExitCode,
                      StdOut,
                      StdErr) then begin
    if ExitCode = 0 then begin
      Success := true;
    end;
  end;

  ProgramArgs.Free;

  RunProgram := Success;
end;

//*****************************************************************************

function TS3ExtStorageSystem.RunProgram(ProgramPath: String): Boolean;
var
  StdOut: String;
  StdErr: String;
  Success: Boolean;
  IsShellScript: Boolean;
  ExecutablePath: String;
  FirstLine: String;
  ProgramArgs: TStringList;
  ExitCode: Integer;
  FileLines: TStringList;
  LineLength: Integer;
begin
  ProgramArgs := nil;
  FileLines := nil;

  StdOut := '';
  StdErr := '';
  Success := false;

  if not JBFileExists(ProgramPath) then begin
    writeLn('RunProgram: error ' + ProgramPath + ' does not exist');
    RunProgram := false;
    exit;
  end;

  IsShellScript := false;
  ExecutablePath := ProgramPath;

  if ProgramPath.EndsWith(SFX_SHELL_SCRIPT) then begin
    FileLines := JBFileReadTextLines(ProgramPath);
    if FileLines.Count = 0 then begin
      FileLines.Free;
      FileLines := nil;
      writeLn('RunProgram: unable to read file ' +
              ProgramPath);
      RunProgram := false;
      exit;
    end;
    FirstLine := FileLines[0];
    if FirstLine.StartsWith('#!') then begin
      LineLength := FirstLine.Length;
      ExecutablePath := FirstLine.Substring(2, LineLength-2);
    end
    else begin
      ExecutablePath := DEFAULT_POSIX_SHELL;
    end;
    FileLines.Free;
    FileLines := nil;
    IsShellScript := true;
  end;

  ProgramArgs := TStringList.Create;
  ExitCode := 0;

  if IsShellScript then begin
    ProgramArgs.Add(ProgramPath);
  end;

  if JBExecuteProgram(ExecutablePath,
                      ProgramArgs,
                      ExitCode,
                      StdOut,
                      StdErr) then begin
    if ExitCode = 0 then begin
      Success := true;
    end;
  end;

  ProgramArgs.Free;

  RunProgram := Success;
end;

//*****************************************************************************

function TS3ExtStorageSystem.PrepareRunScript(ScriptTemplate: String;
                                              RunScript: String;
                                              Kvp: TKeyValuePairs): Boolean;
var
  SourceFile: String;
  FileText: String;
  KvpKeys: TStringList;
  KvpKey: String;
  KvpValue: String;
  i: Integer;
begin
  KvpKeys := nil;

  JBDeleteFileIfExists(RunScript);

  SourceFile := JBPathJoin(ScriptDirectory, ScriptTemplate);
  if not JBFileExists(SourceFile) then begin
    writeLn('error: source file does not exist ' + SourceFile);
    PrepareRunScript := false;
    exit;
  end;

  if not JBFileCopy(SourceFile, RunScript) then begin
    PrepareRunScript := false;
    exit;
  end;

  FileText := JBFileReadAllText(RunScript);
  if FileText.Length = 0 then begin
    PrepareRunScript := false;
    exit;
  end;

  KvpKeys := Kvp.GetKeys;
  for i := 0 to KvpKeys.Count-1 do begin
    KvpKey := KvpKeys[i];
    KvpValue := Kvp.GetValue(KvpKey);
    FileText := FileText.Replace(KvpKey, KvpValue);
  end;
  KvpKeys.Free;
  KvpKeys := nil;

  if not JBFileWriteAllText(RunScript, FileText) then begin
    PrepareRunScript := false;
    exit;
  end;

  PrepareRunScript := true;
end;

//*****************************************************************************

function TS3ExtStorageSystem.RunScriptNameForTemplate(ScriptTemplate: String): String;
begin
  RunScriptNameForTemplate := PREFIX_RUN_SCRIPT_NAME + ScriptTemplate;
end;

//*****************************************************************************

function TS3ExtStorageSystem.GetScriptSuffix: String;
begin
  {$IFDEF WINDOWS}
  GetScriptSuffix := SFX_BATCH_FILE;
  {$ELSE}
  GetScriptSuffix := SFX_SHELL_SCRIPT;
  {$ENDIF}
end;

//*****************************************************************************

end.

