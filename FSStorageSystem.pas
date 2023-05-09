unit FSStorageSystem;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, JBSysUtils, PropertySet, StorageSystem, SysUtils;

const
  METADATA_FILE_SUFFIX = '.meta';

type
  TFSStorageSystem = Class(TStorageSystem)
  private
    RootDir: String;
    DebugMode: Boolean;

  public
    constructor Create(aRootDir: String;
                       aDebugMode: Boolean);
    destructor Destroy; override;
	
	function Enter: Boolean; override;
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

  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TFSStorageSystem.Create(aRootDir: String;
                                    aDebugMode: Boolean);
begin
  inherited Create;
  RootDir := aRootDir;
  DebugMode := aDebugMode;
end;

//*******************************************************************************

destructor TFSStorageSystem.Destroy;
begin
  writeLn('TFSStorageSystem.Destroy');
  inherited;
end;

//*******************************************************************************

function TFSStorageSystem.Enter: Boolean;
begin
  if not JBDirectoryExists(RootDir) then
    Enter := JBCreateDirectory(RootDir)
  else
    Enter := true;
end;

//*******************************************************************************

procedure TFSStorageSystem.Leave;
begin
  // nothing to do
end;

//*******************************************************************************

function TFSStorageSystem.ListAccountContainers: TStringList;
begin
  ListAccountContainers := JBListDirsInDirectory(RootDir);
end;

//*******************************************************************************

function TFSStorageSystem.GetContainerNames: TStringList;
begin
  GetContainerNames := ListAccountContainers;
end;

//*******************************************************************************

function TFSStorageSystem.HasContainer(ContainerName: String): Boolean;
var
  ListContainers: TStringList;
  i: Integer;
begin
  ListContainers := ListAccountContainers;
  if ListContainers.Count > 0 then begin
    for i := 0 to ListContainers.Count-1 do begin
      if ContainerName = ListContainers[i] then begin
        HasContainer := true;
		exit;
      end;
    end;
  end;
  HasContainer := false;
end;

//*******************************************************************************

function TFSStorageSystem.CreateContainer(ContainerName: String): Boolean;
var
  ContainerCreated: Boolean;
  ContainerDir: String;
begin
  ContainerDir := JBPathJoin(RootDir, ContainerName);
  ContainerCreated := JBCreateDirectory(ContainerDir);
  if ContainerCreated then begin
    if DebugMode then begin
      writeLn('container created: ' + ContainerName);
    end;
  end;
  CreateContainer := ContainerCreated;
end;

//*******************************************************************************

function TFSStorageSystem.DeleteContainer(ContainerName: String): Boolean;
var
  ContainerDeleted: Boolean;
  ContainerDir: String;
begin
  ContainerDir := JBPathJoin(RootDir, ContainerName);
  ContainerDeleted := JBDeleteDirectory(ContainerDir);
  if ContainerDeleted then begin
    if DebugMode then begin
      writeLn('container deleted: ' + ContainerName);
    end;
  end;

  DeleteContainer := ContainerDeleted;
end;

//*******************************************************************************

function TFSStorageSystem.ListContainerContents(ContainerName: String): TStringList;
var
  ContainerContents: TStringList;
  ContainerDir: String;
begin
  ContainerDir := JBPathJoin(RootDir, ContainerName);
  if JBDirectoryExists(ContainerDir) then
    ContainerContents := JBListFilesInDirectory(ContainerDir)
  else
    ContainerContents := TStringList.Create;

  ListContainerContents := ContainerContents;
end;

//*******************************************************************************

function TFSStorageSystem.GetObjectMetadata(ContainerName: String;
                                            ObjectName: String;
                                            DictProps: TPropertySet): Boolean;
var
  RetrievedMetadata: Boolean;
  ContainerDir: String;
  ObjectPath: String;
  MetaPath: String;
begin
  RetrievedMetadata := false;
  if (ContainerName.Length > 0) and (ObjectName.Length > 0) then begin
    ContainerDir := JBPathJoin(RootDir, ContainerName);
    if JBDirectoryExists(ContainerDir) then begin
      ObjectPath := JBPathJoin(ContainerDir, ObjectName);
      MetaPath := ObjectPath + METADATA_FILE_SUFFIX;
      if JBFileExists(MetaPath) then begin
        RetrievedMetadata := DictProps.ReadFromFile(MetaPath);
      end;
    end;
  end;

  GetObjectMetadata := RetrievedMetadata;
end;

//*******************************************************************************

function TFSStorageSystem.PutObject(ContainerName: String;
                                    ObjectName: String;
                                    FileContents: TMemoryStream;
                                    Headers: TPropertySet): Boolean;
var
  ObjectAdded: Boolean;
  ContainerDir: String;
  ObjectPath: String;
  MetaPath: String;
begin
  ObjectAdded := false;
  if (ContainerName.Length > 0) and
     (ObjectName.Length > 0) and
     (FileContents.length > 0) then begin

    ContainerDir := JBPathJoin(RootDir, ContainerName);
    if JBDirectoryExists(ContainerDir) then begin
      ObjectPath := JBPathJoin(ContainerDir, ObjectName);
      ObjectAdded := JBFileWriteAllBytes(ObjectPath, FileContents);
      if ObjectAdded then begin
        if DebugMode then begin
          writeLn('object added: ' + ContainerName + '/' + ObjectName);
        end;
        if Headers <> nil then begin
          if Headers.Count > 0 then begin
            MetaPath := ObjectPath + METADATA_FILE_SUFFIX;
            Headers.WriteToFile(MetaPath);
          end;
        end;
      end
      else begin
        writeLn('FileWriteAllBytes failed to write object contents, put failed');
      end;
    end
    else begin
      if DebugMode then begin
        writeLn('container does not exist, cannot put object');
      end;
    end;
  end
  else begin
    if DebugMode then begin
      if ContainerName.Length = 0 then begin
        writeLn('container name is missing, cannot put object');
      end
      else begin
        if ObjectName.Length = 0 then begin
          writeLn('object name is missing, cannot put object');
        end
        else begin
          if FileContents.Count = 0 then begin
            writeLn('object content is empty, cannot put object');
          end;
        end;
      end;
    end;
  end;

  PutObject := ObjectAdded;
end;

//*******************************************************************************

function TFSStorageSystem.PutObjectFromFile(ContainerName: String;
                                            ObjectName: String;
                                            FilePath: String;
                                            Headers: TPropertySet): Boolean;
var
  ObjectAdded: Boolean;
  ContainerDir: String;
  ObjectPath: String;
  MetaPath: String;
begin
  ObjectAdded := false;

  if (ContainerName.Length > 0) and
     (ObjectName.Length > 0) and
     (FilePath.Length > 0) then begin

    ContainerDir := JBPathJoin(RootDir, ContainerName);
    if JBDirectoryExists(ContainerDir) then begin
      ObjectPath := JBPathJoin(ContainerDir, ObjectName);
      ObjectAdded := JBFileCopy(FilePath, ObjectPath);
      if ObjectAdded then begin
        if DebugMode then begin
          writeLn('object added: ' + ContainerName + '/' + ObjectName);
        end;
        if Headers <> nil then begin
          if Headers.Count > 0 then begin
            MetaPath := ObjectPath + METADATA_FILE_SUFFIX;
            Headers.WriteToFile(MetaPath);
          end;
        end;
      end
      else begin
        writeLn('FileCopy failed to copy object contents, put failed');
      end;
    end
    else begin
      if DebugMode then begin
        writeLn('container does not exist, cannot put object');
      end;
    end;
  end
  else begin
    if DebugMode then begin
      if ContainerName.Length = 0 then begin
        writeLn('container name is missing, cannot put object');
      end;
      if ObjectName.Length = 0 then begin
        writeLn('object name is missing, cannot put object');
      end;
      if FilePath.Length = 0 then begin
        writeLn('object file path is empty, cannot put object');
      end;
    end;
  end;

  PutObjectFromFile := ObjectAdded;
end;

//*******************************************************************************

function TFSStorageSystem.DeleteObject(ContainerName: String;
                                       ObjectName: String): Boolean;
var
  ObjectDeleted: Boolean;
  ContainerDir: String;
  ObjectPath: String;
  MetaPath: String;
begin
  ObjectDeleted := false;
  if ContainerName.Length > 0 and ObjectName.Length > 0 then begin
    ContainerDir := JBPathJoin(RootDir, ContainerName);
    ObjectPath := JBPathJoin(ContainerDir, ObjectName);

    if JBFileExists(ObjectPath) then begin
      ObjectDeleted := JBDeleteFile(ObjectPath);
      if ObjectDeleted then begin
        if DebugMode then begin
          writeLn('object deleted: ' + ContainerName + '/' + ObjectName);
        end;
        MetaPath := ObjectPath + METADATA_FILE_SUFFIX;
        if JBFileExists(MetaPath) then begin
          JBDeleteFile(MetaPath);
        end;
      end
      else begin
        if DebugMode then begin
          writeLn('delete of object file failed');
        end;
      end;
    end
    else begin
      if DebugMode then begin
        writeLn('cannot delete object, path does not exist');
      end;
    end;
  end
  else begin
    if DebugMode then begin
      writeLn('cannot delete object, container name or object name is missing');
    end;
  end;

  DeleteObject := ObjectDeleted;
end;

//*******************************************************************************

function TFSStorageSystem.GetObject(ContainerName: String;
                                    ObjectName: String;
                                    LocalFilePath: String): Int64;
var
  BytesRetrieved: Int64;
  ContainerDir: String;
  ObjectPath: String;
  ObjFileContents: TMemoryStream;
begin
  BytesRetrieved := 0;
  ObjFileContents := nil;

  if (ContainerName.Length > 0) and
     (ObjectName.Length > 0) and
     (LocalFilePath.Length > 0) then begin

    ContainerDir := JBPathJoin(RootDir, ContainerName);
    ObjectPath := JBPathJoin(ContainerDir, ObjectName);

    if JBFileExists(ObjectPath) then begin
	  try
        ObjFileContents := JBFileReadAllBytes(ObjectPath);
        if ObjFileContents.Count > 0 then begin
          if DebugMode then begin
            writeLn('attempting to write object to ' + LocalFilePath);
          end;
          if JBFileWriteAllBytes(LocalFilePath, ObjFileContents) then begin
            BytesRetrieved := Int64(ObjFileContents.Count);
          end;
        end
        else begin
          writeLn('error: unable to read object file ' + ObjectPath);
        end;
	  finally
	    if ObjFileContents <> nil then begin
		  ObjFileContents.Free;
		  ObjFileContents := nil;
		end;
	  end;
    end;
  end;

  GetObject := BytesRetrieved;
end;

//*******************************************************************************

end.
