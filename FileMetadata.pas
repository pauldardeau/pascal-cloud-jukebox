unit FileMetadata;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, SysUtils;

type
  TFileMetadata = Class(TObject)
  public
    FileUid: String;
    FileName: String;
    OriginFileSize: Integer;
    StoredFileSize: Integer;
    PadCharCount: Integer;
    FileTime: String;
    Md5Hash: String;
    Compressed: Boolean;
    Encrypted: Boolean;
    ContainerName: String;
    ObjectName: String;

  public
    constructor Create;
    destructor Destroy; override;
    function IsEqualTo(aFm: TFileMetadata): Boolean;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TFileMetadata.Create;
begin
  inherited;
  FileUid := '';
  FileName := '';
  OriginFileSize := 0;
  StoredFileSize := 0;
  PadCharCount := 0;
  FileTime := '';
  Md5Hash := '';
  Compressed := false;
  Encrypted := false;
  ContainerName := '';
  ObjectName := '';
end;

//*******************************************************************************

destructor TFileMetadata.Destroy;
begin
  inherited;
end;

//*******************************************************************************

function TFileMetadata.IsEqualTo(aFm: TFileMetadata): Boolean;
begin
  IsEqualTo := (FileUid = aFm.FileUid) and
               (FileName = aFm.FileName) and
               (OriginFileSize = aFm.OriginFileSize) and
               (StoredFileSize = aFm.StoredFileSize) and
               (PadCharCount = aFm.PadCharCount) and
               (FileTime = aFm.FileTime) and
               (Md5Hash = aFm.Md5Hash) and
               (Compressed = aFm.Compressed) and
               (Encrypted = aFm.Encrypted) and
               (ContainerName = aFm.ContainerName) and
               (ObjectName = aFm.ObjectName);
end;

//*******************************************************************************

end.

