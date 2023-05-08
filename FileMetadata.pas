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
  writeLn('TFileMetadata.Destroy');
  inherited;
end;

//*******************************************************************************

end.

