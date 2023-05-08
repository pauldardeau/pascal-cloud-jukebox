unit StorageSystem;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, PropertySet, SysUtils;

type
  TStorageSystem = Class(TObject)
  public
    function Enter: Boolean; virtual; abstract;
    procedure Leave; virtual; abstract;
    function ListAccountContainers: TStringList; virtual; abstract;
    function GetContainerNames: TStringList; virtual; abstract;
    function HasContainer(ContainerName: String): Boolean; virtual; abstract;
    function CreateContainer(ContainerName: String): Boolean; virtual; abstract;
    function DeleteContainer(ContainerName: String): Boolean; virtual; abstract;
    function ListContainerContents(ContainerName: String): TStringList; virtual; abstract;
    function GetObjectMetadata(ContainerName: String;
                               ObjectName: String;
                               DictProps: TPropertySet): Boolean; virtual; abstract;
    function PutObject(ContainerName: String;
                       ObjectName: String;
                       FileContents: TMemoryStream;
                       Headers: TPropertySet): Boolean; virtual; abstract;
    function PutObjectFromFile(ContainerName: String;
                               ObjectName: String;
                               FilePath: String;
                               Headers: TPropertySet): Boolean; virtual; abstract;
    function DeleteObject(ContainerName: String;
                          ObjectName: String): Boolean; virtual; abstract;
    function GetObject(ContainerName: String;
                       ObjectName: String;
                       LocalFilePath: String): Int64; virtual; abstract;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

end.

