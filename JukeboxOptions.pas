unit JukeboxOptions;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, SysUtils;

type
  TJukeboxOptions = Class(TObject)
  public
    DebugMode: Boolean;
    CheckDataIntegrity: Boolean;
    FileCacheCount: Integer;
    NumberSongs: Integer;
    SuppressMetadataDownload: Boolean;
    Directory: String;

  public
    constructor Create;
	destructor Destroy; override;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TJukeboxOptions.Create;
begin
  inherited;
  DebugMode := false;
  CheckDataIntegrity := false;
  FileCacheCount := 3;
  NumberSongs := 0;
  SuppressMetadataDownload := false;
  Directory := '';
end;

//*******************************************************************************

destructor TJukeboxOptions.Destroy;
begin
  writeLn('TJukeboxOptions.Destroy');
  inherited;
end;

//*******************************************************************************

end.

