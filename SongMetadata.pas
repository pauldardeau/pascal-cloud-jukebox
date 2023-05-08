unit SongMetadata;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, FileMetadata, SysUtils;

type
  TSongMetadata = Class(TObject)
  public
    Fm: TFileMetadata;
    ArtistUid: String;
    ArtistName: String;
    AlbumUid: String;
    SongName: String;

  public
    constructor Create;
    constructor Create(aFm: TFileMetadata);
    destructor Destroy; override;
  end;

implementation

//*******************************************************************************

constructor TSongMetadata.Create;
begin
  inherited;
  Fm := TFileMetadata.Create;
  ArtistUid := '';
  ArtistName := '';
  AlbumUid := '';
  SongName := '';
end;

//*******************************************************************************

constructor TSongMetadata.Create(aFm: TFileMetadata);
begin
  inherited Create;
  Fm := aFm;
  ArtistUid := '';
  ArtistName := '';
  AlbumUid := '';
  SongName := '';
end;

//*******************************************************************************

destructor TSongMetadata.Destroy;
begin
  writeLn('TSongMetadata.Destroy');
  Fm.Free;
  inherited;
end;

//*******************************************************************************

end.

