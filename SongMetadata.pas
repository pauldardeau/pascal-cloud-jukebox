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
    constructor Create(otherSm: TSongMetadata);
    destructor Destroy; override;
    function IsEqualTo(Song: TSongMetadata): Boolean;
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

constructor TSongMetadata.Create(otherSm: TSongMetadata);
begin
  inherited Create;
  Fm := TFileMetadata.Create(otherSm.Fm);
  ArtistUid := otherSm.ArtistUid;
  ArtistName := otherSm.ArtistName;
  AlbumUid := otherSm.AlbumUid;
  SongName := otherSm.SongName;
end;

//*******************************************************************************

destructor TSongMetadata.Destroy;
begin
  Fm.Free;
  inherited;
end;

//*******************************************************************************

function TSongMetadata.IsEqualTo(Song: TSongMetadata): Boolean;
begin
  IsEqualTo := Fm.IsEqualTo(Song.Fm) and
               (ArtistUid = Song.ArtistUid) and
               (ArtistName = Song.ArtistName) and
               (AlbumUid = Song.AlbumUid) and
               (SongName = Song.SongName);
end;

//*******************************************************************************

end.

