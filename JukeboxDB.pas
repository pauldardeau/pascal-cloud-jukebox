unit JukeboxDB;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, FileMetadata, JBUtils, SongMetadata, SysUtils, SQLDB, SQLite3Conn;

const
  STRING_OF_SINGLE_QUOTE = '#39';  // #39 is single quote character

type
  TListSongMetadata = specialize TFPGObjectList<TSongMetadata>;

  TJukeboxDB = Class(TObject)
  private
    DebugPrint: Boolean;
    DbConnection: TSQLite3Connection;
    DbTrans: TSQLTransaction;
    DbQuery: TSQLQuery;
    MetadataDbFilePath: String;
    InTransaction: Boolean;

  public
    constructor Create(aMetadataDbFilePath: String; aDebugPrint: Boolean);
    destructor Destroy; override;
    function IsOpen: Boolean;
    function Open: Boolean;
    function Close: Boolean;
    function Enter: Boolean;
    procedure Leave;
    function BeginTransaction: Boolean;
    function Rollback: Boolean;
    function Commit: Boolean;
    function HaveTables: Boolean;
    function GetPlaylist(PlaylistName: String): String;
    function SongsForArtist(ArtistName: String): TListSongMetadata;
    function RetrieveSong(SongUid: String): TSongMetadata;
    function RetrieveSongs(Artist: String; Album: String): TListSongMetadata;
    procedure ShowListings;
    procedure ShowArtists;
    procedure ShowGenres;
    procedure ShowAlbums;
    procedure ShowPlaylists;
	function SongsForQueryResults(QueryResults: TSQLQuery): TListSongMetadata;
	function SqlWhereClause: String;
  end;

implementation

//*******************************************************************************

constructor TJukeboxDB.Create(aMetadataDbFilePath: String; aDebugPrint: Boolean);
begin
  inherited Create;
  DebugPrint := aDebugPrint;
  DbConnection := TSQLite3Connection.Create(nil);
  MetadataDbFilePath := aMetadataDbFilePath;
  InTransaction := false;
end;

//*******************************************************************************

destructor TJukeboxDB.Destroy;
begin
  writeLn('TJukeboxDB.Destroy');
  Close;
  inherited;
end;

//*******************************************************************************

function TJukeboxDB.IsOpen: Boolean;
begin
  IsOpen := DbConnection.Connected;
end;

//*******************************************************************************

function TJukeboxDB.Open: Boolean;
var
  DidOpen: Boolean;
begin
  DidOpen := false;
  if not IsOpen then begin
    DbConnection.DatabaseName := MetadataDbFilePath;
    DbConnection.HostName := 'localhost';
    DbConnection.CharSet := 'UTF8';
    try
      DbConnection.Open;
      DbTrans := TSQLTransaction.Create(nil);
      DbQuery := TSQLQuery.Create(nil);
      DbConnection.Transaction := DbTrans;
      DbTrans.Database := DbConnection;
      DbQuery.Database := DbConnection;
      DbQuery.Transaction := DbTrans;
      DidOpen := true;
    except
      on E: Exception do begin
      end;
    end;
  end;
  Open := DidOpen;
end;

//*******************************************************************************

function TJukeboxDB.Close: Boolean;
var
  DidClose: Boolean;
begin
  DidClose := false;
  if DbConnection.Connected then begin
    DbTrans.Commit;
    DbQuery.Close;
    DbConnection.Close;

    DbQuery.Free;
    DbTrans.Free;
    DbConnection.Free;

    DbTrans := nil;
    DbQuery := nil;
    DbConnection := nil;

    DidClose := true;
  end;
  Close := DidClose;
end;

//*******************************************************************************

function TJukeboxDB.Enter: Boolean;
var
  DidEnter: Boolean;
begin
  DidEnter := false;

  if Open then begin
    if DbConnection.Connected then begin
      if DebugPrint then begin
        writeLn('have db connection');
      end;
    end;
    DidEnter := true;
  end
  else begin
    writeLn('unable to connect to database');
  end;

  Enter := DidEnter;
end;

//*******************************************************************************

procedure TJukeboxDB.Leave;
begin
  Close;
end;

//*******************************************************************************

function TJukeboxDB.BeginTransaction: Boolean;
var
  DidBeginTransaction: Boolean;
begin
  DidBeginTransaction := false;
  if not InTransaction then begin
    DbTrans.Active := true;
  end;
  BeginTransaction := DidBeginTransaction;
end;

//*******************************************************************************

function TJukeboxDB.Rollback: Boolean;
var
  DidRollback: Boolean;
begin
  DidRollback := false;
  if InTransaction then begin
    DbTrans.Rollback;
    DbTrans.Active := false;
    DidRollback := true;
    InTransaction := false;
  end;
  Rollback := DidRollback;
end;

//*******************************************************************************

function TJukeboxDB.Commit: Boolean;
var
  DidCommit: Boolean;
begin
  DidCommit := false;
  if InTransaction then begin
    DbTrans.Commit;
    DbTrans.Active := false;
    DidCommit := true;
    InTransaction := false;
  end;
  Commit := DidCommit;
end;

//*******************************************************************************

function TJukeboxDB.HaveTables: Boolean;
var
  Sql: String;
  HaveTablesInDb: Boolean;
  RowCount: Integer;
begin
  HaveTablesInDb := false;
  if IsOpen then begin
    Sql := 'SELECT COUNT(*) ' +
           'FROM sqlite_master ' +
           'WHERE type="table" AND name="song"';
    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      if not DbQuery.EOF then begin
        RowCount := DbQuery.Fields.Fields[0].AsInteger;
        if RowCount > 0 then begin
          HaveTablesInDb := true;
        end;
      end;
    end;

    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end; 
  HaveTables := HaveTablesInDb;
end;

//*******************************************************************************

function TJukeboxDB.GetPlaylist(PlaylistName: String): String;
var
  Sql: String;
  PlObject: String;
begin
  PlObject := '';
  if PlaylistName.Length > 0 then begin
    If IsOpen then begin
      Sql := 'SELECT playlist_uid ' +
             'FROM playlist ' +
             'WHERE playlist_name = :playlist_name';
      DbQuery.SQL.Text := Sql;
      DbQuery.Params.ParamByName('playlist_name').AsString := PlaylistName;
      BeginTransaction;
      DbQuery.Open;

      if DbQuery.Active then begin
        if not DbQuery.EOF then begin
          PlObject := DbQuery.Fields.Fields[0].AsString;
        end;
      end;

      Commit;
      DbQuery.Close;
    end
    else begin
      writeLn('error: DbConnection is closed');
    end;
  end;
  GetPlaylist := PlObject;
end;

//*******************************************************************************

function TJukeboxDB.SongsForArtist(ArtistName: String): TListSongMetadata;
var
  Songs: TListSongMetadata;
  SqlQuery: String;
begin
  Songs := TListSongMetadata.Create;
  if DbConnection <> nil then begin
    SqlQuery := 'SELECT song_uid,' +
                       'file_time,' +
                       'origin_file size,' +
                       'stored_file size,' +
                       'pad_char_count,' +
                       'artist_name,' +
                       'artist_uid,' +
                       'song_name,' +
                       'md5_hash,' +
                       'compressed,' +
                       'encrypted,' +
                       'container_name,' +
                       'object_name,' +
                       'album_uid ' +
                'FROM song' +
                SqlWhereClause +
                ' AND artist = :artist_name';
    
    DbQuery.SQL.Text := SqlQuery;
	DbQuery.Params.ParamByName('artist_name').AsString := ArtistName;
    BeginTransaction;
    DbQuery.Open;

    Songs := SongsForQueryResults(DbQuery);
  end;

  SongsForArtist := Songs;
end;

//*******************************************************************************

function TJukeboxDB.RetrieveSong(SongUid: String): TSongMetadata;
var
  Song: TSongMetadata;
  SqlQuery: String;
  QueryResults: TListSongMetadata;
begin
  Song := nil;

  if DbConnection <> nil then begin
    SqlQuery := 'SELECT song_uid,' +
                       'file_time,' +
                       'origin_file_size,' +
                       'stored_file_size,' +
                       'pad_char_count,' +
                       'artist_name,' +
                       'artist_uid,' +
                       'song_name,' +
                       'md5_hash,' +
                       'compressed,' +
                       'encrypted,' +
                       'container_name,' +
                       'object_name,' +
                       'album_uid ' +
                'FROM song ' +
                'WHERE song_uid = :song_uid';

    DbQuery.SQL.Text := SqlQuery;
	DbQuery.Params.ParamByName('song_uid').AsString := SongUid;
    BeginTransaction;
    DbQuery.Open;
    
    QueryResults := SongsForQueryResults(DbQuery);
    if QueryResults.Count > 0 then begin
      Song := QueryResults[0];
    end;
  end;

  RetrieveSong := Song;
end;

//*******************************************************************************

function TJukeboxDB.RetrieveSongs(Artist: String; Album: String): TListSongMetadata;
var
  Songs: TListSongMetadata;
  SqlQuery: String;
  AddedClause: String;
  EncodedArtist: String;
  EncodedAlbum: String;
begin
  Songs := TListSongMetadata.Create;
  if DbConnection <> nil then begin
    SqlQuery := 'SELECT song_uid,' +
                       'file_time,' +
                       'origin_file_size,' +
                       'stored_file_size,' +
                       'pad_char_count,' +
                       'artist_name,' +
                       'artist_uid,' +
                       'song_name,' +
                       'md5_hash,' +
                       'compressed,' +
                       'encrypted,' +
                       'container_name,' +
                       'object_name,' +
                       'album_uid ' +
                'FROM song';

    SqlQuery := SqlQuery + SqlWhereClause;
    
    if Artist.Length > 0 then begin
      EncodedArtist := EncodeValue(Artist);
      if Album.Length > 0 then begin
        EncodedAlbum := EncodeValue(Album);
        AddedClause := ' AND object_name LIKE ' +
		               STRING_OF_SINGLE_QUOTE +
		               EncodedArtist +
					   '--' +
					   EncodedAlbum +
					   '%' +
					   STRING_OF_SINGLE_QUOTE;
      end
      else begin
        AddedClause := ' AND object_name LIKE ' +
		               STRING_OF_SINGLE_QUOTE +
		               EncodedArtist +
					   '--%' +
					   STRING_OF_SINGLE_QUOTE;
      end;
      SqlQuery := SqlQuery + AddedClause;
    end;

    if DebugPrint then begin
      writeLn('executing query: ' + SqlQuery);
    end;

    DbQuery.SQL.Text := SqlQuery;
    BeginTransaction;
    DbQuery.Open;

    Songs := SongsForQueryResults(DbQuery);
  end;
  
  RetrieveSongs := Songs;
end;

//*******************************************************************************

procedure TJukeboxDB.ShowListings;
var
  Sql: String;
  ArtistName: String;
  SongName: String;
begin
  if IsOpen then begin
    Sql := 'SELECT artist_name, song_name ' +
           'FROM song ' +
           'ORDER BY artist_name, song_name';
    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      while not DbQuery.EOF do begin
        ArtistName := DbQuery.Fields.Fields[0].AsString;
        SongName := DbQuery.Fields.Fields[1].AsString;
        writeLn(ArtistName + ' - ' + SongName);
        DbQuery.Next;
      end;
    end;
    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end;
end;

//*******************************************************************************

procedure TJukeboxDB.ShowArtists;
var
  Sql: String;
begin
  if IsOpen then begin
    Sql := 'SELECT DISTINCT artist_name ' +
           'FROM song ' +
           'ORDER BY artist_name';

    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      while not DbQuery.EOF do begin
        writeLn(DbQuery.Fields.Fields[0].AsString);
        DbQuery.Next;
      end;
    end;
    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end;
end;

//*******************************************************************************

procedure TJukeboxDB.ShowGenres;
var
  Sql: String;
begin
  if IsOpen then begin
    Sql := 'SELECT genre_name ' +
           'FROM genre ' +
           'ORDER BY genre_name';

    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      while not DbQuery.EOF do begin
        writeLn(DbQuery.Fields.Fields[0].AsString);
        DbQuery.Next;
      end;
    end;
    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end;
end;

//*******************************************************************************

procedure TJukeboxDB.ShowAlbums;
var
  Sql: String;
  AlbumName: String;
  ArtistName: String;
begin
  if IsOpen then begin
    Sql := 'SELECT album.album_name, artist.artist_name ' +
           'FROM album, artist ' +
           'WHERE album.artist_uid = artist.artist_uid ' +
           'ORDER BY album.album_name';

    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      while not DbQuery.EOF do begin
        AlbumName := DbQuery.Fields.Fields[0].AsString;
        ArtistName := DbQuery.Fields.Fields[1].AsString;
        writeLn(AlbumName + ' (' + ArtistName + ')');
        DbQuery.Next;
      end;
    end;
    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end;
end;

//*******************************************************************************

procedure TJukeboxDB.ShowPlaylists;
var
  Sql: String;
  PlUid: String;
  PlName: String;
begin
  if IsOpen then begin
    Sql := 'SELECT playlist_uid, playlist_name ' +
           'FROM playlist ' +
           'ORDER BY playlist_uid';

    DbQuery.SQL.Text := Sql;
    BeginTransaction;
    DbQuery.Open;

    if DbQuery.Active then begin
      while not DbQuery.EOF do begin
        PlUid := DbQuery.Fields.Fields[0].AsString;
        PlName := DbQuery.Fields.Fields[1].AsString;
        writeLn(PlUid + ' - ' + PlName);
        DbQuery.Next;
      end;
    end;
    Commit;
    DbQuery.Close;
  end
  else begin
    writeLn('error: DbConnection is closed');
  end;
end;

//*******************************************************************************

function TJukeboxDB.SongsForQueryResults(QueryResults: TSQLQuery): TListSongMetadata;
var
  ResultSongs: TListSongMetadata;
  song: TSongMetadata;
begin
  ResultSongs := TListSongMetadata.Create;
  
  try
    if QueryResults.Active then begin
      while not QueryResults.EOF do begin
        song := TSongMetadata.Create;
        song.Fm.FileUid := QueryResults.Fields.Fields[0].AsString;
        song.Fm.FileTime := QueryResults.Fields.Fields[1].AsString;
        song.Fm.OriginFileSize := QueryResults.Fields.Fields[2].AsInteger;
        song.Fm.StoredFileSize := QueryResults.Fields.Fields[3].AsInteger;
        song.Fm.PadCharCount := QueryResults.Fields.Fields[4].AsInteger;
        song.ArtistName := QueryResults.Fields.Fields[5].AsString;
        song.ArtistUid := QueryResults.Fields.Fields[6].AsString;
        song.SongName := QueryResults.Fields.Fields[7].AsString;
        song.Fm.Md5Hash := QueryResults.Fields.Fields[8].AsString;
        song.Fm.Compressed := QueryResults.Fields.Fields[9].AsInteger = 1;
        song.Fm.Encrypted := QueryResults.Fields.Fields[10].AsInteger = 1;
        song.Fm.ContainerName := QueryResults.Fields.Fields[11].AsString;
        song.Fm.ObjectName := QueryResults.Fields.Fields[12].AsString;
        song.AlbumUid := QueryResults.Fields.Fields[13].AsString;
        ResultSongs.Add(song);
        
		QueryResults.Next;
      end;
    end;
  finally
    Commit;
    QueryResults.Close;
  end;

  SongsForQueryResults := ResultSongs;
end;

//*******************************************************************************

function TJukeboxDB.SqlWhereClause: String;
begin
  SqlWhereClause := ' WHERE encrypted = 0';
end;

//*******************************************************************************

end.

