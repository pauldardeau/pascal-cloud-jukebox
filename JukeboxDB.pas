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
    function CreateTable(SqlStatement: String): Boolean;
    function CreateTables: Boolean;
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
    function InsertSong(Song: TSongMetadata): Boolean;
    function UpdateSong(Song: TSongMetadata): Boolean;
    function StoreSongMetadata(Song: TSongMetadata): Boolean;
    function InsertPlaylist(PlUid: String;
                            PlName: String;
                            PlDesc: String): Boolean;
    function DeleteSong(SongUid: String): Boolean;
    function DeletePlaylist(PlName: String): Boolean;
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

function TJukeboxDB.CreateTable(SqlStatement: String): Boolean;
var
  DidSucceed: Boolean;
begin
  DidSucceed := false;

  //TODO: implement CreateTable

  CreateTable := DidSucceed;
end;

//*******************************************************************************

function TJukeboxDB.CreateTables: Boolean;
var
  DidSucceed: Boolean;
  CreateGenreTable: String;
  CreateArtistTable: String;
  CreateAlbumTable: String;
  CreateSongTable: String;
  CreatePlaylistTable: String;
  CreatePlaylistSongTable: String;
begin
  DidSucceed := false;

  if DbConnection <> nil then begin
    if DebugPrint then begin
      writeLn('creating tables');
    end;

    CreateGenreTable := 'CREATE TABLE genre (' +
                        'genre_uid TEXT UNIQUE NOT NULL, ' +
                        'genre_name TEXT UNIQUE NOT NULL, ' +
                        'genre_description TEXT);';

    CreateArtistTable := 'CREATE TABLE artist (' +
                         'artist_uid TEXT UNIQUE NOT NULL,' +
                         'artist_name TEXT UNIQUE NOT NULL,' +
                         'artist_description TEXT);';

    CreateAlbumTable := 'CREATE TABLE album (' +
                        'album_uid TEXT UNIQUE NOT NULL,' +
                        'album_name TEXT UNIQUE NOT NULL,' +
                        'album_description TEXT,' +
                        'artist_uid TEXT NOT NULL REFERENCES artist(artist_uid),' +
                        'genre_uid TEXT REFERENCES genre(genre_uid));';

    CreateSongTable := 'CREATE TABLE song (' +
                       'song_uid TEXT UNIQUE NOT NULL,' +
                       'file_time TEXT,' +
                       'origin_file_size INTEGER,' +
                       'stored_file_size INTEGER,' +
                       'pad_char_count INTEGER,' +
                       'artist_name TEXT,' +
                       'artist_uid TEXT REFERENCES artist(artist_uid),' +
                       'song_name TEXT NOT NULL,' +
                       'md5_hash TEXT NOT NULL,' +
                       'compressed INTEGER,' +
                       'encrypted INTEGER,' +
                       'container_name TEXT NOT NULL,' +
                       'object_name TEXT NOT NULL,' +
                       'album_uid TEXT REFERENCES album(album_uid));';

    CreatePlaylistTable := 'CREATE TABLE playlist (' +
                           'playlist_uid TEXT UNIQUE NOT NULL,' +
                           'playlist_name TEXT UNIQUE NOT NULL,' +
                           'playlist_description TEXT);';

    CreatePlaylistSongTable := 'CREATE TABLE playlist_song (' +
                               'playlist_song_uid TEXT UNIQUE NOT NULL,' +
                               'playlist_uid TEXT NOT NULL REFERENCES playlist(playlist_uid),' +
                               'song_uid TEXT NOT NULL REFERENCES song(song_uid));';

    DidSucceed := CreateTable(CreateGenreTable) and
                  CreateTable(CreateArtistTable) and
                  CreateTable(CreateAlbumTable) and
                  CreateTable(CreateSongTable) and
                  CreateTable(CreatePlaylistTable) and
                  CreateTable(CreatePlaylistSongTable);
  end;

  CreateTables := DidSucceed;
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

function TJukeboxDB.InsertSong(Song: TSongMetadata): Boolean;
var
  InsertSuccess: Boolean;
  Sql: String;
  IntValue: Integer;
begin
  InsertSuccess := false;

  if (DbConnection <> nil) and (Song <> nil) then begin
    Sql := 'INSERT INTO song VALUES (:file_uid,' +
                                    ':file_time,' +
                                    ':origin_file_size,' +
                                    ':stored_file_size,' +
                                    ':pad_char_count,' +
                                    ':artist_name,' +
                                    ':artist_uid,' +
                                    ':song_name,' +
                                    ':md5_hash,' +
                                    ':compressed,' +
                                    ':encrypted,' +
                                    ':container_name,' +
                                    ':object_name,' +
                                    ':album_uid);';

    DbQuery.SQL.Text := Sql;
    DbQuery.Params.ParamByName('file_uid').AsString := Song.Fm.FileUid;
    DbQuery.Params.ParamByName('file_time').AsString := Song.Fm.FileTime;
    DbQuery.Params.ParamByName('origin_file_size').AsInteger := Song.Fm.OriginFileSize;
    DbQuery.Params.ParamByName('stored_file_size').AsInteger := Song.Fm.StoredFileSize;
    DbQuery.Params.ParamByName('pad_char_count').AsInteger := Song.Fm.PadCharCount;
    DbQuery.Params.ParamByName('artist_name').AsString := Song.ArtistName;
    DbQuery.Params.ParamByName('artist_uid').AsString := '';
    DbQuery.Params.ParamByName('song_name').AsString := Song.SongName;
    DbQuery.Params.ParamByName('md5_hash').AsString := Song.Fm.Md5Hash;
    if Song.Fm.Compressed then
      IntValue := 1
    else
      IntValue := 0;
    DbQuery.Params.ParamByName('compressed').AsInteger := IntValue;
   
    if Song.Fm.Encrypted then
      IntValue := 1
    else
      IntValue := 0; 
    DbQuery.Params.ParamByName('encrypted').AsInteger := IntValue;
    DbQuery.Params.ParamByName('container_name').AsString := Song.Fm.ContainerName;
    DbQuery.Params.ParamByName('object_name').AsString := Song.Fm.ObjectName;
    DbQuery.Params.ParamByName('album_uid').AsString := Song.AlbumUid;

    DbQuery.ExecSQL;

    if DbQuery.RowsAffected = 0 then begin
      Rollback;
    end
    else begin
      InsertSuccess := Commit;
    end;
  end;

  InsertSong := InsertSuccess;
end;

//*******************************************************************************

function TJukeboxDB.UpdateSong(Song: TSongMetadata): Boolean;
var
  UpdateSuccess: Boolean;
begin
  UpdateSuccess := false;

  //TODO: implement UpdateSong

  UpdateSong := UpdateSuccess;
end;

//*******************************************************************************

function TJukeboxDB.StoreSongMetadata(Song: TSongMetadata): Boolean;
var
  DbSong: TSongMetadata;
  Success: Boolean;
begin
  Success := false;

  DbSong := RetrieveSong(Song.Fm.FileUid);
  if DbSong <> nil then begin
    if not Song.IsEqualTo(DbSong) then begin
      Success := UpdateSong(Song);
    end
    else begin
      Success := true;  // no insert or update needed (already up-to-date)
    end;
  end
  else begin
    // song is not in the database, insert it
    Success := InsertSong(Song);
  end;

  StoreSongMetadata := Success;
end;

//*******************************************************************************

function TJukeboxDB.InsertPlaylist(PlUid: String;
                                   PlName: String;
                                   PlDesc: String): Boolean;
var
  InsertSuccess: Boolean;
  Sql: String;
  RowsAffected: Int64;
begin
  InsertSuccess := false;

  if (DbConnection <> nil) and
     (PlUid.Length > 0) and
     (PlName.Length > 0) then begin

    if not BeginTransaction then begin
      InsertPlaylist := false;
      exit;
    end;

    Sql := 'INSERT INTO playlist VALUES (:pl_uid,:pl_name,:pl_desc)';

    DbQuery.SQL.Text := Sql;
    DbQuery.Params.ParamByName('pl_uid').AsString := PlUid;
    DbQuery.Params.ParamByName('pl_name').AsString := PlName;
    DbQuery.Params.ParamByName('pl_desc').AsString := PlDesc;

    DbQuery.ExecSQL;

    RowsAffected := DbQuery.RowsAffected;
    if RowsAffected = 0 then begin
      Rollback;
    end
    else begin
      InsertSuccess := Commit;
    end;
  end;

  InsertPlaylist := InsertSuccess;
end;

//*******************************************************************************

function TJukeboxDB.DeleteSong(SongUid: String): Boolean;
var
  WasDeleted: Boolean;
  Sql: String;
  RowsAffected: Int64;
begin
  WasDeleted := false;
  if DbConnection <> nil then begin
    if SongUid.Length > 0 then begin
      if not BeginTransaction then begin
        writeLn('error: begin transaction failed');
        DeleteSong := false;
        exit;
      end;

      Sql := 'DELETE FROM song WHERE song_uid = :song_uid';

      DbQuery.SQL.Text := Sql;
      DbQuery.Params.ParamByName('song_uid').AsString := SongUid;

      DbQuery.ExecSQL;

      RowsAffected := DbQuery.RowsAffected;
      if RowsAffected = 0 then begin
        Rollback;
        writeLn('error: unable to delete song ' + SongUid);
      end
      else begin
        WasDeleted := Commit;
      end;
    end;
  end;

  DeleteSong := WasDeleted;
end;

//*******************************************************************************

function TJukeboxDB.DeletePlaylist(PlName: String): Boolean;
var
  DeleteSuccess: Boolean;
  Sql: String;
  RowsAffected: Int64;
begin
  DeleteSuccess := false;

  if (DbConnection <> nil) and (PlName.Length > 0) then begin
    if not BeginTransaction then begin
      DeletePlaylist := false;
      exit;
    end;

    Sql := 'DELETE FROM playlist WHERE playlist_name = :playlist_name';

    DbQuery.SQL.Text := Sql;
    DbQuery.Params.ParamByName('playlist_name').AsString := PlName;

    DbQuery.ExecSQL;

    RowsAffected := DbQuery.RowsAffected;
    if RowsAffected = 0 then begin
      Rollback;
    end
    else begin
      DeleteSuccess := Commit;
    end;
  end;

  DeletePlaylist := DeleteSuccess;
end;

//*******************************************************************************

end.

