program testdb;

{$mode objfpc}{$H+}{$J-}

uses
  JukeboxDB;

var
  jdb: TJukeboxDB;

begin
  jdb := TJukeboxDB.Create('jukebox_db.sqlite3', true);
  if jdb.Enter then begin
    jdb.ShowListings;
    writeLn('**** Playlists ****');
    jdb.ShowPlaylists;
    writeLn('**** Artists ****');
    jdb.ShowArtists;
    writeLn('**** Genres ****');
    jdb.ShowGenres;
    writeLn('**** Albums ****');
    jdb.ShowAlbums;
    writeLn('**** HaveTables ****');
    if jdb.HaveTables then begin
      writeLn('HaveTables is true');
    end
    else begin
      writeLn('HaveTables is false');
    end;

    jdb.Leave;
  end
  else begin
    writeLn('error: unable to enter jukeboxDB');
  end;
end.
