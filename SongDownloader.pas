unit SongDownloader;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, AbstractJukebox, SongMetadata, SysUtils;

type
  TListSongMetadata = specialize TFPGObjectList<TSongMetadata>;

  TSongDownloader = Class(TObject)
  private
    jukebox: TAbstractJukebox;     // weak
    ListSongs: TListSongMetadata;

  public
    constructor Create(jb: TAbstractJukebox; aSongList: TListSongMetadata);
    destructor Destroy; override;
    procedure Run;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TSongDownloader.Create(jb: TAbstractJukebox;
                                   aSongList: TListSongMetadata);
begin
  inherited Create;
  jukebox := jb;
  ListSongs := aSongList;
end;

//*******************************************************************************

destructor TSongDownloader.Destroy;
begin
  writeLn('TSongDownloader.Destroy');
  if ListSongs <> nil then begin
    ListSongs.Free;
    ListSongs := nil;
  end;
  inherited;
end;

//*******************************************************************************

procedure TSongDownloader.Run;
var
  i: Integer;
  Song: TSongMetadata;
begin
  if ListSongs <> nil then begin
    if ListSongs.Count > 0 then begin
      jukebox.BatchDownloadStart;

      for i := 0 to ListSongs.Count-1 do begin
        if jukebox.IsExitRequested then begin
          break;
        end
        else begin
          Song := ListSongs[i];
          if Song <> nil then begin
            jukebox.DownloadSong(Song);
          end;
        end;
      end;

      jukebox.BatchDownloadComplete;
    end;
  end;
end;

//*******************************************************************************

end.

