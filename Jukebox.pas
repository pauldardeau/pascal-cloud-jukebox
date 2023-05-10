unit Jukebox;

{$mode objfpc}{$H+}{$J-}

interface

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  CRT, Classes, fgl, FileMetadata, JukeboxDB, JukeboxOptions, PropertySet,
  PropertyValue, AbstractJukebox, SongDownloader, SongDownloaderThread,
  SongMetadata, StorageSystem, SysUtils, JBSysUtils, JBUtils, JBPlatform,
  Process, IniReader, KeyValuePairs, fpjson, jsonparser;

const
  DOUBLE_DASHES = '--';

  // container suffixes
  SFX_ALBUM_CONTAINER = 'albums';
  SFX_ALBUM_ART_CONTAINER = 'album-art';
  SFX_METADATA_CONTAINER = 'music-metadata';
  SFX_PLAYLIST_CONTAINER = 'playlists';
  SFX_SONG_CONTAINER = '-artist-songs';

  // directories
  DIR_ALBUM_ART_IMPORT = 'album-art-import';
  DIR_PLAYLIST_IMPORT = 'playlist-import';
  DIR_SONG_IMPORT = 'song-import';
  DIR_SONG_PLAY = 'song-play';

  // files
  DOWNLOAD_EXTENSION = '.download';
  JUKEBOX_PID_FILE_NAME = 'jukebox.pid';
  JSON_FILE_EXT = '.json';
  SETTINGS_INI_FILE_NAME = 'settings.ini';
  DEFAULT_DB_FILE_NAME = 'jukebox_db.sqlite3';

  // audio file INI
  AUDIO_INI_FILE_NAME = 'audio_player.ini';
  AUDIO_PLAYER_EXE_FILE_NAME = 'audio_player_exe_file_name';
  AUDIO_PLAYER_COMMAND_ARGS = 'audio_player_command_args';
  AUDIO_PLAYER_RESUME_ARGS = 'audio_player_resume_args';

  // placeholders
  PH_AUDIO_FILE_PATH = '%%AUDIO_FILE_PATH%%';
  PH_START_SONG_TIME_OFFSET = '%%START_SONG_TIME_OFFSET%%';

type
  TListSongMetadata = specialize TFPGObjectList<TSongMetadata>;

  TJukebox = Class(TAbstractJukebox)
  private
    JukeboxOptions: TJukeboxOptions;
    StorageSystem: TStorageSystem;
    DebugPrint: Boolean;
    JukeboxDb: TJukeboxDB;
    ContainerPrefix: String;
    CurrentDir: String;
    SongImportDirPath: String;
    PlaylistImportDirPath: String;
    SongPlayDirPath: String;
    AlbumArtImportDirPath: String;
    MetadataDbFile: String;
    MetadataContainer: String;
    PlaylistContainer: String;
    AlbumContainer: String;
    AlbumArtContainer: String;
    SongList: TListSongMetadata;
    NumberSongs: Integer;
    SongIndex: Integer;
    AudioPlayerExeFileName: String;
    AudioPlayerCommandArgs: String;
    AudioPlayerResumeArgs: String;
    AudioPlayerProcess: TProcess;
    SongPlayLengthSeconds: Integer;
    CumulativeDownloadBytes: Int64;
    CumulativeDownloadTime: Integer;
    ExitRequested: Boolean;
    IsPaused: Boolean;
    SongStartTime: Double;
    SongSecondsOffset: Integer;
    SongPlayIsResume: Boolean;
    IsRepeatMode: Boolean;
    SongDownloaderThread: TSongDownloaderThread;
    AudioIniFilePath: String;
    SettingsIniFilePath: String;

  public
    class function InitializeStorageSystem(StorageSys: TStorageSystem;
                                           aContainerPrefix: String;
                                           aDebugPrint: Boolean): Boolean; static;
    constructor Create(JbOptions: TJukeboxOptions;
                       StorageSys: TStorageSystem;
                       aContainerPrefix: String;
                       aDebugPrint: Boolean);
    destructor Destroy; override;
    procedure InstallSignalHandlers;
    function IsExitRequested: Boolean; override;
    function Enter: Boolean;
    procedure Leave;
    procedure TogglePausePlay;
    procedure AdvanceToNextSong;
    procedure PrepareForTermination;
    procedure DisplayInfo;
    function GetMetadataDbFilePath: String;
    function StoreSongMetadata(FsSong: TSongMetadata): Boolean;
    function StoreSongPlaylist(FileName: String;
                               FileContents: TMemoryStream): Boolean;
    function ContainerForSong(SongUid: String): String;
    procedure ImportSongs;
    function SongPathInPlaylist(Song: TSongMetadata): String;
    function CheckFileIntegrity(Song: TSongMetadata): Boolean;
    procedure BatchDownloadStart; override;
    procedure BatchDownloadComplete; override;
    function RetrieveFile(Fm: TFileMetadata; DirPath: String): Int64;
    function DownloadSong(Song: TSongMetadata): Boolean; override;
    procedure PlaySong(Song: TSongMetadata);
    procedure DownloadSongs;
    procedure DownloadSongs(DlSongs: TListSongMetadata);
    procedure RunSongDownloaderThread;
    procedure PlaySongs(Shuffle: Boolean; Artist: String; Album: String);
    procedure PlaySongList(aSongList: TListSongMetadata; Shuffle: Boolean);
    procedure ShowListContainers;
    procedure ShowListings;
    procedure ShowArtists;
    procedure ShowGenres;
    procedure ShowAlbums;
    function ReadFileContents(FilePath: String;
                              out Contents: TMemoryStream): Boolean;
    function UploadMetadataDb: Boolean;
    procedure ImportPlaylists;
    procedure ShowPlaylists;
    procedure ShowAlbum(Artist: String; Album: String);
    procedure ShowPlaylist(Playlist: String);
    procedure PlayPlaylist(Playlist: String);
    procedure PlayAlbum(Artist: String; Album: String);
    function DeleteSong(SongUid: String; UploadMetadata: Boolean): Boolean;
    function DeleteArtist(Artist: String): Boolean;
    function DeleteAlbum(Album: String): Boolean;
    function DeletePlaylist(PlaylistName: String): Boolean;
    procedure ImportAlbumArt;
    function GetAlbumTrackObjectList(Artist: String;
                                     AlbumName: String;
                                     ListTrackObjects: TStringList): Boolean;
    function GetPlaylistSongs(PlaylistName: String;
                              ListSongs: TListSongMetadata): Boolean;
    procedure ReadAudioPlayerConfig;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

class function TJukebox.InitializeStorageSystem(StorageSys: TStorageSystem; aContainerPrefix: String; aDebugPrint: Boolean): Boolean;
var
  ArtistSongChars: String;
  i: Integer;
  ch: Char;
  ContainerName: String;
  ContainerNames: TStringList;
  CnrName: String;
begin
  // create the containers that will hold songs
  ArtistSongChars := '0123456789abcdefghijklmnopqrstuvwxyz';

  for i := 1 to ArtistSongChars.Length do begin
    ch := ArtistSongChars[i];
    ContainerName := aContainerPrefix + ch + SFX_SONG_CONTAINER;
    if not StorageSys.CreateContainer(ContainerName) then begin
      writeLn('error: unable to create container ' + ContainerName);
      InitializeStorageSystem := false;
      exit;
    end;
  end;

  // create the other (non-song) containers
  ContainerNames := TStringList.Create;
  ContainerNames.Add(SFX_METADATA_CONTAINER);
  ContainerNames.Add(SFX_ALBUM_ART_CONTAINER);
  ContainerNames.Add(SFX_ALBUM_CONTAINER);
  ContainerNames.Add(SFX_PLAYLIST_CONTAINER);

  for i := 0 to ContainerNames.Count-1 do begin
    ContainerName := ContainerNames[i];
    CnrName := aContainerPrefix + ContainerName;
    if not StorageSys.CreateContainer(CnrName) then begin
      writeLn('error: unable to create container ' + CnrName);
      InitializeStorageSystem := false;
      exit;
    end;
  end;

  // delete metadata DB file if present
  if JBFileExists(DEFAULT_DB_FILE_NAME) then begin
    if aDebugPrint then begin
      writeLn('deleting existing metadata DB file');
    end;
    JBDeleteFile(DEFAULT_DB_FILE_NAME);
  end;

  InitializeStorageSystem := true;
end;

//*******************************************************************************

constructor TJukebox.Create(JbOptions: TJukeboxOptions;
                            StorageSys: TStorageSystem;
                            aContainerPrefix: String;
                            aDebugPrint: Boolean);
begin
  inherited Create;
  //{$IFNDEF WINDOWS}
  //if GlobalJukebox = nil then begin
  //  GlobalJukebox := self;
  //end
  //else begin
  //  //TODO: throw an exception (only 1 Jukebox instance allowed)
  //end;
  //{$ENDIF}

  JukeboxOptions := JbOptions;
  StorageSystem := StorageSys;
  DebugPrint := aDebugPrint;
  JukeboxDb := nil;
  ContainerPrefix := aContainerPrefix;
  CurrentDir := JbOptions.Directory;
  SongImportDirPath := JBPathJoin(CurrentDir, DIR_SONG_IMPORT);
  PlaylistImportDirPath := JBPathJoin(CurrentDir, DIR_PLAYLIST_IMPORT);
  SongPlayDirPath := JBPathJoin(CurrentDir, DIR_SONG_PLAY);
  AlbumArtImportDirPath := JBPathJoin(CurrentDir, DIR_ALBUM_ART_IMPORT);
  MetadataDbFile := DEFAULT_DB_FILE_NAME;
  MetadataContainer := ContainerPrefix + SFX_METADATA_CONTAINER;
  PlaylistContainer := ContainerPrefix + SFX_PLAYLIST_CONTAINER;
  AlbumContainer := ContainerPrefix + SFX_ALBUM_CONTAINER;
  AlbumArtContainer := ContainerPrefix + SFX_ALBUM_ART_CONTAINER;
  SongList := TListSongMetadata.Create;
  NumberSongs := 0;
  SongIndex := -1;
  AudioPlayerExeFileName := '';
  AudioPlayerCommandArgs := '';
  AudioPlayerResumeArgs := '';
  AudioPlayerProcess := nil;
  SongPlayLengthSeconds := 20;
  CumulativeDownloadBytes := 0;
  CumulativeDownloadTime := 0;
  ExitRequested := false;
  IsPaused := false;
  SongSecondsOffset := 0;
  SongPlayIsResume := false;
  IsRepeatMode := false;
  SongDownloaderThread := nil;
  AudioIniFilePath := JBPathJoin(JbOptions.Directory, AUDIO_INI_FILE_NAME);
  SettingsIniFilePath := JBPathJoin(JbOptions.Directory, SETTINGS_INI_FILE_NAME);

  if JukeboxOptions.DebugMode then begin
    DebugPrint := true;
  end;
  if DebugPrint then begin
    writeLn('currentDir = ' + CurrentDir);
    writeLn('songImportDirPath = ' + SongImportDirPath);
    writeLn('songPlayDirPath = ' + SongPlayDirPath);
    writeLn('playlistImportDirPath = ' + PlaylistImportDirPath);
    writeLn('albumArtImportDirPath = ' + AlbumArtImportDirPath);
  end;
end;

//*******************************************************************************

destructor TJukebox.Destroy;
begin
  writeLn('TJukebox.Destroy');

  if JukeboxDb <> nil then begin
    JukeboxDb.Free;
    JukeboxDb := nil;
  end;

  if AudioPlayerProcess <> nil then begin
    if AudioPlayerProcess.Running then begin
      AudioPlayerProcess.Terminate(5);
    end;
    AudioPlayerProcess.Free;
    AudioPlayerProcess := nil;
  end;

  if SongDownloaderThread <> nil then begin
    SongDownloaderThread.Free;
    SongDownloaderThread := nil;
  end;

  if SongList <> nil then begin
    SongList.Free;
    SongList := nil;
  end;

  inherited;
end;

//*******************************************************************************

//class function SigHandler(signum: Integer);
//begin
//  {$IFNDEF WINDOWS}
//  if Jukebox.GlobalJukebox <> nil then begin
//    if signum = SIGUSR1 then begin
//      Jukebox.GlobalJukebox.TogglePausePlay;
//    end
//    else if signum = SIGUSR2 then begin
//      Jukebox.GlobalJukebox.AdvanceToNextSong;
//    end
//    else if signum = SIGINT then begin
//      Jukebox.GlobalJukebox.PrepareForTermination;
//    end
//    else if signum = SIGWINCH then begin
//      Jukebox.GlobalJukebox.DisplayInfo;
//    end;
//  end;
//  {$ENDIF}
//end;

//*******************************************************************************

procedure TJukebox.InstallSignalHandlers;
begin
  //TODO: implement InstallSignalHandlers
end;

//*******************************************************************************

function TJukebox.IsExitRequested: Boolean;
begin
  IsExitRequested := ExitRequested;
end;

//*******************************************************************************

function TJukebox.Enter: Boolean;
var
  EnterSuccess: Boolean;
  MetadataFileInContainer: Boolean;
  Container: String;
  ContainerContents: TStringList;
  MetadataDbFilePath: String;
  i: Integer;
  DownloadFile: String;
begin
  EnterSuccess := false;

  // look for stored metadata in the storage system
  if StorageSystem.HasContainer(MetadataContainer) and
     not JukeboxOptions.SuppressMetadataDownload then begin

    // metadata container exists, retrieve container listing
    MetadataFileInContainer := false;
    ContainerContents :=
      StorageSystem.ListContainerContents(MetadataContainer);

    if ContainerContents.Count > 0 then begin
      for i := 0 to ContainerContents.Count-1 do begin
        Container := ContainerContents[i];
        if Container = MetadataDbFile then begin
          MetadataFileInContainer := true;
          break;
        end;
      end;
    end;

    // does our metadata DB file exist in the metadata container?
    if MetadataFileInContainer then begin
      // download it
      MetadataDbFilePath := GetMetadataDbFilePath;

      if JBFileExists(MetadataDbFilePath) then begin
        if DebugPrint then begin
          writeLn('deleting existing metadata DB file');
        end;
        JBDeleteFile(MetadataDbFilePath);
      end;

      DownloadFile := MetadataDbFilePath; // + Jukebox.downloadExtension;
      if StorageSystem.GetObject(MetadataContainer,
                                 MetadataDbFile,
                                 DownloadFile) > 0 then begin
        if DebugPrint then begin
          writeLn('metadata DB file downloaded');
        end;
      end
      else begin
        if DebugPrint then begin
          writeLn('error: unable to retrieve metadata DB file');
        end;
      end;
    end
    else begin
      if DebugPrint then begin
        writeLn('no metadata container in storage system');
      end;
    end;

    JukeboxDb := TJukeboxDB.Create(GetMetadataDbFilePath, DebugPrint);
    EnterSuccess := JukeboxDb.Enter;
    if not EnterSuccess then begin
      JukeboxDb.Free;
      JukeboxDb := nil;
      writeLn('unable to connect to database');
    end;
  end
  else begin
    if DebugPrint then begin
      if not StorageSystem.HasContainer(MetadataContainer) then begin
        writeLn('metadata container "' + MetadataContainer +
                '" does not exist');
      end;
    end;
  end;

  Enter := EnterSuccess;
end;

//*******************************************************************************

procedure TJukebox.Leave;
begin
  if JukeboxDb <> nil then begin
    JukeboxDb.Leave;
    JukeboxDb.Free;
    JukeboxDb := nil;
  end;
end;

//*******************************************************************************

procedure TJukebox.TogglePausePlay;
begin
  IsPaused := not IsPaused;
  if IsPaused then begin
    writeLn('paused');
    //if AudioPlayerProcess <> nil then begin
    //  // capture current song position (seconds into song)
    //  AudioPlayerProcess.Stop;
    //  AudioPlayerProcess.Free;
    //  AudioPlayerProcess := nil;
    //end;
  end
  else begin
    writeLn('resuming play');
  end;
end;

//*******************************************************************************

procedure TJukebox.AdvanceToNextSong;
begin
  writeLn('advancing to next song');
  //if AudioPlayerProcess <> nil then begin
  //  AudioPlayerProcess.Stop;
  //  AudioPlayerProcess := nil;
  //end;
end;

//*******************************************************************************

procedure TJukebox.PrepareForTermination;
begin
  writeLn('Ctrl-C detected, shutting down');

  // indicate that it's time to shutdown
  ExitRequested := true;

  // terminate audio player if it's running
  //if AudioPlayerProcess <> nil then begin
  //  AudioPlayerProcess.Stop;
  //  AudioPlayerProcess.Free;
  //  AudioPlayerProcess := nil;
  //end;
end;

//*******************************************************************************

procedure TJukebox.DisplayInfo;
var
  MaxIndex: Integer;
  FirstSong: TSongMetadata;
  SecondSong: TSongMetadata;
  ThirdSong: TSongMetadata;
begin
  if SongList.Count > 0 then begin
    MaxIndex := SongList.Count - 1;
    if (SongIndex+3) <= MaxIndex then begin
      writeLn('----- songs on deck -----');
      FirstSong := SongList[SongIndex+1];
      writeLn(FirstSong.Fm.FileUid);
      SecondSong := SongList[SongIndex+2];
      writeLn(SecondSong.Fm.FileUid);
      ThirdSong := SongList[SongIndex+3];
      writeLn(ThirdSong.Fm.FileUid);
      writeLn('-------------------------');
    end;
  end;
end;

//*******************************************************************************

function TJukebox.GetMetadataDbFilePath: String;
begin
  GetMetadataDbFilePath := JBPathJoin(CurrentDir, MetadataDbFile);
end;

//*******************************************************************************

function TJukebox.StoreSongMetadata(FsSong: TSongMetadata): Boolean;
var
  StoreSuccess: Boolean;
  //DbSong: TSongMetadata;
begin
  StoreSuccess := false;
  if JukeboxDb <> nil then begin
    {
    DbSong := JukeboxDb.RetrieveSong(FsSong.Fm.FileUid);
    if DbSong <> nil then begin
      if not FsSong.Equals(DbSong) then begin
        StoreSuccess := JukeboxDb.UpdateSong(FsSong);
      end
      else begin
        StoreSuccess := true; // no insert or update needed (already up-to-date)
      end;
      DbSong.Free;
      DbSong := nil;
    end
    else begin
      // song is not in the database, insert it
      StoreSuccess := JukeboxDb.InsertSong(FsSong);
    end;
    }
  end;
  StoreSongMetadata := StoreSuccess;
end;

//*******************************************************************************

function TJukebox.StoreSongPlaylist(FileName: String;
                                    FileContents: TMemoryStream): Boolean;
begin
  //TODO: implement StoreSongPlaylist
  StoreSongPlaylist := false;
end;

//*******************************************************************************

function TJukebox.ContainerForSong(SongUid: String): String;
var
  ArtistLetter: String;
  Artist: String;
begin
  if SongUid.Length = 0 then begin
    ContainerForSong := '';
    exit;
  end;

  Artist := ArtistFromFileName(SongUid);
  if Artist.Length = 0 then begin
    ContainerForSong := '';
    exit;
  end;

  if Artist.StartsWith('A ') then begin
    ArtistLetter := Artist[3];
  end
  else if Artist.StartsWith('The ') then begin
    ArtistLetter := Artist[5];
  end
  else begin
    ArtistLetter := Artist[1];
  end;

  ContainerForSong := ContainerPrefix +
                      ArtistLetter.ToLower +
                      SFX_SONG_CONTAINER;
end;

//*******************************************************************************

procedure TJukebox.ImportSongs;
var
  DirListing: TStringList;
  NumEntries: Double;
  ProgressBarWidth: Integer;
  ProgressCharsPerIteration: Double;
  ProgressBarChar: Char;
  ProgressBarChars: Double;
  BarChars: Integer;
  FullPath: String;
  FileName: String;
  FileImportCount: Integer;
  CumulativeUploadTime: Integer;
  CumulativeUploadBytes: Int64;
  i: Integer;
  ListingEntry: String;
  FileExtension: String;
  FileRoot: String;
  Artist: String;
  Album: String;
  Song: String;
  ObjectName: String;
  FileRead: Boolean;
  FileSize: Int64;
  ContainerName: String;
  fsSong: TSongMetadata;
  Md5Hash: String;
  FileContents: TMemoryStream;
  cumulativeUploadKb: Double;
begin
  FileContents := nil;
  if JukeboxDb <> nil then begin
    if not JukeboxDb.IsOpen then begin
      exit;
    end;

    DirListing := JBListFilesInDirectory(SongImportDirPath);
    if DirListing.Count = 0 then begin
      DirListing.Free;
      DirListing := nil;
      exit;
    end;

    NumEntries := Double(DirListing.Count);
    ProgressbarChars := 0.0;
    ProgressBarWidth := 40;
    ProgressCharsPerIteration := Double(ProgressBarWidth) / NumEntries;
    ProgressbarChar := '#';
    BarChars := 0;

    if not DebugPrint then begin
      // setup progressbar
      {
      var aChar: Char
      var count: Int32
      aChar = ' '
      count = Int32(progressbarWidth)
      print("[%s]", String(aChar, count))
      //sys.stdout.flush()
      aChar = "\b"
      count = Int32(progressbarWidth+1)
      print(String(aChar, count)) // return to start of line, after '['
      }
    end;

    CumulativeUploadTime := 0;
    CumulativeUploadBytes := 0;
    FileImportCount := 0;

    for i := 0 to DirListing.Count-1 do begin
      ListingEntry := DirListing[i];
      FullPath := JBPathJoin(SongImportDirPath, ListingEntry);
      // ignore it if it is not a file
      if JBFileExists(FullPath) then begin
        FileName := ListingEntry;
        JBPathSplitExt(FullPath, FileRoot, FileExtension);
        if FileExtension.Length > 0 then begin
          FileSize := JBGetFileSize(FullPath);
          Artist := ArtistFromFileName(FileName);
          Album := AlbumFromFileName(FileName);
          Song := SongFromFileName(FileName);

          if (FileSize > 0) and
             (Artist.Length > 0) and
             (Album.Length > 0) and
             (Song.Length > 0) then begin

            ObjectName := FileName;
            fsSong := TSongMetadata.Create;
            fsSong.Fm.FileUid := objectName;
            fsSong.AlbumUid := '';
            fsSong.Fm.OriginFileSize := FileSize;
            //let oFile = RemObjects.Elements.RTL.File(fullPath)
            //TODO: assign fileTime
            //fsSong.Fm.FileTime = oFile.DateModified  // DateModified is a DateTime
            fsSong.ArtistName := artist;
            fsSong.SongName := song;
            Md5Hash := JBMd5ForFile(SettingsIniFilePath, FullPath);
            if Md5Hash.Length > 0 then begin
              fsSong.Fm.Md5Hash := Md5Hash;
            end;
            fsSong.Fm.Compressed := false;
            fsSong.Fm.Encrypted := false;
            fsSong.Fm.ObjectName := objectName;
            fsSong.Fm.PadCharCount := 0;

            fsSong.Fm.ContainerName := ContainerForSong(FileName);

            // read file contents
            FileRead := false;

            FileContents := JBFileReadAllBytes(FullPath);
            if FileContents.Size > 0 then begin
              FileRead := true;
            end
            else begin
              writeLn('error: unable to read file ' + FullPath);
            end;

            if FileRead then begin
              if FileContents.Size > 0 then begin
                fsSong.Fm.StoredFileSize := FileContents.Size;
                //startUploadTime := time.Now()

                ContainerName := ContainerPrefix + fsSong.Fm.ContainerName;

                // store song file to storage system
                if StorageSystem.PutObject(ContainerName,
                                           fsSong.Fm.ObjectName,
                                           FileContents,
                                           nil) then begin
                  //endUploadTime := time.Now()
                  // endUploadTime - startUploadTime
                  //uploadElapsedTime := endUploadTime.Add(-startUploadTime)
                  //cumulativeUploadTime.Add(uploadElapsedTime)
                  CumulativeUploadBytes := CumulativeUploadBytes + FileContents.Size;

                  // store song metadata in local database
                  if not StoreSongMetadata(fsSong) then begin
                    // we stored the song to the storage system, but were unable to store
                    // the metadata in the local database. we need to delete the song
                    // from the storage system since we won't have any way to access it
                    // since we can't store the song metadata locally.
                    writeLn('unable to store metadata, deleting obj "' +
                            fsSong.Fm.ObjectName + '"');
                    StorageSystem.DeleteObject(ContainerName,
                                               fsSong.Fm.ObjectName);
                  end
                  else begin
                    inc(FileImportCount);
                  end
                end
                else begin
                  writeLn('error: unable to upload ' + fsSong.Fm.ObjectName +
                          ' to ' + ContainerName);
                end;
              end;
            end;

            if FileContents <> nil then begin
              FileContents.Free;
              FileContents := nil;
            end;
          end;

          if not DebugPrint then begin
            {
            progressbarChars := progressbarChars + Double(progressCharsPerIteration);
            if Integer(progressbarChars) > barChars then begin
              const numNewChars = Integer(progressbarChars) - barChars;
              if numNewChars > 0 then begin
                // update progress bar
                //for j in 1...numNewChars do begin
                //  write(progressbarChar);
                //end;
                //sys.stdout.flush()
                barChars := barChars + numNewChars;
              end;
            end;
            }
          end;

        end;
      end;
    end;  // for each file in import directory

    DirListing.Free;
    DirListing := nil;

    if not DebugPrint then begin
      {
      // if we have not filled up the progress bar, fill it now
      if barChars < progressbarWidth then begin
        //const numNewChars = progressbarWidth - barChars;
        //for j in 1...numNewChars do begin
        //  write(progressbarChar);
        //end;
        //sys.stdout.flush()
      end;
      writeLn('');
      }
    end;

    if FileImportCount > 0 then begin
      UploadMetadataDb;
    end;

    writeLn(IntToStr(FileImportCount) + ' song files imported');

    if CumulativeUploadTime > 0 then begin
      cumulativeUploadKb := Double(CumulativeUploadBytes) / 1000.0;
      {writeLn('average upload throughput = ' {0} KB/sec,
              cumulativeUploadKb/CumulativeUploadTime); }
    end;
  end;
end;

//*******************************************************************************

function TJukebox.SongPathInPlaylist(Song: TSongMetadata): String;
begin
  SongPathInPlaylist := JBPathJoin(SongPlayDirPath, Song.Fm.FileUid);
end;

//*******************************************************************************

function TJukebox.CheckFileIntegrity(Song: TSongMetadata): Boolean;
var
  FileIntegrityPassed: Boolean;
  FilePath: String;
  PlaylistMd5: String;
begin
  FileIntegrityPassed := true;

  if JukeboxOptions.CheckDataIntegrity then begin
    FilePath := SongPathInPlaylist(Song);
    if JBFileExists(FilePath) then begin
      if DebugPrint then
        writeLn('checking integrity for ' + Song.Fm.FileUid);

      PlaylistMd5 := JBMd5ForFile(SettingsIniFilePath, FilePath);
      if PlaylistMd5.Length = 0 then begin
        writeLn('error: unable to calculate MD5 hash for file ' + FilePath);
        FileIntegrityPassed := false;
      end
      else begin
        if PlaylistMd5 = Song.Fm.Md5Hash then begin
          if DebugPrint then
            writeLn('integrity check SUCCESS');

          FileIntegrityPassed := true;
        end
        else begin
          writeLn('file integrity check failed: ' + Song.Fm.FileUid);
          FileIntegrityPassed := false;
        end;
      end;
    end
    else begin
      // file does not exist
      writeLn('file does not exist');
      FileIntegrityPassed := false;
    end;
  end
  else begin
    if DebugPrint then
      writeLn('file integrity bypassed, no jukebox options or check integrity ' +
              'not turned on');
  end;

  CheckFileIntegrity := FileIntegrityPassed;
end;

//*******************************************************************************

procedure TJukebox.BatchDownloadStart;
begin
  CumulativeDownloadBytes := 0;
  CumulativeDownloadTime := 0;
end;

//*******************************************************************************

procedure TJukebox.BatchDownloadComplete;
var
  CumulativeDownloadKb: Real;
begin
  if not ExitRequested then begin
    if CumulativeDownloadTime > 0 then begin
      CumulativeDownloadKb := Real(CumulativeDownloadBytes) / 1000.0;
      writeLn('average download throughput = {0} KB/sec',
              CumulativeDownloadKb/Int64(CumulativeDownloadTime));
    end;
    CumulativeDownloadBytes := 0;
    CumulativeDownloadTime := 0;
  end;
end;

//*******************************************************************************

function TJukebox.RetrieveFile(Fm: TFileMetadata; DirPath: String): Int64;
var
  BytesRetrieved: Int64;
  ContainerName: String;
  LocalFilePath: String;
begin
  BytesRetrieved := 0;

  if DirPath.Length > 0 then begin
    ContainerName := ContainerPrefix + Fm.ContainerName;
    LocalFilePath := JBPathJoin(DirPath, Fm.FileUid);
    BytesRetrieved := StorageSystem.GetObject(ContainerName,
                                              Fm.ObjectName,
                                              LocalFilePath);
  end;

  RetrieveFile := BytesRetrieved;
end;

//*******************************************************************************

function TJukebox.DownloadSong(Song: TSongMetadata): Boolean;
var
  FilePath: String;
  SongBytesRetrieved: Int64;
begin
  if ExitRequested then begin
    DownloadSong := false;
    exit;
  end;

  FilePath := SongPathInPlaylist(Song);
  SongBytesRetrieved := RetrieveFile(Song.Fm, SongPlayDirPath);
  if ExitRequested then begin
    DownloadSong := false;
    exit;
  end;

  if DebugPrint then begin
    writeLn('bytes retrieved: ' + IntToStr(SongBytesRetrieved));
  end;

  if SongBytesRetrieved > 0 then begin
    CumulativeDownloadBytes := CumulativeDownloadBytes + SongBytesRetrieved;

    // are we checking data integrity?
    // if so, verify that the storage system retrieved the same length that
    // has been stored
    if JukeboxOptions.CheckDataIntegrity then begin
      if DebugPrint then begin
        writeLn('verifying data integrity');
      end;

      if SongBytesRetrieved <> Song.Fm.StoredFileSize then begin
        writeLn('error: file size check failed for ' + FilePath);
        DownloadSong := false;
        exit;
      end;
    end;

    if CheckFileIntegrity(Song) then begin
      DownloadSong := true;
      exit;
    end
    else begin
      // we retrieved the file, but it failed our integrity check
      // if file exists, remove it
      if JBFileExists(FilePath) then begin
        JBDeleteFile(FilePath);
      end;
    end;
  end;

  DownloadSong := false;
end;

//*******************************************************************************

procedure TJukebox.PlaySong(Song: TSongMetadata);
var
  ExitCode: Integer;
  SongFilePath: String;
  didResume: Boolean;
  commandArgs: String;
  placeholder: String;
  posPlaceholder: Integer;
  theSongStartTime: String;
  minutes: Double;
  remainingSeconds: Integer;
  secondsText: String;
  Args: TStringArray;
  i: Integer;
  FileNotFoundPath: String;
begin
  ExitCode := -1;

  SongFilePath := SongPathInPlaylist(Song);

  if JBFileExists(SongFilePath) then begin
    writeLn('playing ' + Song.Fm.FileUid);
    if AudioPlayerExeFileName.Length > 0 then begin
      didResume := false;
      commandArgs := '';
      if SongPlayIsResume then begin
        placeholder := PH_START_SONG_TIME_OFFSET;
        posPlaceholder :=
            AudioPlayerResumeArgs.IndexOf(placeholder);
        if posPlaceholder > -1 then begin
          commandArgs := AudioPlayerResumeArgs;
          theSongStartTime := '';
          minutes := SongSecondsOffset / 60;
          if minutes > 0 then begin
            theSongStartTime := minutes.ToString;
            theSongStartTime := theSongStartTime + ':';
            remainingSeconds := SongSecondsOffset mod 60;
            secondsText := remainingSeconds.ToString;
            if secondsText.length = 1 then begin
              secondsText := '0' + secondsText;
            end;
            theSongStartTime := theSongStartTime + secondsText;
          end
          else begin
            theSongStartTime := SongSecondsOffset.ToString;
          end;
          //writeLn('resuming at ' + songStartTime);
          commandArgs := commandArgs.Replace(PH_START_SONG_TIME_OFFSET, theSongStartTime);
          commandArgs := commandArgs.Replace(PH_AUDIO_FILE_PATH, SongFilePath);
          didResume := true;
          //writeLn('commandArgs: ' + commandArgs);
        end;
      end;

      if not didResume then begin
        commandArgs := AudioPlayerCommandArgs;
        commandArgs := commandArgs.Replace(PH_AUDIO_FILE_PATH, SongFilePath);
      end;

      Args := commandArgs.Split(' ');

      AudioPlayerProcess := TProcess.Create(nil);
      AudioPlayerProcess.Executable := AudioPlayerExeFileName;

      for i := 0 to Length(Args)-1 do begin
        AudioPlayerProcess.Parameters.Add(Args[i]);
      end;

      AudioPlayerProcess.Options := AudioPlayerProcess.Options + [poWaitOnExit];
      AudioPlayerProcess.Execute;
      ExitCode := AudioPlayerProcess.ExitCode;

      AudioPlayerProcess.Free;
      AudioPlayerProcess := nil;

      // if the audio player failed or is not present, just sleep
      // for the length of time that audio would be played
      if ExitCode <> 0 then begin
        JBSleepSeconds(SongPlayLengthSeconds);
      end;
    end
    else begin
      // we do not know about an audio player, so simulate a
      // song being played by sleeping
      JBSleepSeconds(SongPlayLengthSeconds);
    end;

    if not IsPaused then begin
      // delete the song file from the play list directory
      JBDeleteFile(SongFilePath);
    end;
  end
  else begin
    writeLn('song file does not exist: ' + SongFilePath);
    FileNotFoundPath := JBPathJoin(JukeboxOptions.Directory, '404.txt');
    JBFileAppendAllText(FileNotFoundPath, SongFilePath + LineEnding);
  end;
end;

//*******************************************************************************

procedure TJukebox.DownloadSongs;
var
  DirListing: TStringList;
  SongFileCount: Integer;
  DlSongs: TListSongMetadata;
  FileName: String;
  i: Integer;
  j: Integer;
  FileExtension: String;
  FileCacheCount: Integer;
  CheckIndex: Integer;
  FilePath: String;
  si: TSongMetadata;
begin
  // scan the play list directory to see if we need to download more songs
  DirListing := JBListFilesInDirectory(SongPlayDirPath);
  if DirListing.Count = 0 then begin
    DirListing.Free;
    DirListing := nil;
    // log error
    exit;
  end;

  DlSongs := TListSongMetadata.Create;

  SongFileCount := 0;
  for i := 0 to DirListing.Count-1 do begin
    FileName := DirListing[i];
    FileExtension := JBGetFileExtension(FileName);

    if (FileExtension.Length > 0) and
       (FileExtension <> DOWNLOAD_EXTENSION) then begin

      inc(SongFileCount);
    end;
  end;

  DirListing.Free;
  DirListing := nil;

  FileCacheCount := JukeboxOptions.FileCacheCount;

  if SongFileCount < FileCacheCount then begin
    // start looking at the next song in the list
    CheckIndex := SongIndex + 1;
    for j := 1 to NumberSongs do begin
      if CheckIndex >= NumberSongs then begin
        CheckIndex := 0;
      end;
      if CheckIndex <> SongIndex then begin
        si := SongList[CheckIndex];
        FilePath := SongPathInPlaylist(si);
        if not JBFileExists(FilePath) then begin
          DlSongs.Add(si);
          if DlSongs.Count >= FileCacheCount then begin
            break;
          end;
        end;
      end;
      inc(CheckIndex);
    end;
  end;

  if DlSongs.Count > 0 then begin
    DownloadSongs(DlSongs);
  end;

  DlSongs.Free;
  DlSongs := nil;
end;

//*******************************************************************************

procedure TJukebox.DownloadSongs(DlSongs: TListSongMetadata);
begin
  if DlSongs.Count > 0 then begin
    if SongDownloaderThread = nil then begin
      if DebugPrint then begin
        writeLn('creating SongDownloaderThread');
      end;
      SongDownloaderThread := TSongDownloaderThread.Create(self, DlSongs);
    end
    else begin
      if DebugPrint then begin
        writeLn('Not downloading more songs b/c Downloader <> nil or ' +
                'DownloadThread <> nil');
      end;
    end;
  end;
end;

//*******************************************************************************

procedure TJukebox.RunSongDownloaderThread;
begin
  SongDownloaderThread.Start;
end;

//*******************************************************************************

procedure TJukebox.PlaySongs(Shuffle: Boolean; Artist: String; Album: String);
var
  HaveSongs: Boolean;
  aSongList: TListSongMetadata;
  ListTrackObjects: TStringList;
  TrackObjectName: String;
  i: Integer;
  Song: TSongMetadata;
begin
  if JukeboxDb <> nil then begin
    HaveSongs := false;
    if (Artist.Length > 0) and (Album.Length > 0) then begin
      aSongList := TListSongMetadata.Create;
      ListTrackObjects := TStringList.Create;
      if GetAlbumTrackObjectList(Artist, Album, ListTrackObjects) then begin
        if ListTrackObjects.Count > 0 then begin
          for i := 0 to ListTrackObjects.Count-1 do begin
            TrackObjectName := ListTrackObjects[i];
            Song := JukeboxDb.RetrieveSong(TrackObjectName);
            if Song <> nil then begin
              aSongList.Add(Song);
            end;
          end;
          if aSongList.Count = ListTrackObjects.Count then begin
            HaveSongs := true;
            if SongList <> nil then begin
              SongList.Free;
              SongList := nil;
            end;
            SongList := aSongList;
          end;
        end;
      end
      else begin
        aSongList.Free;
        aSongList := nil;
        ListTrackObjects.Free;
        ListTrackObjects := nil;
      end;
    end;

    if not HaveSongs then begin
      if SongList <> nil then begin
        SongList.Free;
        SongList := nil;
      end;
      SongList := JukeboxDb.RetrieveSongs(Artist, Album);
    end;

    PlaySongList(SongList, Shuffle);
  end;
end;

//*******************************************************************************

procedure TJukebox.ReadAudioPlayerConfig;
var
  osIdentifier: String;
  iniReader: TIniReader;
  kvpAudioPlayer: TKeyValuePairs;
  key: String;
  placeholder: String;
  posPlaceholder: Integer;
  charsToStrip: array [0..1] of Char;
begin
  iniReader := nil;
  kvpAudioPlayer := nil;

  if not JBFileExists(AudioIniFilePath) then begin
    writeLn('error: missing ' + AudioIniFilePath + ' config file');
    exit;
  end;

  osIdentifier := JBGetPlatformIdentifier;
  if osIdentifier = PLATFORM_UNKNOWN then begin
    writeLn('error: no audio-player specific lookup defined for this OS (unknown)');
    exit;
  end;

  charsToStrip[0] := '"';

  AudioPlayerExeFileName := '';
  AudioPlayerCommandArgs := '';
  AudioPlayerResumeArgs := '';

  iniReader := TIniReader.Create(AudioIniFilePath);
  if not iniReader.ReadFile then begin
    iniReader.Free;
    writeLn('error: unable to read ini config file ' + AudioIniFilePath);
    exit;
  end;

  kvpAudioPlayer := TKeyValuePairs.Create;
  if not iniReader.ReadSection(osIdentifier, kvpAudioPlayer) then begin
    kvpAudioPlayer.Free;
    iniReader.Free;
    writeLn('error: no config section present for ' + osIdentifier);
    exit;
  end;

  iniReader.Free;
  iniReader := nil;

  key := AUDIO_PLAYER_EXE_FILE_NAME;

  if kvpAudioPlayer.ContainsKey(key) then begin
    AudioPlayerExeFileName := kvpAudioPlayer.GetValue(key);

    if AudioPlayerExeFileName.StartsWith('"') and
       AudioPlayerExeFileName.EndsWith('"') then begin

      AudioPlayerExeFileName :=
        AudioPlayerExeFileName.Trim(charsToStrip);
    end;

    AudioPlayerExeFileName := AudioPlayerExeFileName.Trim;

    if AudioPlayerExeFileName.Length = 0 then begin
      writeLn('error: no value given for ' + key + ' within [' +
              osIdentifier + ']');
      kvpAudioPlayer.Free;
      exit;
    end;

    if DebugPrint then begin
      writeLn('audio player: ' + AudioPlayerExeFileName);
    end;
  end
  else begin
    writeLn('error: ' + AUDIO_INI_FILE_NAME + ' missing value for ' +
            key + ' within [' + osIdentifier + ']');
    kvpAudioPlayer.Free;
    exit;
  end;

  key := AUDIO_PLAYER_COMMAND_ARGS;

  if kvpAudioPlayer.ContainsKey(key) then begin
    AudioPlayerCommandArgs := kvpAudioPlayer.GetValue(key);

    if AudioPlayerCommandArgs.StartsWith('"') and
       AudioPlayerCommandArgs.EndsWith('"') then begin

      AudioPlayerCommandArgs :=
        AudioPlayerCommandArgs.Trim(charsToStrip);
    end;

    AudioPlayerCommandArgs := AudioPlayerCommandArgs.Trim;
    if AudioPlayerCommandArgs.Length = 0 then begin
      writeLn('error: no value given for ' + key + ' within [' +
              osIdentifier + ']');
      kvpAudioPlayer.Free;
      exit;
    end;

    placeholder := PH_AUDIO_FILE_PATH;
    posPlaceholder := AudioPlayerCommandArgs.IndexOf(placeholder);
    if posPlaceholder = -1 then begin
      writeLn('error: ' + key + ' value does not contain placeholder ' +
              placeholder);
      kvpAudioPlayer.Free;
      exit;
    end;
  end
  else begin
    writeLn('error: ' + AUDIO_INI_FILE_NAME + ' missing value for ' +
            key + ' within [' + osIdentifier + ']');
    kvpAudioPlayer.Free;
    exit;
  end;

  key := AUDIO_PLAYER_RESUME_ARGS;

  if kvpAudioPlayer.ContainsKey(key) then begin
    AudioPlayerResumeArgs := kvpAudioPlayer.GetValue(key).Trim;

    if AudioPlayerResumeArgs.StartsWith('"') and
       AudioPlayerResumeArgs.EndsWith('"') then begin

      AudioPlayerResumeArgs := AudioPlayerResumeArgs.Trim(charsToStrip);
    end;

    AudioPlayerResumeArgs := AudioPlayerResumeArgs.Trim;
    if AudioPlayerResumeArgs.Length > 0 then begin
      placeholder := PH_START_SONG_TIME_OFFSET;
      posPlaceholder := AudioPlayerResumeArgs.IndexOf(placeholder);
      if posPlaceholder = -1 then begin
        writeLn('error: ' + key + ' value does not contain placeholder ' +
                placeholder);
        writeLn('ignoring ' + key + ', using ' +
                AUDIO_PLAYER_COMMAND_ARGS + ' for song resume');
        AudioPlayerResumeArgs := '';
      end;
    end;
  end;

  kvpAudioPlayer.Free;
  kvpAudioPlayer := nil;

  if AudioPlayerResumeArgs.Length = 0 then begin
    AudioPlayerResumeArgs := AudioPlayerCommandArgs;
  end;
end;

//*******************************************************************************

procedure TJukebox.PlaySongList(aSongList: TListSongMetadata; Shuffle: Boolean);
var
  pidAsText: String;
  pidFilePath: String;
begin
  if SongList <> nil then begin
    SongList.Free;
    SongList := nil;
  end;

  SongList := aSongList;
  NumberSongs := aSongList.Count;
  SongIndex := 0;

  if NumberSongs = 0 then begin
    writeLn('no songs in jukebox');
    exit;
  end;

  // does play list directory exist?
  if not JBDirectoryExists(SongPlayDirPath) then begin
    if DebugPrint then begin
      writeLn(DIR_SONG_PLAY + ' directory does not exist, creating it');
    end;
    JBCreateDirectory(SongPlayDirPath);
  end
  else begin
    // play list directory exists, delete any files in it
    if DebugPrint then begin
      writeLn('deleting existing files in ' + DIR_SONG_PLAY + ' directory');
    end;
    JBDeleteFilesInDirectory(SongPlayDirPath);
  end;

  InstallSignalHandlers;

  ReadAudioPlayerConfig;

  if AudioPlayerExeFileName.Length = 0 then begin
    writeLn('error: no audio player configured');
    exit;
  end;

  if Shuffle then begin
    //JBShuffleList(aSongList);
  end;

  writeLn('downloading first song...');

  if DownloadSong(aSongList[0]) then begin
    writeLn('first song downloaded. starting playing now.');

    pidAsText := IntToStr(JBGetPid) + LineEnding;
    pidFilePath := JBPathJoin(JukeboxOptions.Directory,
                              JUKEBOX_PID_FILE_NAME);
    JBFileWriteAllText(pidFilePath, pidAsText);

    try
      while true do begin
        if not ExitRequested then begin
          if not IsPaused then begin
            if SongIndex >= NumberSongs then begin
              SongIndex := 0;
            end;
            DownloadSongs;
            PlaySong(SongList[SongIndex]);
          end;
          if not IsPaused then begin
            inc(SongIndex);
            if SongIndex >= NumberSongs then begin
              SongIndex := 0;
            end;
          end
          else begin
            JBSleepSeconds(1);
          end;
        end
        else begin
          break;
        end;
      end;
    finally
      JBDeleteFile(pidFilePath);
    end;
  end
  else begin
    writeLn('error: unable to download songs');
  end;
end;

//*******************************************************************************

procedure TJukebox.ShowListContainers;
var
  ListContainers: TStringList;
  i: Integer;
begin
  ListContainers := StorageSystem.GetContainerNames;
  if ListContainers.Count > 0 then begin
    for i := 0 to ListContainers.Count-1 do begin
      writeLn(ListContainers[i]);
    end;
  end
  else begin
    writeLn('error: unable to retrieve list of containers');
  end;
  ListContainers.Free;
  ListContainers := nil;
end;

//*******************************************************************************

procedure TJukebox.ShowListings;
begin
  if JukeboxDb <> nil then begin
    JukeboxDb.ShowListings;
  end;
end;

//*******************************************************************************

procedure TJukebox.ShowArtists;
begin
  if JukeboxDb <> nil then begin
    JukeboxDb.ShowArtists;
  end;
end;

//*******************************************************************************

procedure TJukebox.ShowGenres;
begin
  if JukeboxDb <> nil then begin
    JukeboxDb.ShowGenres;
  end;
end;

//*******************************************************************************

procedure TJukebox.ShowAlbums;
begin
  if JukeboxDb <> nil then begin
    JukeboxDb.ShowAlbums;
  end;
end;

//*******************************************************************************

function TJukebox.ReadFileContents(FilePath: String;
                                   out Contents: TMemoryStream): Boolean;
var
  FileContents: TMemoryStream;
  Success: Boolean;
begin
  FileContents := JBFileReadAllBytes(FilePath);
  if FileContents.Size = 0 then begin
    writeLn('error: unable to read file ' + FilePath);
    Contents := nil;
    FileContents.Free;
    Success := false;
  end
  else begin
    Contents := FileContents;
    Success := true;
  end;

  ReadFileContents := Success;
end;

//*******************************************************************************

function TJukebox.UploadMetadataDb: Boolean;
var
  MetadataDbUpload: Boolean;
  HaveMetadataContainer: Boolean;
  DbFilePath: String;
  DbFileContents: TMemoryStream;
begin
  DbFileContents := nil;
  MetadataDbUpload := false;
  HaveMetadataContainer := false;
  if not StorageSystem.HasContainer(MetadataContainer) then
    HaveMetadataContainer :=
      StorageSystem.CreateContainer(MetadataContainer)
  else
    HaveMetadataContainer := true;

  if HaveMetadataContainer then begin
    if JukeboxDb <> nil then begin
      if DebugPrint then
        writeLn('uploading metadata db file to storage system');

      JukeboxDb.Close;
      JukeboxDb := nil;

      DbFilePath := GetMetadataDbFilePath;
      DbFileContents := JBFileReadAllBytes(DbFilePath);

      if DbFileContents.Size > 0 then begin
        MetadataDbUpload := StorageSystem.PutObject(MetadataContainer,
                                                    MetadataDbFile,
                                                    DbFileContents,
                                                    nil);
      end
      else begin
        writeLn('error: unable to read metadata db file');
      end;

      if DbFileContents <> nil then begin
        DbFileContents.Free;
        DbFileContents := nil;
      end;

      if DebugPrint then begin
        if MetadataDbUpload then
          writeLn('metadata db file uploaded')
        else
          writeLn('unable to upload metadata db file');
      end;
    end;
  end;

  UploadMetadataDb := MetadataDbUpload;
end;

//*******************************************************************************

procedure TJukebox.ImportPlaylists;
var
  FileRead: Boolean;
  FileContents: TMemoryStream;
  FileImportCount: Integer;
  HaveContainer: Boolean;
  DirListing: TStringList;
  FullPath: String;
  ObjectName: String;
  FileName: String;
  i: Integer;
begin
  FileContents := nil;

  if JukeboxDb <> nil then begin
    if JukeboxDb.IsOpen then begin
      FileImportCount := 0;
      DirListing := JBListFilesInDirectory(PlaylistImportDirPath);
      if DirListing.Count = 0 then begin
        DirListing.Free;
        DirListing := nil;
        writeLn('no playlists found');
        exit;
      end;

      HaveContainer := false;
      if not StorageSystem.HasContainer(PlaylistContainer) then
        HaveContainer :=
          StorageSystem.CreateContainer(PlaylistContainer)
      else
        HaveContainer := true;

      if not HaveContainer then begin
        writeLn('error: unable to create container for playlists. unable to import');
        exit;
      end;

      for i := 0 to DirListing.Count-1 do begin
        FileName := DirListing[i];
        FullPath := JBPathJoin(PlaylistImportDirPath, FileName);
        ObjectName := FileName;
        FileContents := nil;
        FileRead := ReadFileContents(FullPath, FileContents);
        if FileRead then begin
          if StorageSystem.PutObject(PlaylistContainer,
                                     ObjectName,
                                     FileContents,
                                     nil) then begin
            writeLn('put of playlist succeeded');
            if not StoreSongPlaylist(ObjectName, FileContents) then begin
              writeLn('storing of playlist to db failed');
              StorageSystem.DeleteObject(PlaylistContainer,
                                         ObjectName);
            end
            else begin
              writeLn('storing of playlist succeeded');
              inc(FileImportCount);
            end;
          end;
        end;
        if FileContents <> nil then begin
          FileContents.Free;
          FileContents := nil;
        end;
      end;

      DirListing.Free;
      DirListing := nil;

      if FileImportCount > 0 then begin
        writeLn(IntToStr(FileImportCount) + ' playlists imported');
        UploadMetadataDb;
      end
      else begin
        writeLn('no files imported');
      end;
    end;
  end;
end;

//*******************************************************************************

procedure TJukebox.ShowPlaylists;
var
  ContainerContents: TStringList;
  NumberPlaylists: Integer;
  i: Integer;
begin
  ContainerContents :=
    StorageSystem.ListContainerContents(PlaylistContainer);

  NumberPlaylists := ContainerContents.Count;
  if NumberPlaylists > 0 then begin
    for i := 0 to NumberPlaylists-1 do begin
      writeLn(ContainerContents[i]);
    end;
  end
  else begin
    writeLn('no playlists found');
  end;

  ContainerContents.Free;
  ContainerContents := nil;
end;

//*******************************************************************************

function TJukebox.GetAlbumTrackObjectList(Artist: String;
                                          AlbumName: String;
                                          ListTrackObjects: TStringList): Boolean;
var
  Success: Boolean;
  EncodedArtistAlbum: String;
  JsonFileName: String;
  LocalJsonFile: String;
  FileStream: TFileStream;
  Parser: TJSONParser;
  jData: TJSONData;
  jObject: TJSONObject;
  jArray: TJSONArray;
  i: Integer;
  TrackObject: String;
begin
  Success := false;
  EncodedArtistAlbum := EncodeArtistAlbum(Artist, AlbumName);
  JsonFileName := EncodedArtistAlbum + JSON_FILE_EXT;
  LocalJsonFile := JBPathJoin(SongPlayDirPath, JsonFileName);
  if StorageSystem.GetObject(AlbumContainer,
                             JsonFileName,
                             LocalJsonFile) > 0 then begin

    FileStream := TFileStream.Create(LocalJsonFile, fmOpenRead);
    try
      Parser := TJSONParser.Create(FileStream);
      try
        try
          jData := Parser.Parse;
          jObject := jData as TJSONObject;
          jArray := jObject.Arrays['tracks'];

          for i := 0 to jArray.Count-1 do begin
            jObject := jArray.Items[i] as TJSONObject;
            TrackObject := jObject.Get('object');
            ListTrackObjects.Add(TrackObject);
          end;
          Success := true;
        except
          on e: exception do writeLn('exception: ' + e.message);
        end;
      finally
        Parser.Free;
        Parser := nil;
      end;
    finally
      FileStream.Free;
      FileStream := nil;
    end;
  end
  else begin
    writeLn('Unable to retrieve ' + JsonFileName + ' from ' + AlbumContainer);
  end;
  GetAlbumTrackObjectList := Success;
end;

//*******************************************************************************

function TJukebox.GetPlaylistSongs(PlaylistName: String;
                                   ListSongs: TListSongMetadata): Boolean;
var
  Success: Boolean;
  JsonFileName: String;
  LocalJsonFile: String;
  PlaylistJsonContents: String;
  FileExtensions: TStringList;
  SongsAdded: Integer;
  SongFound: Boolean;
  FileStream: TFileStream;
  Parser: TJSONParser;
  jData: TJSONData;
  jObject: TJSONObject;
  jArray: TJSONArray;
  SongArtist: String;
  SongAlbum: String;
  SongName: String;
  EncodedSong: String;
  i: Integer;
  j: Integer;
  FileExtension: String;
  SongUid: String;
  Song: TSongMetadata;
begin
  Success := false;
  SongsAdded := 0;

  JsonFileName := EncodeValue(PlaylistName) + JSON_FILE_EXT;
  LocalJsonFile := JBPathJoin(SongPlayDirPath, JsonFileName);

  if StorageSystem.GetObject(PlaylistContainer,
                             JsonFileName,
                             LocalJsonFile) > 0 then begin

    FileStream := TFileStream.Create(LocalJsonFile, fmOpenRead);
    
    try
      Parser := TJSONParser.Create(FileStream);
   
      try
        try
          jData := Parser.Parse;
          jObject := jData as TJSONObject;
          jArray := jObject.Arrays['songs'];

          FileExtensions := TStringList.Create;
          FileExtensions.Add('.flac');
          FileExtensions.Add('.m4a');
          FileExtensions.Add('.mp3');

          for i := 0 to jArray.Count-1 do begin
            jObject := jArray.Items[i] as TJSONObject;
            SongArtist := jObject.Get('artist');
            SongAlbum := jObject.Get('album');
            SongName := jObject.Get('song');
            EncodedSong := EncodeArtistAlbumSong(SongArtist,
                                                 SongAlbum,
                                                 SongName);
            SongFound := false;

            for j := 0 to FileExtensions.Count-1 do begin
              FileExtension := FileExtensions[j];
              SongUid := EncodedSong + FileExtension;
              Song := JukeboxDB.RetrieveSong(SongUid);

              if Song <> nil then begin
                ListSongs.Add(Song);
                inc(SongsAdded);
                SongFound := true;
                break;
              end;
            end;

            if not SongFound then begin
              writeLn('error: unable to retrieve metadata for ' + EncodedSong);
            end;
          end;
        except
          on e: exception do writeLn('exception: ' + e.message);
        end;
      finally
        Parser.Free;
        Parser := nil;

        if FileExtensions <> nil then begin
          FileExtensions.Free;
          FileExtensions := nil;
        end;
      end;
    finally
      FileStream.Free;
      FileStream := nil;
    end;
  end
  else begin
    writeLn('Unable to retrieve ' + JsonFileName + ' from ' + PlaylistContainer);
  end;
  GetPlaylistSongs := Success;
end;

//*******************************************************************************

procedure TJukebox.ShowAlbum(Artist: String; Album: String);
var
  ListTrackObjects: TStringList;
  SongName: String;
  SongObject: String;
  i: Integer;
  j: Integer;
begin
  ListTrackObjects := TStringList.Create;
  if GetAlbumTrackObjectList(Artist, Album, ListTrackObjects) then begin
    writeLn('Album: ' + Album + ' (' + Artist + ')');
    i := 1;
    for j := 0 to ListTrackObjects.Count-1 do begin
      SongObject := ListTrackObjects[j];
      SongName := SongFromFileName(SongObject);
      writeLn(IntToStr(i) + '  ' + SongName);
      inc(i);
    end;
  end
  else begin
    writeLn('Unable to retrieve album ' + Artist + '/' + Album);
  end;
  ListTrackObjects.Free;
  ListTrackObjects := nil;
end;

//*******************************************************************************

procedure TJukebox.ShowPlaylist(Playlist: String);
var
  ListSongs: TListSongMetadata;
  i: Integer;
  Song: TSongMetadata;
begin
  ListSongs := TListSongMetadata.Create;
  if GetPlaylistSongs(Playlist, ListSongs) then begin
    for i := 0 to ListSongs.Count-1 do begin
      Song := ListSongs[i];
      writeLn(Song.SongName + ' : ' + Song.ArtistName);
    end;
  end
  else begin
    writeLn('unable to retrieve playlist ' + Playlist +
            ' in ' + PlaylistContainer);
  end;
  ListSongs.Free;
  ListSongs := nil;
end;

//*******************************************************************************

procedure TJukebox.PlayPlaylist(Playlist: String);
var
  PlaylistSongsFound: Boolean;
  TheListSongs: TListSongMetadata;
begin
  //ScopePlaylist = Playlist;
  PlaylistSongsFound := false;
  TheListSongs := TListSongMetadata.Create;
  if GetPlaylistSongs(Playlist, TheListSongs) then begin
    if TheListSongs.Count > 0 then begin
      PlaylistSongsFound := true;
      PlaySongList(TheListSongs, false);
    end
  end;

  if not PlaylistSongsFound then begin
    writeLn('error: unable to retrieve playlist songs');
  end;
end;

//*******************************************************************************

procedure TJukebox.PlayAlbum(Artist: String; Album: String);
begin
  PlaySongs(false, Artist, Album);
end;

//*******************************************************************************

function TJukebox.DeleteSong(SongUid: String; UploadMetadata: Boolean): Boolean;
var
  IsDeleted: Boolean;
  DbDeleted: Boolean;
  Container: String;
  SsDeleted: Boolean;
begin
  IsDeleted := false;
  if SongUid.Length > 0 then begin
    if JukeboxDb <> nil then begin
      //DbDeleted := JukeboxDb.DeleteSong(SongUid);
      //Container := ContainerForSong(SongUid);
      //if Container.Length > 0 then begin
      //  SsDeleted := StorageSystem.DeleteObject(Container, SongUid);
      //  if DbDeleted and UploadMetadata then
      //    UploadMetadataDb;
      //  IsDeleted := DbDeleted or SsDeleted;
      //end;
    end;
  end;

  DeleteSong := IsDeleted;
end;

//*******************************************************************************

function TJukebox.DeleteArtist(Artist: String): Boolean;
var
  IsDeleted: Boolean;
  TheSongList: TListSongMetadata;
  i: Integer;
  Song: TSongMetadata;
begin
  IsDeleted := false;
  if Artist.Length > 0 then begin
    if JukeboxDb <> nil then begin
      TheSongList := JukeboxDb.RetrieveSongs(Artist, '');
      if TheSongList.Count = 0 then begin
        writeLn('no artist songs in jukebox');
      end
      else begin
        //for i := 0 to TheSongList.Count-1 do begin
        //  Song := TheSongList[i];
        //  if not DeleteSong(Song.Fm.ObjectName, false) then begin
        //    writeLn('error deleting song ' + Song.Fm.ObjectName);
        //    DeleteArtist := false;
        //    exit;
        //  end;
        //end;
        UploadMetadataDb;
        IsDeleted := true;
      end;
      TheSongList.Free;
      TheSongList := nil;
    end;
  end;

  DeleteArtist := IsDeleted;
end;

//*******************************************************************************

function TJukebox.DeleteAlbum(Album: String): Boolean;
var
  AlbumDeleted: Boolean;
  ContainsDoubleDash: Boolean;
  Artist: String;
  AlbumName: String;
  NumSongsDeleted: Integer;
  i: Integer;
  ListAlbumSongs: TListSongMetadata;
  Song: TSongMetadata;
  NameComponents: TStringArray;
begin
  AlbumDeleted := false;
  ContainsDoubleDash := Album.Contains(DOUBLE_DASHES);
  if ContainsDoubleDash then begin
    NameComponents := Album.Split(DOUBLE_DASHES);
    if Length(NameComponents) = 2 then begin
      Artist := NameComponents[0];
      AlbumName := NameComponents[1];
      if JukeboxDb <> nil then begin
        ListAlbumSongs := JukeboxDb.RetrieveSongs(Artist, AlbumName);
        if ListAlbumSongs.Count > 0 then begin
          NumSongsDeleted := 0;
          for i := 0 to ListAlbumSongs.Count-1 do begin
            Song := ListAlbumSongs[i];
            writeLn(Song.Fm.ContainerName + ' ' + Song.Fm.ObjectName);
            // delete each song audio file
            //DIFFERENCE
            if StorageSystem.DeleteObject(ContainerPrefix + Song.Fm.ContainerName,
                                          Song.Fm.ObjectName) then begin
              //inc(NumSongsDeleted);
              // delete song metadata
              //JukeboxDb.DeleteSong(Song.Fm.ObjectName);
            end
            else begin
              writeLn('error: unable to delete song ' +
                      Song.Fm.ObjectName);
            end;
          end;
          if NumSongsDeleted > 0 then begin
            // upload metadata db
            UploadMetadataDb;
            AlbumDeleted := true;
          end
          else begin
            writeLn('no songs found for artist=' + Artist +
                    ' album name=' + AlbumName);
          end;
        end;
        ListAlbumSongs.Free;
        ListAlbumSongs := nil;
      end;
    end;
  end
  else begin
    writeLn('specify album with the-artist--the-song-name format');
  end;

  DeleteAlbum := AlbumDeleted;
end;

//*******************************************************************************

function TJukebox.DeletePlaylist(PlaylistName: String): Boolean;
var
  IsDeleted: Boolean;
  ObjectName: String;
  DbDeleted: Boolean;
begin
  IsDeleted := false;
  if JukeboxDb <> nil then begin
    ObjectName := JukeboxDb.GetPlaylist(PlaylistName);
    if ObjectName.Length > 0 then begin
    end
    else begin
      writeLn('invalid playlist name');
    end;
  end;

  DeletePlaylist := IsDeleted;
end;

//*******************************************************************************

procedure TJukebox.ImportAlbumArt;
var
  FileRead: Boolean;
  FileContents: TMemoryStream;
  FileImportCount: Integer;
  HaveContainer: Boolean;
  FullPath: String;
  DirListing: TStringList;
  ObjectName: String;
  FileName: String;
  i: Integer;
begin
  if JukeboxDb <> nil then begin
    if JukeboxDb.IsOpen then begin
      FileImportCount := 0;
      DirListing := JBListFilesInDirectory(AlbumArtImportDirPath);
      if DirListing.Count = 0 then begin
        DirListing.Free;
        DirListing := nil;
        writeLn('no album art found');
        exit;
      end;

      HaveContainer := false;

      if not StorageSystem.HasContainer(AlbumArtContainer) then
        HaveContainer :=
          StorageSystem.CreateContainer(AlbumArtContainer)
      else
        HaveContainer := true;

      if not HaveContainer then begin
        writeLn('error: unable to create container for album art. unable to import');
        exit;
      end;

      for i := 0 to DirListing.Count-1 do begin
        FileName := DirListing[i];
        FullPath := JBPathJoin(AlbumArtImportDirPath, FileName);
        ObjectName := FileName;
        FileContents := nil;
        FileRead := ReadFileContents(FullPath, FileContents);
        if FileRead then begin
          if StorageSystem.PutObject(AlbumArtContainer,
                                     ObjectName,
                                     FileContents,
                                     nil) then begin
            inc(FileImportCount);
          end;
        end;
        if FileContents <> nil then begin
          FileContents.Free;
          FileContents := nil;
        end;
      end;

      if FileImportCount > 0 then begin
        writeLn(IntToStr(FileImportCount) + ' album art files imported');
      end
      else begin
        writeLn('no files imported');
      end;
    end;
  end;
end;

//*******************************************************************************

end.

