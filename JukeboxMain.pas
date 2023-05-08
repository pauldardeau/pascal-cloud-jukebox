unit JukeboxMain;

{$MODE OBJFPC}{$H+}{$J-}

interface

uses
  Classes, JBSysUtils, Jukebox, PropertySet, StorageSystem, SysUtils,
  S3ExtStorageSystem, StringSet, ArgumentParser, JukeboxOptions,
  PropertyValue;

const
  ARG_PREFIX           = '--';
  ARG_DEBUG            = 'debug';
  ARG_FILE_CACHE_COUNT = 'file-cache-count';
  ARG_INTEGRITY_CHECKS = 'integrity-checks';
  ARG_STORAGE          = 'storage';
  ARG_ARTIST           = 'artist';
  ARG_PLAYLIST         = 'playlist';
  ARG_SONG             = 'song';
  ARG_ALBUM            = 'album';
  ARG_COMMAND          = 'command';
  ARG_FORMAT           = 'format';
  ARG_DIRECTORY        = 'directory';

  CMD_DELETE_ALBUM       = 'delete-album';
  CMD_DELETE_ARTIST      = 'delete-artist';
  CMD_DELETE_PLAYLIST    = 'delete-playlist';
  CMD_DELETE_SONG        = 'delete-song';
  CMD_EXPORT_ALBUM       = 'export-album';
  CMD_EXPORT_ARTIST      = 'export-artist';
  CMD_EXPORT_PLAYLIST    = 'export-playlist';
  CMD_HELP               = 'help';
  CMD_IMPORT_ALBUM       = 'import-album';
  CMD_IMPORT_ALBUM_ART   = 'import-album-art';
  CMD_IMPORT_PLAYLISTS   = 'import-playlists';
  CMD_IMPORT_SONGS       = 'import-songs';
  CMD_INIT_STORAGE       = 'init-storage';
  CMD_LIST_ALBUMS        = 'list-albums';
  CMD_LIST_ARTISTS       = 'list-artists';
  CMD_LIST_CONTAINERS    = 'list-containers';
  CMD_LIST_GENRES        = 'list-genres';
  CMD_LIST_PLAYLISTS     = 'list-playlists';
  CMD_LIST_SONGS         = 'list-songs';
  CMD_PLAY               = 'play';
  CMD_PLAY_ALBUM         = 'play-album';
  CMD_PLAY_PLAYLIST      = 'play-playlist';
  CMD_RETRIEVE_CATALOG   = 'retrieve-catalog';
  CMD_SHOW_ALBUM         = 'show-album';
  CMD_SHOW_PLAYLIST      = 'show-playlist';
  CMD_SHUFFLE_PLAY       = 'shuffle-play';
  CMD_UPLOAD_METADATA_DB = 'upload-metadata-db';
  CMD_USAGE              = 'usage';

  SS_FS = 'fs';
  SS_S3 = 's3';

  CREDS_FILE_SUFFIX      = '_creds.txt';
  CREDS_CONTAINER_PREFIX = 'container_prefix';

  S3_ENDPOINT_URL = 'endpoint_url';
  S3_REGION       = 'region';

  FS_ROOT_DIR = 'root_dir';

  AUDIO_FILE_TYPE_MP3  = 'mp3';
  AUDIO_FILE_TYPE_M4A  = 'm4a';
  AUDIO_FILE_TYPE_FLAC = 'flac';

type
  TJukeboxMain = Class(TObject)
  private
    Artist: String;
    Album: String;
    Song: String;
    Playlist: String;
    DebugMode: Boolean;
    Directory: String;
	JukeboxOptions: TJukeboxOptions;
	Jukebox: TJukebox;
	StorageSystem: TStorageSystem;

  public
    constructor Create;
	destructor Destroy; override;
    //function ConnectFsSystem(Credentials: TPropertySet;
    //                         Prefix: String): TStorageSystem;
    function ConnectS3System(Credentials: TPropertySet;
                             Prefix: String): TStorageSystem;
    function ConnectStorageSystem(SystemName: String;
                                  Credentials: TPropertySet;
                                  Prefix: String): TStorageSystem;
    function InitStorageSystem(StorageSys: TStorageSystem;
                               ContainerPrefix: String): Boolean;
    procedure ShowUsage;
    function RunJukeboxCommand(Command: String): Integer;
    function Run(ConsoleArgs: TStringList): Int32;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TJukeboxMain.Create;
begin
  inherited;
  StorageSystem := nil;
  JukeboxOptions := nil;
  Jukebox := nil;
  DebugMode := false;
end;

//*******************************************************************************

destructor TJukeboxMain.Destroy;
begin
  writeLn('TJukeboxMain.Destroy');
  if Jukebox <> nil then begin
    Jukebox.Leave;
    Jukebox.Free;
	Jukebox := nil;
  end;
  
  if JukeboxOptions <> nil then begin
    JukeboxOptions.Free;
	JukeboxOptions := nil;
  end;
  
  if StorageSystem <> nil then begin
    StorageSystem.Leave;
	StorageSystem.Free;
	StorageSystem := nil;
  end;
  
  inherited;
end;

//*******************************************************************************

{
function TJukeboxMain.ConnectFsSystem(Credentials: TPropertySet;
                                      Prefix: String): TStorageSystem;
var
  RootDir: String;
begin
  if Credentials.Contains(FS_ROOT_DIR) then begin
    RootDir := Credentials.GetStringValue(FS_ROOT_DIR);
    if DebugMode then begin
      writeLn(FS_ROOT_DIR + ' = ' + RootDir);
    end;
    ConnectFsSystem := TFSStorageSystem.Create(RootDir, DebugMode);
    exit;
  end
  else begin
    writeLn('error: ' + FS_ROOT_DIR + ' must be specified in ' +
            SS_FS + CREDS_FILE_SUFFIX);
    ConnectFsSystem := nil;
    exit;
  end;
end;
}
//*******************************************************************************

function TJukeboxMain.ConnectS3System(Credentials: TPropertySet;
                                      Prefix: String): TStorageSystem;
var
  theEndpointUrl: String;
  theRegion: String;
begin
  theEndpointUrl := '';
  theRegion := '';

  if Credentials.Contains(S3_ENDPOINT_URL) then begin
    theEndpointUrl := Credentials.GetStringValue(S3_ENDPOINT_URL);
  end;

  if Credentials.Contains(S3_REGION) then begin
    theRegion := Credentials.GetStringValue(S3_REGION);
  end;

  if DebugMode then begin
    writeLn(S3_ENDPOINT_URL + ' = ' + theEndpointUrl);
    if (theRegion.Length > 0) then begin
      writeLn(S3_REGION + ' = ' + theRegion);
    end;
  end;

  ConnectS3System := TS3ExtStorageSystem.Create(theEndpointUrl,
                                                theRegion,
                                                Directory,
                                                DebugMode);
end;

//*******************************************************************************

function TJukeboxMain.ConnectStorageSystem(SystemName: String;
                                           Credentials: TPropertySet;
                                           Prefix: String): TStorageSystem;
begin
  //if SystemName = SS_FS then begin
  //  ConnectStorageSystem := ConnectFsSystem(Credentials, Prefix);
  //end
  if (SystemName = SS_S3) or (SystemName= 's3ext') then begin
    ConnectStorageSystem := ConnectS3System(Credentials, Prefix);
  end
  else begin
    writeLn('error: unrecognized storage system ' + SystemName);
    ConnectStorageSystem := nil;
  end;
end;

//*******************************************************************************

function TJukeboxMain.InitStorageSystem(StorageSys: TStorageSystem;
                                        ContainerPrefix: String): Boolean;
var
  Success: Boolean;
begin
  if TJukebox.InitializeStorageSystem(StorageSys,
                                      ContainerPrefix,
                                      DebugMode) then begin
    writeLn('storage system successfully initialized');
    Success := true;
  end
  else begin
    writeLn('error: unable to initialize storage system');
    Success := false;
  end;
  InitStorageSystem := Success;
end;

//*******************************************************************************

procedure TJukeboxMain.ShowUsage;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  sb.Append('Supported Commands:');
  sb.Append(LineEnding);

  sb.Append(CMD_DELETE_ALBUM + '       - delete specified album');
  sb.Append(LineEnding);

  sb.Append(CMD_DELETE_ARTIST + '      - delete specified artist');
  sb.Append(LineEnding);

  sb.Append(CMD_DELETE_PLAYLIST + '    - delete specified playlist');
  sb.Append(LineEnding);

  sb.Append(CMD_DELETE_SONG + '        - delete specified song');
  sb.Append(LineEnding);

  sb.Append(CMD_EXPORT_ALBUM + '       - FUTURE');
  sb.Append(LineEnding);

  sb.Append(CMD_EXPORT_ARTIST + '      - FUTURE');
  sb.Append(LineEnding);

  sb.Append(CMD_EXPORT_PLAYLIST + '    - FUTURE');
  sb.Append(LineEnding);

  sb.Append(CMD_HELP + '               - show this help message');
  sb.Append(LineEnding);
  
  sb.Append(CMD_IMPORT_ALBUM_ART + '   - import all album art from album-art-import subdirectory');
  sb.Append(LineEnding);

  sb.Append(CMD_IMPORT_PLAYLISTS + '   - import all new playlists from playlist-import subdirectory');
  sb.Append(LineEnding);

  sb.Append(CMD_IMPORT_SONGS + '       - import all new songs from song-import subdirectory');
  sb.Append(LineEnding);

  sb.Append(CMD_INIT_STORAGE + '       - initialize storage system');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_ALBUMS + '        - show listing of all available albums');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_ARTISTS + '       - show listing of all available artists');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_CONTAINERS + '    - show listing of all available storage containers');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_GENRES + '        - show listing of all available genres');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_PLAYLISTS + '     - show listing of all available playlists');
  sb.Append(LineEnding);

  sb.Append(CMD_LIST_SONGS + '         - show listing of all available songs');
  sb.Append(LineEnding);

  sb.Append(CMD_PLAY + '               - start playing songs');
  sb.Append(LineEnding);

  sb.Append(CMD_PLAY_PLAYLIST + '      - play specified playlist');
  sb.Append(LineEnding);

  sb.Append(CMD_SHOW_ALBUM + '         - show songs in a specified album');
  sb.Append(LineEnding);

  sb.Append(CMD_SHOW_PLAYLIST + '      - show songs in specified playlist');
  sb.Append(LineEnding);

  sb.Append(CMD_SHUFFLE_PLAY + '       - play songs randomly');
  sb.Append(LineEnding);

  sb.Append(CMD_RETRIEVE_CATALOG + '   - retrieve copy of music catalog');
  sb.Append(LineEnding);

  sb.Append(CMD_UPLOAD_METADATA_DB + ' - upload SQLite metadata');
  sb.Append(LineEnding);

  sb.Append(CMD_USAGE + '              - show this help message');
  sb.Append(LineEnding);

  writeLn(sb.ToString);
  sb.Free;
end;

//*******************************************************************************

function TJukeboxMain.RunJukeboxCommand(Command: String): Integer;
var
  ExitCode: Integer;
  Shuffle: Boolean;
begin
  ExitCode := 0;
  Shuffle := false;

  if Command = CMD_IMPORT_SONGS then begin
    jukebox.ImportSongs;
  end
  else if Command = CMD_IMPORT_PLAYLISTS then begin
    jukebox.ImportPlaylists;
  end
  else if Command = CMD_PLAY then begin
    jukebox.PlaySongs(Shuffle, Artist, Album);
  end
  else if Command = CMD_SHUFFLE_PLAY then begin
    Shuffle := true;
    jukebox.PlaySongs(Shuffle, Artist, Album);
  end
  else if Command = CMD_LIST_SONGS then begin
    jukebox.ShowListings;
  end
  else if Command = CMD_LIST_ARTISTS then begin
    jukebox.ShowArtists;
  end
  else if Command = CMD_LIST_CONTAINERS then begin
    jukebox.ShowListContainers;
  end
  else if Command = CMD_LIST_GENRES then begin
    jukebox.ShowGenres;
  end
  else if Command = CMD_LIST_ALBUMS then begin
    jukebox.ShowAlbums;
  end
  else if Command = CMD_SHOW_ALBUM then begin
    if (Artist.Length > 0) and (Album.Length > 0) then begin
      jukebox.ShowAlbum(Artist, Album);
    end
    else begin
      writeLn('error: artist and album must be specified using ' +
              ARG_PREFIX + ARG_ARTIST + ' and ' +
              ARG_PREFIX + ARG_ALBUM + ' options');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_PLAY_ALBUM then begin
    if (Artist.Length > 0) and (Album.Length > 0) then begin
      jukebox.PlayAlbum(Artist, Album);
    end
    else begin
      writeLn('error: artist and album must be specified using ' +
              ARG_PREFIX + ARG_ARTIST + ' and ' + ARG_PREFIX + ARG_ALBUM +
              ' options');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_LIST_PLAYLISTS then begin
    jukebox.ShowPlaylists;
  end
  else if Command = CMD_SHOW_PLAYLIST then begin
    if Playlist.Length > 0 then begin
      jukebox.ShowPlaylist(Playlist);
    end
    else begin
      writeLn('error: playlist must be specified using ' +
              ARG_PREFIX + ARG_PLAYLIST + ' option');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_PLAY_PLAYLIST then begin
    if Playlist.Length > 0 then begin
      jukebox.PlayPlaylist(Playlist);
    end
    else begin
      writeLn('error: playlist must be specified using ' +
              ARG_PREFIX + ARG_PLAYLIST + ' option');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_RETRIEVE_CATALOG then begin
    writeLn(CMD_RETRIEVE_CATALOG + ' not yet implemented');
  end
  else if Command = CMD_DELETE_SONG then begin
    if Song.Length > 0 then begin
      if jukebox.DeleteSong(Song, true) then begin
        writeLn('song deleted');
      end
      else begin
        writeLn('error: unable to delete song');
        ExitCode := 1;
      end;
    end
    else begin
      writeLn('error: song must be specified using ' +
              ARG_PREFIX + ARG_SONG + ' option');
      ExitCode := 1;
    end
  end
  else if Command = CMD_DELETE_ARTIST then begin
    if Artist.Length > 0 then begin
      if jukebox.DeleteArtist(Artist) then begin
        writeLn('artist deleted');
      end
      else begin
        writeLn('error: unable to delete artist');
        ExitCode := 1;
      end
    end
    else begin
      writeLn('error: artist must be specified using ' +
              ARG_PREFIX + ARG_ARTIST + ' option');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_DELETE_ALBUM then begin
    if Album.Length > 0 then begin
      if jukebox.DeleteAlbum(Album) then begin
        writeLn('album deleted');
      end
      else begin
        writeLn('error: unable to delete album');
        ExitCode := 1;
      end;
    end
    else begin
      writeLn('error: album must be specified using ' +
              ARG_PREFIX + ARG_ALBUM + ' option');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_DELETE_PLAYLIST then begin
    if Playlist.Length > 0 then begin
      if jukebox.DeletePlaylist(Playlist) then begin
        writeLn('playlist deleted');
      end
      else begin
        writeLn('error: unable to delete playlist');
        ExitCode := 1;
      end;
    end
    else begin
      writeLn('error: playlist must be specified using ' +
              ARG_PREFIX + ARG_PLAYLIST + ' option');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_UPLOAD_METADATA_DB then begin
    if jukebox.UploadMetadataDb then begin
      writeLn('metadata db uploaded');
    end
    else begin
      writeLn('error: unable to upload metadata db');
      ExitCode := 1;
    end;
  end
  else if Command = CMD_IMPORT_ALBUM_ART then begin
    jukebox.ImportAlbumArt;
  end;

  RunJukeboxCommand := ExitCode;
end;

//*******************************************************************************

function TJukeboxMain.Run(ConsoleArgs: TStringList): Int32;
var
  SupportedSystems: TStringSet;
  HelpCommands: TStringSet;
  NonHelpCommands: TStringSet;
  UpdateCommands: TStringSet;
  AllCommands: TStringSet;
  Creds: TPropertySet;
  ExitCode: Integer;
  StorageType: String;
  OptParser: TArgumentParser;
  Args: TPropertySet;
  Storage: String;
  ContainerPrefix: String;
  CredsFile: String;
  CredsFilePath: String;
  Key: String;
  Value: String;
  Command: String;
  jb: TJukebox;
  FileContents: String;
  FileLines: TStringArray;
  FileLine: String;
  LineTokens: TStringArray;
  i: Integer;
  FileCacheCount: Integer;
begin
  ExitCode := 0;
  StorageType := SS_FS;
  Artist := '';
  Album := '';
  Song := '';
  Playlist := '';
  
  SupportedSystems := nil;
  HelpCommands := nil;
  NonHelpCommands := nil;
  UpdateCommands := nil;
  AllCommands := nil;
  Creds := nil;

  OptParser := TArgumentParser.Create(false);
  OptParser.AddOptionalBoolFlag(ARG_PREFIX+ARG_DEBUG, 'run in debug mode');
  OptParser.AddOptionalIntArgument(ARG_PREFIX+ARG_FILE_CACHE_COUNT, 'number of songs to buffer in cache');
  OptParser.AddOptionalBoolFlag(ARG_PREFIX+ARG_INTEGRITY_CHECKS, 'check file integrity after download');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_STORAGE, 'storage system type (s3, fs)');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_ARTIST, 'limit operations to specified artist');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_PLAYLIST, 'limit operations to specified playlist');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_SONG, 'limit operations to specified song');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_ALBUM, 'limit operations to specified album');
  OptParser.AddOptionalStringArgument(ARG_PREFIX+ARG_DIRECTORY, 'specify directory where audio player should run');
  OptParser.AddRequiredArgument(ARG_COMMAND, 'command for jukebox');

  Args := OptParser.ParseArgs(ConsoleArgs);
  if Args = nil then begin
    writeLn('error: unable to obtain command-line arguments');
    Run := 1;
    exit;
  end;
  
  OptParser.Free;
  OptParser := nil;

  JukeboxOptions := TJukeboxOptions.Create;

  if Args.Contains(ARG_DEBUG) then begin
    DebugMode := true;
    JukeboxOptions.DebugMode := true;
  end;

  if Args.Contains(ARG_FILE_CACHE_COUNT) then begin
    FileCacheCount := Args.GetIntValue(ARG_FILE_CACHE_COUNT);
    if DebugMode then begin
      writeLn('setting file cache count=' + IntToStr(FileCacheCount));
    end;
    JukeboxOptions.FileCacheCount := FileCacheCount;
  end;

  if Args.Contains(ARG_INTEGRITY_CHECKS) then begin
    if DebugMode then begin
      writeLn('setting integrity checks on');
    end;
    JukeboxOptions.CheckDataIntegrity := true;
  end;

  if Args.Contains(ARG_STORAGE) then begin
    Storage := Args.GetStringValue(ARG_STORAGE);
    SupportedSystems := TStringSet.Create;
    SupportedSystems.Add(SS_FS);
    SupportedSystems.Add(SS_S3);
    if not SupportedSystems.Contains(Storage) then begin
      writeLn('error: invalid storage type ' + Storage);
      writeLn('supported systems are: ' + SupportedSystems.ToString);
	  SupportedSystems.Free;
	  SupportedSystems := nil;
	  Args.Free;
      Run := 1;
      exit;
    end
    else begin
	  SupportedSystems.Free;
	  SupportedSystems := nil;
	  
      if DebugMode then begin
        writeLn('setting storage system to ' + Storage);
      end;
      StorageType := Storage;
    end;
  end;

  if Args.Contains(ARG_ARTIST) then begin
    Artist := Args.GetStringValue(ARG_ARTIST);
  end;

  if Args.Contains(ARG_PLAYLIST) then begin
    Playlist := Args.GetStringValue(ARG_PLAYLIST);
  end;

  if Args.Contains(ARG_SONG) then begin
    Song := Args.GetStringValue(ARG_SONG);
  end;

  if Args.Contains(ARG_ALBUM) then begin
    Album := Args.GetStringValue(ARG_ALBUM);
  end;

  if Args.Contains(ARG_DIRECTORY) then begin
    Directory := Args.GetStringValue(ARG_DIRECTORY);
  end
  else begin
    Directory := JBGetCurrentDirectory;
  end;

  if Args.Contains(ARG_COMMAND) then begin
    if DebugMode then begin
      writeLn('using storage system type ' + StorageType);
    end;

    ContainerPrefix := '';
    CredsFile := StorageType + CREDS_FILE_SUFFIX;
    Creds := TPropertySet.Create;
    CredsFilePath := JBPathJoin(Directory, CredsFile);

    if JBFileExists(CredsFilePath) then begin
      if DebugMode then begin
        writeLn('reading creds file ' + CredsFilePath);
      end;

      FileContents := JBFileReadAllText(CredsFilePath);
      if FileContents.Length > 0 then begin
        FileLines := FileContents.Split(LineEnding);

        for i := 0 to Length(FileLines)-1 do begin
          FileLine := FileLines[i];
          LineTokens := FileLine.Split('=');
          if Length(LineTokens) = 2 then begin
            Key := LineTokens[0].Trim;
            Value := LineTokens[1].Trim;
            if (Key.Length > 0) and (Value.Length > 0) then begin
              Creds.Add(Key, TPropertyValue.Create(Value));
              if Key = CREDS_CONTAINER_PREFIX then begin
                ContainerPrefix := Value;
              end;
            end;
          end;
        end;
      end
      else begin
        if DebugMode then begin
          writeLn('error: unable to read file ' + CredsFilePath);
        end;
      end;
    end
    else begin
      writeLn('no creds file (' + CredsFilePath + ')');
    end;

    if DebugMode then begin
      writeLn('completed processing of creds file');
    end;

    Command := Args.GetStringValue(ARG_COMMAND);
	
	Args.Free;
	Args := nil;

    HelpCommands := TStringSet.Create;
    HelpCommands.Add(CMD_HELP);
    HelpCommands.Add(CMD_USAGE);

    NonHelpCommands := TStringSet.Create;
    NonHelpCommands.Add(CMD_IMPORT_SONGS);
    NonHelpCommands.Add(CMD_PLAY);
    NonHelpCommands.Add(CMD_SHUFFLE_PLAY);
    NonHelpCommands.Add(CMD_LIST_SONGS);
    NonHelpCommands.Add(CMD_LIST_ARTISTS);
    NonHelpCommands.Add(CMD_LIST_CONTAINERS);
    NonHelpCommands.Add(CMD_LIST_GENRES);
    NonHelpCommands.Add(CMD_LIST_ALBUMS);
    NonHelpCommands.Add(CMD_RETRIEVE_CATALOG);
    NonHelpCommands.Add(CMD_IMPORT_PLAYLISTS);
    NonHelpCommands.Add(CMD_LIST_PLAYLISTS);
    NonHelpCommands.Add(CMD_SHOW_ALBUM);
    NonHelpCommands.Add(CMD_PLAY_ALBUM);
    NonHelpCommands.Add(CMD_SHOW_PLAYLIST);
    NonHelpCommands.Add(CMD_PLAY_PLAYLIST);
    NonHelpCommands.Add(CMD_DELETE_SONG);
    NonHelpCommands.Add(CMD_DELETE_ALBUM);
    NonHelpCommands.Add(CMD_DELETE_PLAYLIST);
    NonHelpCommands.Add(CMD_DELETE_ARTIST);
    NonHelpCommands.Add(CMD_UPLOAD_METADATA_DB);
    NonHelpCommands.Add(CMD_IMPORT_ALBUM_ART);

    // Commands that will alter the cloud storage (or content)
    // These commands may require a different set of credentials
    UpdateCommands := TStringSet.Create;
    UpdateCommands.Add(CMD_IMPORT_SONGS);
    UpdateCommands.Add(CMD_IMPORT_PLAYLISTS);
    UpdateCommands.Add(CMD_DELETE_SONG);
    UpdateCommands.Add(CMD_DELETE_ALBUM);
    UpdateCommands.Add(CMD_DELETE_PLAYLIST);
    UpdateCommands.Add(CMD_DELETE_ARTIST);
    UpdateCommands.Add(CMD_UPLOAD_METADATA_DB);
    UpdateCommands.Add(CMD_IMPORT_ALBUM_ART);
    UpdateCommands.Add(CMD_INIT_STORAGE);

    writeLn('merging all commands to AllCommands');
    AllCommands := TStringSet.Create;
    AllCommands.Append(HelpCommands);
    AllCommands.Append(NonHelpCommands);
    AllCommands.Append(UpdateCommands);

    writeLn('checking commands');

    if not AllCommands.Contains(Command) then begin
      writeLn('Unrecognized command ' + Command);
      writeLn('');
      ShowUsage;
    end
    else begin
      if HelpCommands.Contains(Command) then begin
        ShowUsage;
      end
      else begin
        //if not Options.ValidateOptions then begin
        //  Utils.ProgramExit(1);
        //end;

        if Command = CMD_UPLOAD_METADATA_DB then begin
          JukeboxOptions.SuppressMetadataDownload := true;
        end
        else begin
          JukeboxOptions.SuppressMetadataDownload := false;
        end;

        JukeboxOptions.Directory := Directory;

        StorageSystem := ConnectStorageSystem(StorageType,
                                              Creds,
                                              ContainerPrefix);

        if StorageSystem = nil then begin
          writeLn('error: unable to connect to storage system');
          Run := 1;
          exit;
        end;

        if not StorageSystem.Enter then begin
          writeLn('error: unable to enter storage system');
          Run := 1;
          exit;
        end;

        if Command = CMD_INIT_STORAGE then begin
          if InitStorageSystem(StorageSystem, ContainerPrefix) then begin
            ExitCode := 0;
          end
          else begin
            ExitCode := 1;
          end;
          Run := ExitCode;
          exit;
        end;

        jb := TJukebox.Create(JukeboxOptions,
                              StorageSystem,
                              ContainerPrefix,
                              DebugMode);
        if jb.Enter then begin
		  // the jukebox instance is now assigned to instance variable of JukeboxMain
		  // and is now owned by JukeboxMain
		  Jukebox := jb;
		  jb := nil;
          ExitCode := RunJukeboxCommand(Command);
        end
        else begin
		  jb.Free;
		  jb := nil;
          writeLn('error: unable to enter jukebox');
          ExitCode := 1;
        end;
      end;
    end;
  end
  else begin
    Args.Free;
	Args := nil;
    writeLn('Error: no command given');
    ShowUsage;
  end;
  
  if HelpCommands <> nil then begin
    HelpCommands.Free;
    HelpCommands := nil;
  end;

  if NonHelpCommands <> nil then begin
    NonHelpCommands.Free;
    NonHelpCommands := nil;
  end;
  
  if UpdateCommands <> nil then begin
    UpdateCommands.Free;
    UpdateCommands := nil;
  end;
  
  if AllCommands <> nil then begin
    AllCommands.Free;
    AllCommands := nil;
  end;

  Run := ExitCode;
end;

//*******************************************************************************

end.

