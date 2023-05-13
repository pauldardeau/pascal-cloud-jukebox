unit SongDownloaderThread;

{$mode objfpc}{$H+}{$J-}

interface

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  CRT, Classes, fgl, AbstractJukebox, SongDownloader, SongMetadata, SysUtils;

type
  TListSongMetadata = specialize TFPGObjectList<TSongMetadata>;

  TSongDownloaderThread = Class(TThread)
  private
    SongDownloader: TSongDownloader;
    Completed: Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(jb: TAbstractJukebox; aSongList: TListSongMetadata);
    destructor Destroy; override;
    function IsCompleted: Boolean;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TSongDownloaderThread.Create(jb: TAbstractJukebox;
                                         aSongList: TListSongMetadata);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  Completed := false;
  SongDownloader := TSongDownloader.Create(jb, aSongList);
end;

//*******************************************************************************

destructor TSongDownloaderThread.Destroy;
begin
  SongDownloader.Free;
  inherited;
end;

//*******************************************************************************

procedure TSongDownloaderThread.Execute;
begin
  SongDownloader.Run;
  Completed := true;
end;

//*******************************************************************************

function TSongDownloaderThread.IsCompleted: Boolean;
begin
  IsCompleted := Completed;
end;

//*******************************************************************************

end.

