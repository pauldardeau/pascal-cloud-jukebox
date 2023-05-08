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

  protected
    procedure Execute; override;

  public
    constructor Create(jb: TAbstractJukebox; aSongList: TListSongMetadata);
    destructor Destroy; override;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

//*******************************************************************************

constructor TSongDownloaderThread.Create(jb: TAbstractJukebox;
                                         aSongList: TListSongMetadata);
begin
  inherited Create(true);
  SongDownloader := TSongDownloader.Create(jb, aSongList);
end;

//*******************************************************************************

destructor TSongDownloaderThread.Destroy;
begin
  inherited;
end;

//*******************************************************************************

procedure TSongDownloaderThread.Execute;
begin
  SongDownloader.Run;
end;

//*******************************************************************************

end.
