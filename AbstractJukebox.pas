unit AbstractJukebox;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, SongMetadata, SysUtils;

type
  TAbstractJukebox = Class(TObject)
  public
    function IsExitRequested: Boolean; virtual; abstract;
    procedure BatchDownloadStart; virtual; abstract;
    procedure BatchDownloadComplete; virtual; abstract;
    function DownloadSong(Song: TSongMetadata): Boolean; virtual; abstract;
  end;

//*******************************************************************************
//*******************************************************************************

implementation

end.

