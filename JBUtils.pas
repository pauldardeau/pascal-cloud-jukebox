unit JBUtils;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, StrUtils, SysUtils, Types;

const
  DOUBLE_DASHES = '--';

function DecodeValue(EncodedValue: String): String;
function EncodeValue(Value: String): String;
function EncodeArtistAlbum(Artist: String; Album: String): String;
function EncodeArtistAlbumSong(Artist: String;
                               Album: String;
                               Song: String): String;
function ArtistFromFileName(FileName: String): String;
function AlbumFromFileName(FileName: String): String;
function SongFromFileName(FileName: String): String;


implementation

//*******************************************************************************

function RemovePunctuation(S: String): String;
var
  Flags: TReplaceFlags;
begin
  Flags := [rfReplaceAll];

  if StrUtils.ContainsStr(S, '''') then begin
    S := StrUtils.StringReplace(S, '''', '', Flags);
  end;

  if StrUtils.ContainsStr(S, '!') then begin
    S := StrUtils.StringReplace(S, '!', '', Flags);
  end;

  if StrUtils.ContainsStr(S, '?') then begin
    S := StrUtils.StringReplace(S, '?', '', Flags);
  end;

  if StrUtils.ContainsStr(S, ' & ') then begin
    S := StrUtils.StringReplace(S, '& ', '', Flags);
  end;

  if StrUtils.ContainsStr(S, ',') then begin
    S := StrUtils.StringReplace(S, ',', '', Flags);
  end;

  RemovePunctuation := S;
end;

//*******************************************************************************

function DecodeValue(EncodedValue: String): String;
var
  Flags: TReplaceFlags;
begin
  Flags := [rfReplaceAll];
  DecodeValue := StrUtils.StringReplace(EncodedValue, '-', '', Flags);
end;

//*******************************************************************************

function EncodeValue(Value: String): String;
var
  ValueWithoutPunctuation: String;
  Flags: TReplaceFlags;
begin
  ValueWithoutPunctuation := RemovePunctuation(Value);
  Flags := [rfReplaceAll];
  EncodeValue := StrUtils.StringReplace(ValueWithoutPunctuation,
                                        ' ',
                                        '-',
                                        Flags);
end;

//*******************************************************************************

function EncodeArtistAlbum(Artist: String; Album: String): String;
begin
  EncodeArtistAlbum := EncodeValue(Artist) + DOUBLE_DASHES + EncodeValue(Album);
end;

//*******************************************************************************

function EncodeArtistAlbumSong(Artist: String;
                               Album: String;
                               Song: String): String;
begin
  EncodeArtistAlbumSong := EncodeArtistAlbum(Artist, Album) +
                           DOUBLE_DASHES +
                           EncodeValue(Song);
end;

//*******************************************************************************

function ComponentsFromFileName(FileName: String): TStringDynArray;
begin
  ComponentsFromFileName := StrUtils.SplitString(FileName, DOUBLE_DASHES);
end;

//*******************************************************************************

function ArtistFromFileName(FileName: String): String;
var
  Components: TStringDynArray;
begin
  Components := ComponentsFromFileName(FileName);
  if Length(Components) = 3 then begin
    ArtistFromFileName := Components[0];
  end
  else begin
    ArtistFromFileName := '';
  end;
end;

//*******************************************************************************

function AlbumFromFileName(FileName: String): String;
var
  Components: TStringDynArray;
begin
  Components := ComponentsFromFileName(FileName);
  if Length(Components) = 3 then begin
    AlbumFromFileName := Components[1];
  end
  else begin
    AlbumFromFileName := '';
  end;
end;

//*******************************************************************************

function SongFromFileName(FileName: String): String;
var
  Components: TStringDynArray;
begin
  Components := ComponentsFromFileName(FileName);
  if Length(Components) = 3 then begin
    SongFromFileName := Components[2];
  end
  else begin
    SongFromFileName := '';
  end;
end;

//*******************************************************************************

end.

