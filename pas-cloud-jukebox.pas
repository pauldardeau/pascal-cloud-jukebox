program pascloudjukebox;

{$MODE OBJFPC}{$H+}{$J-}

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  Classes, JukeboxMain, SysUtils;

var
  jbMain: TJukeboxMain;
  args: TStringList;

begin
  args := TStringList.Create;
  jbMain := TJukeboxMain.Create;
  jbMain.Run(args);
end.
