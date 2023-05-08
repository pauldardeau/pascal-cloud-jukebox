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
  i: Integer;
begin
  args := TStringList.Create;
  for i := 1 to ParamCount do begin
    args.Add(ParamStr(i));
  end;

  jbMain := TJukeboxMain.Create;
  jbMain.Run(args);
  jbMain.Free;
end.
