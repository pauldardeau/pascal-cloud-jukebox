unit StringSet;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, SysUtils;

type
  TMapStringToBoolean = specialize TFPGMap<String,Boolean>;

  TStringSet = Class(TObject)
  private
    MapStrings: TMapStringToBoolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value: String);
    procedure Clear;
    procedure Append(aSet: TStringSet);

    function Contains(Value: String): Boolean;
    function Count: Integer;
    function ToString: String; override;
  end;

implementation

//*******************************************************************************

constructor TStringSet.Create;
begin
  inherited;
  MapStrings := TMapStringToBoolean.Create;
end;

//*******************************************************************************

destructor TStringSet.Destroy;
begin
  writeLn('TStringSet.Destroy');
  MapStrings.Free;
  inherited;
end;

//*******************************************************************************

procedure TStringSet.Add(Value: String);
begin
  MapStrings.Add(Value, true);
end;

//*******************************************************************************

procedure TStringSet.Clear;
begin
  MapStrings.Clear;
end;

//*******************************************************************************

procedure TStringSet.Append(aSet: TStringSet);
var
  i: Integer;
begin
  for i := 0 to aSet.MapStrings.Count-1 do begin
    MapStrings.Add(aSet.MapStrings.Keys[i], true);
  end;
end;

//*******************************************************************************

function TStringSet.Contains(Value: String): Boolean;
begin
  Contains := MapStrings.IndexOf(Value) > -1;
end;

//*******************************************************************************

function TStringSet.Count: Integer;
begin
  Count := MapStrings.Count;
end;

//*******************************************************************************

function TStringSet.ToString: String;
var
  sb: TStringBuilder;
  i: Integer;
  Value: String;
begin
  sb := TStringBuilder.Create;
  for i := 0 to MapStrings.Count-1 do begin
    Value := MapStrings.Keys[i];
    if sb.Length > 0 then begin
      sb.Append(', ');
    end;
    sb.Append(Value);
  end;
  ToString := sb.ToString;
  sb.Free;
end;

//*******************************************************************************

end.

