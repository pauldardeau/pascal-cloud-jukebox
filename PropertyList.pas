unit PropertyList;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, PropertyValue, SysUtils;

type
  TListPropertyValue = specialize TFPGObjectList<TPropertyValue>;

  TPropertyList = Class(TObject)
  private
    ListProps: TListPropertyValue;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Append(PropValue: TPropertyValue);
    procedure Clear;

    function Get(aIndex: Integer): TPropertyValue;
    function GetIntValue(aIndex: Integer): Integer;
    function GetLongValue(aIndex: Integer): Int64;
    function GetULongValue(aIndex: Integer): UInt64;
    function GetBoolValue(aIndex: Integer): Boolean;
    function GetStringValue(aIndex: Integer): String;
    function GetDoubleValue(aIndex: Integer): Double;
    function Count: Integer;
  end;

implementation

//*******************************************************************************

constructor TPropertyList.Create;
begin
  inherited;
  ListProps := TListPropertyValue.Create;
end;

//*******************************************************************************

destructor TPropertyList.Destroy;
begin
  writeLn('TPropertyList.Destroy');
  ListProps.Free;
  inherited;
end;

//*******************************************************************************

procedure TPropertyList.Append(PropValue: TPropertyValue);
begin
  ListProps.Add(PropValue);
end;

//*******************************************************************************

procedure TPropertyList.Clear;
begin
  ListProps.Clear;
end;

//*******************************************************************************

function TPropertyList.Get(aIndex: Integer): TPropertyValue;
begin
  Get := ListProps[aIndex];
end;

//*******************************************************************************

function TPropertyList.GetIntValue(aIndex: Integer): Integer;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetIntValue := Pv.GetIntValue
  else
    GetIntValue := 0;
end;

//*******************************************************************************

function TPropertyList.GetLongValue(aIndex: Integer): Int64;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetLongValue := Pv.GetLongValue
  else
    GetLongValue := 0;
end;

//*******************************************************************************

function TPropertyList.GetULongValue(aIndex: Integer): UInt64;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetULongValue := Pv.GetULongValue
  else
    GetULongValue := 0;
end;

//*******************************************************************************

function TPropertyList.GetBoolValue(aIndex: Integer): Boolean;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetBoolValue := Pv.GetBoolValue
  else
    GetBoolValue := false;
end;

//*******************************************************************************

function TPropertyList.GetStringValue(aIndex: Integer): String;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetStringValue := Pv.GetStringValue
  else
    GetStringValue := '';
end;

//*******************************************************************************

function TPropertyList.GetDoubleValue(aIndex: Integer): Double;
var
  Pv: TPropertyValue;
begin
  Pv := Get(aIndex);
  if Pv <> nil then
    GetDoubleValue := Pv.GetDoubleValue
  else
    GetDoubleValue := 0.0;
end;

//*******************************************************************************

function TPropertyList.Count: Integer;
begin
  Count := ListProps.Count;
end;

//*******************************************************************************

end.

