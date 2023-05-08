unit KeyValuePairs;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, SysUtils;

type
  TMapStringToString = specialize TFPGMap<String,String>;

  TKeyValuePairs = Class(TObject)
  private
    DictKeyValues: TMapStringToString;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPair(Key: String; Value: String);
    procedure Clear;

    function ContainsKey(Key: String): Boolean;
    function GetValue(Key: String): String;
    function Count: Integer;
    function GetKeys: TStringList;
  end;

implementation

//*******************************************************************************

constructor TKeyValuePairs.Create;
begin
  inherited;
  DictKeyValues := TMapStringToString.Create;
end;

//*******************************************************************************

destructor TKeyValuePairs.Destroy;
begin
  writeLn('TKeyValuePairs.Destroy');
  DictKeyValues.Free;
  inherited;
end;

//*******************************************************************************

procedure TKeyValuePairs.AddPair(Key: String; Value: String);
begin
  DictKeyValues.Add(Key, Value);
end;

//*******************************************************************************

procedure TKeyValuePairs.Clear;
begin
  DictKeyValues.Clear;
end;

//*******************************************************************************

function TKeyValuePairs.ContainsKey(Key: String): Boolean;
begin
  ContainsKey := DictKeyValues.IndexOf(Key) > -1;
end;

//*******************************************************************************

function TKeyValuePairs.GetValue(Key: String): String;
var
  Index: Integer;
begin
  Index := DictKeyValues.IndexOf(Key);
  if Index > -1 then
   GetValue := DictKeyValues.Data[Index]
  else
    GetValue := '';
end;

//*******************************************************************************

function TKeyValuePairs.Count: Integer;
begin
  Count := DictKeyValues.Count;
end;

//*******************************************************************************

function TKeyValuePairs.GetKeys: TStringList;
var
  ListKeys: TStringList;
  i: Integer;
  Key: String;
begin
  ListKeys := TStringList.Create;
  for i := 0 to DictKeyValues.Count-1 do begin
    Key := DictKeyValues.Keys[i];
    ListKeys.Append(Key);
  end;
  GetKeys := ListKeys;
end;

//*******************************************************************************

end.

