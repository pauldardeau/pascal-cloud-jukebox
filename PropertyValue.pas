unit PropertyValue;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, SysUtils;

type
  TPropertyValue = Class(TObject)
  private
    DataType: String;
    IntValue: Integer;
    LongValue: Int64;
    ULongValue: UInt64;
    BoolValue: Boolean;
    StringValue: String;
    DoubleValue: Double;

  public
    constructor Create;
    constructor Create(aIntValue: Integer);
    constructor Create(aLongValue: Int64);
    constructor Create(aULongValue: UInt64);
    constructor Create(aBoolValue: Boolean);
    constructor Create(aStringValue: String);
    constructor Create(aDoubleValue: Double);
    destructor Destroy; override;

    function IsInt: Boolean;
    function IsLong: Boolean;
    function IsULong: Boolean;
    function IsBool: Boolean;
    function IsString: Boolean;
    function IsDouble: Boolean;
    function IsNull: Boolean;

    function GetIntValue: Integer;
    function GetLongValue: Int64;
    function GetULongValue: UInt64;
    function GetBoolValue: Boolean;
    function GetStringValue: String;
    function GetDoubleValue: Double;
  end;

const
  TYPE_INT = 'Int';
  TYPE_LONG = 'Long';
  TYPE_ULONG = 'ULong';
  TYPE_BOOL = 'Bool';
  TYPE_STRING = 'String';
  TYPE_DOUBLE = 'Double';
  TYPE_NULL = 'Null';

implementation

//*******************************************************************************

constructor TPropertyValue.Create;
begin
  inherited;
  DataType := TYPE_NULL;
  IntValue := 0;
  LongValue := 0;
  ULongValue := 0;
  BoolValue := false;
  StringValue := '';
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aIntValue: Integer);
begin
  inherited Create;
  DataType := TYPE_INT;
  IntValue := aIntValue;
  LongValue := 0;
  ULongValue := 0;
  BoolValue := false;
  StringValue := '';
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aLongValue: Int64);
begin
  inherited Create;
  DataType := TYPE_LONG;
  IntValue := 0;
  LongValue := aLongValue;
  ULongValue := 0;
  BoolValue := false;
  StringValue := '';
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aULongValue: UInt64);
begin
  inherited Create;
  DataType := TYPE_ULONG;
  IntValue := 0;
  LongValue := 0;
  ULongValue := aULongValue;
  BoolValue := false;
  StringValue := '';
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aBoolValue: Boolean);
begin
  inherited Create;
  DataType := TYPE_BOOL;
  IntValue := 0;
  LongValue := 0;
  ULongValue := 0;
  BoolValue := aBoolValue;
  StringValue := '';
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aStringValue: String);
begin
  inherited Create;
  DataType := TYPE_STRING;
  IntValue := 0;
  LongValue := 0;
  ULongValue := 0;
  BoolValue := false;
  StringValue := aStringValue;
  DoubleValue := 0.0;
end;

//*******************************************************************************

constructor TPropertyValue.Create(aDoubleValue: Double);
begin
  inherited Create;
  DataType := TYPE_DOUBLE;
  IntValue := 0;
  LongValue := 0;
  ULongValue := 0;
  BoolValue := false;
  StringValue := '';
  DoubleValue := aDoubleValue;
end;

//*******************************************************************************

destructor TPropertyValue.Destroy;
begin
  writeLn('TPropertyValue.Destroy');
  inherited;
end;

//*******************************************************************************

function TPropertyValue.IsInt: Boolean;
begin
  IsInt := DataType = TYPE_INT;
end;

//*******************************************************************************

function TPropertyValue.IsLong: Boolean;
begin
  IsLong := DataType = TYPE_LONG;
end;

//*******************************************************************************

function TPropertyValue.IsULong: Boolean;
begin
  IsULong := DataType = TYPE_ULONG;
end;

//*******************************************************************************

function TPropertyValue.IsBool: Boolean;
begin
  IsBool := DataType = TYPE_BOOL;
end;

//*******************************************************************************

function TPropertyValue.IsString: Boolean;
begin
  IsString := DataType = TYPE_STRING;
end;

//*******************************************************************************

function TPropertyValue.IsDouble: Boolean;
begin
  IsDouble := DataType = TYPE_DOUBLE;
end;

//*******************************************************************************

function TPropertyValue.IsNull: Boolean;
begin
  IsNull := DataType = TYPE_NULL;
end;

//*******************************************************************************

function TPropertyValue.GetIntValue: Integer;
begin
  GetIntValue := IntValue;
end;

//*******************************************************************************

function TPropertyValue.GetLongValue: Int64;
begin
  GetLongValue := LongValue;
end;

//*******************************************************************************

function TPropertyValue.GetULongValue: UInt64;
begin
  GetULongValue := ULongValue;
end;

//*******************************************************************************

function TPropertyValue.GetBoolValue: Boolean;
begin
  GetBoolValue := BoolValue;
end;

//*******************************************************************************

function TPropertyValue.GetStringValue: String;
begin
  GetStringValue := StringValue;
end;

//*******************************************************************************

function TPropertyValue.GetDoubleValue: Double;
begin
  GetDoubleValue := DoubleValue;
end;

//*******************************************************************************

end.

