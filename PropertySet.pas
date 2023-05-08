unit PropertySet;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, JBSysUtils, PropertyValue, SysUtils;

const
  VALUE_TRUE = 'true';
  VALUE_FALSE = 'false';

  TYPE_BOOL = 'bool';
  TYPE_STRING = 'string';
  TYPE_INT = 'int';
  TYPE_LONG = 'long';
  TYPE_ULONG = 'ulong';
  TYPE_DOUBLE = 'double';
  TYPE_NULL = 'null';

type
  TMapStringToPropertyValue = specialize TFPGMap<String,TPropertyValue>;

  TPropertySet = Class(TObject)
  private
    MapProps: TMapStringToPropertyValue;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(PropName: String; PropValue: TPropertyValue);
    procedure Clear;

    function Contains(PropName: String): Boolean;
    function Get(PropName: String): TPropertyValue;
    function GetIntValue(PropName: String): Integer;
    function GetLongValue(PropName: String): Int64;
    function GetULongValue(PropName: String): UInt64;
    function GetBoolValue(PropName: String): Boolean;
    function GetStringValue(PropName: String): String;
    function GetDoubleValue(PropName: String): Double;
    function Count: Integer;
    function ToString: String; override;
    function PopulateFromString(EncodedPropertySet: String): Boolean;
    function WriteToFile(FileName: String): Boolean;
    function ReadFromFile(FileName: String): Boolean;
  end;

implementation

//*******************************************************************************

constructor TPropertySet.Create;
begin
  inherited;
  MapProps := TMapStringToPropertyValue.Create;
end;

//*******************************************************************************

destructor TPropertySet.Destroy;
begin
  writeLn('TPropertySet.Destroy');
  MapProps.Free;
  inherited;
end;

//*******************************************************************************

procedure TPropertySet.Add(PropName: String; PropValue: TPropertyValue);
begin
  MapProps.Add(PropName, PropValue);
end;

//*******************************************************************************

procedure TPropertySet.Clear;
begin
  MapProps.Clear;
end;

//*******************************************************************************

function TPropertySet.Contains(PropName: String): Boolean;
begin
  Contains := MapProps.IndexOf(PropName) > -1;
end;

//*******************************************************************************

function TPropertySet.Get(PropName: String): TPropertyValue;
var
  Index: Integer;
begin
  Index := MapProps.IndexOf(PropName);
  if Index > -1 then
    Get := MapProps.Data[Index]
  else
    Get := nil;
end;

//*******************************************************************************

function TPropertySet.GetIntValue(PropName: String): Integer;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetIntValue := Pv.GetIntValue
  else
    GetIntValue := 0;
end;

//*******************************************************************************

function TPropertySet.GetLongValue(PropName: String): Int64;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetLongValue := Pv.GetLongValue
  else
    GetLongValue := 0;
end;

//*******************************************************************************

function TPropertySet.GetULongValue(PropName: String): UInt64;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetULongValue := Pv.GetULongValue
  else
    GetULongValue := 0;
end;

//*******************************************************************************

function TPropertySet.GetBoolValue(PropName: String): Boolean;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetBoolValue := Pv.GetBoolValue
  else
    GetBoolValue := false;
end;

//*******************************************************************************

function TPropertySet.GetStringValue(PropName: String): String;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetStringValue := Pv.GetStringValue
  else
    GetStringValue := '';
end;

//*******************************************************************************

function TPropertySet.GetDoubleValue(PropName: String): Double;
var
  Pv: TPropertyValue;
begin
  Pv := Get(PropName);
  if Pv <> nil then
    GetDoubleValue := Pv.GetDoubleValue
  else
    GetDoubleValue := 0.0;
end;

//*******************************************************************************

function TPropertySet.Count: Integer;
begin
  Count := MapProps.Count;
end;

//*******************************************************************************

function TPropertySet.ToString: String;
var
  Encoded: TStringBuilder;
  PV: TPropertyValue;
  BoolValue: String;
  PropName: String;
  i: Integer;
  PropType: String;
  PropValue: String;
begin
  Encoded := TStringBuilder.Create;
  
  try
    for i := 0 to MapProps.Count-1 do begin
      PropName := MapProps.Keys[i];
      PV := Get(PropName);

      if PV.IsBool then begin
        BoolValue := '';
        if PV.GetBoolValue then begin
          BoolValue := VALUE_TRUE;
        end
        else begin
          BoolValue := VALUE_FALSE;
        end;
        PropType := TYPE_BOOL;
        PropValue := BoolValue;
      end
      else if PV.IsString then begin
        PropType := TYPE_STRING;
        PropValue := PV.GetStringValue;
      end
      else if PV.IsInt then begin
        PropType := TYPE_INT;
        PropValue := IntToStr(PV.GetIntValue);
      end
      else if PV.IsLong then begin
        PropType := TYPE_LONG;
        PropValue := IntToStr(PV.GetLongValue);
      end
      else if PV.IsULong then begin
        PropType := TYPE_ULONG;
        PropValue := IntToStr(PV.GetULongValue);
      end
      else if PV.IsDouble then begin
        PropType := TYPE_DOUBLE;
        PropValue := FloatToStr(PV.GetDoubleValue);
      end
      else if PV.IsNull then begin
        PropType := TYPE_NULL;
        PropValue := '';
      end;
      Encoded.Append(PropType);
      Encoded.Append('|');
      Encoded.Append(PropName);
      Encoded.Append('|');
      Encoded.Append(PropValue);
      Encoded.Append(LineEnding);
    end;

    ToString := Encoded.ToString;
  finally
    Encoded.Free;
  end;
end;

//*******************************************************************************

function TPropertySet.PopulateFromString(EncodedPropertySet: String): Boolean;
var
  Success: Boolean;
  FileLines: TStringArray;
  FileLine: String;
  StrippedFileLine: String;
  Fields: TStringArray;
  DataType: String;
  PropName: String;
  PropValue: String;
  IntValue: Integer;
  LongValue: Int64;
  ULongValue: UInt64;
  DoubleValue: Double;
  i: Integer;
begin
  Success := false;

  if EncodedPropertySet.Length > 0 then begin
    FileLines := EncodedPropertySet.Split(LineEnding);
    for i := 0 to Length(FileLines)-1 do begin
      FileLine := FileLines[i];
      StrippedFileLine := FileLine.Trim;
      if StrippedFileLine.Length > 0 then begin
        Fields := StrippedFileLine.Split('|');
        if Length(Fields) = 3 then begin
          DataType := Fields[0].Trim;
          PropName := Fields[1].Trim;
          PropValue := Fields[2].Trim;
          if (DataType.Length > 0) and (PropName.Length > 0) then begin
            if DataType = TYPE_NULL then begin
              Add(PropName, TPropertyValue.Create);
            end
            else begin
              if PropValue.Length > 0 then begin
                if DataType = TYPE_BOOL then begin
                  if PropValue = VALUE_TRUE then begin
                    Add(PropName, TPropertyValue.Create(true));
                  end
                  else if PropValue = VALUE_FALSE then begin
                    Add(PropName, TPropertyValue.Create(false));
                  end
                  else begin
                    writeLn('error: unrecognized value ' +
                            PropValue +
                            ' for ' +
                            TYPE_BOOL +
                            ' property ' +
                            PropName);
                  end;
                end
                else if DataType = TYPE_STRING then begin
                  Add(PropName, TPropertyValue.Create(PropValue));
                end
                else if DataType = TYPE_INT then begin
                  try
                    IntValue := StrToInt(PropValue);
                    Add(PropName, TPropertyValue.Create(IntValue));
                  except
                    On E: Exception do begin
                      writeLn('error: unrecognized value ' +
                              PropValue +
                              ' for ' +
                              TYPE_INT +
                              ' property ' +
                              PropName);
                    end;
                  end;
                end
                else if DataType = TYPE_LONG then begin
                  try
                    LongValue := StrToInt(PropValue);
                    Add(PropName, TPropertyValue.Create(LongValue));
                  except
                    On E: Exception do begin
                      writeLn('error: unrecognized value ' +
                              PropValue +
                              ' for ' +
                              TYPE_LONG +
                              ' property ' +
                              PropName);
                    end;
                  end;
                end
                else if DataType = TYPE_ULONG then begin
                  try
                    ULongValue := StrToInt(PropValue);
                    Add(PropName, TPropertyValue.Create(ULongValue));
                  except
                    On E: Exception do begin
                      writeLn('error: unrecognized value ' +
                              PropValue +
                              ' for ' +
                              TYPE_ULONG +
                              ' property ' +
                              PropName);
                    end;
                  end;
                end
                else if DataType = TYPE_DOUBLE then begin
                  try
                    DoubleValue := StrToFloat(PropValue);
                    Add(PropName, TPropertyValue.Create(DoubleValue));
                  except
                    On E: Exception do begin
                      writeLn('error: unrecognized value ' +
                              PropValue +
                              ' for ' +
                              TYPE_DOUBLE +
                              ' property ' +
                              PropName);
                    end;
                  end;
                end
                else begin
                  writeLn('error: unrecognized data type ' +
                          DataType +
                          ' for property ' +
                          PropName);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  PopulateFromString := Success;
end;

//*******************************************************************************

function TPropertySet.WriteToFile(FileName: String): Boolean;
begin
  WriteToFile := JBFileWriteAllText(FileName, ToString);
end;

//*******************************************************************************

function TPropertySet.ReadFromFile(FileName: String): Boolean;
begin
  ReadFromFile := PopulateFromString(JBFileReadAllText(FileName)); 
end;

//*******************************************************************************

end.

