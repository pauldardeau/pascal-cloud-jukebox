unit ArgumentParser;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, fgl, PropertySet, PropertyValue, SysUtils;

const
  DOUBLE_DASHES = '--';
  TYPE_BOOL_VALUE = 'bool';
  TYPE_INT_VALUE = 'int';
  TYPE_STRING_VALUE = 'string';

type
  TMapStringToString = specialize TFPGMap<String,String>;

  TArgumentParser = Class(TObject)
  private
    DictAllReservedWords: TMapStringToString;
    DictBoolOptions: TMapStringToString;
    DictIntOptions: TMapStringToString;
    DictStringOptions: TMapStringToString;
    DictCommands: TMapStringToString;
    ListCommands: TStringList;
    DebugMode: Boolean;

  public
    constructor Create(aDebugMode: Boolean);
    destructor Destroy; override;

    function AddOption(O: String; OptionType: String; Help: String): Boolean;
    procedure AddOptionalBoolFlag(Flag: String; Help: String);
    procedure AddOptionalIntArgument(Arg: String; Help: String);
    procedure AddOptionalStringArgument(Arg: String; Help: String);
    procedure AddRequiredArgument(Arg: String; Help: String);
    function ParseArgs(Args: TStringList): TPropertySet;
  end;

implementation

//*******************************************************************************

constructor TArgumentParser.Create(aDebugMode: Boolean);
begin
  inherited Create;
  DictAllReservedWords := TMapStringToString.Create;
  DictBoolOptions := TMapStringToString.Create;
  DictIntOptions := TMapStringToString.Create;
  DictStringOptions := TMapStringToString.Create;
  DictCommands := TMapStringToString.Create;
  ListCommands := TStringList.Create;
  DebugMode := aDebugMode;
end;

//*******************************************************************************

destructor TArgumentParser.Destroy;
begin
  writeLn('TArgumentParser.Destroy');
  DictAllReservedWords.Free;
  DictBoolOptions.Free;
  DictIntOptions.Free;
  DictStringOptions.Free;
  DictCommands.Free;
  ListCommands.Free;
  inherited;
end;

//*******************************************************************************

function TArgumentParser.AddOption(O: String;
                                   OptionType: String;
                                   Help: String): Boolean;
var
  OptionAdded: Boolean;
begin
  OptionAdded := true;

  if OptionType = TYPE_BOOL_VALUE then
    DictBoolOptions[O] := Help
  else if OptionType = TYPE_INT_VALUE then
    DictIntOptions[O] := Help
  else if OptionType = TYPE_STRING_VALUE then
    DictStringOptions[O] := Help
  else
    OptionAdded := false;

  if OptionAdded then
    DictAllReservedWords[O] := OptionType;

  AddOption := OptionAdded;
end;

//*******************************************************************************

procedure TArgumentParser.AddOptionalBoolFlag(Flag: String; Help: String);
begin
  AddOption(Flag, TYPE_BOOL_VALUE, Help);
end;

//*******************************************************************************

procedure TArgumentParser.AddOptionalIntArgument(Arg: String; Help: String);
begin
  AddOption(Arg, TYPE_INT_VALUE, Help);
end;

//*******************************************************************************

procedure TArgumentParser.AddOptionalStringArgument(Arg: String; Help: String);
begin
  AddOption(Arg, TYPE_STRING_VALUE, Help);
end;

//*******************************************************************************

procedure TArgumentParser.AddRequiredArgument(Arg: String; Help: String);
begin
  DictCommands[Arg] := Help;
  ListCommands.Add(Arg);
end;

//*******************************************************************************

function TArgumentParser.ParseArgs(Args: TStringList): TPropertySet;
var
  ps: TPropertySet;
  NumArgs: Integer;
  Working: Boolean;
  I: Integer;
  CommandsFound: Integer;
  Arg: String;
  ArgType: String;
  IntValue: Integer;
  NextArg: String;
  CommandName: String;
begin
  ps := TPropertySet.Create;
  NumArgs := Args.Count;
  Working := true;
  I := 0;
  CommandsFound := 0;

  if NumArgs = 0 then begin
    Working := false;
  end;

  while Working do
  begin
    Arg := Args[I];
    if DictAllReservedWords.IndexOf(Arg) <> -1 then begin
      ArgType := DictAllReservedWords[Arg];
      Arg := Arg.Substring(2);
      if ArgType = TYPE_BOOL_VALUE then begin
        if DebugMode then begin
          writeLn('ArgumentParser: adding key=' + Arg + ' value=true');
        end;
        ps.Add(Arg, TPropertyValue.Create(true));
      end
      else if ArgType = TYPE_INT_VALUE then begin
        inc(I);
        if I < NumArgs then begin
          NextArg := Args[I];
          try
            IntValue := StrToInt(NextArg);
            if DebugMode then begin
              writeLn('ArgumentParser: adding key=' + Arg + ' value=' + IntToStr(IntValue));
            end;
            ps.Add(Arg, TPropertyValue.Create(IntValue));
          except
          end;
        end
        else begin
          // missing int value
          writeLn('ArgumentParser: missing int value for key=' + Arg);
        end;
      end
      else if ArgType = TYPE_STRING_VALUE then begin
        inc(I);
        if I < NumArgs then begin
          NextArg := Args[I];
          if DebugMode then begin
            writeLn('ArgumentParser: adding key=' + Arg + ' value=' + NextArg);
          end;
          ps.Add(Arg, TPropertyValue.Create(NextArg));
        end
        else begin
          // missing string value
          writeLn('ArgumentParser: missing string value for key=' + Arg);
        end;
      end
      else begin
        // unrecognized type
        writeLn('ArgumentParser: unrecognized data type for key=' + Arg);
      end;
    end
    else begin
      if Arg.StartsWith(DOUBLE_DASHES) then begin
        // unrecognized option
        writeLn('ArgumentParser: unrecognized option ' + Arg);
      end
      else begin
        if CommandsFound < ListCommands.Count then begin
          CommandName := ListCommands[CommandsFound];
          if DebugMode then begin
            writeLn('ArgumentParser: adding key=' + CommandName + ' value=' + Arg);
          end;
          ps.Add(CommandName, TPropertyValue.Create(Arg));
          inc(CommandsFound);
        end
        else begin
          // unrecognized command
          writeLn('ArgumentParser: unrecognized command ' + Arg);
        end;
      end;
    end;

    inc(I);
    if I >= NumArgs then begin
      Working := false;
    end;
  end;

  ParseArgs := ps;
end;

//*******************************************************************************

end.
