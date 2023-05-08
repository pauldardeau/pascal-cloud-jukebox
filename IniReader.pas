unit IniReader;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CRT, Classes, JBSysUtils, KeyValuePairs, SysUtils;

const
  EOL_LF = #10;
  EOL_CR = #13;
  OPEN_BRACKET = '[';
  CLOSE_BRACKET = ']';
  COMMENT_IDENTIFIER = '#';

type
  TIniReader = Class(TObject)
  public
    IniFile: String;
    FileContents: String;

  public
    constructor Create(aIniFile: String);
    constructor Create(aIniFile: String; aFileContents: String); // for testing
    destructor Destroy; override;

    function ReadSection(Section: String;
                         var SectionValues: TKeyValuePairs): Boolean;
    function GetSectionKeyValue(Section: String;
                                Key: String;
                                out Value: String): Boolean;
    function HasSection(aSection: String): Boolean;
    function ReadFile: Boolean;
    function BracketedSection(aSectionName: String): String;
  end;

implementation

//*******************************************************************************

constructor TIniReader.Create(aIniFile: String);
begin
  inherited Create;
  IniFile := aIniFile;
  FileContents := '';
end;

//*******************************************************************************

constructor TIniReader.Create(aIniFile: String; aFileContents: String);
begin
  inherited Create;
  IniFile := aIniFile;
  FileContents := aFileContents;
end;

//*******************************************************************************

destructor TIniReader.Destroy;
begin
  writeLn('TIniReader.Destroy');
  inherited;
end;

//*******************************************************************************

function TIniReader.HasSection(aSection: String): Boolean;
var
  SectionId: String;
begin
  SectionId := BracketedSection(aSection);
  HasSection := FileContents.IndexOf(SectionId) <> -1;
end;

//*******************************************************************************

function TIniReader.ReadFile: Boolean;
var
  StrippingComments: Boolean;
  PosCurrent: Integer;
  PosCommentStart: Integer;
  PosCR: Integer;
  PosLF: Integer;
  PosEOL: Integer;
  HaveCR: Boolean;
  HaveLF: Boolean;
  BeforeComment: String;
  AfterComment: String;
begin
  FileContents := JBFileReadAllText(IniFile);
  if Length(FileContents) = 0 then begin
    ReadFile := false;
    exit;
  end;

  // strip out any comments
  StrippingComments := true;
  PosCurrent := 0;

  while StrippingComments do begin
    PosCommentStart := FileContents.IndexOf(COMMENT_IDENTIFIER, PosCurrent);
    if (-1 = PosCommentStart) then begin
      // not found
      StrippingComments := false;
    end
    else begin
      PosCR := FileContents.IndexOf(EOL_CR, PosCommentStart + 1);
      PosLF := FileContents.IndexOf(EOL_LF, PosCommentStart + 1);
      HaveCR := (-1 <> PosCR);
      HaveLF := (-1 <> PosLF);

      if (not HaveCR) and (not HaveLF) then begin
        // no end-of-line marker remaining
        // erase from start of comment to end of file
        FileContents := FileContents.Substring(0, PosCommentStart);
        StrippingComments := false;
      end
      else begin
        // at least one end-of-line marker was found

        // were both types found
        if HaveCR and HaveLF then begin
          PosEOL := PosCR;

          if PosLF < PosEOL then begin
            PosEOL := PosLF;
          end;
        end
        else begin
          if HaveCR then begin
            // CR found
            PosEOL := PosCR;
          end
          else begin
            // LF found
            PosEOL := PosLF;
          end;
        end;

        BeforeComment := FileContents.Substring(0, PosCommentStart);
        AfterComment := FileContents.Substring(PosEOL);
        FileContents := BeforeComment + AfterComment;
        PosCurrent := BeforeComment.Length;
      end;
    end;
  end;

  ReadFile := true;
end;

//*******************************************************************************

function TIniReader.BracketedSection(aSectionName: String): String;
begin
  BracketedSection := OPEN_BRACKET + aSectionName + CLOSE_BRACKET;
end;

//*******************************************************************************

function TIniReader.ReadSection(Section: String;
                                var SectionValues: TKeyValuePairs): Boolean;
var
  SectionId: String;
  PosSection: Integer;
  PosEndSection: Integer;
  StartNextSection: Integer;
  SectionContents: String;
  SectionLines: TStringArray;
  PairsAdded: Integer;
  Success: Boolean;
  Key: String;
  Value: String;
  TrimmedLine: String;
  LineFields: TStringArray;
  SectionLine: String;
  i: Integer;
begin
  SectionId := BracketedSection(Section);
  PosSection := FileContents.IndexOf(SectionId);

  if PosSection = -1 then begin
    ReadSection := false;
    exit;
  end;

  PosEndSection := PosSection + SectionId.Length;
  StartNextSection :=
      FileContents.IndexOf(OPEN_BRACKET, PosEndSection);

  // do we have another section?
  if StartNextSection <> -1 then begin
    // yes, we have another section in the file -- read everything
    // up to the next section
    SectionContents := FileContents.Substring(PosEndSection,
                                              StartNextSection - PosEndSection);
  end
  else begin
    // no, this is the last section -- read everything left in
    // the file
    SectionContents := FileContents.Substring(PosEndSection);
  end;

  SectionContents := SectionContents.Trim;

  if SectionContents.Length = 0 then begin
    writeLn('section in .ini file is empty');
    ReadSection := false;
    exit;
  end;

  SectionLines := SectionContents.Split(LineEnding);
  PairsAdded := 0;

  for i := 0 to Length(SectionLines)-1 do begin
    SectionLine := SectionLines[i];
    TrimmedLine := SectionLine.Trim;
    if TrimmedLine.Length > 0 then begin
      if TrimmedLine.Contains('=') then begin
        LineFields := TrimmedLine.Split('=');
        if Length(LineFields) = 2 then begin
          Key := LineFields[0].Trim;
          Value := LineFields[1].Trim;
          if (Key.Length > 0) and (Value.Length > 0) then begin
            SectionValues.AddPair(Key, Value);
            inc(PairsAdded);
          end;
        end;
      end;
    end;
  end;

  if PairsAdded > 0 then begin
    Success := true;
  end
  else begin
    Success := false;
  end;

  ReadSection := Success;
end;

//*******************************************************************************

function TIniReader.GetSectionKeyValue(Section: String;
                                       Key: String;
                                       out Value: String): Boolean;
var
  Map: TKeyValuePairs;
  StrippedKey: String;
  Success: Boolean;
begin
  Map := TKeyValuePairs.Create;

  if not ReadSection(Section, Map) then begin
    writeLn('IniReader ReadSection returned false');
    Map.Free;
    GetSectionKeyValue := false;
    exit;
  end;

  StrippedKey := Key.Trim;

  if Map.ContainsKey(StrippedKey) then begin
    Value := Map.GetValue(StrippedKey);
    Success := true;
  end
  else begin
    writeLn('map does not contain key ' + StrippedKey);
    Success := false;
  end;

  Map.Free;

  GetSectionKeyValue := Success;
end;

//*******************************************************************************

end.

