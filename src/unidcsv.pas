unit unidcsv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { TData }
  TValuesData = Array of Array of String;

  { TCSVData }

  TCSVData = class(TPersistent)
  private
    FColsCount: Integer;
    FData: TValuesData;
    FDelimiterChar: Char;
    FHasHeader: Boolean;
    FMaxRowsPerFile: Integer;
    FRowsCount: Integer;
    FSeparatorInFileName: String;
    FSpecialChars: TSysCharSet;
    procedure DoSaveRow(Row: Integer; Stream: TFileStream);
    procedure DoSaveData(ARow: Integer; AMaxRows: Integer; const AFileName: String);
    function  GetCellValue(ARow: Integer; ACol: Integer): String;
    procedure SetCellValue(ARow: Integer; ACol: Integer; const AValue: String);
    procedure SetDelimiterChar(const AValue: Char);
  public
    constructor Create; virtual;
    constructor Create(ARows, ACols: Integer; AHasHeader: Boolean);
    procedure Assign(ASource: TPersistent); override;
    procedure SaveToFile(const AFilename: String);
    property DelimiterChar: Char read FDelimiterChar write SetDelimiterChar;
    property CellValue[ARow, ACol: Integer]: String read GetCellValue write SetCellValue;
    property HasHeader: Boolean read FHasHeader write FHasHeader;
    property MaxRowsPerFile: Integer read FMaxRowsPerFile write FMaxRowsPerFile;
    property RowsCount: Integer read FRowsCount write FRowsCount;
    property SeparatorInFileName: String read FSeparatorInFileName write FSeparatorInFileName;
  end;

implementation

uses LazFileUtils;

constructor TCSVData.Create;
begin
  inherited Create;
  FColsCount := 0;
  FDelimiterChar := ',';
  FHasHeader := False;
  FMaxRowsPerFile := 0;
  FSeparatorInFileName := '_';
  FRowsCount := 0;
end;

constructor TCSVData.Create(ARows, ACols: Integer; AHasHeader: Boolean);
begin
  inherited Create;
  FRowsCount := ARows;
  FColsCount := ACols;
  FHasHeader := AHasHeader;

  SetLength(FData, ARows, ACols);
end;

procedure TCSVData.Assign(ASource: TPersistent);
begin
  if (ASource is TCSVData) then
    FDelimiterChar := (ASource as TCSVData).DelimiterChar
  else
    inherited Assign(ASource);
end;

function TCSVData.GetCellValue(ARow: Integer; ACol: Integer): String;
begin
  if (ARow < FRowsCount) and (ACol < FColsCount) then
    Exit(FData[ARow][ACol])
  else
    raise Exception.Create('Linha ou coluna informados além da capacidade da matriz.');
end;

procedure TCSVData.DoSaveRow(Row: Integer; Stream: TFileStream);
var
  J, Length: Integer;
begin
 for J := 0 to FColsCount - 1 do
 begin
   Length := FData[Row, J].Length;
   //the first byte of a Unicode string is a UTF-8 byte
   if Length > 0 then
     Stream.WriteBuffer(FData[Row, J][1], Length);
   if J < FColsCount - 1 then
     Stream.WriteBuffer(FDelimiterChar, SizeOf(Char));
 end;
end;

procedure TCSVData.DoSaveData(ARow: Integer; AMaxRows: Integer; const AFileName: String);
var
  I: Integer;
  FileStream: TFileStream;
  AddedRow: Boolean;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate);
  AddedRow := False;
  try
    //includes header (line 0)
    if FHasHeader then
    begin
      DoSaveRow(0, FileStream);
      AddedRow := True;
    end;

    //includes data (line 1 until N-1)
    for I := 0 to AMaxRows - 1 do
    begin
      if ARow + I < FRowsCount then
      begin
        if AddedRow then
        begin
          FileStream.WriteBuffer(#13#10, SizeOf(Char) * 2);
          AddedRow := False;
        end;
        DoSaveRow(ARow + I, FileStream);
        AddedRow := True;
      end;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TCSVData.SaveToFile(const AFilename: String);
var
  I,
  CurrentRow,
  NumOfRows: Integer;
  TempFileName: String;
  TempFileExt: String;
  TempFile: String;
begin
  I := 1;
  TempFileExt := ExtractFileExt(AFileName);
  TempFileName := ExtractFileNameWithoutExt(AFileName);
  if FHasHeader then
    CurrentRow := 1
  else
    CurrentRow := 0;
  if FMaxRowsPerFile > 0 then
    NumOfRows := FMaxRowsPerFile
  else
  begin
    NumOfRows := FRowsCount;
    TempFile := AFileName;
  end;
  repeat
    if FMaxRowsPerFile > 0 then
    begin
      TempFile := TempFileName + FSeparatorInFileName + IntToStr(I) +  TempFileExt;
      Inc(I);
    end;
    DoSaveData(CurrentRow, NumOfRows, TempFile);
    Inc(CurrentRow, NumOfRows);
  until CurrentRow >= FRowsCount;
end;

procedure TCSVData.SetCellValue(ARow: Integer; ACol: Integer; const AValue: String);
begin
  FData[ARow][ACol] := AValue;
end;

procedure TCSVData.SetDelimiterChar(const AValue: Char);
begin
  if FDelimiterChar <> AValue then
    FDelimiterChar := AValue;
end;


end.
