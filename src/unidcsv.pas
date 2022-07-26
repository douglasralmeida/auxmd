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
    FRowsCount: Integer;
    FSpecialChars: TSysCharSet;
    function  GetCellValue(ARow: Integer; ACol: Integer): String;
    procedure SetCellValue(ARow: Integer; ACol: Integer; const AValue: String);
    procedure SetDelimiterChar(const AValue: Char);
  public
    constructor Create; virtual;
    constructor Create(ARows, ACols: Integer);
    procedure Assign(ASource: TPersistent); override;
    procedure SaveToFile(const AFilename: String);
    property DelimiterChar: Char read FDelimiterChar write SetDelimiterChar;
    property CellValue[ARow, ACol: Integer]: String read GetCellValue write SetCellValue;
  end;

implementation

constructor TCSVData.Create;
begin
  inherited Create;
  FColsCount := 0;
  FDelimiterChar := ',';
  FRowsCount := 0;
end;

constructor TCSVData.Create(ARows, ACols: Integer);
begin
  inherited Create;
  FRowsCount := ARows;
  FColsCount := ACols;

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

procedure TCSVData.SaveToFile(const AFilename: String);
var
  I, J: Integer;
  Length: Integer;
  FileStream: TFileStream;
begin
 FileStream := TFileStream.Create(AFilename, fmCreate);
 try
   for I := 0 to FRowsCount - 1 do
   begin
     for J := 0 to FColsCount - 1 do
     begin
       Length := FData[I, J].Length;
       if Length > 0 then
         FileStream.WriteBuffer(FData[I, J][1], Length);
       if J < FColsCount - 1 then
         FileStream.WriteBuffer(FDelimiterChar, SizeOf(Char));
     end;
     if I < FRowsCount - 1 then
       FileStream.WriteBuffer(#13#10, SizeOf(Char) * 2);
    end;
  finally
    FileStream.Free;
  end;
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

