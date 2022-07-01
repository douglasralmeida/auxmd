unit unidPadrao;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, SynEdit, Generics.Collections, SynEditTypes;

type
  { TModoGeracao }
  TModoGeracao = (mgCadastrar, mgIniciarApuracao);

  { TJanelaPadrao }
  TJanelaPadrao = class(TForm)
    ListaAcoes: TActionList;
    BtoColar: TButton;
    BtoAbrir: TButton;
    BtoSalvar: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    DialogoAbrir: TOpenDialog;
    RadioCadastrar: TRadioButton;
    RadioIniciar: TRadioButton;
    DialogoSalvar: TSaveDialog;
    Editor: TSynEdit;
    DialogoMsg: TTaskDialog;
    procedure BtoAbrirClick(Sender: TObject);
    procedure BtoColarClick(Sender: TObject);
    procedure BtoSalvarClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
  private
    ListaErros: TList<Integer>;
    ModoGeracao: TModoGeracao;
    procedure AnalisarTexto;
    procedure CarregarTexto(NomeArquivo: String);
    function ChecarTexto: Boolean;
    procedure ExibirErro(Msg: String; Detalhes: String);
    procedure ProcessarTexto;
    procedure SalvarComoCSV(NomeArquivo: String);
    procedure SalvarComoTexto(NomeArquivo: String);
  public

  end;

const
  CabecalhoCadastrar = 'NU_NB;ID_VINCULO;NM_CARGO;NM_FUNCAO;NM_INSTITUICAO_EXERCICIO;DT_INI_EXERCICIO;VL_ULTIMA_REMUNERACAO;ESPECIE_BENEFICIO_RPPS;DT_INI_BENEFICIO_RPPS;VINCULO_FAMILIAR;NM_INSTITUIDOR_RPPS;NU_CPF_INSTITUIDOR_RPPS;DT_INI_EXERCICIO_INST;DT_OBITO_INSTITUIDOR;TE_OBSERVACAO;NU_NB_ACUMULADO';
  CabecalhoIniciarApuracao = 'NU_NB';
  ComplementoCadastrar = ';;;;;;;;;;;;;;;';

var
  JanelaPadrao: TJanelaPadrao;

implementation

uses
  LConvEncoding;

{$R *.lfm}

{ TJanelaPadrao }

procedure TJanelaPadrao.AnalisarTexto;
const
  Letras = ['a'..'z', 'A'..'Z'];
var
  I, J: Integer;
  Linha: String;
  LinhaLimpa: String;
begin
  ListaErros.Clear;
  I := 1;
  for Linha in Editor.Lines do
  begin
    LinhaLimpa := Linha.Trim;
    for J := 1 to Length(LinhaLimpa) do
    begin

      //Se tiver mais de 10 caracteres na linha, marcar com erro
      if LinhaLimpa.Length > 10 then
      begin
        ListaErros.Add(I);
        Break;
      end;

      //Se tiver letras na linha, marcar linha com erro
      if LinhaLimpa[J] in Letras then
      begin
        ListaErros.Add(I);
        Break;
      end;
    end;
    Inc(I);
  end;
end;

procedure TJanelaPadrao.CarregarTexto(NomeArquivo: String);
begin
  Editor.Lines.LoadFromFile(DialogoAbrir.Filename);
end;

function TJanelaPadrao.ChecarTexto: Boolean;
begin
  Result := True;
end;

procedure TJanelaPadrao.EditorSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if ListaErros.Contains(Line) then
  begin
     Special := true;
     FG := clWhite;
     BG := clRed;
  end;
end;

procedure TJanelaPadrao.ExibirErro(Msg: String; Detalhes: String);
begin
  DialogoMsg.Title := Msg;
  DialogoMsg.Text := Detalhes;
  DialogoMsg.Execute;
end;

procedure TJanelaPadrao.FormCreate(Sender: TObject);
begin
  ListaErros := TList<Integer>.Create;
end;

procedure TJanelaPadrao.BtoColarClick(Sender: TObject);
begin
  Editor.PasteFromClipboard();
  ProcessarTexto;
end;

procedure TJanelaPadrao.BtoSalvarClick(Sender: TObject);
begin
  if ChecarTexto then
    if RadioCadastrar.Checked or RadioIniciar.Checked then
    begin
       if RadioCadastrar.Checked then
         ModoGeracao := mgCadastrar
       else
         ModoGeracao := mgIniciarApuracao;
      if DialogoSalvar.Execute then
      begin
        if LowerCase(ExtractFileExt(DialogoSalvar.FileName)) = '.csv' then
          SalvarComoCSV(DialogoSalvar.FileName)
        else
          SalvarComoTexto(DialogoSalvar.FileName);
      end;
    end
    else
    begin
      ExibirErro('O formato de saída do arquivo CSV não foi escolhido.',
                    'Escolha o formato de saída e tente salvar o arquivo novamente.');
      if RadioCadastrar.CanFocus then
        RadioCadastrar.SetFocus;
    end;
end;

procedure TJanelaPadrao.EditorChange(Sender: TObject);
begin
  AnalisarTexto;
end;

procedure TJanelaPadrao.BtoAbrirClick(Sender: TObject);
begin
  if DialogoAbrir.Execute then
     if FileExists(DialogoAbrir.FileName) then
     begin
        CarregarTexto(DialogoAbrir.FileName);
        ProcessarTexto;
        EditorChange(Sender);
     end;
end;

procedure TJanelaPadrao.ProcessarTexto;
var
  I: Integer;
begin
  //Apaga todos os caracteres ESPAÇOS, '.' e '-' do texto
  Editor.SearchReplaceEx('-', '', [ssoReplaceAll], TPoint.Create(0, 0));
  Editor.SearchReplaceEx('.', '', [ssoReplaceAll], TPoint.Create(0, 0));
  Editor.SearchReplaceEx(' ', '', [ssoReplaceAll], TPoint.Create(0, 0));

  //Se houver linhas com menos de 10 caracteres, completar com zeros a esquerda
  for I := Editor.Lines.Count -1 downto 0 do
  begin
    if Editor.Lines[i] = '' then
      Editor.Lines.Delete(i)
    else
      Editor.Lines[i] := Editor.Lines[i].PadLeft(10, '0');
  end;
end;

{
procedure TJanelaPadrao.SalvarComoCSV(NomeArquivo: String);
var
  I: Integer;
  Linhas: Integer;
  TextoCSV: TStringStream;
begin
  TextoCSV := TFileStream.Create(NomeArquivo, fmCreate);
  try
     Linhas := Editor.Lines.Count;
     TextoCSV.Write(Cabecalho, Cabecalho.Length);
     //TextoCSV.Write(#13#10, 2);
     for I := 0 to Linhas - 1 do
     begin
       TextoCSV.Write(Editor.Lines[i].Trim);
       TextoCSV.Write(Complemento, Complemento.Length);
       //if i < Linhas - 1 then
       //TextoCSV.Write(#13#10, 2);
     end;
  finally
    TextoCSV.Free;
  end;
end;
}

procedure TJanelaPadrao.SalvarComoCSV(NomeArquivo: String);
var
  Cabecalho: String;
  Complemento: String;
  Linha: String;
  TextoCSV: TStringList;
begin
  case ModoGeracao of
    mgCadastrar:
      begin
        Cabecalho := CabecalhoCadastrar;
        Complemento := ComplementoCadastrar;
      end;
    mgIniciarApuracao:
      begin
        Cabecalho := CabecalhoIniciarApuracao;
        Complemento := '';
      end;
  end;
  TextoCSV := TStringList.Create;
  TextoCSV.TrailingLineBreak := false;
  try
     TextoCSV.AddText(Cabecalho);
     for Linha in Editor.Lines do
       TextoCSV.AddText(Linha + Complemento);
     TextoCSV.SaveToFile(NomeArquivo);
  finally
    TextoCSV.Free;
  end;
end;

procedure TJanelaPadrao.SalvarComoTexto(NomeArquivo: String);
begin
  Editor.Lines.SaveToFile(NomeArquivo);
end;

end.

