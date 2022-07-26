unit unidPadrao;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, ButtonPanel, Buttons, Menus, ComCtrls, SynEdit, Types,
  Generics.Collections, SynEditTypes;

type
  { TModoGeracao }
  TModoGeracao = (mgCadastrar, mgIniciarApuracao);

  { TJanelaPadrao }
  TJanelaPadrao = class(TForm)
    EditorLimpar: TAction;
    EditorColar: TAction;
    EditorRecortar: TAction;
    EditorSelecionarTudo: TAction;
    EditorCopiar: TAction;
    EditorDesfazer: TAction;
    BtoAbrir: TButton;
    btoEditarVar: TButton;
    BtoColar: TButton;
    btoGerarCadastro: TButton;
    btoGerarApuracao: TButton;
    Editor: TSynEdit;
    editPosicaoColuna: TEdit;
    editMaximoItens: TEdit;
    editTextoPersonalizado: TEdit;
    editSeparadorNome: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListaAcoes: TActionList;
    DialogoAbrir: TOpenDialog;
    DialogoSalvar: TSaveDialog;
    DialogoMsg: TTaskDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    painelMensagem: TPanel;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    painelPersonalizarColunas: TPanel;
    painelLimiteMaximo: TPanel;
    CaixaRolagem: TScrollBox;
    btoAtivarTextoPersonalizado: TToggleBox;
    EditorMenu: TPopupMenu;
    btoAtivarLimiteMaximo: TToggleBox;
    NotificadorTimer: TTimer;
    ToolBar1: TToolBar;
    btoNovoModelo: TToolButton;
    btoAbrirModelo: TToolButton;
    btoSalvarModelo: TToolButton;
    ToolButton4: TToolButton;
    procedure BtoAbrirClick(Sender: TObject);
    procedure btoAtivarTextoPersonalizadoChange(Sender: TObject);
    procedure BtoColarClick(Sender: TObject);
    procedure btoDesativarTextoPersonalizadoChange(Sender: TObject);
    procedure btoEditarVarClick(Sender: TObject);
    procedure btoGerarApuracaoClick(Sender: TObject);
    procedure btoGerarCadastroClick(Sender: TObject);
    procedure CaixaRolagemClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure EditorColarExecute(Sender: TObject);
    procedure EditorCopiarExecute(Sender: TObject);
    procedure EditorDesfazerExecute(Sender: TObject);
    procedure EditorRecortarExecute(Sender: TObject);
    procedure EditorSelecionarTudoExecute(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
    procedure btoAtivarLimiteMaximoChange(Sender: TObject);
    procedure btoDesativarLimiteMaximoChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NotificadorTimerTimer(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private
    ListaErros: TList<Integer>;
    ListaVariaveis: TStringList;
    ModoGeracao: TModoGeracao;
    PosicaoColuna: Integer;
    TextoPersonalizado: String;
    MaximoItens: Integer;
    SeparadorNomeArquivo: String;
    UsarTextoPersonalizado: Boolean;
    procedure AnalisarTexto;
    procedure CarregarTexto(NomeArquivo: String);
    function ChecarTexto: Boolean;
    function ChecarForm: Boolean;
    procedure ExibirErro(Msg: String; Detalhes: String);
    procedure ExibirNotificacao(Msg: String);
    function ObterTextoPersonalizado(Texto: String; Pos: Integer): String;
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
  unidCSV, unidVariaveis, LConvEncoding;

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
var
  Texto: String;
begin
  if btoAtivarTextoPersonalizado.Checked then
  begin
    Texto := editTextoPersonalizado.Text;
    if Texto.Contains('{}') and (Editor.Lines.Count <> ListaVariaveis.Count) then
    begin
      ExibirErro('A coluna com texto personalizado foi ativada com uso de variáveis, mas a quantidade de itens está diferente da quantidade de variáveis.',
                          'Corrija as variáveis personalizadas e tente gerar a tabela novamente.');
            Exit(False);
    end;
  end;
  Result := True;
end;

function TJanelaPadrao.ChecarForm: Boolean;
begin
  if btoAtivarTextoPersonalizado.Checked then
  begin
    if Trim(editPosicaoColuna.Text).Length = 0 then
    begin
      ExibirErro('A coluna com texto personalizado foi ativada, mas a posição da coluna personalizada não foi informada.',
                          'Informe o valor do campo tente gerar a tabela novamente.');
            Exit(False);
    end;
    if StrToInt(editPosicaoColuna.Text) <= 0 then
    begin
      ExibirErro('A coluna com texto personalizado foi atividada, mas o campo Posição da Coluna está com um valor negativo.',
                    'Corrija o valor do campo tente gerar a tabela novamente.');
      Exit(False);
    end;
    if Trim(editTextoPersonalizado.Text).Length = 0 then
    begin
      ExibirErro('A coluna com texto personalizado foi ativada, mas o campo Texto Personalizado não foi informado.',
                          'Informe o valor do campo tente gerar a tabela novamente.');
            Exit(False);
    end;
  end;
  if btoAtivarLimiteMaximo.Checked then
  begin
    if Trim(editMaximoItens.Text).Length = 0 then
    begin
      ExibirErro('O limite máximo de itens foi ativado, mas o campo Máximo de Itens Por Arquivo não foi informado.',
                          'Informe o valor do campo tente gerar a tabela novamente.');
            Exit(False);
    end;
    if StrToInt(editMaximoItens.Text) <= 0 then
    begin
      ExibirErro('O limite máximo de itens foi ativado, mas o campo Máximo de Itens por Arquivo está com um valor negativo.',
                    'Corrija o valor do campo tente gerar a tabela novamente.');
      Exit(False);
    end;
  end;
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

procedure TJanelaPadrao.ExibirNotificacao(Msg: String);
begin
  painelMensagem.Caption := Msg;
  painelMensagem.Show;
  NotificadorTimer.Enabled := True;
end;

procedure TJanelaPadrao.FormCreate(Sender: TObject);
begin
  ListaErros := TList<Integer>.Create;
  ListaVariaveis := TStringList.Create;
end;

procedure TJanelaPadrao.btoAtivarLimiteMaximoChange(Sender: TObject);
begin
  if btoAtivarLimiteMaximo.Checked then
    btoAtivarLimiteMaximo.Caption := 'Desat&ivar'
  else
    btoAtivarLimiteMaximo.Caption := 'At&ivar';
  painelLimiteMaximo.Enabled := btoAtivarLimiteMaximo.Checked;
end;

procedure TJanelaPadrao.btoDesativarLimiteMaximoChange(Sender: TObject);
begin

end;

procedure TJanelaPadrao.FormDestroy(Sender: TObject);
begin
  if ListaVariaveis <> nil then
  begin
    ListaVariaveis.Clear;
    FreeAndNil(ListaVariaveis);
  end;
end;

procedure TJanelaPadrao.NotificadorTimerTimer(Sender: TObject);
begin
  painelMensagem.Hide;
end;

procedure TJanelaPadrao.ToolBar1Click(Sender: TObject);
begin

end;

procedure TJanelaPadrao.BtoColarClick(Sender: TObject);
begin
  Editor.PasteFromClipboard();
  ProcessarTexto;
end;

procedure TJanelaPadrao.btoDesativarTextoPersonalizadoChange(Sender: TObject);
begin

end;

procedure TJanelaPadrao.btoEditarVarClick(Sender: TObject);
begin
  if FormVariaveis = nil then
    FormVariaveis := TFormVariaveis.Create(nil);
  try
     FormVariaveis.ShowModal;
     if FormVariaveis.ModalResult = mrOK then
     begin
       ListaVariaveis.Clear;
       ListaVariaveis.AddStrings(FormVariaveis.Editor.Lines);
     end;
  finally
    FormVariaveis.Free;
    FormVariaveis := nil;
  end;
end;

procedure TJanelaPadrao.btoGerarApuracaoClick(Sender: TObject);
begin
  ModoGeracao := mgIniciarApuracao;
  PosicaoColuna := StrToInt(editPosicaoColuna.Text) - 1;
  TextoPersonalizado := editTextoPersonalizado.Text;
  MaximoItens := StrToInt(editMaximoItens.Text);
  SeparadorNomeArquivo := editSeparadorNome.Text;
end;

procedure TJanelaPadrao.btoGerarCadastroClick(Sender: TObject);
begin
  if ChecarTexto and ChecarForm then
  begin
    ModoGeracao := mgCadastrar;
    UsarTextoPersonalizado := btoAtivarTextoPersonalizado.State = cbChecked;
    if Length(editPosicaoColuna.Text) > 0 then
      PosicaoColuna := StrToInt(editPosicaoColuna.Text) - 1
    else
      PosicaoColuna := 0;
    TextoPersonalizado := editTextoPersonalizado.Text;
    if Length(editMaximoItens.Text) > 0 then
      MaximoItens := StrToInt(editMaximoItens.Text)
    else
      MaximoItens := 0;
    SeparadorNomeArquivo := editSeparadorNome.Text;
    if DialogoSalvar.Execute then
    begin
      if LowerCase(ExtractFileExt(DialogoSalvar.FileName)) = '.csv' then
        SalvarComoCSV(DialogoSalvar.FileName)
      else
        SalvarComoTexto(DialogoSalvar.FileName);
    end;
  end;
end;

procedure TJanelaPadrao.CaixaRolagemClick(Sender: TObject);
begin

end;

procedure TJanelaPadrao.EditorChange(Sender: TObject);
begin
  AnalisarTexto;
end;

procedure TJanelaPadrao.EditorColarExecute(Sender: TObject);
begin
  Editor.PasteFromClipboard();
end;

procedure TJanelaPadrao.EditorCopiarExecute(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TJanelaPadrao.EditorDesfazerExecute(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TJanelaPadrao.EditorRecortarExecute(Sender: TObject);
begin
    Editor.CutToClipboard;
end;

procedure TJanelaPadrao.EditorSelecionarTudoExecute(Sender: TObject);
begin
  Editor.SelectAll;
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

procedure TJanelaPadrao.btoAtivarTextoPersonalizadoChange(Sender: TObject);
begin
  if btoAtivarTextoPersonalizado.Checked then
  begin
    btoAtivarTextoPersonalizado.Caption := 'Desa&tivar';
  end
  else
  begin
    btoAtivarTextoPersonalizado.Caption := 'A&tivar';
  end;
  painelPersonalizarColunas.Enabled :=  btoAtivarTextoPersonalizado.Checked;
  btoEditarVar.Enabled :=  btoAtivarTextoPersonalizado.Checked;
end;

function TJanelaPadrao.ObterTextoPersonalizado(Texto: String; Pos: Integer): String;
var
  TextoNormalizado: String;
begin
  if UsarTextoPersonalizado and (ListaVariaveis.Count - 1 >= Pos) then
    TextoNormalizado := Texto.Replace('{}', ListaVariaveis[Pos])
  else
    TextoNormalizado := Texto.Replace('{}', '');
  Result := TextoNormalizado;
end;

procedure TJanelaPadrao.ProcessarTexto;
var
  I: Integer;
begin
  //Apaga todos os caracteres ESPAÇO, '.' e '-' do texto
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

procedure TJanelaPadrao.SalvarComoCSV(NomeArquivo: String);
var
  I, J: Integer;
  Cabecalho: TStringDynArray;
  TotalLinhas: Integer;
  TotalColunas: Integer;
  Linha: String;
  DocumentoCSV: TCSVData;
begin
  case ModoGeracao of
    mgCadastrar:
      begin
        Cabecalho := CabecalhoCadastrar.Split(';');
      end;
    mgIniciarApuracao:
      begin
        Cabecalho := CabecalhoIniciarApuracao.Split(';');
      end;
  end;
  TotalColunas := Length(Cabecalho);
  TotalLinhas := Editor.Lines.Count + 1;
  DocumentoCSV := TCSVData.Create(TotalLinhas, TotalColunas);
  try
    DocumentoCSV.DelimiterChar := ';';

    //adiciona cabecalho
    for I := 0 To TotalColunas - 1 do
      DocumentoCSV.CellValue[0, I] := Cabecalho[I];

     //adiciona os beneficios
    I := 1;
    J := 0;
    for Linha in Editor.Lines do
    begin
      if Length(Trim(Linha)) = 0 then
        Continue;
      for J := 0 To TotalColunas - 1 do
      begin
        if J = 0 then
          DocumentoCSV.CellValue[I, J] := Linha
        else if J = PosicaoColuna then
          DocumentoCSV.CellValue[I, J] := ObterTextoPersonalizado(TextoPersonalizado, I-1)
        else
          DocumentoCSV.CellValue[I, J] := '';
       end;
       Inc(I);
     end;

     //salva o arquivo CSV
     DocumentoCSV.SaveToFile(NomeArquivo);
     ExibirNotificacao('Tabela CSV gerada com sucesso.');
  finally
    DocumentoCSV.Free;
  end;
end;

procedure TJanelaPadrao.SalvarComoTexto(NomeArquivo: String);
begin
  Editor.Lines.SaveToFile(NomeArquivo);
end;

end.

