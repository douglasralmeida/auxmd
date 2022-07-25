unit unidPadrao;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, ButtonPanel, Buttons, Menus, SynEdit, Generics.Collections,
  SynEditTypes;

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
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    painelPersonalizarColunas: TPanel;
    painelPersonalizarColunas1: TPanel;
    CaixaRolagem: TScrollBox;
    btoAtivarTextoPersonalizado: TToggleBox;
    btoDesativarTextoPersonalizado: TToggleBox;
    EditorMenu: TPopupMenu;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    procedure BtoAbrirClick(Sender: TObject);
    procedure btoAtivarTextoPersonalizadoChange(Sender: TObject);
    procedure BtoColarClick(Sender: TObject);
    procedure btoDesativarTextoPersonalizadoChange(Sender: TObject);
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

procedure TJanelaPadrao.btoDesativarTextoPersonalizadoChange(Sender: TObject);
begin
    btoAtivarTextoPersonalizado.State := cbChecked;
end;

procedure TJanelaPadrao.btoGerarCadastroClick(Sender: TObject);
begin
  if ChecarTexto then
//    if RadioCadastrar.Checked or RadioIniciar.Checked then
    begin
 //      if RadioCadastrar.Checked then
         ModoGeracao := mgCadastrar;
//       else
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
//      if RadioCadastrar.CanFocus then
//        RadioCadastrar.SetFocus;
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
  btoDesativarTextoPersonalizado.State := cbUnchecked;
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

