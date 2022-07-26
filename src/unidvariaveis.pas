unit unidvariaveis;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, SynEdit;

type

  { TFormVariaveis }

  TFormVariaveis = class(TForm)
    Button1: TButton;
    btoCancelar: TButton;
    Editor: TSynEdit;
    ListaImagens: TImageList;
    DialogoAbrir: TOpenDialog;
    painelRodape: TPanel;
    ToolBar1: TToolBar;
    btoNovoDoc: TToolButton;
    btoAbrirDoc: TToolButton;
    ToolButton3: TToolButton;
    btoRecortar: TToolButton;
    btoCopiar: TToolButton;
    btoColar: TToolButton;
    procedure btoAbrirDocClick(Sender: TObject);
    procedure btoCancelarClick(Sender: TObject);
    procedure btoColarClick(Sender: TObject);
    procedure btoCopiarClick(Sender: TObject);
    procedure btoNovoDocClick(Sender: TObject);
    procedure btoRecortarClick(Sender: TObject);
  private

  public

  end;

var
  FormVariaveis: TFormVariaveis;

implementation

{$R *.lfm}

{ TFormVariaveis }

procedure TFormVariaveis.btoNovoDocClick(Sender: TObject);
begin
  Editor.ClearAll;
end;

procedure TFormVariaveis.btoRecortarClick(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TFormVariaveis.btoAbrirDocClick(Sender: TObject);
begin
  if DialogoAbrir.Execute then
     if FileExists(DialogoAbrir.FileName) then
     begin
        Editor.Lines.LoadFromFile(DialogoAbrir.FileName);
     end;
end;

procedure TFormVariaveis.btoCancelarClick(Sender: TObject);
begin

end;

procedure TFormVariaveis.btoColarClick(Sender: TObject);
begin
  Editor.PasteFromClipboard();
end;

procedure TFormVariaveis.btoCopiarClick(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

end.

