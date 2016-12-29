unit FormPrincipal;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ActnList, Menus, StdCtrls, Grids, ComCtrls, LCLType, Spin,
  SynFacilUtils, MisUtils, FormConfig, frameEditor, FormControlVista,
  FrameCfgGeneral;
const
  NUM_CUAD = 20;
  ZOOM_INI = 12;

type

  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
    acHerDesp: TAction;
    acHerPunt: TAction;
    acHerRot: TAction;
    acVerConVista: TAction;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    acHerConfig: TAction;
    ActionList1: TActionList;
    acArcAbrir: TAction;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure acVerConVistaExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acHerConfigExecute(Sender: TObject);
  private
    fraMotEdicion: TfraGrafEditor;
    procedure ConfigPropertiesChanged;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  //Configura  fraMotEdicion
  fraMotEdicion:= TfraGrafEditor.Create(self);
  fraMotEdicion.Parent := self;
  fraMotEdicion.Visible:=true;
  fraMotEdicion.Align:=alClient;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  Config.SetLanguage('en');
  Config.Iniciar(nil);  //Inicia la configuración
  Config.OnPropertiesChanged:=@ConfigPropertiesChanged;
  ConfigPropertiesChanged;
  fraMotEdicion.AgregaObjeto;
  fraMotEdicion.AgregaObjeto;
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Config.escribirArchivoIni();
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  fraMotEdicion.Destroy;
end;
procedure TfrmPrincipal.ConfigPropertiesChanged;
//Se cambian las propiedades de la configuración
begin
  StatusBar1.Visible:= Config.fcGeneral.VerBarEst;
  ToolBar1.Visible := Config.fcGeneral.VerBarHer;

  case Config.fcGeneral.StateToolbar of
  stb_SmallIcon: begin
    ToolBar1.ButtonHeight:=22;
    ToolBar1.ButtonWidth:=22;
    ToolBar1.Height:=26;
    ToolBar1.Images:=ImgActions16;
  end;
  stb_BigIcon: begin
    ToolBar1.ButtonHeight:=38;
    ToolBar1.ButtonWidth:=38;
    ToolBar1.Height:=42;
    ToolBar1.Images:=ImgActions32;
  end;
  end;
end;

///////////////////////////// Acciones ///////////////////////////////
procedure TfrmPrincipal.acVerConVistaExecute(Sender: TObject);
begin
  frmControlVista.Exec(fraMotEdicion);
end;
procedure TfrmPrincipal.acHerConfigExecute(Sender: TObject);
begin
  Config.Configurar();
end;

end.

