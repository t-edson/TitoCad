unit FormPrincipal;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ActnList, Menus, StdCtrls, Grids, ComCtrls, LCLType, Spin, LCLProc,
  SynFacilUtils, MisUtils, FormConfig, frameEditor, FormControlVista,
  CadDefinitions, FrameCfgGeneral, FormProject, Globales;
const
  NUM_CUAD = 20;
  ZOOM_INI = 12;

type

  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
    acHerDesp: TAction;
    acHerPunt: TAction;
    acHerRot: TAction;
    acArcNuePro: TAction;
    acArcCerrar: TAction;
    acArcGuar: TAction;
    acArcSalir: TAction;
    acProInsPolylin: TAction;
    acProPropied: TAction;
    acProInsRect: TAction;
    acProInsCubo: TAction;
    acProInsRectan: TAction;
    acProAgrPag: TAction;
    acVerVisSup: TAction;
    acVerConVista: TAction;
    arbNaveg: TTreeView;
    Label2: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    acHerConfig: TAction;
    ActionList1: TActionList;
    acArcAbrir: TAction;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    panNaveg: TPanel;
    PopupProject: TPopupMenu;
    PopupGeomet: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acArcCerrarExecute(Sender: TObject);
    procedure acArcGuarExecute(Sender: TObject);
    procedure acArcNueProExecute(Sender: TObject);
    procedure acArcSalirExecute(Sender: TObject);
    procedure acProAgrPagExecute(Sender: TObject);
    procedure acProPropiedExecute(Sender: TObject);
    procedure acVerConVistaExecute(Sender: TObject);
    procedure acVerVisSupExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acHerConfigExecute(Sender: TObject);
  private
    curProject: TCadProyecto;
    fraMotEdicion: TfraGrafEditor;
    procedure arbNavegMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ConfigPropertiesChanged;
    procedure curPresupModific;
    procedure fraMotEdicionmotEdiChangeView;
    function MensajeGuardarCambios: integer;
    function NodoEsTablero(nod: TTreeNode): boolean;
    function NodoProjectSelec: TTreeNode;
    function NodoSelec: TTreeNode;
    function NodoTablerSelec: TTreeNode;
    function NombreNodo(nod: TTreeNode): string;
    function NombreNodoSelec: string;
    procedure RefrescarEntorno;
    procedure RefrescarPanelNaveg;
    procedure Refrescar;
    procedure RefrescarPanelVista;
    procedure SeleccNodo(nom: string);
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}
const
  BOT_CANCEL    = 3;
  MSJE_SIN_ELEM = '<Sin elementos>';

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  //Configura  fraMotEdicion
  fraMotEdicion:= TfraGrafEditor.Create(self);
  fraMotEdicion.Parent := TabSheet1;
  fraMotEdicion.Visible:=true;
  fraMotEdicion.Align:=alClient;
  fraMotEdicion.motEdi.v2d.backColor:=clBlack;
  fraMotEdicion.motEdi.OnChangeView:=@fraMotEdicionmotEdiChangeView;
  arbNaveg.OnMouseUp:=@arbNavegMouseUp;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  Config.SetLanguage('en');
  Config.Iniciar(nil);  //Inicia la configuración
  Config.OnPropertiesChanged:=@ConfigPropertiesChanged;
  ConfigPropertiesChanged;
  fraMotEdicion.AgregaObjeto;
  fraMotEdicion.AgregaObjeto;
  Refrescar;
acArcNueProExecute(self);
//acVerConVistaExecute(self);
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Config.escribirArchivoIni();
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  if curProject<>nil then curProject.Destroy;
  fraMotEdicion.Destroy;
end;
procedure TfrmPrincipal.ConfigPropertiesChanged;
//Se cambian las propiedades de la configuración
begin
  StatusBar1.Visible:= Config.fcGeneral.VerBarEst;
  ToolBar1.Visible := Config.fcGeneral.VerBarHer;
//  fraMotEdicion.motEdi.incWheel:=Config.fcVista.increWheel;
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
function TfrmPrincipal.NodoSelec: TTreeNode;
{Devuelve el nodo seleccionado actualmente. }
begin
  if curProject = nil then exit(nil);
  Result := arbNaveg.Selected;
end;
function TfrmPrincipal.NombreNodo(nod: TTreeNode): string;
{Devuelve el nombre de un nodo. }
begin
  Result := nod.Text;
end;
function TfrmPrincipal.NombreNodoSelec: string;
{Devuelve el nombre del nodo seleccionado.}
begin
  if arbNaveg.Selected = nil then exit('');
  Result := NombreNodo(arbNaveg.Selected);
end;
function TfrmPrincipal.NodoEsTablero(nod: TTreeNode): boolean;
{Indica si el nodo seleccionado corresponde a un tablero}
begin
  if nod=nil then exit(false);
  Result := (nod.Level = 1) and (nod.Text <> MSJE_SIN_ELEM);
end;
function TfrmPrincipal.NodoProjectSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Level = 0 then exit(NodoSelec);
  exit(nil);
end;
procedure TfrmPrincipal.SeleccNodo(nom: string);
{Seleciona un nodo en al árbol, usando su nombre. Funcionará si el árbol tiene o no enfoque.}
var
  nod: TTreeNode;
begin
  for nod in arbNaveg.Items do begin
    if (NombreNodo(nod) = nom) then begin
      nod.Selected:=true;
      exit;
    end;
  end;
end;
function TfrmPrincipal.NodoTablerSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Visible = false then exit(nil);
  if NodoEsTablero(NodoSelec) then
    exit(NodoSelec);
  exit(nil);
end;
procedure TfrmPrincipal.arbNavegMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  nod: TTreeNode;
begin
  nod := arbNaveg.GetNodeAt(x, y);
  if nod = nil then begin
    //se marcón fuera de un nodo
    arbNaveg.Selected := nil;
  end;
  //Abre menú contextual, de acuerdo al nodo seleccionado
  if Button = mbRight then begin
    if arbNaveg.GetNodeAt(X,Y)<>nil then arbNaveg.GetNodeAt(X,Y).Selected:=true;
    if NodoTablerSelec<>nil then begin
      //Menú de tablero
      PopupGeomet.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
    end;
    if NodoProjectSelec<>nil then begin
      //Menú de presupuesto
      PopupProject.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
    end;
  end;
end;
function TfrmPrincipal.MensajeGuardarCambios: integer;
{Muestra una ventana para confirmar si se guarda o no los cambios. Si se selecciona
cancelar, se devuelve el valor BOT_CANCEL.}
var
  rpta: Byte;
begin
  if (curProject <> nil) and curProject.Modific then begin
    rpta := MsgYesNoCancel('El presupuesto ha sido modificado, ¿Guardar cambios?');
    if rpta = 3 then exit(BOT_CANCEL);
    if rpta = 1 then curProject.GuardarArchivo;
  end;
  Result := 0;   //valor por defecto
end;
procedure TfrmPrincipal.RefrescarEntorno;
{Refresca la pantalla principal para reflejar el estado actual.}
  procedure AccionesModific(estado: boolean);
  begin
    //desactiva opciones de modificación
//    acPreAgrTab.Enabled   :=estado;
//    acPreActPresup.Enabled:=estado;
//    acPreImpTabler.Enabled:=estado;
//    acPreRplMarca.Enabled :=estado;
//    acTabAgrMater.Enabled :=estado;
//    acTabEliTab.Enabled   :=estado;
//    acMatDefNMat.Enabled  :=estado;
//    acMatSubir.Enabled    :=estado;
//    acMatBajar.Enabled    :=estado;
//    acMatElimin.Enabled   :=estado;
  end;
  procedure AccionesReportes(estado: boolean);
  begin
//    acRepPresXLS.Enabled:=estado;
//    acRepPresPDF.Enabled:=estado;
//    acRepDiaCAD.Enabled:=estado;
//    acRepMaterAgrup.Enabled:=estado;
//    acRepListTab.Enabled:=estado;
//    acRepMaterTab.Enabled:=estado;
//    acRepMaterResum.Enabled:=estado;
  end;
  procedure MenuEstadoPresup;
  {Actualiza los menús de estado}
  begin
//    if curProject = nil then begin
//      acPreMarCrea.Enabled:=false;
//      acPreMarTerm.Enabled:=false;
//      acPreMarRev.Enabled:=false;
//      acPreMarCerr.Enabled:=false;
//    end else begin
//      //Las opciones disponibles, dependen del estado actual
//      case curProject.Estado of
//      epCreado  : begin
//        acPreMarCrea.Enabled:=false;
//        acPreMarTerm.Enabled:=true;
//        acPreMarRev.Enabled:=false;
//        acPreMarCerr.Enabled:=false;
//        AccionesModific(true); //opciones de modificación
//        end;
//      epTerminado: begin
//        acPreMarCrea.Enabled:=true;
//        acPreMarTerm.Enabled:=false;
//        acPreMarRev.Enabled:=true;
//        acPreMarCerr.Enabled:=false;
//        AccionesModific(true); //opciones de modificación
//        end;
//      epRevisado: begin
//        acPreMarCrea.Enabled:=true;
//        acPreMarTerm.Enabled:=false;
//        acPreMarRev.Enabled:=false;
//        acPreMarCerr.Enabled:=true;
//        AccionesModific(true); //opciones de modificación
//        end;
//      epCerrado: begin
//        acPreMarCrea.Enabled:=false;
//        acPreMarTerm.Enabled:=false;
//        acPreMarRev.Enabled:=false;
//        acPreMarCerr.Enabled:=false;
//        AccionesModific(false); //opciones de modificación
//        end;
//      end;
//    end;
  end;
begin
  if curProject = nil then begin
//    //No hay presupuesto abierto
//    mnPresup.Enabled:=false;  //desactiva todo el menú
//    mnTablero.Enabled:=false;
//    mnReportes.Enabled:=false;
    Caption := NOM_PROG + ' ' + VER_PROG;
//    acArcGuar.Enabled:=false;
//    acArcGuarCom.Enabled:=false;
//    acArcCerrar.Enabled:=false;
//    acPreAgrTab.Enabled:=false;
//    acPrePropied.Enabled:=false;
//    acPreValidar.Enabled:=false;
//
//    acMatBajar.Enabled:=false;
//    acMatSubir.Enabled:=false;
//
//    AccionesReportes(false);
  end else begin
     //Hay presupuesto abierto
//    mnPresup.Enabled:=true;  //activa todo el menú
//    mnTablero.Enabled:=true;
//    mnReportes.Enabled:=true;
    Caption := NOM_PROG + ' ' + VER_PROG + ' - ' + curProject.nombre;
//    acArcGuar.Enabled := curProject.Modific;
//    acArcGuarCom.Enabled:=true;
//    acArcCerrar.Enabled:=true;
//    acPreAgrTab.Enabled:=true;
//    acPrePropied.Enabled:=true;
//    acPreValidar.Enabled:=true;
//
//    acMatBajar.Enabled:=true;
//    acMatSubir.Enabled:=true;
//
//    AccionesReportes(true);
  end;
//  menuRec.ActualMenusReciente(Self);
  MenuEstadoPresup;  //actualiza los menús de estado
end;
procedure TfrmPrincipal.RefrescarPanelNaveg;
{Refresca el panel con los ítems del presupuesto.}
var
  pag: TCadPage;
  nod: TTreeNode;
  nodProj: TTreeNode;
  nom: String;
  nodCop, nodGeomet, nodVista: TTreeNode;
  ns: String;
begin
  ns := NombreNodoSelec;  //guarda elemento seleccionado
  arbNaveg.Items.Clear;  //limpia elementos
  if curProject = nil then begin
    //No hay presupuesto actual
    PageControl1.Visible := false;
    panNaveg.Visible := false;
    exit;
  end;
  //Hay un presupuesto abierto
  PageControl1.Visible := true;
  panNaveg.Visible := true;
  //Agrega nodo de proyecto
  nodProj := arbNaveg.items.AddChild(nil, curProject.nombre);  //agrega presupuesto actual
  nodProj.ImageIndex := 25;
  nodProj.SelectedIndex:=25;
  //Agrega nodo de las páginas
  arbNaveg.BeginUpdate;
  for pag in curProject.pages do begin
     nod := arbNaveg.Items.AddChild(nodProj, pag.nombre);
     nod.ImageIndex := 34;
     nod.SelectedIndex:=34;
     //Agrega campos de página
     nodGeomet := arbNaveg.items.AddChild(nod, 'Objetos Gráficos');  //agrega presupuesto actual
     nodGeomet.ImageIndex := 27;
     nodGeomet.SelectedIndex:=27;
     nodVista := arbNaveg.items.AddChild(nod, 'Vista Principal');  //agrega presupuesto actual
     nodVista.ImageIndex := 26;
     nodVista.SelectedIndex:=26;
  end;
  arbNaveg.EndUpdate;
   //Hay ítems (tableros u otros), carga tableros
  nodProj.Expanded:=true;   //lo deja expandido
  SeleccNodo(ns);  //restaura la selección
end;
procedure TfrmPrincipal.RefrescarPanelVista;
begin
  fraMotEdicion.motEdi.Refrescar;
end;
procedure TfrmPrincipal.Refrescar;
{Rerfresca toda la interfaz}
begin
  RefrescarEntorno;
  RefrescarPanelNaveg;
  RefrescarPanelVista;
end;
procedure TfrmPrincipal.curPresupModific;
//Llamado cuando el presupuesto ha sido modificado.
begin
  acArcGuar.Enabled:=true;
end;
procedure TfrmPrincipal.fraMotEdicionmotEdiChangeView;
begin
  StatusBar1.Panels[1].Text :=
     'Alfa=' + formatfloat('0.00', fraMotEdicion.Alfa) + ' ' +
     'Fi=' + formatfloat('0.00', fraMotEdicion.Fi);
  //FloatToStr(fraMotEdicion.Alfa);
  StatusBar1.Panels[2].Text :=
     'Zoom=' + formatfloat('0.00', fraMotEdicion.Zoom);
end;
///////////////////////////// Acciones ///////////////////////////////
procedure TfrmPrincipal.acVerConVistaExecute(Sender: TObject);
begin
  frmControlVista.Exec(fraMotEdicion);
end;
procedure TfrmPrincipal.acVerVisSupExecute(Sender: TObject);
begin
  if curProject=nil then exit;
  fraMotEdicion.Alfa:=0;
  fraMotEdicion.Fi:=0;
  fraMotEdicion.motEdi.Refrescar;
end;
procedure TfrmPrincipal.acArcNueProExecute(Sender: TObject);
var
  tmpPresp: TCadProyecto;
begin
  //verifica si hay que guardar cambios
  if MensajeGuardarCambios = BOT_CANCEL then exit;
  //Crea presupuesto temporal
  tmpPresp := TCadProyecto.Create;
  if not frmProject.ExecNew(tmpPresp) then begin
    //se canceló
    tmpPresp.Destroy;  //no nos va a servir
    exit;  //sale dejando el presupuesto actual
  end;
  //Cierra presupuesto actual y asigna el temporal al actual
  acArcCerrarExecute(self);   //cierra actual si estaba abierto
  curProject := tmpPresp;  //apunta al temporal
  curProject.OnModific:=@curPresupModific;
  //curProject.Modific:=true;
  curProject.guardarArchivo;
//  menuRec.AgregArcReciente(curProject.GenerarNombreArch);  //Agrega archivo reciente
  Refrescar;
end;
procedure TfrmPrincipal.acArcGuarExecute(Sender: TObject);
begin

end;
procedure TfrmPrincipal.acArcCerrarExecute(Sender: TObject);
begin
  if curProject = nil then exit;  //no hay presupuesto abierto
  //verifica si hay presupuesto modificado
  if MensajeGuardarCambios = BOT_CANCEL then exit;
  curProject.Destroy;
  curProject := nil;   //lo marca como cerrado
  Refrescar;
end;
procedure TfrmPrincipal.acArcSalirExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmPrincipal.acProAgrPagExecute(Sender: TObject);
begin
  if curProject = nil then exit;  //no hay presupuesto abierto
  curProject.AgregPagina;
  Refrescar;
end;

procedure TfrmPrincipal.acProPropiedExecute(Sender: TObject);
begin
  if curProject = nil then exit;
  if frmProject.Exec(curProject, @RefrescarPanelVista) then begin
    curProject.Modific:=true;
    RefrescarPanelNaveg;
    RefrescarPanelVista;
  end;
end;

procedure TfrmPrincipal.acHerConfigExecute(Sender: TObject);
begin
  Config.Configurar();
end;

end.

