unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, MisUtils, FormConfig, FrameCfgGeneral, FrameCfgVista,
  CadDefinitions, frameVisorGraf, FormProject, Globales, FrameExplorProyectos,
  FormControlVista, FormVistaProp, DefObjGraf;
const
  NUM_CUAD = 20;
  ZOOM_INI = 12;

type

  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
  published
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
    acPagPropied: TAction;
    acPagCamNom: TAction;
    acPagElim: TAction;
    acPagAgrLin: TAction;
    acVisPropied: TAction;
    acVerVisSup: TAction;
    acVerConVista: TAction;
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
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
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
    PopupVista: TPopupMenu;
    PopupPagina: TPopupMenu;
    PopupProject: TPopupMenu;
    PopupObjetos: TPopupMenu;
    Splitter2: TSplitter;
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
    procedure acPagAgrLinExecute(Sender: TObject);
    procedure acPagElimExecute(Sender: TObject);
    procedure acProAgrPagExecute(Sender: TObject);
    procedure acProPropiedExecute(Sender: TObject);
    procedure acVerConVistaExecute(Sender: TObject);
    procedure acVerVisSupExecute(Sender: TObject);
    procedure acVisPropiedExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acHerConfigExecute(Sender: TObject);
  private
    curProject: TCadProyecto;
    curPagina : TCadPagina;
    curVista  : TfraVisorGraf;
    procedure ConfigPropertiesChanged;
    procedure curPresupModific;
    procedure curProject_ChangeActivePage;
    procedure fraExplorProy_ClickDerPagina(pag: TCadPagina);
    procedure fraExplorProy_ClickDerProyec(pro: TCadProyecto);
    procedure fraExplorProy_ClickDerVista(vis: TfraVisorGraf);
    procedure curProject_ChangeView(vista: TfraVisorGraf);
    function MensajeGuardarCambios: integer;
    procedure RefrescarEntorno;
    procedure Refrescar;
    procedure RefrescarPanelVista;
  public
    fraExplorProy : TfraExplorProyectos;  //Explorador de proyectos
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}
const
  BOT_CANCEL    = 3;

procedure TfrmPrincipal.fraExplorProy_ClickDerProyec(pro: TCadProyecto);
begin
  PopupProject.PopUp;
end;
procedure TfrmPrincipal.fraExplorProy_ClickDerPagina(pag: TCadPagina);
begin
  curPagina := pag;
  PopupPagina.PopUp;
end;
procedure TfrmPrincipal.fraExplorProy_ClickDerVista(vis: TfraVisorGraf);
begin
  curVista := vis;
  PopupVista.PopUp;
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  //Configura Explorador de proyectos
  fraExplorProy := TfraExplorProyectos.Create(self);
  fraExplorProy.Parent := self;
  fraExplorProy.Name:='fraExpProy';
  fraExplorProy.Caption:='Explorador de Proyectos1';
  fraExplorProy.OnClickDerProyec:=@fraExplorProy_ClickDerProyec;
  fraExplorProy.OnClickDerPagina:=@fraExplorProy_ClickDerPagina;
  fraExplorProy.OnClickDerVista:=@fraExplorProy_ClickDerVista;
  fraExplorProy.OnBorrarPagina:=@acPagElimExecute;
  fraExplorProy.Iniciar(@curProject);

  //Configura el alineamiento
  fraExplorProy.Align:=alLeft;
  Splitter2.Align:=alLeft;
  fraExplorProy.Visible:=true;
  PageControl1.Align:=alClient;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  Config.SetLanguage('en');
  Config.Iniciar(nil);  //Inicia la configuración
  Config.OnPropertiesChanged:=@ConfigPropertiesChanged;
  ConfigPropertiesChanged;
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
procedure TfrmPrincipal.RefrescarPanelVista;
begin
//  fraMotEdicion.motEdi.Refrescar;
end;
procedure TfrmPrincipal.Refrescar;
{Rerfresca toda la interfaz}
begin
  RefrescarEntorno;
  fraExplorProy.Refrescar;   //Refresca explorador de proyecto
  RefrescarPanelVista;
end;
procedure TfrmPrincipal.curPresupModific;
//Llamado cuando el presupuesto ha sido modificado.
begin
  acArcGuar.Enabled:=true;
end;
procedure TfrmPrincipal.curProject_ChangeView(vista: TfraVisorGraf);
begin
  StatusBar1.Panels[1].Text :=
     'Alfa=' + formatfloat('0.00',  vista.Alfa) + ' ' +
     'Fi=' + formatfloat('0.00', vista.Fi);
  //FloatToStr(fraMotEdicion.Alfa);
  StatusBar1.Panels[2].Text :=
     'Zoom=' + formatfloat('0.00', vista.Zoom);
end;
procedure TfrmPrincipal.curProject_ChangeActivePage;
{Se cambió la página activa del proyecto actual. Hay que mostrarlo en pantalla}
var
  ap: TCadPagina;
begin
  if curProject=nil then exit;
  //Enchufa el visor al PageControl1, para mostralo;
  curProject.HideAllPages;   {oculta primero todas las páginas porque puede que alguna
                              ya haya puesto su "Parent" en eset visor.}
  ap := curProject.ActivePage;
  ap.vista.Parent := TabSheet1;   //Lo coloca aquí
  ap.vista.Left:=Random(200);
  ap.vista.Top:=Random(200);
  ap.vista.Align := alClient;
  ap.vista.Visible := true;  //lo hace visible
end;
///////////////////////////// Acciones ///////////////////////////////
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
  curProject.OnCambiaPerspec:=@curProject_ChangeView;
  curProject.OnChangeActivePage:=@curProject_ChangeActivePage;
  curProject_ChangeActivePage;  //para refrescar en su visor
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
procedure TfrmPrincipal.acVerConVistaExecute(Sender: TObject);
begin
  if curProject = nil then exit;
  frmControlVista.Exec(curProject.ActivePage.vista);
end;
procedure TfrmPrincipal.acVerVisSupExecute(Sender: TObject);
begin
  if curProject=nil then exit;
  curProject.ActivePage.vista.Alfa:=0;
  curProject.ActivePage.vista.Fi:=0;
  curProject.ActivePage.vista.visEdi.Refrescar;
end;
procedure TfrmPrincipal.acVisPropiedExecute(Sender: TObject);
begin
  frmVistaProp.Exec(curVista);
end;
procedure TfrmPrincipal.acProAgrPagExecute(Sender: TObject);
begin
  if curProject = nil then exit;  //no hay presupuesto abierto
  curProject.AddPage;
  Refrescar;
end;
procedure TfrmPrincipal.acProPropiedExecute(Sender: TObject);
begin
  if curProject = nil then exit;
  if frmProject.Exec(curProject, @RefrescarPanelVista) then begin
    curProject.Modific:=true;
    fraExplorProy.Refrescar;
    RefrescarPanelVista;
  end;
end;
procedure TfrmPrincipal.acPagAgrLinExecute(Sender: TObject);  //Agrega línea
var
  p2, p1: TPoint3;
begin
  p1.x:=0; p1.y:=0; p1.z := 0;
  p2.x:=100; p2.y:=100; p2.z :=0;

  curPagina.AddLine(p1, p2);
  curPagina.vista.visEdi.Refrescar;
  Refrescar;
end;
procedure TfrmPrincipal.acPagElimExecute(Sender: TObject);
begin
  if curProject = nil then exit;
  if MsgYesNo('¿Eliminar página "%s"?', [curProject.ActivePage.nombre]) <> 1 then exit;
  curProject.RemovePage(curProject.ActivePage);
  Refrescar;
end;

procedure TfrmPrincipal.acHerConfigExecute(Sender: TObject);
begin
  Config.Configurar();
end;

end.

