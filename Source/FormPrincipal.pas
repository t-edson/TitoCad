unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, LCLProc, LCLType, Buttons, MisUtils, FormConfig,
  FrameCfgGeneral, CadDefinitions, frameCadView, FormProject,
  Globales, FrameExplorProyectos, FormControlVista, FormVistaProp,
  VisGraf3D, FrameComPanel;
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
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
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
    procedure acProInsPolylinExecute(Sender: TObject);
    procedure acProInsRectanExecute(Sender: TObject);
    procedure acProPropiedExecute(Sender: TObject);
    procedure acVerConVistaExecute(Sender: TObject);
    procedure acVerVisSupExecute(Sender: TObject);
    procedure acVisPropiedExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure acHerConfigExecute(Sender: TObject);
  private
    curProject  : TCadProject;
    ExpProyPag  : TCadPage;     //página seleccionada en el explorador de proyecto
    ExpProyVis  : TfraCadView;  //vista seleccionada en el explorador de proyecto
    panCommand  : TfraComPanel;
    procedure ConfigPropertiesChanged;
    procedure curProjectActivePagevistaSendMessage(msg: string);
    procedure curProjectActivePagevistaSendPrompt(msg: string);
    procedure curProject_Modific;
    procedure curProject_ChangeState(VisState: TVisStateTyp);
    procedure curProject_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
    procedure curProject_ChangeActivePage;
    procedure fraExplorProy_ClickDerPagina(pag: TCadPage);
    procedure fraExplorProy_ClickDerProyec(pro: TCadProject);
    procedure fraExplorProy_ClickDerVista(vis: TfraCadView);
    procedure curProject_ChangeView(vista: TfraCadView);
    function MensajeGuardarCambios: integer;
    procedure panCommandComKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure panCommandComReturn(txtPrompt, txtAnswer: string);
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

procedure TfrmPrincipal.fraExplorProy_ClickDerProyec(pro: TCadProject);
begin
  PopupProject.PopUp;
end;
procedure TfrmPrincipal.fraExplorProy_ClickDerPagina(pag: TCadPage);
begin
  ExpProyPag := pag;
  PopupPagina.PopUp;
end;
procedure TfrmPrincipal.fraExplorProy_ClickDerVista(vis: TfraCadView);
begin
  ExpProyVis := vis;
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

  //Panel de comandos
  panCommand  := TfraComPanel.Create(self);
  panCommand.Parent := self;
  panCommand.Align:=alBottom;
  panCommand.ErrorString := 'Error:';  //Para que pinte de rojo
  panCommand.OnComKeyDown := @panCommandComKeyDown;
  panCommand.OnComReturn := @panCommandComReturn;

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
procedure TfrmPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{Intercepta el teclado para administrarlo de acuerdo al control elegido}
begin
  if curProject = nil then exit;
  //Envía todos los comandos al cuadro de comandos
  if TabSheet1.Focused then begin
    panCommand.SetFocus;
  end else if PageControl1.Focused then begin
    panCommand.SetFocus;
  end;
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
procedure TfrmPrincipal.curProject_Modific;
//Llamado cuando el proyecto ha sido modificado.
begin
  acArcGuar.Enabled:=true;
end;
procedure TfrmPrincipal.curProject_ChangeState(VisState: TVisStateTyp);
begin
  StatusBar1.Panels[0].Text := curProject.ActivePage.view.StateAsStr;
end;
procedure TfrmPrincipal.curProject_MouseMoveVirt(Shift: TShiftState; xp,
  yp: Integer; xv, yv, zv: Single);
begin
  StatusBar1.Panels[3].Text :=
     'x=' + formatfloat('0.00', xv) + ' ' +
     'y=' + formatfloat('0.00', yv) + ' ' +
     'z=' + formatfloat('0.00', zv);
end;
procedure TfrmPrincipal.curProject_ChangeView(vista: TfraCadView);
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
  ap: TCadPage;
begin
  if curProject=nil then exit;
  //Enchufa el visor al PageControl1, para mostralo;
  curProject.HideAllPages;   {oculta primero todas las páginas porque puede que alguna
                              ya haya puesto su "Parent" en eset visor.}
  ap := curProject.ActivePage;
  ap.view.Parent := TabSheet1;   //Lo coloca aquí
  ap.view.Left:=Random(200);
  ap.view.Top:=Random(200);
  ap.view.Align := alClient;
  ap.view.Visible := true;  //lo hace visible
end;
function ComponentFromAction(Sender: Tobject): TComponent;
{Devuelve el componente que disparó una acción. Si no l oubica, devuelve NIL}
var
  compSource: TComponent;
begin
  if not (Sender is Taction) then exit(nil);
  compSource := TAction(Sender).ActionComponent;
  //Ya tenemos el componente fuente
  if compSource is TMenuItem then begin
    //Es un ítem de menú, pero ¿cuál?
    exit(TMenuItem(compSource).GetParentComponent)
  end else if compSource is TToolButton then begin
    //Es un botón de una barra de herramientas, pero ¿cuál?
    exit(TToolButton(compSource).GetParentComponent)
  end else begin
    //Es otra cosa
    exit(compSource)
  end;
end;
procedure TfrmPrincipal.panCommandComReturn(txtPrompt, txtAnswer: string);
begin
  if curProject = nil then exit;
//if txtAnswer = '' then exit;
  txtAnswer := UpCase(txtAnswer);  //Convierte a mayúscula
  //Comando introducido.
  curProject.ActivePage.view.ExecuteCommand(txtAnswer);
end;
procedure TfrmPrincipal.panCommandComKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lin: String;
begin
  if curProject = nil then exit;
  if key = VK_ESCAPE then begin
    //Covierte tecla en comando
    curProject.ActivePage.view.ExecuteCommand('CANCEL');
  end;
end;
procedure TfrmPrincipal.curProjectActivePagevistaSendMessage(msg: string);
{Ha llegado un mensaje del proyecto}
begin
  panCommand.AddLine(msg);
end;
procedure TfrmPrincipal.curProjectActivePagevistaSendPrompt(msg: string);
begin
  panCommand.AddPrompt(msg);
end;
///////////////////////////// Acciones ///////////////////////////////
procedure TfrmPrincipal.acArcNueProExecute(Sender: TObject);
var
  tmpPresp: TCadProject;
begin
  //verifica si hay que guardar cambios
  if MensajeGuardarCambios = BOT_CANCEL then exit;
  //Crea proyecto temporal
  tmpPresp := TCadProject.Create;
  if not frmProject.ExecNew(tmpPresp) then begin
    //se canceló
    tmpPresp.Destroy;  //no nos va a servir
    exit;  //sale dejando el proyecto actual
  end;
  //Cierra proyecto actual y asigna el temporal al actual
  acArcCerrarExecute(self);   //cierra actual si estaba abierto
  curProject := tmpPresp;  //apunta al temporal
  curProject.OnModific         :=@curProject_Modific;
  curProject.OnCambiaPerspec   :=@curProject_ChangeView;
  curProject.OnChangeActivePage:=@curProject_ChangeActivePage;
  curProject.OnMouseMoveVirt   :=@curProject_MouseMoveVirt;
  curProject.OnChangeState     :=@curProject_ChangeState;
  curProject.ActivePage.view.OnSendMessage:= @curProjectActivePagevistaSendMessage;
  curProject.ActivePage.view.OnSendPrompt := @curProjectActivePagevistaSendPrompt;
  curProject_ChangeActivePage;  //para refrescar en su visor
  curProject.ActivePage.view.InicVista;  //inicia los ejes
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
  if curProject = nil then exit;  //no hay proyecto abierto
  //verifica si hay proyecto modificado
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
  frmControlVista.Exec(curProject.ActivePage.view);
end;
procedure TfrmPrincipal.acVerVisSupExecute(Sender: TObject);
begin
  if curProject=nil then exit;
  curProject.ActivePage.view.Alfa:=0;
  curProject.ActivePage.view.Fi:=0;
  curProject.ActivePage.view.visEdi.Refresh;
end;
procedure TfrmPrincipal.acVisPropiedExecute(Sender: TObject);
begin
  frmVistaProp.Exec(ExpProyVis);
end;
procedure TfrmPrincipal.acProAgrPagExecute(Sender: TObject);
begin
  if curProject = nil then exit;  //no hay proyecto abierto
  curProject.AddPage;
  Refresh;
end;
procedure TfrmPrincipal.acProInsPolylinExecute(Sender: TObject);
begin

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
  pag: TCadPage;
begin
  if curProject=nil then exit;
  {Se verifica si la acción viene del explorador de proyecto, porque en ese caso, para
  darle la posibilidad de tomar acciones, sobre páginas no activas}
  if ComponentFromAction(Sender) = PopupPagina then pag := ExpProyPag
  else pag := curProject.ActivePage;
  pag.view.ExecuteCommand('LINE');
  Refrescar;
end;
procedure TfrmPrincipal.acProInsRectanExecute(Sender: TObject);
var
  pag: TCadPage;
begin
  if curProject=nil then exit;
  {Se verifica si la acción viene del explorador de proyecto, porque en ese caso, para
  darle la posibilidad de tomar acciones, sobre páginas no activas}
  if ComponentFromAction(Sender) = PopupPagina then pag := ExpProyPag
  else pag := curProject.ActivePage;
  pag.view.ExecuteCommand('RECTANGLE');
  Refrescar;
end;
procedure TfrmPrincipal.acPagElimExecute(Sender: TObject);
begin
  if curProject = nil then exit;
  if MsgYesNo('¿Eliminar página "%s"?', [curProject.ActivePage.name]) <> 1 then exit;
  curProject.RemovePage(curProject.ActivePage);
  Refrescar;
end;

procedure TfrmPrincipal.acHerConfigExecute(Sender: TObject);
begin
  Config.Configurar();
end;

end.

