{Frame Explorador de Proyectos.
Este frame está pensado para interactuar con un poryecto TCadProyectoPtr.
Permite explorar sus elementos. Además genera eevntos cuando por ejemplo se ahce click
derecho en una pa´gina.
La idea es que este explroador no interactúe para nada con el visor del proyecto, ya que
el visor debe ser independiente del explorador de proyecto.
Sin embargo, este explorador, si puede cambiar la página activa del proyecto.}
unit FrameExplorProyectos;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, LCLType, ActnList,
  CadDefinitions, frameVisorGraf, DefObjGraf;
type
  TEvClickDerPro = procedure(pro: TCadProyecto) of object;
  TEvClickDerPag = procedure(pag: TCadPagina) of object;
  TEvClickDerObj = procedure(obj: TCadObjetos_list) of object;
  TEvClickDerVis = procedure(vis: TfraVisorGraf) of object;

  { TfraExplorProyectos }

  TfraExplorProyectos = class(TFrame)
    arbNaveg: TTreeView;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    Label2: TLabel;
    procedure arbNavegKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    curProject: TCadProyectoPtr;
    function NodoEsObjetos(nod: TTreeNode): boolean;
    function NodoEsVista(nod: TTreeNode): boolean;
    function NodoObjetosSelec: TTreeNode;
    function NodoSelec: TTreeNode;
    function NombreNodo(nod: TTreeNode): string;
    function NombreNodoSelec: string;
    procedure SeleccNodo(nom: string);
    //Identificación de nodos seleccionados
    function NodoEsPagina(nod: TTreeNode): boolean;
    function NodoProyecSelec: TTreeNode;
    function NodoPaginaSelec: TTreeNode;
    function NodoVistaSelec: TTreeNode;
    //Eventos del árbol
    procedure arbNavegMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure arbNavegSelectionChanged(Sender: TObject);
  public  //Eventos
    OnClickDerProyec : TEvClickDerPro;
    OnClickDerPagina : TEvClickDerPag;
    OnClickDerVista  : TEvClickDerVis;
    OnClickDerObjetos: TEvClickDerObj;
    OnBorrarPagina   : TNotifyEvent;
    procedure Refrescar;
  public  //Inicialización
    procedure Iniciar(proj: TCadProyectoPtr);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
//  MSJE_SIN_ELEM = '<Sin elementos>';
  IMIDX_PROYEC = 0;   //ïndice de ícon de proyecto
  IMIDX_PAGINA = 1;   //ïndice de ícon de página
  IMIDX_VISTA  = 2;
  IMIDX_OBJGRA = 3;

  { TfraExplorProyectos }
procedure TfraExplorProyectos.arbNavegKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    if OnBorrarPagina<>nil then OnBorrarPagina(self);
  end;
end;
function TfraExplorProyectos.NodoSelec: TTreeNode;
{Devuelve el nodo seleccionado actualmente. }
begin
  if curProject^ = nil then exit(nil);
  Result := arbNaveg.Selected;
end;
function TfraExplorProyectos.NombreNodo(nod: TTreeNode): string;
{Devuelve el nombre de un nodo. }
begin
  Result := nod.Text;
end;
function TfraExplorProyectos.NombreNodoSelec: string;
{Devuelve el nombre del nodo seleccionado.}
begin
  if arbNaveg.Selected = nil then exit('');
  Result := NombreNodo(arbNaveg.Selected);
end;
procedure TfraExplorProyectos.SeleccNodo(nom: string);
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
//Identificación de nodos seleccionados
function TfraExplorProyectos.NodoEsPagina(nod: TTreeNode): boolean;
{Indica si el nodo seleccionado corresponde a un tablero}
begin
  if nod=nil then exit(false);
  Result := (nod.Level = 1) and (nod.ImageIndex = IMIDX_PAGINA);
end;
function TfraExplorProyectos.NodoEsVista(nod: TTreeNode): boolean;
{Indica si el nodo seleccionado corresponde a un tablero}
begin
  if nod=nil then exit(false);
  Result := (nod.Level = 2) and (nod.ImageIndex = IMIDX_VISTA);
end;
function TfraExplorProyectos.NodoEsObjetos(nod: TTreeNode): boolean;
{Indica si el nodo seleccionado corresponde a un tablero}
begin
  if nod=nil then exit(false);
  Result := (nod.Level = 2) and (nod.ImageIndex = IMIDX_OBJGRA);
end;
function TfraExplorProyectos.NodoProyecSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Level = 0 then exit(NodoSelec);
  exit(nil);
end;
function TfraExplorProyectos.NodoPaginaSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Visible = false then exit(nil);
  if NodoEsPagina(NodoSelec) then
    exit(NodoSelec);
  exit(nil);
end;
function TfraExplorProyectos.NodoObjetosSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Visible = false then exit(nil);
  if NodoEsPagina(NodoSelec) then
    exit(NodoSelec);
  exit(nil);
end;
function TfraExplorProyectos.NodoVistaSelec: TTreeNode;
begin
  if NodoSelec=nil then exit(nil);
  if NodoSelec.Visible = false then exit(nil);
  if NodoEsVista(NodoSelec) then
    exit(NodoSelec);
  exit(nil);
end;
//Eventos del árbol
procedure TfraExplorProyectos.arbNavegMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  nod: TTreeNode;
  pag: TCadPagina;
begin
  nod := arbNaveg.GetNodeAt(x, y);
  if nod = nil then begin
    //se marcón fuera de un nodo
    arbNaveg.Selected := nil;
  end;
  //Genera eventos de acuerdo al nodo seleccionado
  if Button = mbRight then begin
    if arbNaveg.GetNodeAt(X,Y)<>nil then arbNaveg.GetNodeAt(X,Y).Selected:=true;
    if NodoProyecSelec<>nil then begin       //proyecto seleccionado
      if OnClickDerProyec<>nil then
        OnClickDerProyec(curProject^);
    end else if NodoPaginaSelec<>nil then begin  //página seleccionada
      pag := curProject^.PageByName(NombreNodoSelec);
      if OnClickDerPagina<>nil then
        OnClickDerPagina(pag);
    end else if NodoVistaSelec<>nil then begin
      pag := curProject^.PageByName(NodoSelec.Parent.Text);
      if OnClickDerVista<>nil then
        OnClickDerVista(pag.vista);
    end else if NodoObjetosSelec<>nil then begin
      pag := curProject^.PageByName(NodoSelec.Parent.Text);
      if OnClickDerObjetos<>nil then
        OnClickDerObjetos(pag.objetosGraf);
    end;
  end;
end;
procedure TfraExplorProyectos.arbNavegSelectionChanged(Sender: TObject);
{El elemento seleccionado ha cambiado. Tomar las acciones correspondientes.}
begin
  if NodoPaginaSelec<>nil then begin
    //Se ha seleccionado una página, cambia la página activa
    curProject^.SetActivePageByName(NodoPaginaSelec.Text);
  end;
end;
procedure TfraExplorProyectos.Refrescar;
{Refresca la apariencia del frame, de acuerdo al proyecto actual.}
var
  pag: TCadPagina;
  nodPag: TTreeNode;
  nodProj: TTreeNode;
  nodGeomet, nodVista, nodObjGraf: TTreeNode;
  og : TObjGraf;
begin
  //muestra su título
  Label2.Caption:=self.Caption;

//  ns := NombreNodoSelec;  //guarda elemento seleccionado
  arbNaveg.Items.Clear;  //limpia elementos
  if curProject^ = nil then begin
    //No hay proyecto actual
    nodProj := arbNaveg.items.AddChild(nil, '<<Sin Proyectos>>');
    exit;
  end;
  //Hay un proyecto abierto
  //Agrega nodo de proyecto
  nodProj := arbNaveg.items.AddChild(nil, curProject^.nombre);  //agrega proyecto actual
  nodProj.ImageIndex    := IMIDX_PROYEC;
  nodProj.SelectedIndex :=IMIDX_PROYEC;
  //Agrega nodo de las páginas
  arbNaveg.BeginUpdate;
  for pag in curProject^.paginas do begin
     nodPag := arbNaveg.Items.AddChild(nodProj, pag.nombre);
     nodPag.ImageIndex   := IMIDX_PAGINA;
     nodPag.SelectedIndex:= IMIDX_PAGINA;
     //Agrega campos de objetos gráficos
     nodGeomet := arbNaveg.items.AddChild(nodPag, 'Objetos Gráficos');
     nodGeomet.ImageIndex   := IMIDX_OBJGRA;
     nodGeomet.SelectedIndex:= IMIDX_OBJGRA;

     for og in pag.vista.objetos do begin
       nodObjGraf := arbNaveg.items.AddChild(nodGeomet, 'Objeto');
     end;
     //Agrega campo de vista
     nodVista := arbNaveg.items.AddChild(nodPag, 'Vista');
     nodVista.ImageIndex   := IMIDX_VISTA;
     nodVista.SelectedIndex:= IMIDX_VISTA;

     nodPag.Expanded:=true;
  end;
  arbNaveg.EndUpdate;
   //Hay ítems (tableros u otros), carga tableros
  nodProj.Expanded:=true;   //lo deja expandido
//  SeleccNodo(ns);  //restaura la selección
  //selecciona el nodo activo
  SeleccNodo(curProject^.ActivePage.nombre);
end;
procedure TfraExplorProyectos.Iniciar(proj: TCadProyectoPtr);
{Inicia el frame, pasándole la dirección del proyecto actual que se está manejando.
Por el momento se asume que solo puede manejar cero o un proyecto.
Notar que se le pasa la dirección de la referecnia, ya que esta puede cambiar. }
begin
  curProject := proj;   //guarda referecnia
  Refrescar;
end;
constructor TfraExplorProyectos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  arbNaveg.OnMouseUp:=@arbNavegMouseUp;
  arbNaveg.OnSelectionChanged:=@arbNavegSelectionChanged;
end;

destructor TfraExplorProyectos.Destroy;
begin

  inherited Destroy;
end;


end.

