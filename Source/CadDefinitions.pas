unit CadDefinitions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, MisUtils,
  frameVisorGraf, DefObjGraf, ObjGraficos, Graphics;
type
  {Usado solo para pasar parámetros geométricos.
   Podría usarse el tipo TMotPoint, de MoTGraf3D, pero se quiere evitar una dependencia
   más, y además, el formato de datos de TMotPoint, tiene más campos, ya que está
   orientado al procesamiento gráfico}
  TPoint3 = record   //representa a un punto virtual
    x,y,z : Single;
  end;

  Tunidades = (
    tmuMetros,
    tmuPies
  );

  //Tipo de objeto gráfico. Se sigue el estándar de Autocad
  TCadTipObjGraf = (
    etyLine      //línea
   ,etyCircle    //círculo
   ,etyPolyline  //polilínea
   ,etyInsert    //bloque
  );

  TCadObjGraf = class
    nombre: string;
    tipTab: TCadTipObjGraf;
  end;
  TCadObjetos_list = specialize TFPGObjectList<TCadObjGraf>;

  TEveCambiaPerspec = procedure(vista: TfraVisorGraf) of object;
  TCadProyecto = class;

  { TCadPagina }
  TCadPagina = class
  private
    procedure vistaCambiaPerspec;
    procedure vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
  public
    nombre     : string;
    padre      : TCadProyecto;      //Referencia al objeto padre.
    objetosGraf: TCadObjetos_list;  //Lista de elementos gráficos
    objetos : TlistObjGraf; //Lista de objetos
    procedure AddLine(const p1, p2: TPoint3);
  public  //Manejo de las vistas
    vista: TfraVisorGraf;   //una sola vista por el momento
    OnCambiaPerspec: TEveCambiaPerspec;  //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
  public  //Inicialización
    constructor Create;
    destructor Destroy; override;
  end;
  TCadPagina_list = specialize TFPGObjectList<TCadPagina>;

{ TCadProyecto }
  TCadProyecto = class
  private
    FActivePage: TCadPagina;
    fModific  : boolean;   //indica si ha sido modificado
    procedure pag_CambiaPerspec(vista: TfraVisorGraf);
    procedure pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
    procedure SetActivePage(AValue: TCadPagina);
    procedure SetModific(AValue: boolean);
  public
    nombre : string;
    creadoPor: string;
    notas    : string;
    unidades : Tunidades;
    OnModific : procedure of object; //Proyecto modificado
    OnCambiaPerspec: TEveCambiaPerspec;  //Cambia x_des, y_des, x_cam, alfa, ...
    OnMouseMoveVirt: TEveMouseVisGraf;
    property Modific: boolean read fModific write SetModific;
    procedure GuardarArchivo;
  public  //Campos de página
    paginas: TCadPagina_list; {Lista de páginas. Debe contener al menos una.}
    OnChangeActivePage: procedure of object;
    Property ActivePage: TCadPagina read FActivePage write SetActivePage;
    function IndexOfPage(pag: TCadPagina): integer;
    function PrevPage(pag: TCadPagina): TCadPagina;
    function NextPage(pag: TCadPagina): TCadPagina;
    function PageByName(pagName: string): TCadPagina;
    procedure SetActivePageByName(pagName: string);
    function AddPage: TCadPagina;
    procedure RemovePage(pagName: TCadPagina);
    procedure RemovePage(name: string);
    procedure HideAllPages;
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;

  TCadProyectoPtr = ^TCadProyecto;

implementation

procedure TCadPagina.vistaCambiaPerspec;
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec(self.vista);   //identifica a la página
end;

procedure TCadPagina.vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, 0);
end;

procedure TCadPagina.AddLine(const p1, p2: TPoint3);
{Agrega una línea a la lista de objetos.}
var
  lin : TObjGrafDXF;
begin
  lin := TObjGrafDXF.Create(vista.visEdi.v2d);
  lin.P0.x := p1.x;
  lin.P0.y := p1.y;
  lin.P0.z := p1.z;

  lin.P1.x:=p2.x;
  lin.P1.y:=p2.y;
  lin.P1.z:=p2.z;

  lin.Ubicar(p1.x, p1.y);

  vista.AgregarObjGrafico(lin);
end;
constructor TCadPagina.Create;
var
  og: TMiObjeto;
begin
  objetosGraf := TCadObjetos_list.Create(true);
  objetos := TlistObjGraf.Create(true);   //contenedor
  vista:= TfraVisorGraf.Create(nil, objetos);  //crea una vista

//  vista.Parent := TabSheet1;
//  vista.Visible:=true;
//  vista.Align:=alClient;
  vista.visEdi.v2d.backColor:=clBlack;
  vista.visEdi.VerEjesCoor:=true;
  vista.visEdi.VerPuntoGiro:=true;
  vista.visEdi.VerCuadric:=true;
//  vista.VisEdiGraf.OnChangeView:=@fraMotEdicionmotEdiChangeView;
  vista.OnCambiaPerspec:=@vistaCambiaPerspec;
  vista.OnMouseMoveVirt:=@vistaMouseMoveVirt;

og := TMiObjeto.Create(vista.visEdi.v2d);
vista.AgregarObjGrafico(og);

end;
destructor TCadPagina.Destroy;
begin
  vista.Destroy;
  objetos.Destroy;
  objetosGraf.Destroy;
  inherited Destroy;
end;
{ TCadProyecto }
procedure TCadProyecto.SetModific(AValue: boolean);
begin
  if fModific=AValue then Exit;
  fModific:=AValue;
  if FModific then begin
    if OnModific<>nil then OnModific;  //evento
  end;
end;
procedure TCadProyecto.pag_CambiaPerspec(vista: TfraVisorGraf);
{Se genera si alguna página cambia su perspectiva}
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec(vista);
end;
procedure TCadProyecto.pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, zv);
end;
procedure TCadProyecto.GuardarArchivo;
begin

end;
//Campos de página
procedure TCadProyecto.SetActivePage(AValue: TCadPagina);
var
  pag: TCadPagina;
begin
  if FActivePage=AValue then Exit;
  //Verifica si la página solicitada, existe
  for pag in paginas do begin
    if pag = AValue then begin
      //Existe
      FActivePage:=AValue;
//      Modific := true;  //Cambiar de página es un cambio
      if OnChangeActivePage<>nil then OnChangeActivePage;
      exit;
    end;
  end;
  //No existe
end;
function TCadProyecto.IndexOfPage(pag: TCadPagina): integer;
{Devuelve el índice de una página dentro de la lista de páginas. Si no ubica a la página
 devuelve -1.}
var
  i: integer;
begin
  for i:=0 to paginas.Count-1 do begin
    if paginas[i] = pag then exit(i);
  end;
  //No encontró
  exit(-1);
end;
function TCadProyecto.PrevPage(pag: TCadPagina): TCadPagina;
{Devuelve la página anterior a una indicada. Si es la primera, devuelve la misma
 página. Si hay error, devuelve NIL.}
var
  i: integer;
begin
  i := IndexOfPage(pag);
  if i=-1 then exit(niL);
  if i=0 then begin  //no hay anterior, devuelve la misma
    exit(pag);
  end else begin
    exit(paginas[i-1]); //devuelve anterior
  end;
end;
function TCadProyecto.NextPage(pag: TCadPagina): TCadPagina;
{Devuelve la página siguiente a una indicada. Si es la última, devuelve la misma
 página. Si hay error, devuelve NIL.}
var
  i: Integer;
begin
  i := IndexOfPage(pag);
  if i=-1 then exit(niL);
  if i=paginas.Count-1 then begin  //no hay siguiente, devuelve la misma
    exit(pag);
  end else begin
    exit(paginas[i+1]); //devuelve siguiente
  end;
end;
function TCadProyecto.PageByName(pagName: string): TCadPagina;
{Devuelve la referencia a una página, dado su nombre. Si no encuentra la página,
devuelve NIL.}
var
  pag: TCadPagina;
begin
  for pag in paginas do begin
    if pag.nombre = pagName then exit(pag);
  end;
  exit(nil);
end;
procedure TCadProyecto.SetActivePageByName(pagName: string);
var
  pag: TCadPagina;
begin
  for pag in paginas do begin
    if pag.nombre = pagName then begin
      ActivePage := pag;
      exit;
    end;
  end;
  //No existe
end;
function TCadProyecto.AddPage: TCadPagina;
{Agrega una página al proyecto. Devuelve la referecnia a la pa´gina creada.}
var
  pag: TCadPagina;
begin
  pag := TCadPagina.Create;
  pag.nombre:='Página'+IntToStr(paginas.Count+1);
  pag.padre := self;
  pag.OnCambiaPerspec:=@pag_CambiaPerspec;
  pag.OnMouseMoveVirt:=@pag_MouseMoveVirt;
  paginas.Add(pag);
  Modific:=true;   //es un cambio
  Result := pag;
end;
procedure TCadProyecto.RemovePage(pagName: TCadPagina);
{Elimina la página indicada.}
begin
  if paginas.Count=1 then begin
    MsgExc('No se pueden eliminar todas las páginas.');
    exit;
  end;
  if ActivePage = pagName then begin
    //Se está borrando la página activa, hay que moverla
    if pagName = paginas.First then   //es la primera
      ActivePage := NextPage(pagName)  //pasa a la siguiente
    else
      ActivePage := PrevPage(pagName);  //pasa a la anetrior
  end;
  paginas.Remove(pagName);
  Modific:=true;   //es un cambio
end;
procedure TCadProyecto.RemovePage(name: string);
var
  pag: TCadPagina;
begin
  for pag in paginas do begin
    if pag.nombre = name then begin
      RemovePage(pag);
      exit;
    end;
  end;
  //No encontró
  MsgExc('No existe la página: "%s"', [name]);
end;
procedure TCadProyecto.HideAllPages;
{Pone las vistas de todas las páginas en visible := FALSE, de modo que no se mostrarán
en el control asignado.}
var
  pag: TCadPagina;
begin
  for pag in paginas do begin
    pag.vista.Visible:=false;
  end;
end;

//Iniicialización
constructor TCadProyecto.Create;
var
  pag: TCadPagina;
begin
  paginas:= TCadPagina_list.Create(true);
  //Crea una página
  pag := AddPage;
  ActivePage := pag;   //la pone como activa por defecto
end;
destructor TCadProyecto.Destroy;
begin
  paginas.Destroy;
  inherited Destroy;
end;

end.

