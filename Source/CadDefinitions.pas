unit CadDefinitions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl,
  frameVisorGraf, Graphics, MisUtils;   //Parra poder incluir al visor.
type

  Tunidades = (
    tmuMetros,
    tmuPies
  );

  //Tipo de tablero
  TDipTipTab = (
    ttAutosoport,  //Autosoportado
    ttEmpotrado,   //Empotrado
    ttAdosado      //Adosado
  );

  TCadObjetos = class
    nombre: string;
    tipTab: TDipTipTab; //tipo de tablero
  end;
  TCadObjetos_list = specialize TFPGObjectList<TCadObjetos>;

  TEveCambiaPerspec = procedure(vista: TfraVisorGraf) of object;
  TCadProyecto = class;

  { TCadPagina }
  TCadPagina = class
  private
    procedure vistaCambiaPerspec;
  public
    nombre     : string;
    objetosGraf: TCadObjetos_list;  //Lista de elementos gráficos
    padre      : TCadProyecto;      //Referencia al objeto padre.
  public  //Manejo de las vistas
    vista: TfraVisorGraf;   //una sola vista por el momento
    OnCambiaPerspec: TEveCambiaPerspec;  //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    //    visores     : TCadVisor
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;
  TCadPagina_list = specialize TFPGObjectList<TCadPagina>;

{ TCadProyecto }
  TCadProyecto = class
  private
    FActivePage: TCadPagina;
    fModific  : boolean;   //indica si ha sido modificado
    procedure pagCambiaPerspec(vista: TfraVisorGraf);
    procedure SetActivePage(AValue: TCadPagina);
    procedure SetModific(AValue: boolean);
  public
    nombre : string;
    creadoPor: string;
    notas    : string;
    unidades : Tunidades;
    OnModific : procedure of object; //Proyecto modificado
    OnCambiaPerspec: TEveCambiaPerspec;  //Cambia x_des, y_des, x_cam, alfa, ...
    property Modific: boolean read fModific write SetModific;
    procedure GuardarArchivo;
  public  //Campos de página
    paginas: TCadPagina_list; {Lista de páginas. Debe contener al menos una.}
    OnChangeActivePage: procedure of object;
    Property ActivePage: TCadPagina read FActivePage write SetActivePage;
    function IndexOfPage(pag: TCadPagina): integer;
    function PrevPage(pag: TCadPagina): TCadPagina;
    function NextPage(pag: TCadPagina): TCadPagina;
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
constructor TCadPagina.Create;
begin
  objetosGraf := TCadObjetos_list.Create(true);
  vista:= TfraVisorGraf.Create(nil);  //crea una vista

//  vista.Parent := TabSheet1;
//  vista.Visible:=true;
//  vista.Align:=alClient;
  vista.motEdi.v2d.backColor:=clBlack;
//  vista.motEdi.OnChangeView:=@fraMotEdicionmotEdiChangeView;
  vista.OnCambiaPerspec:=@vistaCambiaPerspec;
  vista.AgregaObjeto;
  vista.AgregaObjeto;

end;
destructor TCadPagina.Destroy;
begin
  vista.Destroy;
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
procedure TCadProyecto.pagCambiaPerspec(vista: TfraVisorGraf);
{Se genera si alguna página cambia su perspectiva}
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec(vista);
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
  pag.OnCambiaPerspec:=@pagCambiaPerspec;
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

