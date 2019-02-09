unit CadDefinitions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, MisUtils, DXFya, Graphics, LCLProc,
  frameCadView, DefObjGraf, ObjGraficos, EditionMot3D;
type
  TCadUnits = (
    tmuMeters,
    tmuFeet
  );

  TEveChangePerspec = procedure(vista: TfraCadView) of object;
  TCadProject = class;

  { TCadPagina }
  {Representa a una página de un proyecto.}
  TCadPage = class
  private
    procedure vistaCambiaPerspec;
    procedure vistaChangeState(VisState: TVisStateTyp);
    procedure vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
  public
    name       : string;
    parent     : TCadProject;     //Referencia al objeto padre.
    objects    : TObjGraphList;   //Lista de objetos
  public  //Manejo de las vistas
    view       : TfraCadView;     //Una sola vista por el momento
    OnCambiaPerspec: TEveChangePerspec; //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState  : TEvChangeState;
  public  //Inicialización
    constructor Create;
    destructor Destroy; override;
  end;
  TCadPagina_list = specialize TFPGObjectList<TCadPage>;

{ TCadProject }

  TCadProject = class
  private
    FActivePage: TCadPage;
    fModific  : boolean;   //indica si ha sido modificado
    dxfFile: TDXFfile;
    procedure pag_ChangeState(VisState: TVisStateTyp);
    procedure pag_CambiaPerspec(vista: TfraCadView);
    procedure pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
    procedure SetActivePage(AValue: TCadPage);
    procedure SetModific(AValue: boolean);
  public  //Información del proyecto.
    name     : string;
    author   : string;
    notes    : string;
    units    : TCadUnits;
  public
    OnModific : procedure of object; //Proyecto modificado
    OnCambiaPerspec: TEveChangePerspec;  //Cambia x_des, y_des, x_cam, alfa, ...
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState: TEvChangeState;
    property Modific: boolean read fModific write SetModific;
    procedure LoadFile;
    procedure LoadDXFFile(fileName: string);
    procedure SaveFile;
  public  //Campos de página
    pages  : TCadPagina_list; {Lista de páginas. Debe contener al menos una.}
    OnChangeActivePage: procedure of object;
    Property ActivePage: TCadPage read FActivePage write SetActivePage;
    function IndexOfPage(pag: TCadPage): integer;
    function PrevPage(pag: TCadPage): TCadPage;
    function NextPage(pag: TCadPage): TCadPage;
    function PageByName(pagName: string): TCadPage;
    procedure SetActivePageByName(pagName: string);
    function AddPage: TCadPage;
    procedure RemovePage(pagName: TCadPage);
    procedure RemovePage(pagName: string);
    procedure HideAllPages;
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;

  TCadProjectPtr = ^TCadProject;

implementation

procedure TCadPage.vistaCambiaPerspec;
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec(self.view);   //identifica a la página
end;
procedure TCadPage.vistaChangeState(VisState: TVisStateTyp);
begin
  if OnChangeState<>nil then OnChangeState(VisState);
end;
procedure TCadPage.vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, 0);
end;
constructor TCadPage.Create;
begin
  objects := TObjGraphList.Create(true);   //contenedor
  view:= TfraCadView.Create(nil, objects);  //crea una vista

//  vista.Parent := TabSheet1;
//  vista.Visible:=true;
//  vista.Align:=alClient;
  view.ediMot.v2d.backColor:=clBlack;
  view.ediMot.VerEjesCoor:=true;
  view.ediMot.VerPuntoGiro:=true;
  view.ediMot.VerCuadric:=true;
//  vista.VisEdiGraf.OnChangeView:=@fraMotEdicionmotEdiChangeView;
  view.OnCambiaPerspec:=@vistaCambiaPerspec;
  view.OnMouseMoveVirt:=@vistaMouseMoveVirt;
  view.OnChangeState := @vistaChangeState;


end;
destructor TCadPage.Destroy;
begin
  view.Destroy;
  objects.Destroy;
  inherited Destroy;
end;
{ TCadProject }
procedure TCadProject.SetModific(AValue: boolean);
begin
  if fModific=AValue then Exit;
  fModific:=AValue;
  if FModific then begin
    if OnModific<>nil then OnModific;  //evento
  end;
end;
procedure TCadProject.LoadFile;
begin

end;
procedure TCadProject.LoadDXFFile(fileName: string);
{Carga un archivo DXF}
var
  ent: TDXFentitie;
  lin: TGoEntityLine;
  pol: TGoEntityPolyline;
  ver: TGoEntity;
  i: Integer;
begin
  dxfFile.ReadFromFile(fileName);
  if dxfFile.Er<>'' then MsgErr(dxfFile.Er);
  //Deja solo una página
  while pages.Count>1 do begin
    RemovePage(pages[0]);
  end;
  //Limpia página actual
  ActivePage.view.RemoveAllObjects;
  //Crea objetos para representar a las entidades
  for ent in dxfFile.entities do begin
    case ent.etype of
    etyLine: begin
//        v2d.Linea(x + ent.x0, y + ent.y0,
//                  x + ent.x1, y + ent.y1);

        lin := TGoEntityLine.Create(ActivePage.view.ediMot.v2d);
        lin.etype := ent.etype;
        lin.SetP0(ent.x0, ent.y0, ent.z0);
        lin.SetP1(ent.x1, ent.y1, ent.z1);
        ActivePage.view.ediMot.AddObjGraph(lin);
        ActivePage.view.ediMot.Refresh;
      end;
    etyPolyline: begin
        pol := TGoEntityPolyline.Create(ActivePage.view.ediMot.v2d);
        pol.etype := ent.etype;
        pol.SetP0(ent.x0, ent.y0, ent.z0);
        pol.SetP1(ent.x1, ent.y1, ent.z1);
        pol.vertexs := TObjGrafDXF_list.Create(true);
        pol.polyFlag := ent.polyFlag;
        for i := 0 to ent.vertexs.Count-1 do begin
          ver := TGoEntity.Create(ActivePage.view.ediMot.v2d);
//          ver.Locate(ent.vertexs[i].x0,
//                     ent.vertexs[i].y0,
//                     ent.vertexs[i].z0);
          ver.SetP0(ent.vertexs[i].x0,ent.vertexs[i].y0,ent.vertexs[i].z0);
          ver.SetP1(ent.vertexs[i].x1,ent.vertexs[i].y1,ent.vertexs[i].z1);
//debugln('P0=%f,%f,%f', [ver.P0.x, ver.P0.y, ver.P0.x]);
//debugln('P1=%f,%f,%f', [ver.P1.x, ver.P1.y, ver.P1.x]);
          pol.vertexs.Add(ver);
        end;
        ActivePage.view.ediMot.AddObjGraph(pol);
        ActivePage.view.ediMot.Refresh;
      end;
    end;
  end;
end;
procedure TCadProject.SaveFile;
begin

end;
procedure TCadProject.pag_CambiaPerspec(vista: TfraCadView);
{Se genera si alguna página cambia su perspectiva}
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec(vista);
end;
procedure TCadProject.pag_ChangeState(VisState: TVisStateTyp);
begin
  if OnChangeState<>nil then OnChangeState(VisState);
end;
procedure TCadProject.pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, zv);
end;
//Campos de página
procedure TCadProject.SetActivePage(AValue: TCadPage);
var
  pag: TCadPage;
begin
  if FActivePage=AValue then Exit;
  //Verifica si la página solicitada, existe
  for pag in pages do begin
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
function TCadProject.IndexOfPage(pag: TCadPage): integer;
{Devuelve el índice de una página dentro de la lista de páginas. Si no ubica a la página
 devuelve -1.}
var
  i: integer;
begin
  for i:=0 to pages.Count-1 do begin
    if pages[i] = pag then exit(i);
  end;
  //No encontró
  exit(-1);
end;
function TCadProject.PrevPage(pag: TCadPage): TCadPage;
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
    exit(pages[i-1]); //devuelve anterior
  end;
end;
function TCadProject.NextPage(pag: TCadPage): TCadPage;
{Devuelve la página siguiente a una indicada. Si es la última, devuelve la misma
 página. Si hay error, devuelve NIL.}
var
  i: Integer;
begin
  i := IndexOfPage(pag);
  if i=-1 then exit(niL);
  if i=pages.Count-1 then begin  //no hay siguiente, devuelve la misma
    exit(pag);
  end else begin
    exit(pages[i+1]); //devuelve siguiente
  end;
end;
function TCadProject.PageByName(pagName: string): TCadPage;
{Devuelve la referencia a una página, dado su nombre. Si no encuentra la página,
devuelve NIL.}
var
  pag: TCadPage;
begin
  for pag in pages do begin
    if pag.name = pagName then exit(pag);
  end;
  exit(nil);
end;
procedure TCadProject.SetActivePageByName(pagName: string);
var
  pag: TCadPage;
begin
  for pag in pages do begin
    if pag.name = pagName then begin
      ActivePage := pag;
      exit;
    end;
  end;
  //No existe
end;
function TCadProject.AddPage: TCadPage;
{Agrega una página al proyecto. Devuelve la referecnia a la pa´gina creada.}
var
  pag: TCadPage;
begin
  pag := TCadPage.Create;
  pag.name:='Página'+IntToStr(pages.Count+1);
  pag.parent := self;
  pag.OnCambiaPerspec:=@pag_CambiaPerspec;
  pag.OnMouseMoveVirt:=@pag_MouseMoveVirt;
  pag.OnChangeState:=@pag_ChangeState;
  pages.Add(pag);
  Modific:=true;   //es un cambio
  Result := pag;
end;
procedure TCadProject.RemovePage(pagName: TCadPage);
{Elimina la página indicada.}
begin
  if pages.Count=1 then begin
    MsgExc('No se pueden eliminar todas las páginas.');
    exit;
  end;
  if ActivePage = pagName then begin
    //Se está borrando la página activa, hay que moverla
    if pagName = pages.First then   //es la primera
      ActivePage := NextPage(pagName)  //pasa a la siguiente
    else
      ActivePage := PrevPage(pagName);  //pasa a la anetrior
  end;
  pages.Remove(pagName);
  Modific:=true;   //es un cambio
end;
procedure TCadProject.RemovePage(pagName: string);
var
  pag: TCadPage;
begin
  for pag in pages do begin
    if pag.name = pagName then begin
      RemovePage(pag);
      exit;
    end;
  end;
  //No encontró
  MsgExc('No existe la página: "%s"', [pagName]);
end;
procedure TCadProject.HideAllPages;
{Pone las vistas de todas las páginas en visible := FALSE, de modo que no se mostrarán
en el control asignado.}
var
  pag: TCadPage;
begin
  for pag in pages do begin
    pag.view.Visible:=false;
  end;
end;
//Iniicialización
constructor TCadProject.Create;
var
  pag: TCadPage;
begin
  dxfFile:= TDXFfile.Create;
  pages:= TCadPagina_list.Create(true);
  //Crea una página
  pag := AddPage;
  ActivePage := pag;   //la pone como activa por defecto
end;
destructor TCadProject.Destroy;
begin
  pages.Destroy;
  dxfFile.Destroy;
  inherited Destroy;
end;

end.

