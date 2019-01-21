{Define los objetos gráficos con los que trabajará nuestra aplicación.
 Todos ellos deben descender de TObjGraf, para que puedadn ser tratados por el motor
 de edición "ogMotEdicion".}

unit ObjGraficos;
{$mode objfpc}{$H+}
interface
uses
  Classes, Graphics, LCLProc, fgl, DXFya,
  MotGraf3d, DefObjGraf;

type

TObjGrafDXF = class;
TObjGrafDXF_list = specialize TFPGObjectList<TObjGrafDXF>;

{ TObjGrafDXF }
{Se define al objeto para que sea compatible con archivos DXF.}
TObjGrafDXF = class(TObjGraph)  //objeto gráfico DXF
private
  pc0, pc1, pcM: TPtoCtrl;
  procedure PtoCtrl0_Move(xvTar, yvTar, dx, dy: Single);
  procedure PtoCtrl1_Move(xvTar, yvTar, dx, dy: Single);
  procedure PtoCtrlM_Move(xvTar, yvTar, dxv, dyv: Single);
  function SelectRect(const P0, P1: TMotPoint; xp, yp: integer): boolean;
public  //Campos equivalentes a los de una entidad DXF
  etype : TDXFentType;   //Tipo de entidad
  idDxf : string;        //Identificador de la entidad
  layer : string;
  color : string;
  style : string;
  isComplex: boolean;
  polyFlag: integer; {Bandera para polilíneas. Mapa de bits, cero por defecto:
    1 = This is a closed polyline (or a polygon mesh closed in the M direction).
    2 = Curve-fit vertices have been added.
    4 = Spline-fit vertices have been added.
    8 = This is a 3D polyline.
    16 = This is a 3D polygon mesh.
    32 = The polygon mesh is closed in the N direction.
    64 = The polyline is a polyface mesh.
    128 = The linetype pattern is generated continuously around the vertices of this polyline.}
  //Propiedades gráficas
  P0    : TMotPoint;
  P1    : TMotPoint;
  radius: double;
  vertexs: TObjGrafDXF_list;   {Lista de Vertex. Solo se instancia para objetos
                               complejos. OJO!!! Es muy pesado guardar una lista de
                               TObjGrafDXF. Debería optimizarse}
  blkName: string;    //usado cuando es de tipo etyInsert.
public
  procedure SetP0(const xv,yv,zv: Single);
  procedure SetP1(const xv,yv,zv: Single);
  procedure ReubicElemen; override;
public
  procedure Draw; override;  //Dibuja el objeto gráfico
  function LoSelecciona(xp, yp:integer): Boolean; override;
  constructor Create(mGraf: TMotGraf); override;
  destructor Destroy; override;
end;

implementation

{ TObjGrafDXF }
procedure TObjGrafDXF.PtoCtrl0_Move(xvTar, yvTar, dx, dy: Single);
begin
  P0.x:=xvTar;
  P0.y:=yvTar;
  ReConstGeom;
end;
procedure TObjGrafDXF.PtoCtrl1_Move(xvTar, yvTar, dx, dy: Single);
begin
  P1.x:=xvTar;
  P1.y:=yvTar;
  ReConstGeom;
end;
procedure TObjGrafDXF.PtoCtrlM_Move(xvTar, yvTar, dxv, dyv: Single);
begin
  ReConstGeom;
end;
procedure TObjGrafDXF.SetP0(const xv, yv, zv: Single);
begin
  P0.x:=xv;
  P0.y:=yv;
  P0.z:=zv;
  ReubicElemen;
end;
procedure TObjGrafDXF.SetP1(const xv, yv, zv: Single);
begin
  P1.x:=xv;
  P1.y:=yv;
  P1.z:=zv;
  ReubicElemen;
end;
constructor TObjGrafDXF.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  //Notar que los puntos de control son estáticos, aunque tal vez sea mejor, crearlos
  //solo cuando el objeto está seleccionado.
  pc0:=AddPtoControl(TD_SUP_IZQ,@PtoCtrl0_Move);
  pc1:=AddPtoControl(TD_SUP_IZQ,@PtoCtrl1_Move);
  pcM:=AddPtoControl(TD_SUP_IZQ,@PtoCtrlM_Move);
  ReConstGeom;     //Se debe llamar después de crear los puntos de control para poder ubicarlos
  nombre := 'Objeto';
end;
destructor TObjGrafDXF.Destroy;
begin
  if vertexs<>nil then vertexs.Destroy;
  inherited Destroy;
end;

procedure TObjGrafDXF.ReubicElemen;
begin
  //Ubica puntos de control
  pc0.Locate(P0);
  pc1.Locate(P1);
  pcM.Locate((P0.x + P1.x)/2, (P0.y + P1.y)/2, (P0.z + P1.z)/2 );
end;
procedure TObjGrafDXF.Draw;
var
  pdc  : TPtoCtrl;
  vtx: TObjGrafDXF;
  i: Integer;
begin
  If Marcado and Highlight Then begin
    v2d.SetPen(TColor($FF8000), 2, psSolid);
  end else begin
    v2d.SetPen(clWhite, 1);
  end;
  case etype of
  etyLine: begin
      v2d.Line(P0, P1);
    end;
//  etyCircle: begin
//      v2d.Circulo(xv + ent.x0, y + ent.y0,
//                  ent.radius);
//    end;
  etyPolyline: begin
    //Se asume que cada vertex es una forma independiente
    if (polyFlag and $0001) <> 0 then begin
      //Polígono cerrado
      for vtx in vertexs do begin
        vtx.Draw;
      end;
    end else begin
      //Polígono abierto
      for i:=0 to vertexs.Count-2 do begin
        vertexs[i].Draw;
      end;
    end;
  end;
  end;
  //---------------dibuja marca de seleccion--------------
  if Selected Then begin
    for pdc in PtosControl do pdc.Dibujar;   //Dibuja puntos de control
  end;
end;
function TObjGrafDXF.SelectRect(const P0, P1: TMotPoint; xp, yp: integer): boolean;
{Indica si las coordenadas (xp, yp) seleccionan a la recta definida por los puntos P0 y
P1. Se asume que los puntos P0 y P1, tienen ya sus coordenadas de pantalla actualizadas.}
var
  a, b: Single;
  dx, dy: Int16;
const
  DSEL = 5;   //Tolerancia en pixeles
begin
  if P0.xp = P1.xp then begin  //Caso recta vertical
     if abs(P0.xp - xp)>DSEL then exit(false);  //excede distancia horizontal
     if P0.yp = P1.yp then begin  //Caso de un punto
       Result := (abs(P0.yp - yp) < DSEL);
     end else begin //Caso de recta vertical común
       if P0.yp > P1.yp then begin  //P0 arriba
          Result := (yp<P0.yp+DSEL) and (yp>P1.yp-DSEL);
       end else begin               //P1 arriba
          Result := (yp<P1.yp+DSEL) and (yp>P0.yp-DSEL);
       end;
     end;
  end else if P0.xp < P1.xp then begin  //P0 a la izquierda
     if xp<P0.xp-DSEL then exit(false);  //escapa de límite
     if xp>P1.xp+DSEL then exit(false);  //escapa de límite
     //Simplifica la comparación, viendo solo una distancia vertical
//     a := (P1.yp - P0.yp)/(P1.xp - P0.xp);  //pendiente
//     b := P0.yp - a*P0.xp;  //Define ecuación de la recta y=ax+b
//     Result := abs(a*xp + b - yp) < DSEL;
     //Forma alternativa, sin divisiones
     dx := P1.xp - P0.xp;   //siempre positivo
     dy := P1.yp - P0.yp;   //positivo o negativo
     if abs(dy)<dx then begin
       Result := abs( (xp - P0.xp)*dy - (yp-P0.yp)*dx ) < DSEL * dx;
     end else begin //abs(dy), es mayor a dx
       Result := abs( (xp - P0.xp)*dy - (yp-P0.yp)*dx ) < DSEL * abs(dy);
     end;
  end else begin                        //P1 a la izquierda
     if xp<P1.xp-DSEL then exit(false);  //escapa de límite
     if xp>P0.xp+DSEL then exit(false);  //escapa de límite
     //Define ecuación de la recta y=ax+b
//     a := (P0.yp - P1.yp)/(P0.xp - P1.xp);  //pendiente
//     b := P1.yp - a*P1.xp;
//     Result := abs(a*xp + b - yp) < DSEL;
      dx := P0.xp - P1.xp;   //siempre positivo
      dy := P0.yp - P1.yp;   //positivo o negativo
      if abs(dy)<dx then begin
        Result := abs( (xp - P1.xp)*dy - (yp-P1.yp)*dx ) < DSEL * dx;
      end else begin //abs(dy), es mayor a dx
        Result := abs( (xp - P1.xp)*dy - (yp-P1.yp)*dx ) < DSEL * abs(dy);
      end;
  end;
end;
function TObjGrafDXF.LoSelecciona(xp, yp: integer): Boolean;
{Versión personalizada}
var
  vtx: TObjGrafDXF;
begin
  {No debería ser necesario actualizar las coordenadas de pantalla de P0 y P1, ya que
  si esta recta se mostró en pantalla, es porque se actualizaron sus coordenadas de
  pantalla:
  v2d.XYpant(P0);
  v2d.XYpant(P1);
  }
  case etype of
  etyLine: begin
    Result := SelectRect(P0, P1, xp, yp);
  end;
//  etyCircle: begin
//    end;
  etyPolyline: begin
    for vtx in self.vertexs do begin
      if SelectRect(vtx.P0, vtx.P1, xp, yp) then exit(true);
    end;
    exit(false);
  end;
  else
    Result := false;
  end;
end;

end.

