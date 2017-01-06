{Define los objetos gráficos con los que trabajará nuestra aplicación.
 Todos ellos deben descender de TObjGraf, para que puedadn ser tratados por el motor
 de edición "ogMotEdicion".}

unit ObjGraficos;
{$mode objfpc}{$H+}
interface
uses
  Graphics, fgl,
  MotGraf3d, DefObjGraf;

type
//Tipo de entidad gráfica
TDXFentType = (
   etyLine      //línea
  ,etyCircle    //círculo
  ,etyPolyline  //polilínea
  ,etyInsert    //bloque
);

TObjGrafDXF = class;
TObjGrafDXF_list = specialize TFPGObjectList<TObjGrafDXF>;
{ TMiObjeto }
TMiObjeto = class(TObjGraf)  //objeto gráfico que dibujaremos
  procedure Dibujar; override;  //Dibuja el objeto gráfico
  constructor Create(mGraf: TMotGraf); override;
private
  procedure ReubicElemen; override;
end;

{ TObjGrafDXF }
{Se define al objeto para que sea compatible con archivos DXF.}
TObjGrafDXF = class(TObjGraf)  //objeto gráfico DXF
public  //Campos equivalentes a los de una entidad DXF
  etype: TDXFentType;   //tipo de entidad
  idDxf: string;        //identificador de la entidad
  layer: string;
  color: string;
  style: string;
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
    //propiedades gráficas
    x0, y0, z0: Double;
    x1, y1, z1: Double;
    radius: double;
    vertexs: TObjGrafDXF_list;   {Lista de Vertex. Solo se instancia para objetos
                                 complejos. OJO!!! Es muy pesado guardar una lista de
                                 TObjGrafDXF. Debería optimizarse}
    blkName: string;    //usado cuando es de tipo etyInsert.
public
  procedure Dibujar; override;  //Dibuja el objeto gráfico
  constructor Create(mGraf: TMotGraf); override;
private
  procedure ReubicElemen; override;
end;

implementation

{ TMiObjeto }
constructor TMiObjeto.Create(mGraf: TMotGraf);
begin
  inherited;
  pc_SUP_IZQ.tipDesplaz:=TD_CEN_IZQ;
  ReConstGeom;     //Se debe llamar después de crear los puntos de control para poder ubicarlos
  nombre := 'Objeto';
end;
procedure TMiObjeto.ReubicElemen;
//Reubica elementos, del objeto. Es llamado cuando se cambia la posición del objeto, con
//o sin cambio de las dimensiones.
var
  x2: Single;
begin
  inherited;
  x2 := x + width;
end;
procedure TMiObjeto.Dibujar();
begin
  //Dibuja etiqueta
//  v2d.SetPen(clGray, 1);
  v2d.SetText(clWhite, 11,'', false);
  v2d.Texto(X + 2, Y + Height + 20, 0, nombre);
  //muestra un rectángulo
  v2d.SetPen(clWhite, 1, psSolid);
  v2d.FijaRelleno(clBlack);
  v2d.rectangXYr(x, y+10, x+width, y+height,0);
  inherited;
end;

{ TObjGrafDXF }
constructor TObjGrafDXF.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  pc_SUP_IZQ.tipDesplaz:=TD_CEN_IZQ;
  ReConstGeom;     //Se debe llamar después de crear los puntos de control para poder ubicarlos
  nombre := 'Objeto';
end;
procedure TObjGrafDXF.ReubicElemen;
begin
  inherited ReubicElemen;
end;
procedure TObjGrafDXF.Dibujar;
begin
  v2d.SetPen(clWhite, 1);
  case etype of
  etyLine: begin
      v2d.Line(x0, y0, z0,
               x1, y1, z1);
    end;
//  etyCircle: begin
//      v2d.Circulo(x + ent.x0, y + ent.y0,
//                  ent.radius);
//    end;
//  etyPolyline: begin
//      //Por eficiencia, se dibuja la polilínea directamente del canvas
//      SetLength(Ptos, ent.vertexs.Count);   //dimensiona
//      //transforma puntos
//      for i:= 0 to ent.vertexs.Count-1 do begin
//        Ptos[i].x := v2d.XPant(x + ent.vertexs[i].x0);
//        Ptos[i].y := v2d.YPant(y + ent.vertexs[i].y0);
//      end;
//      //v2d.Canvas.Polygon(Ptos);   //dibuja
//      v2d.Canvas.Polyline(Ptos);
//    end;
  end;
{
  //Dibuja etiqueta
//  v2d.SetPen(clGray, 1);
  v2d.SetText(clWhite, 11,'', false);
  v2d.Texto(X + 2, Y + Height + 20, 0, nombre);
  //muestra un rectángulo
  v2d.SetPen(clWhite, 1, psSolid);
  v2d.FijaRelleno(clBlack);
  v2d.rectangXYr(x, y+10, x+width, y+height,0);
  }
  inherited Dibujar;
end;

end.

