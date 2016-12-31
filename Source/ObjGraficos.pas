{Aquí se deben definir los objetos gráficos con los que trabajará nuestra aplicación.
 Todos ellos deben descender de TObjGraf, para que puedadn ser tratados por el motor
 de edición "ogMotEdicion".}

unit ObjGraficos;
{$mode objfpc}{$H+}
interface
uses
  Controls, Classes, SysUtils, Graphics, GraphType, LCLIntf, Dialogs,
  MotGraf3d, ogDefObjGraf;

type

{ TEjes }
TEjes = class(TObjGraf)  //Ejes coordenados
  procedure Dibujar; override;  //Dibuja el objeto gráfico
  constructor Create(mGraf: TMotGraf); override;
end;

{ TMiObjeto }
TMiObjeto = class(TObjGraf)  //objeto gráfico que dibujaremos
  procedure Dibujar; override;  //Dibuja el objeto gráfico
  constructor Create(mGraf: TMotGraf); override;
private
  procedure ReubicElemen; override;
end;

implementation

{ TEjes }
procedure TEjes.Dibujar;
begin
  inherited Dibujar;
end;
constructor TEjes.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
end;
{ TMiObjeto }
constructor TMiObjeto.Create(mGraf: TMotGraf);
begin
  inherited;
  pc_SUP_IZQ.tipDesplaz:=TD_CEN_IZQ;
  ReConstGeom;     //Se debe llamar después de crear los puntos de control para poder ubicarlos
  nombre := 'Objeto';
end;
procedure TMiObjeto.Dibujar();
begin
  //Dibuja etiqueta
  v2d.SetPen(clGray, 1, psSolid);
  v2d.SetText(clBlack, 11,'', true);
  v2d.Texto(X + 2, Y -20, nombre);
  //muestra un rectángulo
  v2d.SetPen(clBlack, 1, psSolid);
  v2d.FijaRelleno(TColor($D5D5D5));
  v2d.rectangXYr(x, y+10, x+width, y+height,0);
  inherited;
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

end.

