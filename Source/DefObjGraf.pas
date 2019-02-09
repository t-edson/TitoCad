{Unidad ogDefObjGraf
====================
Por Tito Hinostroza 24/09/2014

Descripcion
===========
Define a los objetos gráficos primarios que serán usados por los objetos de mayor nivel
a usar en un editor de objetos gráficos.
El objeto TObjGraf, es el objeto base del que deben derivarse los objetos más específicos
que se dibujarán en pantalla.
Se incluyen también la definición de puntos de control, que permiten redimensionar al
objeto; y de botones que pueden incluirse en los objetos graficos.
En esta unidad solo deben estar definidos los objetos básicos, los que se pueden usar en
muchas aplicaciones. Los más específicos se deben poner en otra unidad.
No se recomienda modificar esta unidad para adecuar los objetos gráficos a la aplicación.
Si se desea manejar otra clase de objetos generales, es mejor crear otra clase general a
partir de TObjGraf.
La jerarquía de clases es:

TObjVisible ----------------------------------------> TObjGraf ---> Derivar objetos aquí
              |                                          |
               --> TPtoCtrl --(Se incluyen en)-----------
              |                                          |
               --> TogButton --(Se pueden incluir en)----

}
unit DefObjGraf;
{$mode objfpc}{$H+}
interface
uses
  Classes, Controls, SysUtils, Fgl, Graphics, GraphType, Types, ExtCtrls, MotGraf3D;

const
  ANCHO_MIN = 20;    //Ancho mínimo de objetos gráficos en pixels (Coord Virtuales)
  ALTO_MIN = 20;     //Alto mínimo de objetos gráficos en Twips (Coord Virtuales)

type
  { TObjVsible }
  //Clase base para todos los objetos visibles
  TObjVsible = class
  private
    procedure Setx(AValue: Single);
    procedure Sety(AValue: Single);
  protected
    fx,fy,fz  : Single;    //coordenadas virtuales
    v2d       : TMotGraf;  //motor gráfico
    Xant,Yant : Integer;   //coordenadas anteriores
  public
    //Cuadro de selección
    Width     : Single;    //ancho
    Height    : Single;    //alto
  public
    Id        : Integer;   //Identificador del Objeto. No usado por la clase. Se deja para facilidad de identificación.
    Selected  : Boolean;   //indica si el objeto está seleccionado
    Visible   : boolean;   //indica si el objeto es visible
    procedure Crear(mGraf: TMotGraf; ancho0, alto0: Integer);  //no es constructor
    procedure Locate(const xv, yv, zv: Single);  //Fija posición
    procedure Locate(const P: TMotPoint);  //Fija posición
    function LoSelec(xp, yp: Integer): Boolean;
    function StartMove(xr, yr: Integer): Boolean;
    property x: Single read fx;
    property y: Single read fy;
    property z: Single read fz;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TPosicPCtrol = (   //tipo de desplazamiento de punto de control
    TD_SIN_POS,  //sin posición. No se reubicará automáticamente
    TD_SUP_IZQ,  //superior izquierda, desplaza ancho (por izquierda) y alto (por arriba)
    TD_SUP_CEN,  //superior central, desplaza alto por arriba
    TD_SUP_DER,  //superior derecha, desplaza ancho (por derecha) y alto (por arriba)

    TD_CEN_IZQ,  //central izquierda, desplaza ancho (por izquierda)
    TD_CEN_DER,  //central derecha, desplaza ancho (por derecha)

    TD_INF_IZQ,  //inferior izquierda
    TD_INF_CEN,  //inferior central
    TD_INF_DER   //inferior izquierda
   );

  TPtoCtrl = class;
  {Evento que genera un punto de control cuando está siendo desplazado pro el Mouse.
  (xvTra, yvTar) es el punto objetivo a donde se espera que se ubique el punto de
  control, y dxv/dyv, son los desplazamientos esperados de acuerdo al desplazamiento
  del ratón. El desplazamiento final se puede obtener solo con dx y dy, pero se envía
  también el punto objetivo, para cuando se quiera limitar el desplazamiento de un
  punto de control.}
  TEvPtoCtrlMoveXY = procedure(cp: TPtoCtrl; xvTar, yvTar, dxv, dyv: Single) of object;

  { TPtoCtrl }
  TPtoCtrl = class(TObjVsible)
  private
    fTipDesplaz: TPosicPCtrol;
    procedure SetTipDesplaz(AValue: TPosicPCtrol);
  public
    dataPtr : pointer;   {Campo auxiliar para pasar referencias a cualquier tipo de
                          dato. En la versión actual se usa solamente para pasar referencias
                          a objetos TMotPoint, para poder controlarlos. }
    //El tipo de desplazamiento, por lo general debe depender únicamente de la posicion
    property scrollType: TPosicPCtrol read fTipDesplaz write SetTipDesplaz;
    procedure Draw();
    procedure StartMove(xr, yr: Integer; x0, y0: Single);
    procedure Mover(xr, yr: Integer);  //Dimensiona las variables indicadas
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    function LoSelec(xp, yp: Integer):boolean;
  private
    mousePtr: Integer;  //Tipo de puntero del mouse
    OnCtrlPointMoveXY: TEvPtoCtrlMoveXY;  //Evento de desplazamiento del Pto de Control.
    xvTar, yvTar: Single;  {Coordenadas objetivo para las dimensiones. Usada para generar
                            el evento OnCtrlPointMoveXY.}
  public
    constructor Create(mGraf: TMotGraf; scrollType0: TPosicPCtrol;
      ProcMove: TEvPtoCtrlMoveXY);
  end;
  TPtosControl = specialize TFPGObjectList<TPtoCtrl>;  //Lista para gestionar los puntos de control

  { Objeto Tbot - Permite gestionar los botones}

//Procedimiento-evento para evento Click en Botón
  TEvenBTclk = procedure(estado: Boolean) of object;

  TTipBot =
   (BOT_CERRAR,   //botón cerrar
    BOT_EXPAND,   //botón expandir/contraer
    BOT_CHECK,    //check
    BOT_REPROD);   //reproducir/detener

  TSBOrientation =
   (SB_HORIZONT,    //horizontal
    SB_VERTICAL);   //vertical

  TObjGraph = class;
  TEventSelec = procedure(obj: TObjGraph) of object; //Procedimiento-evento para seleccionar
  TEventCPunt = procedure(TipPunt: Integer) of object; //Procedimiento-evento para cambiar puntero

  { TObjGraf }
  {Este es el Objeto padre de todos los objetos gráficos visibles que son administrados por el
   motor de edición}

  { TObjGraph }

  TObjGraph = class(TObjVsible)
  private
    procedure SetXcent(AValue: Single);
    procedure SetYCent(AValue: Single);
    function GetXCent: Single;  //Coordenada Xcentral del objeto
    function GetYCent: Single;  //Coordenada Ycentral del objeto
  public
    Name        : String;    //Identificación del objeto
    Marked      : Boolean;   //Indica que está marcado, porque el ratón pasa por encima
    DibSimplif  : Boolean;   //indica que se está en modo de dibujo simplificado
    Highlight   : Boolean;   //indica si permite el resaltado del objeto
    SizeLocked  : boolean;   //protege al objeto de redimensionado
    PosLocked   : Boolean;   //Indica si el objeto está bloqueado para movimiento
    SelLocked   : Boolean;   //Indica si el objeto está bloqueado para selección
    FillColor   : TColor;    //Color de relleno
    Proceso     : Boolean;   //Bandera
    Resizing    : boolean;  //indica que el objeto está dimensionándose
    Erased      : boolean;   //bandera para eliminar al objeto
    property Xcent: Single read GetXCent write SetXcent;
    property YCent: Single read GetYCent write SetYCent;
    procedure Selec;         //Método único para seleccionar al objeto
    procedure Deselec;       //Método único para quitar la selección del objeto
    procedure Delete;        //Método para eliminar el objeto
    function IsSelectedBy(xr, yr:integer): Boolean; virtual;
    procedure Draw; virtual;  //Dibuja el objeto gráfico
    procedure StartMove(xr, yr : Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer); virtual;  //Metodo que funciona como evento mouse_down
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer; solto_objeto: Boolean); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                 MousePos: TPoint; var Handled: Boolean); virtual;
  public
    Tipo        : Integer;   //Tipo de objeto. No usado por la librería. Queda para el usuario.
    Data        : string;    //Dato adicional. No usado por la librería. Queda para el usuario.
    Obj         : pointer;   //Dato adicional. No usado por la librería. Queda para el usuario.
  protected  //Posición y tamaño
    procedure Relocate(newX, newY, newZ: Single);
    procedure Resize; virtual;
  public  //Eventos de la clase
    OnSelec  : TEventSelec;
    OnDeselec: TEventSelec;
    OnCamPunt: TEventCPunt;
  public  // Puntos de control
    pcx        : TPtoCtrl;      //variable para Punto de Control
    PtosControl: TPtosControl;  //Lista de puntos de control
    function SelecPtoControl(xp, yp: integer): TPtoCtrl;
    function AddControlPoint(tipDesplaz0: TPosicPCtrol; ProcDimen: TEvPtoCtrlMoveXY
      ): TPtoCtrl;
    procedure Mover(xr, yr : Integer; nobjetos : Integer); virtual;
  public  //Inicialización
    constructor Create(mGraf: TMotGraf); virtual;
    destructor Destroy; override;
  end;

  TObjGraphList = specialize TFPGObjectList<TObjGraph>;

implementation
const
  ANC_PCT2 = 5;       //mitad del ancho de punto de control

{ TObjVsible }
procedure TObjVsible.Crear(mGraf: TMotGraf; ancho0, alto0: Integer);
begin
  v2d := mGraf;
  width:=ancho0;
  height :=alto0;
  visible := true;
end;
procedure TObjVsible.Setx(AValue: Single);
begin
  if fx=AValue then Exit;
  fx:=AValue;
end;
procedure TObjVsible.Sety(AValue: Single);
begin
  if fy=AValue then Exit;
  fy:=AValue;
end;
procedure TObjVsible.Locate(const xv, yv, zv: Single);
{Ubica en coordenadas virtuales}
begin
  fx := xv;
  fy := yv;
  fz := zv;
end;
procedure TObjVsible.Locate(const P: TMotPoint);
{Ubica en coordenadas virtuales}
begin
  fx := P.x;
  fy := P.y;
  fz := P.z;
end;

function TObjVsible.LoSelec(xp, yp: Integer): Boolean;
//Indica si las coordenadas de ratón seleccionan al objeto en su posición actual
var xv, yv: Single;    //coordenadas virtuales
begin
    v2d.XYvirt(xp, yp, 0, xv, yv);
    LoSelec := False;    //valor por defecto
    If (xv > fx - 2) And (xv < fx + width + 2) And
       (yv > fy - 2) And (yv < fy + height + 2) Then
        LoSelec := True;
end;
function TObjVsible.StartMove(xr, yr: Integer): Boolean;
begin
  Result := false;  //por el momento, no devuelve valor
  if not visible then exit;    //validación
  //captura posición actual, para calcular los desplazamientos
  Xant := xr;
  Yant := yr;
end;
constructor TObjVsible.Create;
begin
  inherited Create;
end;
destructor TObjVsible.Destroy;
begin
  inherited Destroy;
end;

{ TObjGraf }
function TObjGraph.SelecPtoControl(xp, yp:integer): TPtoCtrl;
//Indica si selecciona a algún punto de control y devuelve la referencia.
var pdc: TPtoCtrl;
begin
  SelecPtoControl := NIL;      //valor por defecto
  for pdc in PtosControl do
      if pdc.LoSelec(xp,yp) then begin SelecPtoControl := pdc; Exit; end;
end;
procedure TObjGraph.SetXcent(AValue: Single);
begin
  Locate(AValue-width/2, y, z);
end;
procedure TObjGraph.SetYCent(AValue: Single);
begin
  Locate(x, AValue-height/2, z);
end;
function TObjGraph.GetXCent: Single;
begin
   Result := fx + width / 2;
end;
function TObjGraph.GetYCent: Single;
begin
   Result := fy + height / 2;
end;
procedure TObjGraph.Selec;
begin
   if Selected then exit;    //ya está Selected
   Selected := true; //se marca como Selected
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnSelec) then OnSelec(self);   //llama al evento
   { TODO : Aquí se debe activar los controles para dimensionar el objeto }
end;
procedure TObjGraph.Deselec;
begin
   if not Selected then exit;    //ya está Selected
   Selected := false; //se marca como selccionado
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnDeselec) then OnDeselec(self);  //llama al evento
   { TODO : Aquí se debe desactivar los controles para dimensionar el objeto }
end;
procedure TObjGraph.Delete;
begin
  //Marca para eliminarse
  Erased := true;
end;
procedure TObjGraph.Mover(xr, yr: Integer; nobjetos: Integer);
{Metodo que funciona como evento MouseMove del objeto. Lo normales que produzca un
 desplazamiento del objeto.
"nobjetos" es la cantidad de objetos que se mueven. Usualmente es sólo uno}
var dx , dy: Single;
begin
//     If ArrastBoton Then Exit;       //Arrastrando botón  { TODO : Revisar }
//     If ArrastFila Then Exit;        //Arrastrando botón  { TODO : Revisar }
     If Selected Then begin
        v2d.ObtenerDesplazXY( xr, yr, Xant, Yant, dx, dy);
        if Proceso then   //algún elemento del objeto ha procesado el evento de movimiento
           begin
              if pcx <> NIL then begin
                 //hay un punto de control procesando el evento MouseMove
                 if not SizeLocked then
                   pcx.Mover(xr, yr);   //permite dimensionar el objeto
              end;
//              Proceso := True;  'ya alguien ha capturado el evento
           end
        else  //ningún elemento del objeto lo ha procesado, pasamos a mover todo el objeto
           begin
              Relocate(fx + dx, fy + dy, fz);   //Reubica los elementos.
              Proceso := False;
           End;
        Xant := xr; Yant := yr;
     End;
end;

function TObjGraph.IsSelectedBy(xr, yr:integer): Boolean;
//Devuelve verdad si la coordenada de pantalla xr,yr cae en un punto tal
//que "lograria" la seleccion de la forma.
var xv , yv : Single; //corodenadas virtuales
begin
    v2d.XYvirt(xr, yr, 0, xv, yv);
    IsSelectedBy := False; //valor por defecto
    //verifica área de selección
    If (xv > fx - 1) And (xv < fx + width + 1) And (yv > fy - 1) And (yv < fy + height + 1) Then
      IsSelectedBy := True;
    if Selected then begin   //Selected, tiene un área mayor de selección
      if SelecPtoControl(xr,yr) <> NIL then IsSelectedBy := True;
    end;
End;
procedure TObjGraph.Draw;
const tm = 3;
var
  pdc  : TPtoCtrl;
begin
  //---------------dibuja remarcado --------------
  If Marked and Highlight Then begin
    v2d.SetPen(clBlue, 2, psSolid);   //RGB(128, 128, 255)
    v2d.rectangXY(fx - tm, fy - tm, fx + width + tm, fy + height + tm,0);
  End;
  //---------------dibuja marca de seleccion--------------
  If Selected Then begin
//    v2d.FijaLapiz(psSolid, 1, clGreen);
//    v2d.rectang(fx, fy, fx + width, fy + height);
     for pdc in PtosControl do pdc.Draw;   //Dibuja puntos de control
  End;
end;
procedure TObjGraph.StartMove(xr, yr: Integer);
//Procedimiento para procesar el evento StartMove de los objetos gráficos
//Se ejecuta al inicio de movimiento al objeto
begin
  Xant := xr; Yant := yr;
  Proceso := False;
  if not Selected then exit;   //para evitar que responda antes de seleccionarse
  //Busca si algún punto de control lo procesa
  pcx := SelecPtoControl(xr,yr);
  if pcx <> NIL  then begin
      pcx.StartMove(xr, yr, fx, fy);     //prepara para movimiento fy dimensionamiento
      Proceso := True;      //Marcar para indicar al editor fy a Mover() que este objeto procesará
                            //el evento fy no se lo pasé a los demás que pueden estar seleccionados.
      Resizing := True; //Marca bandera
   end;
  { TODO : Verificar por qué, a veces se puede iniciar el movimiento del objeto cuando el puntero está en modo de dimensionamiento. }
end;
procedure TObjGraph.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
//Metodo que funciona como evento "MouseDown"
begin
//  CapturoEvento := NIL;
  Proceso := False;
  If IsSelectedBy(xp, yp) Then begin  //sólo responde instantáneamente al caso de selección
    If Not Selected Then Selec;
    Proceso := True;{ TODO : Verificar si es útil la bandera "Proceso" }
  End;
End;
procedure TObjGraph.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
//Metodo que funciona como evento MouseUp
//la bandera "solto_objeto" indica que se ha soltado el objeto despues de estarlo arrastrando
begin
    Proceso := False;
    //verifica si cae de un arrastre
    If solto_objeto And Selected Then begin
        Proceso := True; Exit;    //no quita la selección
    end;
    //Se soltó el ratón
    If Button = mbLeft Then  begin          //soltó izquierdo
       //Pasa evento

    end else If Button = mbRight Then begin //soltó derecho
        If IsSelectedBy(xp, yp) Then
            Proceso := True;
    end;
    //Restaura puntero si estaba dimensionándose por si acaso
    if Resizing then begin
       if not pcx.LoSelec(xp,yp) then //se salio del foco
          if Assigned(OnCamPunt) then OnCamPunt(crDefault);  //pide retomar el puntero
       Resizing := False;    //quita bandera, por si estaba Resizing
       exit;
    end;
end;
procedure TObjGraph.MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer);
//Respuesta al evento MouseMove. Se debe recibir cuando el Mouse pasa por encima del objeto
var pc: TPtoCtrl;
begin
    if not Selected then Exit;
    //Aquí se supone que tomamos el control porque está Selected
    //Procesa el cambio de puntero.
    if Assigned(OnCamPunt) then begin
        pc := SelecPtoControl(xp,yp);
        if pc<> NIL then
           OnCamPunt(pc.mousePtr)  //cambia a supuntero
        else
           OnCamPunt(crDefault);
    end;
end;
procedure TObjGraph.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;
procedure TObjGraph.Resize;
{Se llama cuando se cambian las dimensiones del objeto (Width, Height).}
begin
  //Se debe implementar de forma que se reubiquen todas las partes del objeto
  //porque se ha producido un cambio en las dimensiones.
  //Se podrían ubicar puntos de control, pero eso es tarea de la implementación.

end;
procedure TObjGraph.Relocate(newX, newY, newZ: Single);
{Se llama cuando se cambian las coordenadas del objeto (X,Y,Z). Por lo general se
necesitará también reacomodar todo el contenido.}
begin
  Locate(newX, newY, newZ);
  Resize;   //Reubica elementos por si acaso
end;
function TObjGraph.AddControlPoint(tipDesplaz0: TPosicPCtrol; ProcDimen: TEvPtoCtrlMoveXY): TPtoCtrl;
//Agrega un punto de control
begin
  Result := TPtoCtrl.Create(v2d, tipDesplaz0, ProcDimen);
  PtosControl.Add(Result);
end;
constructor TObjGraph.Create(mGraf: TMotGraf);
begin
  inherited Create;
  erased := false;
  v2d := mGraf;   //asigna motor gráfico
  width := 100;   //width por defecto
  height := 100;    //height por defecto
  fx := 100;
  fy := 100;
  PtosControl:= TPtosControl.Create(True);   //Crea lista con administración de objetos
  Selected := False;
  Marked := False;
  Proceso := false;
  DibSimplif := false;
  Highlight := true;
end;
destructor TObjGraph.Destroy;
begin
  PtosControl.Free;    //Libera Puntos de Control fy lista
  inherited Destroy;
end;
 //////////////////////////////  TPtoCtrl  //////////////////////////////
procedure TPtoCtrl.SetTipDesplaz(AValue: TPosicPCtrol);
//CAmbiando el tipo de desplazamiento se define el tipo de puntero
begin
  if fTipDesplaz=AValue then Exit;
  fTipDesplaz:=AValue;
  //actualiza tipo de puntero
  case scrollType of
  TD_SUP_IZQ: mousePtr := crSizeNW;
  TD_SUP_CEN: mousePtr := crSizeNS;
  TD_SUP_DER: mousePtr := crSizeNE;

  TD_CEN_IZQ: mousePtr := crSizeWE;
  TD_CEN_DER: mousePtr := crSizeWE;

  TD_INF_IZQ: mousePtr := crSizeNE;
  TD_INF_CEN: mousePtr := crSizeNS;
  TD_INF_DER: mousePtr := crSizeNW;
  else        mousePtr := crDefault ;
  end;
end;
constructor TPtoCtrl.Create(mGraf: TMotGraf; scrollType0: TPosicPCtrol;
  ProcMove: TEvPtoCtrlMoveXY);
begin
  inherited Crear(mGraf, 2*ANC_PCT2, 2*ANC_PCT2);    //crea
  scrollType := scrollType0;  //actualiza propiedad
  OnCtrlPointMoveXY := ProcMove;     //Asigna evento para cambiar dimensiones
  visible := true;             //lo hace visible
  fx :=0;
  fy :=0;
end;
procedure TPtoCtrl.Draw();
//Dibuja el Punto de control en la posición definida
var
  d: Single;
begin
    if not visible then exit;    //validación
    v2d.SetPen(TColor($FF8000), 1);
    v2d.FijaRelleno(TColor($FF8000));
    d := ANC_PCT2 / v2d.Zoom;  //corrige para que slaga siempre con el mismso tamaño
    v2d.rectangXYr(fx - d, fy - d, fx + d, fy + d, fz);
end;
procedure TPtoCtrl.StartMove(xr, yr: Integer; x0, y0: Single);
//Procedimiento para procesar el evento StartMove del punto de control
begin
    if not visible then exit;    //validación
    inherited StartMove(xr,yr);
    {Inicia en las coordenadas actuales del Pto de Control, }
    xvTar := fx;
    yvTar := fy;
end;
procedure TPtoCtrl.Mover(xr, yr: Integer);
//Realiza el cambio de las variables indicadas de acuerdo al tipo de control y a
//las variaciones indicadas (dx, dy)
var dx, dy: Single;
begin
    if not visible then exit;    //validación
    v2d.ObtenerDesplazXY(xr, yr, Xant, Yant, dx, dy);
    if OnCtrlPointMoveXY <>nil then OnCtrlPointMoveXY(self, xvTar, yvTar, dx, dy);
    xvTar := xvTar + dx;
    yvTar := yvTar + dy;
    Xant := xr; Yant := yr;   //actualiza, para el cálculo de ObtenerDesplazXY()
end;
procedure TPtoCtrl.MouseUp(Button: TMouseButton; Shift: TShiftState; xp,  yp: Integer);
//Procesa el evento MouseUp del "mouse".
begin
end;
function TPtoCtrl.LoSelec(xp, yp: Integer): boolean;
//Indica si las coordenadas lo selecciona
var xp0, yp0 : Integer; //corodenadas virtuales
begin
    LoSelec := False;
    if not visible then exit;    //validación
    v2d.XYpant(fx, fy, fz, xp0, yp0);   //obtiene sus coordenadas en pantalla
    //compara en coordenadas de pantalla
    If (xp >= xp0 - ANC_PCT2) And (xp <= xp0 + ANC_PCT2) And
       (yp >= yp0 - ANC_PCT2) And (yp <= yp0 + ANC_PCT2) Then
         LoSelec := True;
End;

end.

