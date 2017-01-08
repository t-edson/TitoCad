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
  protected
    fx,fy,fz  : Single;    //coordenadas virtuales
    v2d       : TMotGraf;  //motor gráfico
    Xant,Yant : Integer;   //coordenadas anteriores
  public
    Id        : Integer;   //Identificador del Objeto. No usado por la clase. Se deja para facilidad de identificación.
    Width     : Single;    //ancho
    Height    : Single;    //alto
    Selected  : Boolean;   //indica si el objeto está seleccionado
    Visible   : boolean;   //indica si el objeto es visible
    procedure Crear(mGraf: TMotGraf; ancho0, alto0: Integer);  //no es constructor
    procedure Ubicar(const xv, yv, zv: Single);  //Fija posición
    procedure Ubicar(const P: TMotPoint);  //Fija posición
    function LoSelec(xp, yp: Integer): Boolean;
    function StartMove(xr, yr: Integer): Boolean;
    property x: Single read fx;
    property y: Single read fy;
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

  {Evento que genera un putno de control cuando está siendo desplazado pro el Moues.
  (xvTra, yvTar) es el punto objetivo a donde se espera que se ubique el punto de
  control, y dxv/dyv, son los desplazamientos esperados de acuerdo al desplazamiento
  del ratón. El desplazamiento final se puede obtener solo con dx y dy, pero se envía
  también el punto objetivo, para cuando se quiera limitar el desplazamiento de un
  punto de control.}
  TEvPtoCtrlMoveXY = procedure(xvTar, yvTar, dxv, dyv: Single) of object;

  { TPtoCtrl }
  TPtoCtrl = class(TObjVsible)
  private
    fTipDesplaz: TPosicPCtrol;
    procedure SetTipDesplaz(AValue: TPosicPCtrol);
  public
    //El tipo de desplazamiento, por lo general debe depender  nicamente de la posicion
    property tipDesplaz: TPosicPCtrol read fTipDesplaz write SetTipDesplaz;
    constructor Crear(mGraf: TMotGraf; tipDesplaz0: TPosicPCtrol;
      ProcMove: TEvPtoCtrlMoveXY);
    procedure Dibujar();
    procedure StartMove(xr, yr: Integer; x0, y0: Single);
    procedure Mover(xr, yr: Integer);  //Dimensiona las variables indicadas
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    function LoSelec(xp, yp: Integer):boolean;
  private
    tipPuntero: Integer;  //Tipo de puntero
    OnPtoCtrlMoveXY : TEvPtoCtrlMoveXY;  //Evento de desplazamiento del Pto de Control.
    xvTar, yvTar: Single;  {Coordenadas objetivo para las dimensiones. Usada para generar
                            el evento OnPCdimen}
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

  { TogButton }
  TogButton = class(TObjVsible)
    estado     : Boolean;   //Permite ver el estado del botón o el check
    drawBack   : boolean;   //indica si debe dibujar el fondo
    constructor Create(mGraf: TMotGraf; tipo0: TTipBot; EvenBTclk0: TEvenBTclk);
    procedure Dibujar;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
  private
    tipo       : TTipBot;
    OnClick: TEvenBTclk
  end;

  TogButtons = specialize TFPGObjectList<TogButton>;       //Para gestionar los botones

  TObjGraf = class;
  TEventSelec = procedure(obj: TObjGraf) of object; //Procedimiento-evento para seleccionar
  TEventCPunt = procedure(TipPunt: Integer) of object; //Procedimiento-evento para cambiar puntero

  { TObjGraf }
  {Este es el Objeto padre de todos los objetos gráficos visibles que son administrados por el
   motor de edición}
  TObjGraf = class(TObjVsible)
  private
  protected
    pcx        : TPtoCtrl;      //variable para Punto de Control
    PtosControl: TPtosControl;  //Lista de puntos de control
    Buttons    : TogButtons;    //Lista para contener botones
    procedure ReubicElemen; virtual;
    procedure ReConstGeom; virtual; //Reconstruye la geometría del objeto
    function SelecPtoControl(xp, yp: integer): TPtoCtrl;
  public
    Nombre      : String;    //Identificación del objeto
    Marcado     : Boolean;   //Indica que está marcado, porque el ratón pasa por encima
    DibSimplif  : Boolean;   //indica que se está en modo de dibujo simplificado
    Highlight   : Boolean;   //indica si permite el resaltado del objeto
    SizeLocked  : boolean;   //protege al objeto de redimensionado
    PosLocked   : Boolean;   //Indica si el objeto está bloqueado para movimiento
    SelLocked   : Boolean;   //Indica si el objeto está bloqueado para selección
    Tipo        : Integer;   //Tipo de objeto. No usado por la librería. Queda para el usuario.
    Data        : string;    //Dato adicional. No usado por la librería. Queda para el usuario.
    Obj         : pointer;   //Dato adicional. No usado por la librería. Queda para el usuario.
    Relleno     : TColor;    //Color de relleno
    Proceso     : Boolean;   //Bandera
    Resizing    : boolean;  //indica que el objeto está dimensionándose
    Erased      : boolean;   //bandera para eliminar al objeto
    //Eventos de la clase
    OnSelec  : TEventSelec;
    OnDeselec: TEventSelec;
    OnCamPunt: TEventCPunt;
    function XCent: Single;  //Coordenada Xcentral del objeto
    function YCent: Single;  //Coordenada Ycentral del objeto
    procedure Ubicar(x0, y0: Single);
    procedure Selec;         //Método único para seleccionar al objeto
    procedure Deselec;       //Método único para quitar la selección del objeto
    procedure Delete;        //Método para eliminar el objeto
    procedure Mover(xr, yr : Integer; nobjetos : Integer); virtual;
    function LoSelecciona(xr, yr:integer): Boolean; virtual;
    procedure Dibujar; virtual;  //Dibuja el objeto gráfico
    procedure StartMove(xr, yr : Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer); virtual;  //Metodo que funciona como evento mouse_down
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer; solto_objeto: Boolean); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                 MousePos: TPoint; var Handled: Boolean); virtual;
    function AddPtoControl(tipDesplaz0: TPosicPCtrol; ProcDimen: TEvPtoCtrlMoveXY
      ): TPtoCtrl;
    function AddButton(ancho0, alto0: Integer; tipo0: TTipBot;
      EvenBTclk0: TEvenBTclk): TogButton;
    constructor Create(mGraf: TMotGraf); virtual;
    destructor Destroy; override;
  end;

  TlistObjGraf = specialize TFPGObjectList<TObjGraf>;

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
procedure TObjVsible.Ubicar(const xv, yv, zv: Single);
{Ubica en coordenadas virtuales}
begin
  fx := xv;
  fy := yv;
  fz := zv;
end;
procedure TObjVsible.Ubicar(const P: TMotPoint);
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

{ TogButton }
constructor TogButton.Create(mGraf: TMotGraf; tipo0: TTipBot;
  EvenBTclk0: TEvenBTclk);
begin
   inherited Crear(mGraf, 16, 16);    //crea
   tipo := tipo0;
   OnClick := EvenBTclk0;
   estado := FALSE;   //inicia en 0 (check no marcado, o botón por contraer)
   drawBack := true;
end;
procedure TogButton.Dibujar;
//Dibuja el botón de acuerdo a su tipo y estado
begin
{  case tipo of
  BOT_CERRAR: begin
       if drawBack then v2d.DibBorBoton(fx,fy,width,height);
       v2d.DibVnormal(fx+2,fy+2,10,5);
       v2d.DibVnormal(fx+2,fy+12,10,-5);
     end;
  BOT_EXPAND:
      if estado then begin
        if drawBack then v2d.DibBorBoton(fx,fy,width,height);
//         v2d.DibVnormal(fx+2,fy+7,10,-5);
//         v2d.DibVnormal(fx+2,fy+11,10,-5);
         v2d.FijaColor(COL_GRIS, COL_GRIS, 1);
         v2d.DrawTrianUp(fx+2,fy+4,width-4,height-10);
      end else begin
         if drawBack then v2d.DibBorBoton(fx,fy,width,height);
//         v2d.DibVnormal(fx+2,fy+2,10,5);
//         v2d.DibVnormal(fx+2,fy+6,10,5);
        v2d.FijaColor(COL_GRIS, COL_GRIS, 1);
        v2d.DrawTrianDown(fx+2,fy+5,width-4,height-10);
      end;
  BOT_CHECK: begin  //botón check
     if estado then begin   //dibuja solo borde
        v2d.DibBorBoton(fx,fy,15,15);
     end else begin         //dibuja con check
        v2d.DibBorBoton(fx,fy,15,15);
        v2d.DibCheck(fx+2,fy+2,10,8);
     end;
    end;
  BOT_REPROD: begin  //botón reproducir
     if estado then begin   //dibuja solo borde
       v2d.FijaColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(fx,fy,fx+width, fy+height);
       v2d.FijaColor(clBlack, clBlack, 1);
       v2d.RectangR(fx+6,fy+6,fx+width-6, fy+height-6);
     end else begin         //dibuja con check
       v2d.FijaColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(fx,fy,fx+width, fy+height);
       v2d.FijaColor(clBlack, clBlack, 1);
       v2d.poligono(fx+6, fy+3,
                    fx+18, fy + height/2,
                    fx+6, fy + height - 4);
     end;
    end;
  end;
}
end;
procedure TogButton.MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
   if LoSelec(xp,yp) then begin    //se soltó en el botón
      //cambia el estado, si aplica
      if tipo in [BOT_EXPAND, BOT_CHECK, BOT_REPROD] then estado := not estado;
      if Assigned(OnClick) then
         OnClick(estado);    //ejecuta evento
   end;
end;

{ TObjGraf }
function TObjGraf.SelecPtoControl(xp, yp:integer): TPtoCtrl;
//Indica si selecciona a algún punto de control y devuelve la referencia.
var pdc: TPtoCtrl;
begin
  SelecPtoControl := NIL;      //valor por defecto
  for pdc in PtosControl do
      if pdc.LoSelec(xp,yp) then begin SelecPtoControl := pdc; Exit; end;
end;
function TObjGraf.XCent: Single;
begin
   Result := fx + width / 2;
end;
function TObjGraf.YCent: Single;
begin
   Result := fy + height / 2;
end;
procedure TObjGraf.Selec;
begin
   if Selected then exit;    //ya está Selected
   Selected := true; //se marca como Selected
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnSelec) then OnSelec(self);   //llama al evento
   { TODO : Aquí se debe activar los controles para dimensionar el objeto }
end;
procedure TObjGraf.Deselec;
begin
   if not Selected then exit;    //ya está Selected
   Selected := false; //se marca como selccionado
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnDeselec) then OnDeselec(self);  //llama al evento
   { TODO : Aquí se debe desactivar los controles para dimensionar el objeto }
end;
procedure TObjGraf.Delete;
begin
  //Marca para eliminarse
  Erased := true;
end;
procedure TObjGraf.Mover(xr, yr: Integer; nobjetos: Integer);
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
              fx := fx + dx; fy := fy + dy;
              ReubicElemen;  //reubica los elementos
              Proceso := False;
           End;
        Xant := xr; Yant := yr;
     End;
end;

function TObjGraf.LoSelecciona(xr, yr:integer): Boolean;
//Devuelve verdad si la coordenada de pantalla xr,yr cae en un punto tal
//que "lograria" la seleccion de la forma.
var xv , yv : Single; //corodenadas virtuales
begin
    v2d.XYvirt(xr, yr, 0, xv, yv);
    LoSelecciona := False; //valor por defecto
    //verifica área de selección
    If (xv > fx - 1) And (xv < fx + width + 1) And (yv > fy - 1) And (yv < fy + height + 1) Then
      LoSelecciona := True;
    if Selected then begin   //Selected, tiene un área mayor de selección
      if SelecPtoControl(xr,yr) <> NIL then LoSelecciona := True;
    end;
End;
procedure TObjGraf.Dibujar;
const tm = 3;
var
  pdc  : TPtoCtrl;
  bot  : TogButton;
begin
  //dibuja Buttons
  for bot in Buttons do bot.Dibujar;     //Dibuja Buttons
  //---------------dibuja remarcado --------------
  If Marcado and Highlight Then begin
    v2d.SetPen(clBlue, 2, psSolid);   //RGB(128, 128, 255)
    v2d.rectangXY(fx - tm, fy - tm, fx + width + tm, fy + height + tm,0);
  End;
  //---------------dibuja marca de seleccion--------------
  If Selected Then begin
//    v2d.FijaLapiz(psSolid, 1, clGreen);
//    v2d.rectang(fx, fy, fx + width, fy + height);
     for pdc in PtosControl do pdc.Dibujar;   //Dibuja puntos de control
  End;
end;
procedure TObjGraf.StartMove(xr, yr: Integer);
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
procedure TObjGraf.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
//Metodo que funciona como evento "MouseDown"
begin
//  CapturoEvento := NIL;
  Proceso := False;
  If LoSelecciona(xp, yp) Then begin  //sólo responde instantáneamente al caso de selección
    If Not Selected Then Selec;
    Proceso := True;{ TODO : Verificar si es útil la bandera "Proceso" }
  End;
End;
procedure TObjGraf.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
//Metodo que funciona como evento MouseUp
//la bandera "solto_objeto" indica que se ha soltado el objeto despues de estarlo arrastrando
var
  bot: TogButton;
begin
    Proceso := False;
    //verifica si cae de un arrastre
    If solto_objeto And Selected Then begin
        Proceso := True; Exit;    //no quita la selección
    end;
    //Se soltó el ratón
    If Button = mbLeft Then  begin          //soltó izquierdo
       //pasa evento a los controles
       for bot in Buttons do bot.MouseUp(Button, Shift, xp, yp);
    end else If Button = mbRight Then begin //soltó derecho
        If LoSelecciona(xp, yp) Then
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
procedure TObjGraf.MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer);
//Respuesta al evento MouseMove. Se debe recibir cuando el Mouse pasa por encima del objeto
var pc: TPtoCtrl;
begin
    if not Selected then Exit;
    //Aquí se supone que tomamos el control porque está Selected
    //Procesa el cambio de puntero.
    if Assigned(OnCamPunt) then begin
        pc := SelecPtoControl(xp,yp);
        if pc<> NIL then
           OnCamPunt(pc.tipPuntero)  //cambia a supuntero
        else
           OnCamPunt(crDefault);
    end;
end;
procedure TObjGraf.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;
constructor TObjGraf.Create(mGraf: TMotGraf);
begin
  inherited Create;
  erased := false;
  v2d := mGraf;   //asigna motor gráfico
  width := 100;   //width por defecto
  height := 100;    //height por defecto
  fx := 100;
  fy := 100;
  PtosControl:= TPtosControl.Create(True);   //Crea lista con administración de objetos
  Buttons    := TogButtons.Create(True);     //Crea lista con administración de objetos
  Selected := False;
  Marcado := False;
  Proceso := false;
  DibSimplif := false;
  Highlight := true;
end;
procedure TObjGraf.ReubicElemen;
begin
  //Deberúa ubicar puntos de control, pero eso es tarea de la implementación

end;
procedure TObjGraf.ReConstGeom;
begin
  ReubicElemen;   //Reubicación de elementos
end;
destructor TObjGraf.Destroy;
begin
  Buttons.Free;        //Libera Buttons fy Lista
  PtosControl.Free;    //Libera Puntos de Control fy lista
  inherited Destroy;
end;
procedure TObjGraf.Ubicar(x0, y0: Single);
//Ubica al objeto en unas coordenadas específicas
begin
  fx := x0;
  fy := y0;
  ReubicElemen;   //reubica sus elementos
end;
function TObjGraf.AddButton(ancho0, alto0: Integer; tipo0: TTipBot;
  EvenBTclk0: TEvenBTclk): TogButton;
//Agrega un botón al objeto.
begin
  Result := TogButton.Create(v2d, tipo0, EvenBTclk0);
  Result.width := ancho0;
  Result.height := alto0;
  Buttons.Add(Result);
end;
function TObjGraf.AddPtoControl(tipDesplaz0: TPosicPCtrol; ProcDimen: TEvPtoCtrlMoveXY): TPtoCtrl;
//Agrega un punto de control
begin
  Result := TPtoCtrl.Crear(v2d, tipDesplaz0, ProcDimen);
  PtosControl.Add(Result);
end;
 //////////////////////////////  TPtoCtrl  //////////////////////////////
procedure TPtoCtrl.SetTipDesplaz(AValue: TPosicPCtrol);
//CAmbiando el tipo de desplazamiento se define el tipo de puntero
begin
  if fTipDesplaz=AValue then Exit;
  fTipDesplaz:=AValue;
  //actualiza tipo de puntero
  case tipDesplaz of
  TD_SUP_IZQ: tipPuntero := crSizeNW;
  TD_SUP_CEN: tipPuntero := crSizeNS;
  TD_SUP_DER: tipPuntero := crSizeNE;

  TD_CEN_IZQ: tipPuntero := crSizeWE;
  TD_CEN_DER: tipPuntero := crSizeWE;

  TD_INF_IZQ: tipPuntero := crSizeNE;
  TD_INF_CEN: tipPuntero := crSizeNS;
  TD_INF_DER: tipPuntero := crSizeNW;
  else        tipPuntero := crDefault ;
  end;
end;
constructor TPtoCtrl.Crear(mGraf: TMotGraf; tipDesplaz0: TPosicPCtrol;
  ProcMove: TEvPtoCtrlMoveXY);
begin
  inherited Crear(mGraf, 2*ANC_PCT2, 2*ANC_PCT2);    //crea
  tipDesplaz := tipDesplaz0;  //actualiza propiedad
  OnPtoCtrlMoveXY := ProcMove;     //Asigna evento para cambiar dimensiones
  visible := true;             //lo hace visible
  fx :=0;
  fy :=0;
end;
procedure TPtoCtrl.Dibujar();
//Dibuja el Punto de control en la posición definida
var
  xpan, ypan: integer;
begin
    if not visible then exit;    //validación
    v2d.SetPen(TColor($FF8000), 1);
    v2d.FijaRelleno(TColor($FF8000));
    v2d.XYpant(fx,fy,fz, xpan, ypan);
    v2d.rectangXYr(fx - ANC_PCT2, fy - ANC_PCT2,
                   fx + ANC_PCT2, fy + ANC_PCT2, fz);
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
    if OnPtoCtrlMoveXY <>nil then OnPtoCtrlMoveXY(xvTar, yvTar, dx, dy);
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

