{Unidad CadVisor
====================
Por Tito Hinostroza 04/01/2017

Descripción
============
Define la clase TVisGraf3D que implementa un visor-editor de objetos gráficos.
Para trabajar debe asociarse con un control PaintBox (donde aprecerán los objetos
gráficos) y una lista de objetos de tipo TlistObjGraf, definidos en la unidad
ogDefObjGraf.
Está unidad está basada en la librería ogEditGraf, en donde sería equivalente a
la unidad ogMotEdicion, con la diferecnia de que aquí ,la clase principal, no incluye
al contenedor de objetos sino solo una referecncia.
La idea es que esta clase provea, de una capa de mayor nivel sobre el motor gráfico
para mostrar y editar objetos con el ratón.
En resumen la clase TvisGraf3D, debe cumplir con los siguientes requerimientos:

1. Estar asociado a una lista de objetos "TlistObjGraf". No incluye el contenedor,
   sino que es solo un visor.
2. Debe interceptar los eventos del mouse, para la edición. Esta es una de las
   funciones principales de esta clase.
3. No debe incluir, el control por teclado de lso objetos. Ese trabajo es parte de otra
   capa, que debe correr por encima de esta clase.
4. Un proceso externo, puede modificar también, a los objetos "TlistObjGraf". En ese
   caso debe informar a esta clase, para que se adapte a los cambios.
5. Esta clase, no debe agregar ni eliminar objetos. Esas acciones recaen sobre una capa
   superior y caen en el requerimiento 5.
6. Esta clase, sí puede producir cambios en los objetos individuales (posición,
   dimensionamiento, ...). Cuando sea así, ínformará a través del evento OnModif.
7. Esta clase debe restringir el acceso al motor gráfico. La idea es que pueda adaptarse
   sin problemas a otros motores gráficos.

}
unit VisGraf3D;
{$mode objfpc}{$H+}
INTERFACE
uses
  Classes, Controls, ExtCtrls, Graphics,
  MotGraf3D, DefObjGraf;

const
  CUR_DEFEC = crDefault;          //cursor por defecto

  ZOOM_MAX_CONSULT = 5  ;  //Define el zoom máximo que se permite en un diagrama
  ZOOM_MIN_CONSULT = 0.1;  //Define el zoom mínimo que se permite en un diagrama

  FACTOR_AMPLIA_ZOOM = 1.15;  //Factor de ampliación del zoom
  DESPLAZ_MENOR = 10;
type
  EstadosPuntero = (
      EP_NORMAL,      //No se está realizando ninguna operación
      EP_SELECMULT,   //Esta en modo de selección múltiple
      EP_MOV_OBJS,    //Indica que se esta moviendo una o mas objetos
      EP_DESP_PANT,   //Indica desplazamiento con ratón + <Shift> + <Ctrl>
      EP_DIMEN_OBJ,   //Indica que se está dimensionando un objeto
      EP_RAT_ZOOM);   //Indica que se está en un proceso de Zoom


  TOnClickDer = procedure(x,y:integer) of object;

  { TVisGraf3D }

  TVisGraf3D = class
  private
    procedure v2dChangeView;
  protected
    PB   : TPaintBox;    //Control de Salida
    CapturoEvento: TObjGraf;  //referencia a objeto que capturo el movimiento
    ultMarcado   : TObjGraf;  //nombre del objeto marcado
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; xp, yp: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer); virtual;
    procedure Paint(Sender: TObject);
    procedure PBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public  //eventos
    OnClickDer: TOnClickDer;
    OnMouseUp: TMouseEvent;          //cuando se suelta el botón
    OnMouseDown: TMouseEvent;
    OnMouseMove: TMouseMoveEvent;
    OnDblClick: TNotifyEvent;
    OnObjectsMoved: procedure of object;
    OnChangeView: procedure of object;
  public
    EstPuntero   : EstadosPuntero; //Estado del puntero
    ParaMover    : Boolean;   //bandera de control para el inicio del movimiento
    objetos    : TlistObjGraf;   //referencia a la lisat de objetos
    seleccion  : TlistObjGraf;
    v2d        : TMotGraf;    //salida gráfica
    incWheel   : Single;      //Incremento de ámgulo con la rueda del mouse
    OnModif    : procedure of object;   //Este visor indica los cambios con este evento
    procedure PBDblClick(Sender: TObject);
    function Seleccionado: TObjGraf;
    function ObjPorNombre(nom: string): TObjGraf;
    procedure Refrescar;
    procedure SeleccionarTodos;
    procedure DeseleccionarTodos();
  protected
    x1Sel    : integer;
    y1Sel    : integer;
    x2Sel    : integer;
    y2Sel    : integer;
    x1Sel_a  : integer;
    y1Sel_a  : integer;
    x2Sel_a  : integer;
    y2Sel_a  : integer;
    //coordenadas del raton
    x_pulso: integer;
    y_pulso: integer;
    x_cam_a: Single;  //coordenadas anteriores de x_cam
    y_cam_a: Single;
    procedure AmpliarClick(factor: real=FACTOR_AMPLIA_ZOOM; xr: integer=0;
      yr: integer=0);
    function AnteriorVisible(c: TObjGraf): TObjGraf;
    procedure Desplazar(dx, dy: integer);
    procedure DibujRecSeleccion;

    function enRecSeleccion(X, Y: Single): Boolean;
    procedure InicMover;
    procedure InicRecSeleccion(X, Y: Integer);
    procedure moverAbajo(desp: Double=DESPLAZ_MENOR);
    procedure moverArriba(desp: Double=DESPLAZ_MENOR);
    procedure moverDerecha(desp: Double=DESPLAZ_MENOR);
    procedure moverIzquierda(desp: Double=DESPLAZ_MENOR);
    function NumeroVisibles: Integer;
    function PrimerVisible: TObjGraf;
    function RecSeleccionNulo: Boolean;
    procedure ReducirClick(factor: Real=FACTOR_AMPLIA_ZOOM; x_zoom: Real=0;
      y_zoom: Real=0);
    function SeleccionaAlguno(xp, yp: Integer): TObjGraf;
    procedure SeleccionarAnterior;
    procedure SeleccionarSiguiente;
    function SiguienteVisible(c: TObjGraf): TObjGraf;
    function UltimoVisible: TObjGraf;
    function VerificarMovimientoRaton(X, Y: Integer): TObjGraf;
    procedure VerificarParaMover(xp, yp: Integer);
  public
    procedure ObjGraf_Select(obj: TObjGraf);    //Respuesta a Evento
    procedure ObjGraf_Unselec(obj: TObjGraf);    //Respuesta a Evento
    procedure ObjGraf_SetPointer(Punt: integer);      //Respuesta a Evento
  public //Inicialización
    procedure RestaurarEstado;
    constructor Create(PB0: TPaintBox; objectList: TlistObjGraf);
    destructor Destroy; override;
  end;

implementation
uses
  FormConfig;
procedure TVisGraf3D.v2dChangeView;
begin
  if OnChangeView<>nil then OnChangeView;
end;
procedure TVisGraf3D.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
var
  ogs: TObjGraf;  //objeto seleccionado
begin
    if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, Xp, Yp);
    x_pulso := xp;
    y_pulso := yp;
    InicMover;   //por si acaso, para iniciar movimiento
    if Shift >= [ssCtrl, ssShift]  Then begin   //Contiene Shift+Ctrl
        //Inicia estado de ZOOM. Puede convertirse en EP_DESP_PANT
        //si luego se genera el evento Move()
        EstPuntero := EP_RAT_ZOOM;
        Exit;  //Ya no se necesita procesar
    end;
    ogs := SeleccionaAlguno(xp, yp);  //verifica si selecciona a un objeto
    if Button = mbRight then
      begin  //pulso derecho-------------------
        if ogs = nil Then begin  //Ninguno seleccionado
            DeseleccionarTodos;
            Refrescar;
            EstPuntero := EP_SELECMULT;  //inicia seleccion multiple
            InicRecSeleccion(x_pulso, y_pulso);
        end else begin //Selecciona a uno, pueden haber otros seleccionados
//msgbox('Lo selecciona');
            if ogs.Selected Then  begin  //Se marcó sobre un seleccionado
//                if Shift = [] Then DeseleccionarTodos;
                ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
                exit;
            end;
            //Se selecciona a uno que no tenía selección
            if Shift = [ssRight] Then  //Sin Control ni Shift
              DeseleccionarTodos;
            ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
            Refrescar;
             //ParaMover = True       ;  //listo para mover
        end;
      end
    else If Button = mbLeft Then begin   //pulso izquierdo-----------------
        If ogs = NIL Then  begin  //No selecciona a ninguno
            DeseleccionarTodos;
            Refrescar;
            EstPuntero := EP_SELECMULT;  //inicia seleccion multiple
            InicRecSeleccion(x_pulso, y_pulso);
        end Else begin     //selecciona a uno, pueden haber otros seleccionados
            If ogs.Selected Then begin //Se marcó sobre un seleccionado
                //No se quita la selección porque puede que se quiera mover
                //varios objetos seleccionados. Si no se mueve, se quitará la
                //selección en MouseUp
                //If Shift = 0 Then Call DeseleccionarTodos
                ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
                ParaMover := True;  //listo para mover
                Exit;               //Se sale sin desmarcar
            End;
            //Se selecciona a uno que no tenía selección
            If Shift = [ssLeft] Then  //Sin Control ni Shift
               DeseleccionarTodos;
            ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
            ParaMover := True;            //listo para mover
        End;
    End;
End;
procedure  TVisGraf3D.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
var o: TObjGraf;
begin
    //Verifica si la selección es NULA
    If (EstPuntero = EP_SELECMULT) And RecSeleccionNulo Then EstPuntero := EP_NORMAL;
    //Procesa de acuerdo al estado
    Case EstPuntero of
    EP_RAT_ZOOM:    //------ Zoom con el Ratón ------
      begin
        If Button = mbLeft Then AmpliarClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click izquierdo
        If Button = mbRight Then ReducirClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click derecho
//      EstPuntero = EP_NORMAL   //Legalmente debería ponerse a normal. Pero si se
                                 //hace, es posible que un click consecutivo muy
                                 //rápido, no dispare el evento MouseDown (dispara
                                 //el DblClick en su lugar), y se desactivaría el
                                 //modo ZOOM lo que es molesto.
      end;
    EP_DESP_PANT:     //------ Desplazamiento de Pantalla ------
        EstPuntero := EP_NORMAL;
    EP_MOV_OBJS:     //------ Moviendo Objetos ------
      begin
//Debug.Print "Esatado EP_MOV_OBJS"
        For o In seleccion do  //Pasa el evento a la selección
            o.MouseUp(Sender, Button, Shift, xp, yp, EstPuntero = EP_MOV_OBJS);
        EstPuntero := EP_NORMAL;  //fin de movimiento
        Refrescar;
        //Genera eventos. Los objetos movidos se pueden determinar a partir de la selección.
        if OnObjectsMoved<>nil then OnObjectsMoved;
      end;
    EP_SELECMULT :  //------ En selección múltiple, Botón izquierdo o derecho
      begin
        if objetos.Count > 100 Then begin  //Necesita actualizar porque la selección múltiple es diferente
          for o in objetos do
            if enRecSeleccion(o.XCent, o.YCent) And Not o.Selected Then o.Selec;
        end;
        EstPuntero := EP_NORMAL;
      end;
    EP_NORMAL:  //------ En modo normal
      begin
        o := SeleccionaAlguno(xp, yp);  //verifica si selecciona a un objeto
        If Button = mbRight Then //----- solto derecho -------------------
          begin
(*            If o = NIL Then  //Ninguno seleccionado
                RaiseEvent ClickDerDiag    //Genera evento
            Else    ;  //Hay uno que lo selecciona, o más???
                If Not o.Seleccionado Then Call o.SoltoRaton(Button, Shift, xr, yr)    ;  //Pasa el evento
                RaiseEvent ClickDerSel     //Genera evento
            End If*)
          end
        else If Button = mbLeft Then begin //----- solto izquierdo -----------
            If o = NIL Then    //No selecciona a ninguno
//                DeseleccionarTodos
            else begin         //Selecciona a alguno
                If Shift = [] Then DeseleccionarTodos;
                o.Selec;   //selecciona
                o.MouseUp(Sender, Button, Shift, xp, yp, false);
                Refrescar;
                //Verifica si el objeto está pidiendo que lo eliminen
//Este código se comentó porque no se le encontró ninguna utilidad
//                if o.Erased then begin
//                  EliminarObjGrafico(o);
//                  Refrescar;
//                end;
            End;
            CapturoEvento := NIL;      //inicia bandera de captura de evento
            ParaMover := False;        //por si aca
        end;
      end;
    EP_DIMEN_OBJ:  //Se soltó mientras se estaba dimensionado un objeto
      begin
        //pasa evento a objeto que se estaba dimensionando
        CapturoEvento.MouseUp(Sender, Button, Shift, xp, yp, false);
        //termina estado
        EstPuntero := EP_NORMAL;
        CapturoEvento := NIL;      //inicia bandera de captura de evento
        ParaMover := False;        //por si aca
      end;
    End;
    if Button = mbRight then
      if OnClickDer<> nil then OnClickDer(xp,yp);  //evento
    if OnMouseUp<>nil then OnMouseUp(Sender, Button, Shift, xp, yp);
End;
procedure TVisGraf3D.MouseMove(Sender: TObject; Shift: TShiftState;
  X,  Y: Integer);
var
  s: TObjGraf;
  dx, dy: Single;
begin
  if OnMouseMove<>nil then OnMouseMove(Sender, Shift, X, Y);
  If Shift = [ssCtrl, ssShift, ssRight] Then begin //<Shift>+<Ctrl> + <Botón derecho>
     EstPuntero := EP_DESP_PANT;
//     MoverDesp(x_pulso - X, y_pulso - Y);
     v2d.ObtenerDesplaz2( x, y, x_pulso, y_pulso, dx, dy);
     v2d.x_cam -= dx;
     v2d.y_cam -= dy;
     x_pulso := X; y_pulso := Y;  {Tal vez deba usar otras variables aparte de x_pulso, e
                                   y_pulso,  para no interferir}

     Refrescar;
     exit;
  end;
  If ParaMover = True Then VerificarParaMover(X, Y);
  If EstPuntero = EP_SELECMULT Then begin  //modo seleccionando multiples formas
      x2Sel := X;
      y2Sel := Y;
      //verifica los que se encuentran seleccionados
      if objetos.Count < 100 Then begin//sólo anima para pocos objetos
          for s In objetos do begin
            if s.SelLocked then continue;
            if enRecSeleccion(s.XCent, s.YCent) And Not s.Selected Then begin
              s.Selec;
            End;
            if Not enRecSeleccion(s.XCent, s.YCent) And s.Selected Then begin
              s.Deselec;
            end;
          end;
      End;
      Refrescar
  end Else If EstPuntero = EP_MOV_OBJS Then begin  //mueve la selección
      if OnModif<>nil then OnModif;  //cambio
      for s in seleccion do
          s.Mover(x,y, seleccion.Count);
      Refrescar;
  end Else If EstPuntero = EP_DIMEN_OBJ Then begin
      //se está dimensionando un objeto
      CapturoEvento.Mover(X, Y, seleccion.Count);
      Refrescar;
  end else begin
      If CapturoEvento <> NIL Then begin
         CapturoEvento.Mover(X, Y, seleccion.Count);
         Refrescar;
      end Else begin  //Movimiento simple
          s := VerificarMovimientoRaton(X, Y);
          if s <> NIL then s.MouseMove(Sender, Shift, X, Y);  //pasa el evento
      end;
  end;
end;
procedure TVisGraf3D.Paint(Sender: TObject);
var
  o:TObjGraf;
begin
//    PB.canvas.Brush.Color := clWhite; //rgb(255,255,255);
//    PB.canvas.FillRect(PB.ClientRect); //fondo
    v2d.Clear;
    If EstPuntero = EP_SELECMULT Then DibujRecSeleccion;
    //Dibuja objetos
    for o In objetos do begin
      o.Dibujar;
    end;
    //Dibuja eje
    if Config.fcVista.VerEjesCoor then begin
      v2d.SetPen(clRed, 1);
      v2d.Line(0,0,0,100,0,0);
      v2d.Line(0,0,0,0,100,0);
      v2d.Line(0,0,0,0,0,100);
      v2d.Texto(100,10,0,'x');
      v2d.Texto(0,100,0,'y');
    end;
end;
procedure TVisGraf3D.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  d: Single;
begin
  if Shift = [ssCtrl] then begin
    if WheelDelta>0 then d := incWheel else d := -incWheel;
    v2d.Alfa := v2d.Alfa + d;
  end;
  if Shift = [ssShift] then begin
    if WheelDelta>0 then d := incWheel else d := -incWheel;
    v2d.Fi := v2d.Fi + d;
  end;
  Refrescar;
end;
procedure TVisGraf3D.Refrescar();  //   Optional s: TObjGraf = Nothing
begin
  PB.Invalidate;
end;
procedure TVisGraf3D.InicMover;
//Procedimiento que inicia un desplazamiento de la pantalla. Se debe llamar cada vez que se puede
//iniciar el proceso de desplazamiento
begin
    x_cam_a := v2d.x_cam;
    y_cam_a := v2d.y_cam;
end;

function TVisGraf3D.SeleccionaAlguno(xp, yp: Integer): TObjGraf;
//Rutina principal para determinar la selección de objetos. Si (xp,yp)
//selecciona a algún objeto, devuelve la referencia, sino devuelve "NIL"
var
  i: Integer;
  s: TObjGraf;
begin
  //Verifica primero entre los que están seleccionados
  Result := NIL; //valor por defecto
  //Explora objetos priorizando los que están encima
  For i := seleccion.Count-1 downTo 0 do begin
    s := seleccion[i];
    If not s.SelLocked and s.LoSelecciona(xp, yp) Then begin
        Result:= s;
        Exit;
    End;
  end;
  //Explora objetos priorizando los que están encima
  For i := objetos.Count-1 downTo 0 do begin
    s := objetos[i];
    If not s.SelLocked and s.LoSelecciona(xp, yp) Then begin
        Result := s;
        Exit;
    End;
  end;
End;
procedure TVisGraf3D.VerificarParaMover(xp, yp: Integer);
{Si se empieza el movimiento, selecciona primero algun elemento que
pudiera estar debajo del puntero y actualiza "EstPuntero".
Solo se debe ejecutar una vez al inicio del movimiento, para ello se
usa la bandera ParaMover, que debe ponerse a FALSE aquí.}
var s: TObjGraf;
begin
    for s In seleccion  do begin  //da prioridad a los elementos seleccionados
      if s.PosLocked then continue;
      s.StartMove(xp, yp);      //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin  //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then EstPuntero := EP_DIMEN_OBJ else EstPuntero := EP_NORMAL;
          ParaMover := False;    //para que ya no se llame otra vez
          Exit;
      end;
    end;
    for s In objetos do begin
      if s.PosLocked then continue;
      s.StartMove(xp, yp);    //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin   //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then EstPuntero := EP_DIMEN_OBJ else EstPuntero := EP_NORMAL;
          EstPuntero := EP_NORMAL;
          ParaMover := False;   //para que ya no se llame otra vez
          exit;
      end;
    end;
    //Ningún objeto ha capturado, el evento, asumimos que se debe realizar
    //el desplazamiento simple de los objetos seleccionados
//Debug.Print "   VerifParaMover: EP_MOV_OBJS"
    EstPuntero := EP_MOV_OBJS;
    CapturoEvento := nil;      //ningún objeto capturo el evento
    ParaMover := False;        //para que ya no se llame otra vez
End;
function TVisGraf3D.VerificarMovimientoRaton(X, Y: Integer): TObjGraf;
//Anima la marcación de los objetos cuando el ratón pasa encima de ellos
//Devuelve referencia al objeto por el que pasa el cirsor
var s: TObjGraf;
begin

    s := SeleccionaAlguno(X, Y);    //verifica si selecciona a un objeto
    Result := s;  //devuelve referencia
//    If Not s = NIL Then
//        If s.Id = ID_CONECTOR Then  ;  //Or s.Seleccionado
//            Set s = Nothing  ;  //no válido para conectores
//        End If
//    End If
    //Se refresca la pantalla optimizando
    If s = NIL Then begin  //No hay ninguno por marcar
      If ultMarcado <> NIL Then begin
            //Si ya había uno marcado, se actualiza el dibujo y la bandera
            ultMarcado.Marcado := False;  //se desmarca
            ultMarcado := NIL;
            Refrescar;
        End;
      PB.Cursor := CUR_DEFEC;   //restaura cursor
    end
    Else begin   //Hay uno por marcar
      If ultMarcado = NIL Then begin
         //No había ninguno marcado
         ultMarcado := s;      //guarda
         s.Marcado := True;    //lo marca
         Refrescar;            //y se dibuja
      end Else begin  //ya había uno marcado
           If ultMarcado = s Then  //es el mismo
               //no se hace nada
           Else begin    //había otro marcado
               ultMarcado.Marcado := False;  //se desmarca
               ultMarcado := s ;   //actualiza
               s.Marcado := True;
               Refrescar;          //y se dibuja
           End;
        End;
    End;

End;
//***********Funciones para administrar los elementos visibles y seleccion por teclado**********
function TVisGraf3D.NumeroVisibles: Integer;
//devuelve el número de objetos visibles
var
  v: TObjGraf;
  tmp: Integer;
begin
  tmp := 0;
  For v in objetos do begin
    if v.visible then Inc(tmp);
  end;
  Result := tmp;
end;
function TVisGraf3D.PrimerVisible: TObjGraf;
 //devuelve el primer objeto visible
var
  i: integer;
begin
  for i:=0 to objetos.Count-1 do begin
    if objetos[i].visible then begin
      Result := objetos[i];
      exit;
    end;
  end;
End;
function TVisGraf3D.UltimoVisible: TObjGraf;
 //devuelve el último objeto visible
var
  i: Integer;
begin
  for i:=objetos.Count-1 downto 0 do begin
    if objetos[i].visible then begin
      Result := objetos[i];
      exit;
    end;
  end;
end;
function TVisGraf3D.SiguienteVisible(c: TObjGraf): TObjGraf;
//devuelve el siguiente objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objetos
    For i := 0 To objetos.Count-1 do begin
      if objetos[i] = c Then break;
    end;
    //calcula el siguiente elemento
    repeat
      Inc(i);
      If i >= objetos.Count Then begin  //se ha llegado al final del conjunto
        Result := PrimerVisible;
        Exit;
      end;
    until objetos[i].visible;
    //selecciona el siguiente visible
    Result := objetos[i];
end;
function TVisGraf3D.AnteriorVisible(c: TObjGraf): TObjGraf;
//devuelve el anterior objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objetos
    For i := 0 To objetos.Count-1 do begin
      If objetos[i] = c Then break;
    end;
    //calcula el elemento anterior
    repeat
      Dec(i);
      If i < 0 Then begin  //se ha llegado al inicio
        Result := UltimoVisible;
        Exit;
      End;
    until objetos[i].visible;
    //selecciona el siguiente visible
    Result := objetos[i];
End;

procedure TVisGraf3D.SeleccionarSiguiente;
//Selecciona el siguiente elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el primero
var
  s: TObjGraf;
begin
    if NumeroVisibles() = 0 Then exit;
    if seleccion.Count = 1 Then begin  //hay uno seleccionado
        s := seleccion[0];   //toma el seleccionado
        s := SiguienteVisible(s);
        DeseleccionarTodos;
        s.Selec;
    end else begin     //hay cero o más de uno seleccionado
        s := PrimerVisible;  //selecciona el primero
        DeseleccionarTodos;
        s.Selec;
    end;
    Refrescar;
end;
procedure TVisGraf3D.SeleccionarAnterior;
//Selecciona el anterior elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el ultimo
var
  s: TObjGraf;
begin
    if NumeroVisibles() = 0 Then exit;
    if seleccion.Count = 1 then begin     //hay uno seleccionado
        s := seleccion[0];    //toma el seleccionado
        s := AnteriorVisible(s);
        DeseleccionarTodos;
        s.Selec;
    end else begin               //hay cero o más de uno seleccionado
        s := UltimoVisible;   //selecciona el ultimo
        DeseleccionarTodos;
        s.Selec;
    end;
    Refrescar;
end;

procedure TVisGraf3D.PBDblClick(Sender: TObject);
begin
  if OnDblClick<>nil then OnDblClick(Sender);
end;
//******************* Funciones de visualización **********************
procedure TVisGraf3D.AmpliarClick(factor: real = FACTOR_AMPLIA_ZOOM;
                        xr: integer = 0; yr: integer = 0);
var anc_p: Real ;  //ancho de pantalla
    alt_p: Real ;  //alto de pantalla
    x_zoom, y_zoom: Single;
begin
    If v2d.zoom < ZOOM_MAX_CONSULT Then
        v2d.zoom := v2d.zoom * factor;
    If (xr <> 0) Or (yr <> 0) Then begin  //se ha especificado una coordenada central
        anc_p := PB.width / v2d.zoom;
        alt_p := PB.Height / v2d.zoom;
        v2d.XYvirt(xr, yr, 0, x_zoom, y_zoom);     //convierte
        v2d.FijarVentana(PB.Width, PB.Height,
                x_zoom - anc_p / 2, x_zoom + anc_p / 2, y_zoom - alt_p / 2, y_zoom + alt_p / 2);
    End;
    Refrescar;
End;
procedure TVisGraf3D.ReducirClick(factor: Real = FACTOR_AMPLIA_ZOOM;
                        x_zoom: Real = 0; y_zoom: Real = 0);
begin
    If v2d.zoom > ZOOM_MIN_CONSULT Then
        v2d.zoom := v2d.zoom / factor;
    Refrescar;
End;
/////////////////////////  Funciones de selección /////////////////////////////
procedure TVisGraf3D.SeleccionarTodos;
var s: TObjGraf;
begin
    For s In objetos do s.Selec; //selecciona todos
End;
procedure TVisGraf3D.DeseleccionarTodos();
var s: TObjGraf;
begin
  For s In objetos do //no se explora "seleccion" porque se modifica con "s.Deselec"
    if s.Selected then s.Deselec;
//  seleccion.Clear; //No se puede limpiar simplemente la lista. Se debe llamar a s.Deselec
End;
function  TVisGraf3D.Seleccionado: TObjGraf;
//Devuelve el objeto seleccionado. Si no hay ninguno seleccionado, devuelve NIL.
begin
  Result := nil;   //valor por defecto
  if seleccion.Count = 0 then exit;  //no hay
  //hay al menos uno
  Result := seleccion[seleccion.Count-1];  //devuelve el único o último
End;
function  TVisGraf3D.ObjPorNombre(nom: string): TObjGraf;
//Devuelve la referecnia a un objeto, dado el nombre. Si no encuentra, devuelve NIL.
var s: TObjGraf;
begin
  Result := nil;   //valor por defecto
  if nom = '' then exit;
  For s In objetos do
    if s.nombre = nom then begin
       Result := s;
       break;
    end;
End;

procedure TVisGraf3D.moverAbajo(desp: Double = DESPLAZ_MENOR) ;  //abajo
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(desp / z));
    Refrescar;
end;
procedure TVisGraf3D.moverArriba(desp: Double = DESPLAZ_MENOR) ;  //arriba
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(-desp / z));
    Refrescar;
end;
procedure TVisGraf3D.moverDerecha(desp: Double = DESPLAZ_MENOR) ;  //derecha
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(desp / z), 0);
    Refrescar;
end;
procedure TVisGraf3D.moverIzquierda(desp: Double = DESPLAZ_MENOR) ;  //izquierda
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(-desp / z), 0);
    Refrescar;
end;
procedure TVisGraf3D.Desplazar(dx, dy: integer);
begin
//Procedimiento "estandar" para hacer un desplazamiento de la pantalla
//Varía los parámetros de la perspectiva "x_cam" e "y_cam"
    v2d.Desplazar(dx, dy);
end;

/////////////////////////   Funciones del Rectángulo de Selección /////////////////////////
procedure TVisGraf3D.DibujRecSeleccion();
//Dibuja por métodos gráficos el rectángulo de selección en pantalla
begin
    v2d.SetPen(clGreen, 1, psDot);
    v2d.rectang0(x1Sel, y1Sel, x2Sel, y2Sel);

    x1Sel_a := x1Sel; y1Sel_a := y1Sel;
    x2Sel_a := x2Sel; y2Sel_a := y2Sel;
End;
procedure TVisGraf3D.InicRecSeleccion(X, Y: Integer);
//Inicia el rectángulo de selección, con las coordenadas
begin
    x1Sel:= X; y1Sel := Y;
    x2Sel := X; y2Sel := Y;
    x1Sel_a := x1Sel;
    y1Sel_a := y1Sel;
    x2Sel_a := x2Sel;
    y2Sel_a := y2Sel;
End;
function TVisGraf3D.RecSeleccionNulo: Boolean;
 //Indica si el rectángulo de selección es de tamaño NULO o despreciable
begin
    If (x1Sel = x2Sel) And (y1Sel = y2Sel) Then
        RecSeleccionNulo := True
    Else
        RecSeleccionNulo := False;
End;
function TVisGraf3D.enRecSeleccion(X, Y: Single): Boolean;
//Devuelve verdad si (x,y) esta dentro del rectangulo de seleccion.
var xMin, xMax: Integer;   //coordenadas mínimas y máximas del recuadro
    yMin, yMax: Integer;
    xx1, yy1: Single;
    xx2, yy2: Single;
begin
    //guarda coordenadas mínimas y máximas
    If x1Sel < x2Sel Then begin
        xMin := x1Sel;
        xMax := x2Sel;
    end Else begin
        xMin := x2Sel;
        xMax := x1Sel;
    End;
    If y1Sel < y2Sel Then begin
        yMin := y1Sel;
        yMax := y2Sel;
    end Else begin
        yMin := y2Sel;
        yMax := y1Sel;
    End;

    v2d.XYvirt(xMin, yMin, 0, xx1, yy1);
    v2d.XYvirt(xMax, yMax, 0, xx2, yy2);

    //verifica si está en región
    If (X >= xx1) And (X <= xx2) And (Y >= yy1) And (Y <= yy2) Then
        enRecSeleccion := True
    Else
        enRecSeleccion := False;
End;
(*
Public Sub GuardarPerspectiva(nar: Integer)
;  //Escribe datos de perspectiva en disco
    Print #nar, "<PERS>"   ;  //Marcador
    ;  //guarda parámetro de visualización
    Print #nar, N2f(v2d.zoom) & w & w & w & _
                N2f(v2d.x_cam) & w & N2f(v2d.y_cam) & ",,,,,"
    Print #nar, "</PERS>"   ;  //Marcador final
End Sub

Public Sub LeePerspectiva(nar: Integer)
;  //lee datos de perspectiva. No lee marcador inicial
Dim a(): String
Dim tmp: String
    ;  //lee perspectiva inicial
    Line Input #nar, tmp   ;  //lee parámetros
    a = Split(tmp, w)
    v2d.zoom = f2N(a(0))
    v2d.x_cam = f2N(a(3)): v2d.y_cam = f2N(a(4))
    Line Input #nar, tmp   ;  //lee marcador de fin
    Call v2d.GuardarPerspectivaEn(Pfinal)   ;  //para que no se regrese
End Sub
*)
////////////////// Eventos para atender requerimientos de objetos "TObjGraf" ///////////////////////
procedure TVisGraf3D.ObjGraf_Select(obj: TObjGraf);
//Agrega un objeto gráfico a la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere seleccionar un objeto se debe usar la forma objeto.Selec.
begin
//    If obj.Seleccionado Then Exit;  //Ya está seleccionado. No debe ser necesario
  seleccion.Add(obj);      { TODO : Verificar si se puede manejar bien el programa sin usar la propiedad "NombreObj"}
End;
procedure TVisGraf3D.ObjGraf_Unselec(obj: TObjGraf);
//Quita un objeto gráfico de la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere quitar la seleccion a un objeto se debe usar la forma objeto.Deselec.
begin
//    If not obj.Seleccionado Then Exit;
  seleccion.Remove(obj);
End;
procedure TVisGraf3D.ObjGraf_SetPointer(Punt: integer);
//Procedimiento que cambia el puntero del mouse. Es usado para proporcionar la los objetos "TObjGraf"
//la posibilidad de cambiar el puntero.
begin
  PB.Cursor := Punt;        //define cursor
end;
//Inicialización
procedure TVisGraf3D.RestaurarEstado;
{Resatura el estado del Visor, poniéndolo en estado EP_NORMAL. }
begin
  EstPuntero := EP_NORMAL;
  ParaMover := false;
  CapturoEvento := NIL;
  ultMarcado := NIL;
  PB.Cursor := CUR_DEFEC;        //define cursor
end;
constructor TVisGraf3D.Create(PB0: TPaintBox; objectList: TlistObjGraf);
{Metodo de inicialización de la clase Visor. Debe indicarse el PaintBox de
salida donde se controlarán los objetos gráficos.
y también debe recibir la lista de objetos a administrar.}
begin
  PB := PB0;  //asigna control de salida
  objetos := objectList;
  //intercepta eventos
  PB.OnMouseUp  :=@MouseUp;
  PB.OnMouseDown:=@MouseDown;
  PB.OnMouseMove:=@MouseMove;
  PB.OnMouseWheel:=@PBMouseWheel;
  PB.OnDblClick :=@PBDblClick;
  PB.OnPaint    :=@Paint;

  //inicia motor
  v2d := TMotGraf.Create(PB);    //Inicia motor gráfico
  v2d.SetFont('MS Sans Serif');  //define tipo de letra
  v2d.OnChangeView:=@v2dChangeView;
//  objetos := TlistObjGraf.Create(TRUE);   //crea lista con "posesión" de objetos
  seleccion := TlistObjGraf.Create(FALSE);  {crea lista sin posesión", porque la
                                             administración la hará "objetos".}
  RestaurarEstado;
  incWheel  := 0.1;
end;
destructor TVisGraf3D.Destroy;
begin
  seleccion.Free;
//  objetos.Free;  //limpia lista y libera objetos apuntados
  v2d.Free;      //Libera
  //resatura eventos
  PB.OnMouseUp:=nil;
  PB.OnMouseDown:=nil;
  PB.OnMouseMove:=nil;
  PB.OnPaint:=nil;
  inherited;     //llama al destructor
end;

end.

