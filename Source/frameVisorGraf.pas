{                                frameVisCplex
Este Frame será usado para colocar nuestro editor gráfico. Incluye un PaintBox,
como salida gráfica.
Al ser un frame, puede incluirse en un formulario cualquiera.

                                              Por Tito Hinostroza  04/01/2017
}
unit frameVisorGraf;
{$mode objfpc}{$H+}
interface
uses
  Classes, Forms, ExtCtrls,
  ObjGraficos, VisGraf3D, DefObjGraf;
type
  TOnObjetosElim = procedure of object;
  {Evento para movimiento del Mouse. Notar que además de las coordenaadas del ratón,
  proporciona coordenadas virtuales}
  TEveMouseVisGraf = procedure(Shift: TShiftState; xp, yp: Integer;
                                      xv, yv, zv: Single) of object;

  { TfraVisorGraf }

  TfraVisorGraf = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function GetAlfa: Single;
    function GetFi: Single;
    function GetxCam: Single;
    function GetyCam: Single;
    function GetZoom: Single;
    procedure motEdiChangeView;
    procedure motEdiModif;
    procedure SetAlfa(AValue: Single);
    procedure SetFi(AValue: Single);
    procedure SetxCam(AValue: Single);
    procedure SetxDes(AValue: integer);
    function GetxDes: integer;
    procedure SetyCam(AValue: Single);
    procedure SetyDes(AValue: integer);
    function GetyDes: integer;
    procedure SetZoom(AValue: Single);
    procedure visEdiChangeState(VisState: TVisStateTyp);
    procedure visEdiMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  public
    objetos : TlistObjGraf; //Lista de objetos
    visEdi  : TVisGraf3D;  //motor de edición  (La idesa es que pueda haber más de uno)
    Modif   : Boolean;      //bandera para indicar Diagrama Modificado
    OnObjetosElim: TOnObjetosElim;   //cuando se elminan uno o más objetos
    OnCambiaPerspec: procedure of object; //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState: TEvChangeState;  //Cambia el estado del Visor
    procedure AgregarObjGrafico(og: TObjGraf; AutoPos: boolean=true);
    procedure EliminarObjGrafico(obj: TObjGraf);
    procedure EliminarTodosObj;
    procedure ElimSeleccion;
  public //Propiedades reflejadas
    property xDes: integer read GetxDes write SetxDes;
    property yDes: integer read GetyDes write SetyDes;
    property xCam: Single read GetxCam write SetxCam;
    property yCam: Single read GetyCam write SetyCam;
    property Alfa: Single read GetAlfa write SetAlfa;
    property Fi: Single read GetFi write SetFi;
    property Zoom: Single read GetZoom write SetZoom;
  public  //Inicialización
    procedure InicVista;
    constructor Create(AOwner: TComponent; ListObjGraf: TlistObjGraf);
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

procedure TfraVisorGraf.AgregarObjGrafico(og: TObjGraf; AutoPos: boolean = true);
//Agrega un objeto grafico al editor. El objeto gráfico debe haberse creado previamente,
//y ser de tipo TObjGraf o un descendiente. "AutoPos", permite posicionar automáticamente
//al objeto en pantalla, de modo que se evite ponerlo siempre en la misma posición.
var
  x: single;
  y: single;
begin
  Modif := True;        //Marca el editor como modificado
  //Posiciona tratando de que siempre aparezca en pantalla
  if AutoPos Then begin  //Se calcula posición
    x := visEdi.v2d.Xvirt(100, 100) + 30 * objetos.Count Mod 400;
    y := visEdi.v2d.Yvirt(100, 100) + 30 * objetos.Count Mod 400;
    og.Ubicar(x,y);
  end;
  //configura eventos para ser controlado por este editor
  og.OnSelec   := @visEdi.ObjGraf_Select;     //referencia a procedimiento de selección
  og.OnDeselec := @visEdi.ObjGraf_Unselec;    //referencia a procedimiento de "de-selección"
  og.OnCamPunt := @visEdi.ObjGraf_SetPointer; //procedimiento para cambiar el puntero
//  Refrescar(s)   ;             //Refresca objeto
  objetos.Add(og);               //agrega elemento
end;
procedure TfraVisorGraf.EliminarObjGrafico(obj: TObjGraf);  //elimina un objeto grafico
begin
  Modif := True;  //Marca documento como modificado
  obj.Deselec;  //por si acaso
  objetos.Remove(obj);
  obj := nil;
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TfraVisorGraf.EliminarTodosObj;
//Elimina todos los objetos gráficos existentes
begin
  if objetos.Count=0 then exit;  //no hay qué eliminar
  //elimina
  visEdi.DeseleccionarTodos;  //por si acaso hay algun simbolo seleccionado
  objetos.Clear;          //limpia la lista de objetos
  visEdi.RestaurarEstado;
  Modif := true;          //indica que se modificó
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TfraVisorGraf.ElimSeleccion;
//Elimina la selección.
var
  v: TObjGraf;
  tmp: TOnObjetosElim;
begin
  tmp := OnObjetosElim;  //guarda evento
  OnObjetosElim := nil; //para evitar llamar muchas veces
  For v In visEdi.seleccion  do  //explora todos
    EliminarObjGrafico(v);
  OnObjetosElim := tmp;  //restaura
  if OnObjetosElim<>nil then OnObjetosElim;  //llama evento
  visEdi.Refrescar;
end;
function TfraVisorGraf.GetxDes: integer;
begin
  Result := visEdi.v2d.x_des;
end;
procedure TfraVisorGraf.SetxDes(AValue: integer);
begin
  visEdi.v2d.x_des:=AValue;
end;
function TfraVisorGraf.GetyDes: integer;
begin
  Result := visEdi.v2d.y_des;
end;
procedure TfraVisorGraf.SetyDes(AValue: integer);
begin
  visEdi.v2d.y_des:=AValue;
end;
function TfraVisorGraf.GetxCam: Single;
begin
  Result := visEdi.v2d.x_cam;
end;
procedure TfraVisorGraf.SetxCam(AValue: Single);
begin
  visEdi.v2d.x_cam:=AValue;
end;
function TfraVisorGraf.GetyCam: Single;
begin
  Result := visEdi.v2d.y_cam;
end;
procedure TfraVisorGraf.SetyCam(AValue: Single);
begin
  visEdi.v2d.y_cam:=AValue;
end;
function TfraVisorGraf.GetZoom: Single;
begin
  Result := visEdi.v2d.Zoom;
end;
procedure TfraVisorGraf.motEdiChangeView;
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec();
end;
procedure TfraVisorGraf.motEdiModif;
{Se ejecuta cuando el visor reporta cambios (dimensionamieno, posicionamiento, ...) en
 alguno de los objetos gráficos.}
begin
  Modif := true;
end;
procedure TfraVisorGraf.SetZoom(AValue: Single);
begin
  visEdi.v2d.Zoom:=AValue;
end;
procedure TfraVisorGraf.visEdiChangeState(VisState: TVisStateTyp);
begin
  if OnChangeState<>nil then OnChangeState(VisState);
end;
procedure TfraVisorGraf.visEdiMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  xv, yv: Single;
begin
  visEdi.v2d.XYvirt(X, Y, 0, xv, yv);
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, X, Y, xv, yv, 0);
end;
function TfraVisorGraf.GetAlfa: Single;
begin
  Result := visEdi.v2d.Alfa;
end;
procedure TfraVisorGraf.SetAlfa(AValue: Single);
begin
  visEdi.v2d.Alfa := AValue;
end;
function TfraVisorGraf.GetFi: Single;
begin
  REsult := visEdi.v2d.Fi;
end;
procedure TfraVisorGraf.SetFi(AValue: Single);
begin
  visEdi.v2d.Fi := AValue;
end;
procedure TfraVisorGraf.InicVista;
{Ubica la perspectiva y los ejes, de forma que el origen (0,0) aparezza en la
esquina inferior izquierda. Se debe llamar cuando ya el frame tenga su tamaño final}
begin
  visEdi.v2d.Alfa:=0;
  visEdi.v2d.Fi:=0;
  visEdi.v2d.Zoom:=0.5;
  //Ubica (0,0) a 10 pixeles del ángulo inferior izquierdo
  visEdi.v2d.x_cam:=((PaintBox1.Width div 2)-10)/visEdi.v2d.Zoom;
  visEdi.v2d.y_cam:=((PaintBox1.Height div 2)-10)/visEdi.v2d.Zoom;
end;
constructor TfraVisorGraf.Create(AOwner: TComponent; ListObjGraf: TlistObjGraf);
begin
  inherited Create(AOwner);
  objetos := ListObjGraf;  //recibe lista de objetos
  //objetos:= TlistObjGraf.Create(true);  //lista de objetos
  visEdi := TVisGraf3D.Create(PaintBox1, objetos);
  visEdi.OnModif:=@motEdiModif;
  visEdi.OnChangeView:=@motEdiChangeView;
  visEdi.OnMouseMove:=@visEdiMouseMove;
  visEdi.OnChangeState:=@visEdiChangeState;
//  self.OnKeyDown:=;
end;
destructor TfraVisorGraf.Destroy;
begin
  visEdi.Destroy;
  //objetos.Destroy;
  inherited;
end;

end.

