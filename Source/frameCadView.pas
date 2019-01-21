{                                frameVisCplex
Este Frame será usado para colocar nuestro editor gráfico. Incluye un PaintBox,
como salida gráfica.
Al ser un frame, puede incluirse en un formulario cualquiera.

                                              Por Tito Hinostroza  04/01/2017
}
unit frameCadView;
{$mode objfpc}{$H+}
interface
uses
  Classes, Forms, ExtCtrls, EditionMot3D, DefObjGraf;
type
  TOnObjetosElim = procedure of object;
  {Evento para movimiento del Mouse. Notar que además de las coordenaadas del ratón,
  proporciona coordenadas virtuales}
  TEveMouseVisGraf = procedure(Shift: TShiftState; xp, yp: Integer;
                                      xv, yv, zv: Single) of object;

  { TfraCadView }

  TfraCadView = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function GetAlfa: Single;
    function GetFi: Single;
    function GetxCam: Single;
    function GetyCam: Single;
    function GetZoom: Single;
    procedure motEdi_ChangeView;
    procedure visEdiSendPrompt(msg: string);
    procedure visEdi_Modif;
    procedure SetAlfa(AValue: Single);
    procedure SetFi(AValue: Single);
    procedure SetxCam(AValue: Single);
    procedure SetxDes(AValue: integer);
    function GetxDes: integer;
    procedure SetyCam(AValue: Single);
    procedure SetyDes(AValue: integer);
    function GetyDes: integer;
    procedure SetZoom(AValue: Single);
    procedure visEdi_ChangeState(VisState: TVisStateTyp);
    procedure visEdi_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure visEdi_SendMessage(msg: string);
  public
    objects : TObjGraphList; //Referencia a Lista de objetos
    ediMot  : TEditionMot3D; //Motor de edición  (La idesa es que pueda usarse más de uno)
    Modif   : Boolean;       //Bandera para indicar Diagrama Modificado
    OnObjetosElim  : TOnObjetosElim;   //cuando se elminan uno o más objetos
    OnCambiaPerspec: procedure of object; //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState  : TEvChangeState;  //Cambia el estado del Visor
    OnSendMessage  : TEvSendMessage;  //Envía un mensaje. Usado para respuesta a comandos
    OnSendPrompt   : TEvSendMessage;  //Envía una petición de comando.
    procedure RemoveAllObjects;
  public //Propiedades reflejadas
    property xDes: integer read GetxDes write SetxDes;
    property yDes: integer read GetyDes write SetyDes;
    property xCam: Single read GetxCam write SetxCam;
    property yCam: Single read GetyCam write SetyCam;
    property Alfa: Single read GetAlfa write SetAlfa;
    property Fi: Single read GetFi write SetFi;
    property Zoom: Single read GetZoom write SetZoom;
    procedure ExecuteCommand(command: string);
    function StateAsStr: string; //Cadena de descripción de estado
  public  //Inicialización
    procedure InicVista;
    constructor Create(AOwner: TComponent; ListObjGraf: TObjGraphList);
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

procedure TfraCadView.RemoveAllObjects;
//Elimina todos los objetos gráficos existentes
begin
  if objects.Count=0 then exit;  //no hay qué eliminar
  //elimina
  ediMot.UnselectAll();  //por si acaso hay algun simbolo seleccionado
  objects.Clear;          //limpia la lista de objects
  ediMot.RestoreState;
  Modif := true;          //indica que se modificó
  if OnObjetosElim<>nil then OnObjetosElim;
End;
function TfraCadView.GetxDes: integer;
begin
  Result := ediMot.v2d.x_des;
end;
procedure TfraCadView.SetxDes(AValue: integer);
begin
  ediMot.v2d.x_des:=AValue;
end;
function TfraCadView.GetyDes: integer;
begin
  Result := ediMot.v2d.y_des;
end;
procedure TfraCadView.SetyDes(AValue: integer);
begin
  ediMot.v2d.y_des:=AValue;
end;
function TfraCadView.GetxCam: Single;
begin
  Result := ediMot.v2d.x_cam;
end;
procedure TfraCadView.SetxCam(AValue: Single);
begin
  ediMot.v2d.x_cam:=AValue;
end;
function TfraCadView.GetyCam: Single;
begin
  Result := ediMot.v2d.y_cam;
end;
procedure TfraCadView.SetyCam(AValue: Single);
begin
  ediMot.v2d.y_cam:=AValue;
end;
function TfraCadView.GetZoom: Single;
begin
  Result := ediMot.v2d.Zoom;
end;
procedure TfraCadView.ExecuteCommand(command: string);
begin
  ediMot.ExecuteCommand(command);
end;
function TfraCadView.StateAsStr: string;
begin
  Result := ediMot.StateAsStr;
end;
procedure TfraCadView.motEdi_ChangeView;
begin
  if OnCambiaPerspec<>nil then OnCambiaPerspec();
end;
procedure TfraCadView.visEdi_Modif;
{Se ejecuta cuando el visor reporta cambios (dimensionamieno, posicionamiento, ...) en
 alguno de los objetos gráficos.}
begin
  Modif := true;
end;
procedure TfraCadView.SetZoom(AValue: Single);
begin
  ediMot.v2d.Zoom:=AValue;
end;
procedure TfraCadView.visEdi_ChangeState(VisState: TVisStateTyp);
begin
  if OnChangeState<>nil then OnChangeState(VisState);
end;
procedure TfraCadView.visEdi_MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  xv, yv: Single;
begin
  ediMot.v2d.XYvirt(X, Y, 0, xv, yv);
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, X, Y, xv, yv, 0);
end;
procedure TfraCadView.visEdi_SendMessage(msg: string);
begin
  if OnSendMessage<>nil then OnSendMessage(msg);
end;
procedure TfraCadView.visEdiSendPrompt(msg: string);
begin
  if OnSendPrompt<>nil then OnSendPrompt(msg);
end;

function TfraCadView.GetAlfa: Single;
begin
  Result := ediMot.v2d.Alfa;
end;
procedure TfraCadView.SetAlfa(AValue: Single);
begin
  ediMot.v2d.Alfa := AValue;
end;
function TfraCadView.GetFi: Single;
begin
  REsult := ediMot.v2d.Fi;
end;
procedure TfraCadView.SetFi(AValue: Single);
begin
  ediMot.v2d.Fi := AValue;
end;
procedure TfraCadView.InicVista;
{Ubica la perspectiva y los ejes, de forma que el origen (0,0) aparezza en la
esquina inferior izquierda. Se debe llamar cuando ya el frame tenga su tamaño final}
begin
  ediMot.v2d.Alfa:=0;
  ediMot.v2d.Fi:=0;
  ediMot.v2d.Zoom:=0.5;
  //Ubica (0,0) a 10 pixeles del ángulo inferior izquierdo
  ediMot.v2d.x_cam:=((PaintBox1.Width div 2)-10)/ediMot.v2d.Zoom;
  ediMot.v2d.y_cam:=((PaintBox1.Height div 2)-10)/ediMot.v2d.Zoom;
  ediMot.RestoreState;  //Para iniciar comando
end;
constructor TfraCadView.Create(AOwner: TComponent; ListObjGraf: TObjGraphList);
begin
  inherited Create(AOwner);
  objects := ListObjGraf;  //recibe lista de objects
  //objects:= TlistObjGraf.Create(true);  //lista de objects
  ediMot := TEditionMot3D.Create(PaintBox1, objects);
  ediMot.OnModif      :=@visEdi_Modif;
  ediMot.OnChangeView :=@motEdi_ChangeView;
  ediMot.OnMouseMove  :=@visEdi_MouseMove;
  ediMot.OnChangeState:=@visEdi_ChangeState;
  ediMot.OnSendMessage:=@visEdi_SendMessage;
  ediMot.onSendPrompt := @visEdiSendPrompt;
end;
destructor TfraCadView.Destroy;
begin
  ediMot.Destroy;
  //objetos.Destroy;
  inherited;
end;

end.

