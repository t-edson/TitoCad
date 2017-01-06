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
  public
    objetos : TlistObjGraf; //Lista de objetos
    visEdi  : TVisGraf3D;  //motor de edición  (La idesa es que pueda haber más de uno)
    Modif   : Boolean;      //bandera para indicar Diagrama Modificado
    OnObjetosElim: TOnObjetosElim;   //cuando se elminan uno o más objetos
    OnCambiaPerspec: procedure of object; //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
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
{procedure TVisGraf3D.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//Procesa el evento KeyDown()
//var
//  v: TObjGraf;
begin
  If Shift = [] Then begin  //********************* Teclas normales ***********************
      //If tec = 13 Then PropiedSeleccion ;  //Debe procesarlo el diagrama
      If Key = VK_DELETE Then ElimSeleccion;  //DELETE
      If Key = 9 Then SeleccionarSiguiente;  //TAB
      If Key = 27 Then begin  //ESCAPE
          DeseleccionarTodos;
          Refrescar;
      end;
      If seleccion.Count = 0 Then     ;  //si no hay objetos seleccionados
          If Key = 37 Then Call moverDerecha(DESPLAZ_MENOR)        ;  //derecha
          If Key = 39 Then Call moverIzquierda(DESPLAZ_MENOR)      ;  //izquierda
          If Key = 40 Then Call moverArriba(DESPLAZ_MENOR)         ;  //arriba
          If Key = 38 Then Call moverAbajo(DESPLAZ_MENOR)          ;  //abajo
      Else        ;  //hay seleccionados
          If Key = 37 Then ;  //derecha
              For Each v In seleccion
                  If Not v.Bloqueado Then v.X = v.X - DESPLAZ_MENOR
              Next
              Call Refrescar
          End If
          If Key = 39 Then ;  //izquierda
              For Each v In seleccion
                  If Not v.Bloqueado Then v.X = v.X + DESPLAZ_MENOR
              Next
              Call Refrescar
          End If
          If Key = 40 Then ;  //arriba
              For Each v In seleccion
                  If Not v.Bloqueado Then v.Y = v.Y + DESPLAZ_MENOR
              Next
              Call Refrescar
          End If
          If Key = 38 Then ;  //abajo
              For Each v In seleccion
                  If Not v.Bloqueado Then v.Y = v.Y - DESPLAZ_MENOR
              Next
              Call Refrescar
          End If
      end If
  end else If Shift = [ssShift] Then begin //**********************Shift + ************************
      If Key = 9 Then Call SeleccionarAnterior              ;  //TAB
  end else If Shift = [ssCtrl] Then begin  //**********************Ctrl + ************************
      If Key = 107 Then Call AmpliarClick      ;  //+
      If Key = 109 Then Call ReducirClick      ;  //-
      If Key = 37 Then Call moverDerecha(DESPLAZ_MAYOR)   ;  //derecha
      If Key = 39 Then Call moverIzquierda(DESPLAZ_MAYOR) ;  //izquierda
      If Key = 40 Then Call moverArriba(DESPLAZ_MAYOR)    ;  //arriba
      If Key = 38 Then Call moverAbajo(DESPLAZ_MAYOR)     ;  //abajo
  end else If Shift = [ssShift, ssCtrl] Then  begin  //******************Shift + Ctrl*************************
    picSal.MousePointer := vbSizeAll;  //indica modo Zoom + desplazamiento
  end;
end;}

constructor TfraVisorGraf.Create(AOwner: TComponent; ListObjGraf: TlistObjGraf);
begin
  inherited Create(AOwner);
  objetos := ListObjGraf;  //recibe lista de objetos
  //objetos:= TlistObjGraf.Create(true);  //lista de objetos
  visEdi := TVisGraf3D.Create(PaintBox1, objetos);
  visEdi.v2d.Alfa:=0.7;
  visEdi.v2d.Fi:=0.7;
  visEdi.OnModif:=@motEdiModif;
  visEdi.OnChangeView:=@motEdiChangeView;
end;

destructor TfraVisorGraf.Destroy;
begin
  visEdi.Destroy;
  //objetos.Destroy;
  inherited;
end;

end.
