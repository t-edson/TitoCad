{                                frameVisCplex
Este Frame será usado para colocar nuestro editor gráfico. Requiere un  objeto TPaintBox,
como salida gráfica. Para que funcione como editor de objetos gráficos, debe crearse una
instancia de "TModEdicion" y darle la referencia del PaintBox.
Aquí también se deben poner las rutinas que permiten agregar los diversos objetos
gráficos con los que trabajará nuestra aplicación.

                                              Por Tito Hinostroza  11/05/2014
}
unit frameEditor;
{$mode objfpc}{$H+}
interface

uses
  Classes, Forms, ExtCtrls,
  ObjGraficos, VisGraf3D, DefObjGraf;
type
  TOnObjetosElim = procedure of object;

  { TfraGrafEditor }

  TfraGrafEditor = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function GetAlfa: Single;
    function GetFi: Single;
    function GetxCam: Single;
    function GetyCam: Single;
    function GetZoom: Single;
    procedure motEdiModif;
    procedure SetAlfa(AValue: Single);
    procedure SetFi(AValue: Single);
    procedure SetxCam(AValue: Single);
    procedure SetxDesp(AValue: integer);
    function GetxDesp: integer;
    procedure SetyCam(AValue: Single);
    procedure SetyDesp(AValue: integer);
    function GetyDesp: integer;
    procedure SetZoom(AValue: Single);
  public
    objetos : TlistObjGraf; //Lista de objetos
    motEdi  : TVisGraf3D;  //motor de edición
    Modif   : Boolean;      //bandera para indicar Diagrama Modificado
    OnObjetosElim: TOnObjetosElim;   //cuando se elminan uno o más objetos
    procedure AgregarObjGrafico(og: TObjGraf; AutoPos: boolean=true);
    procedure EliminarObjGrafico(obj: TObjGraf);
    procedure EliminarTodosObj;
    procedure ElimSeleccion;
    function AgregaObjeto: TMiObjeto;
  public //Propiedades reflejadas
    property xDesp: integer read GetxDesp write SetxDesp;
    property yDesp: integer read GetyDesp write SetyDesp;
    property xCam: Single read GetxCam write SetxCam;
    property yCam: Single read GetyCam write SetyCam;
    property Alfa: Single read GetAlfa write SetAlfa;
    property Fi: Single read GetFi write SetFi;
    property Zoom: Single read GetZoom write SetZoom;
  public  //Inicialización
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

procedure TfraGrafEditor.AgregarObjGrafico(og: TObjGraf; AutoPos: boolean = true);
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
    x := motEdi.v2d.Xvirt(100, 100) + 30 * objetos.Count Mod 400;
    y := motEdi.v2d.Yvirt(100, 100) + 30 * objetos.Count Mod 400;
    og.Ubicar(x,y);
  end;
  //configura eventos para ser controlado por este editor
  og.OnSelec   := @motEdi.ObjGraf_Select;     //referencia a procedimiento de selección
  og.OnDeselec := @motEdi.ObjGraf_Unselec;    //referencia a procedimiento de "de-selección"
  og.OnCamPunt := @motEdi.ObjGraf_SetPointer; //procedimiento para cambiar el puntero
//  Refrescar(s)   ;             //Refresca objeto
  objetos.Add(og);               //agrega elemento
end;
procedure TfraGrafEditor.EliminarObjGrafico(obj: TObjGraf);  //elimina un objeto grafico
begin
  Modif := True;  //Marca documento como modificado
  obj.Deselec;  //por si acaso
  objetos.Remove(obj);
  obj := nil;
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TfraGrafEditor.EliminarTodosObj;
//Elimina todos los objetos gráficos existentes
begin
  if objetos.Count=0 then exit;  //no hay qué eliminar
  //elimina
  motEdi.DeseleccionarTodos;  //por si acaso hay algun simbolo seleccionado
  objetos.Clear;          //limpia la lista de objetos
  motEdi.RestaurarEstado;
  Modif := true;          //indica que se modificó
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TfraGrafEditor.ElimSeleccion;
//Elimina la selección.
var
  v: TObjGraf;
  tmp: TOnObjetosElim;
begin
  tmp := OnObjetosElim;  //guarda evento
  OnObjetosElim := nil; //para evitar llamar muchas veces
  For v In motEdi.seleccion  do  //explora todos
    EliminarObjGrafico(v);
  OnObjetosElim := tmp;  //restaura
  if OnObjetosElim<>nil then OnObjetosElim;  //llama evento
  motEdi.Refrescar;
end;

function TfraGrafEditor.AgregaObjeto: TMiObjeto;
//Agrega un objeto de tipo TMiObjeto al editor.
var o: TMiObjeto;
begin
  o := TMiObjeto.Create(motEdi.v2d);
  AgregarObjGrafico(o);
  Result := o;
end;

function TfraGrafEditor.GetxDesp: integer;
begin
  Result := motEdi.v2d.x_des;
end;
procedure TfraGrafEditor.SetxDesp(AValue: integer);
begin
  motEdi.v2d.x_des:=AValue;
end;
function TfraGrafEditor.GetyDesp: integer;
begin
  Result := motEdi.v2d.y_des;
end;
procedure TfraGrafEditor.SetyDesp(AValue: integer);
begin
  motEdi.v2d.y_des:=AValue;
end;
function TfraGrafEditor.GetxCam: Single;
begin
  Result := motEdi.v2d.x_cam;
end;
procedure TfraGrafEditor.SetxCam(AValue: Single);
begin
  motEdi.v2d.x_cam:=AValue;
end;
function TfraGrafEditor.GetyCam: Single;
begin
  Result := motEdi.v2d.y_cam;
end;
procedure TfraGrafEditor.SetyCam(AValue: Single);
begin
  motEdi.v2d.y_cam:=AValue;
end;
function TfraGrafEditor.GetZoom: Single;
begin
  Result := motEdi.v2d.Zoom;
end;
procedure TfraGrafEditor.motEdiModif;
{Se ejecuta cuando el visor reporta cambios (dimensionamieno, posicionamiento, ...) en
 alguno de los objetos gráficos.}
begin
  Modif := true;
end;
procedure TfraGrafEditor.SetZoom(AValue: Single);
begin
  motEdi.v2d.Zoom:=AValue;
end;
function TfraGrafEditor.GetAlfa: Single;
begin
  Result := motEdi.v2d.Alfa;
end;
procedure TfraGrafEditor.SetAlfa(AValue: Single);
begin
  motEdi.v2d.Alfa := AValue;
end;
function TfraGrafEditor.GetFi: Single;
begin
  REsult := motEdi.v2d.Fi;
end;
procedure TfraGrafEditor.SetFi(AValue: Single);
begin
  motEdi.v2d.Fi := AValue;
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

constructor TfraGrafEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  objetos:= TlistObjGraf.Create(true);  //lista de objetos
  motEdi := TVisGraf3D.Create(PaintBox1, objetos);
  motEdi.v2d.Alfa:=0.7;
  motEdi.v2d.Fi:=0.7;
  motEdi.OnModif:=@motEdiModif;
end;

destructor TfraGrafEditor.Destroy;
begin
  motEdi.Destroy;
  objetos.Destroy;
  inherited;
end;

end.

