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
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Graphics, GraphType, lclType,
  dialogs, lclProc, ObjGraficos, ogMotEdicion;

type

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
    procedure motEdiMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
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
    motEdi: TModEdicion;  //motor de edición
    function AgregaObjeto: TMiObjeto;
    property xDesp: integer read GetxDesp write SetxDesp;
    property yDesp: integer read GetyDesp write SetyDesp;
    property xCam: Single read GetxCam write SetxCam;
    property yCam: Single read GetyCam write SetyCam;
    property Alfa: Single read GetAlfa write SetAlfa;
    property Fi: Single read GetFi write SetFi;
    property Zoom: Single read GetZoom write SetZoom;
  public
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

function TfraGrafEditor.AgregaObjeto: TMiObjeto;
//Agrega un objeto de tipo TMiObjeto al editor.
var o: TMiObjeto;
begin
  o := TMiObjeto.Create(motEdi.v2d);
  motEdi.AgregarObjGrafico(o);
  Result := o;
end;

procedure TfraGrafEditor.motEdiMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  xv, yv: Single;
begin
//  motEdi.v2d.XYvirt(x,y,0,xv, yv);
//  motEdi.v2d.Line(xv-10,yv,0,xv+10,yv,0);
//  motEdi.v2d.Line(xv,yv-10,0,xv,yv+10,0);
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
constructor TfraGrafEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  motEdi := TModEdicion.Create(PaintBox1);
  motEdi.v2d.Alfa:=0.7;
  motEdi.v2d.Fi:=0.7;
  motEdi.OnMouseMove:=@motEdiMouseMove;
end;

destructor TfraGrafEditor.Destroy;
begin
  motEdi.Destroy;
  inherited;
end;

end.

