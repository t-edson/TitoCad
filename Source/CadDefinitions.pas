unit CadDefinitions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl;
type

  Tunidades = (
    tmuMetros,
    tmuPies
  );

  //Tipo de tablero
  TDipTipTab = (
    ttAutosoport,  //Autosoportado
    ttEmpotrado,   //Empotrado
    ttAdosado      //Adosado
  );

  TItemGraf = class
    nombre: string;
    tipTab: TDipTipTab; //tipo de tablero
  end;
  TitemGraf_list = specialize TFPGObjectList<TItemGraf>;

  { TMinProyecto }
  TMinProyecto = class
  private
    fModific  : boolean;   //indica si ha sido modificado
    procedure SetModific(AValue: boolean);
  public
    nombre   : string;
    creadoPor: string;
    notas    : string;
    unidades : Tunidades;
    OnModific : procedure of object; //Presupuesto modificado
    itemsGeom : TitemGraf_list;    //Lista de elementos gráficos
    property Modific: boolean read fModific write SetModific;
    procedure GuardarArchivo;
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMinProyecto }

procedure TMinProyecto.SetModific(AValue: boolean);
begin
  if fModific=AValue then Exit;
  fModific:=AValue;
  if FModific then begin
    if OnModific<>nil then OnModific;  //evento
  end;
end;

procedure TMinProyecto.GuardarArchivo;
begin

end;

constructor TMinProyecto.Create;
begin
  itemsGeom := TitemGraf_list.Create(true);
end;

destructor TMinProyecto.Destroy;
begin
  itemsGeom.Destroy;
  inherited Destroy;
end;

end.

