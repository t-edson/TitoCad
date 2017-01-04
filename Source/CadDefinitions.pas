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

  { TCadProyecto }
  TCadPage = class
  private
  public
    nombre   : string;
    itemsGeom : TitemGraf_list;    //Lista de elementos gráficos
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;
  TCadPage_list = specialize TFPGObjectList<TCadPage>;

  TCadProyecto = class
  private
    fModific  : boolean;   //indica si ha sido modificado
    procedure SetModific(AValue: boolean);
  public
    nombre : string;
    creadoPor: string;
    notas    : string;
    unidades : Tunidades;
    OnModific : procedure of object; //Presupuesto modificado
    pages: TCadPage_list;
    property Modific: boolean read fModific write SetModific;
    function AgregPagina: TCadPage;
    procedure GuardarArchivo;
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TCadPage.Create;
begin
  itemsGeom := TitemGraf_list.Create(true);
end;
destructor TCadPage.Destroy;
begin
  itemsGeom.Destroy;
  inherited Destroy;
end;

{ TCadProyecto }
procedure TCadProyecto.SetModific(AValue: boolean);
begin
  if fModific=AValue then Exit;
  fModific:=AValue;
  if FModific then begin
    if OnModific<>nil then OnModific;  //evento
  end;
end;

function TCadProyecto.AgregPagina: TCadPage;
{Agrega una página al proyecto. Devuelve la referecnia a la pa´gina creada.}
var
  pag: TCadPage;
begin
  pag := TCadPage.Create;
  pag.nombre:='Página'+IntToStr(pages.Count+1);
  pages.Add(pag);
  Result := pag;
end;

procedure TCadProyecto.GuardarArchivo;
begin

end;
constructor TCadProyecto.Create;
begin
  pages:= TCadPage_list.Create(true);
  //Crea una página
  AgregPagina;
end;
destructor TCadProyecto.Destroy;
begin
  pages.Destroy;
  inherited Destroy;
end;

end.

