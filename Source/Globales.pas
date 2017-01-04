unit Globales;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

const
  NOM_PROG = 'TitoCad';   //nombre de programa
  {$I ../version.txt}   //versión del programa

type
  TEvRefrescar = procedure of object;

  procedure VerifSalir(Aceptado, ErrorDatos: boolean; var CanClose: boolean);

implementation

procedure VerifSalir(Aceptado, ErrorDatos: boolean; var CanClose: boolean);
{Rutina de verificación para el evento OnCloseQuery(). Decide si activar o no la bandera
"CanClose", en base a los valores d "Aceptado" y "ErrorDatos".}
begin
  if Aceptado then begin
    //Se ha pulsado el botón ACEPTAR. Puede que haya habido error
    if ErrorDatos then begin
      //Se ha pulsado ACEPTAR pero hay errores, en los datos
      CanClose := false;  //para que no se cierre
      Aceptado := false;  //para poder cerrar luego con el botón [X] del formulario
    end else begin
      //No hubo errores con ACEPTAR
      //Se cierra normalmente
    end;
  end else begin
    //Se ha pulsado CANCELAR o el botón [X] del formulario
    //Se cierra normalmente
  end;
end;

end.

