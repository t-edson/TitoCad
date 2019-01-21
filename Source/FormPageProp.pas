{Formulario de propiedades de un objeto página.}
unit FormPageProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Buttons, CadDefinitions;

type

  { TfrmPageProp }

  TfrmPageProp = class(TForm)
    btnAceptar: TBitBtn;
    btnAplicar: TBitBtn;
    btnCancelar: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure btnAceptarClick(Sender: TObject);
    procedure btnAplicarClick(Sender: TObject);
  private
    page: TCadPage;
  public
    procedure Exec(page0: TCadPage);
  end;

var
  frmPageProp: TfrmPageProp;

implementation
{$R *.lfm}

procedure TfrmPageProp.btnAceptarClick(Sender: TObject);
begin

end;

procedure TfrmPageProp.btnAplicarClick(Sender: TObject);
begin

end;

procedure TfrmPageProp.Exec(page0: TCadPage);
begin
  page := page0;
  Edit1.Text  := page.name;
  Label2.Caption := 'Número de objetos ' + IntToStr(page.objects.Count);
  Showmodal;
end;

end.

