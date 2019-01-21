{Formulario de propiedades de un objeto Vista.}
unit FormViewProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin, CadDefinitions, frameCadView;
type

  { TfrmViewProp }

  TfrmViewProp = class(TForm)
    btnAplicar: TBitBtn;
    btnCancelar: TBitBtn;
    btnAceptar: TBitBtn;
    chkVerEjes: TCheckBox;
    chkVerPtoGiro: TCheckBox;
    chkVerCuadric: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    spnLongEje: TSpinEdit;
    procedure btnAceptarClick(Sender: TObject);
    procedure btnAplicarClick(Sender: TObject);
  private
    vista: TfraCadView;
  public
    procedure Exec(vista0: TfraCadView);
  end;

var
  frmViewProp: TfrmViewProp;

implementation
{$R *.lfm}

{ TfrmViewProp }

procedure TfrmViewProp.btnAceptarClick(Sender: TObject);
begin
  btnAplicarClick(self);
end;

procedure TfrmViewProp.btnAplicarClick(Sender: TObject);
begin
  vista.ediMot.VerEjesCoor  := chkVerEjes.Checked;
  vista.ediMot.LonEjesCoor  := spnLongEje.Value;
  vista.ediMot.VerPuntoGiro := chkVerPtoGiro.Checked;
  vista.ediMot.VerCuadric   := chkVerCuadric.Checked;
  vista.ediMot.Refresh;
end;

procedure TfrmViewProp.Exec(vista0: TfraCadView);
begin
  vista := vista0;
  chkVerEjes.Checked    := vista.ediMot.VerEjesCoor;
  spnLongEje.Value      := vista.ediMot.LonEjesCoor;
  chkVerPtoGiro.Checked := vista.ediMot.VerPuntoGiro;
  chkVerCuadric.Checked := vista.ediMot.VerCuadric;
  Showmodal;
end;

end.

