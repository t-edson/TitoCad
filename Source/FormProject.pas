unit FormProject;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel, Buttons, ComCtrls, Spin, Menus, MisUtils,
  BasicGrilla, CadDefinitions, Globales;
type
  { TfrmProject }
  TfrmProject = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    txtNombre: TEdit;
    Label5: TLabel;
    txtCreadoPor: TEdit;
    Label8: TLabel;
    txtNotas: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    Aceptado  : boolean;   //Indica que se ha pulsado el botón ACEPTAR
    ErrorDatos: boolean;   //Indica que hubo error de datos al Aceptar
    presup    : TCadProject; //Referencia al presupuesto
    procRefresc: TEvRefrescar;
  public
    function Exec(presup0: TCadProject; procRefrescar: TEvRefrescar;
      soloLect: boolean=false): boolean;
    function ExecNew(presup0: TCadProject): boolean;
  end;

var
  frmProject: TfrmProject;

implementation
{$R *.lfm}
{ TfrmProject }
procedure TfrmProject.FormCreate(Sender: TObject);
begin
  Aceptado := false;
end;
procedure TfrmProject.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  VerifSalir(Aceptado, ErrorDatos, CanClose);
end;
procedure TfrmProject.OKButtonClick(Sender: TObject);
begin
  //Validaciones
  if trim(txtNombre.Text) = '' then begin
    MsgExc('Error en Cliente: ' + txtNombre.Text);
    txtNombre.Visible:=true;
    PageControl1.ActivePage := TTabSheet(txtNombre.Parent);  //activa página
    txtNombre.SetFocus;
    ErrorDatos := true;
    Aceptado  := true;
    exit;
  end;
  //Asignación
  presup.name   := txtNombre.Text;
  presup.notes    := txtNotas.Text;
  presup.author:= txtCreadoPor.TextHint;
  Aceptado := true;
  //self.Hide;
end;
procedure TfrmProject.CancelButtonClick(Sender: TObject);
begin
  Aceptado := false;
end;

function TfrmProject.Exec(presup0: TCadProject; procRefrescar: TEvRefrescar;
  soloLect: boolean): boolean;
begin
  presup := presup0;
  procRefresc := procRefrescar;

  txtNombre.Text   := presup.name;
  txtCreadoPor.Text:= presup.author;
  txtNotas.Text    := presup.notes;

  ButtonPanel1.OKButton.Enabled := not soloLect;

  Self.ShowModal;  //se muestra modal
  Result := Aceptado;
end;
function TfrmProject.ExecNew(presup0: TCadProject): boolean;
{Abre la ventana y la configura de modo apropiado, de modo que la ventana permita configurar
las propiedades iniciales de un nuevo presupuesto.}
var
  maxord: String;
begin
  maxord := '1'; //frmAbrirPresup.LeerMaxOrdinalPresup;
  //configura valores iniciales
  presup0.name := 'Proyecto' + maxord;
  //Abre ventana de propiedades
  Result := Exec(presup0, nil);
end;

end.

