{Formulario para controlar la vista de un objeto "frameGrafEditor"}
unit FormControlVista;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, frameEditor;
type

  { TfrmControlVista }

  TfrmControlVista = class(TForm)
    btnFijar: TButton;
    btnLeer: TButton;
    btnLimpiar: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    spnYdesp: TSpinEdit;
    spnXdesp: TSpinEdit;
    spnAlfa: TFloatSpinEdit;
    spnXcam: TFloatSpinEdit;
    spnFi: TFloatSpinEdit;
    spnYcam: TFloatSpinEdit;
    spnZoom: TFloatSpinEdit;
    Timer1: TTimer;
    procedure btnFijarClick(Sender: TObject);
    procedure btnLeerClick(Sender: TObject);
    procedure btnLimpiarClick(Sender: TObject);
    procedure spnAlfaClick(Sender: TObject);
    procedure spnAlfaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    fraEditor: TfraGrafEditor;
  public
    procedure Exec(fraEditor0: TfraGrafEditor);
  end;

var
  frmControlVista: TfrmControlVista;

implementation
{$R *.lfm}

procedure TfrmControlVista.btnLeerClick(Sender: TObject);
begin
  spnXdesp.Value:= fraEditor.xDesp;
  spnYdesp.Value:= fraEditor.yDesp;

  spnXcam.Value := fraEditor.xCam;
  spnYcam.Value := fraEditor.yCam;

  spnAlfa.Value := fraEditor.Alfa;
  spnFi.Value   := fraEditor.Fi;
  spnZoom.Value := fraEditor.Zoom;

end;
procedure TfrmControlVista.btnLimpiarClick(Sender: TObject);
begin
  spnAlfa.Value := 0;
  spnFi.Value := 0;
  spnZoom.Value := 1;
  btnFijarClick(self);
  fraEditor.PaintBox1.Invalidate;
end;
procedure TfrmControlVista.btnFijarClick(Sender: TObject);
begin
  fraEditor.xDesp := spnXdesp.Value;
  fraEditor.yDesp := spnYdesp.Value;

  fraEditor.xCam  := spnXcam.Value;
  fraEditor.yCam  := spnYcam.Value;

  fraEditor.Alfa:=spnAlfa.Value;
  fraEditor.Fi:=spnFi.Value;
  fraEditor.Zoom:=spnZoom.Value;
  fraEditor.PaintBox1.Invalidate;
end;

procedure TfrmControlVista.spnAlfaClick(Sender: TObject);
begin
  btnFijarClick(self);
end;
procedure TfrmControlVista.spnAlfaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  btnFijarClick(self);
end;

procedure TfrmControlVista.Timer1Timer(Sender: TObject);
begin
//  btnLeerClick(self);  //actualiza
end;

procedure TfrmControlVista.Exec(fraEditor0: TfraGrafEditor);
begin
  fraEditor:= fraEditor0;
  self.Show;
end;

end.

