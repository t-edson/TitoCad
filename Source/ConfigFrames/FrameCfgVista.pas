unit FrameCfgVista;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, Menus,
  MiConfigBasic;

type

  { TfraCfgVista }

  TfraCfgVista = class(TFrame)
    chkVerEjes: TCheckBox;
    chkVerCoorPunt: TCheckBox;
    spnIncrWheel: TFloatSpinEdit;
    Label2: TLabel;
    spnLongEje: TFloatSpinEdit;
    Label1: TLabel;
    procedure chkVerEjesChange(Sender: TObject);
  public
    VerEjesCoor: boolean;
    LongEjeCoor: Double;
    VerCoorPunt: boolean;
    incrWheel  : Double;
    procedure Iniciar(cfgFile: TMiConfigBasic); //Inicia el frame
    procedure SetLanguage(lang: string);
  end;

implementation
{$R *.lfm}

procedure TfraCfgVista.chkVerEjesChange(Sender: TObject);
begin
  spnLongEje.Enabled := chkVerEjes.Checked;
  label1.Enabled:= chkVerEjes.Checked;
end;

procedure TfraCfgVista.Iniciar(cfgFile: TMiConfigBasic);
begin
  cfgFile.Asoc_Bol(self.Name + '/VerEjesCoor', @VerEjesCoor , chkVerEjes, true);
  cfgFile.Asoc_Dbl(self.Name + '/LongEjeCoor', @LongEjeCoor , spnLongEje, 100);
  cfgFile.Asoc_Bol(self.Name + '/VerCoorPunt', @VerCoorPunt,  chkVerCoorPunt, true);
  cfgFile.Asoc_Dbl(self.Name + '/incrWheel',   @incrWheel  , spnIncrWheel, 100);
  chkVerEjesChange(self);
end;

procedure TfraCfgVista.SetLanguage(lang: string);
begin

end;

end.

