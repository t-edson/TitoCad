unit FrameCfgGeneral;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Menus,
  MiConfigBasic;

type
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  { TfraCfgGeneral }
  TfraCfgGeneral = class(TFrame)
    chkVerBarEst: TCheckBox;
    chkVerBarHer: TCheckBox;
    RadioGroup1: TRadioGroup;
  public
    VerBarHer: boolean;
    StateToolbar: TStyleToolbar;
    VerBarEst: boolean;
    VerInspVar: boolean;
    procedure Iniciar(cfgFile: TMiConfigBasic); //Inicia el frame
    procedure SetLanguage(lang: string);
  end;

implementation
{$R *.lfm}

{ TfraCfgGeneral }
procedure TfraCfgGeneral.Iniciar(cfgFile: TMiConfigBasic);
begin
  cfgFile.Asoc_Bol(self.Name + '/VerBarHer', @VerBarHer  ,  chkVerBarHer, true);
  cfgFile.Asoc_Enum(self.Name + '/StateStatusbar',@StateToolbar, SizeOf(TStyleToolbar), RadioGroup1, 1);
  cfgFile.Asoc_Bol(self.Name + '/VerBarEst', @VerBarEst  ,  chkVerBarEst, true);
end;

procedure TfraCfgGeneral.SetLanguage(lang: string);
begin

end;

end.

