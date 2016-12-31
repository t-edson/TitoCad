program TitoCad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormPrincipal, EvalExpres, FormConfig, FrameCfgSynEdit,
  FrameCfgGeneral, FrameCfgPanCom, FormControlVista, FrameCfgVista, FormProject
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmControlVista, frmControlVista);
  Application.CreateForm(TfrmProject, frmProject);
  Application.Run;
end.

