program TitoCad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormPrincipal, FormConfig, FormControlVista, FormProject,
  FormViewProp, FormPageProp
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmControlVista, frmControlVista);
  Application.CreateForm(TfrmProject, frmProject);
  Application.CreateForm(TfrmViewProp, frmViewProp);
  Application.CreateForm(TfrmPageProp, frmPageProp);
  Application.Run;
end.

