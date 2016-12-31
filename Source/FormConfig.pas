{Unidad con formulario de configuración para manejar las propiedades de
 una aplicación. Está pensado para usarse con frames de la clase Tframe,
 definida en la unidad "PropertyFrame".
 }
unit FormConfig;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Forms, Graphics, SynEdit, Buttons, ComCtrls, StdCtrls,
  MisUtils, SynFacilCompletion,
  FrameCfgGeneral, FrameCfgVista, FrameCfgPanCom, FrameCfgSynEdit,
  MiConfigXML, MiConfigUtils, Classes;

type
  TEvCambiaProp = procedure of object;  //evento para indicar que hay cambio

  { TConfig }

  TConfig = class(TForm)
    bitAceptar: TBitBtn;
    bitAplicar: TBitBtn;
    bitCancel: TBitBtn;
    TreeView1: TTreeView;
    procedure bitAceptarClick(Sender: TObject);
    procedure bitAplicarClick(Sender: TObject);
    procedure bitCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    procedure cfgFilePropertiesChanges;
    procedure LeerDeVentana;
    procedure MostEnVentana;
  public
    msjError: string;
    edTerm  : TSynEdit;    //referencia al editor SynEdit
    edPCom  : TSynEdit;    //referencia al editor panel de comando
    edMacr  : TSynEdit;    //referencia al editor panel de comando
    edRemo  : TSynEdit;    //referencia al editor remoto
    //Frames de configuración
    fcGeneral : TfraCfgGeneral;
    fcVista   : TfraCfgVista;
    fcPanCom  : TfraCfgPanCom;   //Panel de comandos
    fcPanComEd: TfraCfgSynEdit;  //Editor de panel de comandos
    //Eventos
    OnPropertiesChanged: procedure of object;
    procedure Iniciar(hl0: TSynFacilComplet);
    procedure LeerArchivoIni;
    procedure escribirArchivoIni;
    procedure Configurar(Id: string='');
    function ContienePrompt(const linAct: string): integer;
    procedure SetLanguage(lang: string);
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

  { TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  fcGeneral := TfraCfgGeneral.Create(self);
  fcGeneral.Name:= 'General';  //Necesario para que genere su etiqueta en el XML
  fcGeneral.Parent := self;

  fcVista := TfraCfgVista.Create(self);
  fcVista.Name:='Vista';
  fcVista.Parent := self;

  fcPanCom := TfraCfgPanCom.Create(self);
  fcPanCom.Name:= 'PanComGen';
  fcPanCom.Parent := self;

  fcPanComEd := TfraCfgSynEdit.Create(self);
  fcPanComEd.Name := 'EdTer';  //Necesario para que genere su etiqueta en el XML
  fcPanComEd.parent := self;

  TreeView1.Items.Clear;  //Limpia árbol
  LinkFrameToTreeView(TreeView1, '1',   dic('General'), fcGeneral);
  LinkFrameToTreeView(TreeView1, '1.1', dic('Vista'), fcVista);
  LinkFrameToTreeView(TreeView1, '2',   dic('Panel de Comandos'), fcPanCom);
  LinkFrameToTreeView(TreeView1, '2.1', dic('General'), fcPanCom);
  LinkFrameToTreeView(TreeView1, '2.2', dic('Editor') , fcPanComEd);

  //selecciona primera opción
  TreeView1.Items[0].Selected:=true;
  TreeView1SelectionChanged(self);
  cfgFile.OnPropertiesChanges:=@cfgFilePropertiesChanges;
end;
procedure TConfig.FormDestroy(Sender: TObject);
begin
//  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  MostEnVentana;   //carga las propiedades en el frame
end;
procedure TConfig.cfgFilePropertiesChanges;
begin
  if OnPropertiesChanged<>nil then OnPropertiesChanged;
  fcPanComEd.ConfigEditor;  //para que actualice su editor
end;
procedure TConfig.Iniciar(hl0: TSynFacilComplet);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //inicia Frames
  fcGeneral.Iniciar(cfgFile);
  fcVista.Iniciar(cfgFile);
  fcPanCom.Iniciar(cfgFile);
  fcPanComEd.Iniciar(cfgFile, edTerm, clBlack);
  //lee parámetros del archivo de configuración.
  LeerArchivoIni;
end;
procedure TConfig.TreeView1SelectionChanged(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  ShowFrameOfNode(self, Treeview1.Selected, 145, 0);
end;

procedure TConfig.bitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if cfgFile.MsjErr<>'' then exit;  //hubo error
  self.Close;   //porque es modal
end;
procedure TConfig.bitAplicarClick(Sender: TObject);
begin
  LeerDeVentana;       //Escribe propiedades de los frames
  if cfgFile.MsjErr<>'' then begin
    msgerr(cfgFile.MsjErr);
    exit;
  end;
  escribirArchivoIni;   //guarda propiedades en disco
  if edTerm<>nil then edTerm.Invalidate;     //para que refresque los cambios
  if edPCom<>nil then edPCom.Invalidate;     //para que refresque los cambios
end;
procedure TConfig.bitCancelClick(Sender: TObject);
begin
  self.Hide;
end;
procedure TConfig.Configurar(Id: string='');
//Muestra el formulario, de modo que permita configurar la sesión actual
var
  it: TTreeNode;
begin
  if Id<> '' then begin  /////se pide mostrar un Id en especial
    //oculta los demás
    it := TTreeNodeFromId(Id,TreeView1);
    if it <> nil then it.Selected:=true;
    TreeView1SelectionChanged(self);
  end else begin ////////muestra todos
    for it in TreeView1.Items do begin
      it.Visible:=true;
    end;
  end;
  Showmodal;
end;

function TConfig.ContienePrompt(const linAct: string): integer;
{Verifica si una cadena contiene al prompt. La verificación es simple, compara solo
el inicio de de la cadena.
Si la cadena contiene al prompt, devuelve la longitud del prompt hallado, de otra forma
devuelve cero.
Se usa para el resaltador de sintaxis y el manejo de pantalla.}
var
  l: Integer;
  p: SizeInt;
begin
   l := length(fcPanCom.Prompt);  //El prompt lo define la configuración
   if l=0 then exit(0);
   if copy(linAct,1,l) = fcPanCom.Prompt then begin
     Result := length(fcPanCom.Prompt);  //el tamaño del prompt
   end else begin
     Result := 0;
   end;
end;

procedure TConfig.LeerDeVentana;
//Lee las propiedades de la ventana de configuración.
begin
  cfgFile.WindowToProperties;   //puede generar error
end;
procedure TConfig.MostEnVentana;
//Muestra las propiedades en la ventana de configuración.
begin
  cfgFile.PropertiesToWindow;  //puede generar error
end;
procedure TConfig.LeerArchivoIni;
begin
  cfgFile.FileToProperties;
end;

procedure TConfig.escribirArchivoIni;
//Escribe el archivo de configuración
begin
  cfgFile.PropertiesToFile;
end;

procedure TConfig.SetLanguage(lang: string);
//Rutina de traducción
begin
  fcGeneral.SetLanguage(lang);
  fcVista.SetLanguage(lang);
  fcPanCom.SetLanguage(lang);
  fcPanComEd.SetLanguage(lang);

  case lowerCase(lang) of
  'es': begin
//      TTreeNodeFromId('1',TreeView1).Text:='Conexión';
//      TTreeNodeFromId('1.1',TreeView1).Text:='General';
//      TTreeNodeFromId('1.2',TreeView1).Text:='Detec.de Prompt';
//      TTreeNodeFromId('1.3',TreeView1).Text:='Rutas/Archivos';
//      TTreeNodeFromId('2',TreeView1).Text:='Terminal';
//      TTreeNodeFromId('2.1',TreeView1).Text:='Pantalla';
//      TTreeNodeFromId('2.2',TreeView1).Text:='Editor';
//      TTreeNodeFromId('2.3',TreeView1).Text:='Comando Recurrente';
    end;
  'en': begin
//      TTreeNodeFromId('1',TreeView1).Text:='Connection';
//      TTreeNodeFromId('1.1',TreeView1).Text:='General';
//      TTreeNodeFromId('1.2',TreeView1).Text:='Prompt detection';
//      TTreeNodeFromId('1.3',TreeView1).Text:='Paths/Files';
//      TTreeNodeFromId('2',TreeView1).Text:='Terminal';
//      TTreeNodeFromId('2.1',TreeView1).Text:='Screen';
//      TTreeNodeFromId('2.2',TreeView1).Text:='Editor';
//      TTreeNodeFromId('2.3',TreeView1).Text:='Recurring command';
    end;
  end;
end;

end.

