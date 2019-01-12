unit FrameComPanel;
{Frame que define un panel de comandos, orinetado a recibir comandos cortor de una sola
línea con respuestas igualmenet cortas.
Se compone de dos zonas:
+---------------------------------+
|                                 |
|  Zona de Historial de comandos  |
|                                 |
+---------------------------------+
|  Zona de comando actual         |
+---------------------------------+

La zona de comando actual, suele tener un "prompt" fijo y permite escribir la
respuesta o comando.
La zona de Historial puede contener 0 o más líneas, mientras que la zona de comando
actual, es de solo una línea.

Cada vez que se pulsa <Enter>, se pasa la línea actual a la zona de historial
y se produce el evento: OnComReturn, que tiene como parámetros:

* txtPrompt -> El prompt de la línea de comando cuandos e pulsó <enter>.
* txtAnswer -> La respuesta que se escribió en el prompt.
* setPrompt -> Indica si se debe fijar un nuevo prompt.
* newPrompt -> Permite escribir un nuevo prompt en la zona de comandos.

Para escribir el primer prompt, se puede acceder al método AddPrompt().

El prompt se escribe por defecto con fondo coloreado.

Para cambiar los atributos del texto en pantalla, se puede cambiar los campos:

  textColor  : TColor;   //Color del texto del terminal
  prmpColor  : TColor;   //Color del texto del prompt
  prmpColorBk: TColor;   //Color del fondo deñ prompt

Cuando se fija un "prompt", sea por el método AddPrompt(), o como respuesta al
evento OnComReturn, se admiten ciertos caracteres especiales para dibujar caracteres
especiales. Estos son:

"_" -> No dibuja fondo ni escribe ningún caracter.
">" -> Dibuja una flecha apuntando a la derecha.
"<" -> Dibuja una flecha (invertida) apuntando a la derecha.

Por ejemplo, un prompt con diseño, podría definrise con:

cp.AddPrompt('_>Comando>_');

Un programa típico podría ser:

procedure TForm1.FormCreate(Sender: TObject);
begin
   cp := TfraComPanel.Create(self);
   cp.Parent := self;
   cp.Align := alClient;
   cp.OnComReturn := @cpEnter;
   cp.ErrorString:= 'Error:'
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  cp.AddPrompt(' Comando>_');
  cp.SetFocus;
end;

procedure TForm1.cpEnterKey(txtPrompt, txtAnswer: string);
begin
  if txtAnswer = 'help' then begin
    cp.AddLine('Error: No hay ayuda');
  end else begin
    cp.AddLine('¿Qué es: ' + txtAnswer + '?');
  end;
  cp.AddPrompt(' Comando>_');
end;

}
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  LCLType, LCLProc;

type

  { TfraComPanel }

  TfraComPanel = class(TFrame)
    Timer1: TTimer;
    procedure FrameClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    tic: integer;   //Contador para temporización
    procedure DrawLineHistory(n: integer; yPos: integer);
    procedure DrawLineCommand(txt: string; yPos: integer);
    procedure Frame1Enter(Sender: TObject);
    procedure Frame1Exit(Sender: TObject);
    procedure Frame1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Frame1KeyPress(Sender: TObject; var Key: char);
    procedure SetCursorPos(newPosX, newPosY: integer);
  protected
    lines     : TStringList;
    buffText  : string;    //Texto de la última línea
    posX, posY: integer; //Coord. del cursor
    cursorOn  : boolean; //Bandera para haver visible al cursor
  protected  //Geometría del panel
    fontHeight: Integer;   //Tamaño de letra
    fontWidth : Integer;   //Ancho de letra
    lineHeight: Integer;   //Altura de línea en pantalla
    nRows     : Integer;   //Número de líneas en pantalla
    rowOffset : Integer;   //Pixeles sobrantes en pantalla
    rowEnd    : Integer;   //Línea inicial a dibujar
    rowBegin  : Integer;   //Línea final a dibujar
    prompt     : string;   //Texto del prompt.
    procedure Paint; override;
  public
    errorColor : TColor;   //Color del texto del terminal con error
    textColor  : TColor;   //Color del texto del terminal
    prmpColor  : TColor;   //Color del texto del prompt
    prmpColorBk: TColor;   //Color del fondo deñ prompt
    fontSize   : Integer;  //Tamaño de la letra del terminal
    fontName   : string;   //Nombre de la fuente
    timerCursor: integer;  //Periodo de parpadeo del cursor
    ErrorString: string;
    OnComReturn : procedure( //Cuando se pulsa <Enter>
      txtPrompt, txtAnswer: string) of object;
    OnComKeyDown: procedure(  //Reemplaza al evento OnKeyDown
                    Sender: TObject; var Key: Word; Shift: TShiftState) of object;
    procedure AddLine(txt: string);
    procedure AddPrompt(txt: string);
    procedure SetFont;
    procedure SetFont(fColor: TColor);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfraComPanel }

procedure TfraComPanel.Timer1Timer(Sender: TObject);
begin
  inc(tic);
  if tic > timerCursor then tic := 0;
  //Refresco de cursor
  if (tic = timerCursor) and self.Focused then begin
    //Refresca cursor
    cursorOn := not cursorOn;  //Genera parpadeo
    Invalidate;
  end;
end;
procedure TfraComPanel.DrawLineHistory(n: integer; yPos: integer);
{Dibuja la línea "n" de lines[].}
begin
  Canvas.Brush.Color:=clBlack;
  Canvas.FillRect(0, yPos, self.Width, yPos + lineHeight);  //Fondo
  if n>lines.Count-1 then begin
    exit;
  end;
  if n<0 then begin
    exit;
  end;
  //Hay datos de lines[]
//debugln('DrawLine1[%d]: textColor = %d', [n,textColor]);
//if textColor = 255 then RaiseAndCatchException();
  if pos(ErrorString, lines[n]) <> 0 then begin
    //SetFont(errorColor);
    Canvas.Font.Color:= errorColor;
  end else begin
    //SetFont(textColor);
    Canvas.Font.Color:= textColor;
  end;
//debugln('DrawLine2[%d]: textColor = %d', [n,textColor]);
  Canvas.TextOut(0, yPos, lines[n]);  //Texto
end;
procedure TfraComPanel.DrawLineCommand(txt: string; yPos: integer);
{Dibuja la línea de comnados.}
  procedure CharTriangleRight(xPos, yPos: integer);
  {Dibuja le caracter triángulo mirando a la derecha: |>}
  begin
    Canvas.Polygon([Point(xPos, yPos),
                    Point(xPos+fontWidth, yPos + fontHeight div 2),
                    Point(xPos, yPos + lineHeight)]);
  end;
  procedure CharTriangleLeft(xPos, yPos: integer);
  {Dibuja le caracter triángulo mirando a la derecha: >|}
  begin
    Canvas.Polygon([Point(xPos, yPos),
                    Point(xPos+fontWidth, yPos),
                    Point(xPos+fontWidth, yPos + fontHeight),
                    Point(xPos, yPos + fontHeight),
                    Point(xPos+fontWidth, yPos + fontHeight div 2)
                   ]);
  end;
var
  xCur: Integer;
  xCar : Integer;
  c: Char;
  remain: String;
begin
  //Fondo
  Canvas.Brush.Color:=clBlack;
  Canvas.FillRect(0, yPos, self.Width, yPos + lineHeight);  //Fondo
  if prompt<>'' then begin  //Hay AddPrompt.
    //Dibuja el prompt con atributos de color
    Canvas.Brush.Color := prmpColorBk;
    Canvas.Pen.Color := prmpColorBk;
    Canvas.Font.Color := prmpColor;
    //Dibuja caracter por caracter
    xCar := 0;
    for c in prompt do begin
      if c = '>' then begin
        //Caracter especial
        CharTriangleRight(xCar, yPos);
      end else if c = '<' then begin
        //Caracter especial
        CharTriangleLeft(xCar, yPos);
      end else if c = '_' then begin
        //Caracter especial
        //No imprime nada ni el fondo
      end else begin
        //Caracter común
        Canvas.FillRect(xCar, yPos, xCar+fontWidth, yPos + fontHeight);
        Canvas.TextOut(xCar, yPos, c);  //Texto
      end;
      inc(xCar, fontWidth);
    end;
    //Escribe texto restante
    Canvas.Font.Color  := textColor;
    //SetFont(textColor);
    Canvas.Brush.Style := bsClear;   //Sin fondo
    remain := copy(txt, length(prompt)+1, length(txt));
    Canvas.TextOut(xCar, yPos, remain);  //Texto
  end else begin
    Canvas.TextOut(0, yPos, txt);  //Texto
  end;
  if cursorOn then begin
    //Dibuje cursor
    xCur := (posX-1) * fontWidth;
    Canvas.Brush.Color:=$00AA00;
    Canvas.FillRect(xCur, yPos, xCur+fontWidth, yPos + lineHeight);
  end;
end;
procedure TfraComPanel.Paint;
var
  i, yLin: Integer;
begin
  with Canvas do begin
    //Draw History of commands
    SetFont;
    yLin := rowOffset - lineHeight;  //Dejará la útlima línea vacía
    for i:=0 to nRows-1 do begin
      //Line(0, yLin, 300, yLin);
      DrawLineHistory(rowBegin+i, yLin);
      yLin += lineHeight;
    end;
    //Draw Command area
    DrawLineCommand(buffText, yLin);
  end;
  inherited Paint;
end;
procedure TfraComPanel.SetCursorPos(newPosX, newPosY: integer);
begin
  posX := newPosX;
  posY := newPosY;
  //Protección
  if posX < length(prompt)+1 then posX := length(prompt)+1;
  if PosX > length(buffText) + 1 then PosX := length(buffText) + 1;
  //Hace cursosr visible para que se note el probable cambio de posición
  cursorOn := true;
  tic := 0; ///Para iniciar nueva cuenta
  Invalidate;
end;
procedure TfraComPanel.Frame1Enter(Sender: TObject);
begin
//  AddLine('Enter');
end;
procedure TfraComPanel.Frame1Exit(Sender: TObject);
begin
//  AddLine('Exit');
end;
procedure TfraComPanel.Frame1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  newPromp, oldAnswer, oldPrompt: String;
begin
  case Key of
  VK_LEFT: begin
    if Shift=[] then begin
      SetCursorPos(posX-1, posY);
      Key := 0; //Para que no cambie el enfoque
    end else if Shift=[ssCtrl] then begin
      if posX <= 1 then exit;  //Ya está al inicio
      if buffText[posX-1]=' ' then dec(posX);
      if posX <= 1 then exit;  //No se puede hacer nada
      if buffText[posX]=' ' then begin
        //Está en espacio
        while (posX>1) and (buffText[posX]=' ') do begin
          dec(posX)
        end;
        //Está en otro caracter
        while (posX>1) and (buffText[posX]<>' ') do begin
          dec(posX)
        end;
        inc(posX);   //Corrige posición
      end else begin
        //Está en otro caracter
        while (posX>1) and (buffText[posX]<>' ') do begin
          dec(posX)
        end;
        inc(posX);   //Corrige posición
      end;
      SetCursorPos(posX, posY);
    end;
  end;
  VK_RIGHT: begin
    if Shift=[] then begin
      SetCursorPos(posX+1, posY);
      Key := 0; //Para que no cambie el enfoque
    end else begin
      if posX >= length(buffText) + 1 then exit;  //Ya está al final
//      if buffText[posX+1]=' ' then inc(posX);
//      if posX >= length(buffText) + 1 then exit;  //Ya está al final
      if buffText[posX]=' ' then begin
        //Está en espacio
        while (posX<length(buffText) + 1) and (buffText[posX]=' ') do begin
          inc(posX)
        end;
//        dec(posX);   //Corrige posición
      end else begin
        //Está en otro caracter
        while (posX<length(buffText) + 1) and (buffText[posX]<>' ') do begin
          inc(posX)
        end;
        //Está en espacio
        while (posX<length(buffText) + 1) and (buffText[posX]=' ') do begin
          inc(posX)
        end;
      end;
      SetCursorPos(posX, posY);
    end;
  end;
  VK_HOME: begin
    SetCursorPos(1, posY);
  end;
  VK_END: begin
    SetCursorPos(Length(buffText)+1, posY);
  end;
  VK_BACK: begin
    if Shift=[] then begin
      if PosX>length(prompt)+1 then begin
        Delete(buffText, posX-1, 1);
        SetCursorPos(posX-1, posY);
      end else begin
        Beep;
      end;
    end;
  end;
  VK_DELETE: begin
    Delete(buffText, posX, 1);
    SetCursorPos(posX, posY);  //Notar que el cursor puede cubrir temporalmente un caracter útil
  end;
  VK_RETURN: begin
    if buffText = '' then exit;   //No acepta líneas vacías

    newPromp := '';
    oldPrompt := prompt;
    oldAnswer := copy(buffText, length(prompt)+1, length(buffText));
    buffText := '';
    prompt := '';
    SetCursorPos(1, posY);
    if OnComReturn<>nil then OnComReturn(oldPrompt, oldAnswer);
  end;
  end;
  if OnComKeyDown<>nil then OnComKeyDown(Sender, Key, Shift);
end;
procedure TfraComPanel.Frame1KeyPress(Sender: TObject; var Key: char);
begin
  if (ord(Key)>=32) and (ord(Key)<127) then begin
    //Caracteres imprimibles
    if posX = Length(buffText)+1 then begin
      //Al final de la línea
      buffText+= Key;
      PosX := PosX + 1;
    end else if posX < Length(buffText)+1 then begin
      //En medio de la línea
      Insert(Key, buffText, posX);
      PosX := PosX + 1;
    end;
    SetCursorPos(posX, posY);
  end;
end;
procedure TfraComPanel.FrameResize(Sender: TObject);
begin
  SetFont;
  //Recalcula variables de aspecto
  fontHeight := Canvas.GetTextHeight('Ñ');
  fontWidth  := Canvas.GetTextWidth('X');
  lineHeight := fontHeight;  //Altura de línea
  nRows      := self.Height div lineHeight;
  rowOffset  := self.Height mod lineHeight;
  rowEnd   := lines.Count;
  rowBegin := rowEnd - nRows;  //Podría ser negativa
end;
procedure TfraComPanel.FrameClick(Sender: TObject);
begin
  self.SetFocus;
end;
procedure TfraComPanel.AddLine(txt: string);
var
  i: Integer;
begin
  lines.Add(txt);
  //Limita cantidad de líneas
  if lines.Count>1000 then begin
     for i:=1 to 100 do lines.Delete(0);
  end;
  //Calcula línea inicial y final a mostrar de lines[]
  rowEnd := lines.Count;
  rowBegin := rowEnd - nRows;  //Podría ser negativa
end;
procedure TfraComPanel.AddPrompt(txt: string);
{Escribe un "prompt" o mensaje para recibir comando.
Se escribe siempre en la última línea}
begin
  prompt := txt;  //Completa con espacios
  buffText := prompt;
  SetCursorPos(Length(buffText)+1, posY);
end;
procedure TfraComPanel.SetFont;
begin
  Canvas.Font.Name := fontName;
  Canvas.Font.Size := fontSize;
  Canvas.Font.Color:= textColor;
end;
procedure TfraComPanel.SetFont(fColor: TColor);
begin
  //Actualiza campos
  textColor := fColor;
  //Actualiza en Canvas
  Canvas.Font.Color:= textColor;
end;
constructor TfraComPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lines:= TStringList.Create;
  textColor := clLime;
  prmpColor := clWhite;
  prmpColorBk := $AA6030;
  errorColor := clRed;
  fontSize   := 11;
  fontName   := 'Consolas';
  self.OnEnter := @Frame1Enter;
  self.OnExit := @Frame1Exit;
  self.OnKeyPress := @Frame1KeyPress;
  self.OnKeyDown := @Frame1KeyDown;
//  Canvas.Font.Name := 'Courier';
  //Canvas.Font.Bold := true;
  posX := 1;
  posY := 1;
  timerCursor := 5;
end;
destructor TfraComPanel.Destroy;
begin
  lines.Destroy;
  inherited Destroy;
end;

end.

