{
Unidad que define al objeto TEvalExpres, que permite evaluar el resultado de una
expresión aritmética en un cadena de una línea.
Reoonoce las variables. Estas deben crearse con TEvalExpres.AisgVariable().

                                                         Por Tito Hinostroza 17/12/2016
}
unit EvalExpres;
{$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, math, Forms, LCLType;
Const
  FIN_CON = #0;    //Fin de contexto

Type
  //Tipo de operando
  TPTipOper = (TIP_DES,    //tipo desconocido
               TIP_NUM,    //tipo número
               TIP_CAD);    //tipo cadena

  //Categoría para clasificar a los operandos
  TPCatOper = (
    COP_NULL, //Operando nulo
    COP_CONS,   //Es una constante
    COP_VAR,    //Es una variable
    COP_FUNC,   //Es una función
    COP_EXPR);  //Es resultado de una expresión

  { Texpre }
  //Tipo expresión. Se usa para manejo de evaluación aritmética.
  Texpre = object      //Tipo expresión
    txt: String;      //Texto de la expresión
    tip: TPTipOper;    //Tipo de dato que devuelve la expresión
    cat: TPCatOper;   //Categoría de expresión
  private
    fTxt: String;    //Valor numérico de la expresión
    fNum: Single;    //Valor numérico de la expresión
    procedure FijTxt(txt0: string);
    function LeeTxt: string;
    procedure FijNum(n0: single);
    function LeeNum: single;
  public
    CadError: string;
    property valTxt: string read LeeTxt write FijTxt;
    property valNum: single read LeeNum write FijNum;
  End;

  { TContexto }
  {Estructura que define a un objeto contexto. Se usa tanto para leer la entrada como para
   escribir en la salida.}
  TContexto = class
    col      : Integer;  //columna actual
    lin      : string;   {Líneas de texto}
    constructor Create;
    destructor Destroy; override;
    //Métodos de lectura
    Function IniCont:Boolean;
    Function FinCont:Boolean;
    Function VerCar:Char;
    Function CogCar:Char;
    Function VerCarAnt: Char;
    Function VerCarSig: Char;
    Function CapBlancos:Boolean;

    //Métodos de escritura
    procedure CurPosIni;
    procedure CurPosFin;
  End;

  //Define a una variable.
  //Se define como registro clásico, para optimizar la velocidad.
  TEVar= record
    nomb: string[12];   //Nombre de la variable
    valor: Double;      //Valor de la variable
  end;

  { TEvalExpres }
  {Objeto evaluador de expresiones.}
  TEvalExpres = class
  public
    cEnt : TContexto;   //referencia al contexto de entrada actual
    vars : array of TEVar;  //Se puede hacer estático, si se quiere ganar velocidad
    nVars: integer;     //Número de variables
    //rutinas basicas de lectura
    Function VerCarN(numcar:Integer): String;
    function Capturar(cap: String): Boolean;
    function CogCarERR(car: char): Boolean;
    //Rutinas avanzadas de lectura
    function CogNumero(var n:Single):boolean;
    function CogCadena(var s: string):boolean;
    function CogIdentif(var s: string):boolean;
    function cogOperador: String;           //coge operador
    function jerOp(oper: String): Integer;  //jerarquía de operador
    function Evaluar(Op1: Texpre; opr: String; Op2: Texpre): Texpre;
    function CogOperando: Texpre;
    function CogExpresion(jerar: Integer): Texpre;
    function CogExpresionPar: Texpre;
    function AsigVariable(const VarName: string; value: Double): integer;
    function EvaluarLinea(lin: string): Texpre;
  public  //Campos para manejo de error
    ErrorCol : integer;   //número de columna del error
    ErrorStr : string;    //cadena de error
    procedure GenError(msje: String; col: integer=-1);
  public  //Inicialización
    procedure Iniciar(txt: string);   //Prepara la secuencia de preprocesamiento
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ Texpre }
procedure Texpre.FijTxt(txt0: string);
//Fija valor de texto de un operando
begin
  tip := TIP_CAD;  //se fija como cadeana, de otra forma no podría recibir este valor
  fTxt := txt0;
end;
function Texpre.LeeTxt: string;
//Lee la variable como texto
begin
  if tip = TIP_CAD then  //si ya es texto, la lectura es directa
    Result := ftxt
  else if tip = TIP_NUM then //Si es numérico, se hace una transformación
    //siempre se podrá transformar
    Result := FloatToStr(fNum)   //pero no deja de ser numérico
  else
    Result := '';
end;
procedure Texpre.FijNum(n0: single);
begin
  tip := TIP_NUM;  //se fija como número, de otra forma no podría recibir este valor
  fNum := n0;
end;
function Texpre.LeeNum: single;
begin
  if tip = TIP_CAD then begin //si es texto, se hace una transformación
    //puede que no se pueda transformar
    if not TryStrToFloat(ftxt, Result) then  //pero no deja de ser texto
      CadError := 'Número inválido.';
  end else if tip = TIP_NUM then //Si ya es numérico, la lectura es directa
    Result := fNum
  else
    Result := 0;
end;
{ TContexto }
//********************************************************************************
//Funciones Básicas para administración de los Contextos
//********************************************************************************
constructor TContexto.Create;
begin
  CurPosFin;   //inicia fil y col
end;
destructor TContexto.Destroy;
begin
  inherited Destroy;
end;
function TContexto.IniCont: Boolean;
//Devuelve verdadero si se está al inicio del Contexto (columna 1)
begin
  Result := (col = 1);
end;
function TContexto.FinCont: Boolean;
//Devuelve verdadero si se ha pasado del final del Contexto actual
begin
  Result := (col >= Length(lin) + 1);
end;
function TContexto.VerCar: Char;
//Devuelve el caracter actual
//Si no hay texto en el Contexto actual o si se ha llegado al final del
//texto, devuelve FIN_CON.
begin
  if FinCont Then exit(FIN_CON);
  Result := lin[col];
end;
function TContexto.CogCar: Char;
//Lee un caracter del contexto y avanza el cursor una posición.
//Si no hay texto en el Contexto actual o si se ha llegado al final del
//texto, devuelve FIN_CON.
begin
  if FinCont Then exit(FIN_CON);
  Result := lin[col];
  inc(col);
end;
function TContexto.VerCarAnt: Char;
//Echa un vistazo al caracter anterior del Contexto
//Si no hay caracter anterior, devuelve caracter nulo.
begin
  if IniCont Then exit(#0);        //No hay caracter anterior
  Result := lin[col-1];
end;
function TContexto.VerCarSig: Char;
//Devuelve el catacter siguiente al actual.
//Si no hay caracter siguiente, devuelve caracter nulo.
begin
  if FinCont Then exit(#0);
  Result := lin[col+1];
end;
function TContexto.CapBlancos: Boolean;
//Coge los blancos iniciales del contexto de entrada.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
  Result := False;
  while not FinCont and (VerCar in [' ', #9]) do
    CogCar;
end;
procedure TContexto.CurPosIni;
//Mueve la posición al inicio del contenido.
begin
  col := 1;   //posiciona al inicio
end;
procedure TContexto.CurPosFin;
//Mueve la posición al final del contenido.
begin
  col := length(lin)+1;   //posiciona al final
end;

{ TEvalExpres }
function TEvalExpres.VerCarN(numcar: Integer): String;
//Devuelve los N caracteres a partir de la posición actual, del Contexto actual.
//Si no hay texto en el Contexto actual o si se ha llegado al final del
//texto, devuelve FIN_CON.
begin
  if cEnt.FinCont Then Exit(FIN_CON);
  Result := copy(cEnt.lin, cEnt.col, numcar);
End;
function TEvalExpres.Capturar(cap: String): Boolean;
{Coge la cadena dada ignorando los blancos iniciales.}
Var i:Integer;
begin
  Result := False;
  cEnt.CapBlancos;     //quita blancos iniciales
  i := 1;
  while Not cEnt.FinCont And (i <= Length(cap)) do begin
    if cEnt.VerCar = cap[i] then begin
      cEnt.CogCar;
      i := i + 1;
    end else begin
      exit;     //fallo en algun caracter
    end;
  end;
  if i > Length(cap) then begin   //encontró toda la cadena
    Result := true;
  end;
End;
function TEvalExpres.CogCarERR(car: char): Boolean;
{Coge el caracter indicado. Si no lo encuentra genera error y devuelve FALSE.}
begin
  if cEnt.VerCar=car then begin
    //Es el caracter buscado
    cEnt.CogCar;
    exit(true);
  end else begin
    GenError('Error en expresión. Se esperaba "'+ car +'"');
    exit(false);
  end;
end;
function TEvalExpres.CogNumero(var n: Single): boolean;
{Veririfca si lo que sigues es un número y de ser así, intenta tomarlo.
Puede geenrar error al convertir el número}
Var car:char;
    temp:String;
begin
    car := cEnt.VerCar;
    If Not (car in ['0'..'9','.','-']) Then      //primer caracter no valido
       exit(false);        //no es numero
    if (car in ['.','-']) and not (cEnt.VerCarSig in ['0'..'9']) then
       exit(false);    //no es válido
    temp := cEnt.CogCar;   //acumula primer dígito
    //busca hasta encontar fin de identificador
    While cEnt.VerCar in ['0'..'9','.'] do begin
      car := cEnt.CogCar;     //toma el caracter
      temp += car;     //acumula
    end;
    //se llego al final del número
    if not TryStrToFloat(temp, n) then begin
      GenError('Error en número: ' + temp, cEnt.col);
    end;
    Result := true;  //indica que hubo número
end;
function TEvalExpres.CogCadena(var s: string): boolean;
{Coge una constante de tipo cadena (entre apóstrofos) desde la posicion
 donde se encuentra el archivo, hasta el delimitador o fin de línea.
 Si no encuentra una cadena, devuelve FALSE}
var
  car : char;
begin
  if cEnt.VerCar <> '''' Then   //primer caracter no valido
    exit(false);        //no es constante cadena
  cEnt.CogCar;     //toma el caracter
  s := '';         //inicia para acumular
  //busca hasta encontar fin de cadena
  while not cEnt.FinCont do begin
      car := cEnt.CogCar;
      If car = '''' Then begin  //caracter de fin
        exit(true);  //sale
      end Else begin
        s += car;     //acumula
      End;
  end;
  //se llego al final del archivo
  GenError('No se encontro fin de cadena');
  Result := true;    //indica que se encontró cadena
end;
function TEvalExpres.CogIdentif(var s: string): boolean;
{Coge un identificador, que debe corresponder a una variable.}
begin
  if not (cEnt.VerCar in ['a'..'z','A'..'Z']) then   //primer caracter no valido
    exit(false);        //no es constante cadena
  s := '';         //inicia para acumular
  //busca hasta encontar fin de identificador
  while not cEnt.FinCont and (cEnt.VerCar in ['a'..'z','A'..'Z']) do begin
    s += cEnt.CogCar;
  end;
  Result := true;    //indica que se encontró identificador
end;
function TEvalExpres.cogOperador: String;
{Coge un operador en la posición del contexto actual. Si no encuentra
 devuelve cadena vacía y no coge caracteres, salvo espacios iniciales.}
begin
  cogOperador := '';
  cEnt.CapBlancos;     //quita blancos iniciales
  Case cEnt.VerCar of //completa con operador de más caracteres
  '+': begin
         Result := cEnt.CogCar;
//           If VerCar = '+' Then begin CogCar; Result := '++' end;
//           If VerCar = '=' Then begin CogCar; Result := '+=' end;
        end;
  '-': begin
         Result := cEnt.CogCar;
//           If VerCar() = '-' Then begin CogCar; Result := '--' end;
//           If VerCar() = '=' Then begin CogCar; Result := '-=' end;
      end;
  '*': begin
        Result := cEnt.CogCar;
//          If VerCar() = '=' Then begin CogCar; Result := '*=' end;
      end;
  '/': begin
        Result := cEnt.CogCar;
//          If VerCar() = '=' Then begin CogCar; Result := '/=' end;
      end;
  '\': begin
        Result := cEnt.CogCar;
      end;
  '%': begin
        Result := cEnt.CogCar;
      end;
  '^': begin
        Result := cEnt.CogCar;
      end;
//    '=': begin
//          Result := CogCar;
//          If VerCar() = '=' Then begin CogCar; Result := '==' end;
//          If VerCar() = '<' Then begin CogCar; Result := '=<' end;     //operador 'menor'
//          If VerCar() = '>' Then begin CogCar; Result := '=>' end;     //operador 'mayor'
//        end;
//    '>': begin
//          Result := CogCar;
//          If VerCar() = '=' Then begin CogCar; Result := '>=' end;
//          If VerCar() = '>' Then begin CogCar; Result := '>>' end;
//          If VerCar() = '+' Then begin CogCar; Result := '>+' end;
//          If VerCar() = '-' Then begin CogCar; Result := '>-' end;
//        end;
//    '|': begin
//          Result := CogCar;
//          If VerCar() = '|' Then begin CogCar; Result := '||' end;    //OR
//          If VerCar() = '!' Then begin CogCar; Result := '|!' end;    //XOR
//        end;
//    '~': begin                            //operador LIKE
//          Result := CogCar;
//        end;
//    '&': begin
//          Result := CogCar;
//          If VerCar = '&' Then begin CogCar; Result := '&&' end;     //AND
//        end;
  End;
End;
function TEvalExpres.jerOp(oper: String): Integer;
//Devuelve la jerarquía de un operador ver documentación técnica.
begin
    Case oper of
//    '>>', '<<', '>+', '>-': jerOp = 1: Exit Function
//    '=': jerOp := 2;
//    '&&', '||', '!', '|!': jerOp := 3;
//    '==', '<>', '>', '>=', '<', '<=', '~': jerOp := 4;
    '+', '-'{, '|', '&'}: jerOp := 5;
    '*', '/', '\', '%': jerOp := 6;
//    '=<', '=>': jerOp := 7;
    '^'{, '++', '--', '+=', '-=', '*=', '/='}: jerOp := 8;
    Else jerOp := 0;
    End;
End;
function TEvalExpres.Evaluar(Op1: Texpre; opr: String; Op2: Texpre): Texpre;
//Devuelve el resultado y tipo de una operación
begin
    ErrorStr:='';
    Evaluar.cat := COP_EXPR;    //ahora es expresión por defecto
    Case opr of
    '': begin     //Sin operador. Y se supone sin Op2
          //no hay nada que hacer, ya está en la pila
          Evaluar := Op1;
        end;
{    '=': begin    //Asignación
          If Op1.cat = CAT_OP_VARIABLE Then begin  //Asignación a una variable
              Evaluar.val := Op2.val;
              Evaluar.tip := Op2.tip;
              //OJO!!!!!!!!!! aún no hace la asignación
          end Else
              Perr.GenError('Sólo se puede asignar valor a una variable', PosAct);
         end;}
    '+': begin
          Evaluar.valNum := Op1.valNum + Op2.valNum;  //Fuerza a Evaluar.tip := TIP_NUM
         end;
    '-': begin
          Evaluar.valNum := Op1.valNum - Op2.valNum;
         end;
    '*': begin
          Evaluar.valNum := Op1.valNum * Op2.valNum;
         end;
    '/': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluar.valNum := Op1.valNum / Op2.valNum;
          End;
         end;
    '\': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluar.valNum := round(Op1.valNum) div round(Op2.valNum);
          end;
         end;
    '%': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluar.valNum := round(Op1.valNum) mod round(Op2.valNum);
          end;
         end;
    '^': begin
          if (Op2.valNum = 0) And (Op2.valNum = 0) Then
              GenError('No se puede Evaluar 0^0')
          else begin   //error
              Evaluar.valNum := power(Op1.valNum, Op2.valNum);
          end;
         end;
{    '++': begin       //mono-operando, sólo Op1
          Op1.val := val(Op1.val) + 1  //incrementa
          Evaluar.val := Op1.val;
          Evaluar.tip := TIP_NUM;
         end;
    '--': begin       //mono-operando
          Op1.val := val(Op1.val) - 1  //decrementa
          Evaluar.val := Op1.val;
          Evaluar.tip := TIP_NUM;
    //operadores de comparación
         end;
    '==': begin
          If Op1.val := Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '<>': begin
          If Op1.val <> Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '>': begin
          If Op1.val > Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '<': begin
          If Op1.val < Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '>=': begin
          If Op1.val >= Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '<=': begin
          If Op1.val <= Op2.val Then
              Evaluar.val := 1
          Else    //error
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '|': begin    //concatenación de cadenas
          Evaluar.val := Op1.val & Op2.val
          Evaluar.tip := TIP_CAD
         end;
    '~': begin    //comparación de cadenas
          If (Op1.val Like Op2.val) Then
              Evaluar.val := 1
          Else    //no cuadra
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '&&': begin   //And lógico
          If (val(Op1.val) = 1 And val(Op2.val) = 1) Then
              Evaluar.val := 1
          Else    //no cuadra
              Evaluar.val := 0
          Evaluar.tip := TIP_NUM
         end;
    '||': begin
          If (val(Op1.val) = 0 And val(Op2.val) = 0) Then
              Evaluar.val := 0
          Else    //no cuadra
              Evaluar.val := 1
          Evaluar.tip := TIP_NUM
         end;
    '!': begin
          If val(Op1.val) = 1 Then
              Evaluar.val := 0
          Else    //no cuadra
              Evaluar.val := 1
          Evaluar.tip := TIP_NUM
         end;}
    else begin
        GenError('No se reconoce operador: ' + opr, cEnt.col);
        Exit;
         End;
    end;
    //Completa campos de evaluar
    Evaluar.txt := Op1.txt + opr + Op2.txt;   //texto de la expresión
//    Evaluar.uop := opr;   //última operación ejecutada
End;
function TEvalExpres.CogOperando: Texpre;
{Coge un operando en la posición actual del contenido. Si no enceuntra
el operando o es erróneo, genera Error.}
var
  cad : String;
  num : single;
  exp : Texpre;
  i: Integer;
begin
  cEnt.CapBlancos;   //quita blancos iniciales
  if cEnt.FinCont then begin
    //No hay
    Result.txt := '';   //indica que no hay operando
    exit;
  end;
  if CogNumero(num) then begin
    if ErrorStr<>'' then exit;  //pudo haber error en número
    Result.txt := '#';   //indica número
    Result.valNum := num;   //fija tipo a número
    Result.cat := COP_CONS;
  end else if CogIdentif(cad) then begin
    //Es un identificador.
    //Busca si es una variable
    for i:=0 to nVars-1 do begin
      if vars[i].nomb = cad then begin
        Result.txt := '$';   //indica número
        Result.valNum := vars[i].valor;
        Result.cat := COP_VAR;
        exit;
      end;
    end;
    //No es variable, busca si es función
    case cad of
    'abs': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := abs(exp.valNum);
      exit;  //sale sin error
    end;
    'sgn': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := Sign(exp.valNum);
      exit;  //sale sin error
    end;
    'sgn2': begin  //variación de la función Sgn()
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      if exp.valNum<0 then Result.valNum := 0
      else Result.valNum := exp.valNum;
      exit;  //sale sin error
    end;
    'int': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := Int(exp.valNum);
      exit;  //sale sin error
    end;
    'sen': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := sin(exp.valNum);
      exit;  //sale sin error
    end;
    'cos': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := cos(exp.valNum);
      exit;  //sale sin error
    end;
    'tan': begin
      exp := CogExpresionPar;
      if ErrorStr<>'' then exit;
      Result.valNum := tan(exp.valNum);
      exit;  //sale sin error
    end;
    end;
    //No es variable ni función.
    GenError('Función o variable desconocida: '+cad, cEnt.col);
    Result.txt := '';   //indica que no hay operando
  end else if CogCadena(cad) Then begin   //Constante cadena
    Result.txt := 'A';   //indica cadena
    Result.valTxt := cad;    //fija tipo a número
    Result.cat := COP_CONS;
  end else If cEnt.VerCar = '(' Then begin
    Result := CogExpresionPar;
    exit;  //Puede salir con error
  end else begin
    //Debe ser otra cosa
    Result.txt := '';    //indica que no hay operando
    exit;  //no devuelve nada
  end;
end;
function TEvalExpres.CogExpresion(jerar: Integer): Texpre;
//Toma una expresión completa, en la posición actual del contenido
//Si no encuentra una expresión, genera error
var Op1, Op2 : Texpre;
    opr, opr2 : String;
    jerOpr, jerOpr2: Integer;
    pos1, pos2 : integer;
begin
    cEnt.CapBlancos;  //quita blancos iniciales
    Op1 := CogOperando;  //error
    if ErrorStr<>'' then exit;
//    if Op1.txt = '' then begin
//      Result.cat := COP_NULL;  //No se reconoce expresión
//      GenError('Se esperaba expresión.');
//      exit;
//    end;
    opr := cogOperador;
    if opr = '' Then begin
      Result := Op1;
      Exit
    End;
    jerOpr := jerOp(opr);     //Hay operador, tomar su jerarquía
    //-------------------------- ¿Delimitada por jerarquía? ---------------------
    if jerOpr <= jerar then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      Exit;
    End;
    while opr <> '' do begin
        pos1 := cEnt.col;    //Guarda por si lo necesita
        Op2 := CogOperando;
        if ErrorStr<>'' then exit;
//        If Op2.txt = '' Then   begin //error
//           GenError('Error en expresión. Se esperaba operando.');
//           exit;
//        end;
        pos2 := cEnt.col;    //Guarda por si lo necesita
        opr2 := cogOperador;
        If opr2 <> '' Then begin  //Hay otro operador
            jerOpr2 := jerOp(opr2);
            //¿Delimitado por jerarquía de operador?
            If jerOpr2 <= jerar Then begin  //sigue uno de menor jerarquía, hay que salir
                cEnt.col := pos2;   //antes de coger el operador
                Result := Evaluar(Op1, opr, Op2);
                Exit;
            End;
            If jerOpr2 > jerOpr Then begin    //y es de mayor jerarquía, retrocede
                cEnt.col:= pos1;        //retrocede
                Op2 := CogExpresion(jerOpr);        //evalua primero
                opr2 := cogOperador;    //actualiza el siguiente operador
            End;
        End;

        Op1 := Evaluar(Op1, opr, Op2);    //evalua resultado
        if ErrorStr<>'' then exit;
        opr := opr2;
        jerOpr := jerOp(opr);    //actualiza operador anterior
    end;
    Result := Op1;
    Result.cat := COP_EXPR;
end;
function TEvalExpres.CogExpresionPar: Texpre;
{Coge una expresión que debe estar encerrada entre paréntesis. Puede genera error}
begin
  if not CogCarERR('(') then exit;  //sale con error
  Result := CogExpresion(0);
  if ErrorStr<>'' then exit;  //sale con error
  cEnt.CapBlancos;
  if not CogCarERR(')') then exit;  //sale con error
end;
function TEvalExpres.AsigVariable(const VarName: string; value: Double): integer;
{Asigna un valor numérico a una variable. Si no existe la crea.
Devuelve el índice de la variable en el arreglo vasr[].}
var
  i: Integer;
begin
  //Busca variable
  for i:=0 to nVars-1 do begin
    if vars[i].nomb = VarName then begin
      vars[i].valor := value;
      exit(i);
    end;
  end;
  //No se encontró, se debe crear
  inc(nVars);
  setlength(vars, nVars);
  Result := nVars-1;
  vars[Result].nomb := VarName;
  vars[Result].valor := value;
end;
function TEvalExpres.EvaluarLinea(lin: string): Texpre;
{Evalúa la expresión que está contenida en "lin"}
begin
  ErrorStr:='';          //Inicia bandera de error
  Iniciar(lin);   //Inicia cadena
  Result := CogExpresion(0);  //coge expresión
  if ErrorStr<>'' then exit;  //puede generar error
  //Verifica si terminó de procesar toda la línea
  if not cEnt.FinCont then
    GenError('Error de sintaxis.');
end;
procedure TEvalExpres.GenError(msje: String; col: integer = -1);
begin
  ErrorStr := msje;
  if col = -1 then ErrorCol := cEnt.col  //por dfeecto
  else ErrorCol := col;
end;
constructor TEvalExpres.Create;
begin
  cEnt := TContexto.Create;   //Crea un contexto
  nVars := 0;
  setlength(vars, nVars);
end;
destructor TEvalExpres.Destroy;
begin
  cEnt.Destroy;
  inherited;
end;
procedure TEvalExpres.Iniciar(txt: string);
//Inicia la maquinaria de manejo de Contextos
begin
  cEnt.lin := txt;
  cEnt.CurPosIni;       //posiciona al inicio
end;

end.

