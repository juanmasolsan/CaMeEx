(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 12:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-20 16:38:06
 *)
{

MIT License

Copyright (c) 2023 Juan Manuel Soltero Sánchez

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

}
unit UnidadAddCatalogo;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , StdCtrls
  , ComCtrls
  , ExtCtrls
  , Control_Formulario_Avanzado
  , InterfaceConectorDatos;

const
  // Pasos del asistente
  PASO_SELECCION_INICIO = 0;

  // Pasos que tiene el asistente
  PASO_SELECCION_MEDIO  = PASO_SELECCION_INICIO;
  PASO_ESCANEAR         = PASO_SELECCION_MEDIO;   //TODO: ponerle el +1
  PASO_GUARDAR          = PASO_ESCANEAR + 1;

  // Paso Final del asistente
  PASO_SELECCION_FINAL  = PASO_GUARDAR;

  // Paso del asistente que es el de cancelación
  PASO_CANCELAR         = -1;



type
  { TForm_AddCatalogo }
  TForm_AddCatalogo = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button_Cancelar: TButton;
    Button_Atras: TButton;
    Button_Siguiente: TButton;
    Image1: TImage;
    Label_Titulo_Asistente_Add: TLabel;
    Shape1: TShape;
    procedure Button_SiguienteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button_AtrasClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FGestorDatos : IConectorDatos;
    FPasoActual  : longint;
    FFrames      : array of TFrame;
  protected
    // Dependiendo del paso actual, se ejecuta una acción u otra
    procedure DoAccionesAtrasSigioente(direccion : longint);

    // Para escribir el título del asistente
    procedure DoTitulo(titulo : String);

    // Para mostrar el frame correspondiente al paso actual
    procedure DoPasoVisible(paso : longint);

  public
    { public declarations }
    // Configura el asistente para agregar un nuevo catalogo
    procedure AgregarNuevoCatalogo(GestorDatos : IConectorDatos);

    // Paso - Escanear medio
    procedure DoPasoEscanear();

  end;

var
  Form_AddCatalogo: TForm_AddCatalogo;



implementation

uses
  AppString, UnidadScan;


{$R *.lfm}

{ TForm_AddCatalogo }

procedure TForm_AddCatalogo.FormCreate(Sender: TObject);
begin
  ActivarGuardadoPosicion(true);

  // Crear el array de Frames
  SetLength(FFrames, PASO_SELECCION_FINAL + 1);
  //FAtributos               := TFrame_Atributos.Create(TComponent(Pointer(@Pagina_Atributos)^));
  //FAtributos.Parent        := Pagina_Atributos;

  // Inicializa el asistente
  DoAccionesAtrasSigioente(0);
end;

procedure TForm_AddCatalogo.FormDestroy(Sender: TObject);
var
  t : longint;
begin


  // Liberar los frames
  for t := 0 to High(FFrames) do
    FreeAndNil(FFrames[t]);
end;

// Configura el asistente para agregar un nuevo catalogo
procedure TForm_AddCatalogo.AgregarNuevoCatalogo(GestorDatos : IConectorDatos);
begin
  FGestorDatos := GestorDatos;

  FPasoActual   := PASO_SELECCION_MEDIO;
end;


// Dependiendo del paso actual, se ejecuta una acción u otra
procedure TForm_AddCatalogo.DoAccionesAtrasSigioente(direccion : longint);
begin

  FPasoActual += direccion;

  if FPasoActual < PASO_SELECCION_INICIO then
    FPasoActual := PASO_SELECCION_INICIO
  else
    if FPasoActual > PASO_SELECCION_FINAL then
      FPasoActual := PASO_SELECCION_FINAL;

  Button_Atras.Enabled := FPasoActual > PASO_SELECCION_INICIO;

  Button_Atras.visible    := (FPasoActual >= PASO_SELECCION_INICIO) AND (FPasoActual < PASO_SELECCION_FINAL);
  Button_Cancelar.visible := Button_Atras.visible;

  if FPasoActual = PASO_SELECCION_FINAL then
    Button_Siguiente.Caption := Message_Asistente_Nuevo_Catalogo_Cerrar
  else
    Button_Siguiente.Caption := Message_Asistente_Nuevo_Catalogo_Siguiente;

  // Dependiendo muestra un frame u otro
  DoPasoVisible(FPasoActual);

  // Dependiendo del paso actual, se ejecuta una acción u otra
  case FPasoActual of
    //PASO_SELECCION_INICIO : DoTitulo(Message_Asistente_Nuevo_Catalogo_Titulo);
    PASO_ESCANEAR         : DoPasoEscanear();
    PASO_GUARDAR          : DoTitulo(Message_Asistente_Nuevo_Catalogo_Cerrar);
    //PASO_SELECCION_FINAL  : DoTitulo(Message_Asistente_Nuevo_Catalogo_Guardar);
  end;

end;




procedure TForm_AddCatalogo.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TForm_AddCatalogo.Button_SiguienteClick(Sender: TObject);
begin
  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSigioente(1);
end;

procedure TForm_AddCatalogo.Button_AtrasClick(Sender: TObject);
begin
  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSigioente(-1);
end;

// Para escribir el título del asistente
procedure TForm_AddCatalogo.DoTitulo(titulo : String);
begin
  Label_Titulo_Asistente_Add.Caption := titulo;
  caption                            := Message_Asistente_Nuevo_Catalogo_titulo + ' ['+ inttostr(FPasoActual + 1) + '/' + inttostr(High(FFrames) + 1)+'] - '+ titulo;
end;


// Para mostrar el frame correspondiente al paso actual
procedure TForm_AddCatalogo.DoPasoVisible(paso : longint);
var
  t : longint;
begin
  for t := 0 to High(FFrames) do
    if FFrames[t] <> nil then
      FFrames[t].Visible := t = paso;
end;


// Paso - Escanear medio
procedure TForm_AddCatalogo.DoPasoEscanear();
begin
  // Crear el frame si no existe
  if FFrames[PASO_ESCANEAR] = nil then
  begin
    FFrames[PASO_ESCANEAR]        := TFrame_Scan.Create(Self);
    FFrames[PASO_ESCANEAR].Parent := Self;
  end;

  DoTitulo(Message_Asistente_Nuevo_Catalogo_Escanear_Medio);

end;


end.
