(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 12:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-20 17:43:04
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

{$i ../../DirectivasCompilacion.inc}

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
  , InterfaceConectorDatos, ItemDato, ItemExtension, ItemRutaCompleta, MotorScan;

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
    FScan        : TMotorScan;
  protected
    // Dependiendo del paso actual, se ejecuta una acción u otra
    procedure DoAccionesAtrasSiguiente(direccion : longint);

    // Para escribir el título del asistente
    procedure DoTitulo(titulo : String);

    // Para mostrar el frame correspondiente al paso actual
    procedure DoPasoVisible(paso : longint);

    // Guarda lo escaneado en el gestor de datos
    procedure DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);

    // Cuando termina el escaneo correctamente
    procedure DoOnTerminarScanAsync();
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

  // Inicializar el Motor de Escaneo
  FScan := TMotorScan.Create;

  // Inicializa el asistente
  DoAccionesAtrasSiguiente(0);
end;

procedure TForm_AddCatalogo.FormDestroy(Sender: TObject);
var
  t : longint;
begin


  // Liberar los frames
  for t := 0 to High(FFrames) do
    FreeAndNil(FFrames[t]);

  if assigned(FScan) then
  begin
    FScan.free;
  end;
end;

// Configura el asistente para agregar un nuevo catalogo
procedure TForm_AddCatalogo.AgregarNuevoCatalogo(GestorDatos : IConectorDatos);
begin
  FGestorDatos := GestorDatos;

  FPasoActual   := PASO_SELECCION_MEDIO;
end;


// Dependiendo del paso actual, se ejecuta una acción u otra
procedure TForm_AddCatalogo.DoAccionesAtrasSiguiente(direccion : longint);
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
  DoAccionesAtrasSiguiente(1);
end;

procedure TForm_AddCatalogo.Button_AtrasClick(Sender: TObject);
begin
  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSiguiente(-1);
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
    FFrames[PASO_ESCANEAR]        := TFrame_Scan.CreateEx(Self, FScan);
    FFrames[PASO_ESCANEAR].Parent := Self;
  end;

  DoTitulo(Message_Asistente_Nuevo_Catalogo_Escanear_Medio);

  // Iniciar el escaneo
  TFrame_Scan(FFrames[PASO_ESCANEAR]).Iniciar();

  {$IFNDEF ESCANEAR_DIRECTORIO_GRANDE}
    {$IFNDEF ESCANEAR_DIRECTORIO_VSCODE_EXTENSIONS}
      FScan.ScanDirAsync(Curdir, @DoOnTerminarScanAsync, '.git;img\iconos');
    {$ELSE}
      FScan.ScanDirAsync('C:\DAM_02\comun\programas\vscode\data\extensions\', @DoOnTerminarScanAsync, '.git;img\iconos');
      //FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync, '.git;img\iconos');
    {$ENDIF ESCANEAR_DIRECTORIO_VSCODE_EXTENSIONS}

  {$ELSE}
    FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync, '.git;img\iconos');
  {$ENDIF}
end;

// Cuando termina el escaneo correctamente
procedure TForm_AddCatalogo.DoOnTerminarScanAsync();
begin
  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSiguiente(1);
end;

// Guarda lo escaneado en el gestor de datos
procedure TForm_AddCatalogo.DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);

  procedure ProcesarHijo(Item : TItemDato);
  var
    t, total : integer;
    Actual : TItemDato;
  begin
    if item <> nil then
    begin
      total := item.HijosCount()-1;
      for t := 0 to total do
      begin
        Actual := item.GetHijo(t);

        // Guarda los datos del archivo o directorio
        SistemaGuardado.AddDato(Actual);

        // Guarda los datos de todo los hijos
        ProcesarHijo(Actual);
      end;
    end;
  end;

var
  t, total : integer;
  Item     : TItemDato;
begin
  if assigned(Scan) then
  begin
    // Actualiza los datos del catalogo
    Scan.Root.TotalArchivos    := Scan.TotalArchivos;
    Scan.Root.TotalDirectorios := Scan.TotalDirectorios;
    Scan.Root.Size             := Scan.TotalSize;

    // Guarda los datos del catalogo
    SistemaGuardado.AddCatalogo(Scan.Root);

    // Guarda una copia del catalogo en la tabla de datos
    Scan.Root.IdPadre        := Scan.Root.Id;
    Scan.Root.IdCatalogo     := Scan.Root.Id;
    Scan.Root.IdRutaCompleta := 0;
    Scan.Root.IdExtension    := 0;
    SistemaGuardado.AddDato(Scan.Root);


    // Guarda los iconos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtensionIcono(TItemExtension(Scan.ListaExtensiones.Items[t]));
    end;

    // Guarda los datos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtension(TItemExtension(Scan.ListaExtensiones.Items[t]));
    end;

    // Guarda los datos de las rutas completas
    total := Scan.ListaRutaCompleta.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddRutaCompleta(TItemRutaCompleta(Scan.ListaRutaCompleta.Items[t]));
    end;


    // Guarda los datos de los archivos y directorios
    total := Scan.Root.HijosCount()-1;
    for t := 0 to total do
    begin
      Item := Scan.Root.GetHijo(t);
      if item <> nil then
      begin
        // Guarda los datos del archivo o directorio
        SistemaGuardado.AddDato(Item);

        // Guarda los datos de todo los hijos
        ProcesarHijo(Item);
      end;
    end;
  end;
end;

end.