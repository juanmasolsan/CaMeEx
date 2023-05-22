(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 12:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-23 00:50:00
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
  , InterfaceConectorDatos, ItemDato, ItemExtension, ItemRutaCompleta, MotorScan
  , UnidadScan
  , FrameCancelado
  , FrameGuardar
  , FrameSelecionarMedio
  , Types;

const
  // Pasos del asistente
  // Paso del asistente que es el de cancelación
  PASO_CANCELAR         = 0;

  // Pasos que tiene el asistente
  PASO_SELECCION_MEDIO  = PASO_CANCELAR + 1;
  PASO_ESCANEAR         = PASO_SELECCION_MEDIO + 1;
  PASO_GUARDAR          = PASO_ESCANEAR + 1;

  // Paso Final del asistente
  PASO_SELECCION_FINAL  = PASO_GUARDAR;



type
  { TForm_AddCatalogo }
  TForm_AddCatalogo = class(TForm)
    Bevel2: TBevel;
    Button_Cancelar: TButton;
    Button_Atras: TButton;
    Button_Siguiente: TButton;
    Frame_Cancelado1: TFrame_Cancelado;
    Frame_Guardar1: TFrame_Guardar;
    Frame_Scan1: TFrame_Scan;
    Frame_SelecionarMedio1: TFrame_SelecionarMedio;
    Image1: TImage;
    Label_Titulo_Asistente_Add: TLabel;
    PanelesAsistente: TPageControl;
    Shape1: TShape;
    TabSheet_SelecionarMedio: TTabSheet;
    TabSheet_Guardar: TTabSheet;
    TabSheet_Cancelado: TTabSheet;
    TabSheet_Scan: TTabSheet;
    procedure Button_CancelarClick(Sender: TObject);
    procedure Button_SiguienteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Button_AtrasClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FGestorDatos   : IConectorDatos;
    FPasoActual    : longint;
    FScan          : TMotorScan;
    FScanCancelado : boolean;
    FScaneando     : boolean;
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

    // Metodo que recoge la acción de cancelar el escaneo
    procedure DoCancelarScan();

  public
    { public declarations }

    // Sobreescribe el showmodal
    function ShowModal : Integer; override;

    // Configura el asistente para agregar un nuevo catalogo
    procedure AgregarNuevoCatalogo(GestorDatos : IConectorDatos);

    // Paso - Escanear medio
    procedure DoPasoEscanear();

    // Paso - Escanear cancelar
    procedure DoPasoCancelar();

    // Paso - Escanear guardar
    procedure DoPasoGuardar();

    // Paso - Selecionar Medio
    procedure DoPasoSeleccionMedio();
  end;

var
  Form_AddCatalogo: TForm_AddCatalogo;



implementation

uses
  AppString
  , UnidadLoading;


{$R *.lfm}

{ TForm_AddCatalogo }

procedure TForm_AddCatalogo.FormCreate(Sender: TObject);
begin
  ActivarGuardadoPosicion(true);

  // Inicializa el asistente
  DoAccionesAtrasSiguiente(PASO_SELECCION_MEDIO);
end;

procedure TForm_AddCatalogo.FormDestroy(Sender: TObject);
var
  t : longint;
begin
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

  if FPasoActual < PASO_CANCELAR then
    FPasoActual := PASO_CANCELAR
  else
    if FPasoActual > PASO_SELECCION_FINAL then
      FPasoActual := PASO_SELECCION_FINAL;

  Button_Siguiente.Enabled := (FPasoActual = PASO_SELECCION_MEDIO) OR ((FPasoActual = PASO_SELECCION_FINAL) AND FScanCancelado) OR (FPasoActual = PASO_CANCELAR);
  Button_Atras.Enabled     := (FPasoActual > PASO_SELECCION_MEDIO) and (FPasoActual <> PASO_ESCANEAR);

  Button_Atras.visible    := (FPasoActual > PASO_CANCELAR) AND (FPasoActual < PASO_SELECCION_FINAL);
  Button_Cancelar.visible := Button_Atras.visible;// AND (FPasoActual <> PASO_ESCANEAR);

  if ((FPasoActual = PASO_SELECCION_FINAL) AND FScanCancelado) OR (FPasoActual = PASO_CANCELAR) then
  begin
    //Button_Siguiente.Enabled := true;
    Button_Siguiente.Caption := Message_Asistente_Nuevo_Catalogo_Cerrar;
  end
  else
    if (FPasoActual = PASO_SELECCION_FINAL) then
      Button_Siguiente.Caption := Message_Asistente_Nuevo_Catalogo_Guardar
    else
      Button_Siguiente.Caption := Message_Asistente_Nuevo_Catalogo_Siguiente;

  // Dependiendo muestra un frame u otro
  DoPasoVisible(FPasoActual);

  // Dependiendo del paso actual, se ejecuta una acción u otra
  case FPasoActual of
    PASO_CANCELAR         : DoPasoCancelar();
    PASO_SELECCION_MEDIO  : DoPasoSeleccionMedio();
    PASO_ESCANEAR         : DoPasoEscanear();
    PASO_GUARDAR          : DoPasoGuardar();
    //PASO_SELECCION_FINAL  : DoTitulo(Message_Asistente_Nuevo_Catalogo_Guardar);
  end;

end;

procedure TForm_AddCatalogo.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TForm_AddCatalogo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // Si está escaneando solicita al usuario que hacer
  if FScaneando then
  begin
    CanClose := false;
    Frame_Scan1.Cancelar();

  end;
end;

procedure TForm_AddCatalogo.Button_SiguienteClick(Sender: TObject);
begin

  case FPasoActual of
    PASO_CANCELAR         : Close();

    PASO_SELECCION_MEDIO  : begin
                              if not Frame_SelecionarMedio1.IsValidate() then
                                exit;
                            end;

    PASO_ESCANEAR         : beep;

    PASO_GUARDAR          : begin
                              Button_Siguiente.Enabled := false;
                              try
                                // Guarda lo escaneado en el gestor de datos
                                DoGuardarEscaneado(FScan, FGestorDatos);

                                // Devuelve que se ha guardado y todo está ok
                                ModalResult := mrOk;

                                // Cierra el asistente
                                Close();
                              finally
                                Button_Siguiente.Enabled := true;
                              end;
                              // Sale del método
                              exit;
                            end;
  end;

  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSiguiente(1);
end;

procedure TForm_AddCatalogo.Button_CancelarClick(Sender: TObject);
begin
  case FPasoActual of
    PASO_SELECCION_MEDIO  : close();
    PASO_ESCANEAR         : if FScaneando then Frame_Scan1.Cancelar();
    PASO_GUARDAR          : Close();
  end;
end;

procedure TForm_AddCatalogo.Button_AtrasClick(Sender: TObject);
begin
  // Dependiendo del paso actual, se ejecuta una acción u otra
  DoAccionesAtrasSiguiente(-1);
end;

// Para escribir el título del asistente
procedure TForm_AddCatalogo.DoTitulo(titulo : String);
var
  Extra : string;
begin
  Label_Titulo_Asistente_Add.Caption := titulo;

  if FPasoActual > PASO_CANCELAR then
    Extra := ' ['+ inttostr(FPasoActual) + '/' + inttostr(PanelesAsistente.PageCount - 1)+']'
  else
    Extra := '';

  caption                            := Message_Asistente_Nuevo_Catalogo_titulo + Extra + ' - '+ titulo;
end;


// Para mostrar el frame correspondiente al paso actual
procedure TForm_AddCatalogo.DoPasoVisible(paso : longint);
begin
  PanelesAsistente.ActivePageIndex := paso;
end;


// Paso - Escanear medio
procedure TForm_AddCatalogo.DoPasoEscanear();

 function GetExcluir() : string;
 begin
   Result := Frame_SelecionarMedio1.EditExcluir.Lines.Text;
   Result := StringReplace(Result, #13 + #10, ';', [rfReplaceAll, rfIgnoreCase]);
   Result := StringReplace(Result, #13, ';', [rfReplaceAll, rfIgnoreCase]);
   Result := StringReplace(Result, #10, ';', [rfReplaceAll, rfIgnoreCase]);
 end;

begin
  DoTitulo(Message_Asistente_Nuevo_Catalogo_Titulo_Escanear_Medio);

  // Se libera el anterior
  if FScan <> nil then
   FScan.Free;

  {$IFDEF ESCANEAR_DIRECTAMENTE}
  // Inicializar el Motor de Escaneo
  FScan := TMotorScan.Create('', '', TItemDatoTipo.Root);
  {$ELSE}
  FScan := TMotorScan.Create(Frame_SelecionarMedio1.Edit_Nombre.Text, Frame_SelecionarMedio1.EditDescripcion.Text, Frame_SelecionarMedio1.GetTipoCatalogo);
  {$ENDIF ESCANEAR_DIRECTAMENTE}


  FScanCancelado := false;
  FScaneando     := true;

  // Iniciar el escaneo
  Frame_Scan1.Iniciar(@DoCancelarScan, FScan);

{$IFDEF ESCANEAR_DIRECTAMENTE}

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

{$ELSE}

  FScan.ScanDirAsync(Frame_SelecionarMedio1.EditRuta.Text, @DoOnTerminarScanAsync, GetExcluir() );

{$ENDIF ESCANEAR_DIRECTAMENTE}
end;

// Paso - Escanear cancelar
procedure TForm_AddCatalogo.DoPasoCancelar();
begin
  DoTitulo(Message_Asistente_Nuevo_Catalogo_Titulo_Cancelar);
end;


// Paso - Escanear guardar
procedure TForm_AddCatalogo.DoPasoGuardar();
begin
  Frame_Guardar1.MostrarEstadisticaas(FScan);

  DoTitulo(Message_Asistente_Nuevo_Catalogo_Titulo_Guardar);
end;

// Paso - Selecionar Medio
procedure TForm_AddCatalogo.DoPasoSeleccionMedio();
begin
  Frame_Guardar1.MostrarEstadisticaas(FScan);

  DoTitulo(Message_Asistente_Nuevo_Catalogo_Titulo_Selector_Medio);
end;



// Cuando termina el escaneo correctamente
procedure TForm_AddCatalogo.DoOnTerminarScanAsync();
begin
  FScaneando := false;

  // Dependiendo del paso actual, se ejecuta una acción u otra
  if not FScanCancelado then
    DoAccionesAtrasSiguiente(1)
  else
  begin
    FPasoActual := PASO_CANCELAR;
    DoAccionesAtrasSiguiente(0);
  end;

end;

// Guarda lo escaneado en el gestor de datos
procedure TForm_AddCatalogo.DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);
begin
  if not FScanCancelado then
  begin

    // Muestra el mensaje de guardando
    FormLoadingShow(Self, Message_Asistente_Nuevo_Catalogo_Titulo_Guardando, Message_Asistente_Nuevo_Catalogo_Cuerpo_Guardando);
    try
      // Guarda lo escaneado en el gestor de datos
      Frame_Guardar1.SaveAsync(SistemaGuardado, Scan);

    finally
      // Oculta el mensaje de guardando
      FormLoadingHide();
    end;

  end;
end;

// Metodo que recoge la acción de cancelar el escaneo
procedure TForm_AddCatalogo.DoCancelarScan();
begin
  // Cancela el escaneo
  FScanCancelado := True;
end;


// Sobre escribe el showmodal
function TForm_AddCatalogo.ShowModal : Integer;
begin
  inherited ShowModal();

  if Frame_Guardar1.GuardadoCorrecto then
    ModalResult := mrOk
  else
    ModalResult := mrCancel;
end;

end.
