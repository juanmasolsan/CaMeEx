(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 22:29:39
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-21 14:27:15
 *)
unit FrameGuardar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, SynEdit,
  MotorScan, InterfaceConectorDatos;

type

  { TFrame_Guardar }

  TFrame_Guardar = class(TFrame)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    SalidaLog: TSynEdit;
  private
    FGuardadoCorrecto : boolean;
  public
    // Muestra un resumen de todo lo que ha encontrado
    procedure MostrarEstadisticaas(ScanActivo : TMotorScan);
    function SaveAsync(Gestor : IConectorDatos; Scan : TMotorScan) : boolean;

    // Indica si se ha guardado correctamente
    property GuardadoCorrecto : boolean read FGuardadoCorrecto Default false;

  end;

implementation

uses
  AppString
  , Utilidades
  , ItemDato
  , ItemExtension
  , ItemRutaCompleta
  ;

{$R *.lfm}

type
  // Estado del thread
  TThreadEstado = class
    Inicio    : qword;
    Resultado : boolean;
  end;

  { TGuardarThread }
  TGuardarThread = class(TThread)
    private
      FEstado : TThreadEstado;
      FGestor : IConectorDatos;
      FScan   : TMotorScan;
      FFrame  : TFrame_Guardar;
    protected
      // Ejecuta el thread
      procedure Execute; override;

      // Guarda lo escaneado en el gestor de datos
      procedure DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);
    public
      // constructor
      Constructor Create(CreateSuspended : boolean; Frame : TFrame_Guardar; Gestor : IConectorDatos; Scan : TMotorScan; Estado : TThreadEstado);
    end;

{ TGuardarThread }
constructor TGuardarThread.Create(CreateSuspended : boolean; Frame : TFrame_Guardar; Gestor : IConectorDatos; Scan : TMotorScan; Estado : TThreadEstado);
begin
  // Inicializa el thread
  FEstado := Estado;
  FGestor := Gestor;
  FScan   := Scan;
  FFrame  := Frame;

  // Para que se libere la memoria al finalizar
  FreeOnTerminate := true;

  // Crea el thread
  inherited Create(CreateSuspended);
end;

procedure TGuardarThread.Execute;
begin
  // Inicializa el resultado
  FEstado.Resultado := false;

  // Guarda lo escaneado en el gestor de datos
  DoGuardarEscaneado(FScan, FGestor);

  // Establece el tiempo distinto de 0
  FEstado.Inicio   := GetTickCount64();
end;

// Guarda lo escaneado en el gestor de datos
procedure TGuardarThread.DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);

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

    // Le indica al frame que se ha guardado correctamente
    FFrame.FGuardadoCorrecto := true;
  end;
end;




// Muestra un resumen de todo lo que ha encontrado
procedure TFrame_Guardar.MostrarEstadisticaas(ScanActivo : TMotorScan);
begin
  SalidaLog.Clear;

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(ScanActivo.Root.Nombre);
  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Total_Directorios + '  ' + PuntearNumeracion(ScanActivo.TotalDirectorios));
  SalidaLog.lines.Add(Message_Total_Archivos + '  ' + PuntearNumeracion(ScanActivo.TotalArchivos));

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Total_Size + '  ' + PuntearNumeracion(ScanActivo.TotalSize) + ' (' + ConvertirSizeEx(ScanActivo.TotalSize, ',##', '.0' ) + ')');

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Tiempo_Escaneo + '  ' + MostrarTiempoTranscurrido(ScanActivo.ScanInicio, ScanActivo.ScanFinal));

  SalidaLog.Repaint;

end;


// Guarda los datos de forma asyn
function TFrame_Guardar.SaveAsync(Gestor : IConectorDatos; Scan : TMotorScan) : boolean;
var
  Estado : TThreadEstado;
begin
  // Crear el estado del thread
  Estado := TThreadEstado.Create();
  try
    // Inicializa el estado
    Estado.inicio    := 0;
    Estado.Resultado := False;

    // Crea el thread
    TGuardarThread.Create(false, self, Gestor, Scan, Estado);

    // Espera a que termine
    while Estado.Inicio = 0 do
    begin
      Application.ProcessMessages();
      sleep(1);
    end;

    // Devuelve el resultado
    Result := Estado.Resultado;

  finally
    // Libera el estado
    Estado.free;
  end;
end;


end.

