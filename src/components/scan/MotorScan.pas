(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-07 14:57:44
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-01 15:52:43
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


unit MotorScan;

{$mode ObjFPC}{$H+}

interface

uses
    LCLIntf
  , LCLType
  , Classes
  , SysUtils
  , FileUtil
  , LazFileUtils
  , LazUTF8
  , Contnrs
  , ItemBaseDatos
  , ItemCatalogo
  , ItemDato


  ;


type
  { TOnTerminarScanAsync }
  TOnTerminarScanAsync = procedure() of object;

  { TMotorScanCustom }
  TMotorScanCustom = class
  private
    FDetener_Escaneo_Busqueda_Archivos: boolean;
    FMascaraArchivo                   : RawByteString;
    FRoot                             : TItemCatalogo;
    FTotalArchivos                    : Integer;
    FTotalDirectorios                 : Integer;
    FTotalSize                        : Int64;
    FProcesando                       : RawByteString;

    FCriticalSection_Totales          : TCriticalSection;
    FCriticalSection_InfoProgreso     : TCriticalSection;

    FOnTerminarScanAsync              : TOnTerminarScanAsync;

    FScanInicio                       : TDateTime;
    FScanFinal                        : TDateTime;

    FListaExclusion                   : TStringList;

    FListaExtensiones                 : TFPHashList;
    FListaRutaCompleta                : TFPHashList;


    // Getters
    function GetTotalArchivos(): Integer;
    function GetTotalDirectorios(): Integer;
    function GetTotalSize(): Int64;
    function GetProcesando(): RawByteString;

  protected
    // Devuelve la ruta a procesar, incluyendo la máscara de archivo
    function GetRutaProcesar(Ruta : RawByteString): RawByteString; virtual;

    // Procesa un archivo o directorio encontrado
    function DoProcesarItem(const SearchRec: TSearchRec; const Padre: TItemDato; RealPath : RawByteString) : TItemDato; virtual;

    // Realiza un escaneo de archivos y directorios recursivo de un directorio dado
    procedure DoScanDir(Directorio : RawByteString; Padre: TItemDato);

    // Evento que se ejecuta cuando termina el escaneo Async
    procedure DoTerminarScanAsync(); virtual;

    // Resetea los datos del escaneo
    procedure DoResetData(Excluir : string);

    // Devuelve el id de la extensión
    function GetIdExtension(RutaCompleta: RawByteString; Ext: RawByteString; Dir : Boolean): qword;

    // Limpiar la lista de extensiones
    procedure DoLimpiarListaExtensiones();

    // Devuelve el id de la ruta completa
    function GetIdRutaCompleta(RutaCompleta: RawByteString; IdCatalog : Qword): qword;

    // Limpiar la lista de rutas completas
    procedure DoLimpiarListaRutaCompleta();

  public
    // Constructor de la clase
    constructor Create();

    // Destructor de la clase
    destructor Destroy(); override;

    // Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado
    procedure ScanDir(Directorio : RawByteString; Excluir : string); virtual;

    // Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado de forma asíncrona
    procedure ScanDirAsync(Directorio : RawByteString; OnTerminarScan : TOnTerminarScanAsync; Excluir : string); virtual;

    // Métodos de la clase - Detiene el escaneo de archivos y directorios
    procedure StopScan(); virtual;

    // Métodos de la clase - Devuelve si un archivo o directorio debe ser excluido
    function IsExcluido(Actual : string) : boolean;

    // Propiedades de la clase
    property MascaraArchivo   : RawByteString read FMascaraArchivo write FMascaraArchivo;
    property Root             : TItemCatalogo read FRoot;

    // Contadores de archivos y directorios encontrados
    property TotalArchivos    : Integer read GetTotalArchivos;
    property TotalDirectorios : Integer read GetTotalDirectorios;

    // Tamaño total de los archivos encontrados
    property TotalSize        : Int64 read GetTotalSize;

    // Información de progreso
    property Procesando       : RawByteString read GetProcesando;

    // Tiempo de escaneo
    property ScanInicio       : TDateTime read FScanInicio;
    property ScanFinal        : TDateTime read FScanFinal;

    // Lista de extensiones
    property ListaExtensiones  : TFPHashList read FListaExtensiones;

    // Lista de Rutas Completas
    property ListaRutaCompleta : TFPHashList read FListaRutaCompleta;

    // Evento que se ejecuta cuando termina el escaneo Async
    property OnTerminarScanAsync : TOnTerminarScanAsync read FOnTerminarScanAsync write FOnTerminarScanAsync;
  end;

type
  { TMotorScan }
  TMotorScan = class(TMotorScanCustom);


// Funciones auxiliares
function IsExeByExtension(Ext: RawByteString): Boolean;
function GetImageIndex(Ext: RawByteString; Dir : Boolean): Integer;


implementation

uses
  Control_Contine
, Control_Logger
, Control_CRC
, Utilidades
, ItemExtension, ItemRutaCompleta, graphics;


function IsExeByExtension(Ext: RawByteString): Boolean;
var
  _ext : RawByteString;
begin
  result := false;
  _ext := lowerCase(ext);
  if (_ext = '.exe') or (_ext = '.com') or (_ext = '.bat') or (_ext = '.pif') or (_ext = '.cmd') or (_ext = '.sh') then
    begin
      result := true;
    end;
end;

function GetImageIndex(Ext: RawByteString; Dir : Boolean): Integer;
begin
  result := -1;

  if dir then
    begin
      result := 0;
      exit;
    end;

  if IsExeByExtension(Ext) then
  begin
    result := 3;
    exit;
  end;
end;

type
  { TTMotorScanDirThread }
  TMotorScanDirThread = class(TThread)
    private
      FDirectorio : RawByteString;
      FScan       : TMotorScanCustom;
      FExcluir    : string;
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean; Scan : TMotorScanCustom; Directorio : RawByteString; Excluir : string);
    end;

{ TMotorScanDirThread }
constructor TMotorScanDirThread.Create(CreateSuspended : boolean; Scan : TMotorScanCustom; Directorio : RawByteString; Excluir : string);
begin
  FDirectorio     := Directorio;
  FScan           := Scan;
  FreeOnTerminate := true;
  FExcluir        := Excluir;
  inherited Create(CreateSuspended);
end;

procedure TMotorScanDirThread.Execute;
begin
  // Realizar el escaneo
  FScan.ScanDir(FDirectorio, FExcluir);
end;


{ TMotorScanCustom }
// Constructor de la clase
constructor TMotorScanCustom.Create();
begin
  inherited Create();

  //TODO: Pasarle un nombre, el tipo de objeto y la descripcion
  FRoot                              := TItemCatalogo.create('ROOT', TItemDatoTipo.Root, now(), 0, 'Descripcion', 0, 0);
  FDetener_Escaneo_Busqueda_Archivos := false;
  FMascaraArchivo                    := '*';
  FListaExclusion                    := TStringList.Create;
  FListaExtensiones                  := TFPHashList.Create;
  FListaRutaCompleta                 := TFPHashList.Create;

  // Inicializar el objeto de sincronización
  InitializeCriticalSection(FCriticalSection_Totales);

  // Inicializar el objeto de sincronización
  InitializeCriticalSection(FCriticalSection_InfoProgreso);
end;

destructor TMotorScanCustom.Destroy();
begin
  // Eliminar el objeto raíz
  FRoot.Free;

  // Eliminar la lista de exclusiones
  FListaExclusion.Free;

  // Eliminar la lista de extensiones
  DoLimpiarListaExtensiones();
  FListaExtensiones.Free;

  // Eliminar la lista de rutas completas
  DoLimpiarListaRutaCompleta();
  FListaRutaCompleta.Free;

  // Eliminar el objeto de sincronización
  DeleteCriticalSection(FCriticalSection_Totales);

  // Eliminar el objeto de sincronización
  DeleteCriticalSection(FCriticalSection_InfoProgreso);

  // Llamar al destructor de la clase base
  inherited Destroy();
end;


// Devuelve la ruta a procesar, incluyendo la máscara de archivo
function TMotorScanCustom.GetRutaProcesar(Ruta : RawByteString): RawByteString;
begin
  Result := Ruta;

  // Si la MascaraArchivo no está vacia la añade a la ruta, que además, si la ruta no termina con un separador de directorios, añadirlo
  if FMascaraArchivo <> '' then
    result := IncludeTrailingBackslash(result) + FMascaraArchivo;
end;

// Métodos de la clase - Detiene el escaneo de archivos y directorios
procedure TMotorScanCustom.StopScan();
begin
  // Indica que se debe detener el escaneo
  FDetener_Escaneo_Busqueda_Archivos := true;
end;

// Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado
procedure TMotorScanCustom.ScanDir(Directorio : RawByteString; Excluir : string);
begin
  // Inicializa los datos
  DoResetData(Excluir);

  try
    // Inicia el escaneo de archivos y directorios
    DoScanDir(Directorio, FRoot);
  except
    on e: Exception do LogAddException('Excepción Detectada Procesando Ruta : ' + Directorio, e);
  end;

  // Terminar el escaneo
  DoTerminarScanAsync();
end;

// Inicia un escaneo de archivos y directorios de un directorio dado
procedure TMotorScanCustom.DoScanDir(Directorio : RawByteString; Padre: TItemDato);
var
  RootName   : RawByteString;
  DosError   : Integer;
  SearchRec  : TSearchRec;
  FlagsDir   : Integer = faAnyFile;
  Actual     : TItemDato;

begin
  // Obtener la ruta a procesar con la MascaraArchivo
  RootName := GetRutaProcesar(Directorio);

  // Inicializar el enumerador
  DosError := FindFirstUtf8(RootName, FlagsDir, SearchRec);
  try
    // Procesar el primer archivo o directorio encontrado
    while DosError = 0 do
    begin

      // Procesa el archivo o directorio encontrado si es válido
      if ((SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') ) then
      begin
        //TODO: Poder excluir del scan con patrones

        // Actualizar el proceso actual protegido para evitar problemas de concurrencia
        EnterCriticalSection(FCriticalSection_InfoProgreso);
        try
          FProcesando := IncludeTrailingBackslash(Directorio) + SearchRec.Name;
        finally
          LeaveCriticalSection(FCriticalSection_InfoProgreso);
        end;

        // Procesar el archivo o directorio encontrado
        Actual := DoProcesarItem(SearchRec, Padre, IncludeTrailingBackslash(Directorio) + SearchRec.Name);

        // Si es un directorio, llamar recursivamente a la función para procesar su contenido
        if (Actual <> nil) and ((SearchRec.Attr and faDirectory)= faDirectory) then
        begin
          try
            DoScanDir(IncludeTrailingBackslash(Directorio) + SearchRec.Name, Actual);
          except
            on e: Exception do LogAddException('Excepción Detectada Procesando Ruta : ' + IncludeTrailingBackslash(Directorio) + SearchRec.Name, e);
          end;
        end;
      end;

      // Obtener el siguiente archivo o directorio
      DosError := FindNextUtf8(SearchRec);

      // Si se ha solicitado detener el escaneo, salir del bucle
      if FDetener_Escaneo_Busqueda_Archivos then
        DosError := 1;
    end;

  finally
    // Cerrar el enumerador
    FindCloseUtf8(SearchRec);
  end;
end;

// Procesa un archivo o directorio encontrado
function TMotorScanCustom.DoProcesarItem(const SearchRec: TSearchRec; const Padre: TItemDato; RealPath : RawByteString) : TItemDato;
var
  Item        : TItemDato;
  Tipo        : TItemDatoTipo;
  RutaCompleta: RawByteString;
  Extension   : RawByteString;

begin

  // Determinar el tipo de archivo o directorio
  if (SearchRec.Attr and faDirectory)= faDirectory then
    begin
      Tipo := TItemDatoTipo.Directorio;

      // Protección de acceso concurrente
      EnterCriticalSection(FCriticalSection_Totales);
      try
        FTotalDirectorios += 1;
      finally
        LeaveCriticalSection(FCriticalSection_Totales);
      end;
    end
  else
    begin
      Tipo := TItemDatoTipo.Archivo;

      // Protección de acceso concurrente
      EnterCriticalSection(FCriticalSection_Totales);
      try
        FTotalArchivos += 1;
        FTotalSize     += SearchRec.Size;
      finally
        LeaveCriticalSection(FCriticalSection_Totales);
      end;
    end;

  // Generar la string que identifica al archivo o directorio
  RutaCompleta := IncludeTrailingBackslash(Padre.GetFullPath()) +  SearchRec.Name;

  // Obtener la extensión del archivo
  Extension    := LowerCase(ExtractFileExt(SearchRec.Name));

  if IsExcluido(RutaCompleta) then
  begin
    result := nil;
    exit;
  end;

  // Crear el objeto TItemDato
  Item := TItemDato.Create(SearchRec.Name,
                            Tipo,
                            FileDateToDateTime(SearchRec.Time),
                            SearchRec.Size,
                            SearchRec.Attr,
                            Extension,
                            GetImageIndex(Extension, Tipo = TItemDatoTipo.Directorio),
                            GetIdExtension(RealPath, Extension, Tipo = TItemDatoTipo.Directorio),
                            GetIdRutaCompleta(RutaCompleta, FRoot.Id),
                            FRoot.Id,
                            0
    );

  // Añadir el objeto TItemDato al padre
  Padre.AddHijo(Item);

  // Devolver el objeto TItemDato
  Result := Item;
end;

// Getter
function TMotorScanCustom.GetTotalArchivos(): Integer;
begin
  EnterCriticalSection(FCriticalSection_Totales);
  try
    Result := FTotalArchivos;
  finally
    LeaveCriticalSection(FCriticalSection_Totales);
  end;
end;

// Getter
function TMotorScanCustom.GetTotalDirectorios(): Integer;
begin
  EnterCriticalSection(FCriticalSection_Totales);
  try
    Result := FTotalDirectorios;
  finally
    LeaveCriticalSection(FCriticalSection_Totales);
  end;
end;

// Getter
function TMotorScanCustom.GetTotalSize(): Int64;
begin
  EnterCriticalSection(FCriticalSection_Totales);
  try
    Result := FTotalSize;
  finally
    LeaveCriticalSection(FCriticalSection_Totales);
  end;
end;

// Getter
function TMotorScanCustom.GetProcesando(): RawByteString;
begin
  EnterCriticalSection(FCriticalSection_InfoProgreso);
  try
    Result := FProcesando;
  finally
    LeaveCriticalSection(FCriticalSection_InfoProgreso);
  end;
end;

// Evento que se ejecuta cuando termina el escaneo Async
procedure TMotorScanCustom.DoTerminarScanAsync();
begin
  // Actualiza la fecha y hora de finalización del escaneo
  FScanFinal := Now;

  // Ejecuta el evento que se ejecuta cuando termina el escaneo
  if Assigned(FOnTerminarScanAsync) then
    FOnTerminarScanAsync();
end;

// Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado de forma asíncrona
procedure TMotorScanCustom.ScanDirAsync(Directorio : RawByteString; OnTerminarScan : TOnTerminarScanAsync; Excluir : string);
begin
  // Inicializa el evento que se ejecuta cuando termina el escaneo
  FOnTerminarScanAsync := OnTerminarScan;

  // Inicializa el hilo que ejecuta el escaneo de archivos y directorios
  TMotorScanDirThread.create(false, Self, Directorio, Excluir);
end;

// Resetea los datos del escaneo
procedure TMotorScanCustom.DoResetData(Excluir : string);
begin
  // Limpia el Root
  FRoot.HijosClear();

  // Inicializa la lista de excluidos
  FListaExclusion.text := stringReplace(Excluir, ';', #13, [rfReplaceAll]).ToLower().Replace('\', '/');

  // Indica que no se debe detener el escaneo
  FDetener_Escaneo_Busqueda_Archivos := false;

  // Inicializar los contadores de archivos y directorios
  FTotalArchivos                     := 0;
  FTotalDirectorios                  := 0;

  // Inicializar el tamaño total de los archivos encontrados
  FTotalSize                         := 0;

  // Inicializar la información de progreso
  FScanInicio                        := Now();
  FScanFinal                         := Now();
end;

// Métodos de la clase - Devuelve si un archivo o directorio debe ser excluido
function TMotorScanCustom.IsExcluido(Actual : string) : boolean;
var
  t, total : integer;
begin
  // Se convierte a minisculas y se reemplazan los separadores de directorios
  Actual := Actual.ToLower().Replace('\', '/');

  total := FListaExclusion.count - 1;
  for t := 0 to total do
  begin
    // Si el archivo o directorio actual contiene la cadena de la lista de excluidos
    if IsContiene(Actual, FListaExclusion[t]) then
    begin
      // Debe ser excluido
      result := true;
      exit;
    end;
  end;

  result := false;
end;

// Devuelve el id de la extensión
function TMotorScanCustom.GetIdExtension(RutaCompleta: RawByteString; Ext: RawByteString; Dir : Boolean): qword;
var
  datoExtension   : TItemExtension;
  textoDescripcion: RawByteString = '';
  Icono           : TPortableNetworkGraphic = nil;
begin

  Ext := lowercase(Ext);

  if (Ext = '') then
  begin
    if Dir then
      Ext := '<dir>'
    else
      Ext := '<none>';
  end
  else
    if (Ext = '.exe') then
      Ext := '<exe>.('+ Get_CRC64ToString(RutaCompleta)+')';

  // Si no existe la extensión en la lista de extensiones
  datoExtension := TItemExtension(FListaExtensiones.Find(Ext));
  if datoExtension = nil then
  begin
      // Se obtiene la descripción de la extensión desde el sistema
      Icono := GetGenericFileIcon(RutaCompleta, textoDescripcion,  Dir);

      if Icono <> nil then
      begin
        Icono.savetofile('out/img_' + Get_CRC64ToString(Ext)+ '.png');
      end;

    if textoDescripcion = ''  then
      if not Dir then
        textoDescripcion := 'Archivo del tipo ' + Ext
      else
        textoDescripcion := 'Carpeta de archivos';

    // Si es un directorio la ext es especial
    if Dir then
      Ext := '<dir>';

    // Se crea el objeto TItemExtension y se añade a la lista de extensiones
    datoExtension := TItemExtension.Create(Ext, textoDescripcion, Icono);
    FListaExtensiones.Add(Ext, datoExtension);
  end;

  // Se devuelve el id de la extensión
  result := datoExtension.Id;
end;

// Limpiar la lista de extensiones
procedure TMotorScanCustom.DoLimpiarListaExtensiones();
var
  t : integer;
begin
  for t := 0 to FListaExtensiones.Count - 1 do
    TItemExtension(FListaExtensiones.Items[t]).Free();

  FListaExtensiones.Clear();
end;

// Devuelve el id de la ruta completa
function TMotorScanCustom.GetIdRutaCompleta(RutaCompleta: RawByteString; IdCatalog : Qword): qword;
var
  datoRutaCompleta : TItemRutaCompleta;

begin
  // Se elimina el último separador de directorios
  RutaCompleta := ExtractFilePath(ExcludeTrailingPathDelimiter(RutaCompleta));

  // Se convierte \ en /
  RutaCompleta := StringReplace(RutaCompleta, '\', '/', [rfReplaceAll]);

  // Si la ruta es vacía o es el directorio raíz
  if (RutaCompleta = '') or (RutaCompleta = '/')
  then
  begin
    result := 0;
    exit;
  end;

  // Si no existe la RutaCompleta en la lista de RutaCompleta
  datoRutaCompleta := TItemRutaCompleta(FListaRutaCompleta.Find(RutaCompleta));
  if datoRutaCompleta = nil then
  begin
    // Se crea el objeto TItemRutaCompleta y se añade a la lista de RutaCompleta
    datoRutaCompleta := TItemRutaCompleta.Create(RutaCompleta, IdCatalog);
    FListaRutaCompleta.Add(RutaCompleta, datoRutaCompleta);
  end;

  // Se devuelve el id de la extensión
  result := datoRutaCompleta.Id;
end;

// Limpiar la lista de rutas completas
procedure TMotorScanCustom.DoLimpiarListaRutaCompleta();
var
  t : integer;
begin
  for t := 0 to FListaRutaCompleta.Count - 1 do
    TItemExtension(FListaRutaCompleta.Items[t]).Free();

  FListaRutaCompleta.Clear();
end;


end.
