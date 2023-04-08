(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-07 14:57:44
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-09 00:55:44
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
  , ItemData
  ;


type
  { TOnTerminarScanAsync }
  TOnTerminarScanAsync = procedure() of object;

  { TMotorScanCustom }
  TMotorScanCustom = class
  private
    FDetener_Escaneo_Busqueda_Archivos: boolean;
    FMascaraArchivo                   : RawByteString;
    FRoot                             : TDatoItem;
    FTotalArchivos                    : Integer;
    FTotalDirectorios                 : Integer;
    FTotalSize                        : Int64;
    FProcesando                       : RawByteString;

    FCriticalSection_Totales          : TCriticalSection;
    FCriticalSection_InfoProgreso     : TCriticalSection;

    FOnTerminarScanAsync              : TOnTerminarScanAsync;

    // Getters
    function GetTotalArchivos(): Integer;
    function GetTotalDirectorios(): Integer;
    function GetTotalSize(): Int64;
    function GetProcesando(): RawByteString;

  protected
    // Devuelve la ruta a procesar, incluyendo la máscara de archivo
    function GetRutaProcesar(Ruta : RawByteString): RawByteString; virtual;

    // Procesa un archivo o directorio encontrado
    function DoProcesarItem(const SearchRec: TSearchRec; const Padre: TDatoItem) : TDatoItem; virtual;

    // Realiza un escaneo de archivos y directorios recursivo de un directorio dado
    procedure DoScanDir(Directorio : RawByteString; Padre: TDatoItem);

    // Evento que se ejecuta cuando termina el escaneo Async
    procedure DoTerminarScanAsync(); virtual;

  public
    // Constructor de la clase
    constructor Create();

    // Destructor de la clase
    destructor Destroy(); override;

    // Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado
    procedure ScanDir(Directorio : RawByteString); virtual;

    // Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado de forma asíncrona
    procedure ScanDirAsync(Directorio : RawByteString; OnTerminarScan : TOnTerminarScanAsync); virtual;

    // Métodos de la clase - Detiene el escaneo de archivos y directorios
    procedure StopScan(); virtual;

    // Propiedades de la clase
    property MascaraArchivo : RawByteString read FMascaraArchivo write FMascaraArchivo;
    property Root  : TDatoItem read FRoot;

    // Contadores de archivos y directorios encontrados
    property TotalArchivos    : Integer read GetTotalArchivos;
    property TotalDirectorios : Integer read GetTotalDirectorios;

    // Tamaño total de los archivos encontrados
    property TotalSize        : Int64 read GetTotalSize;

    // Información de progreso
    property Procesando       : RawByteString read GetProcesando;

    // Evento que se ejecuta cuando termina el escaneo Async
    property OnTerminarScanAsync : TOnTerminarScanAsync read FOnTerminarScanAsync write FOnTerminarScanAsync;
  end;

type
  { TMotorScan }
  TMotorScan = class(TMotorScanCustom);

implementation

uses
crc
;


type
  { TTMotorScanDirThread }
  TMotorScanDirThread = class(TThread)
    private
      FDirectorio : RawByteString;
      FScan       : TMotorScanCustom;
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean; Scan : TMotorScanCustom; Directorio : RawByteString);
    end;

{ TMotorScanDirThread }
constructor TMotorScanDirThread.Create(CreateSuspended : boolean; Scan : TMotorScanCustom; Directorio : RawByteString);
begin
  FDirectorio     := Directorio;
  FScan           := Scan;
  FreeOnTerminate := true;
  inherited Create(CreateSuspended);
end;

procedure TMotorScanDirThread.Execute;
begin
  FScan.DoScanDir(FDirectorio, FScan.Root);
  FScan.DoTerminarScanAsync();
end;


{ TMotorScanCustom }
// Constructor de la clase
constructor TMotorScanCustom.Create();
begin
  inherited Create();

  FRoot                              := TDatoItem.create(TDatoItemTipo.Root);
  FDetener_Escaneo_Busqueda_Archivos := false;
  FMascaraArchivo                    := '*';

  // Inicializar el objeto de sincronización
  InitializeCriticalSection(FCriticalSection_Totales);

  // Inicializar el objeto de sincronización
  InitializeCriticalSection(FCriticalSection_InfoProgreso);
end;

destructor TMotorScanCustom.Destroy();
begin
  // Eliminar el objeto raíz
  FRoot.Free;

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
procedure TMotorScanCustom.ScanDir(Directorio : RawByteString);
begin
  // Indica que no se debe detener el escaneo
  FDetener_Escaneo_Busqueda_Archivos := false;

  // Inicializar los contadores de archivos y directorios
  FTotalArchivos                     := 0;
  FTotalDirectorios                  := 0;

  // Inicializar el tamaño total de los archivos encontrados
  FTotalSize                         := 0;

  // Inicia el escaneo de archivos y directorios
  DoScanDir(Directorio, FRoot);
end;

// Inicia un escaneo de archivos y directorios de un directorio dado
procedure TMotorScanCustom.DoScanDir(Directorio : RawByteString; Padre: TDatoItem);
var
  RootName   : RawByteString;
  DosError   : Integer;
  SearchRec  : TSearchRec;
  FlagsDir   : Integer = faAnyFile;
  Actual     : TDatoItem;

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
        Actual := DoProcesarItem(SearchRec, Padre);

        // Si es un directorio, llamar recursivamente a la función para procesar su contenido
        if (SearchRec.Attr and faDirectory)= faDirectory then
          DoScanDir(IncludeTrailingBackslash(Directorio) + SearchRec.Name, Actual);
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
function TMotorScanCustom.DoProcesarItem(const SearchRec: TSearchRec; const Padre: TDatoItem) : TDatoItem;
var
  Item        : TDatoItem;
  Tipo        : TDatoItemTipo;
  Id          : Qword = 0;
  IdData      : RawByteString;
  RutaCompleta: RawByteString;
begin

  // Determinar el tipo de archivo o directorio
  if (SearchRec.Attr and faDirectory)= faDirectory then
    begin
      Tipo := TDatoItemTipo.Directorio;

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
      Tipo := TDatoItemTipo.Archivo;

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
  IdData := lowercase(RutaCompleta) + '|' +
            IntToStr(SearchRec.Size) + '|' +
            IntToStr(SearchRec.Time) + '|' +
            IntToStr(SearchRec.Attr);

  // Generar el Id a partir de la string anterior (Basado en CRC64)
  Id := crc64(0,nil,0);
  Id := crc64(Id, @IdData[1], length(IdData));

  // Crear el objeto TDatoItem
  Item := TDatoItem.Create(Id,
                            Tipo,
                            SearchRec.Attr,
                            FileDateToDateTime(SearchRec.Time),
                            SearchRec.Size,
                            SearchRec.Name);

  // Añadir el objeto TDatoItem al padre
  Padre.AddHijo(Item);

  // Devolver el objeto TDatoItem
  Result := Item;

  Sleep(10); //TODO: Eliminar solo es para probar el funcionamiento con directorios pequeños
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
  if Assigned(FOnTerminarScanAsync) then
    FOnTerminarScanAsync();
end;

// Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado de forma asíncrona
procedure TMotorScanCustom.ScanDirAsync(Directorio : RawByteString; OnTerminarScan : TOnTerminarScanAsync);
var
  ScanDirThread : TMotorScanDirThread;
begin
  // Inicializa el evento que se ejecuta cuando termina el escaneo
  FOnTerminarScanAsync := OnTerminarScan;

  // Inicializa el hilo que ejecuta el escaneo de archivos y directorios
  ScanDirThread := TMotorScanDirThread.create(false, Self, Directorio);
end;



end.