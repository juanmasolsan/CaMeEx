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
  { TMotorScanCustom }
  TMotorScanCustom = class
  private
    FDetener_Escaneo_Busqueda_Archivos : boolean;
    FMascaraArchivo : RawByteString;
    FRoot  : TDatoItem;
  protected
    // Devuelve la ruta a procesar, incluyendo la máscara de archivo
    function GetRutaProcesar(Ruta : RawByteString): RawByteString; virtual;

    // Procesa un archivo o directorio encontrado
    function DoProcesarItem(const SearchRec: TSearchRec; const Padre: TDatoItem) : TDatoItem; virtual;

    // Realiza un escaneo de archivos y directorios recursivo de un directorio dado
    procedure DoScanDir(Directorio : RawByteString; Padre: TDatoItem);

  public
    // Constructor de la clase
    constructor Create();

    // Destructor de la clase
    destructor Destroy(); override;

    // Métodos de la clase - Inicia un escaneo de archivos y directorios de un directorio dado
    procedure ScanDir(Directorio : RawByteString); virtual;

    // Métodos de la clase - Detiene el escaneo de archivos y directorios
    procedure StopScan(); virtual;

    // Propiedades de la clase
    property MascaraArchivo : RawByteString read FMascaraArchivo write FMascaraArchivo;
    property Root  : TDatoItem read FRoot;
  end;

type
  { TMotorScan }
  TMotorScan = class(TMotorScanCustom);


implementation

// Constructor de la clase
constructor TMotorScanCustom.Create();
begin
  inherited Create();

  FRoot                              := TDatoItem.create(TDatoItemTipo.Root);
  FDetener_Escaneo_Busqueda_Archivos := false;
  FMascaraArchivo                    := '*';
end;

destructor TMotorScanCustom.Destroy();
begin
  // Eliminar el objeto raíz
  FRoot.Free;

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
  DosError := FindFirstUtf8(RootName, faAnyFile, SearchRec);
  try
    // Procesar el primer archivo o directorio encontrado
    while DosError = 0 do
    begin

      // Procesa el archivo o directorio encontrado si es válido
      if ((SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') ) then
      begin
        //TODO: Poder excluir del scan con patrones

        //TODO: Procesar o guardar el archivo o directorio encontrado
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
  Item : TDatoItem;
  Tipo : TDatoItemTipo;
  Id   : int64 = -1;
begin

  // Determinar el tipo de archivo o directorio
  if (SearchRec.Attr and faDirectory)= faDirectory then
    Tipo := TDatoItemTipo.Directorio
  else
    Tipo := TDatoItemTipo.Archivo;

  // Crear el objeto TDatoItem
  //TODO: Generar el ID del objeto
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
end;


end.

