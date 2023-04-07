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

unit ItemData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Fgl
  ;

type

  // Predefinicón de la clase
  { TDatoItem }
  TDatoItem = class;

  // Clase que puede contener una lista de TDatoItem
  { TArrayDatoItem }
  TArrayDatoItem = class(specialize TFPGList<TDatoItem>)
  private
  protected
    // Limpia internamente la lista
    procedure Do_Clear;
  public
    // Destructor de la clase
    destructor Destroy; override;

    // Limpia la lista
    procedure Clear; virtual;
  end;

  // Tipos de items
  TDatoItemTipo = (
                    NoDefinido,
                    //TODO: Definir los distintos tipos de Root (DVD, USB, HDD, Carpeta, Unidad  de red, etc)
                    Root,
                    Directorio,
                    Archivo,
                    ArchivoExe
  );


  // Clase que contiene los datos de un item del catálogo
  { TDatoItem }
  TDatoItem = class
  private
    FId                  : int64;
    FTipo                : TDatoItemTipo;
    FAtributos           : Integer;
    FDateTime            : TDateTime;
    FSize                : int64;
    FNombre              : RawByteString;
    FExtension           : RawByteString;
    FImageIndex          : Integer;
    FImageIndexSistema   : Integer;

    FHijos               : TArrayDatoItem;

    FParentId            : int64;
    FParent              : TDatoItem;


  protected
    // Obtiene el índice de la imagen
    function GetImageIndex() : Integer; virtual;

    // Obtiene el índice de la imagen del sistema
    function GetImageIndexSistema() : Integer; virtual;
  public
    // Constructores de la clase
    constructor Create(const AId: int64; const ATipo: TDatoItemTipo; const AAtributos: Integer; const ADateTime: TDateTime; const ASize: int64; const ANombre: RawByteString);
    constructor Create(ATipo: TDatoItemTipo);

    // Destructor de la clase
    destructor Destroy; override;

    // Añade un hijo a la lista de hijos
    function AddHijo(Hijo : TDatoItem) : Integer; virtual;

    // Obtiene el número de hijos
    function HijosCount() : Integer; virtual;

    // Obtiene un hijo
    function GetHijo(Index : Integer) : TDatoItem; virtual;

    // Propiedades
    property Id               : int64 read FId;
    property Tipo             : TDatoItemTipo read FTipo;
    property Atributos       : Integer read FAtributos;
    property DateTime         : TDateTime read FDateTime;
    property Size             : int64 read FSize;
    property Nombre           : RawByteString read FNombre;
    property Extension        : RawByteString read FExtension;
    property ImageIndex       : Integer read FImageIndex;
    property ImageIndexSistema: Integer read FImageIndexSistema write FImageIndexSistema;

    property ParentId         : int64 read FParentId;
    property Parent           : TDatoItem read FParent;
  end;


implementation


{ TArrayDatoItem }
// Destructor de la clase
destructor TArrayDatoItem.Destroy;
begin
  Clear;
  inherited Destroy;
end;

// Limpia internamente la lista
procedure TArrayDatoItem.Do_Clear;
var
  _t, total : longint;
begin
  total := count -1;
  for _t := total downto 0 do
    begin
      Items[_t].Free;
    end;
end;

// Limpia la lista
procedure TArrayDatoItem.Clear;
begin
  Do_Clear;
  inherited Clear;
end;

{TDatoItem}
// Constructor de la clase
constructor TDatoItem.Create(const AId: int64; const ATipo: TDatoItemTipo; const AAtributos: Integer; const ADateTime: TDateTime; const ASize: int64; const ANombre: RawByteString);
begin
  // Llamamos al constructor de la clase padre
  inherited Create;

  // Inicializamos las propiedades
  FId                := AId;
  FTipo              := ATipo;
  FAtributos         := AAtributos;
  FDateTime          := ADateTime;
  FSize              := ASize;
  //TODO: El nombre en el root sea el nombre del dispositivo/medio
  FNombre            := ANombre;

  //TODO: Obtener la extensión del nombre
  //FExtension         := AExtension;

  // Obtenemos los índices de las imágenes
  FImageIndex        := GetImageIndex();
  FImageIndexSistema := GetImageIndexSistema();

  // Inicializamos la lista de hijos
  FHijos             := TArrayDatoItem.Create;
end;

// Constructor de la clase
constructor TDatoItem.Create(ATipo: TDatoItemTipo);
begin
  //TODO: El nombre en el root sea el nombre del dispositivo/medio
  Create(-1, ATipo, 0, 0, 0, '');
end;

// Destructor de la clase
destructor TDatoItem.Destroy;
begin
  // Liberamos la lista de hijos
  FHijos.Free;

  // Llamamos al destructor de la clase padre
  inherited Destroy;
end;

// Obtiene el índice de la imagen
function TDatoItem.GetImageIndex() : Integer;
begin
  //TODO: Implementar
  Result := -1;
end;

// Obtiene el índice de la imagen del sistema
function TDatoItem.GetImageIndexSistema() : Integer;
begin
  //TODO: Implementar
  Result := -1;
end;

// Añade un hijo a la lista de hijos
function TDatoItem.AddHijo(Hijo : TDatoItem) : Integer;
begin
  Hijo.FParentId := FId;
  Hijo.FParent   := Self;

  Result := FHijos.Add(Hijo);
end;

// Obtiene el número de hijos
function TDatoItem.HijosCount() : Integer;
begin
  Result := FHijos.Count;
end;

// Obtiene un hijo
function TDatoItem.GetHijo(Index : Integer) : TDatoItem;
begin
  Result := FHijos[Index];
end;


end.