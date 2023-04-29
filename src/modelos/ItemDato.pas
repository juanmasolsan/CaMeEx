(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 17:35:50
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-29 17:05:38
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

unit ItemDato;

{$mode ObjFPC}{$H+}

interface

{$notes off}
{$hints off}

uses
  Classes
  , SysUtils
  , Fgl
  , ItemBaseDatos
  ;

type
  // Predefinicón de la clase
  { TItemDato }
  TItemDato = class;

  // Clase que puede contener una lista de TItemDato
  { TArrayItemDato }
  TArrayItemDato = class(specialize TFPGList<TItemBaseDatos>)
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


  // Clase que contiene los datos de un item del catálogo
  { TItemDato }
  TItemDato = class(TItemBaseDatos)
  private
    FAtributos           : Integer;
    FExtension           : RawByteString;
    FImageIndex          : Integer;
    FImageIndexSistema   : Integer;
    FIdExtension         : Qword;
    FIdRutaCompleta      : Qword;
    FIdCatalogo          : Qword;
    FIdPadre             : Qword;

    FHijos               : TArrayItemDato;
    FParent              : TItemDato;






  protected
    // Obtiene el índice de la imagen
    function GetImageIndex() : Integer; virtual;

    // Obtiene el índice de la imagen del sistema
    function GetImageIndexSistema() : Integer; virtual;
  public
    // Constructores de la clase
    constructor Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64;
      AAtributos           : Integer;
      AExtension           : RawByteString;
      AImageIndex          : Integer;
      AIdExtension         : Qword;
      AIdRutaCompleta      : Qword;
      AIdCatalogo          : Qword;
      AIdPadre             : Qword
    );

    // Constructores de la clase
    constructor Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64 = 0);

    // Destructor de la clase
    destructor Destroy; override;

    // Añade un hijo a la lista de hijos
    function AddHijo(Hijo : TItemDato) : Integer; virtual;

    // Obtiene el número de hijos
    function HijosCount() : Integer; virtual;

    // Obtiene un hijo
    function GetHijo(Index : Integer) : TItemDato; virtual;

    // Limpia la lista de hijos
    procedure HijosClear(); virtual;


    // Obtiene la ruta completa del item
    function GetFullPath() : string; virtual;

    // Devuielve los datos del item en una string
    function ToString() : string; override;

    // Propiedades
    property Atributos        : Integer read FAtributos;
    property Extension        : RawByteString read FExtension;
    property ImageIndex       : Integer read FImageIndex;
    property ImageIndexSistema: Integer read FImageIndexSistema write FImageIndexSistema;

    property IdExtension      : Qword read FIdExtension;
    property IdRutaCompleta   : Qword read FIdRutaCompleta;
    property IdCatalogo       : Qword read FIdCatalogo;


    property IdPadre         : Qword read FIdPadre;
    property Parent           : TItemDato read FParent;
  end;


implementation

uses
  Utilidades;



{ TArrayItemDato }
// Destructor de la clase
destructor TArrayItemDato.Destroy;
begin
  Clear;
  inherited Destroy;
end;

// Limpia internamente la lista
procedure TArrayItemDato.Do_Clear;
var
  _t, total : longint;
begin
  total := count -1;
  for _t := total downto 0 do
    begin
      {%H-}Items[_t].Free;
    end;
end;

// Limpia la lista
procedure TArrayItemDato.Clear;
begin
  Do_Clear;
  inherited Clear;
end;

{TItemDato}
// Constructor de la clase
constructor TItemDato.Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64;
    AAtributos           : Integer;
    AExtension           : RawByteString;
    AImageIndex          : Integer;
    AIdExtension         : Qword;
    AIdRutaCompleta      : Qword;
    AIdCatalogo          : Qword;
    AIdPadre             : Qword
    );
begin

  // Inicializamos las propiedades
  //FAtributos         := AAtributos;
  //TODO: El nombre en el root sea el nombre del dispositivo/medio
  //FNombre            := ANombre;

  //TODO: Obtener la extensión del nombre
  //FExtension         := AExtension;

  // Obtenemos los índices de las imágenes
  FImageIndex        := GetImageIndex();
  FImageIndexSistema := GetImageIndexSistema();

  // Inicializamos los identificadores
  FAtributos         := AAtributos;
  FExtension         := AExtension;
  FIdExtension       := AIdExtension;
  FIdRutaCompleta    := AIdRutaCompleta;
  FIdCatalogo        := AIdCatalogo;
  FIdPadre           := 0;


  // Llamamos al constructor de la clase padre
  inherited Create(ANombre, ATipo, AFecha, ASize);


  // Inicializamos la lista de hijos
  FHijos             := TArrayItemDato.Create;
end;

constructor TItemDato.Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64 = 0);
begin
  Create(ANombre, ATipo, AFecha, ASize,
      0,
      '',
      0,
      0,
      0,
      0,
      0
      );
end;

// Destructor de la clase
destructor TItemDato.Destroy;
begin
  // Liberamos la lista de hijos
  FHijos.Free;

  // Llamamos al destructor de la clase padre
  inherited Destroy;
end;

// Obtiene el índice de la imagen
function TItemDato.GetImageIndex() : Integer;
begin
  //TODO: Implementar
  Result := -1;
end;

// Obtiene el índice de la imagen del sistema
function TItemDato.GetImageIndexSistema() : Integer;
begin
  //TODO: Implementar
  Result := -1;
end;

// Añade un hijo a la lista de hijos
function TItemDato.AddHijo(Hijo : TItemDato) : Integer;
begin
  Hijo.FIdPadre := Id;
  Hijo.FParent   := Self;

  Result := FHijos{%H-}.Add(Hijo);
end;

// Obtiene el número de hijos
function TItemDato.HijosCount() : Integer;
begin
  Result := FHijos.Count;
end;

// Obtiene un hijo
function TItemDato.GetHijo(Index : Integer) : TItemDato;
begin
  Result := TItemDato(FHijos{%H-}[Index]);
end;

// Limpia la lista de hijos
procedure TItemDato.HijosClear();
begin
  FHijos.Clear;
end;


function TItemDato.ToString() : string;
begin
  Result := '[Id: ' + AgregarEspaciosAlInicio(IntToStr(Id), 20) + '] [Tipo: ' + AgregarEspaciosAlFinal(GetTipoString(), 10) +'] [Atributos: ' + AgregarEspaciosAlInicio(AtributosToStr(FAtributos, false), 6) + '] [DateTime: ' + FechaToStr(Fecha)  + '] [Size: ' + AgregarEspaciosAlInicio(PuntearNumeracion(Size, true), 10)  + '] [ImageIndex: ' + IntToStr(FImageIndex) + '] [ImageIndexSistema: ' + IntToStr(FImageIndexSistema)+ '] [' + GetFullPath()+ ']';
end;

// Obtiene la ruta completa del item
function TItemDato.GetFullPath() : string;
var
  Padre : TItemDato;
begin
  // Inicializamos la ruta
  Result := Nombre;

  // Obtenemos el padre
  Padre := FParent;

  // Recorremos los padres
  while ((Padre <> nil) and (Padre.Nombre <> '') and ((Padre.Tipo <> TItemDatoTipo.NoDefinido) or (Padre.Tipo <> TItemDatoTipo.Root))) do
    begin
      // Añadimos el nombre del padre
      Result :=  IncludeTrailingBackslash(Padre.Nombre) + Result;

      // Obtenemos el padre del padre
      Padre := Padre.FParent;
    end;
end;




end.
