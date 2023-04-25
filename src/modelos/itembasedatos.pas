(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 16:25:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-25 19:06:14
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

unit ItemBaseDatos;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  ItemBase
;

type

  // Tipos de items
  TItemDatoTipo = (
                    NoDefinido,
                    Directorio,
                    Archivo,
                    ArchivoExe,
                    Root,
                    RootDVD,
                    RootUSB,
                    RootHDD,
                    RootCarpeta,
                    RootUnidadRed
  );

  { ItemBaseDatos }
  TItemBaseDatos = class(TItemBase)
  private
    FTipo  : TItemDatoTipo;
    FFecha : TDateTime;
    FSize  : int64;
  protected
    // Para poder Generar el Id
    function DoGenerarId(const extra: string): Qword; override;
  public
    // Constructor de la clase
    constructor Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64 = 0);

    // Devuelve el tipo en formato string
    function GetTipoString(): string; virtual;

    // Propiedades
    property Tipo  : TItemDatoTipo read FTipo;
    property Fecha : TDateTime read FFecha write FFecha;
    property Size  : int64 read FSize;
  end;


implementation

uses
  TypInfo
;

// Convierte el nombre de un TDatoItemTipo a string
function TDatoItemTipoToString(Value: TItemDatoTipo): string;
begin
  Result := GetEnumName(typeInfo(TItemDatoTipo), Ord(Value));
end;

{ TItemBaseDatos }
constructor TItemBaseDatos.Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64 = 0);
begin
  FTipo  := ATipo;
  FFecha := AFecha;
  FSize  := ASize;
  inherited Create(ANombre);
end;

// Para poder Generar el Id
function TItemBaseDatos.DoGenerarId(const extra: string): Qword;
begin
  Result :=  inherited DoGenerarId(GetTipoString() + ' | ' + extra);
  // + DateTimeToStr(Fecha) + ' | ' + IntToStr(FSize) + ' | '
end;

// Devuelve el tipo en formato string
function TItemBaseDatos.GetTipoString(): string;
begin
  Result := TDatoItemTipoToString(FTipo);
end;

end.