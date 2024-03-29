(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 16:39:00
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-11 19:14:09
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

unit ItemCatalogo;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
  , ItemBaseDatos
  , ItemDato
;

type

  { TItemCatalogo }
  TItemCatalogo = class(TItemDato)
  private
    FTotalArchivos   : Qword;
    FTotalDirectorios: Qword;
  protected
    // Para poder Generar el Id
    function DoGenerarId(const extra: string): Qword; override;
  public
    // Constructor de la clase
    constructor Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64; const ADescripcion: RawByteString = ''; ATotalArchivos: Qword = 0; ATotalDirectorios: Qword = 0);

    // Agrega información extra
    procedure AddExtraInfo(catalogo : TItemCatalogo);

    // Resetea la información extra
    procedure ResetExtraInfo();

    // Propiedades
    property TotalArchivos   : Qword         read FTotalArchivos write FTotalArchivos;
    property TotalDirectorios: Qword         read FTotalDirectorios write FTotalDirectorios;
  end;


implementation


{ TItemCatalogo }
constructor TItemCatalogo.Create(const ANombre: RawByteString; ATipo : TItemDatoTipo; AFecha : TDateTime; ASize : int64; const ADescripcion: RawByteString = ''; ATotalArchivos: Qword = 0; ATotalDirectorios: Qword = 0);
begin
  Descripcion       := ADescripcion;
  FTotalArchivos    := ATotalArchivos;
  FTotalDirectorios := ATotalDirectorios;

  inherited Create(ANombre, ATipo, AFecha, ASize);
end;

// Para poder Generar el Id
function TItemCatalogo.DoGenerarId(const extra: string): Qword;
begin
  Result := inherited DoGenerarId(extra);
end;

// Agrega información extra
procedure TItemCatalogo.AddExtraInfo(catalogo : TItemCatalogo);
begin
  FTotalArchivos    += catalogo.TotalArchivos;
  FTotalDirectorios += catalogo.TotalDirectorios;
  Size              := Size + catalogo.Size;
end;

// Resetea la información extra
procedure TItemCatalogo.ResetExtraInfo();
begin
  FTotalArchivos    := 0;
  FTotalDirectorios := 0;
  Size              := 0;
end;


end.
