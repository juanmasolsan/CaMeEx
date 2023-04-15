(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 16:02:15
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-15 16:17:53
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

unit ItemRutaCompleta;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  ItemBase
;

type
  { TItemRutaCompleta }
  TItemRutaCompleta = class(TItemBase)
  private
    FIdCatalogo        : Qword;
  protected
    // Para poder Generar el Id
    function DoGenerarId(): Qword; override;
  public
    // Constructor de la clase
    constructor Create(const ANombre: RawByteString; const AIdCatalogo: Qword);

    // Propiedades
    property IdCatalogo      : Qword read FIdCatalogo;
  end;


implementation

uses
  Control_CRC
;

{ TItemRutaCompleta }
constructor TItemRutaCompleta.Create(const ANombre: RawByteString; const AIdCatalogo: Qword);
begin
  FIdCatalogo := AIdCatalogo;
  inherited Create(ANombre);
end;

// Para poder Generar el Id
function TItemRutaCompleta.DoGenerarId(): Qword;
begin
  Result := CRC64_From_String(Nombre + ' | ' +  inttostr(FIdCatalogo));
end;

end.