(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 15:04:15
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-15 15:17:22
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

unit ItemExtension;

{$mode ObjFPC}{$H+}

interface


type
  { TItemExtension }
  TItemExtension = class
  private
    FId                  : Qword;
    FExtension           : RawByteString;
    FDescripcion         : RawByteString;
  protected
  public
    // Constructor de la clase
    constructor Create(const AExtension: RawByteString; const ADescripcion: RawByteString);

    // Propiedades
    property Id               : Qword read FId;
    property Extension        : RawByteString read FExtension;
    property Descripcion      : RawByteString read FDescripcion;
  end;


implementation

uses
  Control_CRC
;

{ TItemExtension }
constructor TItemExtension.Create(const AExtension: RawByteString; const ADescripcion: RawByteString);
begin
  inherited Create;
  FId          := CRC64_From_String(AExtension);
  FExtension   := AExtension;
  FDescripcion := ADescripcion;
end;

end.
