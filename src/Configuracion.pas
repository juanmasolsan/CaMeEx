(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-04 22:47:21
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-04 22:50:43
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

unit Configuracion;

{$mode ObjFPC}{$H+}

interface


const
  COLUMNA_NOMBRE    = 0;
  COLUMNA_SIZE      = COLUMNA_NOMBRE + 1;
  COLUMNA_TIPO      = COLUMNA_SIZE + 1;
  COLUMNA_FECHA     = COLUMNA_TIPO + 1;
  COLUMNA_ATRIBUTOS = COLUMNA_FECHA + 1;
  COLUMNA_RUTA      = COLUMNA_ATRIBUTOS + 1;

var
  // Formato de la fecha y hora
  TipoHora          : RawByteString = 'dd/mm/yyyy  hh:mm:ss';

  // Columnas de la lista de archivos por la que ordenar y su dirección
  FColumnnaOrden                           : integer = COLUMNA_TIPO;
  FColumnnaOrden_Direccion                 : integer = 0;




implementation


end.
