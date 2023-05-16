(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-16 15:54:31
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-16 16:32:08
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
unit AppString;

{$mode ObjFPC}{$H+}

interface

uses
  DefaultTranslator
  , LCLTranslator
  ;


resourcestring
  Message_Excepcion_Detectada = 'Excepción Detectada';

  Message_Total_Directorios = 'Total Directorios      :';
  Message_Total_Archivos    = 'Total Archivos         :';
  Message_Total_Size        = 'Total tamaño detectado :';
  Message_Tiempo_Escaneo    = 'Tiempo de escaneo      :';

  Message_Guardando       = 'Guardando datos ...';
  Message_Tiempo_Guardado = 'Tiempo en guardar    :';
  Message_Tiempo_Empleado = 'Tiempo empleado :';
  Message_Directorios     = 'Directorios :';
  Message_Archivos        = 'Archivos :';

  Message_Estadisticas    =  '%s directorio(s) y %s archivo(s) con %s elementos en total y ocupando %s  (%s bytes) - Tiempo empleado :  %s  ms';

  Message_DLG_Cancelar_escaneo = 'Escaneo en progreso.\r¿Realmente quiere cancelar el escaneo actual?';
  Message_Atencion             =  'Atención';


implementation


end.