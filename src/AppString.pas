(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-16 15:54:31
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-30 00:21:59
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
  Message_Catalogo        = 'Catalogo de Medio Extraible';

  Message_Estadisticas    =  '%s directorio(s) y %s archivo(s) con %s elementos en total y ocupando %s  (%s bytes) - Tiempo empleado :  %s  ms';

  Message_DLG_Cancelar_escaneo = 'Escaneo en progreso.\r¿Realmente quiere cancelar el escaneo actual?';
  Message_Atencion             =  'Atención';

  Message_Eliminar_Datos          =  '¿Realmente quiere eliminar el item(s) seleccionado(s)?\r\rEsta acción no se puede deshacer.';
  Message_Eliminar_Catalogo       =  '¿Realmente quiere eliminar el catalogo "%s"?\r\rEsta acción no se puede deshacer.';
  Message_Eliminar_All_Catalogos  =  '¿Realmente quiere eliminar todos los catalogos?\r\rEsta acción no se puede deshacer.\r\rSe eliminaran todos los catalogos y sus datos asociados.\r\rPregunta (%u/2)';

  Message_Espera_Eliminar_Titulo   =  'Eliminando datos';
  Message_Espera_Eliminar_Archivos =  'Se están eliminando los datos solicitados ...';
  Message_Espera_Eliminar_Catalogo =  'Se está eliminando el catalogo seleccionado ...';
  Message_Espera_Eliminar_Catalogo_All =  'Se están eliminando todos los datos almacenados ...';

  Message_Propiedades_de =  'Propiedades de :';
  Message_Atributos      =  'Atributos';
  Message_Calculando     =  'Calculando ...';
  Message_Contiene       =  '%s Archivos y %s Carpetas ';

  Message_Asistente_Search = 'Búsqueda Avanzada';


  Message_Asistente_Nuevo_Catalogo_titulo    =  'Añadir nuevo catalogo';
  Message_Asistente_Nuevo_Catalogo_Siguiente =  'Siguiente';
  Message_Asistente_Nuevo_Catalogo_Cerrar    =  'Cerrar';
  Message_Asistente_Nuevo_Catalogo_Guardar   =  'Guardar';

  Message_Asistente_Nuevo_Catalogo_Titulo_Cancelar       = 'Se ha cancelado la operación ...';
  Message_Asistente_Nuevo_Catalogo_Titulo_Escanear_Medio = 'Analizando el medio seleccionado ...';
  Message_Asistente_Nuevo_Catalogo_Titulo_Guardar        = 'Guardar datos del análisis ...';

  Message_Asistente_Nuevo_Catalogo_Titulo_Guardando      = 'Guardando los datos del análisis ...';
  Message_Asistente_Nuevo_Catalogo_Cuerpo_Guardando      = 'Se están guardando los datos del análisis, esto puede tardar un poco.\r\rPor favor espere.';

  Message_Asistente_Nuevo_Catalogo_Titulo_Selector_Medio = 'Datos básicos ...';

  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Carpeta = 'Seleccionar Carpeta ...';
  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Usb     = 'Unidad USB';
  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Dvd     = 'Unidad DVD/CD/BD';
  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Hdd     = 'Disco Duro/SSD';
  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Red     = 'Unidad de Red';
  Message_Asistente_Nuevo_Catalogo_Tipo_Medio_RAM     = 'Unidad RAM Disk';

  Message_Exportacion_Generado_Por =  'Generado por :';
  Message_Exportacion_Titulo       =  'Exportación Generada por :';
  Message_Exportacion_Nombre       =  'Nombre     :';
  Message_Exportacion_Size         =  'Tamaño     :';
  Message_Exportacion_Tipo         =  'Tipo       :';
  Message_Exportacion_Fecha        =  'Modificado :';
  Message_Exportacion_Atributos    =  'Atributos  :';
  Message_Exportacion_Ruta         =  'Ruta       :';
  Message_Exportacion_Espera_Titulo=  'Exportando datos';
  Message_Exportacion_Espera_Info  =  'Se están exportando los datos solicitados ...';

  Buscar_Cajon_TExto               = 'Buscar...';




implementation


end.
