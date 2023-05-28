(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-04 22:47:21
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 23:40:50
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

uses
  Graphics
  , ItemDato
  ;


type
  // Forma en la que mostrar el tamaño de los archivos
  TFormatoSize = (Normal, Puntuada, Automatico);

  // Forma en la que mostrar los iconos
  TFormatoIconos = (PorDefecto, Sistema, Mixto);


const
  COLUMNA_NOMBRE    = 0;
  COLUMNA_SIZE      = COLUMNA_NOMBRE + 1;
  COLUMNA_TIPO      = COLUMNA_SIZE + 1;
  COLUMNA_FECHA     = COLUMNA_TIPO + 1;
  COLUMNA_ATRIBUTOS = COLUMNA_FECHA + 1;
  COLUMNA_RUTA      = COLUMNA_ATRIBUTOS + 1;

var
  // Formato de la fecha y hora
  TipoHora                                  : RawByteString = 'dd/mm/yyyy  hh:mm:ss';

  // Columnas de la lista de archivos por la que ordenar y su dirección
  FColumnnaOrden                            : integer = COLUMNA_TIPO;
  FColumnnaOrden_Direccion                  : integer = 0;


  // Forma en la que mostrar el tamaño de los archivos
  FFormatoSize                              : TFormatoSize = TFormatoSize.Puntuada;

  // Forma en la que mostrar los iconos
  FFormatoIconos                            : TFormatoIconos = TFormatoIconos.PorDefecto;


  // Dimensiones del árbol de directorios
  FArbolAncho                               : integer = 230;

  // Dimensiones del Log
  FLogAlto                                  : integer = 200;

  // Resaltado de la columna de ordenación
  FResaltarColumnaOrden                     : boolean = true;
  FResaltarColumnaOrdenColor                : TColor = $eeeeee;



  // Colores varios
  FColor_Highlight                          : TColor =  clMenuHighlight;
  FColor_BtnShadow                          : TColor =  clBtnShadow;
  FForzarColorTexto_Normal                  : Tcolor = clWindowText;
  FForzarColorTexto_Seleccionado            : Tcolor = clHighlightText;
  FForzarColorTexto_Hot                     : Tcolor = clHighlightText;

  // Colores de los archivos
  FUsarColorDiferenciarArchivos             : boolean = true;
  FColor_SymLink                            : TColor = clBlue;
  FColor_Oculto_Sistema                     : TColor = clRed;
  FColor_SoloLectura                        : TColor = clGray;

  // Colores de los catalogos
  FUsarColoresCatalogos                     : boolean = true;

  // Colores de los catalogos
  FColor_Catalogos                          : array[0..9] of TColor = (
    clnone,
    clnone,
    clnone,
    clnone,
    clBlue,
    clRed,
    clGreen,
    clPurple,
    clTeal,
    clMaroon
  );

  // Muestra la información extra de los catalogos
  FExtraInfoCatalogos                      : boolean = true;


  // Para auto ocultar los botones de expandir/colapsar del arbol de catalogos
  FAutoOcultarBotonesArbol                 : boolean = false;

  // Mostrar los botones de expandir/colapsar del arbol de catalogos con un estilo moderno
  FVerBotonesArbolModernos                 : boolean = false;

  // Mostrar las lineas de los nodos del arbol de catalogos
  FVerLineasArbol                          : boolean = true;

  // Mostrar las lineas de los nodos del arbol de catalogos punteadas o no
  FVerLineasArbol_Punteadas                : boolean = true;

  // Mostrar la barra de búsqueda rápida
  FVerBarraBusqueda                        : boolean = true;

  // Mostrar la barra de herramientas
  FVerBarraHerramientas                    : boolean = true;

  // Mostrar la barra de estado
  FVerBarraEstado                          : boolean = true;

function GetColorCatalogo(const Indice: integer; IsRoot : Boolean): TColor;


function GetImageIndexByItemDato(const Item: TItemDato): integer;



var
  ColorThemeStyle : Tcolor = clWindow;

// Calcula los colores
procedure ReCalcularColores;

implementation

uses
  Utilidades
  , ItemBaseDatos
  , GestorExtensiones
  ;

var
  // Colores de los catalogos
  FColor_Catalogos_Nodo : array[0..9] of TColor;
  FColor_Catalogos_Root : array[0..9] of TColor;



procedure CalcularColores;
var
  t: integer;
begin
  for t := 0 to 9 do
  begin
    //FColor_Catalogos2[t] := Blend(FColor_Catalogos2[t], clWindow, 105);
    FColor_Catalogos_Nodo[t] := Blend(FColor_Catalogos[t], ColorThemeStyle, 105);
    FColor_Catalogos_Root[t] := Blend(FColor_Catalogos[t], ColorThemeStyle, 110);
  end;
end;

function GetColorCatalogo(const Indice: integer; IsRoot : Boolean): TColor;
begin
  if IsRoot then
    Result := FColor_Catalogos_Root[Indice]
  else
    Result := FColor_Catalogos_Nodo[Indice];
end;


procedure ReCalcularColores;
begin
  CalcularColores;
end;

function GetImageIndexByItemDato(const Item: TItemDato): integer;
begin
  Result := Item.ImageIndex;
  if Result = -1 then
    Result := 2;

  if Item.Tipo >= TItemDatoTipo.Root then
    Result := integer(Item.Tipo);

  case FFormatoIconos of
    Sistema : Result := GetExtensionIcopnIndexById(Item.IdExtension, Result);
    Mixto   : begin
                if Item.Tipo <> TItemDatoTipo.Directorio then
                  Result := GetExtensionIcopnIndexById(Item.IdExtension, Result);
              end;
  end;
end;


initialization
  CalcularColores;


end.
