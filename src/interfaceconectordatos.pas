(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:21:53
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-24 19:28:53
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

unit InterfaceConectorDatos;

{$mode ObjFPC}{$H+}

interface

uses
  ItemExtension, ItemRutaCompleta, ItemCatalogo, ItemDato;

type
  { IConectorDatos }
  IConectorDatos = interface

    // Inicializa el conector de datos
    procedure Iniciar(Curdir: string; SaveDir : string);

    // Finaliza el conector de datos
    procedure Finalizar();

    // Añade una extension
    procedure AddExtension(Extension : TItemExtension);

    // Añade una ruta completa
    procedure AddRutaCompleta(Ruta : TItemRutaCompleta);

    // Añade un catálogo
    procedure AddCatalogo(Catalogo : TItemCatalogo);

    // Añade un dato
    procedure AddDato(Dato : TItemDato);

    // Devuelve todos los catalogos
    function GetAllCatalogos() : TArrayItemDato;

    // Devuelve un catalogo por su id
    function GetCatalogosById(id : qword) : TItemCatalogo;

    // Devuelve la lista de datos que contiene un catalogo y que desciendan de un padre
    function GetDatos(Catalogo : TItemCatalogo; Padre : TItemDato) : TArrayItemDato;

    // Elimina todos los catalogos
    function DeleteAllCatalogos() : boolean;

    // Elimina un catalogo
    function DeleteCatalogo(Catalogo : TItemCatalogo) : boolean;

    // Elimina un dato
    function DeleteDato(Dato : TItemDato) : boolean;

  end;

implementation

end.

