(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:21:53
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-17 16:45:25
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

    // Añade el icono de una extension
    procedure AddExtensionIcono(Extension : TItemExtension);

    // Devuelve los datos de una extensión
    function GetExtensionById(Id : Qword) : TItemExtension;

    // Añade una ruta completa
    procedure AddRutaCompleta(Ruta : TItemRutaCompleta);

    // Devuelve los datos de una extensión de un Item
    function GetRutaCompleta(Item : TItemDato) : RawByteString;

    // Añade un catálogo
    procedure AddCatalogo(Catalogo : TItemCatalogo);

    // Añade un dato
    procedure AddDato(Dato : TItemDato);

    // Devuelve todos los catalogos
    function GetAllCatalogos() : TArrayItemDato;

    // Devuelve un catalogo por su id
    function GetCatalogosById(id : qword) : TItemCatalogo;

    // Devuelve la lista de datos que contiene un catalogo y que desciendan de un padre
    function GetDatos(Padre : TItemDato) : TArrayItemDato;

    // Devuelve la lista de directorios que contiene un padre
    //function GetDirectorios(Padre : TItemDato) : TArrayItemDato;
    function GetDirectorios(Padre : TItemDato; Listado : TArrayItemDato) : integer;

    // Elimina todos los catalogos
    function DeleteAllCatalogos() : boolean;

    // Elimina todos los catalogos de forma asyn
    function DeleteAllCatalogosAsync() : boolean;


    // Elimina un catalogo
    function DeleteCatalogo(Catalogo : TItemCatalogo) : boolean;

    // Elimina un dato
    function DeleteDato(Dato : TItemDato) : boolean;

    // Elimina un dato de forma async
    function DeleteDatoAsync(Dato : TItemDato) : boolean;

    // Actualiza un catálogo
    procedure UpdateCatalogo(Catalogo : TItemCatalogo);

    // Actualiza los totales de un catálogo
    procedure UpdateTotalesCatalogo(Catalogo : TItemCatalogo);

    // Para que se marque el inicio de una transaccion
    procedure BeginUpdate();

    // Para que se marque el final de una transaccion
    procedure EndUpdate();
  end;

implementation

end.

