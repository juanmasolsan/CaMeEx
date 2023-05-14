(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-03 22:31:23
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-14 14:04:12
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

unit GestorExtensiones;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  Controls
  , InterfaceConectorDatos;


// Inicializa el sistema que devuelve la descripción e icono index de las extensiones
procedure SetExtensionesConfig(GestorDatos: IConectorDatos; ImagenList : TImageList; ImagenList32 : TImageList);

// Devuelve la descripción de la extensión
function GetExtensionDescripcionById(IdExtension: Qword): RawByteString;

// Devuelve el Icon Index de la extensión
function GetExtensionIcopnIndexById(IdExtension: Qword; PorDefecto : longint): longint;

// Limpia la cache de extensiones
procedure ClearCacheExtensiones;


implementation

uses
  Contnrs
  , ItemExtension
  , Control_CRC;


var
  // Gestor de datos
  FGestorDatos   : IConectorDatos = nil;

  // Lista de iconos
  FImagenList    : TImageList     = nil;

  // Lista de iconos
  FImagenList32  : TImageList     = nil;


// Lista de listas de cadenas de texto
  cacheExtensiones :  TFPHashList = nil;

// DEfinición de la clase que se guarda en la lista de cadenas de texto y los index de los iconos
type
  TCacheDataExtension = class
    Descripcion : String;
    ImageIndex  : longint;
  end;

// Inicializa el sistema que devuelve la descripción e icono index de las extensiones
procedure SetExtensionesConfig(GestorDatos: IConectorDatos; ImagenList : TImageList; ImagenList32 : TImageList);
begin
  FGestorDatos  := GestorDatos;
  FImagenList   := ImagenList;
  FImagenList32 := ImagenList32;
end;

// Recupera los datos de la extensión
procedure GetExtensionById(IdExtension: Qword; UID  : string);
var
  Extension : TItemExtension;
  Datos     : TCacheDataExtension;
begin
  if FGestorDatos = nil then exit;

  // Recupera los datos de la extensión
  Extension := FGestorDatos.GetExtensionById(IdExtension);
  if Extension = nil then exit;


  // Crear el item de cache para su guardado
  Datos := TCacheDataExtension.create;
  Datos.Descripcion := Extension.Descripcion;


  if FImagenList <> nil then
  begin
    Datos.ImageIndex := FImagenList.Add(Extension.Icono, nil);
  end;


  // Guarda el item en la lista de cache
  cacheExtensiones.add(UID, Datos);

  // Libera la extensión
  Extension.Free;
end;

// Devuelve la descripción de la extensión
function GetExtensionDescripcionById(IdExtension: Qword): RawByteString;
var
  UID  : string;
  datoCache : TCacheDataExtension;
begin
  // Inicializa el resultado
  result := '';

  // Genera el UID de la extensión
  UID := 'ext.' + Get_CRC64ToString(inttostr(IdExtension));

  // Busca la extensión en la lista de cache
  datoCache := TCacheDataExtension(cacheExtensiones.Find(UID));
  if datoCache = nil then
  begin
    // Si no está en la lista de cache, la recupera de la base de datos
    GetExtensionById(IdExtension, UID);

    // Vuelve a buscar la extensión en la lista de cache
    datoCache := TCacheDataExtension(cacheExtensiones.Find(UID));
  end;

  // Si la extensión está en la lista de cache, devuelve la descripción
  if datoCache <> nil then
    result := datoCache.Descripcion;
end;

// Devuelve el Icon Index de la extensión
function GetExtensionIcopnIndexById(IdExtension: Qword; PorDefecto : longint): longint;
var
  UID  : string;
  datoCache : TCacheDataExtension;
begin
  // Inicializa el resultado
  result := PorDefecto;

  // Genera el UID de la extensión
  UID := 'ext.' + Get_CRC64ToString(inttostr(IdExtension));

  // Busca la extensión en la lista de cache
  datoCache := TCacheDataExtension(cacheExtensiones.Find(UID));
  if datoCache = nil then
  begin
    // Si no está en la lista de cache, la recupera de la base de datos
    GetExtensionById(IdExtension, UID);

    // Vuelve a buscar la extensión en la lista de cache
    datoCache := TCacheDataExtension(cacheExtensiones.Find(UID));
  end;

  // Si la extensión está en la lista de cache, devuelve el icon index
  if datoCache <> nil then
    result := datoCache.ImageIndex;
end;



// Limpia la cache de extensiones
procedure ClearCacheExtensiones;
var
  i : integer;
begin
  for i := 0 to cacheExtensiones.count - 1 do
    TCacheDataExtension(cacheExtensiones.Items[i]).Free;
  cacheExtensiones.clear;
end;





initialization
  cacheExtensiones := TFPHashList.create;




finalization
  // Limpia la cache de extensiones
  ClearCacheExtensiones;

  // Libera la lista de cache
  cacheExtensiones.free;

end.
