(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-10 19:52:21
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-10 22:47:31
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
unit Control_Contine;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , strutils
  ;

// Comprueba si dentro de la cadena de texto existe la cadena de Contiene
function IsContiene(const Texto : string; const Contiene : string) : boolean;

// Comprueba si dentro de la cadena de texto existe la cadena de Contiene
function IsContiene(const Texto : string; const Contiene : TStringList) : boolean;

implementation

uses
  Contnrs
  ;

// Lista de listas de cadenas de texto
var
  cacheListas :  TFPHashList = nil;

// Obtiene la lista de cadenas de texto de la cadena de texto
function CacheGetLista(const Contiene : string) : Pointer;
begin
  result := cacheListas.Find(Contiene);
end;

// Añade la lista de cadenas de texto de la cadena de texto a lista de cache
procedure CacheAddLista(const Contiene : string; Lista : TStringList);
begin
  cacheListas. add(Contiene, Lista);
end;

// Limpia la lista de cache
procedure CacheClear();
var
  total, t : longint;
begin
  total := cacheListas.Count -1;
  for t := 0 to total do
    TStringList(cacheListas.Items[t]).free;

  cacheListas.Clear();
end;


// Comprueba si dentro de la cadena de texto existe la cadena de Contiene
function IsContiene(const Texto : string; const Contiene : TStringList) : boolean;
var
  posicion       : longint = 1;

// Comprueba si dentro de la cadena de texto existe la cadena de buscar
  function Localizada(buscar : string) : boolean;
  begin
    posicion := PosEx(buscar, Texto, posicion);
    result   := posicion > 0;
    if result then
      posicion := posicion + length(buscar);
  end;

var
  total, t     : longint;
  Actual       : string;
  IsLocalizado : boolean = false;
  AvanceProximo: boolean = false;
begin
    total := Contiene.count -1;
    for t := 0 to total do
    begin
      Actual := Contiene[t];
      if Actual = '' then
      begin
        if  IsLocalizado then
        begin
          result := IsLocalizado;
          exit;
        end;
        posicion      := 1;
        IsLocalizado  := false;
        AvanceProximo := false;
        continue;
      end;

      if AvanceProximo then
        continue;

      if Actual = '*' then
      begin
        inc(posicion);
        continue;
      end;

      IsLocalizado := Localizada(Actual);
      if not IsLocalizado then
        AvanceProximo := true;
  end;

  result := IsLocalizado;
end;

// Comprueba si dentro de la cadena de texto existe la cadena de Contiene
function IsContiene(const Texto : string; const Contiene : string) : boolean;
var
  Listado : TStringList;
begin
  // Intenta recuperar la lista de cadenas de texto de la cache
  Listado := TStringList(CacheGetLista(Contiene));
  if Listado <> nil then
  begin
    // Si la lista de cadenas de texto existe en la cache, la usa
    result := IsContiene(Texto, Listado);
    exit;
  end
  else
  begin
    // Si la lista de cadenas de texto no existe en la cache, la crea y la añade a la cache
    Listado      := TStringList.create;
    Listado.text := stringReplace(Contiene, '*', #13 + '*'+#13, [rfReplaceAll, rfIgnoreCase]);
    CacheAddLista(Contiene, Listado);
    Result       := IsContiene(Texto, Listado);
  end;
end;


initialization
  // Inicializa la lista de cache
  cacheListas := TFPHashList.create;

finalization
  // Limpia la lista de cache
  CacheClear();

  // Libera la lista de cache
  cacheListas.free;

end.
