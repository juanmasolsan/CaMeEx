(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-04 23:56:10
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-05 00:31:27
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

unit OrdenarLista;

{$mode ObjFPC}{$H+}

interface

uses
  ItemDato
  ;


// Devuelve -1 si Item1 es menor que Item2, 0 si son iguales y 1 si Item1 es mayor que Item2
function ListSortFuncDos(const Item1, Item2: TItemDato; Columna : integer; Orden : longint): Integer;


implementation

uses
  SysUtils
  , math
  , ItemBaseDatos;

type
  TOrdenarPor      = (opNombre, opSize, opExtension, opFecha, opAtributos, opRuta);
  TOrdenOrdenarPor = (oopAscendente, oopDescencente);

// Devuelve -1 si A es menor que A, 0 si son iguales y 1 si A es mayor que A
function MiniInt(A : integer) : integer;
begin
  Result := 0;
  if A > 0 then
    Result := 1
  else
    if A < 0 then
      Result := -1;
end;

// Devuelve -1 si A es menor que A, 0 si son iguales y 1 si A es mayor que A
function Comparar_Texto(A, B : String) : integer;
begin
  Result := MiniInt(CompareStr(A, B));
end;

// Devuelve 0, -1 o 1 dependiendo de la comparación de las extensiones
function OrdenarPorExtensionDos(Orden : Integer; Ext1, Ext2 : String; D1, D2 : Boolean): Integer; {$IFDEF FORCE_INLINE} inline; {$ENDIF}
var
 T2    : Integer;

 function AclararPosicion : integer;
 begin
  Result := Orden;
  if (orden = -1) and (T2 = -1) then Result := -1;
  if (orden = -1) and (T2 =  0) then Result := -1;
  if (orden = -1) and (T2 =  1) then Result :=  1;
  if (orden =  0) and (T2 = -1) then Result := -1;
  if (orden =  0) and (T2 =  0) then Result :=  0;
  if (orden =  0) and (T2 =  1) then Result :=  1;
  if (orden =  1) and (T2 = -1) then Result := -1;
  if (orden =  1) and (T2 =  0) then Result :=  1;
  if (orden =  1) and (T2 =  1) then Result :=  1;
 end;

begin
 if (D1 = true) and (D2 = true) then
  begin
    Result := Orden;
    exit;
  end;

  T2 := Comparar_Texto(ext1, ext2);
  Result := AclararPosicion;
end;

// Emula el resultado de ordenación de Windows Explorer
function OrdenarListaComoExplorer(Nombre1, Nombre2, Ext1, Ext2,
 Ruta1, Ruta2: string; Atributos1, Atributos2 : longint;
 Fecha1, Fecha2 : TDateTime; Dir1, Dir2 : Boolean; Size1, Size2 : int64; OrdenarPor : TOrdenarPor; TipoOrden  : TOrdenOrdenarPor) : integer; {$IFDEF FORCE_INLINE} inline; {$ENDIF}
begin
 Result    := 0;
 case OrdenarPor of
  opNombre        : Result := Comparar_Texto(Nombre1, Nombre2);               // Ordena Por Nombre
  opSize          : Result := MiniInt(CompareValue(Size1, Size2));            // Ordena Por Tamaño
  opFecha         : Result := MiniInt(CompareValue(Fecha1, Fecha2));          // Ordena Por Fecha
  opAtributos     : Result := MiniInt(CompareValue(Atributos1, Atributos2));  // Ordena Por Atributos
  opRuta          : Result := Comparar_Texto(Nombre1, Nombre2);               // Ordena Por Nombre
 end;
end;


// Emula el resultado de ordenación del antiguo Winfile
function OrdenarListaComoWinfile(Nombre1, Nombre2, Ext1, Ext2,
  Ruta1, Ruta2: String; Atributos1, Atributos2 : longint;
  Fecha1, Fecha2 : TDateTime; Dir1, Dir2 : Boolean; Size1, Size2 : int64; OrdenarPor : TOrdenarPor; TipoOrden  : TOrdenOrdenarPor) : integer; {$IFDEF FORCE_INLINE} inline; {$ENDIF}
var
  NombreDiff : Integer;

begin
  result := 0;
  if Dir1 and not Dir2 then
    begin
      Result := 0;
      exit;
    end
    else
    if not Dir1 and Dir2 then
    begin
      Result := -1;
      if (TipoOrden = oopAscendente) then
        Result := 1;
      exit;
    end
    else
    if Dir1 and Dir2 then
    begin
      Result := Comparar_Texto(Nombre1, Nombre2);
      if (OrdenarPor = opFecha) then
        Result := MiniInt(CompareValue(Fecha1, Fecha2))
      else
        if (OrdenarPor = opAtributos) then
          Result := MiniInt(CompareValue(Atributos1, Atributos2));
      exit;
    end;

  if Dir1 then
    Ext1 := ' ';

  if Dir2 then
    Ext2 := ' ';

  case OrdenarPor of
    opExtension  : begin  // Ordena por Extension
                    NombreDiff := OrdenarListaComoWinfile(Nombre1, Nombre2, Ext1, Ext2, Ruta1, Ruta2, Atributos1, Atributos2, Fecha1, Fecha2, Dir1, Dir2, Size1, Size2, opNombre, oopAscendente);
                    Result     := OrdenarPorExtensionDos(NombreDiff, Ext1, Ext2, Dir1, Dir2);
                    if Dir1 and Dir2 then
                      Result := Comparar_Texto(Nombre1, Nombre2)
                  end;
  else
    result := OrdenarListaComoExplorer(Nombre1, Nombre2, Ext1, Ext2, Ruta1, Ruta2, Atributos1, Atributos2, Fecha1, Fecha2, Dir1, Dir2, Size1, Size2, OrdenarPor, TipoOrden);
  end;

  // Si Son Iguales los  ordena por el nombre
  if (result = 0)  then
    Result := Comparar_Texto(Nombre1, Nombre2);
end;


// Devuelve -1 si Item1 es menor que Item2, 0 si son iguales y 1 si Item1 es mayor que Item2
function ListSortFuncDos(const Item1, Item2: TItemDato; Columna : integer; Orden : longint): Integer;
begin
  Result := 0;
  if Item1 = nil then exit;
  if Item2 = nil then exit;

  try
    Result := OrdenarListaComoWinfile(lowercase(Item1.Nombre), lowercase(Item2.Nombre),
                      lowercase(ExtractFileExt(Item1.Nombre)),  lowercase(ExtractFileExt(Item2.Nombre)),
                      '', '',
                      Item1.Atributos,                       Item2.Atributos,
                      Item1.Fecha,                           Item2.Fecha,
                      Item1.Tipo = TItemDatoTipo.Directorio, Item2.Tipo = TItemDatoTipo.Directorio,
                      Item1.Size,                            Item2.Size,
                      TOrdenarPor(Columna),
                      TOrdenOrdenarPor(Orden)
                      );
  except
    Result := 0;
  end;
end;


end.

