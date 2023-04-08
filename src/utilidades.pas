(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-08 16:21:30
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-08 18:13:03
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

unit Utilidades;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
  ;

// Convierte los atributos de un archivo en una cadena de caracteres
function AtributosToStr(atributos : Dword; Mayusculas : Boolean = false): String;

// Agrega espacios al inicio de una cadena hasta alcanzar la longitud máxima
function AgregarEspaciosAlInicio(const Cadena : String; LongitudMaxima : Integer) : String;

// Agrega espacios al final de una cadena hasta alcanzar la longitud máxima
function AgregarEspaciosAlFinal(const Cadena : String; LongitudMaxima : Integer) : String;

// Puntea un número con sus separadores de miles
function PuntearNumeracion(Entrada : int64; Puntear : Boolean = true): String;

// Convierte una fecha en una cadena de caracteres
function FechaToStr(fecha : TDateTime): String;

// Convierte un tamaño en bytes en una cadena de caracteres leible por humanos
function ConvertirSizeEx(Entrada : int64; Decimal1 : string = ',##'; Decimal2 : string = '.00' ): String;


implementation

uses
  StrUtils
  ;

// Convierte los atributos de un archivo en una cadena de caracteres
function AtributosToStr(atributos : Dword; Mayusculas : Boolean = false): String;
begin
  result := '';

  {$WARNINGS-}
  if Boolean(atributos and faSymLink)   then result := result + 'l';
  if Boolean(atributos and faDirectory) then result := result + 'd';
  if not Boolean(atributos and faDirectory) and Boolean(atributos and faArchive)   then result := result + 'a';
  if Boolean(atributos and faReadOnly)  then result := result + 'r';
  if Boolean(atributos and faHidden)    then result := result + 'h';
  if Boolean(atributos and faSysFile)   then result := result + 's';
  if Boolean(atributos and faVolumeId)  then result := result + 'v';
  {$WARNINGS+}

  if Mayusculas then
    Result := UpperCase(Result);
end;

// Agrega espacios al inicio de una cadena hasta alcanzar la longitud máxima
function AgregarEspaciosAlInicio(const Cadena : String; LongitudMaxima : Integer) : String;
begin
  Result := AddChar(' ', Cadena, LongitudMaxima);
end;

// Agrega espacios al final de una cadena hasta alcanzar la longitud máxima
function AgregarEspaciosAlFinal(const Cadena : String; LongitudMaxima : Integer) : String;
begin
  Result := AddCharR(' ', Cadena, LongitudMaxima);
end;

// Puntea un número con sus separadores de miles
function PuntearNumeracion(Entrada : int64; Puntear : Boolean = true): String;
const
  BYTE_PUNTO = char(ord('.'));

var
  i, t    : longint;
  total   : longint;
  Maximo  : longint;
  cuanto  : longint;
  Actual  : longint;
  PEntrada: Pchar;
  PSalida : Pchar;

begin
  Result := inttostr(Entrada);

  if not Puntear then
    exit;

  total    := length(Result);
  PEntrada := @Result[Total];

  Maximo  := 0;
  if Entrada < 0 then
    dec(Maximo);

  cuanto := ((total - 1 + Maximo) div 3);
  Maximo := total + cuanto;

  Setlength(Result, Maximo);

  PSalida  := @Result[Maximo];

  Actual := 0;
  i := 0;
  t := 1;
  repeat
    PSalida^ := PEntrada^;
    dec(PSalida);
    dec(PEntrada);
    if Actual < cuanto then
    begin
      if i = 2 then
      begin
        PSalida^ := BYTE_PUNTO;
        dec(PSalida);
        i := 0;
        inc(Actual);
      end
      else
        inc(i);
    end;
    inc(t);
  until (t > Maximo);
end;

// Convierte una fecha en una cadena de caracteres
function FechaToStr(fecha : TDateTime): String;
const
  FORMATO_FECHA_HORA = 'dd/mm/yyyy  hh:mm:ss';
begin
  Result := FormatDateTime(FORMATO_FECHA_HORA, fecha);
end;

// Convierte un tamaño en bytes en una cadena de caracteres leible por humanos
function ConvertirSizeEx(Entrada : int64; Decimal1 : string = ',##'; Decimal2 : string = '.00' ): String;
const
  CKiloByte = 1024;
  CMegaByte = CKiloByte * 1024;
  CGygaByte = CMegaByte * 1024;
  CTeraByte = CGygaByte * 1024;

var
  Mirar : string;
begin
  Mirar := '#'+Decimal1+'0'+Decimal2;

  if Entrada < CKiloByte then
    Result := FormatFloat(Mirar+' B',Entrada)
  else
    if Entrada < CMegaByte then
    Result := FormatFloat(Mirar+' KB', Entrada / CKiloByte)
    else
    if Entrada < CGygaByte then
      Result := FormatFloat(Mirar+' MB', Entrada / CMegaByte)
    else
      if Entrada < CTeraByte then
      Result := FormatFloat(Mirar+' GB', Entrada / CGygaByte)
      else
      Result := FormatFloat(Mirar+' TB', Entrada / CTeraByte)
end;




end.

