(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-03-23 16:15:28
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-06 13:20:07
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
unit Control_Imagen_Manipulacion_Mini;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Graphics
  , IntfGraphics
  , GraphType
  , FPimage
  ;

type
TRGBA_32Bits = record
  Blue:  Byte;
  Green: Byte;
  Red:   Byte;
  Alpha: Byte;
end;
PRGBA_32Bits = ^TRGBA_32Bits;

PRGBA_32BitsArray = ^TRGBA_32BitsArray;
TRGBA_32BitsArray = array[word] of TRGBA_32Bits;

TRGBA_24Bits = record
  Blue:  Byte;
  Green: Byte;
  Red:   Byte;
end;
PRGBA_24Bits = ^TRGBA_32Bits;

PRGBA_24BitsArray = ^TRGBA_24BitsArray;
TRGBA_24BitsArray = array[word] of TRGBA_24Bits;


// Crea un TLazIntfImage con el tamaño y color de fondo indicado
function Control_Imagen_Crear_LazIntfImage(Width, Height : LongInt; ColorFondo : TFPColor; FillColor : Boolean = false; BitsPerPixel : longint = 32) : TLazIntfImage;

// Convierte una imagen de 24 bits a 32 bits
procedure Control_Imagen_Convertir_RGBA(var srcbit: TLazIntfImage);

// Aplica transparencia a una imagen
procedure Control_Imagen_Transparencia(const srcbit: TLazIntfImage; Transparencia : byte; Forzar : boolean);


var
  FColorTransparentePNG            : TFPColor     = (red   : 65535; green : 0; blue  : 65535; alpha : 0);          // Color fucsia


implementation

// Crea un TLazIntfImage con el tamaño y color de fondo indicado
function Control_Imagen_Crear_LazIntfImage(Width, Height : LongInt; ColorFondo : TFPColor; FillColor : Boolean = false; BitsPerPixel : longint = 32) : TLazIntfImage;
begin
  // Dependiendo de la cantidad de bits por pixel, se crea un TLazIntfImage con o sin canal alfa
  if BitsPerPixel = 32 then
    Result             := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha, riqfUpdate])
  else
    Result             := TLazIntfImage.Create(0, 0, [riqfRGB, riqfUpdate]);

  // Se establece el tamaño de la imagen
  Result.Width  := Width;
  Result.Height := Height;

  // Se establece el color de fondo
  if FillColor then
    Result.FillPixels(ColorFondo);
end;

// Convierte una imagen de 24 bits a 32 bits
procedure Control_Imagen_Convertir_RGBA(var srcbit: TLazIntfImage);
var
  desbit             : TLazIntfImage;
  Maximo_X, Maximo_Y : integer;
  MX, MY             : integer;

  procedure ProcesarLinea(Index: PtrInt);
  var
    RawLine_Src        : PRGBA_24BitsArray;
    RawLine_Dest       : PRGBA_32BitsArray;
    x, y               : integer;
    newx, newy         : integer;

  begin
    y    := Index;
    newy := y;

    if (newy < 0) or (newy > MY) then
    exit;

    RawLine_Src        := srcbit.GetDataLineStart(y);
    RawLine_Dest       := desbit.GetDataLineStart(newy);

    for x := 0 to Maximo_X do
    begin
      newx := x;

      if (newx < 0) or (newx > MX) then
        continue;

      RawLine_Dest^[newx].Blue  := RawLine_Src^[x].Blue;
      RawLine_Dest^[newx].Green := RawLine_Src^[x].Green;
      RawLine_Dest^[newx].Red   := RawLine_Src^[x].Red;
      RawLine_Dest^[newx].Alpha := 255;
    end;
  end;


  procedure Convertir_24_2_32;
  var
    z                  : integer;
  begin
    Maximo_X := srcbit.Width - 1;
    Maximo_Y := srcbit.Height - 1;
    for z := 0 to Maximo_Y  do
    begin
      ProcesarLinea(z);
    end;
  end;

begin
  Maximo_X := srcbit.Width - 1;
  Maximo_Y := srcbit.Height - 1;
  MX       := srcbit.Width - 1;
  MY       := srcbit.Height - 1;

  case srcbit.DataDescription.BitsPerPixel of
    24 : begin
          desbit := Control_Imagen_Crear_LazIntfImage(srcbit.Width, srcbit.Height, FColorTransparentePNG, true);
          Convertir_24_2_32;
          srcbit.Free;
          srcbit := desbit;
        end;
  end;
end;

// Aplica transparencia a una imagen
procedure Control_Imagen_Transparencia(const srcbit: TLazIntfImage; Transparencia : byte; Forzar : boolean);
var
  Maximo_X, Maximo_Y : integer;
  NivelTransparencia : integer;
  z                  : integer;

  procedure ProcesarLinea(Index: PtrInt);
  var
    x, y               : integer;
    RawLine_Src        : PRGBA_32BitsArray;
  begin
    y    := Index;
    RawLine_Src := srcbit.GetDataLineStart(y);
    for x := 0 to Maximo_X do
      if not Forzar then
      begin
        if RawLine_Src^[x].Alpha >= Transparencia then
          RawLine_Src^[x].Alpha := byte(NivelTransparencia)
      end
      else
        RawLine_Src^[x].Alpha := byte(NivelTransparencia);
  end;

begin
  Maximo_X           := srcbit.Width -1;
  Maximo_Y           := srcbit.Height -1;
  if Transparencia > 0 then
    NivelTransparencia := longint((255 * Transparencia) div 100)
  else
    NivelTransparencia := 0;

  for z := 0 to Maximo_Y  do
    begin
    ProcesarLinea(z);
    end;
end;

end.

