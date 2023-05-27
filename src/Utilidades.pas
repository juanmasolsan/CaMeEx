(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-08 16:21:30
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 01:31:04
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
{$IFDEF WINDOWS}
  Windows,
  ShellApi,
{$ENDIF}
  Classes,
  SysUtils
  , graphics
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

// Devuelve el tiempo transcurrido desde el inicio
function MostrarTiempoTranscurrido(Inicio : TDateTime; MarcaTiempo : string = 'hh:mm:ss') : string;

// Devuelve el tiempo transcurrido desde el inicio hasta el final
function MostrarTiempoTranscurrido(Inicio : TDateTime; Final : TDateTime; MarcaTiempo : string = 'hh:mm:ss') : string;



// Devuelve el tipo de archivo/directorio
function GetGenericFileType(AExtension: RawByteString; IsDir : boolean = false): RawByteString;

// Devuelve el icono del archivo/directorio, además del tipo de archivo/directorio
function GetGenericFileIcon(AExtension: RawByteString; var InfoExtension : RawByteString; Size: longint;  IsDir : boolean = false): TPortableNetworkGraphic;

// Dibuja un rectangulo mezcando color por el nivel de BlendLevel
procedure Dibujar_FillRect_Blend(Acanvas : TCanvas; ARect: TRect; Color: TColor; BlendLevel : byte; RoundX, RoundY : longint);

// Dibuja rectangulo de selección con un toque moderno
procedure Dibujar_Seleccion_Moderna_Blend_V2(Acanvas : TCanvas; ARect: TRect; Color: TColor; Color_Base : TColor; BlendLevel : byte; Nivel : byte = 25; Extra_Borde : longint = 0);

// Dibuja un rectangulo con gradiante haciendo blending entre los colores
procedure Dibujar_GradientFill_Blend(Acanvas : TCanvas; ARect: TRect; ColorInicio, ColorFinal: TColor; BlendLevel : byte; ADirection: TGradientDirection);

// Calcula el color mezclado
function Blend(Color1, Color2: TColor; A: Byte): TColor; inline;

// Aplica transparencia a una imagen
procedure CrearTransparencia(Imagen : TFPImageBitmap; BlendLevel : byte; Forzar : boolean);

var
  // Color que se usa de base para hacer degradados
  Dibujar_Color_Degradado_Base : TColor = clWindow;



implementation

uses
  StrUtils
  , intfgraphics
  , Control_Imagen_Manipulacion_Mini
  , Control_Logger, AppString;

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

// Devuelve el tiempo transcurrido desde el inicio
function MostrarTiempoTranscurrido(Inicio : TDateTime; MarcaTiempo : string = 'hh:mm:ss') : string;
var
  TiempoTranscurrido : TDateTime;
begin
  TiempoTranscurrido := Now - Inicio;
  Result := FormatDateTime(MarcaTiempo, TiempoTranscurrido);
end;

// Devuelve el tiempo transcurrido desde el inicio hasta el final
function MostrarTiempoTranscurrido(Inicio : TDateTime; Final : TDateTime; MarcaTiempo : string = 'hh:mm:ss') : string;
var
  TiempoTranscurrido : TDateTime;
begin
  TiempoTranscurrido := Final - Inicio;
  Result := FormatDateTime(MarcaTiempo, TiempoTranscurrido);
end;





// Devuelve el tipo de archivo/directorio
function GetGenericFileType(AExtension: RawByteString; IsDir : boolean = false): RawByteString;
{$IFDEF WINDOWS}
var
  AInfo: SHFileInfoW;
  attr : Dword;
begin
  if IsDir then
    attr := FILE_ATTRIBUTE_DIRECTORY
  else
    attr := FILE_ATTRIBUTE_NORMAL;

  fillchar(AInfo, sizeof(AInfo), 0);

  if SHGetFileInfo(PWideChar(UTF8Decode(AExtension)), attr, AInfo, SizeOf(AInfo), SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0 then
    Result :=  AInfo.szTypeName
  else
    Result := '';
end;
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

// Devuelve el icono del archivo/directorio, además del tipo de archivo/directorio
function GetGenericFileIcon(AExtension: RawByteString; var InfoExtension : RawByteString; Size: longint; IsDir : boolean = false): TPortableNetworkGraphic;
{$IFDEF WINDOWS}
var
  AInfo: SHFileInfoW;
  AIcon: TIcon;
  attr : Dword;
  Png  : TPortableNetworkGraphic;
  Inter: TLazIntfImage;

  TipoIcono : longint;
begin
  Result := nil;

  if Size = 16 then
    TipoIcono := SHGFI_SMALLICON
  else
    TipoIcono := SHGFI_LARGEICON;

  if IsDir then
    attr := FILE_ATTRIBUTE_DIRECTORY
  else
    attr := FILE_ATTRIBUTE_NORMAL;

  fillchar(AInfo, sizeof(AInfo), 0);

  if SHGetFileInfo(PWideChar(UTF8Decode(AExtension)), attr, AInfo, SizeOf(SHFileInfoW), SHGFI_ICON or TipoIcono or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0 then
  begin
    InfoExtension := AInfo.szTypeName;

    if AInfo.hIcon <> 0 then
    begin
      AIcon := TIcon.Create;
      try
        try
          AIcon.Handle := AInfo.hIcon;
          Inter := AIcon.CreateIntfImage;
          try
            Png := TPortableNetworkGraphic.Create;
            Png.LoadFromIntfImage(Inter);
            Result := Png;
          finally
            Inter.Free;
          end;
        except
            on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
        end;
      finally
        AIcon.Free;
      end;
    end;
  end;
end;
{$ELSE}
begin
  InfoExtension := '';
  Result        := nil;
end;
{$ENDIF}


// Genera un PNG de 32bits
function Internal_CrearBitmap32(ARect: TRect) : TPortableNetworkGraphic;
var
  Temporal : TLazIntfImage;
begin
  try
    // Crea un bitmap de 32bits
    Temporal := Control_Imagen_Crear_LazIntfImage(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, FColorTransparentePNG, true);
    try
      // Carga el bitmap en la imagen
      Result := TPortableNetworkGraphic.Create;

      // Carga el bitmap en la imagen
      Result.LoadFromIntfImage(Temporal);
    finally
      // Libera el bitmap
      Temporal.Free;
    end;
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

// Aplica transparencia a un PNG de 32bits
procedure Internal_CrearTransparencia(Imagen : TFPImageBitmap; BlendLevel : byte; Forzar : boolean);
var
  Escribir   : TLazIntfImage;
begin
  Escribir := Imagen.CreateIntfImage;
  try
    // Convierte el bitmap a RGBA
    Control_Imagen_Convertir_RGBA(Escribir);

    // Aplica la transparencia
    Control_Imagen_Transparencia(Escribir, BlendLevel, Forzar);
    try
      // Carga en la imagen los cambios aplicados
      Imagen.LoadFromIntfImage(Escribir);
    except
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
  finally
    Escribir.Free;
  end;
end;


// Dibuja un rectangulo mezcando color por el nivel de BlendLevel sobre un PNG el cual devuelve
function Internal_Dibujar_Recta_Blend(ARect: TRect; Color: TColor; BlendLevel : byte; RoundX, RoundY : longint) : TPortableNetworkGraphic;
var
  NuevaRecta : TRect;
begin
  // Crea un bitmap de 32bits
  Result                    := Internal_CrearBitmap32(ARect);

  // Crea un rectangulo con las dimensiones del bitmap
  NuevaRecta                := Rect(0,0, Result.Width, Result.Height);

  // Añade el color al rectangulo
  Result.Canvas.Brush.Color := Color;

  // Dibuja el rectangulo dependiendo si hay que redondear las esquinas o no
  if (RoundX > 0) or  (RoundY > 0) then
    Result.Canvas.RoundRect(NuevaRecta, RoundX, RoundY)
  else
    Result.Canvas.FillRect(NuevaRecta);

  // Aplica la transparencia
  Internal_CrearTransparencia(Result, BlendLevel, true);
end;


// Dibuja un rectangulo mezcando color por el nivel de BlendLevel
procedure Dibujar_FillRect_Blend(Acanvas : TCanvas; ARect: TRect; Color: TColor; BlendLevel : byte; RoundX, RoundY : longint);
var
  Temporal : TPortableNetworkGraphic;
begin
  // Dibuja el rectangulo
  Temporal := Internal_Dibujar_Recta_Blend(ARect, Color, BlendLevel, RoundX, RoundY);
  try
    // Dibuja el rectangulo sobre el canvas final
    Acanvas.Draw(ARect.Left, ARect.Top, Temporal);
  finally
    Temporal.Free;
  end;
end;

function TrueInflateRect(const R: TRect; const I: Integer): TRect; inline;
begin
  Result := R;
  with Result do
    begin
      Left   := Left - I;
      Top    := Top - I;
      Right  := Right + I;
      Bottom := Bottom + I;
    end;
end;

// Calcula el color mezclado
function Blend(Color1, Color2: TColor; A: Byte): TColor; inline;
var
  c1, c2: LongInt;
  r , g,  b, v1, v2: longint;
begin
  A      := Round(2.55 * A);
  c1     := ColorToRGB(Color1);
  c2     := ColorToRGB(Color2);
  v1     := Byte(c1);
  v2     := Byte(c2);
  r      := Longint(A * (v1 - v2) shr 8 + v2);
  v1     := Byte(c1 shr 8);
  v2     := Byte(c2 shr 8);
  g      := Longint(A * (v1 - v2) shr 8 + v2);
  v1     := Byte(c1 shr 16);
  v2     := Byte(c2 shr 16);
  b      := Longint(A * (v1 - v2) shr 8 + v2);
  Result := (b shl 16) + (g shl 8) + r;
end;


function Internal_Dibujar_GradientFill_Blend(ARect: TRect; ColorInicio, ColorFinal: TColor; BlendLevel : byte; ADirection: TGradientDirection) : TPortableNetworkGraphic;
var
  NuevaRecta : TRect;
begin
  Result     := Internal_CrearBitmap32(ARect);
  NuevaRecta := Rect(0,0, Result.Width, Result.Height);
  Result.Canvas.GradientFill(NuevaRecta, ColorInicio, ColorFinal, ADirection);
  if BlendLevel <> 255 then
    Internal_CrearTransparencia(Result, BlendLevel, true);
end;

// Dibuja un rectangulo con gradiante haciendo blending entre los colores
procedure Dibujar_GradientFill_Blend(Acanvas : TCanvas; ARect: TRect; ColorInicio, ColorFinal: TColor; BlendLevel : byte; ADirection: TGradientDirection);
var
  Temporal : TPortableNetworkGraphic;
begin
  Temporal := Internal_Dibujar_GradientFill_Blend(ARect, ColorInicio, ColorFinal, BlendLevel, ADirection);
  try
    Acanvas.Draw(ARect.Left, ARect.Top, Temporal);
  finally
    Temporal.Free;
  end;
end;

// Dibuja rectangulo de selección con un toque moderno
procedure Dibujar_Seleccion_Moderna_Blend_V2(Acanvas : TCanvas; ARect: TRect; Color: TColor; Color_Base : TColor; BlendLevel : byte; Nivel : byte = 25; Extra_Borde : longint = 0);
var
  Minimo     : longint;
begin
  Minimo := Nivel;

  if Nivel <> 255 then
    Minimo := Nivel - 15;

  if Minimo < 0 then
    Minimo := 0;

  // El fondo
  Dibujar_GradientFill_Blend(Acanvas, ARect, Blend(color, Color_Base, Minimo), Blend(color, Color_Base, Nivel), BlendLevel, gdVertical);

  if Extra_Borde > 100 then exit;
  // El borde
  Acanvas.Brush.Style := bsClear;
  Acanvas.Pen.Color   := Blend(color, Color_Base, 50);
  Acanvas.RoundRect(ARect, 2, 2);

  // El Interior
  Acanvas.Pen.Color   := Blend(color, Color_Base, 10);
  ARect               := TrueInflateRect(ARect, Extra_Borde);
  Acanvas.RoundRect(ARect, 2, 2);
end;

procedure CrearTransparencia(Imagen : TFPImageBitmap; BlendLevel : byte; Forzar : boolean);
begin
 Internal_CrearTransparencia(Imagen, BlendLevel, Forzar );
end;



end.

