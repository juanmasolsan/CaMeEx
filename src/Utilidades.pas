(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-08 16:21:30
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-01 14:42:43
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
function GetGenericFileIcon(AExtension: RawByteString; var InfoExtension : RawByteString; IsDir : boolean = false): TPortableNetworkGraphic;


implementation

uses
  StrUtils
  , intfgraphics;

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
function GetGenericFileIcon(AExtension: RawByteString; var InfoExtension : RawByteString; IsDir : boolean = false): TPortableNetworkGraphic;
{$IFDEF WINDOWS}
var
  AInfo: SHFileInfoW;
  AIcon: TIcon;
  attr : Dword;
  Png  : TPortableNetworkGraphic;
  Inter: TLazIntfImage;
begin
  Result := nil;

  if IsDir then
    attr := FILE_ATTRIBUTE_DIRECTORY
  else
    attr := FILE_ATTRIBUTE_NORMAL;

  fillchar(AInfo, sizeof(AInfo), 0);

  if SHGetFileInfo(PWideChar(UTF8Decode(AExtension)), attr, AInfo, SizeOf(SHFileInfoW), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0 then
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



end.

