(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-11 16:39:28
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 23:53:23
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

unit Control_Logger;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf
  , LCLType
  , Classes
  , SysUtils
  ;


type
  { TLogLevel }
	TLogLevel = (Off, Severe, Warning, Info, All);

  { TLogger }
  TLogger = class
  private
    FLogLevel                  : TLogLevel;
    FLogFileName               : RawByteString;
    FLogFile                   : TFileStream;
    FAppend                    : Boolean;
    FCriticalSection_Escritura : TCriticalSection;
    procedure SetLogLevel(AValue: TLogLevel);
  public
    // Constructor y destructor
    constructor Create(archivo: RawByteString; nivel: TLogLevel; Append: boolean = false);
    destructor Destroy; override;

    // Escribe un mensaje en el archivo de log
    procedure Log(ALevel: TLogLevel; AMessage: RawByteString);

    // Escribe una excepción en el archivo de log
    procedure LogException(AMessage: RawByteString; AException: Exception);

    // Establece el nivel de log
    property LogLevel   : TLogLevel read FLogLevel write SetLogLevel;
  end;

// Crea el objeto de log
procedure LogCreate(archivo: RawByteString; nivel: TLogLevel; Append: boolean = false);

// Loguea un mensaje en el archivo de log
procedure LogAdd(ALevel: TLogLevel; AMessage: RawByteString);

// Loguea una Excepcion en el archivo de log
procedure LogAddException(AMessage: RawByteString; AException: Exception);

// Establece el nivel de log
procedure LogSetLevel(ALevel: TLogLevel);


implementation

uses
  TypInfo
  ;


function TLogLevelToString(Value: TLogLevel): string;
begin
  Result := GetEnumName(typeInfo(TLogLevel), Ord(Value));
end;


var
  // Objeto de log
  Logger : TLogger = nil;


// Crea el objeto de log
procedure LogCreate(archivo: RawByteString; nivel: TLogLevel; Append: boolean = false);
begin
  if Logger = nil then
    Logger := TLogger.Create(archivo, nivel, Append);
end;

// Loguea un mensaje en el archivo de log
procedure LogAdd(ALevel: TLogLevel; AMessage: RawByteString);
begin
  if Logger <> nil then
    Logger.Log(ALevel, AMessage);
end;

// Establece el nivel de log
procedure LogSetLevel(ALevel: TLogLevel);
begin
  if Logger <> nil then
    Logger.LogLevel := ALevel;
end;

// Loguea una Excepcion en el archivo de log
procedure LogAddException(AMessage: RawByteString; AException: Exception);
begin
  if Logger <> nil then
    Logger.LogException(AMessage, AException);
end;

{ TLogger }
constructor TLogger.Create(archivo: RawByteString; nivel: TLogLevel; Append: boolean = false);
var
  modo: word = fmCreate;
begin
  // Llamada al constructor de la clase base
  inherited Create;

  // Inicializar el objeto de sincronización
  InitializeCriticalSection(FCriticalSection_Escritura);

  // Inicialización de las propiedades
  FLogLevel    := nivel;
  FLogFileName := archivo;
  FAppend      := Append;

  // Establece el modo de apertura del archivo de log
  if Append then
  begin
    if not fileExists(archivo) then
      modo := fmCreate OR fmOpenWrite
    else
      modo := fmOpenWrite;
  end;

  // Creación del archivo de log
  FLogFile := TFileStream.Create(archivo, modo OR fmShareDenyNone);
end;

destructor TLogger.Destroy;
begin
  // Cierre del archivo de log
  FLogFile.Free;

  // Eliminar el objeto de sincronización
  DeleteCriticalSection(FCriticalSection_Escritura);

  // Llamada al destructor de la clase base
  inherited Destroy;
end;

// Loguea un mensaje en el archivo de log
procedure TLogger.Log(ALevel: TLogLevel; AMessage: RawByteString);
begin
  if ALevel <= FLogLevel then
  begin
    // Protección de acceso concurrente
    EnterCriticalSection(FCriticalSection_Escritura);
    try
      // Posiciona el puntero de escritura al final del archivo
      FLogFile.Seek(0, soFromEnd);

      // Escribe el mensaje en el archivo de log
      AMessage := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' [' + TLogLevelToString(ALevel) + '] ' + AMessage + RawByteString(sLineBreak);
      FLogFile.Write(Pointer(AMessage)^, Length(AMessage));
    finally
      LeaveCriticalSection(FCriticalSection_Escritura);
    end;
  end;
end;

// Escribe una excepción en el archivo de log
procedure TLogger.LogException(AMessage: RawByteString; AException: Exception);
var
	texto  : RawByteString;

	i      : Integer;
	Frames: PPointer;
begin
    // Protección de acceso concurrente
    EnterCriticalSection(FCriticalSection_Escritura);
    try
      // Posiciona el puntero de escritura al final del archivo
      FLogFile.Seek(0, soFromEnd);

      // Escribe el mensaje en el archivo de log
      texto := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' [' + TLogLevelToString(TLogLevel.Severe) + '] ' + AMessage + RawByteString(sLineBreak);
      texto += '--------------------------------------------------------------------------------' + RawByteString(sLineBreak);

      if AException <> nil then
        texto += AException.ClassName+' - '+ AException.Message + RawByteString(sLineBreak);

      texto += BackTraceStrFunc(ExceptAddr) + RawByteString(sLineBreak);

      Frames := ExceptFrames;
      for i:= 0 to ExceptFrameCount - 1 do
        texto += BackTraceStrFunc(Frames[i]) + RawByteString(sLineBreak);

      texto += '--------------------------------------------------------------------------------' + RawByteString(sLineBreak);

      // Escribe el mensaje en el archivo de log
      FLogFile.Write(Pointer(texto)^, Length(texto));
    finally
      LeaveCriticalSection(FCriticalSection_Escritura);
    end;
end;

// Establece el nivel de log
procedure TLogger.SetLogLevel(AValue: TLogLevel);
begin
  if FLogLevel = AValue then Exit;
  FLogLevel := AValue;
end;


initialization

finalization
  if Logger <> nil then
  begin
    Logger.Free;
    Logger := nil;
  end;

end.
