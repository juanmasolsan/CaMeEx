(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-25 15:51:34
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-25 17:11:00
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

unit GestorExportacion;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  Controls
, classes
, ItemDato
;

type

  { TFormatoExportacion }
  TFormatoExportacion = (feNONE, feTXT, feHTML);

  { TGestorExportacion }

  { TGestorExportacionBase }
  TGestorExportacionBase = class
  private
    FNombreArchivo     : string;
    FArchivoSalida     : TStringList;
    FProgramaGenerador : string;
    FFormato           : TFormatoExportacion;
  protected

  public
    // Constructor
    constructor Create(const NombreArchivo: string; ProgramaGenerador : string; Formato : TFormatoExportacion); virtual;

    // Destructor
    destructor Destroy; override;

    // Añade el header del archivo
    procedure AddHeader; virtual;

    // Añade el footer del archivo
    procedure AddFooter; virtual;

    // Añade un Item al archivo
    procedure AddItem(const Item: TItemDato); virtual;

    // Guarda el archivo
    procedure Save(); virtual;
  end;

  { TGestorExportacion }
  TGestorExportacion = class(TGestorExportacionBase);


implementation

uses
  AppString;

{ TGestorExportacionBase }

// Constructor
constructor TGestorExportacionBase.Create(const NombreArchivo: string; ProgramaGenerador : string; Formato : TFormatoExportacion);
begin
  // Crea el archivo
  FArchivoSalida := TStringList.Create;

  // Asigna el nombre del archivo
  FNombreArchivo := NombreArchivo;

  // Asigna el programa generador
  FProgramaGenerador := ProgramaGenerador;

  // Asigna el formato
  FFormato := Formato;

  // Llama al constructor de la clase padre
  inherited Create;
end;

// Destructor
destructor TGestorExportacionBase.Destroy;
begin
  // Libera el archivo
  FArchivoSalida.Free;

  // Llama al destructor de la clase padre
  inherited Destroy;
end;

// Añade el header del archivo
procedure TGestorExportacionBase.AddHeader;
begin
  FArchivoSalida.Add('-----------------------------------------------------------');
  FArchivoSalida.Add(Message_Exportacion_Generado_Por + ' :_____PROGRAMA_____GENERADOR_____: (:_____FECHA_____:)');
  FArchivoSalida.Add('-----------------------------------------------------------');
  FArchivoSalida.Add('');
end;

// Añade el footer del archivo
procedure TGestorExportacionBase.AddFooter;
begin
  //
end;

// Añade un Item al archivo
procedure TGestorExportacionBase.AddItem(const Item: TItemDato);
begin
  //
  FArchivoSalida.Add(Item.Nombre);
end;

// Guarda el archivo
procedure TGestorExportacionBase.Save();
var
  Texto : string;
begin
  // Asigna el texto
  Texto := FArchivoSalida.text;

  // Añade las variables
  Texto := StringReplace(Texto, ':_____FECHA_____:', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), [rfReplaceAll, rfIgnoreCase]);
  Texto := StringReplace(Texto, ':_____PROGRAMA_____GENERADOR_____:', FProgramaGenerador, [rfReplaceAll, rfIgnoreCase]);

  // Asigna el texto
  FArchivoSalida.text := Texto;

  // Guarda el archivo
  FArchivoSalida.SaveToFile(FNombreArchivo);
end;






initialization





finalization

end.
