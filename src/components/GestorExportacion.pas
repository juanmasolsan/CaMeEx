(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-25 15:51:34
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-27 12:36:44
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
  , ItemBaseDatos
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

{ TGestorExportacionTxt }
  TGestorExportacionTxt = class(TGestorExportacionBase)
  private
    FTotalDirectorios : Qword;
    FTotalArchivos    : Qword;
    FTotalSize        : Qword;
  protected

  public
    // Añade el header del archivo
    procedure AddHeader; override;

    // Añade el footer del archivo
    procedure AddFooter; override;

    // Añade un Item al archivo
    procedure AddItem(const Item: TItemDato); override;
  end;

{ TGestorExportacionHtml }
  TGestorExportacionHtml = class(TGestorExportacionTxt)
  private
  protected
  public
    // Añade el header del archivo
    procedure AddHeader; override;

    // Añade el footer del archivo
    procedure AddFooter; override;

    // Añade un Item al archivo
    procedure AddItem(const Item: TItemDato); override;
  end;




  { TGestorExportacion }
  TGestorExportacion = class(TGestorExportacionHtml);


implementation

uses
  AppString, Utilidades, GestorExtensiones;

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


{ TGestorExportacionTxt }

// Añade el header del archivo
procedure TGestorExportacionTxt.AddHeader;
begin
  // Si no es TXT, llama al padre
  if FFormato <> feTXT then
  begin
    inherited AddHeader;
    exit;
  end;

  // Asigna los valores
  FTotalDirectorios := 0;
  FTotalArchivos    := 0;
  FTotalSize        := 0;

  FArchivoSalida.Add('-----------------------------------------------------------');
  FArchivoSalida.Add(Message_Exportacion_Generado_Por + ' :_____PROGRAMA_____GENERADOR_____: (:_____FECHA_____:)');
  FArchivoSalida.Add(Message_Total_Directorios + ' :_____TOTAL_____DIRECTORIOS_____:');
  FArchivoSalida.Add(Message_Total_Archivos + ' :_____TOTAL_____ARCHIVOS_____:');
  FArchivoSalida.Add(Message_Total_Size + ' :_____TOTAL_____SIZE_____:');
  FArchivoSalida.Add('-----------------------------------------------------------');
  FArchivoSalida.Add('');
  FArchivoSalida.Add('');
end;

// Añade el footer del archivo
procedure TGestorExportacionTxt.AddFooter;
var
  Texto : string;
begin
  // Si no es TXT, llama al padre
  if FFormato <> feTXT then
  begin
    inherited AddFooter;
    exit;
  end;

  // Asigna el texto
  Texto := FArchivoSalida.text;

  // Añade las variables
  Texto := StringReplace(Texto, ':_____TOTAL_____ARCHIVOS_____:', PuntearNumeracion(FTotalArchivos), [rfReplaceAll, rfIgnoreCase]);
  Texto := StringReplace(Texto, ':_____TOTAL_____DIRECTORIOS_____:', PuntearNumeracion(FTotalDirectorios), [rfReplaceAll, rfIgnoreCase]);
  Texto := StringReplace(Texto, ':_____TOTAL_____SIZE_____:', ConvertirSizeEx(FTotalSize) + ' (' + PuntearNumeracion(FTotalSize) + ' bytes)', [rfReplaceAll, rfIgnoreCase]);

  // Asigna el texto
  FArchivoSalida.text := Texto;
end;

// Añade un Item al archivo
procedure TGestorExportacionTxt.AddItem(const Item: TItemDato);
var
  Fecha : string;
begin
  // Si no es TXT, llama al padre
  if FFormato <> feTXT then
  begin
    inherited AddItem(Item);
    exit;
  end;

  case Item.Tipo of
    Directorio :  begin
                    FTotalDirectorios := FTotalDirectorios + 1;
                  end;

    Archivo    :  begin
                    FTotalArchivos := FTotalArchivos + 1;
                    FTotalSize     := FTotalSize + Item.Size;
                  end;
  end;

  // Si no tiene descripcion, la busca
  if Item.Descripcion = '' then
    Item.Descripcion := GetExtensionDescripcionById(Item.IdExtension);

  // Convierte la fecha
  DateTimeToString(Fecha, 'dd/mm/yyyy hh:mm:ss', Item.Fecha);

  // Añade el item
  FArchivoSalida.Add(Message_Exportacion_Nombre + ' ' + Item.Nombre);
  FArchivoSalida.Add(Message_Exportacion_Size + ' ' + PuntearNumeracion(Item.Size, True) + ' - ' + ConvertirSizeEx(Item.Size));
  FArchivoSalida.Add(Message_Exportacion_Tipo + ' ' + Item.Descripcion);
  FArchivoSalida.Add(Message_Exportacion_Fecha + ' ' + Fecha);
  FArchivoSalida.Add(Message_Exportacion_Atributos + ' ' + AtributosToStr(Item.Atributos, false));
  FArchivoSalida.Add(Message_Exportacion_Ruta + ' ' + Item.Ruta);

  FArchivoSalida.Add('');
  FArchivoSalida.Add('');
end;







{ TGestorExportacionHtml }

// Añade el header del archivo
procedure TGestorExportacionHtml.AddHeader;
begin
  // Si no es Html, llama al padre
  if FFormato <> feHTML then
  begin
    inherited AddHeader();
    exit;
  end;

  FArchivoSalida.Add('<!DOCTYPE html>');
  FArchivoSalida.Add('<html lang="es">');
  FArchivoSalida.Add('    <head>');
  FArchivoSalida.Add('        <meta charset="UTF-8">');
  FArchivoSalida.Add('        <title>' + Message_Exportacion_Titulo + ' :_____PROGRAMA_____GENERADOR_____: (:_____FECHA_____:)</title>');
  FArchivoSalida.Add('        <meta name="keywords" content="Report :_____FECHA_____:">');
  FArchivoSalida.Add('        ');

  FArchivoSalida.Add('<style>');

  FArchivoSalida.Add('    table, th, td {');
  FArchivoSalida.Add('        border:1px solid #CCCCCC;border-collapse:collapse;');
  FArchivoSalida.Add('    }');
  FArchivoSalida.Add('');
  FArchivoSalida.Add('    td {');
  FArchivoSalida.Add('        padding: 5px;');
  FArchivoSalida.Add('    }');
  FArchivoSalida.Add('');
  FArchivoSalida.Add('    footer {');
  FArchivoSalida.Add('        position: fixed;');
  FArchivoSalida.Add('        bottom: 0;');
  FArchivoSalida.Add('        width: 100%;');
  FArchivoSalida.Add('        background-color: #CCCCCC;');
  FArchivoSalida.Add('        color: #000000;');
  FArchivoSalida.Add('        text-align: center;');
  FArchivoSalida.Add('        font-size: 0.8em;');
  FArchivoSalida.Add('        padding: 5px;');
  FArchivoSalida.Add('    }');
  FArchivoSalida.Add('');
  FArchivoSalida.Add('    td.Size {');
  FArchivoSalida.Add('        text-align: right;');
  FArchivoSalida.Add('    }');

  FArchivoSalida.Add('</style>');

  FArchivoSalida.Add('    </head>');
  FArchivoSalida.Add('    <body>');
  FArchivoSalida.Add('<center>');
  FArchivoSalida.Add('<hr>');
  FArchivoSalida.Add('<h3>' + Message_Exportacion_Generado_Por + ' :_____PROGRAMA_____GENERADOR_____: (:_____FECHA_____:)</h3>');

  FArchivoSalida.Add('     <table border=0>');
  FArchivoSalida.Add('         <tr>');
  FArchivoSalida.Add('             <th>'+Message_Total_Directorios+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Total_Archivos+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Total_Size+'</th>');
  FArchivoSalida.Add('         </tr>');

FArchivoSalida.Add('         <tr>');
  FArchivoSalida.Add('             <td>:_____TOTAL_____DIRECTORIOS_____:</td>');
  FArchivoSalida.Add('             <td>:_____TOTAL_____ARCHIVOS_____:</td>');
  FArchivoSalida.Add('             <td>:_____TOTAL_____SIZE_____:</td>');
  FArchivoSalida.Add('         </tr>');
  FArchivoSalida.Add('     </table>');
  FArchivoSalida.Add('<hr>');


  FArchivoSalida.Add('     <table>');
  FArchivoSalida.Add('         <tr>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Nombre+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Size+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Tipo+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Fecha+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Atributos+'</th>');
  FArchivoSalida.Add('             <th>'+Message_Exportacion_Ruta+'</th>');
  FArchivoSalida.Add('         </tr>');

end;

// Añade el footer del archivo
procedure TGestorExportacionHtml.AddFooter;
var
  Texto : string;
begin
  // Si no es Html, llama al padre
  if FFormato <> feHTML then
  begin
    inherited AddFooter();
    exit;
  end;


  FArchivoSalida.Add('     </table>');
  FArchivoSalida.Add('</center>');
  FArchivoSalida.Add('        <footer>');
  FArchivoSalida.Add('            <strong>&copy; 2023 :_____PROGRAMA_____GENERADOR_____:</strong>');
  FArchivoSalida.Add('        </footer>');
  FArchivoSalida.Add('    </body>');
  FArchivoSalida.Add('</html>');

  // Asigna el texto
  Texto := FArchivoSalida.text;

  // Añade las variables
  Texto := StringReplace(Texto, ':_____TOTAL_____ARCHIVOS_____:', PuntearNumeracion(FTotalArchivos), [rfReplaceAll, rfIgnoreCase]);
  Texto := StringReplace(Texto, ':_____TOTAL_____DIRECTORIOS_____:', PuntearNumeracion(FTotalDirectorios), [rfReplaceAll, rfIgnoreCase]);
  Texto := StringReplace(Texto, ':_____TOTAL_____SIZE_____:', ConvertirSizeEx(FTotalSize) + ' (' + PuntearNumeracion(FTotalSize) + ' bytes)', [rfReplaceAll, rfIgnoreCase]);

  // Asigna el texto
  FArchivoSalida.text := Texto;
end;


// Añade un Item al archivo
procedure TGestorExportacionHtml.AddItem(const Item: TItemDato);
var
  Fecha : string;

begin
  // Si no es Html, llama al padre
  if FFormato <> feHTML then
  begin
    inherited AddItem(Item);
    exit;
  end;

  case Item.Tipo of
    Directorio :  begin
                    FTotalDirectorios := FTotalDirectorios + 1;
                  end;

    Archivo    :  begin
                    FTotalArchivos := FTotalArchivos + 1;
                    FTotalSize     := FTotalSize + Item.Size;
                  end;
  end;

  // Si no tiene descripcion, la busca
  if Item.Descripcion = '' then
    Item.Descripcion := GetExtensionDescripcionById(Item.IdExtension);

  // Convierte la fecha
  DateTimeToString(Fecha, 'dd/mm/yyyy hh:mm:ss', Item.Fecha);

  // Añade el item
  FArchivoSalida.Add('   <tr>');
  FArchivoSalida.Add('     <td>');
  FArchivoSalida.Add('         ' + Item.Nombre);
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('     <td class="Size">');
  FArchivoSalida.Add('          ' + ConvertirSizeEx(Item.Size));
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('     <td>');
  FArchivoSalida.Add('         ' + Item.Descripcion);
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('     <td>');
  FArchivoSalida.Add('         ' + Fecha);
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('     <td>');
  FArchivoSalida.Add('         ' + AtributosToStr(Item.Atributos, false));
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('     <td>');
  FArchivoSalida.Add('         ' + Item.Ruta);
  FArchivoSalida.Add('     </td>');
  FArchivoSalida.Add('   </tr>');

  FArchivoSalida.Add('');
  FArchivoSalida.Add('');

end;





initialization





finalization

end.
