(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-05 21:58:26
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-20 00:04:43
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

program cameex;

{$mode objfpc}{$H+}
{$i ./DirectivasCompilacion.inc}


uses
  {$IFDEF FUGAS_DE_MEMORIA_DETECTAR}
  HeapTrc,
  {$ENDIF FUGAS_DE_MEMORIA_DETECTAR}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  {$IFDEF FUGAS_DE_MEMORIA_DETECTAR_VOLCADOR_ARCHIVO}
  SysUtils,
  {$ENDIF FUGAS_DE_MEMORIA_DETECTAR_VOLCADOR_ARCHIVO}
  Interfaces, // this includes the LCL widgetset
  //
{$IFDEF DARKSTYLE_USAR}
  LCLType,LCLIntf,
  uDarkStyleParams,
	uMetaDarkStyle,
	uDarkStyleSchemes,
{$ENDIF DARKSTYLE_USAR}

  Forms, AppString, Configuracion, UnidadPrincipal, UnidadLoading,
  UnidadPropiedades, UnidadAtributos, UnidadScan, Control_About, graphics;

{$R *.res}

begin
{$IFDEF FUGAS_DE_MEMORIA_DETECTAR_VOLCADOR_ARCHIVO}
  // Gestión del archivo de volcado de fugas de memoria
  // Si existe, lo borramos
  if FileExists('fugas_de_memoria.trc') then
  begin
    DeleteFile('fugas_de_memoria.trc');
  end;

  // Activamos el volcado de fugas de memoria
  SetHeapTraceOutput('fugas_de_memoria.trc');
{$ENDIF FUGAS_DE_MEMORIA_DETECTAR_VOLCADOR_ARCHIVO}

  RequireDerivedFormResource:=True;

{$IFDEF DARKSTYLE_USAR}
  PreferredAppMode:=pamForceDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);


  ColorThemeStyle := RGBToColor(42, 42, 42);

  ReCalcularColores;
{$ENDIF DARKSTYLE_USAR}

  Application.Title:='CaMeEx';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

