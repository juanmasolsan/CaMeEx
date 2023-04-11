(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-05 21:58:48
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-11 22:28:15
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

//TODO: Eliminar, solo es para pruebas
{.$DEFINE ESCANEAR_DIRECTORIO_GRANDE}


unit UnidadPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , Menus
  , ComCtrls
  , ExtCtrls, StdCtrls
  , laz.VirtualTrees
  , SynEdit
  , Control_Logger
  , Control_Formulario_Avanzado
  , MotorScan
  , UnidadScan
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Arbol: TLazVirtualDrawTree;
    Button1: TButton;
    ImageListToolbar: TImageList;
    Lista: TLazVirtualDrawTree;
    MenuPrincipal: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemAcercaDe: TMenuItem;
    MenuItemSalir: TMenuItem;
    PanelInferior: TPanel;
    PanelPrincipal: TPanel;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SalidaLog: TSynEdit;
    Timer1: TTimer;
    Timer_UpdateUI: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemAcercaDeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer_UpdateUITimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    FScan        : TMotorScan;
    FVentanaScan : TFormScan;
  protected
    procedure DoOnTerminarScanAsync();
  public

  end;

var
  Form1: TForm1;

implementation

uses appinfo
, Control_About
, ItemData
, Utilidades
;

{$R *.lfm}

function Get_Titulo_Ventana(MostarTituloApp : boolean; Extra : string = ''; Version : boolean = true): string;
begin
  Result := '';

  if MostarTituloApp then
    begin
      Result := NOMBRE_PROGRAMA;
      if Version then
        Result := Result + ' v.' + VERSION_PROGRAMA;
    end;

  if Extra <> '' then
    if Result <> '' then
      Result := Result + ' - ' + Extra
    else
      Result := Extra;
end;



{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption                 := Get_Titulo_Ventana(true, '', true);
  Application.Title       := Get_Titulo_Ventana(true, '', false);

  // Opciones avanzadas
  ActivarArchivoConfig('cameex_config.ini', false, true, false, NOMBRE_PROGRAMA);
  ActivarGuardadoPosicion;

  // Inicializar el Logger

  LogCreate(IncludeTrailingBackslash(DirectorioConfig) + NOMBRE_PROGRAMA + '.log' , TLogLevel.all, true);

  LogAdd(TLogLevel.info, 'Iniciando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');

  // Inicializar el Motor de Escaneo
  FScan := TMotorScan.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LogAdd(TLogLevel.info, 'Finalizando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');
end;

procedure TForm1.MenuItemAcercaDeClick(Sender: TObject);
begin
  Mostrar_Acerca_de(NOMBRE_PROGRAMA, VERSION_PROGRAMA, FECHA_PROGRAMA, NOMBRE_AUTOR, 110, APP_WEB, AUTOR_EMAIL);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(FScan) then
  begin
    FScan.free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a : integer;
  b : integer = 0;
begin
  try
    a := 100 div b;
  except
    on E: Exception do LogAddException('Excepción Detectada', E);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  // Timer1.Enabled := true;
  //FScan.ScanDir(Curdir);
  //DoOnTerminarScanAsync();

  FVentanaScan := TFormScan.CreateEx(self, FScan);

  {$IFNDEF ESCANEAR_DIRECTORIO_GRANDE}
  FScan.ScanDirAsync(Curdir, @DoOnTerminarScanAsync, '.git;img\iconos');
  {$ELSE}
  FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync);
  {$ENDIF}
//  Timer1.Enabled := true;


  // Muestra la ventana de escaneo
  FVentanaScan.ShowModal;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled := false;
  if assigned(FScan) then
  begin
    FScan.StopScan();
  end;
end;

procedure TForm1.Timer_UpdateUITimer(Sender: TObject);
begin
  //
  //SalidaLog.Lines.add('Timer_UpdateUITimer');
  if assigned(FScan) then
  begin
    StatusBar1.simpleText := 'Procesando : ' + FScan.Procesando;
  end;
end;

procedure TForm1.DoOnTerminarScanAsync();

  procedure ProcesarHijo(Ruta: string; Item : TDatoItem);
  var
    t, total : integer;
    Actual : TDatoItem;
  begin
    if item <> nil then
    begin
      total := item.HijosCount()-1;
      for t := 0 to total do
      begin
        Actual := item.GetHijo(t);

        SalidaLog.Lines.Add(Actual.ToString());

        ProcesarHijo(Ruta + IncludeTrailingBackslash(Actual.Nombre), Actual);
      end;
    end;
  end;

var
  t, total : integer;
  Item     : TDatoItem;
begin

  if assigned(FVentanaScan) then
  begin
    // Indica que ya termino el escaneo
    FVentanaScan.Terminar();

    // Libera la asignación de memoria
    FVentanaScan := nil;
  end;


  SalidaLog.lines.beginUpdate();
  try
    SalidaLog.Clear;

    {$IFNDEF ESCANEAR_DIRECTORIO_GRANDE}
      total := FScan.Root.HijosCount()-1;
      for t := 0 to total do
      begin
        Item := FScan.Root.GetHijo(t);
        if item <> nil then
        begin
          //SalidaLog.Lines.Add(Item.Nombre);
          SalidaLog.Lines.Add(Item.ToString());
          ProcesarHijo(IncludeTrailingBackslash(Item.Nombre), Item);
        end;
      end;
    {$ENDIF}
    SalidaLog.lines.Add('------------------------------------------');
    SalidaLog.lines.Add('Total Directorios      :  ' + PuntearNumeracion(FScan.TotalDirectorios));
    SalidaLog.lines.Add('Total Archivos         :  ' + PuntearNumeracion(FScan.TotalArchivos));

    SalidaLog.lines.Add('------------------------------------------');
    SalidaLog.lines.Add('Total tamaño detectado :  ' + PuntearNumeracion(FScan.TotalSize) + ' (' + ConvertirSizeEx(FScan.TotalSize, ',##', '.0' ) + ')');

    SalidaLog.lines.Add('------------------------------------------');
    SalidaLog.lines.Add('Tiempo de escaneo      :  ' + MostrarTiempoTranscurrido(FScan.ScanInicio, FScan.ScanFinal));
  finally
    SalidaLog.lines.endUpdate();
  end;
end;

end.
