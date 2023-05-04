(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-05 21:58:48
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-05 00:20:47
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

{$i ./DirectivasCompilacion.inc}


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
  , InterfaceConectorDatos
  , ItemDato, ItemCatalogo;


type
  // Defino la estructura interna de los datos de los nodos de la lista de archivos
  PrListaData = ^rTListaData;
  rTListaData = record
    NodeData : TItemDato;
  end;



  { TForm1 }

  TForm1 = class(TForm)
    Arbol: TLazVirtualStringTree;
    Button1: TButton;
    Button2: TButton;
    ImageListArchivos: TImageList;
    ImageListToolbar: TImageList;
    Lista: TLazVirtualStringTree;
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
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListaCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ListaGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure ListaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure ListaHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure MenuItemAcercaDeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer_UpdateUITimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    FScan           : TMotorScan;
    FVentanaScan    : TFormScan;
    FGestorDatos    : IConectorDatos;
    FListaCatalogos : TArrayItemDato;
    FListaArchivos  : TArrayItemDato;
  protected
    procedure DoOnTerminarScanAsync();
    procedure DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);
    procedure DoLoadListaArchivos(Catalogo : TItemCatalogo; Padre : TItemDato);

    procedure DoLiberarListaArchivos();
    function AddNodeLista(Dato: TItemDato): boolean;

    // Carga la configuración del programa
    procedure DoConfiguracionLoad();

    // Guarda la configuración del programa
    procedure DoConfiguracionSave();

    // Aplica la configuración del programa
    procedure DoConfiguracionAplicar();

  public

  end;


var
  Form1: TForm1;


implementation

uses appinfo
, Control_About
, Configuracion
, OrdenarLista
, Utilidades
, ConectorDatos
, ItemBaseDatos
, ItemExtension, ItemRutaCompleta, GestorExtensiones;

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

  // Agrega al logger el inicio del programa
  LogAdd(TLogLevel.info, 'Iniciando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');

  // Inicializar el Gestor de Datos
  FGestorDatos := TConectorDatos.Create;
  FGestorDatos.Iniciar(Curdir, DirectorioConfig);

  // Inicializar el Motor de Escaneo
  FScan := TMotorScan.Create;

  // Inicializar la lista de archivos
  Lista.NodeDataSize    := Sizeof(rTListaData);
  Lista.DoubleBuffered  := true;


  // Inicializa el sistema que devuelve la descripción e icono index de las extensiones
  SetExtensionesConfig(FGestorDatos, ImageListArchivos);


  // Carga la configuración del programa
  DoConfiguracionLoad();

  // Aplica la configuración del programa
  DoConfiguracionAplicar();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  // Libera la asignación de memoria
  DoLiberarListaArchivos();

  // Finalizar la lista de catalogos
  if assigned(FListaCatalogos) then
    begin
      FListaCatalogos.clear;
      FListaCatalogos.free;
    end;

  // Finalizar el Gestor de Datos
  FGestorDatos.Finalizar();

  // Guarda la configuración del programa
  DoConfiguracionSave();


  LogAdd(TLogLevel.info, 'Finalizando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');
end;

// Carga la configuración del programa
procedure TForm1.DoConfiguracionLoad();
begin
  // Carga la configuración de las columnas y el orden
  FColumnnaOrden           := ArchivoConfiguracion.ReadInteger('Config', 'ColumnnaOrden', FColumnnaOrden);
  FColumnnaOrden_Direccion := ArchivoConfiguracion.ReadInteger('Config', 'ColumnnaOrden_Direccion', FColumnnaOrden_Direccion);
end;

// Guarda la configuración del programa
procedure TForm1.DoConfiguracionSave();
begin
  // Guarda la configuración de las columnas y el orden
  ArchivoConfiguracion.WriteInteger('Config', 'ColumnnaOrden', FColumnnaOrden);
  ArchivoConfiguracion.WriteInteger('Config', 'ColumnnaOrden_Direccion', FColumnnaOrden_Direccion);

end;

// Aplica la configuración del programa
procedure TForm1.DoConfiguracionAplicar();
begin
  // Aplica la configuración de las columnas y el orden
  Lista.Header.SortColumn    := FColumnnaOrden;
  Lista.Header.SortDirection := TSortDirection(FColumnnaOrden_Direccion);
end;


procedure TForm1.ListaCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data_1       : PrListaData;
  Data_2       : PrListaData;
begin
  Result := 1;


  Data_1  := Sender.GetNodeData(Node1);
  if Data_1 = nil then
    exit;

  Data_2  := Sender.GetNodeData(Node2);
  if Data_2 = nil then
    exit;

  if Data_1^.NodeData  = nil then exit;
  if Data_2^.NodeData  = nil then exit;


  //Data_1^.Data^.WF_OrdenarPor := TOrdenarPor(Column);
  //Data_2^.Data^.WF_OrdenarPor := TOrdenarPor(Column);

(*
 case CompararPor of
  sdAscending  : Data_1^.Data^.WF_OrdenOrdenarPor := oopAscendente;
  sdDescending : Data_1^.Data^.WF_OrdenOrdenarPor := oopDescencente;
 end;
*)


 Result := ListSortFuncDos(Data_1^.NodeData, Data_2^.NodeData, FColumnnaOrden, FColumnnaOrden_Direccion);
end;

procedure TForm1.ListaGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PrListaData;
  Datos: TItemDato;
begin
  try
    NodeData := Lista.GetNodeData(Node);
    if NodeData <> nil then
    begin
      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        if Column = 0 then
        begin
          ImageIndex := Datos.ImageIndex;
          if ImageIndex = -1 then
            ImageIndex := 2;

          ImageIndex := GetExtensionIcopnIndexById(Datos.IdExtension, ImageIndex);
        end;
      end;
    end;
  except
  end;
end;

procedure TForm1.ListaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeData: PrListaData;
  Datos: TItemDato;
begin
  CellText := '';
  try
    NodeData := Lista.GetNodeData(Node);
    if NodeData <> nil then
    begin
      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        case Column of
          COLUMNA_NOMBRE    : CellText := Datos.Nombre;
          COLUMNA_SIZE      : CellText := inttostr(Datos.Size);
          COLUMNA_TIPO      : CellText := GetExtensionDescripcionById(Datos.IdExtension);
          COLUMNA_FECHA     : DateTimeToString(CellText, TipoHora, Datos.Fecha);
          COLUMNA_ATRIBUTOS : CellText := AtributosToStr(Datos.Atributos, false);
          COLUMNA_RUTA      : CellText := FGestorDatos.GetRutaCompleta(Datos);
        end;
      end;
    end;
  except
  end;
end;

procedure TForm1.ListaHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
 // Guarda las opciones de columna y orden en sus variables
  FColumnnaOrden           := HitInfo.Column;
   FColumnnaOrden_Direccion := integer(not boolean(Lista.Header.SortDirection));

 // Aplica al header la nueva config
  Lista.Header.SortColumn    := FColumnnaOrden;
  Lista.Header.SortDirection := TSortDirection(FColumnnaOrden_Direccion);

  // Ordena la lista
  Lista.SortTree(Lista.Header.SortColumn, Lista.Header.SortDirection);
end;

procedure TForm1.DoLiberarListaArchivos();
begin
  // Libera la asignación de memoria
  if assigned(FListaArchivos) then
  begin
    FListaArchivos.clear;
    FListaArchivos.free;
    FListaArchivos := nil;
  end;
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
  //FScan.ScanDirAsync(Curdir, @DoOnTerminarScanAsync, '');
  {$ELSE}
  FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync, '');
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

  procedure ProcesarHijo(Ruta: string; Item : TItemDato);
  var
    t, total : integer;
    Actual : TItemDato;
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
  Inicio   : TDateTime;
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
      {$IFDEF MOSTRAR_INFO_ESCANEO}
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


  FGestorDatos.BeginUpdate();
  try
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add('Guardando en la base de datos ...');

    Inicio   := now;
    try
      // Guarda los datos del catalogo en la base de datos
      DoGuardarEscaneado(FScan, FGestorDatos);
    finally
      SalidaLog.lines.Add('------------------------------------------');
      SalidaLog.lines.Add('Tiempo para guardar    :  ' + MostrarTiempoTranscurrido(Inicio, now));
    end;
  finally
    FGestorDatos.EndUpdate();
  end;

end;

procedure TForm1.DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);

  procedure ProcesarHijo(Item : TItemDato);
  var
    t, total : integer;
    Actual : TItemDato;
  begin
    if item <> nil then
    begin
      total := item.HijosCount()-1;
      for t := 0 to total do
      begin
        Actual := item.GetHijo(t);

        // Guarda los datos del archivo o directorio
        SistemaGuardado.AddDato(Actual);

        // Guarda los datos de todo los hijos
        ProcesarHijo(Actual);
      end;
    end;
  end;


var
  t, total : integer;
  Item     : TItemDato;
begin
  if assigned(Scan) then
  begin
    // Actualiza los datos del catalogo
    Scan.Root.TotalArchivos    := Scan.TotalArchivos;
    Scan.Root.TotalDirectorios := Scan.TotalDirectorios;
    Scan.Root.Size             := Scan.TotalSize;

    // Guarda los datos del catalogo
    SistemaGuardado.AddCatalogo(Scan.Root);

    // Guarda los datos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtension(TItemExtension(Scan.ListaExtensiones.Items[t]));
    end;

    // Guarda los iconos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtensionIcono(TItemExtension(Scan.ListaExtensiones.Items[t]));
    end;



    // Guarda los datos de las rutas completas
    total := Scan.ListaRutaCompleta.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddRutaCompleta(TItemRutaCompleta(Scan.ListaRutaCompleta.Items[t]));
    end;


    // Guarda los datos de los archivos y directorios
    total := FScan.Root.HijosCount()-1;
    for t := 0 to total do
    begin
      Item := FScan.Root.GetHijo(t);
      if item <> nil then
      begin
        // Guarda los datos del archivo o directorio
        SistemaGuardado.AddDato(Item);

        // Guarda los datos de todo los hijos
        ProcesarHijo(Item);
      end;
    end;


  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  // Cargar Catallogos
end;



procedure TForm1.DoLoadListaArchivos(Catalogo : TItemCatalogo; Padre : TItemDato);
var
  t, total      : integer;
begin
  Lista.BeginUpdate;
  try
    // Limpia la lista
    Lista.Clear;

    // Libera la asignación de memoria
    DoLiberarListaArchivos();

    // Carga los datos del catalogo
    if assigned(Catalogo) then
    begin
      // Carga los datos del catalogo
      FListaArchivos := FGestorDatos.GetDatos(Catalogo, Padre);
      if assigned(FListaArchivos) then
      begin
        // Carga los datos del catalogo
        total := FListaArchivos.count -1;
        for t := 0 to total do
        begin
          AddNodeLista(TItemDato(FListaArchivos[t]));
        end;
      end;
    end;

    // Ordena la lista
    Lista.SortTree(Lista.Header.SortColumn, Lista.Header.SortDirection);

  finally
    Lista.EndUpdate;
  end;
end;

function TForm1.AddNodeLista(Dato: TItemDato): boolean;
var
  Node   : PVirtualNode;
  Data   : PrListaData;
begin
  Node           := Lista.AddChild(nil);
  Data           := Lista.GetNodeData(Node);
  Data^.NodeData := Dato;
  Result         := True;
end;



procedure TForm1.Button2Click(Sender: TObject);
var
  CatalogoActual : TItemCatalogo;
  tiempo : qword;
begin
  tiempo := GetTickCount64();

  // Finalizar la lista de catalogos
  if assigned(FListaCatalogos) then
    begin
      FListaCatalogos.clear;
      FListaCatalogos.free;
    end;

  // Cargar Lista
  FListaCatalogos := FGestorDatos.GetAllCatalogos();
  if assigned(FListaCatalogos) then
    if FListaCatalogos.Count > 0 then
    begin
      // Carga el primer catalogo
      CatalogoActual := TItemCatalogo(FListaCatalogos[0]);
      if assigned(CatalogoActual) then
      begin
        // Carga los datos del catalogo
        DoLoadListaArchivos(CatalogoActual, nil);
      end;
    end;

  tiempo := GetTickCount64() - tiempo;

  SalidaLog.lines.Add('Tiempo empleado : ' + IntToStr(tiempo) + ' ms.');
end;


end.
