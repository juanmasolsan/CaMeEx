(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-05 21:58:48
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-17 19:06:32
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
  LCLType
  , lclintf
  , LMessages
  , Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , Menus
  , ComCtrls
  , ExtCtrls, StdCtrls
  , laz.VirtualTrees
  , DefaultTranslator, LCLTranslator
  , VirtualTreesExtended
  , SynEdit
  , Control_Logger
  , Control_Formulario_Avanzado
  , MotorScan
  , UnidadScan
  , InterfaceConectorDatos
  , ItemBaseDatos
  , ItemDato, ItemCatalogo, VirtualTreesExtras;



type
  // Defino la estructura interna de los datos de los nodos de la lista de archivos
  PrListaData = ^rTListaData;
  rTListaData = record
    IsRutaSeleccionada: Boolean;
    IsPrevExpanded    : Boolean;
    TipoCatalogo      : TItemDatoTipo;
    TipoNode          : TItemDatoTipo;
    NodeData          : TItemDato;
  end;



  { TForm1 }

  TForm1 = class(TForm)
    Arbol: TLazVirtualStringTree;
    Button1: TButton;
    Button2: TButton;
    ImageListArchivos32: TImageList;
    ImageListArchivos: TImageList;
    ImageListToolbar: TImageList;
    Lista: TLazVirtualStringTree;
    MenuItem_Perfil_PorDefecto: TMenuItem;
    MenuItem_Perfil_Mixto: TMenuItem;
    MenuItem_Perfil_Moderno: TMenuItem;
    MenuItem_Perfil_Retro: TMenuItem;
    MenuItem_Perfiles: TMenuItem;
    MenuItem_Ver_Colores_Atributos: TMenuItem;
    MenuItem_Lista_de_Archivos_Resaltar_Columna_Orden: TMenuItem;
    MenuItem_Lista_de_Archivos: TMenuItem;
    MenuItem_Arbol_Catalogos_Ver_Lineas_Punteadas: TMenuItem;
    MenuItem_Arbol_Catalogo_Ver_Lineas: TMenuItem;
    MenuItem_Arbol_Catalogo_Botones_Modernos: TMenuItem;
    MenuItem_Arbol_Catalogos_AutoOculta_Botones: TMenuItem;
    MenuItem_Catalogos_Mostrar_Info_extra: TMenuItem;
    MenuItem_Catalogos_color: TMenuItem;
    MenuItem_Catalogos: TMenuItem;
    MenuItem_Iconos_Mixto: TMenuItem;
    MenuItem_Iconos_Sistema: TMenuItem;
    MenuItem_Iconos_PorDefecto: TMenuItem;
    MenuItem_Iconos: TMenuItem;
    MenuItem_Size_AutoMatico: TMenuItem;
    MenuItem_Size_Punteada: TMenuItem;
    MenuItem_Size_Normal: TMenuItem;
    MenuItem_Sizes: TMenuItem;
    MenuItemVer: TMenuItem;
    MenuPrincipal: TMainMenu;
    MenuItemArchivo: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemAyuda: TMenuItem;
    MenuItemAcercaDe: TMenuItem;
    MenuItemSalir: TMenuItem;
    PanelInferior: TPanel;
    PanelPrincipal: TPanel;
    SalidaLog: TSynEdit;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Barra_Estado: TStatusBar;
    Timer1: TTimer;
    Timer_UpdateUI: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ArbolBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure ArbolChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ArbolDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure ArbolExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure ArbolHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ArbolKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ArbolResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListaAfterItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure ListaBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      {%H-}CellPaintMode: TVTCellPaintMode; CellRect: TRect; var {%H-}ContentRect: TRect);
    procedure ListaCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; {%H-}Column: TColumnIndex; var Result: Integer);
    procedure ListaDblClick(Sender: TObject);
    procedure ListaGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure ListaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure ListaHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ListaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListaPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);
    procedure ListaResize(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItemAcercaDeClick(Sender: TObject);
    procedure MenuItem_Arbol_Catalogos_AutoOculta_BotonesClick(Sender: TObject);
    procedure MenuItem_Arbol_Catalogos_Ver_Lineas_PunteadasClick(Sender: TObject
      );
    procedure MenuItem_Arbol_Catalogo_Botones_ModernosClick(Sender: TObject);
    procedure MenuItem_Arbol_Catalogo_Ver_LineasClick(Sender: TObject);
    procedure MenuItem_Catalogos_colorClick(Sender: TObject);
    procedure MenuItem_Catalogos_Mostrar_Info_extraClick(Sender: TObject);
    procedure MenuItem_Iconos_PorDefectoClick(Sender: TObject);
    procedure MenuItem_Lista_de_Archivos_Resaltar_Columna_OrdenClick(
      Sender: TObject);
    procedure MenuItem_Perfil_MixtoClick(Sender: TObject);
    procedure MenuItem_Size_NormalClick(Sender: TObject);
    procedure MenuItem_Ver_Colores_AtributosClick(Sender: TObject);
    procedure PanelInferiorResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer_UpdateUITimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    FScan            : TMotorScan;
    FVentanaScan     : TFormScan;
    FGestorDatos     : IConectorDatos;
    FListaCatalogos  : TArrayItemDato;
    FListaArchivos   : TArrayItemDato;
    FListaDirectorios: TArrayItemDato;
    FAplicandoConfig : boolean;

    FConfiguracionColumnas  : TListaDatosColumnasHeaderVirtualTrees;
    FIniciado_Header        : Boolean;
    FMenuHeader             : Boolean;
    FIsListaResizing        : Boolean;

    // Para la navegación
    FCatalogoID   : Qword;
    FPadreID      : Qword;
    FTempPadre    : TItemDato;
    FNodeArbolActual  : PVirtualNode;
    FNodeArbolRaiz    : PVirtualNode;
    FNodeArbolRaizDato: TItemCatalogo;

    FCatalogoSeleccionadoNodo : PVirtualNode;
    FCatalogoSeleccionadoItem : TItemCatalogo;
  protected
    procedure DoOnTerminarScanAsync();
    procedure DoGuardarEscaneado(Scan : TMotorScan; SistemaGuardado : IConectorDatos);
    procedure DoLoadListaArchivos(Padre : TItemDato);
    procedure DoLoadListaCatalogos();
    procedure DoLoadListaDirectorios(Node : PVirtualNode; Padre : TItemDato);

    procedure DoLiberarListaArchivos();
    function AddNode(Sender: TBaseVirtualTree; Dato: TItemDato; Padre : PVirtualNode; TieneHijos : boolean; TipoCatalogo : TItemDatoTipo; TipoNode : TItemDatoTipo): boolean;

    procedure DoLiberarListaCatalogos();
    procedure DoLiberarListaDirectorios(Full: boolean);


    // Devuelve la ruta de un item
    function GetRutaFromItem(Item: TItemDato) : RawByteString;

    // Carga la configuración del programa
    procedure DoConfiguracionLoad();

    // Guarda la configuración del programa
    procedure DoConfiguracionSave();

    // Aplica la configuración del programa
    procedure DoConfiguracionAplicar();

    // Inicializa el header de la lista de archivos
    procedure DoHeader_Iniciar(MenuHeader : boolean = true; CargarConfig : boolean = false);

    // Actualiza el header de la lista de archivos
    procedure DoHeader_Update;

    // Evento de hacer click en emenú contextual del header de la lista de archivos
    procedure DoHeader_MenuColumnasClick(Sender: TObject);

    // Pone una columna como visible o no
    procedure DoHeader_IsVisible(Id : integer; IsHeaderVisible : Boolean);

    // Usa blend para dibujar la selección o el hover
    procedure DoDibujarSeleccionModerna(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; NivelBlend : longint = 90);

    // Aplica color a los textos de los nodos dependiendo de su estado
    procedure DoSetColor_Texto(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode);

    // Aplica color a los textos de los nodos dependiendo de los atributos del item
    procedure DoColorear_Dependiendo_De_Los_Atributos(Data : TItemDato; NCanvas : TCanvas);

    // Abre un directorio en la lista de archivos y muestra su contenido
    procedure DoNavegarElementoLista();
    procedure DoLoadListaArchivosAsync({%H-}Data: PtrInt);

    // Reajusta el tamaño de la última columna
    procedure DoResizeControlDatos(Sender: TLazVirtualStringTree);

    // Carga la lista de catálogos
    procedure DoLoadListaCatalogosAsync({%H-}Data: PtrInt);

    // Sincroniza el arbol con la vista de la lista de directorios
    procedure DoSincronizarListaArbol();

    // Marca todos los nodos que esté en la ruta seleccionada
    procedure DoMarcarRutaActual(Node: PVirtualNode);

    // Crea el nodo raiz del arbol
    procedure DoCrearNodoRootArbol();

    // Libera el nodo raiz del arbol
    procedure DoLiberarNodoRootArbol();

    // Muestra Información del directorio actual
    procedure DoEstadisticas(Columna : longint; Texto : string);

    // Ajusta el alto dependiendo si se debe o no mostrar la info de los catalogos
    procedure DoAjustarNodosCatalogos();

    // Elimina un item del arbol/lista
    function DoEliminarItem(Sender: TLazVirtualStringTree; Node: PVirtualNode; IsDesdeArbol : boolean) : boolean;

    // Confirma si se debe eliminar todos los catalogos
    function DoConfirmarEliminarTodo() : boolean;

    // Confirma si se debe eliminar el catalogo seleccionado
    function DoConfirmarEliminarCatalogo(Catalogo : TItemCatalogo) : boolean;

    // Confirma si se debe eliminar los datos seleccionados
    function DoConfirmarEliminarDatos() : boolean;

    // Actualiza el nodo raiz con el total de datos
    procedure DoUpdateDatosNodoRaiz();

    // Oculta/Elimina los nodos que encuentre iguales a al Item que se le pasa
    procedure DoEliminarItemArbol(Item : TItemDato);

    procedure DoCalcularAsync({%H-}Data: PtrInt);

    // Muestra el formulario de carga
    procedure DoFormLoadingShow(MensajeTitulo : string; MensajeNormal : string);

    // Oculta el formulario de carga
    procedure DoFormLoadingHide();

    // Activa o desactiva los controles
    procedure DoActivarShowModal(activar : boolean);

  public

  end;


var
  Form1: TForm1;


implementation

uses
  GraphType
  ,  appinfo
  , Control_About
  , Configuracion
  , OrdenarLista
  , Utilidades
  , ConectorDatos
  , ItemExtension, ItemRutaCompleta, GestorExtensiones, AppString, UnidadLoading;

{$R *.lfm}

const
  PERFIL_DEFECTO = 0;
  PERFIL_RETRO   = PERFIL_DEFECTO + 1;
  PERFIL_MODERNO = PERFIL_RETRO + 1;
  PERFIL_MIXTO   = PERFIL_MODERNO + 1;




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

  // Necesarío para que funcione la navegación por la lista de archivos
  FTempPadre    := TItemDato.create('', TItemDatoTipo.Directorio, now, 0);


  // Inicializar el Motor de Escaneo
  FScan := TMotorScan.Create;

  // Inicializar la lista de archivos
  PanelPrincipal.DoubleBuffered := true;
  Lista.NodeDataSize            := Sizeof(rTListaData);
  Lista.DoubleBuffered          := true;

  // Inicializar del arbol de directorios
  Arbol.NodeDataSize            := Sizeof(rTListaData);
  Arbol.DoubleBuffered          := true;

  // Inicializar el arbol de catalogos
  FListaDirectorios := TArrayItemDato.create;;

  // Inicializa el sistema que devuelve la descripción e icono index de las extensiones
  SetExtensionesConfig(FGestorDatos, ImageListArchivos, ImageListArchivos32);


  // Carga la configuración del programa
  DoConfiguracionLoad();

  // Aplica la configuración del programa
  FAplicandoConfig := false;
  DoConfiguracionAplicar();

  // Inicializa variables
  FIsListaResizing := false;

  // Inicializa el header de la lista
  DoHeader_Iniciar(true);

  // Lanza el método de forma asíncrona para cargar la lista de catalogos
  application.QueueAsyncCall(@DoLoadListaCatalogosAsync, 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Libera la asignación de memoria
  DoLiberarListaArchivos();

  // Finalizar la lista de catalogos
  DoLiberarListaCatalogos();

  // Finalizar la lista de directorios
  DoLiberarListaDirectorios(true);

  // Finalizar el Gestor de Datos
  FGestorDatos.Finalizar();

  // Finalizar los objectos necesarios para la navegación por la lista de archivos
  FTempPadre.free;

  // Libera el nodo root del arbol
  DoLiberarNodoRootArbol();

  // Guarda la configuración del programa
  DoConfiguracionSave();

  LogAdd(TLogLevel.info, 'Finalizando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');
end;

procedure TForm1.ListaAfterItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  DoDibujarSeleccionModerna(Sender, TargetCanvas, Node, ItemRect);
end;

procedure TForm1.ListaBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
 if Sender <> Arbol then
  if Column = TLazVirtualStringTree(Pointer(@Sender)^).Header.SortColumn then
    if FResaltarColumnaOrden then
      if not Sender.Selected[Node] then
        if not (Sender.HotNode = Node) then
          begin
            Dibujar_FillRect_Blend(TargetCanvas,  CellRect,  FResaltarColumnaOrdenColor,  25,  0, 0);
          end;
end;

// Carga la configuración del programa
procedure TForm1.DoConfiguracionLoad();
begin
  // Carga la configuración de las columnas y el orden
  FColumnnaOrden           := ArchivoConfiguracion.ReadInteger('Config', 'ColumnnaOrden', FColumnnaOrden);
  FColumnnaOrden_Direccion := ArchivoConfiguracion.ReadInteger('Config', 'ColumnnaOrden_Direccion', FColumnnaOrden_Direccion);

  // Carga la configuración del formato del tamaño
  FFormatoSize             := TFormatoSize(ArchivoConfiguracion.ReadInteger('Config', 'FormatoSize', integer(FFormatoSize)));

  // Carga la configuración de resalto de la columna por la que se ordena
  FResaltarColumnaOrden    := ArchivoConfiguracion.ReadBool('Config', 'ResaltarColumnaOrden', FResaltarColumnaOrden);


  // Carga la configuración de la forma en la que mostrar los iconos
  FFormatoIconos           := TFormatoIconos(ArchivoConfiguracion.ReadInteger('Config', 'FormatoIconos', integer(FFormatoIconos)));

  // Carga la configuración de las columnas
  FConfiguracionColumnas    := HeaderVirtualTrees_Get(Lista);
  FConfiguracionColumnas    := HeaderVirtualTrees_CargarColumnas(ArchivoConfiguracion, 'Columnas.Lista.Archivos', FConfiguracionColumnas);

  // Carga las Dimensiones del árbol de directorios
  FArbolAncho               := ArchivoConfiguracion.ReadInteger('Config', 'ArbolAncho', FArbolAncho);

  // Carga las Dimensiones del Log
  FLogAlto                  := ArchivoConfiguracion.ReadInteger('Config', 'LogAlto', FLogAlto);

  // Carga la configuración de la forma en la dibujar el fondo de los catalogos
  FUsarColoresCatalogos     := ArchivoConfiguracion.ReadBool('Config', 'UsarColoresCatalogos', FUsarColoresCatalogos);

  // Carga la configuración de la forma de dibujar los nodos de los catalogos
  FExtraInfoCatalogos       := ArchivoConfiguracion.ReadBool('Config', 'ExtraInfoCatalogos', FExtraInfoCatalogos);

  // Carga la configuración de la forma de dibujar los nodos de los catalogos
  FAutoOcultarBotonesArbol  := ArchivoConfiguracion.ReadBool('Config', 'AutoOcultarBotonesArbol', FAutoOcultarBotonesArbol);
  FVerBotonesArbolModernos  := ArchivoConfiguracion.ReadBool('Config', 'VerBotonesArbolModernos', FVerBotonesArbolModernos);
  FVerLineasArbol           := ArchivoConfiguracion.ReadBool('Config', 'VerLineasArbol', FVerLineasArbol);
  FVerLineasArbol_Punteadas := ArchivoConfiguracion.ReadBool('Config', 'VerLineasArbol_Punteadas', FVerLineasArbol_Punteadas);



  // Carga la configuración para diferenciar los archivos por colores
  FUsarColorDiferenciarArchivos:= ArchivoConfiguracion.ReadBool('Config', 'UsarColorDiferenciarArchivos', FUsarColorDiferenciarArchivos);

end;

// Guarda la configuración del programa
procedure TForm1.DoConfiguracionSave();
begin
  // Guarda la configuración de las columnas y el orden
  ArchivoConfiguracion.WriteInteger('Config', 'ColumnnaOrden', FColumnnaOrden);
  ArchivoConfiguracion.WriteInteger('Config', 'ColumnnaOrden_Direccion', FColumnnaOrden_Direccion);

  // Guarda la configuración del formato del tamaño
  ArchivoConfiguracion.WriteInteger('Config', 'FormatoSize', integer(FFormatoSize));

  // Guarda la configuración de la forma en la que mostrar los iconos
  ArchivoConfiguracion.WriteInteger('Config', 'FormatoIconos', integer(FFormatoIconos));

  // Guarda la configuración de la forma en la que mostrar los iconos
  ArchivoConfiguracion.WriteBool('Config', 'ResaltarColumnaOrden', FResaltarColumnaOrden);

  // Guarda la configuración de las columnas
  if (Lista.Header.Columns.Count -1) > 1 then
  begin
    FConfiguracionColumnas := HeaderVirtualTrees_Get(Lista);
    HeaderVirtualTrees_GuardarColumnas(ArchivoConfiguracion, 'Columnas.Lista.Archivos', FConfiguracionColumnas);
  end;

  // Guarda las Dimensiones del árbol de directorios
  ArchivoConfiguracion.WriteInteger('Config', 'ArbolAncho', FArbolAncho);

  // Guarda las Dimensiones del Log
  ArchivoConfiguracion.WriteInteger('Config', 'LogAlto', FLogAlto);

  // Guarda la configuración de la forma en la dibujar el fondo de los catalogos
  ArchivoConfiguracion.WriteBool('Config', 'UsarColoresCatalogos', FUsarColoresCatalogos);

  // Guarda la configuración de la forma de dibujar los nodos de los catalogos
  ArchivoConfiguracion.WriteBool('Config', 'ExtraInfoCatalogos', FExtraInfoCatalogos);

  // Guarda la configuración de la forma de dibujar los nodos de los catalogos
  ArchivoConfiguracion.WriteBool('Config', 'AutoOcultarBotonesArbol', FAutoOcultarBotonesArbol);
  ArchivoConfiguracion.WriteBool('Config', 'VerBotonesArbolModernos', FVerBotonesArbolModernos);
  ArchivoConfiguracion.WriteBool('Config', 'VerLineasArbol', FVerLineasArbol);
  ArchivoConfiguracion.WriteBool('Config', 'VerLineasArbol_Punteadas', FVerLineasArbol_Punteadas);

  // Guarda la configuración para diferenciar los archivos por colores
  ArchivoConfiguracion.WriteBool('Config', 'UsarColorDiferenciarArchivos', FUsarColorDiferenciarArchivos);

  // Guarda la configuración en el archivo
  ArchivoConfiguracion.UpdateFile;

end;

// Aplica la configuración del programa
procedure TForm1.DoConfiguracionAplicar();
begin
  if FAplicandoConfig then exit;
  FAplicandoConfig := true;
  try
    // Aplica la configuración de las columnas y el orden
    Lista.Header.SortColumn    := FColumnnaOrden;
    Lista.Header.SortDirection := TSortDirection(FColumnnaOrden_Direccion);

    // Aplica la configuración del formato del tamaño
    MenuItem_Sizes.Items[longint(FFormatoSize)].Checked := true;

    // Guarda la configuración de la forma en la que mostrar los iconos
    MenuItem_Lista_de_Archivos_Resaltar_Columna_Orden.Checked := FResaltarColumnaOrden;

    // Aplica la configuración del formato de los iconos
    MenuItem_Iconos.Items[longint(FFormatoIconos)].Checked  := true;

    // Aplica la configuración de las columnas
    DoHeader_Update;

    // Aplica la configuración de las Dimensiones del árbol de directorios
    Arbol.width := FArbolAncho;

    // Aplica la configuración de las Dimensiones del Log
    PanelInferior.Height := FLogAlto;

    // Aplica la configuración de la forma en la dibujar el fondo de los catalogos
    MenuItem_Catalogos_color.Checked := FUsarColoresCatalogos;

    // Aplica la configuración de la forma de dibujar los nodos de los catalogos
    MenuItem_Catalogos_Mostrar_Info_extra.Checked := FExtraInfoCatalogos;
    Arbol.DibujarInfoCatalogo                     := FExtraInfoCatalogos;

    // Aplica la configuración al arbol
    Arbol.MostrarLineasArbol          := FVerLineasArbol;
    Arbol.MostrarLineasArbolPunteadas := FVerLineasArbol_Punteadas;
    Arbol.OcultarBotonesAlperderFoco  := FAutoOcultarBotonesArbol;
    Arbol.DibujarBotonesModernos      := FVerBotonesArbolModernos;

    // Aplica la config a los menus
    MenuItem_Arbol_Catalogos_AutoOculta_Botones.Checked   := FAutoOcultarBotonesArbol;
    MenuItem_Arbol_Catalogos_Ver_Lineas_Punteadas.Checked := FVerLineasArbol_Punteadas;
    MenuItem_Arbol_Catalogo_Botones_Modernos.Checked      := FVerBotonesArbolModernos;
    MenuItem_Arbol_Catalogo_Ver_Lineas.Checked            := FVerLineasArbol;
    MenuItem_Ver_Colores_Atributos.Checked                := FUsarColorDiferenciarArchivos;


    // Aplica la configuración a los root del arbol
    DoAjustarNodosCatalogos();


    // Aplica la configuración al arbol
    Arbol.Repaint;

    // Aplica la configuración a la lista de archivos
    Lista.Repaint;
  finally
    FAplicandoConfig := false;
  end;
end;

// Inicializa el header de la lista de archivos
procedure TForm1.DoHeader_Iniciar(MenuHeader : boolean = true; CargarConfig : boolean = false);
var
  t, total     : integer;
begin
  if Lista.Header.Columns.Count = 0 then exit;

  if FIniciado_Header then exit;
    FIniciado_Header := true;

  FMenuHeader := MenuHeader;

  if CargarConfig then
    FConfiguracionColumnas := HeaderVirtualTrees_Get(Lista);

  Total        := Lista.Header.Columns.Count -1;
  for t := 0 to Total do
    Lista.Header.Columns.Items[t].Tag := t;

  if FMenuHeader then
    HeaderVirtualTrees_PopUpMenu_Crear(Lista, @DoHeader_MenuColumnasClick);
end;

// Evento de hacer click en emenú contextual del header de la lista de archivos
procedure TForm1.DoHeader_MenuColumnasClick(Sender: TObject);
begin
  if TMenuItem(Pointer(@Sender)^).Tag = -1 then exit;
  TMenuItem(Pointer(@Sender)^).Checked := not TMenuItem(Pointer(@Sender)^).Checked;
  DoHeader_IsVisible(TMenuItem(Pointer(@Sender)^).Tag, TMenuItem(Pointer(@Sender)^).Checked);
end;

// Pone una columna como visible o no
procedure TForm1.DoHeader_IsVisible(Id : integer; IsHeaderVisible : Boolean);
begin
  FConfiguracionColumnas.Datos[Id].IsVisible := IsHeaderVisible;
  DoHeader_Update;
end;

// Actualiza el header de la lista de archivos
procedure TForm1.DoHeader_Update;
begin
  HeaderVirtualTrees_Set(Lista, FConfiguracionColumnas);
end;

// Compara dos nodos de la lista de archivos
procedure TForm1.ListaCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
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

  if Sender = Lista then
    begin
    // Comprueba si la columna es la de la ruta
    if Column = COLUMNA_RUTA then
    begin
      // Obtiene la ruta de los items
      GetRutaFromItem(Data_1^.NodeData);
      GetRutaFromItem(Data_2^.NodeData);
    end;

    // Compara los items
    Result := ListSortFuncDos(Data_1^.NodeData, Data_2^.NodeData, FColumnnaOrden, FColumnnaOrden_Direccion, Column <> COLUMNA_RUTA);
  end
  else
    Result := ListSortFuncDos(Data_1^.NodeData, Data_2^.NodeData, COLUMNA_TIPO, 0, true);
end;

procedure TForm1.ListaDblClick(Sender: TObject);
begin
  DoNavegarElementoLista();
end;

// Obtiene el index de la imagen a usar el el nodo
procedure TForm1.ListaGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PrListaData;
  Datos: TItemDato;
begin
  try
    NodeData := Sender.GetNodeData(Node);
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

          if Datos.Tipo >= TItemDatoTipo.Root then
            ImageIndex := integer(Datos.Tipo);

          case FFormatoIconos of
            Sistema : ImageIndex := GetExtensionIcopnIndexById(Datos.IdExtension, ImageIndex);
            Mixto   : begin
                        if Datos.Tipo <> TItemDatoTipo.Directorio then
                          ImageIndex := GetExtensionIcopnIndexById(Datos.IdExtension, ImageIndex);
                      end;
          end;

          Ghosted := (((Datos.Atributos and faHidden{%H-})= faHidden{%H-}) or (Datos.Nombre[1] = '.')) ;
        end;
      end;
    end;
  except
  end;
end;

// Devuelve la ruta de un item
function TForm1.GetRutaFromItem(Item: TItemDato) : RawByteString;
begin
  Result := '';
  if Item <> nil then
  begin
    if Item.Ruta = '' then
      Item.Ruta := FGestorDatos.GetRutaCompleta(Item);

    Result := Item.Ruta;
  end;
end;

procedure TForm1.ListaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeData: PrListaData;
  Datos: TItemDato;
begin
  CellText := '';
  try
    NodeData := Sender.GetNodeData(Node);
    if NodeData <> nil then
    begin
      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        case Column of
          COLUMNA_NOMBRE    : CellText := Datos.Nombre;
          COLUMNA_SIZE      : begin
                                if Datos.Tipo <> TItemDatoTipo.Directorio then
                                  case FFormatoSize of
                                    Normal     : CellText := IntToStr(Datos.Size) + '  ';
                                    Puntuada   : CellText := PuntearNumeracion(Datos.Size, True) + '  ';
                                    Automatico : CellText := ConvertirSizeEx(Datos.Size) + '  ';
                                  end
                              end;
          COLUMNA_TIPO      : CellText := GetExtensionDescripcionById(Datos.IdExtension);
          COLUMNA_FECHA     : DateTimeToString(CellText, TipoHora, Datos.Fecha);
          COLUMNA_ATRIBUTOS : CellText := AtributosToStr(Datos.Atributos, false);
          COLUMNA_RUTA      : CellText := GetRutaFromItem(Datos);
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

procedure TForm1.ListaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  case key of
    VK_RETURN : begin
                  if Shift = [] then
                  begin
                    DoNavegarElementoLista();
                  end;
                end;

    VK_DELETE : begin
                  if Shift = [] then
                  begin
                    Node := Lista.GetFirstSelected();
                    if Node <> nil then
                      DoEliminarItem(Lista, Node, false);
                  end;
                end;

  end;
end;

procedure TForm1.ListaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  NodeData: PrListaData;
  Datos   : TItemDato;
begin
  try
    NodeData := Sender.GetNodeData(Node);
    if NodeData <> nil then
    begin
      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        // Aplica el color del texto dependiendo del estado del node
        DoSetColor_Texto(Sender, TargetCanvas, Node);

        if not ((vsSelected in Node^.States) or (Sender.HotNode = Node)) then
          DoColorear_Dependiendo_De_Los_Atributos(Datos, TargetCanvas);
      end;

      // Resalta el nodo como que está en la ruta actual
      TargetCanvas.Font.Bold := NodeData^.IsRutaSeleccionada;
    end;

    // Quita el subrayado del texto
    TargetCanvas.Font.Underline := false;
  except
  end;
end;

procedure TForm1.ListaResize(Sender: TObject);
begin

  // Evita que se ejecute el evento si se esta redimensionando la lista
  if FIsListaResizing then exit;

  // Marca como redimensionando
  FIsListaResizing := true;
  try
    // Reajusta el tamaño de la última columna
    DoResizeControlDatos(Lista);

    // Redibuja el panel principal para evitar parpadeos en el dibujado
    PanelPrincipal.Repaint;

  finally
    // Marca como no redimensionando
    FIsListaResizing := false;
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  beep;
end;

// Reajusta el tamaño de la última columna
procedure TForm1.DoResizeControlDatos(Sender: TLazVirtualStringTree);
var
  Columna   : TVirtualTreeColumn;
  t, total  : longint;
  SizeColumna      : longint;

begin
  // Inicializa el tamaño de las columnas
  SizeColumna  := 0;


  // Obtiene el total de columnas visibles
  total := high(Sender.Header.Columns.GetVisibleColumns);

  // Obtiene el tamaño de las columnas visibles
  for t := 0 to total -1 do
    inc(SizeColumna, Sender.Header.Columns.GetVisibleColumns[t].Width);

  // Obtiene la última columna visible
  Columna  := Sender.Header.Columns.GetVisibleColumns[total];

  // Obtiene el tamaño de la columna final restandole el tamaño del ancho del scrollbar vertical
  SizeColumna := Sender.Width - SizeColumna - GetSystemMetrics(SM_CXVSCROLL);

  // Si el tamaño de la columna es menor a 100, se establece en 100
  if SizeColumna < 100 then
    SizeColumna := 100;

  // Establece el tamaño de la columna
  Columna.Width := SizeColumna;
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

procedure TForm1.DoLiberarListaCatalogos();
begin
  // Libera la asignación de memoria
  if assigned(FListaCatalogos) then
  begin
    FListaCatalogos.clear;
    FListaCatalogos.free;
    FListaCatalogos := nil;
  end;
end;


procedure TForm1.DoLiberarListaDirectorios(Full: boolean);
begin
  // Libera la asignación de memoria
  if assigned(FListaDirectorios) then
  begin
    FListaDirectorios.clear;
    if Full then
    begin
      FListaDirectorios.free;
      FListaDirectorios := nil;
    end;
  end;
end;





procedure TForm1.MenuItemAcercaDeClick(Sender: TObject);
begin
  Mostrar_Acerca_de(NOMBRE_PROGRAMA, VERSION_PROGRAMA, FECHA_PROGRAMA, NOMBRE_AUTOR, 110, APP_WEB, AUTOR_EMAIL);
end;

procedure TForm1.MenuItem_Arbol_Catalogos_AutoOculta_BotonesClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FAutoOcultarBotonesArbol         := MenuItem_Arbol_Catalogos_AutoOculta_Botones.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.MenuItem_Arbol_Catalogos_Ver_Lineas_PunteadasClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerLineasArbol_Punteadas := MenuItem_Arbol_Catalogos_Ver_Lineas_Punteadas.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.MenuItem_Arbol_Catalogo_Botones_ModernosClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerBotonesArbolModernos     := MenuItem_Arbol_Catalogo_Botones_Modernos.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.MenuItem_Arbol_Catalogo_Ver_LineasClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerLineasArbol          := MenuItem_Arbol_Catalogo_Ver_Lineas.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.MenuItem_Catalogos_colorClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FUsarColoresCatalogos := MenuItem_Catalogos_color.Checked;

  DoConfiguracionAplicar();

  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm1.MenuItem_Catalogos_Mostrar_Info_extraClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FExtraInfoCatalogos := MenuItem_Catalogos_Mostrar_Info_extra.Checked;

  DoConfiguracionAplicar();

  Arbol.Repaint;
end;

procedure TForm1.MenuItem_Iconos_PorDefectoClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FFormatoIconos := TFormatoIconos(TMenuItem(Pointer(@Sender)^).Tag);
  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm1.MenuItem_Lista_de_Archivos_Resaltar_Columna_OrdenClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FResaltarColumnaOrden := MenuItem_Lista_de_Archivos_Resaltar_Columna_Orden.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.MenuItem_Perfil_MixtoClick(Sender: TObject);
begin
  case TMenuItem(Pointer(@Sender)^).Tag of
    PERFIL_DEFECTO  :  begin
                        FFormatoSize                  := TFormatoSize.Puntuada;
                        FFormatoIconos                := TFormatoIconos.PorDefecto;
                        FResaltarColumnaOrden         := true;
                        FUsarColorDiferenciarArchivos := true;
                        FUsarColoresCatalogos         := true;
                        FExtraInfoCatalogos           := true;
                        FAutoOcultarBotonesArbol      := false;
                        FVerBotonesArbolModernos      := false;
                        FVerLineasArbol               := true;
                        FVerLineasArbol_Punteadas     := false;
                      end;

    PERFIL_RETRO   :  begin
                        FFormatoSize                  := TFormatoSize.Normal;
                        FFormatoIconos                := TFormatoIconos.PorDefecto;
                        FResaltarColumnaOrden         := false;
                        FUsarColorDiferenciarArchivos := false;
                        FUsarColoresCatalogos         := false;
                        FExtraInfoCatalogos           := false;
                        FAutoOcultarBotonesArbol      := false;
                        FVerBotonesArbolModernos      := false;
                        FVerLineasArbol               := true;
                        FVerLineasArbol_Punteadas     := true;
                      end;

    PERFIL_MODERNO : begin
                        FFormatoSize                  := TFormatoSize.Automatico;
                        FFormatoIconos                := TFormatoIconos.Sistema;
                        FResaltarColumnaOrden         := true;
                        FUsarColorDiferenciarArchivos := true;
                        FUsarColoresCatalogos         := true;
                        FExtraInfoCatalogos           := true;
                        FAutoOcultarBotonesArbol      := true;
                        FVerBotonesArbolModernos      := true;
                        FVerLineasArbol               := false;
                        FVerLineasArbol_Punteadas     := false;
                      end;

    PERFIL_MIXTO   : begin
                        FFormatoSize                  := TFormatoSize.Puntuada;
                        FFormatoIconos                := TFormatoIconos.Mixto;
                        FResaltarColumnaOrden         := true;
                        FUsarColorDiferenciarArchivos := true;
                        FUsarColoresCatalogos         := true;
                        FExtraInfoCatalogos           := true;
                        FAutoOcultarBotonesArbol      := false;
                        FVerBotonesArbolModernos      := false;
                        FVerLineasArbol               := true;
                        FVerLineasArbol_Punteadas     := false;
                      end;
  end;


  DoConfiguracionAplicar();

end;

procedure TForm1.MenuItem_Size_NormalClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FFormatoSize := TFormatoSize(TMenuItem(Pointer(@Sender)^).Tag);
  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm1.MenuItem_Ver_Colores_AtributosClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FUsarColorDiferenciarArchivos := MenuItem_Ver_Colores_Atributos.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm1.PanelInferiorResize(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FLogAlto := PanelInferior.Height;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(FScan) then
  begin
    FScan.free;
  end;

  // Limpia el arbol de directorios
  Arbol.Clear;

  // Limpia la lista de archivos
  Lista.Clear;

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
    {$IFNDEF ESCANEAR_DIRECTORIO_VSCODE_EXTENSIONS}
      FScan.ScanDirAsync(Curdir, @DoOnTerminarScanAsync, '.git;img\iconos');
    {$ELSE}
      FScan.ScanDirAsync('C:\DAM_02\comun\programas\vscode\data\extensions\', @DoOnTerminarScanAsync, '.git;img\iconos');
      //FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync, '.git;img\iconos');
    {$ENDIF ESCANEAR_DIRECTORIO_VSCODE_EXTENSIONS}




  {$ELSE}
  FScan.ScanDirAsync('C:\DAM_02\', @DoOnTerminarScanAsync, '.git;img\iconos');
  {$ENDIF}
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
    Barra_Estado.simpleText := 'Procesando : ' + FScan.Procesando;
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
    SalidaLog.lines.Add(Message_Total_Directorios + '  ' + PuntearNumeracion(FScan.TotalDirectorios));
    SalidaLog.lines.Add(Message_Total_Archivos + '  ' + PuntearNumeracion(FScan.TotalArchivos));

    SalidaLog.lines.Add('------------------------------------------');
    SalidaLog.lines.Add(Message_Total_Size + '  ' + PuntearNumeracion(FScan.TotalSize) + ' (' + ConvertirSizeEx(FScan.TotalSize, ',##', '.0' ) + ')');

    SalidaLog.lines.Add('------------------------------------------');
    SalidaLog.lines.Add(Message_Tiempo_Escaneo + '  ' + MostrarTiempoTranscurrido(FScan.ScanInicio, FScan.ScanFinal));
  finally
    SalidaLog.lines.endUpdate();
  end;


  FGestorDatos.BeginUpdate();
  try
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add('');
    SalidaLog.lines.Add(Message_Guardando);

    Inicio   := now;
    try
      // Guarda los datos del catalogo en la base de datos
      DoGuardarEscaneado(FScan, FGestorDatos);
    finally
      SalidaLog.lines.Add('------------------------------------------');
      SalidaLog.lines.Add(Message_Tiempo_Guardado + '  ' + MostrarTiempoTranscurrido(Inicio, now));
    end;
  finally
    FGestorDatos.EndUpdate();
  end;


  // Cargar Lista
  DoLoadListaCatalogos();
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

    // Guarda una copia del catalogo en la tabla de datos
    Scan.Root.IdPadre        := Scan.Root.Id;
    Scan.Root.IdCatalogo     := Scan.Root.Id;
    Scan.Root.IdRutaCompleta := 0;
    Scan.Root.IdExtension    := 0;
    SistemaGuardado.AddDato(Scan.Root);


    // Guarda los iconos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtensionIcono(TItemExtension(Scan.ListaExtensiones.Items[t]));
    end;

    // Guarda los datos de las Extensiones
    total := Scan.ListaExtensiones.Count - 1;
    for t := 0 to total do
    begin
      SistemaGuardado.AddExtension(TItemExtension(Scan.ListaExtensiones.Items[t]));
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
var
  tiempo : qword;
begin

//  ShowMessage(Message_Directorios);

  tiempo := GetTickCount64();
  DoFormLoadingShow('Espera ...', 'Probando');
  try



    tiempo := GetTickCount64;
    while GetTickCount64 - tiempo < 10000 do
    begin
      Application.ProcessMessages();
      sleep(1);
    end;

    //LanzarThread();
  finally
    DoFormLoadingHide();
  end;



  SetDefaultLang('en');
  SalidaLog.lines.Add('Finalizado el loading');

exit;
  tiempo := GetTickCount64();

  // Cargar Lista
  DoLoadListaCatalogos();

  tiempo := GetTickCount64() - tiempo;

  SalidaLog.lines.Add(Message_Tiempo_Empleado + ' ' + IntToStr(tiempo) + ' ms.');
end;

procedure TForm1.ArbolResize(Sender: TObject);
begin
  // Reajusta el tamaño de la última columna
  DoResizeControlDatos(arbol);

  if FAplicandoConfig then exit;
  FArbolAncho := Arbol.Width;
end;

procedure TForm1.ArbolExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
var
  NodeData: PrListaData;
  Datos   : TItemDato;
begin
  try
    NodeData := Sender.GetNodeData(Node);
    if NodeData <> nil then
    begin
      if NodeData^.IsPrevExpanded then
      begin
        exit;
      end;

      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        DoLoadListaDirectorios(Node, Datos);
        NodeData^.IsPrevExpanded := true;
      end;
    end;
  except
  end;

end;

procedure TForm1.ArbolHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin

end;

procedure TForm1.ArbolKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  Node: PVirtualNode;
begin
  case key of
    VK_RETURN : begin
                  if Shift = [] then
                  begin
                    Node := Arbol.GetFirstSelected();
                    if Node <> nil then
                      Arbol.Expanded[Node] := not Arbol.Expanded[Node];
                  end;
                end;

    VK_DELETE : begin
                  if Shift = [] then
                  begin
                    Node := Arbol.GetFirstSelected();
                    if Node <> nil then
                      DoEliminarItem(Arbol, Node, true);
                  end;
                end;
  end;
end;

procedure TForm1.ArbolChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PrListaData;
  Datos: TItemDato;
begin
  // si el node es el que está seleccionado o es nulo sale
  if (FNodeArbolActual = Node) or (Node = nil)then
    exit;

  FNodeArbolActual := Node;

  // Lo selecciona
  FNodeArbolActual := Node;

  // Marca la ruta actual
  DoMarcarRutaActual(Node);

  try
    NodeData := Sender.GetNodeData(Node);
    if NodeData <> nil then
    begin
      Datos := NodeData^.NodeData;
      if Datos <> nil then
      begin
        if Datos.IdCatalogo <> 0 then
          FCatalogoID := Datos.IdCatalogo
        else
          FCatalogoID := Datos.Id;

        FPadreID    := Datos.Id;

        // Lanza el método de forma asíncrona
        application.QueueAsyncCall(@DoLoadListaArchivosAsync, 0);
      end;
    end;
  except
  end;
end;

procedure TForm1.ArbolDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
var
  PreColor  : TColor;
  PreEstilo : TFontStyles;
  PreSize   : integer;

  NodeData : PrListaData;
  Datos    : TItemCatalogo;
  PosX     : longint;

  IsSelected : boolean;
begin
  if not FExtraInfoCatalogos then
    exit;

  if Node = nil then
    exit;

  if Node^.NodeHeight <> CATALOGO_NODE_ALTURA then
    exit;

  DefaultDraw := false;

  PreColor  := TargetCanvas.Font.Color;
  PreEstilo := TargetCanvas.Font.Style;
  PreSize   := TargetCanvas.Font.Size;

  try
    if Column = 0 then
    begin

      try
        NodeData := Sender.GetNodeData(Node);
        if NodeData <> nil then
        begin
          Datos := TItemCatalogo(NodeData^.NodeData);
          if Datos <> nil then
          begin
            IsSelected := (vsSelected in Node^.States) or (arbol.hotnode = Node);

            // Dibuja el nombre del catalogo
            TargetCanvas.TextOut(CellRect.Left, CellRect.Top + 2, CellText);

            // Dibuja el tamaño del catalogo
            TargetCanvas.Font.Style := [fsBold];
            TargetCanvas.Font.Size := 8;
            if not IsSelected then
              TargetCanvas.Font.Color := clgreen;

            TargetCanvas.TextOut(TargetCanvas.PenPos.X + 5, TargetCanvas.PenPos.Y + 1, ' [' +ConvertirSizeEx(Datos.Size) + ']');

            // Dibuja info extra
            TargetCanvas.Font.Color  := PreColor;
            TargetCanvas.Font.Style := [];
            TargetCanvas.Font.Size := 8;
            if not IsSelected then
              TargetCanvas.Font.Color := clgreen;

            // Dibuja el número de directorio
            TargetCanvas.TextOut(CellRect.Left, TargetCanvas.PenPos.Y + 15, Message_Directorios);
            TargetCanvas.Font.Style := [fsBold];
            TargetCanvas.TextOut(TargetCanvas.PenPos.X + 5, TargetCanvas.PenPos.Y, PuntearNumeracion(Datos.TotalDirectorios));

            // Dibuja el número de Archivos
            TargetCanvas.Font.Style := [];
            TargetCanvas.TextOut(CellRect.Left + 120, TargetCanvas.PenPos.Y, Message_Archivos);
            TargetCanvas.Font.Style := [fsBold];
            TargetCanvas.TextOut(TargetCanvas.PenPos.X + 5, TargetCanvas.PenPos.Y, PuntearNumeracion(Datos.TotalArchivos));
          end;
        end;
      except
      end;

    end;
  finally
    TargetCanvas.Font.Size   := PreSize;
    TargetCanvas.Font.Style  := PreEstilo;
    TargetCanvas.Font.Color  := PreColor;
  end;

end;

procedure TForm1.ArbolBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  NodeData      : PrListaData;
  ColorCatalogo : TColor;
begin

  if not FUsarColoresCatalogos then exit;

  // Dibuja el color de fondo de los catalogos dependiendo del tipo del mismo
    try
      NodeData := arbol.GetNodeData(Node);
      if NodeData <> nil then
      begin
        ColorCatalogo := GetColorCatalogo(integer(NodeData^.TipoCatalogo), NodeData^.TipoNode >= TItemDatoTipo.Root );
        if ColorCatalogo <> clNone then
          ItemColor := ColorCatalogo;
      end;
    except
    end;
end;

procedure TForm1.DoLoadListaArchivos(Padre : TItemDato);
var
  t, total      : integer;
  TotalDir      : integer = 0;
  TotalFile     : integer = 0;
  TotalSize     : QWord   = 0;
  Tiempo        : QWord;
begin
  Tiempo := GetTickCount64();

  Lista.BeginUpdate;
  try
    // Limpia la lista
    Lista.Clear;

    // Libera la asignación de memoria
    DoLiberarListaArchivos();

    // Carga los datos del catalogo
    if assigned(Padre) then
    begin
      // Carga los datos del catalogo
      FListaArchivos := FGestorDatos.GetDatos(Padre);
      if assigned(FListaArchivos) then
      begin
        // Carga los datos del catalogo
        total := FListaArchivos.count -1;
        for t := 0 to total do
        begin
          AddNode(Lista, TItemDato(FListaArchivos{%H-}[t]), nil, false, TItemDatoTipo.NoDefinido, TItemDato(FListaArchivos{%H-}[t]).Tipo);

          if TItemDato(FListaArchivos{%H-}[t]).Tipo = TItemDatoTipo.Directorio then
            inc(TotalDir)
          else
          begin
            inc(TotalFile);
            inc(TotalSize, TItemDato(FListaArchivos{%H-}[t]).Size);
          end;
        end;
      end;
    end;

    // Ordena la lista
    Lista.SortTree(Lista.Header.SortColumn, Lista.Header.SortDirection);

    Tiempo := GetTickCount64() - Tiempo;


    // Muestra los datos de la lista
    //DoEstadisticas(0, inttostr(TotalDir) + ' directorios y ' + inttostr(TotalFile) + ' archivos con '+ inttostr(TotalFile + TotalDir) + ' elementos en total y ocupando ' + ConvertirSizeEx(TotalSize) + ' ( ' + PuntearNumeracion(TotalSize) + ' bytes) - Tiempo empleado :  '+ inttostr(Tiempo) + ' ms');

    DoEstadisticas(0, Format(Message_Estadisticas, [
      PuntearNumeracion(TotalDir),
      PuntearNumeracion(TotalFile),
      PuntearNumeracion(TotalFile + TotalDir),
      ConvertirSizeEx(TotalSize),
      PuntearNumeracion(TotalSize),
      PuntearNumeracion(Tiempo)

    ]));

  finally
    Lista.EndUpdate;
  end;
end;


procedure TForm1.DoLoadListaDirectorios(Node : PVirtualNode; Padre : TItemDato);

  function GetTipoNode(Node : PVirtualNode) : TItemDatoTipo;
  var
    NodeData: PrListaData;
  begin
    result := TItemDatoTipo.NoDefinido;
    try
      NodeData := Arbol.GetNodeData(Node);
      if NodeData <> nil then
      begin
        result := NodeData^.TipoCatalogo;
      end;
    except
    end;
  end;

var
  t, total, inicio         : integer;
  ListaTemporalDirectorios : TArrayItemDato;
begin
  Arbol.BeginUpdate;
  try
      // Carga los datos del catalogo
      inicio := FGestorDatos.GetDirectorios(Padre, FListaDirectorios);
      if inicio > -1 then
      begin
        // Carga los datos del catalogo
        total := FListaDirectorios.count -1;
        for t := inicio to total do
        begin
          AddNode(Arbol, TItemDato(FListaDirectorios{%H-}[t]), Node, TItemDato(FListaDirectorios{%H-}[t]).TieneHijos, GetTipoNode(Node), TItemDato(FListaDirectorios{%H-}[t]).Tipo);
        end;
      end;

    // Ordena el arbol
    Arbol.SortTree(COLUMNA_TIPO, TSortDirection.sdAscending);


  finally
    Arbol.EndUpdate;
  end;
end;


procedure TForm1.DoLoadListaCatalogos();
var
  t, total      : integer;
begin
  Arbol.BeginUpdate;
  try
    // Limpia la lista
    Arbol.Clear;

    // Libera el nodo raiz del arbol
    DoLiberarNodoRootArbol();

    // Libera la asignación de memoria
    DoLiberarListaCatalogos();
    DoLiberarListaDirectorios(false);


    // Crea el nodo raiz del arbol
    DoCrearNodoRootArbol();

    // Carga los datos del catalogo
    FListaCatalogos := FGestorDatos.GetAllCatalogos();
    if assigned(FListaCatalogos) then
    begin
      // Carga los datos del catalogo
      total := FListaCatalogos.count -1;
      for t := 0 to total do
      begin
        // Añade el nodo al arbol
        AddNode(Arbol, TItemDato(FListaCatalogos{%H-}[t]), FNodeArbolRaiz, TItemCatalogo(FListaCatalogos{%H-}[t]).TotalDirectorios > 0, TItemCatalogo(FListaCatalogos{%H-}[t]).Tipo, TItemCatalogo(FListaCatalogos{%H-}[t]).Tipo);
      end;
    end;

    // Actualiza los datos del nodo raiz
    DoUpdateDatosNodoRaiz();

    // Ordena el arbol
    Arbol.SortTree(COLUMNA_TIPO, TSortDirection.sdAscending);

    // Expande el nodo raiz
    Arbol.Expanded[FNodeArbolRaiz] := true;
    Arbol.FocusedNode          := FNodeArbolRaiz;
    Arbol.Selected[FNodeArbolRaiz] := true;

  finally
    Arbol.EndUpdate;
  end;
end;

function TForm1.AddNode(Sender: TBaseVirtualTree; Dato: TItemDato; Padre : PVirtualNode; TieneHijos : boolean; TipoCatalogo : TItemDatoTipo; TipoNode : TItemDatoTipo): boolean;
var
  Node   : PVirtualNode;
  Data   : PrListaData;
begin
  Node             := Sender.AddChild(Padre);

  if FExtraInfoCatalogos and (Dato.Tipo >= TItemDatoTipo.Root) then
    Node^.NodeHeight := CATALOGO_NODE_ALTURA;


  if TieneHijos then
    Node^.States := Node^.States + [vsHasChildren];

  Data               := Sender.GetNodeData(Node);
  Data^.NodeData     := Dato;
  Data^.TipoCatalogo := TipoCatalogo;
  Data^.TipoNode     := TipoNode;
  Result             := True;
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
      CatalogoActual := TItemCatalogo(FListaCatalogos{%H-}[0]);
      if assigned(CatalogoActual) then
      begin
        // Carga los datos del catalogo
        DoLoadListaArchivos(CatalogoActual);
      end;
    end;

  tiempo := GetTickCount64() - tiempo;

  SalidaLog.lines.Add(Message_Tiempo_Empleado + ' ' + IntToStr(tiempo) + ' ms.');
end;


// Usa blend para dibujar la selección o el hover
procedure TForm1.DoDibujarSeleccionModerna(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; NivelBlend : longint = 90);
var
  _Color_Highlight : TColor;
  _Color_BtnShadow : TColor;
begin
  _Color_Highlight := FColor_Highlight;
  _Color_BtnShadow := FColor_BtnShadow;
  NivelBlend       := 75;

  if (vsSelected in Node^.States) and (Sender.HotNode = Node) and Sender.Focused then
    Dibujar_Seleccion_Moderna_Blend_V2(TargetCanvas, ItemRect, FColor_Highlight, _Color_Highlight, NivelBlend - 10, 45)
  else
    if (vsSelected in Node^.States) and Sender.Focused then
      Dibujar_Seleccion_Moderna_Blend_V2(TargetCanvas, ItemRect, FColor_Highlight, _Color_Highlight, NivelBlend, 25)
    else
      if (vsSelected in Node^.States) and (Sender.HotNode = Node) then
        Dibujar_Seleccion_Moderna_Blend_V2(TargetCanvas, ItemRect, FColor_BtnShadow, _Color_BtnShadow, NivelBlend, 45)
    else
      if (vsSelected in Node^.States) then
        Dibujar_Seleccion_Moderna_Blend_V2(TargetCanvas, ItemRect, FColor_BtnShadow, _Color_BtnShadow, NivelBlend, 30)
      else
        if Sender.HotNode = Node then
          Dibujar_Seleccion_Moderna_Blend_V2(TargetCanvas, ItemRect, FColor_Highlight, _Color_Highlight, NivelBlend - 15, 10);
end;

// Aplica color a los textos de los nodos dependiendo de su estado
procedure TForm1.DoSetColor_Texto(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode);
begin
  TargetCanvas.Font.Color := FForzarColorTexto_Normal;

  if (vsSelected in Node^.States) and (Sender.HotNode = Node) and Sender.Focused then
    TargetCanvas.Font.Color := FForzarColorTexto_Seleccionado
  else
    if (vsSelected in Node^.States) and Sender.Focused then
      TargetCanvas.Font.Color := FForzarColorTexto_Seleccionado
    else
      if (vsSelected in Node^.States) and (Sender.HotNode = Node) then
        TargetCanvas.Font.Color := FForzarColorTexto_Seleccionado
        else
          if (vsSelected in Node^.States) then
            TargetCanvas.Font.Color := FForzarColorTexto_Seleccionado
          else
            if Sender.HotNode = Node then
              TargetCanvas.Font.Color := FForzarColorTexto_Hot
end;

// Aplica color a los textos de los nodos dependiendo de los atributos del item
procedure TForm1.DoColorear_Dependiendo_De_Los_Atributos(Data : TItemDato; NCanvas : TCanvas);
begin
  if not FUsarColorDiferenciarArchivos then exit;

  if (Data.Atributos and faSymLink{%H-})= faSymLink{%H-} then
    NCanvas.Font.Color := FColor_SymLink;

  if (((Data.Atributos and faHidden{%H-})= faHidden{%H-})
    or ((Data.Atributos and faSysFile{%H-})= faSysFile{%H-})
    or (Data.Nombre[1] = '.')) then
      NCanvas.Font.Color := FColor_Oculto_Sistema;

  if ((Data.Atributos and faReadOnly)= faReadOnly)  then
    NCanvas.Font.Color := FColor_SoloLectura;
end;

// Navegación por los directorios de la lista de archivos
procedure TForm1.DoNavegarElementoLista();
var
  NodeData: PrListaData;
  Datos   : TItemDato;
begin
  if Lista.SelectedCount <> 1  then
    exit;

  NodeData := Lista.GetNodeData(Lista.GetFirstSelected());
  if NodeData <> nil then
  begin
    Datos := NodeData^.NodeData;
    if Datos <> nil then
    begin
      if Datos.Tipo = TItemDatoTipo.Directorio then
      begin
        FCatalogoID := Datos.IdCatalogo;
        FPadreID    := Datos.Id;

        // Selecciona en el arbol el nodo correspondiente
        DoSincronizarListaArbol();
      end;
    end;
  end;
end;

procedure TForm1.DoLoadListaArchivosAsync({%H-}Data: PtrInt);
begin
  try
    FTempPadre.Id         := FPadreID;
    FTempPadre.IdCatalogo := FCatalogoID;

    // Carga los datos del catalogo
    DoLoadListaArchivos(FTempPadre);
  except
  end;
end;


procedure TForm1.DoLoadListaCatalogosAsync({%H-}Data: PtrInt);
begin
  // Cargar Lista
  DoLoadListaCatalogos();
end;

// Sincroniza el arbol con la vista de la lista de directorios
procedure TForm1.DoSincronizarListaArbol();
var
  NodeData: PrListaData;
  Datos: TItemDato;
  Node: PVirtualNode;
begin
  Node := FNodeArbolActual;
  if node = nil then exit;

  Arbol.Expanded[node] := true;

  Node := Arbol.GetFirst();
  while Node <> nil do
  begin
    try
      //Node^.States := Node^.States - [vsSelected];
      NodeData := arbol.GetNodeData(Node);
      if NodeData <> nil then
      begin
        Datos := NodeData^.NodeData;

        if (Datos <> nil) and (Datos.IdCatalogo = FCatalogoID) and (Datos.Id = FPadreID) then
        begin
(*
          FNodeArbolActual     := node;
          Node^.States         := Node^.States + [vsSelected];
          Arbol.Expanded[node] := true;
          Arbol.FocusedNode    := Node;
          Arbol.ScrollIntoView(Node, true, true);

          // Lanza el método de forma asíncrona
          application.QueueAsyncCall(@DoLoadListaArchivosAsync, 0);
*)

          Arbol.ScrollIntoView(Node, true, true);

          //Node^.States         := Node^.States + [vsSelected];
          Arbol.Expanded[node] := true;
          //Arbol.Selected[Node] := true;
          //Arbol.FocusedNode    := Node;
          ArbolChange(Arbol, node);




          //exit;
        end
      end;
    except
    end;
    Node := Arbol.GetNext(Node);
  end;
end;

// Marca todos los nodos que esté en la ruta seleccionada
procedure TForm1.DoMarcarRutaActual(Node: PVirtualNode);
var
  NodeData: PrListaData;

  procedure AsignarRutaSeleccionada(Nodo: PVirtualNode; valor : boolean);
  var
  NodeData: PrListaData;
  begin
    try
      NodeData := arbol.GetNodeData(Nodo);
      if NodeData <> nil then
      begin
        NodeData^.IsRutaSeleccionada := valor;

        if (FNodeArbolRaiz <> Nodo) and (NodeData^.TipoNode >= TItemDatoTipo.Root) then
        begin
          FCatalogoSeleccionadoItem := TItemCatalogo(NodeData^.NodeData);
          FCatalogoSeleccionadoNodo := Nodo;
        end;

      end;
    except
    end;
  end;


  // Resetea la ruta seleccionada
  procedure ResetRutaSeleccionada();
  var
    Nodo: PVirtualNode;

  begin
    Nodo := Arbol.GetFirst();
    while Nodo <> nil do
    begin
      AsignarRutaSeleccionada(Nodo, false);

      // Elimina el estado de seleccion en caso de no ser el node actual
      if Nodo = Node then
       Nodo^.States := Nodo^.States + [vsSelected]
      else
       Nodo^.States := Nodo^.States - [vsSelected];

      Nodo := Arbol.GetNext(Nodo);
    end;
  end;

var
  Padre : PVirtualNode;

begin
  // Resetea la ruta seleccionada
  ResetRutaSeleccionada();

  // Marca el nodo actual
  AsignarRutaSeleccionada(Node, true);

  // Sale si no hay nodo
  if Node = nil then
    exit;

  // Marca los nodos padres
  Padre := Node^.Parent;
  while Padre <> nil do
  begin
    // Marca el nodo padre
    AsignarRutaSeleccionada(Padre, true);

    // Comprueba si no es el nodo raiz
    if Padre <> Arbol.RootNode then
      Padre := Padre^.Parent
    else
      Padre := nil;
  end;

  // Repinta el arbol
  Arbol.Repaint();
end;

// Crea el nodo raiz del arbol
procedure TForm1.DoCrearNodoRootArbol();
var
  Data           : PrListaData;
begin
  // Crea el nodo raiz
  FNodeArbolRaiz                 := Arbol.AddChild(nil);

  if FExtraInfoCatalogos then
    FNodeArbolRaiz^.NodeHeight   := CATALOGO_NODE_ALTURA;

  FNodeArbolRaizDato            := TItemCatalogo.Create('Catálogos', TItemDatoTipo.Root, now, 0, '', 0, 0);
  FNodeArbolRaizDato.ImageIndex := integer(TItemDatoTipo.Root);
  Data                          := Arbol.GetNodeData(FNodeArbolRaiz);
  Data^.NodeData                := FNodeArbolRaizDato;
  Data^.TipoCatalogo            := TItemDatoTipo.Root;
  Data^.TipoNode                := TItemDatoTipo.Root;


  Arbol.Expanded[FNodeArbolRaiz] := true;
  Arbol.FocusedNode          := FNodeArbolRaiz;
end;

// Libera el nodo raiz del arbol
procedure TForm1.DoLiberarNodoRootArbol();
begin
  if Assigned(FNodeArbolRaiz) then
    Arbol.RemoveFromSelection(FNodeArbolRaiz);

  if Assigned(FNodeArbolRaizDato) then
    FNodeArbolRaizDato.free;
end;

// Muestra Información del directorio actual
procedure TForm1.DoEstadisticas(Columna : longint; Texto : string);
begin
  if Barra_Estado.Panels.Count > 0 then
    Barra_Estado.Panels[Columna].Text := Texto
  else
    Barra_Estado.SimpleText := texto;
end;

// Ajusta el alto dependiendo si se debe o no mostrar la info de los catalogos
procedure Tform1.DoAjustarNodosCatalogos();

  procedure AjustarNodo(Node : PVirtualNode);
  var
    NodeData: PrListaData;
  begin
    try
      NodeData := arbol.GetNodeData(Node);
      if (NodeData <> nil)  and (NodeData^.TipoNode >= TItemDatoTipo.Root) then
      begin
        if not FExtraInfoCatalogos then
          Node^.NodeHeight := Arbol.DefaultNodeHeight
        else
          Node^.NodeHeight := CATALOGO_NODE_ALTURA;
      end;
    except
    end;
  end;

var
  Node : PVirtualNode;
begin
  Node := Arbol.GetFirst();
  while Node <> nil do
    begin
      // ajusta el nodo
      AjustarNodo(Node);

      Node := Arbol.GetNext(Node);
    end;

  // Ajusta el alto del nodo raiz
  AjustarNodo(FNodeArbolRaiz);
end;

// Elimina un item del arbol/lista
function Tform1.DoEliminarItem(Sender: TLazVirtualStringTree; Node: PVirtualNode; IsDesdeArbol : boolean) : boolean;
var
  NodeData : PrListaData;
  total, t : integer;

  NodePadre : PVirtualNode;

begin
  Sender.beginUpdate();
  if not IsDesdeArbol then
    Arbol.beginUpdate();

  try
    try
      NodeData := Sender.GetNodeData(Node);

      if Node = FNodeArbolRaiz then
      begin
        // Se pide confirmación para eliminar todo y se sale si no se confirma
        if not DoConfirmarEliminarTodo() then
          exit;

          DoFormLoadingShow(Message_Espera_Eliminar_Titulo, Message_Espera_Eliminar_Catalogo_All);
          try
            // Elimina el Catalogo de la base de datos
            FGestorDatos.DeleteAllCatalogosAsync();

            // Carga la lista de catalogos
            DoLoadListaCatalogos();

            // Carga la lista de archivos
            DoLoadListaArchivos(nil);
          finally
            DoFormLoadingHide();
          end;
      end
      else
      if (NodeData <> nil)  and (NodeData^.TipoNode >= TItemDatoTipo.Root) then
      begin
        if NodeData^.TipoNode >= TItemDatoTipo.Root then
          begin
            // Se pide confirmación para eliminar y se sale si no se confirma
            if not DoConfirmarEliminarCatalogo(TItemCatalogo(NodeData^.NodeData)) then
              exit;

            DoFormLoadingShow(Message_Espera_Eliminar_Titulo, Message_Espera_Eliminar_Catalogo);
            try
              // Elimina el Catalogo de la base de datos
              FGestorDatos.DeleteCatalogoAsync(TItemCatalogo(NodeData^.NodeData));

              // Elimina el catalogo de la lista
              FListaCatalogos.Remove(NodeData^.NodeData);

              // Libera la información del nodo
              NodeData^.NodeData.free;

              // Elimina el nodo del arbol
              Sender.DeleteNode(Node);

              // Limpia la lista de archivos
              DoLoadListaArchivos(nil);
            finally
              DoFormLoadingHide();
            end;
          end
        end
        else
        begin
          // Se pide confirmación para eliminar y se sale si no se confirma
          if not DoConfirmarEliminarDatos() then
            exit;

          DoFormLoadingShow(Message_Espera_Eliminar_Titulo, Message_Espera_Eliminar_Archivos);
          try
            Node := Sender.GetFirstSelected();
            while Node <> nil do
            begin
              NodeData := Sender.GetNodeData(Node);
              if (NodeData <> nil)  and (NodeData^.TipoNode < TItemDatoTipo.Root) then
              begin
                // Elimina el archivo de la base de datos
                FGestorDatos.DeleteDatoAsync(NodeData^.NodeData);

                // Limpia la lista de archivos
                if Node = FNodeArbolActual then
                  DoLoadListaArchivos(nil);

                if not IsDesdeArbol and (NodeData^.TipoNode = TItemDatoTipo.Directorio) then
                  DoEliminarItemArbol(NodeData^.NodeData);

                NodePadre := Sender.GetPreviousVisible(Node);

                // Elimina el nodo del arbol
                Sender.IsVisible[Node] := false;
              end;

              Node := Sender.GetNextSelected(Node);
            end;
          finally
            DoFormLoadingHide();
          end;

          if NodePadre <> nil then
          begin
            Sender.FocusedNode         := NodePadre;
            Sender.Selected[NodePadre] := true;
          end;

          // Actualiza los datos del nodo al que pertenece
          if FCatalogoSeleccionadoItem <> nil then
          begin
            FGestorDatos.UpdateTotalesCatalogo(FCatalogoSeleccionadoItem);
            if FCatalogoSeleccionadoNodo <> nil then
              if FCatalogoSeleccionadoItem.TotalDirectorios > 0 then
                FCatalogoSeleccionadoNodo^.States := FCatalogoSeleccionadoNodo^.States + [vsHasChildren]
              else
                FCatalogoSeleccionadoNodo^.States := FCatalogoSeleccionadoNodo^.States - [vsHasChildren];
          end;

          // Redibuja el control
          Sender.Repaint();

          if not IsDesdeArbol then
            Arbol.RePaint();

      end;

      // Actualiza los datos del nodo raiz
      DoUpdateDatosNodoRaiz();
    except
    end;
  finally
    Sender.endUpdate();

    if not IsDesdeArbol then
      Arbol.endUpdate();
  end;

end;

// Confirma si se debe eliminar todos los catalogos
function Tform1.DoConfirmarEliminarTodo() : boolean;
begin
  result := IsMessageBoxWarning(Format(Message_Eliminar_All_Catalogos, [1]), Message_Atencion);
  if result then
    result := IsMessageBoxWarning(Format(Message_Eliminar_All_Catalogos, [2]), Message_Atencion);

end;

// Confirma si se debe eliminar el catalogo seleccionado
function Tform1.DoConfirmarEliminarCatalogo(Catalogo : TItemCatalogo) : boolean;
begin
  // Pregunta si realmente quiere borrar los datos
  Result := IsMessageBoxWarning(Message_Eliminar_Catalogo, Message_Atencion);
end;

// Confirma si se debe eliminar los datos seleccionados
function Tform1.DoConfirmarEliminarDatos() : boolean;
begin
  // Pregunta si realmente quiere borrar los datos
  Result := IsMessageBoxWarning(Message_Eliminar_Datos, Message_Atencion);
end;

// Actualiza el nodo raiz con el total de datos
procedure Tform1.DoUpdateDatosNodoRaiz();
var
  total, t : integer;
begin
  if FNodeArbolRaizDato = nil then
    exit;

  // Resetea la información extra del nodo raiz
  FNodeArbolRaizDato.ResetExtraInfo();

  // Carga los datos del catalogo
  if assigned(FListaCatalogos) then
  begin
    // Carga los datos del catalogo
    total := FListaCatalogos.count -1;
    for t := 0 to total do
    begin
      // Añade la información extra del nodo raiz
      FNodeArbolRaizDato.AddExtraInfo(TItemCatalogo(FListaCatalogos{%H-}[t]));
    end;
  end;
end;



// Oculta/Elimina los nodos que encuentre iguales a al Item que se le pasa
procedure Tform1.DoEliminarItemArbol(Item : TItemDato);

  procedure AjustarNodo(Node : PVirtualNode);
  var
    NodeData: PrListaData;
  begin
    try
      NodeData := arbol.GetNodeData(Node);
      if (NodeData <> nil)  and (NodeData^.NodeData <> nil) then
      begin
        if (NodeData^.NodeData.Id = Item.Id) and (NodeData^.NodeData.IdCatalogo = Item.IdCatalogo) then
        begin
          // Elimina el nodo del arbol
          Arbol.IsVisible[Node] := false;
        end;

      end;
    except
    end;
  end;

var
  Node : PVirtualNode;
begin
  Node := Arbol.GetFirst();
  while Node <> nil do
    begin
      // ajusta el nodo
      AjustarNodo(Node);

      Node := Arbol.GetNext(Node);
    end;
end;

// Muestra el formulario de carga
procedure Tform1.DoFormLoadingShow(MensajeTitulo : string; MensajeNormal : string);
begin
  // Desactiva los controles del formulario principal
  DoActivarShowModal(False);

  // Muestra en la statusbar el mensaje
  DoEstadisticas(0, MensajeNormal);

  // Muestra el formulario de carga
  FormLoadingShow(Self, MensajeTitulo, MensajeNormal);
end;

// Oculta el formulario de carga
procedure Tform1.DoFormLoadingHide();
begin
  // Activa los controles del formulario principal
  DoActivarShowModal(true);

  // Oculta el formulario de carga
  FormLoadingHide();
end;

// Activa o desactiva los controles
procedure Tform1.DoActivarShowModal(activar : boolean);
begin
  // Desactiva/Activa los menús
  MenuItemArchivo.Enabled := activar;
  MenuItemVer.Enabled     := activar;
  MenuItemAyuda.Enabled   := activar;

  // Desactiva/Activa el arbol y la lista
  Arbol.Enabled           := activar;
  Lista.Enabled           := activar;

  // Desactiva/Activa los splitters
  Splitter1.Enabled       := activar;
  Splitter2.Enabled       := activar;

  // Desactiva/Activa la toolbar y el panel inferior
  ToolBar1.Enabled        := activar;
  PanelInferior.Enabled   := activar;
end;

end.
