(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-05 21:58:48
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-30 09:18:27
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

{$i ../../DirectivasCompilacion.inc}

unit UnidadPrincipal;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  LCLType
  , lclintf
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
  , Control_Cajon_Busqueda
  , InterfaceConectorDatos
  , ItemBaseDatos
  , ItemDato, ItemCatalogo, VirtualTreesExtras, ImgList
  , FrameBusqueda
  , GestorExportacion
;


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



  { TForm_Principal }

  TForm_Principal = class(TForm)
    Arbol: TLazVirtualStringTree;
    Frame_Busqueda1: TFrame_Busqueda;
    ImageListArchivos32: TImageList;
    ImageListArchivos: TImageList;
    ImageListToolbar: TImageList;
    Lista: TLazVirtualStringTree;
    MenuItemEnglish: TMenuItem;
    MenuItemSpahish: TMenuItem;
    MenuItemIdioma: TMenuItem;
    MenuItemBarraEstado: TMenuItem;
    MenuItemBarraHerramientas: TMenuItem;
    MenuItemBarraBusqueda: TMenuItem;
    MenuItemMostrar: TMenuItem;
    MenuItemExportarPopUpTxt: TMenuItem;
    MenuItemExportarPopUpHtml: TMenuItem;
    Exportar: TMenuItem;
    MenuItemExportarTxt: TMenuItem;
    MenuItemExportarHtml: TMenuItem;
    MenuItemExportar: TMenuItem;
    MenuPopUpItemPropiedades: TMenuItem;
    MenuPopUpItemEliminar: TMenuItem;
    MenuPopUpItemAgregarCatalogo: TMenuItem;
    MenuPopUpItemNuevaBaseDatos: TMenuItem;
    MenuItemBusquedaAvanzada: TMenuItem;
    MenuItemPropiedades: TMenuItem;
    MenuItemEliminar: TMenuItem;
    MenuItemEditar: TMenuItem;
    NotebookPanelIzquiedo: TNotebook;
    PageBusquedaAvanzada: TPage;
    PageNavegar: TPage;
    PanelIzquierdo: TPanel;
    PanelSuperior: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Separator10: TMenuItem;
    MenuItemNuevaBaseDatos: TMenuItem;
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
    MenuItemAgregarCatalogo: TMenuItem;
    MenuItemAyuda: TMenuItem;
    MenuItemAcercaDe: TMenuItem;
    MenuItemSalir: TMenuItem;
    PanelPrincipal: TPanel;
    Separator1: TMenuItem;
    Separator11: TMenuItem;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator14: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TMenuItem;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator19: TMenuItem;
    Separator2: TMenuItem;
    Separator20: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    Splitter1: TSplitter;
    Barra_Estado: TStatusBar;
    Timer_UpdateUI: TTimer;
    Barra_Herramientas: TToolBar;
    ToolButtonBusquedaAvanzada: TToolButton;
    ToolButtonAgregarCatalogo: TToolButton;
    ToolButtonPropiedades: TToolButton;
    ToolButtonEliminar: TToolButton;
    procedure ArbolBeforeItemErase(Sender: TBaseVirtualTree;
      {%H-}TargetCanvas: TCanvas; Node: PVirtualNode; const {%H-}ItemRect: TRect;
      var ItemColor: TColor; var {%H-}EraseAction: TItemEraseAction);
    procedure ArbolChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ArbolDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure ArbolExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var {%H-}Allowed: Boolean);
    procedure ArbolGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure ArbolKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure MenuItemAgregarCatalogoClick(Sender: TObject);
    procedure MenuItemAcercaDeClick(Sender: TObject);
    procedure MenuItemBarraBusquedaClick(Sender: TObject);
    procedure MenuItemBarraEstadoClick(Sender: TObject);
    procedure MenuItemBarraHerramientasClick(Sender: TObject);
    procedure MenuItemBusquedaAvanzadaClick(Sender: TObject);
    procedure MenuItemEliminarClick(Sender: TObject);
    procedure MenuItemEnglishClick(Sender: TObject);
    procedure MenuItemExportarHtmlClick(Sender: TObject);
    procedure MenuItemExportarTxtClick(Sender: TObject);
    procedure MenuItemNuevaBaseDatosClick(Sender: TObject);
    procedure MenuItemPropiedadesClick(Sender: TObject);
    procedure MenuItemSalirClick(Sender: TObject);
    procedure MenuItemSpahishClick(Sender: TObject);
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
    procedure PanelIzquierdoResize(Sender: TObject);
    procedure Timer_UpdateUITimer(Sender: TObject);
    procedure ToolButtonBusquedaAvanzadaClick(Sender: TObject);
  private
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
    FPanelBusqueda   : TPanelBusqueda;
  protected
    procedure DoOnTerminarScanAsync();
    procedure DoLoadListaArchivos(Padre : TItemDato; Query : PCommandBusqueda = nil);
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

    // Precarga los datos de la lista de archivos
    procedure DoPreloadDatosAsync({%H-}Data: PtrInt);

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

    // Para gestionar las acciones de los menús sobre los items
    procedure DoAccionItem(Sender: TObject; isEliminar : boolean);

    // Elimina un item del arbol/lista
    function DoEliminarItem(Sender: TLazVirtualStringTree; Node: PVirtualNode; IsDesdeArbol : boolean; Forzar : boolean = false) : boolean;

    // Muestra las propiedades de un item del arbol/lista
    function DoPropiedadesItem(Sender: TLazVirtualStringTree; Node: PVirtualNode) : boolean;

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

    // Muestra el formulario de carga
    procedure DoFormLoadingShow(MensajeTitulo : string; MensajeNormal : string);

    // Oculta el formulario de carga
    procedure DoFormLoadingHide();

    // Activa o desactiva los controles
    procedure DoActivarShowModal(activar : boolean);

    // Lanza la búsqueda avanzada
    procedure DoBusquedaAvanzada();

    // Lanza la acción agregar un nuevo catalogo
    procedure DoAddNuevoCatalogo();


    // Busca el texto en la lista de archivos
    procedure DoBuscarTexto(const Buscar : string);

    // Al recibir cambios del campo de búsqueda realiza la búsqueda
    procedure DoEditBuscarChange(Sender: TObject);

    // Expo
    procedure DoExportar(Formato : TFormatoExportacion);

    // Inicializar el titulo del formulario
    procedure SetTituloVentana(extra : string = '');

    // Marca todos los nodos que esté en la ruta seleccionada
    function GetRutaFromNodo(Node: PVirtualNode) : RawByteString;

    // Al cambiar de nodo actualiza el titulo de la ventana
    procedure DoArbolChangeTitle(Node: PVirtualNode);

    // Cambia el idioma de la aplicación
    procedure SetIdioma(Idioma : string);

  public

  end;


var
  Form_Principal: TForm_Principal;


implementation

uses
  GraphType
  ,  appinfo
  , Control_About
  , Configuracion
  , OrdenarLista
  , Utilidades
  , ConectorDatos
  , GestorExtensiones, AppString, UnidadLoading, UnidadPropiedades, UnidadAddCatalogo
  ;

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

{ TForm_Principal }
procedure TForm_Principal.FormCreate(Sender: TObject);
begin
  // Inicializar el titulo del formulario
  SetTituloVentana('');

  // Opciones avanzadas
  ActivarArchivoConfig('cameex_config.ini', false, true, false, NOMBRE_PROGRAMA);
  ActivarGuardadoPosicion;

  // Inicializar el Logger
  LogCreate(IncludeTrailingBackslash(DirectorioConfig) + NOMBRE_PROGRAMA + '.log' , TLogLevel.all, true);

  // Agrega al logger el inicio del programa
  LogAdd(TLogLevel.info, 'Iniciando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');


  {$IFDEF WINDOWS}
   // Desactivamos los errores por ejemplo al intentar entrar en una unidad cdrom sin disco
   SetErrorMode(SEM_FailCriticalErrors);
  {$ENDIF WINDOWS}

  // Inicializar el Gestor de Datos
  FGestorDatos := TConectorDatos.Create;
  FGestorDatos.Iniciar(Curdir, DirectorioConfig);

  // Inicializar el buscador para realizar las busquedas
  Frame_Busqueda1.OnBusquedaDatos    := @DoLoadListaArchivos;
  Frame_Busqueda1.OnActivarShowModal := @DoActivarShowModal;


  // Necesarío para que funcione la navegación por la lista de archivos
  FTempPadre    := TItemDato.create('', TItemDatoTipo.Directorio, now, 0);

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

  // Crea e Inicializa el panel de búsqueda
  FPanelBusqueda                        := TPanelBusqueda.Create(TComponent(Pointer(@PanelSuperior)^));
  FPanelBusqueda.Parent                 := PanelSuperior;
  FPanelBusqueda.Top                    := 5;
  FPanelBusqueda.Left                   := 5;
  FPanelBusqueda.Width                  := 350;
  FPanelBusqueda.Align                  := alRight;
  FPanelBusqueda.OnChange               := @DoEditBuscarChange;
  FPanelBusqueda.EditBusqueda.TabOrder  := 2;
  FPanelBusqueda.Visible                := true;



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

procedure TForm_Principal.FormDestroy(Sender: TObject);
begin

  // Marca el nodo raiz del arbol como nulo puesto que ha sido eliminado en la llamada Arbol.clear;
  FNodeArbolRaiz := nil;

  // Libera el nodo root del arbol
  DoLiberarNodoRootArbol();

  // Finalizar los objectos necesarios para la navegación por la lista de archivos
  FTempPadre.free;

  // Libera la asignación de memoria
  DoLiberarListaArchivos();

  // Finalizar la lista de catalogos
  DoLiberarListaCatalogos();

  // Finalizar la lista de directorios
  DoLiberarListaDirectorios(true);

  // Finalizar el Gestor de Datos
  FGestorDatos.Finalizar();

  // Finalizar el Panel de Busqueda
  FPanelBusqueda.Free;

  // Guarda la configuración del programa
  DoConfiguracionSave();

  LogAdd(TLogLevel.info, 'Finalizando ' + NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA + ' (' + FECHA_PROGRAMA + ')');
end;

procedure TForm_Principal.ListaAfterItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  DoDibujarSeleccionModerna(Sender, TargetCanvas, Node, ItemRect);
end;

procedure TForm_Principal.ListaBeforeCellPaint(Sender: TBaseVirtualTree;
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
procedure TForm_Principal.DoConfiguracionLoad();
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

  // Opciones de Mostar/Ocultar
  FVerBarraBusqueda         := ArchivoConfiguracion.ReadBool('Config', 'VerBarraBusqueda', FVerBarraBusqueda);
  FVerBarraHerramientas     := ArchivoConfiguracion.ReadBool('Config', 'VerBarraHerramientas', FVerBarraHerramientas);
  FVerBarraEstado           := ArchivoConfiguracion.ReadBool('Config', 'VerBarraEstado', FVerBarraEstado);

  // Idioma
  FIdioma                   := ArchivoConfiguracion.ReadString('Config', 'Idioma', FIdioma);

  // Si el idioma no se encuentra entre los idiomas disponibles, se pone el idioma por defecto
  if (FIdioma <> 'es') and (FIdioma <> 'en') then
    FIdioma := 'es';

  // Carga la configuración para diferenciar los archivos por colores
  FUsarColorDiferenciarArchivos:= ArchivoConfiguracion.ReadBool('Config', 'UsarColorDiferenciarArchivos', FUsarColorDiferenciarArchivos);

end;

// Guarda la configuración del programa
procedure TForm_Principal.DoConfiguracionSave();
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

  // Opciones de Mostar/Ocultar
  ArchivoConfiguracion.WriteBool('Config', 'VerBarraBusqueda', FVerBarraBusqueda);
  ArchivoConfiguracion.WriteBool('Config', 'VerBarraHerramientas', FVerBarraHerramientas);
  ArchivoConfiguracion.WriteBool('Config', 'VerBarraEstado', FVerBarraEstado);

  // Idioma
  ArchivoConfiguracion.WriteString('Config', 'Idioma', FIdioma);


  // Guarda la configuración para diferenciar los archivos por colores
  ArchivoConfiguracion.WriteBool('Config', 'UsarColorDiferenciarArchivos', FUsarColorDiferenciarArchivos);

  // Guarda la configuración en el archivo
  ArchivoConfiguracion.UpdateFile;

end;

// Aplica la configuración del programa
procedure TForm_Principal.DoConfiguracionAplicar();
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
    PanelIzquierdo.Width := FArbolAncho;

    // Aplica la configuración de las Dimensiones del Log
    //PanelInferior.Height := FLogAlto;

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

    // Opciones de Mostar/Ocultar
    FPanelBusqueda.Visible            := FVerBarraBusqueda;
    PanelSuperior.Visible             := FVerBarraHerramientas;
    Barra_Estado.Visible              := FVerBarraEstado;

    // Solo es usable cuando se vea la barra de herramientas
    MenuItemBarraBusqueda.Enabled     := FVerBarraHerramientas;

    // Idioma
    SetIdioma(FIdioma);

    // Aplica la config a los menus
    MenuItem_Arbol_Catalogos_AutoOculta_Botones.Checked   := FAutoOcultarBotonesArbol;
    MenuItem_Arbol_Catalogos_Ver_Lineas_Punteadas.Checked := FVerLineasArbol_Punteadas;
    MenuItem_Arbol_Catalogo_Botones_Modernos.Checked      := FVerBotonesArbolModernos;
    MenuItem_Arbol_Catalogo_Ver_Lineas.Checked            := FVerLineasArbol;
    MenuItem_Ver_Colores_Atributos.Checked                := FUsarColorDiferenciarArchivos;
    MenuItemBarraBusqueda.Checked                         := FVerBarraBusqueda;
    MenuItemBarraHerramientas.Checked                     := FVerBarraHerramientas;
    MenuItemBarraEstado.Checked                           := FVerBarraEstado;

    MenuItemEnglish.Checked                               := FIdioma = 'en';
    MenuItemSpahish.Checked                               := FIdioma = 'es';

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
procedure TForm_Principal.DoHeader_Iniciar(MenuHeader : boolean = true; CargarConfig : boolean = false);
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
procedure TForm_Principal.DoHeader_MenuColumnasClick(Sender: TObject);
begin
  if TMenuItem(Pointer(@Sender)^).Tag = -1 then exit;
  TMenuItem(Pointer(@Sender)^).Checked := not TMenuItem(Pointer(@Sender)^).Checked;
  DoHeader_IsVisible(TMenuItem(Pointer(@Sender)^).Tag, TMenuItem(Pointer(@Sender)^).Checked);
end;

// Pone una columna como visible o no
procedure TForm_Principal.DoHeader_IsVisible(Id : integer; IsHeaderVisible : Boolean);
begin
  FConfiguracionColumnas.Datos[Id].IsVisible := IsHeaderVisible;
  DoHeader_Update;
end;

// Actualiza el header de la lista de archivos
procedure TForm_Principal.DoHeader_Update;
begin
  HeaderVirtualTrees_Set(Lista, FConfiguracionColumnas);
end;

// Compara dos nodos de la lista de archivos
procedure TForm_Principal.ListaCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
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

procedure TForm_Principal.ListaDblClick(Sender: TObject);
begin
  DoNavegarElementoLista();
end;

// Obtiene el index de la imagen a usar el el nodo
procedure TForm_Principal.ListaGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
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
          ImageIndex  := GetImageIndexByItemDato(Datos);
          Ghosted     := (((Datos.Atributos and faHidden{%H-})= faHidden{%H-}) or (Datos.Nombre[1] = '.'));
        end;
      end;
    end;
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

// Devuelve la ruta de un item
function TForm_Principal.GetRutaFromItem(Item: TItemDato) : RawByteString;
begin
  Result := '';
  if Item <> nil then
  begin
    if Item.Ruta = '' then
      Item.Ruta := FGestorDatos.GetRutaCompleta(Item);

    Result := Item.Ruta;
  end;
end;

procedure TForm_Principal.ListaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
          COLUMNA_TIPO      : begin
                                if Datos.Descripcion = '' then
                                  Datos.Descripcion := GetExtensionDescripcionById(Datos.IdExtension);

                                CellText := Datos.Descripcion;
                              end;
          COLUMNA_FECHA     : DateTimeToString(CellText, TipoHora, Datos.Fecha);
          COLUMNA_ATRIBUTOS : CellText := AtributosToStr(Datos.Atributos, false);
          COLUMNA_RUTA      : CellText := GetRutaFromItem(Datos);
        end;
      end;
    end;
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

procedure TForm_Principal.ListaHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
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

procedure TForm_Principal.ListaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TForm_Principal.ListaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

procedure TForm_Principal.ListaResize(Sender: TObject);
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

procedure TForm_Principal.MenuItemAgregarCatalogoClick(Sender: TObject);
begin
  // Lanza la acción agregar un nuevo catalogo
  DoAddNuevoCatalogo();
end;

// Reajusta el tamaño de la última columna
procedure TForm_Principal.DoResizeControlDatos(Sender: TLazVirtualStringTree);
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


procedure TForm_Principal.DoLiberarListaArchivos();
begin
  // Libera la asignación de memoria
  if assigned(FListaArchivos) then
  begin
    FListaArchivos.clear;
    FListaArchivos.free;
    FListaArchivos := nil;
  end;
end;

procedure TForm_Principal.DoLiberarListaCatalogos();
begin
  // Libera la asignación de memoria
  if assigned(FListaCatalogos) then
  begin
    FListaCatalogos.clear;
    FListaCatalogos.free;
    FListaCatalogos := nil;
  end;
end;


procedure TForm_Principal.DoLiberarListaDirectorios(Full: boolean);
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

procedure TForm_Principal.MenuItemAcercaDeClick(Sender: TObject);
begin
  Mostrar_Acerca_de(NOMBRE_PROGRAMA, VERSION_PROGRAMA, FECHA_PROGRAMA, NOMBRE_AUTOR, 110, APP_WEB, AUTOR_EMAIL);
end;

procedure TForm_Principal.MenuItemBarraBusquedaClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerBarraBusqueda := MenuItemBarraBusqueda.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItemBarraEstadoClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerBarraEstado := MenuItemBarraEstado.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItemBarraHerramientasClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerBarraHerramientas := MenuItemBarraHerramientas.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItemBusquedaAvanzadaClick(Sender: TObject);
begin
  // Lanza la búsqueda avanzada
  DoBusquedaAvanzada();
end;

procedure TForm_Principal.MenuItemEliminarClick(Sender: TObject);
begin
  // Lanza la accion sobre el item seleccionado
  DoAccionItem(Sender, true);
end;

procedure TForm_Principal.MenuItemExportarHtmlClick(Sender: TObject);
begin
  DoExportar(TFormatoExportacion.feHTML);
end;

procedure TForm_Principal.MenuItemExportarTxtClick(Sender: TObject);
begin
  DoExportar(TFormatoExportacion.feTXT);
end;

procedure TForm_Principal.MenuItemNuevaBaseDatosClick(Sender: TObject);
begin
    DoEliminarItem(Arbol, FNodeArbolRaiz, true, true);
end;

procedure TForm_Principal.MenuItemPropiedadesClick(Sender: TObject);
begin
  // Lanza la accion sobre el item seleccionado
  DoAccionItem(Sender, false);
end;

procedure TForm_Principal.MenuItemSalirClick(Sender: TObject);
begin
  close;
end;

procedure TForm_Principal.MenuItem_Arbol_Catalogos_AutoOculta_BotonesClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FAutoOcultarBotonesArbol         := MenuItem_Arbol_Catalogos_AutoOculta_Botones.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItem_Arbol_Catalogos_Ver_Lineas_PunteadasClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerLineasArbol_Punteadas := MenuItem_Arbol_Catalogos_Ver_Lineas_Punteadas.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItem_Arbol_Catalogo_Botones_ModernosClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerBotonesArbolModernos     := MenuItem_Arbol_Catalogo_Botones_Modernos.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItem_Arbol_Catalogo_Ver_LineasClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FVerLineasArbol          := MenuItem_Arbol_Catalogo_Ver_Lineas.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItem_Catalogos_colorClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FUsarColoresCatalogos := MenuItem_Catalogos_color.Checked;

  DoConfiguracionAplicar();

  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm_Principal.MenuItem_Catalogos_Mostrar_Info_extraClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FExtraInfoCatalogos := MenuItem_Catalogos_Mostrar_Info_extra.Checked;

  DoConfiguracionAplicar();

  Arbol.Repaint;
end;

procedure TForm_Principal.MenuItem_Iconos_PorDefectoClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FFormatoIconos := TFormatoIconos(TMenuItem(Pointer(@Sender)^).Tag);
  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm_Principal.MenuItem_Lista_de_Archivos_Resaltar_Columna_OrdenClick(
  Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FResaltarColumnaOrden := MenuItem_Lista_de_Archivos_Resaltar_Columna_Orden.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.MenuItem_Perfil_MixtoClick(Sender: TObject);
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
                        FVerLineasArbol_Punteadas     := true;
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
                        FVerLineasArbol_Punteadas     := true;
                      end;
  end;


  DoConfiguracionAplicar();

end;

procedure TForm_Principal.MenuItem_Size_NormalClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FFormatoSize := TFormatoSize(TMenuItem(Pointer(@Sender)^).Tag);
  Lista.Refresh;
  Arbol.Refresh;
end;

procedure TForm_Principal.MenuItem_Ver_Colores_AtributosClick(Sender: TObject);
begin
  if FAplicandoConfig then exit;
  FUsarColorDiferenciarArchivos := MenuItem_Ver_Colores_Atributos.Checked;

  DoConfiguracionAplicar();
end;

procedure TForm_Principal.PanelIzquierdoResize(Sender: TObject);
begin
  // Reajusta el tamaño de la última columna
  DoResizeControlDatos(arbol);

  if FAplicandoConfig then exit;
  FArbolAncho := PanelIzquierdo.Width;
end;

procedure TForm_Principal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Desactiva el timer de actualización de la UI
  Timer_UpdateUI.Enabled := False;

  // Limpia el arbol de directorios
  Arbol.Clear;

  // Limpia la lista de archivos
  Lista.Clear;

end;


procedure TForm_Principal.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Desactiva el timer de actualización de la UI
  Timer_UpdateUI.Enabled := False;
end;

procedure TForm_Principal.Timer_UpdateUITimer(Sender: TObject);

var
  isArbolSelecionado : boolean = false;
  Selecionados : integer;

  // Determina el total de items selecionados en el listado
  function GetTotalSeleccionados(Listado : TLazVirtualStringTree) : integer;
  begin
    // Determina el total de items selecionados
    Result := Listado.SelectedCount;

    // Si el arbol esta selecionado y el nodo raiz esta selecionado
    if (Result = 1) and isArbolSelecionado and Arbol.Selected[FNodeArbolRaiz] then
      Result := 0;
  end;


begin
  // Iniciliza el total de items selecionados
  Selecionados := 0;

  // Si el control activo es el arbol o la lista
  if (ActiveControl = Arbol) or (ActiveControl = Lista) then
  begin
    // Determina si el arbol esta selecionado
    isArbolSelecionado :=  ActiveControl = arbol;

    // Determina el total de items selecionados
    Selecionados := GetTotalSeleccionados(TLazVirtualStringTree(ActiveControl));
  end;

  // Si el control activo es el arbol muestra los siguientes MenuItems
  MenuPopUpItemNuevaBaseDatos.Visible  := isArbolSelecionado;
  Separator13.Visible                  := isArbolSelecionado;
  MenuPopUpItemAgregarCatalogo.Visible := isArbolSelecionado;

  // Si hay itemns seleccionados activa los siguiente MenuItems
  MenuItemEliminar.Enabled      := Selecionados > 0;
  ToolButtonEliminar.Enabled    := MenuItemEliminar.Enabled;
  MenuPopUpItemEliminar.Enabled := MenuItemEliminar.Enabled;

  // Si hay un itemn seleccionado activa los siguiente MenuItems
  MenuItemPropiedades.Enabled      := Selecionados = 1;
  ToolButtonPropiedades.Enabled    := MenuItemPropiedades.Enabled;
  MenuPopUpItemPropiedades.Enabled := MenuItemPropiedades.Enabled;

  // Menus de exportación
  Separator17.Visible               := isArbolSelecionado;
  MenuItemExportarHtml.Enabled      := Lista.SelectedCount > 0;
  MenuItemExportarTxt.Enabled       := Lista.SelectedCount > 0;
  MenuItemExportarPopUpHtml.Enabled := Lista.SelectedCount > 0;
  MenuItemExportarPopUpTxt.Enabled  := Lista.SelectedCount > 0;

end;

procedure TForm_Principal.ToolButtonBusquedaAvanzadaClick(Sender: TObject);
begin
    NotebookPanelIzquiedo.PageIndex := integer(ToolButtonBusquedaAvanzada.Down);

    // Titulo de la ventana
    if NotebookPanelIzquiedo.PageIndex > 0 then
      SetTituloVentana(Message_Asistente_Search)
    else
      DoArbolChangeTitle(Arbol.GetFirstSelected());
end;

procedure TForm_Principal.DoOnTerminarScanAsync();
begin
  //
end;


procedure TForm_Principal.ArbolExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
var
  NodeData: PrListaData;
  Datos   : TItemDato;
begin

  // Si el node que se está expandiendo es el nodo raiz, sale
  if Node = FNodeArbolRaiz then exit;

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
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;

end;

procedure TForm_Principal.ArbolGetImageIndexEx(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList
  );

begin

  if FExtraInfoCatalogos and ((Node^.Parent = Sender.RootNode) or (Node^.Parent = Self.FNodeArbolRaiz)) then
   ImageList := ImageListArchivos32
  else
   ImageList := ImageListArchivos;


  ListaGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;

procedure TForm_Principal.ArbolKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
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

procedure TForm_Principal.ArbolChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
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

  // Pone el titulo del formulario
  DoArbolChangeTitle(Node);

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
        // Carga la lista con el directorio actual
        if FNodeArbolRaiz <> node then
          DoLoadListaArchivosAsync(0);
      end;
    end;
  except
    on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

procedure TForm_Principal.ArbolDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
var
  PreColor  : TColor;
  PreEstilo : TFontStyles;
  PreSize   : integer;
  NodeData  : PrListaData;
  Datos     : TItemCatalogo;
  IsSelected: boolean;
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
          on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
      end;

    end;
  finally
    TargetCanvas.Font.Size   := PreSize;
    TargetCanvas.Font.Style  := PreEstilo;
    TargetCanvas.Font.Color  := PreColor;
  end;

end;

procedure TForm_Principal.ArbolBeforeItemErase(Sender: TBaseVirtualTree;
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
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
end;

procedure TForm_Principal.DoLoadListaArchivos(Padre : TItemDato; Query : PCommandBusqueda = nil);
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
    if assigned(Padre) or (Query <> nil) then
    begin
      if Query <> nil then
      begin
        // Realiza la búsqueda
        FListaArchivos := FGestorDatos.GetBusquedaDatos(Query^);
      end
      else
      begin
        // Carga los datos del catalogo
        FListaArchivos := FGestorDatos.GetDatos(Padre);
      end;

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


procedure TForm_Principal.DoLoadListaDirectorios(Node : PVirtualNode; Padre : TItemDato);

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
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
  end;

var
  t, total, inicio         : integer;
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


procedure TForm_Principal.DoLoadListaCatalogos();
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


    // Libera la lista de catalogos en el frame de busqueda
    Frame_Busqueda1.ClearListaCatalogos();

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

        // Añade el catalogo a la lista de catalogos del frame de busqueda
        Frame_Busqueda1.AddToListaCatalogos(TItemCatalogo(FListaCatalogos{%H-}[t]));
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

function TForm_Principal.AddNode(Sender: TBaseVirtualTree; Dato: TItemDato; Padre : PVirtualNode; TieneHijos : boolean; TipoCatalogo : TItemDatoTipo; TipoNode : TItemDatoTipo): boolean;
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


// Usa blend para dibujar la selección o el hover
procedure TForm_Principal.DoDibujarSeleccionModerna(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; NivelBlend : longint = 90);
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
procedure TForm_Principal.DoSetColor_Texto(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode);
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
procedure TForm_Principal.DoColorear_Dependiendo_De_Los_Atributos(Data : TItemDato; NCanvas : TCanvas);
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
procedure TForm_Principal.DoNavegarElementoLista();
var
  NodeData: PrListaData;
  Datos   : TItemDato;
begin
  // si no se está en el arbol de directorios, sale
  if NotebookPanelIzquiedo.PageIndex <> 0 then
    exit;

  // si no hay nada selecionado en la lista de archivos, sale
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

procedure TForm_Principal.DoLoadListaArchivosAsync({%H-}Data: PtrInt);
begin
  try
    FTempPadre.Id         := FPadreID;
    FTempPadre.IdCatalogo := FCatalogoID;

    // Carga los datos del catalogo
    DoLoadListaArchivos(FTempPadre);
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;


procedure TForm_Principal.DoLoadListaCatalogosAsync({%H-}Data: PtrInt);
begin
  // Cargar Lista
  DoLoadListaCatalogos();

  // Lanza el método de forma asíncrona para pre cargar los datos
  application.QueueAsyncCall(@DoPreloadDatosAsync, 0);
end;

// Precarga los datos de la lista de archivos
procedure TForm_Principal.DoPreloadDatosAsync({%H-}Data: PtrInt);
begin
  FTempPadre.Id         := 0;
  FTempPadre.IdCatalogo := 0;

  // Carga los datos del catalogo
  FListaArchivos := FGestorDatos.GetDatos(FTempPadre);
end;



// Sincroniza el arbol con la vista de la lista de directorios
procedure TForm_Principal.DoSincronizarListaArbol();
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
          Arbol.ScrollIntoView(Node, true, true);
          Arbol.Expanded[node] := true;
          ArbolChange(Arbol, node);
        end
      end;
    except
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
    Node := Arbol.GetNext(Node);
  end;
end;

// Marca todos los nodos que esté en la ruta seleccionada
procedure TForm_Principal.DoMarcarRutaActual(Node: PVirtualNode);

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
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
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
procedure TForm_Principal.DoCrearNodoRootArbol();
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
procedure TForm_Principal.DoLiberarNodoRootArbol();
begin
  if Assigned(FNodeArbolRaiz) then
    Arbol.RemoveFromSelection(FNodeArbolRaiz);

  if Assigned(FNodeArbolRaizDato) then
    FNodeArbolRaizDato.free;
end;

// Muestra Información del directorio actual
procedure TForm_Principal.DoEstadisticas(Columna : longint; Texto : string);
begin
  if Barra_Estado.Panels.Count > 0 then
    Barra_Estado.Panels[Columna].Text := Texto
  else
    Barra_Estado.SimpleText := texto;

  // Repinta la barra de estado
  application.ProcessMessages;
end;

// Ajusta el alto dependiendo si se debe o no mostrar la info de los catalogos
procedure TForm_Principal.DoAjustarNodosCatalogos();

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
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
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
function TForm_Principal.DoEliminarItem(Sender: TLazVirtualStringTree; Node: PVirtualNode; IsDesdeArbol : boolean; Forzar : boolean = false) : boolean;
var
  NodeData  : PrListaData;
  NodePadre : PVirtualNode;

begin
  Sender.beginUpdate();
  if not IsDesdeArbol then
    Arbol.beginUpdate();

  try
    try
      if (Node = FNodeArbolRaiz) and not Forzar then
        exit;


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

              {$IFNDEF NODELETEDATOS}
                // Elimina el archivo de la base de datos
                FGestorDatos.DeleteDatoAsync(NodeData^.NodeData);
              {$ELSE}
                sleep(500);
              {$ENDIF NODELETEDATOS}

                // Limpia la lista de archivos
                if Node = FNodeArbolActual then
                  DoLoadListaArchivos(nil);

                if not IsDesdeArbol and (NodeData^.TipoNode = TItemDatoTipo.Directorio) then
                  DoEliminarItemArbol(NodeData^.NodeData);

                NodePadre := Sender.GetPreviousVisible(Node);

                // Elimina el nodo de la lista
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
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
  finally
    Sender.endUpdate();

    if not IsDesdeArbol then
      Arbol.endUpdate();
  end;

end;


// Muestra las propiedades de un item del arbol/lista
function TForm_Principal.DoPropiedadesItem(Sender: TLazVirtualStringTree; Node: PVirtualNode) : boolean;
var
  Temp_Form_Propiedades : TForm_Propiedades;
  NodeData: PrListaData;
begin
  try
    NodeData := Sender.GetNodeData(Node);
    if (NodeData <> nil) and (NodeData^.NodeData <> nil) then
    begin
      GetRutaFromItem(NodeData^.NodeData);
      Temp_Form_Propiedades                := TForm_Propiedades.Create(TComponent(Pointer(@Self)^));
      Temp_Form_Propiedades.Mostrar_Propiedades(NodeData^.NodeData, FGestorDatos);
      Temp_Form_Propiedades.Show;
    end;
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;





// Confirma si se debe eliminar todos los catalogos
function TForm_Principal.DoConfirmarEliminarTodo() : boolean;
begin
  result := IsMessageBoxWarning(Format(Message_Eliminar_All_Catalogos, [1]), Message_Atencion);
  if result then
    result := IsMessageBoxWarning(Format(Message_Eliminar_All_Catalogos, [2]), Message_Atencion);

end;

// Confirma si se debe eliminar el catalogo seleccionado
function TForm_Principal.DoConfirmarEliminarCatalogo(Catalogo : TItemCatalogo) : boolean;
begin
  // Pregunta si realmente quiere borrar los datos
  Result := IsMessageBoxWarning(Format(Message_Eliminar_Catalogo, [Catalogo.Nombre]), Message_Atencion);
end;

// Confirma si se debe eliminar los datos seleccionados
function TForm_Principal.DoConfirmarEliminarDatos() : boolean;
begin
  // Pregunta si realmente quiere borrar los datos
  Result := IsMessageBoxWarning(Message_Eliminar_Datos, Message_Atencion);
end;

// Actualiza el nodo raiz con el total de datos
procedure TForm_Principal.DoUpdateDatosNodoRaiz();
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
procedure TForm_Principal.DoEliminarItemArbol(Item : TItemDato);

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
          Arbol.DeleteNode(Node);
        end;

      end;
    except
        on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
    end;
  end;

var
  Node : PVirtualNode;
  Siguiente : PVirtualNode;
begin
  Node := Arbol.GetFirst();

  while Node <> nil do
    begin
      Siguiente := Arbol.GetNext(Node);

      // ajusta el nodo
      AjustarNodo(Node);

      Node := Siguiente;
    end;
end;

// Muestra el formulario de carga
procedure TForm_Principal.DoFormLoadingShow(MensajeTitulo : string; MensajeNormal : string);
begin
  // Oculta el texto en la statusbar
  DoEstadisticas(0, MensajeTitulo + ' - ' + MensajeNormal);


  // Desactiva los controles del formulario principal
  DoActivarShowModal(False);

  // Muestra en la statusbar el mensaje
  DoEstadisticas(0, MensajeNormal);

  // Muestra el formulario de carga
  FormLoadingShow(Self, MensajeTitulo, MensajeNormal);
end;

// Oculta el formulario de carga
procedure TForm_Principal.DoFormLoadingHide();
begin
  // Oculta el texto en la statusbar
  DoEstadisticas(0, '');

  // Activa los controles del formulario principal
  DoActivarShowModal(true);

  // Oculta el formulario de carga
  FormLoadingHide();
end;

// Activa o desactiva los controles
procedure TForm_Principal.DoActivarShowModal(activar : boolean);
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

  // Desactiva/Activa la toolbar y el panel inferior
  Barra_Herramientas.Enabled := activar;

end;

// Para gestionar las acciones de los menús sobre los items
procedure TForm_Principal.DoAccionItem(Sender: TObject; isEliminar : boolean);
var
  Node              : PVirtualNode;
  listado           : TLazVirtualStringTree;
begin
  if (ActiveControl = Arbol) or (ActiveControl = Lista) then
  begin
    // Obtiene el listado activo
    listado := TLazVirtualStringTree(ActiveControl);

    // Obtiene el nodo seleccionado
    Node := listado.GetFirstSelected();

    // Comprueba si el nodo seleccionado no es nulo
    if Node <> nil then
      if isEliminar then
        DoEliminarItem(listado, Node, listado = arbol)
      else
        DoPropiedadesItem(listado, Node)
  end;
end;

// Lanza la búsqueda avanzada
procedure TForm_Principal.DoBusquedaAvanzada();
begin
  // Invierte el botón
  ToolButtonBusquedaAvanzada.Down := not ToolButtonBusquedaAvanzada.Down;

  // Actualiza la UI
  ToolButtonBusquedaAvanzadaClick(ToolButtonBusquedaAvanzada)
end;


// Lanza la acción agregar un nuevo catalogo
procedure TForm_Principal.DoAddNuevoCatalogo();
var
  AddCatalogo : TForm_AddCatalogo;
begin
  // Selecciona la primera página
  NotebookPanelIzquiedo.PageIndex := 0;

  // Actualiza la UI
  Application.ProcessMessages;

  // Crea el formulario de agregar un nuevo catalogo
  AddCatalogo := TForm_AddCatalogo.create(Self);
  try
    // Se le pasan los datos del gestor de datos
    AddCatalogo.AgregarNuevoCatalogo(FGestorDatos);

    // Si realiza el análisis correctamente carga la lista de catalogos
    if AddCatalogo.ShowModal = mrOk then
    begin
      // Cargar Lista
      DoLoadListaCatalogos();
    end;
  finally
    AddCatalogo.Free;
  end;
end;


// Busca el texto en la lista de archivos
procedure TForm_Principal.DoBuscarTexto(const Buscar : string);
var
  Node    : PVirtualNode;
  Busqueda: string;

  function BuscarTexto(DatosItem  : TItemDato): Boolean;
  begin
    Result := false;

    // Comprueba si el item es nulo
    if DatosItem = nil then exit;

    // Comprueba si el texto a buscar es nulo
    if (Busqueda = '') then
    begin
      Result := true;
      exit;
    end;

    // Comprueba si el texto a buscar se encuentra en el nombre, extension o ruta
    if pos(Busqueda, lowercase(DatosItem.Nombre)) > 0 then
      Result := true
    else
      if pos(Busqueda, lowercase(DatosItem.Extension)) > 0 then
        Result := true
      else
        if pos(Busqueda, lowercase(DatosItem.Ruta)) > 0 then
          Result := true
  end;


  function Mostrar_Nodo_Servicio(Listado : TVirtualStringTree): Boolean;
  var
    NodeData  : PrListaData;
  begin
    // Obtiene los datos del nodo
    NodeData := Listado.GetNodeData(Node);

    // Comprueba la búsqueda
    Result    := BuscarTexto(NodeData^.NodeData);
  end;

  procedure Buscar_En_listado(Listado : TVirtualStringTree);
  begin
    Listado.BeginUpdate;
    try
      // Obtiene el primer nodo
      Node := Listado.GetFirst();
      while Node <> nil do
        begin
          // Pone el node en visible o no según el resultado de la búsqueda
          Listado.IsVisible[Node] := Mostrar_Nodo_Servicio(Listado);

          // Obtiene el siguiente nodo
          Node := Listado.GetNext(Node);
        end;
    finally
      Listado.EndUpdate;
    end;
  end;

begin
  // Pone la búsqueda en minúsculas y sin espacios
  Busqueda := trim(lowercase(Buscar));

  // Busca en la lista de archivos
  Buscar_En_listado(Lista);
end;

// Al recibir cambios del campo de búsqueda realiza la búsqueda
procedure TForm_Principal.DoEditBuscarChange(Sender: TObject);
begin
  DoBuscarTexto(FPanelBusqueda.EditBusqueda.Text);
end;


procedure TForm_Principal.DoExportar(Formato : TFormatoExportacion);
var
  Exportacion : TGestorExportacion;
  Node        : PVirtualNode;
  NodeData    : PrListaData;
  Archivo     : string;
  Extension  : string;

begin

  Archivo := 'report_'+FormatDateTime('yyyymmdd_hhnnss', Now);
  case Formato of
    TFormatoExportacion.feHTML : Extension := '.html';
    TFormatoExportacion.feTXT  : Extension := '.txt';
  end;

  // Configura el diálogo de guardado
  Savedialog1.Filter := copy(Extension, 2, length(Extension)) + ' (*' + Extension + ')|*' + Extension;
  Savedialog1.FileName                  := Archivo + Extension;

  // Si no se ha seleccionado ningún archivo sale
  if not Savedialog1.Execute then exit;

  // Muestra el mensaje de espera
  DoFormLoadingShow(Message_Exportacion_Espera_Titulo, Message_Exportacion_Espera_Info);
  try
    Exportacion := TGestorExportacion.create(Savedialog1.FileName, NOMBRE_PROGRAMA + ' v.' + VERSION_PROGRAMA, Formato, ImageListArchivos);
    try
      // Agrega el header
      Exportacion.addHeader();

      // Recorre los nodos seleccionados
      Node := Lista.GetFirstSelected();
      while Node <> nil do
      begin
        NodeData := Lista.GetNodeData(Node);
        if (NodeData <> nil) AND (NodeData^.NodeData <> nil ) then
        begin
          // Agrega el item
          Exportacion.AddItem(NodeData^.NodeData);
        end;
        Node := Lista.GetNextSelected(Node);
      end;

      // Agrega el footer
      Exportacion.addFooter();

      // Guarda el archivo
      Exportacion.save();

    finally
      Exportacion.Free;
    end;

  except
    on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;

  // Muestra el mensaje de espera
  DoFormLoadingHide();
end;

// Inicializar el titulo del formulario
procedure TForm_Principal.SetTituloVentana(extra : string = '');
begin
  Caption                 := Get_Titulo_Ventana(true, extra, true);
  Application.Title       := Get_Titulo_Ventana(true, extra, false);
end;

// Marca todos los nodos que esté en la ruta seleccionada
function TForm_Principal.GetRutaFromNodo(Node: PVirtualNode) : RawByteString;
var
  NodeData: PrListaData;

begin
  // Resetea el resultado
  Result := '';

  // Sale si no hay nodo
  if Node = nil then
    exit;

  // Marca los nodos padres

  try
    while Node <> nil do
    begin
        NodeData := arbol.GetNodeData(Node);
        if NodeData <> nil then
        begin
          Result := '/' + NodeData^.NodeData.Nombre + Result;
        end;

      // Comprueba si no es el nodo raiz
      if Node <> Arbol.RootNode then
        Node := Node^.Parent
      else
        Node := nil;
    end;
  except
      on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
  end;
end;

// Al cambiar de nodo actualiza el titulo de la ventana
procedure TForm_Principal.DoArbolChangeTitle(Node: PVirtualNode);
var
  Ruta : RawByteString;
begin
  // Pone el titulo del formulario
  Ruta := GetRutaFromNodo(Node);

  // Pone el titulo del formulario
  SetTituloVentana(Ruta);
end;

procedure TForm_Principal.MenuItemEnglishClick(Sender: TObject);
begin
  SetIdioma('en');
end;

procedure TForm_Principal.MenuItemSpahishClick(Sender: TObject);
begin
  SetIdioma('es');
end;

// Cambia el idioma de la aplicación
procedure TForm_Principal.SetIdioma(Idioma : string);
begin
  // Asigna el idioma
  FIdioma := Idioma;

  // Traduce la aplicación
  SetDefaultLang(Idioma, Curdir + 'idioma',  'cameex');

  // Traduce la barra de búsqueda
  FPanelBusqueda.EditBusqueda.TextHint := Buscar_Cajon_TExto;


  // Inicializar el titulo del formulario
  SetTituloVentana('');
end;

end.
