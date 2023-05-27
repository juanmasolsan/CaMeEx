(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-03-23 16:19:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 01:33:08
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
unit UnidadPropiedades;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes
  , Forms
  , Controls
  , StdCtrls
  , ComCtrls
  , ExtCtrls
  , UnidadAtributos
  , Control_Formulario_Avanzado
  , ItemBaseDatos
  , ItemDato
  , UnidadPrincipal
  , InterfaceConectorDatos;

type
  { TForm_Propiedades }
  TForm_Propiedades = class(TForm)
    Bevel6: TBevel;
    Button_Aceptar: TButton;
    Button_Cancelar: TButton;
    Button_Aplicar: TButton;
    Edit_Nombre_error: TLabel;
    Label6: TLabel;
    Label_Extra_Ubicacion: TMemo;
    Memo_Descripcion: TMemo;
    PageControl_Propiedades: TPageControl;
    Panel_Catalogo_descripcion: TPanel;
    TabSheet1: TTabSheet;
    Panel_Nombre_Imagen: TPanel;
    Edit_Nombre: TEdit;
    Image_Archivo: TImage;
    Bevel1: TBevel;
    Panel_Tipo: TPanel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label_Tipo_Archivo: TLabel;
    Panel_Datos_Extras: TPanel;
    Bevel3: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label_Extra_Size: TLabel;
    Timer_Size_Dir: TTimer;
    Panel_Contiene: TPanel;
    Bevel5: TBevel;
    Label5: TLabel;
    Label_Contiene: TLabel;
    Timer_Modificado: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button_CancelarClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Memo_DescripcionChange(Sender: TObject);
    procedure Timer_ModificadoTimer(Sender: TObject);
    procedure Edit_NombreChange(Sender: TObject);
    procedure Button_AplicarClick(Sender: TObject);
    procedure Button_AceptarClick(Sender: TObject);
  private
    { private declarations }
    FAtributos                 : TFrame_Atributos;
    FSalir                     : boolean;
    FItem                      : TItemDato;
    FGestorDatos               : IConectorDatos;
    FIsModificado              : boolean;
  protected
    function IsModificado: Boolean;

    // Calcula lo que contiene el item
    procedure DoObtenerDatos_Contiene(Data: PtrInt);

    // Muestra la string de tamaño formateado
    procedure DoMostrarSize(size : qword);

    // Actualiza los datos del catalogo
    procedure DoUpdaCatalogo();
  public
    { public declarations }

    // Muestra las propiedades del item
    procedure Mostrar_Propiedades(const Item : TItemDato; GestorDatos : IConectorDatos);
  end;

var
  Form_Propiedades: TForm_Propiedades;



implementation

uses
  SysUtils
  , Graphics
  , LazFileUtils
  , Utilidades
  , GestorExtensiones
  , Configuracion
  , AppString, ItemCatalogo, Control_Logger;

{$R *.lfm}

{ TForm_Propiedades }
procedure TForm_Propiedades.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TForm_Propiedades.FormCreate(Sender: TObject);
var
  Pagina_Atributos :  TTabSheet;
begin
  FSalir := false;
  ActivarCerrarConESC;
  ActivarGuardadoPosicion(true);

  // Crear pagina de atributos
  Pagina_Atributos         := PageControl_Propiedades.AddTabSheet;
  Pagina_Atributos.Color   := clwindow;
  Pagina_Atributos.Caption := Message_Atributos;
  FAtributos               := TFrame_Atributos.Create(TComponent(Pointer(@Pagina_Atributos)^));
  FAtributos.Parent        := Pagina_Atributos;
end;

function TForm_Propiedades.IsModificado: Boolean;
begin
  result := FIsModificado;
end;

procedure TForm_Propiedades.Button_CancelarClick(Sender: TObject);
begin
  close;
end;

procedure TForm_Propiedades.FormDestroy(Sender: TObject);
begin
  FSalir := true;
  FreeAndNil(FAtributos);
end;

procedure TForm_Propiedades.Memo_DescripcionChange(Sender: TObject);
begin
  FIsModificado := true;
end;

procedure TForm_Propiedades.Timer_ModificadoTimer(Sender: TObject);
begin
  Timer_Modificado.Enabled := false;
  try
    Button_Aplicar.Enabled := IsModificado;
  finally
    Timer_Modificado.Enabled := true;
  end;
end;


procedure TForm_Propiedades.Edit_NombreChange(Sender: TObject);
begin
  FIsModificado := true;
end;

procedure TForm_Propiedades.Button_AplicarClick(Sender: TObject);
begin
  // Actualiza los datos del catalogo
  DoUpdaCatalogo();
end;

procedure TForm_Propiedades.Button_AceptarClick(Sender: TObject);
begin
  // Actualiza los datos del catalogo
  DoUpdaCatalogo();
  close;
end;

procedure TForm_Propiedades.Mostrar_Propiedades(const Item : TItemDato; GestorDatos : IConectorDatos);
var
  ImageIndexSistema : Longint;

  procedure ObtenerDatos_Imagen_Nombre_Descripcion;
  begin
    ImageIndexSistema := GetImageIndexByItemDato(Item);

    if ImageIndexSistema <> - 1 then
    begin
      Form_Principal.ImageListArchivos32.GetBitmap(ImageIndexSistema, Image_Archivo.Picture.Bitmap);
      Form_Principal.ImageListArchivos.GetIcon(ImageIndexSistema, Icon);
    end;

    Edit_Nombre.Text := Item.Nombre;
    Label_Extra_Ubicacion.Lines.Text := Item.Ruta;
  end;


  procedure ObtenerDatos_Contiene;
  begin
    if Panel_Contiene.Visible then
    begin
      // Muestra el tamaño del directorio
      Label_Extra_Size.Caption := Message_Calculando;
      Label_Contiene.Caption   := Message_Calculando;

      // Calcula el tamaño del directorio
      application.QueueAsyncCall(@DoObtenerDatos_Contiene, 0);
    end;
  end;


begin
  // Guarda los datos
  FGestorDatos     := GestorDatos;
  FItem            := Item;

  // Muestra el nombre de la ventana
  Caption := Message_Propiedades_de + ' ' + Item.Nombre;

  // Defeine que se puede mostrar dependiendo del tipo de item
  Panel_Nombre_Imagen.Visible        := true;
  Panel_Contiene.Visible             := true;
  Panel_Contiene.Visible             := (Item.Tipo >= TItemDatoTipo.Root) or (Item.Tipo = TItemDatoTipo.Directorio);
  Panel_Catalogo_descripcion.Visible := Item.Tipo >= TItemDatoTipo.Root;

  // Muestra la imagen, nombre y descripcion del item
  ObtenerDatos_Imagen_Nombre_Descripcion;

  // Muestra el tipo de archivo
  Label_Tipo_Archivo.Caption    := GetExtensionDescripcionById(Item.IdExtension);

  // Pone la edición del nombre en solo lectura si no es un catalogo
  Edit_Nombre.ReadOnly          := Item.Tipo < TItemDatoTipo.Root;

  // Muestra los atributos del Item
  FAtributos.MostrarDatos(Item);

  // Muestra la string de tamaño formateado
  DoMostrarSize(Item.Size);

  // Calcula lo que contiene el item
  ObtenerDatos_Contiene;

  // Muestra la descripcion del catalogo
  if Item.Tipo >= TItemDatoTipo.Root then
  begin
    Memo_Descripcion.Text      := TItemCatalogo(Item).Descripcion;
    Label_Tipo_Archivo.Caption := Message_Catalogo;
  end;

  FIsModificado := false;
end;

// Muestra la string de tamaño formateado
procedure TForm_Propiedades.DoMostrarSize(size : qword);
begin
  Label_Extra_Size.Caption := ConvertirSizeEx(size) + ' ('+PuntearNumeracion(size)+' bytes)';
end;

// Calcula lo que contiene el item
procedure TForm_Propiedades.DoObtenerDatos_Contiene(Data: PtrInt);
var
  TotalDirectorios: integer = 0;
  TotalArchivos   : integer = 0;
  TotalSize       : qword = 0;
begin
  if FGestorDatos = nil then exit;

  // Si es del tipo catalogo, recupera los datos del catalogo
  if FItem.Tipo >= TItemDatoTipo.Root then
  begin
    // Recupera las estadisticas del item
    TotalDirectorios := TItemCatalogo(FItem).TotalDirectorios;
    TotalArchivos    := TItemCatalogo(FItem).TotalArchivos;
    TotalSize        := FItem.Size;
  end
  else
  begin
    // Recupera las estadisticas del item
    FGestorDatos.GetDirectorioEstadisticas(FItem, TotalDirectorios, TotalArchivos, TotalSize);
  end;


  // Muestra la string de tamaño formateado
  DoMostrarSize(TotalSize);

  // Muestra la string de contiene formateada
  Label_Contiene.Caption   := Format(Message_Contiene, [PuntearNumeracion(TotalArchivos), PuntearNumeracion(TotalDirectorios)]);
end;

// Actualiza los datos del catalogo
procedure TForm_Propiedades.DoUpdaCatalogo();
var
  Catalogo: TItemCatalogo;
  IsError : boolean;
begin
  // Si no se ha modificado nada, sale
  if not FIsModificado then exit;

  // Comprueba que el nombre no este vacio
  IsError := Edit_Nombre.Text = '';

  // Muestra el error
  Edit_Nombre_error.Visible := IsError;

  // Sale si hay error
  if IsError then exit;

  // Guarda los datos
  if assigned(FGestorDatos) then
    if assigned(FItem) and not FItem.IsFreed then
      if FItem.Tipo >= TItemDatoTipo.Root then
      begin
        try
          Catalogo := TItemCatalogo(FItem);

          // Actualiza los datos del catalogo
          Catalogo.Nombre      := Edit_Nombre.Text;
          Catalogo.Fecha       := now;
          Catalogo.Descripcion := Memo_Descripcion.Text;

          // Actualiza los datos del catalogo
          FGestorDatos.UpdateCatalogo(Catalogo);

          // Marca que yu
          FIsModificado := false;

          // Muestra los atributos del Item
          FAtributos.MostrarDatos(Catalogo);

          // Actualiza el arbol
          Form_Principal.Arbol.Repaint;

        except
            on E: Exception do LogAddException(Message_Excepcion_Detectada, E);
        end;
    end;
end;

end.
