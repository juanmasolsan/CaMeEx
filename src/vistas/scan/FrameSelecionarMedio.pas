(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-21 22:40:01
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 23:54:58
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
unit FrameSelecionarMedio;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  SynEdit, ItemBaseDatos, Types;



type
  TUnidadDetectada = class
    Ruta : string;
    Tipo : TItemDatoTipo;
  end;

  { TFrame_SelecionarMedio }
  TFrame_SelecionarMedio = class(TFrame)
    ButtonSeleccionarRuta: TButton;
    ComboBoxDispositivos: TComboBox;
    EditExcluir: TMemo;
    EditRuta: TEdit;
    Edit_Nombre: TEdit;
    Edit_Nombre_error: TLabel;
    ComboDispositivo_error: TLabel;
    EditRuta_error: TLabel;
    Image1: TImage;
    ImageDispositivo: TImage;
    Label1: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    EditDescripcion: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure ButtonSeleccionarRutaClick(Sender: TObject);
    procedure ComboBoxDispositivosChange(Sender: TObject);
    procedure ComboBoxDispositivosDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure Edit_NombreChange(Sender: TObject);
  private
  protected
    // Construye las la lista de unidades detectadas
    procedure DoGenerarUnidades();

    // Devuelve la unidad seleccionada
    function GetUnidad(index : longint) : TUnidadDetectada;
  public
    // Constructor y destructor
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // Valida el formulario
    function IsValidate() : boolean;

    // Devuelve el tipo de catalogo seleccionado
    function GetTipoCatalogo() : TItemDatoTipo;
  end;

implementation

uses
  UnidadPrincipal
  , AppString;

{$R *.lfm}


{ TFrame_SelecionarMedio }

constructor TFrame_SelecionarMedio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Genera la lista de unidades
  DoGenerarUnidades();
end;


destructor TFrame_SelecionarMedio.Destroy;
var
  t : integer;
begin

  // Libera los objetos de ComboBoxDispositivos.Items
  for t := 0 to ComboBoxDispositivos.Items.Count - 1 do
    ComboBoxDispositivos.Items.Objects[t].Free;

  // Destructor del padre
  inherited Destroy;
end;

// Genera la lista de unidades
procedure TFrame_SelecionarMedio.DoGenerarUnidades();

  procedure CrearUnidad(Nombre,  Ruta : string; Tipo: TItemDatoTipo);
  var
    Unidad : TUnidadDetectada;
  begin
    Unidad := TUnidadDetectada.Create;
    Unidad.Ruta := ruta;
    Unidad.Tipo := Tipo;
    ComboBoxDispositivos.Items.AddObject(Nombre, Unidad);
  end;

  var
    Drive: Char;
    DriveLetter: string;
    OldMode: Word;
  begin

    // Crea la unidad de Carpeta
    CrearUnidad(Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Carpeta, '', TItemDatoTipo.RootCarpeta);

    // Empty Floppy or Zip drives can generate a Windows error.
    // We disable system errors during the listing.
    // Note that another way to skip these errors would be to use DEVICE_IO_CONTROL.
    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try

      // Search all drive letters
      for Drive := 'A' to 'Z' do
      begin
        DriveLetter := Drive + ':\';

        case GetDriveType(PChar(DriveLetter)) of
          DRIVE_REMOVABLE: CrearUnidad(DriveLetter + ' - ' + Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Usb, DriveLetter, TItemDatoTipo.RootUSB);
          DRIVE_FIXED:     CrearUnidad(DriveLetter + ' - ' + Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Hdd, DriveLetter, TItemDatoTipo.RootHDD);
          DRIVE_REMOTE:    CrearUnidad(DriveLetter + ' - ' + Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Red, DriveLetter, TItemDatoTipo.RootUnidadRed);
          DRIVE_CDROM:     CrearUnidad(DriveLetter + ' - ' + Message_Asistente_Nuevo_Catalogo_Tipo_Medio_Dvd, DriveLetter, TItemDatoTipo.RootDVD);
          DRIVE_RAMDISK:   CrearUnidad(DriveLetter + ' - ' + Message_Asistente_Nuevo_Catalogo_Tipo_Medio_RAM, DriveLetter, TItemDatoTipo.Root);
        end;
      end;

    finally
      // Restores previous Windows error mode.
      SetErrorMode(OldMode);
    end;
end;


procedure TFrame_SelecionarMedio.Edit_NombreChange(Sender: TObject);
begin
  // Valida el formulario
  IsValidate();
end;

// Devuelve la unidad seleccionada
function TFrame_SelecionarMedio.GetUnidad(index : longint) : TUnidadDetectada;
begin
  Result := nil;
  if index > -1 then
    Result := TUnidadDetectada(ComboBoxDispositivos.Items.Objects[index]);
end;

procedure TFrame_SelecionarMedio.ComboBoxDispositivosChange(Sender: TObject);
var
  Index  : integer;
  Unidad : TUnidadDetectada;
begin
  Unidad := GetUnidad(ComboBoxDispositivos.ItemIndex);
  if Unidad <> nil then
  begin
    Index  := integer(Unidad .Tipo);

    // Cambia la imagen del dispositivo
    Form_Principal.ImageListArchivos32.GetBitmap(Index, ImageDispositivo.Picture.Bitmap);

    // Asigna la ruta al edit de ruta
    EditRuta.Text := Unidad.Ruta;

    EditRuta.Enabled              := Unidad .Tipo = TItemDatoTipo.RootCarpeta;
    ButtonSeleccionarRuta.Enabled := EditRuta.Enabled;
  end;


end;

procedure TFrame_SelecionarMedio.ButtonSeleccionarRutaClick(Sender: TObject);
begin
  // Selecciona la ruta
  if SelectDirectoryDialog1.Execute then
    EditRuta.text :=  SelectDirectoryDialog1.FileName;
end;

procedure TFrame_SelecionarMedio.ComboBoxDispositivosDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  indexImage : longint;

  Unidad : TUnidadDetectada;
begin
  // Dibuja el texto y el fondo
  ComboBoxDispositivos.Canvas.FillRect(ARect);                                                    //first paint normal background
  ComboBoxDispositivos.Canvas.TextRect(ARect, 22, ARect.Top, ComboBoxDispositivos.Items[Index]);  //paint item text

  indexImage := integer(TItemDatoTipo.Root);
  Unidad := GetUnidad(Index);
  if Unidad <> nil then
    indexImage := integer(Unidad .Tipo);

  // Dibuja la imagen
  Form_Principal.ImageListArchivos.Draw(ComboBoxDispositivos.Canvas, ARect.Left + 1, ARect.Top + 1, indexImage);  //draw image according to index on canvas
end;

function TFrame_SelecionarMedio.IsValidate() : boolean;
begin
  // Inicializa el resultado
  Result := true;

{$IFNDEF ESCANEAR_DIRECTAMENTE}
  // Oculta los errores
  Edit_Nombre_error.Visible      := false;
  EditRuta_error.Visible         := false;
  ComboDispositivo_error.Visible := ComboBoxDispositivos.ItemIndex = -1;

  // Valida los campos
  if Edit_Nombre.Text = '' then
  begin
    Result                    := false;
    Edit_Nombre_error.Visible := true;
  end;

  if EditRuta.Text = '' then
  begin
    Result                 := false;
    EditRuta_error.Visible := true;
  end;
{$ENDIF ESCANEAR_DIRECTAMENTE}
end;

function TFrame_SelecionarMedio.GetTipoCatalogo() : TItemDatoTipo;
var
 Unidad : TUnidadDetectada;
begin
 Result := TItemDatoTipo.Root;

 Unidad := GetUnidad(ComboBoxDispositivos.ItemIndex);
  if Unidad <> nil then
    Result := Unidad .Tipo;

end;

end.

