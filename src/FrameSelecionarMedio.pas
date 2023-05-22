(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-21 22:40:01
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-22 22:53:11
 *)
unit FrameSelecionarMedio;

{$mode ObjFPC}{$H+}

interface

uses
   Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, SynEdit, FGL, ItemBaseDatos, Types;



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
    Edit_Nombre_error1: TLabel;
    Edit_Nombre_error2: TLabel;
    Image1: TImage;
    ImageDispositivo: TImage;
    Label1: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    EditDescripcion: TMemo;
    procedure ComboBoxDispositivosChange(Sender: TObject);
    procedure ComboBoxDispositivosDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
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
  Result := true;

  Edit_Nombre_error.Visible := false;

  if Edit_Nombre.Text = '' then
  begin
    Result := false;
    Edit_Nombre_error.Visible := true;
  end;
end;

end.

