(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-21 22:40:01
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-22 22:29:19
 *)
unit FrameSelecionarMedio;

{$mode ObjFPC}{$H+}

interface

uses
   Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, SynEdit, FGL, ItemBaseDatos, Types;

type
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
    Image2: TImage;
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
      procedure DoGenerarUnidades();
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;


    function IsValidate() : boolean;
  end;

implementation

uses
   UnidadPrincipal,
 AppString;

{$R *.lfm}


type
  TUnidadDetectada = class
    Ruta : string;
    Tipo : TItemDatoTipo;
  end;


{ TFrame_SelecionarMedio }

constructor TFrame_SelecionarMedio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

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

procedure TFrame_SelecionarMedio.ComboBoxDispositivosChange(Sender: TObject);
begin
  //TODO: Al seleccionar un dispositivo que muestre la ruta actual, si no es carpeta, que la ruta esté bloqueada
end;

procedure TFrame_SelecionarMedio.ComboBoxDispositivosDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  indexImage : longint;
begin
  ComboBoxDispositivos.Canvas.FillRect(ARect);                                                    //first paint normal background
  ComboBoxDispositivos.Canvas.TextRect(ARect, 22, ARect.Top, ComboBoxDispositivos.Items[Index]);  //paint item text

  indexImage := integer(TItemDatoTipo.Root);
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

