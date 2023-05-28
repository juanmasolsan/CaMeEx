(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-23 17:18:08
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-28 23:51:10
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
unit FrameBusqueda;

{$mode ObjFPC}{$H+}

interface

uses
  LCLType,
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons,
  DateTimePicker, SpinEx, ItemCatalogo, Types
  , ItemDato
  , InterfaceConectorDatos
  ;

type

  TOnBusquedaDatos = procedure (Padre : TItemDato; Query : PCommandBusqueda = nil) of object;
  TOnActivarShowModal = procedure (Activar : boolean) of object;

  { TFrame_Busqueda }

  TFrame_Busqueda = class(TFrame)
    BitBtnBusqueda: TBitBtn;
    CheckBoxFechaDesde: TCheckBox;
    CheckBoxSizeDesde: TCheckBox;
    CheckBoxFechaHasta: TCheckBox;
    CheckBoxSizeHasta: TCheckBox;
    ComboBoxDispositivos: TComboBox;
    DateTimePickerDesde: TDateTimePicker;
    DateTimePickerHasta: TDateTimePicker;
    EditTexto: TEdit;
    Edit_FechaHasta_Error: TLabel;
    Edit_SizeHasta_Error: TLabel;
    Edit_FechaDesde_Error: TLabel;
    Edit_SizeDesde_Error: TLabel;
    LabelCatalogo: TLabel;
    LabelTexto: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButtonClearDispositivos: TSpeedButton;
    SpeedButtonClearFechaDesde: TSpeedButton;
    SpeedButtonClearFechaHasta: TSpeedButton;
    SpeedButtonClearSizeDesde: TSpeedButton;
    SpeedButtonClearSizeHasta: TSpeedButton;
    SpinEditExSizeDesde: TSpinEditEx;
    SpinEditExSizeHasta: TSpinEditEx;
    procedure BitBtnBusquedaClick(Sender: TObject);
    procedure ComboBoxDispositivosDrawItem({%H-}Control: TWinControl;
      Index: Integer; ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure EditTextoChange(Sender: TObject);
    procedure EditTextoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SpeedButton1Click(Sender: TObject);
  private
    FOnBusquedaDatos : TOnBusquedaDatos;
    FOnActivarShowModal : TOnActivarShowModal;
  protected
    function DoValidate() : boolean;

    // Devuelve la unidad seleccionada
    function GetCatalogo(index : longint) : TItemCatalogo;

    // Activa o desactiva el showmodal
    procedure DoSetShowModal(Activar : boolean);
  public
    constructor Create(AOwner: TComponent); override;

    // Carga los catalogos en el ComboBoxDispositivos
    procedure AddToListaCatalogos(Catalogo : TItemCatalogo);

    // Limpia el ComboBoxDispositivos
    procedure ClearListaCatalogos();

    // Cuando se realiza la búsqueda
    property OnBusquedaDatos : TOnBusquedaDatos read FOnBusquedaDatos write FOnBusquedaDatos;
    property OnActivarShowModal : TOnActivarShowModal read FOnActivarShowModal write FOnActivarShowModal;

  end;

implementation

uses
  ItemBaseDatos, UnidadPrincipal;

{$R *.lfm}

{ TFrame_Busqueda }

constructor TFrame_Busqueda.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 // Ajusta los campos de fecha con las fecha actual
 DateTimePickerDesde.DateTime := now;
 DateTimePickerHasta.DateTime := now;

  DoValidate();
end;

procedure TFrame_Busqueda.SpeedButton1Click(Sender: TObject);
begin
  case TControl(Sender).tag of
    0 :  EditTexto.Text := '';
    1 : DateTimePickerDesde.DateTime := now;
    2 : DateTimePickerHasta.DateTime := now;
    3 : SpinEditExSizeDesde.Value := 0;
    4 : SpinEditExSizeHasta.Value := 0;
    5 : ComboBoxDispositivos.ItemIndex := -1;
  end;

  // Comprueba de nuevo el formulario
  DoValidate();
end;

procedure TFrame_Busqueda.EditTextoChange(Sender: TObject);
begin

  if SpinEditExSizeDesde.Value < 0 then SpinEditExSizeDesde.Value := 0;
  if SpinEditExSizeHasta.Value < 0 then SpinEditExSizeHasta.Value := 0;

  DoValidate();
end;

procedure TFrame_Busqueda.EditTextoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_RETURN : begin
                  if Shift = [] then
                  begin
                    if DoValidate() then
                      BitBtnBusquedaClick(Sender);
                  end;
                end;
  end;

end;

procedure TFrame_Busqueda.ComboBoxDispositivosDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  indexImage : longint;
  Catalogo   : TItemCatalogo;
begin
  // Dibuja el texto y el fondo
  ComboBoxDispositivos.Canvas.FillRect(ARect);                                                    //first paint normal background
  ComboBoxDispositivos.Canvas.TextRect(ARect, 24, ARect.Top + 3, ComboBoxDispositivos.Items[Index]);  //paint item text

  indexImage := integer(TItemDatoTipo.Root);
  Catalogo := GetCatalogo(Index);
  if Catalogo <> nil then
    indexImage := integer(Catalogo.Tipo);

  // Dibuja la imagen
  Form_Principal.ImageListArchivos.Draw(ComboBoxDispositivos.Canvas, ARect.Left + 1, ARect.Top + 2, indexImage);  //draw image according to index on canvas
end;

function TFrame_Busqueda.DoValidate() : boolean;
begin

 Edit_FechaDesde_Error.Visible := CheckBoxFechaDesde.Checked and CheckBoxFechaHasta.Checked and (DateTimePickerDesde.DateTime > DateTimePickerHasta.DateTime);
 Edit_FechaHasta_Error.Visible := CheckBoxFechaDesde.Checked and CheckBoxFechaHasta.Checked and (DateTimePickerDesde.DateTime > DateTimePickerHasta.DateTime);


  Edit_SizeDesde_Error.Visible := CheckBoxSizeDesde.Checked and CheckBoxSizeHasta.Checked and (SpinEditExSizeDesde.Value > SpinEditExSizeHasta.Value);
  Edit_SizeHasta_Error.Visible := CheckBoxSizeDesde.Checked and CheckBoxSizeHasta.Checked and (SpinEditExSizeDesde.Value > SpinEditExSizeHasta.Value);


  SpeedButton1.Enabled               := EditTexto.Text <> '';
  LabelTexto.Font.Bold               := EditTexto.Text <> '';

  CheckBoxFechaDesde.Font.Bold       := CheckBoxFechaDesde.Checked;
  DateTimePickerDesde.Enabled        := CheckBoxFechaDesde.Checked;
  SpeedButtonClearFechaDesde.Enabled := CheckBoxFechaDesde.Checked;

  CheckBoxFechaHasta.Font.Bold       := CheckBoxFechaHasta.Checked;
  DateTimePickerHasta.Enabled        := CheckBoxFechaHasta.Checked;
  SpeedButtonClearFechaHasta.Enabled := CheckBoxFechaHasta.Checked;

  CheckBoxSizeDesde.Font.Bold        := CheckBoxSizeDesde.Checked;
  SpinEditExSizeDesde.Enabled        := CheckBoxSizeDesde.Checked;
  SpeedButtonClearSizeDesde.Enabled  := CheckBoxSizeDesde.Checked;


  CheckBoxSizeHasta.Font.Bold        := CheckBoxSizeHasta.Checked;
  SpinEditExSizeHasta.Enabled        := CheckBoxSizeHasta.Checked;
  SpeedButtonClearSizeHasta.Enabled  := CheckBoxSizeHasta.Checked;

  SpeedButtonClearDispositivos.Enabled := ComboBoxDispositivos.ItemIndex > -1;
  LabelCatalogo.Font.Bold              := ComboBoxDispositivos.ItemIndex > -1;

  Result := ((EditTexto.Text <> '') or
            (CheckBoxFechaDesde.Checked) or
            (CheckBoxFechaHasta.Checked) or
            (CheckBoxSizeDesde.Checked) or
            (CheckBoxSizeHasta.Checked) or
            (ComboBoxDispositivos.ItemIndex > -1))
            and not Edit_FechaDesde_Error.Visible
            and not Edit_FechaHasta_Error.Visible
            and not Edit_SizeDesde_Error.Visible
            and not Edit_SizeHasta_Error.Visible
            ;

  // Bloque o no el boton si se puede usar con los datos suministrados
  BitBtnBusqueda.Enabled := Result;
end;

// Limpia el ComboBoxDispositivos
procedure TFrame_Busqueda.ClearListaCatalogos();
begin
  ComboBoxDispositivos.items.Clear;
end;

// Carga los catalogos en el ComboBoxDispositivos
procedure TFrame_Busqueda.AddToListaCatalogos(Catalogo : TItemCatalogo);
begin
  ComboBoxDispositivos.Items.AddObject(Catalogo.Nombre, Catalogo);
end;


// Devuelve el Catalogo seleccionado
function TFrame_Busqueda.GetCatalogo(index : longint) : TItemCatalogo;
begin
  Result := nil;
  if index > -1 then
    Result := TItemCatalogo(ComboBoxDispositivos.Items.Objects[index]);
end;

procedure TFrame_Busqueda.BitBtnBusquedaClick(Sender: TObject);
var
  Query    : TCommandBusqueda;
  Catalogo : TItemCatalogo;
begin
  Self.Enabled := false;
  DoSetShowModal(false);
  try
    // Inicializa la consulta
    FillChar(Query, SizeOf(Query), 0);

    // La configuración de búsqueda el id de catalogo
    if ComboBoxDispositivos.ItemIndex > -1 then
      begin
        Catalogo := GetCatalogo(ComboBoxDispositivos.ItemIndex);
        if Catalogo <> nil then
        Query.CatalogoId := Catalogo.Id;
      end;

    // La configuración de búsqueda por el texto
    Query.Texto := trim(EditTexto.Text);

    // La configuración de búsqueda por tamaño desde
    if CheckBoxSizeDesde.Checked then
      Query.SizeDesde := SpinEditExSizeDesde.Value;

    // La configuración de búsqueda por tamaño hasta
    if CheckBoxSizeHasta.Checked then
      Query.SizeHasta := SpinEditExSizeHasta.Value;


    // La configuración de búsqueda por Fecha desde
    if CheckBoxFechaDesde.Checked then
      Query.FechaDesde := DateTimePickerDesde.Date;

    // La configuración de búsqueda por Fecha hasta
    if CheckBoxFechaHasta.Checked then
      Query.FechaHasta := DateTimePickerHasta.Date;


    if assigned(FOnBusquedaDatos) then
      FOnBusquedaDatos(nil, @Query);

  finally
    Self.Enabled := DoValidate();
    DoSetShowModal(true);
  end;
end;

// Activa o desactiva el showmodal
procedure TFrame_Busqueda.DoSetShowModal(Activar : boolean);
begin
  if assigned(FOnActivarShowModal) then
    FOnActivarShowModal(Activar);

  // Para que se vea el cambio
  Application.ProcessMessages;
end;

end.

