(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-23 17:18:08
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-24 16:31:00
 *)
unit FrameBusqueda;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, TAIntervalSources,
  DateTimePicker, SpinEx, ItemCatalogo, Types;

type

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
    procedure ComboBoxDispositivosDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure EditTextoChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  protected
    procedure DoValidate();

    // Devuelve la unidad seleccionada
    function GetCatalogo(index : longint) : TItemCatalogo;
  public
    constructor Create(AOwner: TComponent); override;

    // Carga los catalogos en el ComboBoxDispositivos
    procedure AddToListaCatalogos(Catalogo : TItemCatalogo);

    // Limpia el ComboBoxDispositivos
    procedure ClearListaCatalogos();

  end;

implementation

uses
  ItemBaseDatos, UnidadPrincipal;

{$R *.lfm}

{ TFrame_Busqueda }

constructor TFrame_Busqueda.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  DoValidate();
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

procedure TFrame_Busqueda.DoValidate();
begin
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

  BitBtnBusqueda.Enabled := (EditTexto.Text <> '') or
                            (CheckBoxFechaDesde.Checked) or
                            (CheckBoxFechaHasta.Checked) or
                            (CheckBoxSizeDesde.Checked) or
                            (CheckBoxSizeHasta.Checked) or
                            (ComboBoxDispositivos.ItemIndex > -1);

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

end.

