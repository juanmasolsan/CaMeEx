(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-23 17:18:08
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-24 00:40:39
 *)
unit FrameBusqueda;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, TAIntervalSources,
  DateTimePicker, SpinEx;

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
    Label4: TLabel;
    LabelTexto: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButtonClearDispositivos: TSpeedButton;
    SpeedButtonClearFechaDesde: TSpeedButton;
    SpeedButtonClearFechaHasta: TSpeedButton;
    SpeedButtonClearSizeDesde: TSpeedButton;
    SpeedButtonClearSizeHasta: TSpeedButton;
    SpinEditExSizeDesde: TSpinEditEx;
    SpinEditExSizeHasta: TSpinEditEx;
    procedure EditTextoChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  protected
    procedure DoValidate();
  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

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

end;

procedure TFrame_Busqueda.EditTextoChange(Sender: TObject);
begin
  DoValidate();
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

  SpeedButtonClearDispositivos.Enabled := ComboBoxDispositivos.ItemIndex > 0;

  BitBtnBusqueda.Enabled := (EditTexto.Text <> '') or
                            (CheckBoxFechaDesde.Checked) or
                            (CheckBoxFechaHasta.Checked) or
                            (CheckBoxSizeDesde.Checked) or
                            (CheckBoxSizeHasta.Checked) or
                            (ComboBoxDispositivos.ItemIndex > 0);

end;

end.

