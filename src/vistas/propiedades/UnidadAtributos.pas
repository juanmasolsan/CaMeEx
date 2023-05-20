(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-18 23:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-20 13:50:08
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

unit UnidadAtributos;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , Forms
  , StdCtrls
  , ItemBaseDatos
  , ItemDato
  ;


type

  { TFrame_Atributos }
  TFrame_Atributos = class(TFrame)
    CheckBox_Fecha_Acceso: TLabel;
    CheckBox_Fecha_Creacion: TLabel;
    CheckBox_Fecha_Modificado: TLabel;
    GroupBox_Atributo_Tiempo: TGroupBox;
    Label_Fecha_Acceso: TLabel;
    Label_Fecha_Creacion: TLabel;
    Label_Fecha_Modificado: TLabel;
    GroupBox_Atributo_Windows: TGroupBox;
    CheckBox_Attr_Solo_Lectura: TCheckBox;
    CheckBox_Attr_Oculto: TCheckBox;
    CheckBox_Attr_Sistema: TCheckBox;
  private
    { private declarations }
  protected
    // Devuelve la fecha en formato string
    function DoGetFechaFromDateTime2String(Fecha : TDateTime) : string;

    // Muestra los atributos del Item
    procedure DoShowPermissions_Windows(Mode: Integer; ExtraOculto : boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;

    // Muestra los datos del item
    procedure MostrarDatos(Item : TItemDato);
  end;

implementation

uses
  LCLType
  , SysUtils
  ;

{$R *.lfm}


{ TFrame_Atributos }
// Constructor
constructor TFrame_Atributos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  GroupBox_Atributo_Windows.Visible := true;
end;

// Devuelve la fecha en formato string
function TFrame_Atributos.DoGetFechaFromDateTime2String(Fecha : TDateTime) : string;
begin
  DateTimeToString(Result, 'dd/mm/yyyy hh:nn:ss', Fecha);
end;

// Muestra los atributos del Item
procedure TFrame_Atributos.DoShowPermissions_Windows(Mode: Integer; ExtraOculto : boolean);
begin
{$WARNINGS OFF}
  CheckBox_Attr_Solo_Lectura.Checked := ((Mode AND faReadOnly) = faReadOnly);
  CheckBox_Attr_Oculto.Checked       := ((Mode AND faHidden) = faHidden) or ExtraOculto;
  CheckBox_Attr_Sistema.Checked      := ((Mode AND faSysFile) = faSysFile);
{$WARNINGS ON}
end;

// Muestra los datos del item
procedure TFrame_Atributos.MostrarDatos(Item : TItemDato);
begin
  // Muestras las fechas del item
  if Item.Tipo >= TItemDatoTipo.Root then
  begin
    Label_Fecha_Acceso.Caption     := DoGetFechaFromDateTime2String(Item.Fecha);
    Label_Fecha_Creacion.Caption   := Label_Fecha_Acceso.Caption;
    Label_Fecha_Modificado.Caption := Label_Fecha_Acceso.Caption;
  end
  else
  begin
    Label_Fecha_Acceso.Caption     := DoGetFechaFromDateTime2String(Item.Fecha);
    Label_Fecha_Creacion.Caption   := DoGetFechaFromDateTime2String(Item.FechaCreacion);
    Label_Fecha_Modificado.Caption := DoGetFechaFromDateTime2String(Item.FechaLastAcceso);
  end;

  // Muestra los atributos del item
  DoShowPermissions_Windows(Item.atributos, Item.Nombre[1] = '.');
end;








end.

