(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-18 23:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-19 01:05:47
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
  , Spin
  ;


type

  { TFrame_Atributos }
  TFrame_Atributos = class(TFrame)
    GroupBox_Atributo_Tiempo: TGroupBox;
    CheckBox_Fecha_Creacion: TCheckBox;
    CheckBox_Fecha_Modificado: TCheckBox;
    CheckBox_Fecha_Acceso: TCheckBox;
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
    function DoGetFechaFromDateTime(Datos : string; Fecha : TDateTime) : longint;
    function DoGetFechaFromDateTime2String(Fecha : TDateTime) : string;
    procedure ShowPermissions_Windows(Mode: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  LCLType
  , SysUtils
  ;

{$R *.lfm}


constructor TFrame_Atributos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  GroupBox_Atributo_Windows.Visible := true;
end;


function TFrame_Atributos.DoGetFechaFromDateTime2String(Fecha : TDateTime) : string;
begin
  DateTimeToString(Result, 'dd/mm/yyyy hh:nn:ss', Fecha);
end;

function TFrame_Atributos.DoGetFechaFromDateTime(Datos : string; Fecha : TDateTime) : longint;
var
  StringDateTime : string;
begin
  DateTimeToString(StringDateTime, Datos, Fecha);
  Result := strtoint(StringDateTime);
end;


procedure TFrame_Atributos.ShowPermissions_Windows(Mode: Integer);
begin
{$WARNINGS OFF}
 CheckBox_Attr_Solo_Lectura.Checked := ((Mode AND faReadOnly) = faReadOnly);
 CheckBox_Attr_Oculto.Checked       := ((Mode AND faHidden) = faHidden);
 CheckBox_Attr_Sistema.Checked      := ((Mode AND faSysFile) = faSysFile);
{$WARNINGS ON}
end;









end.

