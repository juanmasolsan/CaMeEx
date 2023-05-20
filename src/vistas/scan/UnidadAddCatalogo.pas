(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 12:18:17
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-20 13:50:54
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
unit UnidadAddCatalogo;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , Forms
  , Controls
  , StdCtrls
  , ComCtrls
  , ExtCtrls
  , Control_Formulario_Avanzado
//  , InterfaceConectorDatos
;

type
  { TForm_AddCatalogo }
  TForm_AddCatalogo = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button_Cancelar: TButton;
    Button_Atras: TButton;
    Button_Siguiente: TButton;
    Image1: TImage;
    Label_Titulo_Asistente_Add: TLabel;
    Shape1: TShape;
    procedure Button_SiguienteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button_AtrasClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    //FGestorDatos : IConectorDatos;
  protected

  public
    { public declarations }
  end;

var
  Form_AddCatalogo: TForm_AddCatalogo;



implementation


{$R *.lfm}

{ TForm_AddCatalogo }

procedure TForm_AddCatalogo.FormCreate(Sender: TObject);
begin
  ActivarGuardadoPosicion(true);

  // Crear pagina de atributos
  //FAtributos               := TFrame_Atributos.Create(TComponent(Pointer(@Pagina_Atributos)^));
  //FAtributos.Parent        := Pagina_Atributos;
end;

procedure TForm_AddCatalogo.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FAtributos);
end;


procedure TForm_AddCatalogo.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TForm_AddCatalogo.Button_SiguienteClick(Sender: TObject);
begin
  // TODO: Implementar Atras
end;

procedure TForm_AddCatalogo.Button_AtrasClick(Sender: TObject);
begin
 // TODO: Implementar Atras
end;



end.
