(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-16 16:17:09
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-17 18:59:55
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

unit UnidadLoading;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls
  , Control_Formulario_Avanzado
  , MotorScan
  ;

type

  { TFormLoading }

  TFormLoading = class(TForm)
    ImageListSpinner: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Fondo: TShape;
    SpeedButton1: TSpeedButton;
    TimerAnimacion: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure TimerAnimacionTimer(Sender: TObject);
  private
    FAnimacionFrames : Integer;
    FAnimacionIndex  : integer;
    FTerminar        : Boolean;
  protected

  public
    // Constructor
    constructor CreateEx(TheOwner: TComponent; MensajeTitulo : string; MensajeNormal : string);

    // Indica que se ha terminado
    procedure Terminar();
  end;

var
  FormLoading: TFormLoading;

// Muestra el formulario de carga
procedure FormLoadingShow(TheOwner: TComponent; MensajeTitulo : string; MensajeNormal : string);

// Oculta el formulario de carga
procedure FormLoadingHide();

implementation

uses
  utilidades
  , AppString;


{$R *.lfm}

{ TFormLoading }
constructor TFormLoading.CreateEx(TheOwner: TComponent; MensajeTitulo : string; MensajeNormal : string);
begin
  // Inicializa el estado de terminación
  FTerminar        := False;

  // Llama al constructor de la clase padre
  inherited Create(TheOwner);

  // Inicializa el mensaje
  Label1.Caption := MensajeTitulo;
  Label2.Caption := MensajeNormal;
end;


procedure TFormLoading.FormCreate(Sender: TObject);
begin
  // Opciones avanzadas
  ActivarGuardadoPosicion;

  // Inicializar el contador interno para la animación
  TimerAnimacion.interval := 100;

  // Obtiene el número total de frames de la animación
  FAnimacionFrames        := ImageListSpinner.Count;
end;

procedure TFormLoading.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FTerminar then exit;
end;

procedure TFormLoading.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
end;

procedure TFormLoading.TimerAnimacionTimer(Sender: TObject);
begin
  // Actualiza el índice de la imagen de la animación
  FAnimacionIndex := (FAnimacionIndex + 1) mod FAnimacionFrames;
  ImageListSpinner.Draw(Fondo.Canvas, 16, 8, FAnimacionIndex);
  Application.ProcessMessages;
end;


procedure TFormLoading.Terminar();
begin
  FTerminar := true;
  close();
end;

// Muestra el formulario de carga
procedure FormLoadingShow(TheOwner: TComponent; MensajeTitulo : string; MensajeNormal : string);
begin
  if FormLoading <> nil then
    exit;

  // Crea el formulario
  FormLoading := TFormLoading.CreateEx(TheOwner, MensajeTitulo, MensajeNormal);

  // Muestra el formulario
  FormLoading.Show;
end;

// Oculta el formulario de carga
procedure FormLoadingHide();
begin
  // Si no hay formulario
  if FormLoading <> nil then
  begin
    // Termina el formulario
    FormLoading.Terminar();

    // Libera el formulario
    FormLoading.Free;

    // Libera la variable
    FormLoading := nil;
  end;
end;



end.

