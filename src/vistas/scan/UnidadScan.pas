(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-09 11:51:16
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-21 00:26:17
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

unit UnidadScan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls
  , Control_Formulario_Avanzado
  , MotorScan
  ;

type
  TOnCancelarScan = procedure() of object;

  { TFrame_Scan }

  TFrame_Scan = class(TFrame)
    Bevel1: TBevel;
    ImageListSpinner: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label_Total_Carpetas: TLabel;
    Label_Tiempo: TLabel;
    Label_Total_Archivos: TLabel;
    Label_Total_Size: TLabel;
    Info_Archivo: TMemo;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButtonCancelar: TSpeedButton;
    TimerUpdateUI: TTimer;
    TimerAnimacion: TTimer;
    procedure SpeedButtonCancelarClick(Sender: TObject);
    procedure TimerAnimacionTimer(Sender: TObject);
    procedure TimerUpdateUITimer(Sender: TObject);
  private
    FAnimacionFrames : Integer;
    FAnimacionIndex  : integer;
    FInicio          : TDateTime;
    FScanActivo      : TMotorScan;
    FTerminar        : Boolean;
    FOnCancelarScan  : TOnCancelarScan;
  protected
    // Cancelar el escaneo
    function DoCancelar() : boolean; virtual;
  public
    // Indica que se ha terminado el escaneo
    procedure Terminar();

    // Solicita la cancelacion
    procedure Cancelar();

    // Inicia el monitoreo del escaneo
    procedure Iniciar(OnCancelarScan : TOnCancelarScan; ScanActivo : TMotorScan);

  end;

var
  FrameScan: TFrame_Scan;

implementation

uses
  utilidades
  , AppString;


{$R *.lfm}

{ TFrame_Scan }

procedure TFrame_Scan.SpeedButtonCancelarClick(Sender: TObject);
begin
  // Cancela el escaneo
  DoCancelar();
end;

procedure TFrame_Scan.TimerAnimacionTimer(Sender: TObject);
begin
  // Actualiza el índice de la imagen de la animación
  FAnimacionIndex := (FAnimacionIndex + 1) mod FAnimacionFrames;
  ImageListSpinner.Draw(Canvas, 32, 8, FAnimacionIndex);

  Application.ProcessMessages;
end;

procedure TFrame_Scan.TimerUpdateUITimer(Sender: TObject);
begin
  // Si se ha marcado para cerrar que no actualice nada
  if FTerminar then exit;

  // Actualiza el tiempo transcurrido
  Label_Tiempo.caption := MostrarTiempoTranscurrido(FInicio);

  // Actualiza los datos del escaneo
  Label_Total_Carpetas.caption := PuntearNumeracion(FScanActivo.TotalDirectorios);
  Label_Total_Archivos.caption := PuntearNumeracion(FScanActivo.TotalArchivos);
  Label_Total_Size.caption     := ConvertirSizeEx(FScanActivo.TotalSize);

  // Actualiza la información del archivo actual
  Info_Archivo.Lines.Text      := FScanActivo.Procesando;
end;


procedure TFrame_Scan.Terminar();
begin
  // Indica que se ha terminado el escaneo
  FTerminar := true;
end;

// Solicita la cancelacion
procedure TFrame_Scan.Cancelar();
begin
  // Solicita la cancelacion
  DoCancelar();
end;

// Cancelar el escaneo
function TFrame_Scan.DoCancelar() : boolean;
begin
  // Pregunta si realmente quiere cancelar el escaneo
  Result := IsMessageBoxInfoEx(Handle, Message_DLG_Cancelar_escaneo, Message_Atencion);

  if Result then
  begin
    // Indica que se ha cancelado el escaneo
    if assigned(FOnCancelarScan) then
      FOnCancelarScan();

    // Cancela el escaneo
    FScanActivo.StopScan();
  end;

end;

// Inicia el monitoreo del escaneo
procedure TFrame_Scan.Iniciar(OnCancelarScan : TOnCancelarScan; ScanActivo : TMotorScan);
begin
  // Asigna el motor de escaneo activo
  FScanActivo      := ScanActivo;

  // Inicializa el estado de terminación
  FTerminar        := False;

  // Inicializar el contador interno para la animación
  TimerAnimacion.interval := 100;

  // Asigna el evento de cancelar
  FOnCancelarScan := OnCancelarScan;

  // Obtiene el número total de frames de la animación
  FAnimacionFrames        := ImageListSpinner.Count;

  // Inicia el contador de tiempo
  FInicio                 := Now;
  TimerUpdateUI.Enabled   := True;
  TimerAnimacion.Enabled  := True;
end;


end.

