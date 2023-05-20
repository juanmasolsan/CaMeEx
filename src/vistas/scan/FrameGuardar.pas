(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-20 22:29:39
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-21 01:27:11
 *)
unit FrameGuardar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, SynEdit, MotorScan;

type

  { TFrame_Guardar }

  TFrame_Guardar = class(TFrame)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    SalidaLog: TSynEdit;
  private

  public
    // Muestra un resumen de todo lo que ha encontrado
    procedure MostrarEstadisticaas(ScanActivo : TMotorScan);

  end;

implementation

uses
  AppString, Utilidades;

{$R *.lfm}

// Muestra un resumen de todo lo que ha encontrado
procedure TFrame_Guardar.MostrarEstadisticaas(ScanActivo : TMotorScan);
begin
  SalidaLog.Clear;

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(ScanActivo.Root.Nombre);
  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Total_Directorios + '  ' + PuntearNumeracion(ScanActivo.TotalDirectorios));
  SalidaLog.lines.Add(Message_Total_Archivos + '  ' + PuntearNumeracion(ScanActivo.TotalArchivos));

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Total_Size + '  ' + PuntearNumeracion(ScanActivo.TotalSize) + ' (' + ConvertirSizeEx(ScanActivo.TotalSize, ',##', '.0' ) + ')');

  SalidaLog.lines.Add('------------------------------------------');
  SalidaLog.lines.Add(Message_Tiempo_Escaneo + '  ' + MostrarTiempoTranscurrido(ScanActivo.ScanInicio, ScanActivo.ScanFinal));

  SalidaLog.Repaint;

end;

end.

