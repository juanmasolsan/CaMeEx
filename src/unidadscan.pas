(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-09 11:51:16
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-09 12:27:24
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
  ;

type

  { TFormScan }

  TFormScan = class(TForm)
    Bevel1: TBevel;
    ImageListSpinner: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label_Archivo: TLabel;
    Label_Total_Carpetas: TLabel;
    Label_Tiempo: TLabel;
    Label_Total_Carpetas1: TLabel;
    Label_Total_Carpetas2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    TimerUpdateUI: TTimer;
    TimerAnimacion: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerAnimacionTimer(Sender: TObject);
  private
    FAnimacionFrames : Integer;
  public

  end;

var
  FormScan: TFormScan;

implementation

{$R *.lfm}

{ TFormScan }

procedure TFormScan.FormCreate(Sender: TObject);
begin
  //
  TimerAnimacion.interval := 100;
  FAnimacionFrames := ImageListSpinner.Count;
end;


procedure TFormScan.TimerAnimacionTimer(Sender: TObject);
begin
  //
  SpeedButton1.ImageIndex := (SpeedButton1.ImageIndex + 1) mod FAnimacionFrames;
end;


end.

