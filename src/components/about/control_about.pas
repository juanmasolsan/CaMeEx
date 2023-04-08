(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-07 16:15:29
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-08 17:44:04
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
unit Control_About;

{$mode objfpc}{$H+}


interface

uses
  LCLIntf
  , Classes
  , SysUtils
  , FileUtil
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ExtCtrls
  , StdCtrls
  , Types
  , Control_Formulario_Avanzado

  ;

type

  { TForm_About }

  TForm_About = class(TForm)
    Image_Built_with: TImage;
    Image_Logo: TImage;
    Label_Build: TLabel;
    Label_Nombre_Programa: TLabel;
    Label_Version: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label_Email: TLabel;
    Label_Url: TLabel;
    Label_Copyrigth: TLabel;
    Timer_Forzar_Cerrar: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image_Built_withClick(Sender: TObject);
    procedure Label_EmailClick(Sender: TObject);
    procedure Label_UrlClick(Sender: TObject);
    procedure Timer_Forzar_CerrarTimer(Sender: TObject);
  private
    { private declarations }
    FMostrar_Modal : boolean;
  public
    { public declarations }
  end;

var
  Form_About : TForm_About = nil;
  Form_About_Forzar_Cerrar : boolean = false;

procedure Mostrar_Acerca_de(NombrePrograma, Version, Fecha : string; CopyRigth : string; ExtraAncho : integer = 0; Web : string = ''; Email : string = ''; ShowModal : boolean = false);


implementation

{$R control_about.lfm}

const
  URL_LAZARUS  = 'http://www.lazarus.freepascal.org/';


procedure Mostrar_Acerca_de(NombrePrograma, Version, Fecha : string; CopyRigth : string; ExtraAncho : integer = 0; Web : string = ''; Email : string = ''; ShowModal : boolean = false);
begin
  Form_About_Forzar_Cerrar := false;

  if Form_About <> nil then
    begin
      if ShowModal then
        Form_About.ShowModal
      else
        Form_About.Show;

      exit;
    end;

  Form_About := TForm_About.Create(nil);
  try
    Form_About.Width := Form_About.Width + ExtraAncho;

    Form_About.Label_Nombre_Programa.Caption := NombrePrograma;
    Form_About.Label_Version.Caption         := Version;
    Form_About.Label_Build.Caption := '';

    if Fecha <> '' then
      Form_About.Label_Build.Caption := 'Build : ' + Fecha;

    Form_About.Label_Copyrigth.Caption      := '';
    if CopyRigth <> '' then
      Form_About.Label_Copyrigth.Caption := '© ' + {%H-}CopyRigth{%H-};

    Form_About.Label_Url.Caption      := 'N/A';
    if Web <>  '' then
      Form_About.Label_Url.Caption := Web;

    Form_About.Label_Email.Caption      := 'N/A';
    if Email <>  '' then
      Form_About.Label_Email.Caption := Email;

    if ShowModal then
    begin
      Form_About.FMostrar_Modal := true;
      Form_About.ShowModal
    end
    else
      Form_About.Show;
  finally
    if ShowModal then
    begin
      Form_About.Free;
      Form_About := nil;
    end;
  end;
end;

{ TForm_About }
procedure TForm_About.FormCreate(Sender: TObject);
var
  Icono : TIcon;
  Actual : longint;
  Size : TSize;
begin
  FMostrar_Modal := false;
  Actual := 0;

  Icono  := TIcon.Create;
  try
    Icono.Assign(application.Icon);
    Size.cx := 128;
    Size.cy := 128;
    Actual := Icono.GetBestIndexForSize(Size);
    Icono.Current := Actual;
    Image_Logo.Picture.Icon.Assign(Icono);
  finally
    Icono.Free;
  end;

  ActivarCerrarConESC;
  ActivarGuardadoPosicion(true);
  ActivarCursorLink(Label_URL);
  ActivarCursorLink(Label_Email);
  ActivarCursorLink(Image_Built_with);

  Image_Built_with.Hint := URL_LAZARUS;
end;

procedure TForm_About.Image_Built_withClick(Sender: TObject);
begin
  beep;
  OpenURL(URL_LAZARUS);
end;

procedure TForm_About.Label_EmailClick(Sender: TObject);
begin
  beep;
  if Label_Email.Caption <> 'N/A' then
    OpenURL(Label_Email.Caption);
end;

procedure TForm_About.Label_UrlClick(Sender: TObject);
begin
  beep;
  if Label_Url.Caption <> 'N/A' then
    OpenURL(Label_Url.Caption);
end;

procedure TForm_About.Timer_Forzar_CerrarTimer(Sender: TObject);
begin
  if Form_About_Forzar_Cerrar then
    begin
      Timer_Forzar_Cerrar.Enabled := false;
      close;
    end;
end;

procedure TForm_About.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if not FMostrar_Modal then
    Form_About := nil;
end;

end.

