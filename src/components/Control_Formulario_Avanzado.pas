(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-06 22:40:12
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-16 17:44:47
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
unit Control_Formulario_Avanzado;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf
  , LCLType
  , Classes
  , SysUtils
  , Controls
  , Graphics
  , forms
  , Dialogs
  , ExtCtrls
  , Inifiles
  , Menus
  ;



type

  { TWindowStates }
  TWindowStates = set of TWindowState;

  { TForm_Avanzado_Custom }
  TForm_Avanzado_Custom = class(TForm)
  private
    FCurdir                  : String;
    FArchivoConfiguracion    : TMemInifile;
    FDirectorio_Aplicacion   : string;
    FIsPostable              : boolean;
    FGuardarPosicion_Activar : boolean;
    FIsPreShow               : boolean;
    FCerrarConESC            : boolean;
    FEstadosAdmitidos        : TWindowStates;
  protected
    procedure DoCreate; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Do_Corregir_Colores_Controles;
    procedure DoGuardarPosicioForm();
    procedure DoRestaurarPosicionForm(SoloPosicion : boolean = false);
  public
    destructor Destroy; override;
    function CloseQuery: boolean; override;

    procedure ActivarArchivoConfig(Archivo : String; GenerateCurDir : boolean = false; GenerateUserDir : boolean = false; {%H-}GenerateUserDirGlobal : boolean = false; NombreDirConfig : string = ''); virtual; overload;
    procedure ActivarArchivoConfig(ArchivoIni : TMemIniFile); virtual; overload;
    procedure ActivarCursorLink(Objeto : TControl); virtual;

    property ArchivoConfiguracion : TMemInifile read FArchivoConfiguracion;
    property Curdir               : String read FCurdir write FCurdir;
    property DirectorioConfig     : String read FDirectorio_Aplicacion;
    property IsPostable           : boolean read FIsPostable;

    procedure ActivarGuardadoPosicion(SoloPosicion : boolean = false; EstadoVentanaAdmitidos : TWindowStates = [wsNormal, wsMaximized, wsFullScreen]); virtual;

    procedure ActivarCerrarConESC; virtual;

    procedure Corregir_Colores_Controles;

    // Muestra un mensaje y devuelve si se ha pulsado el botón Aceptar o Cancelar
    function IsMessageBoxInfo(Mensaje, Titulo : String) : boolean;

    // Muestra un mensaje y devuelve si se ha pulsado el botón Aceptar o Cancelar
    function IsMessageBoxWarning(Mensaje, Titulo : String) : boolean;

  end;






type
  { Puenteamos la clase form }
  TForm = class(TForm_Avanzado_Custom);

implementation

uses
  Control_Directorio_Save
  ;



var
  FForzarColorFormularioActivo  : boolean       = false;
  FForzarColorFormularioColor   : TColor        = clDefault;
  FGlobal_ArchivoConfiguracion  : TMemInifile   = nil;
  FGlobal_Directorio_Aplicacion : string        = '';
  FOnGetApplicationName         : String        = '';


function GetApplicationName : String;
begin
  Result := FOnGetApplicationName;
end;

procedure SetColorGlobalFormularios(NuevoColor : TColor);
begin
  if FForzarColorFormularioColor <>  NuevoColor then
  begin
    FForzarColorFormularioColor  := NuevoColor;
    FForzarColorFormularioActivo := true;
  end;
end;



{ TForm_Avanzado_Custom }

destructor TForm_Avanzado_Custom.Destroy;
begin
  inherited Destroy;
end;


procedure TForm_Avanzado_Custom.DoCreate;
begin
  FEstadosAdmitidos := [wsNormal, wsMinimized, wsMaximized, wsFullScreen];

  if FForzarColorFormularioActivo then
    Color := FForzarColorFormularioColor;

  // Preconfiguracion
  FArchivoConfiguracion  := FGlobal_ArchivoConfiguracion;
  FCurdir                := ExtractFilePath(Application.Params[0]);
  FDirectorio_Aplicacion := FGlobal_Directorio_Aplicacion;
  FIsPostable            := false;


  FCerrarConESC := False;

  inherited DoCreate;

  FIsPreShow := false;

  Do_Corregir_Colores_Controles;
end;

function TForm_Avanzado_Custom.CloseQuery: boolean;
begin
  Result := inherited CloseQuery;

  if FGuardarPosicion_Activar then
    DoGuardarPosicioForm();
end;

procedure TForm_Avanzado_Custom.ActivarArchivoConfig(ArchivoIni : TMemIniFile);
begin
  if FGlobal_ArchivoConfiguracion <> nil then exit;
  FGlobal_ArchivoConfiguracion := ArchivoIni;
  FArchivoConfiguracion := FGlobal_ArchivoConfiguracion;
end;

procedure TForm_Avanzado_Custom.ActivarArchivoConfig(Archivo : String; GenerateCurDir : boolean = false; GenerateUserDir : boolean = false; GenerateUserDirGlobal : boolean = false; NombreDirConfig : string = '');
begin
  if NombreDirConfig = '' then
    FOnGetApplicationName := ChangeFileExt(ExtractFileName(Application.Params[0]), '')
  else
    FOnGetApplicationName := NombreDirConfig;

  OnGetApplicationName := @GetApplicationName;

  if FGlobal_ArchivoConfiguracion <> nil then exit;

  FDirectorio_Aplicacion := Get_Directorio_Save(Archivo, GenerateCurDir, GenerateUserDir, GenerateUserDirGlobal, NombreDirConfig);
  if FDirectorio_Aplicacion <> '' then
    FGlobal_Directorio_Aplicacion := FDirectorio_Aplicacion;

  FIsPostable         :=  DirectoryExists(Curdir + 'portable');
  {$HINTS OFF}
  Archivo := FDirectorio_Aplicacion {%H-}+ Archivo;
  {$HINTS ON}

  if FGlobal_ArchivoConfiguracion = nil then
    FGlobal_ArchivoConfiguracion := TMemInifile.Create(Archivo);

  FArchivoConfiguracion := FGlobal_ArchivoConfiguracion;
end;

procedure TForm_Avanzado_Custom.ActivarCursorLink(Objeto : TControl);
begin
  Objeto.Cursor := crHandPoint
end;

procedure TForm_Avanzado_Custom.ActivarGuardadoPosicion(SoloPosicion : boolean = false;  EstadoVentanaAdmitidos : TWindowStates = [wsNormal, wsMaximized, wsFullScreen]);
begin
  DisableAlign;
  try
    FGuardarPosicion_Activar  := true;

    FEstadosAdmitidos := EstadoVentanaAdmitidos;

    if FGuardarPosicion_Activar then
    begin
      DoRestaurarPosicionForm(SoloPosicion);
    end;

  finally
    EnableAlign;
  end;
end;


procedure TForm_Avanzado_Custom.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if FCerrarConESC then
    if Key <> 0 then
      if Key = 27 then
        close;
end;

procedure TForm_Avanzado_Custom.ActivarCerrarConESC;
begin
  FCerrarConESC := true;
  KeyPreview    := True;
end;

procedure TForm_Avanzado_Custom.Do_Corregir_Colores_Controles;
var
  i: integer;
  Control_Ex : TWinControl;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TWinControl then
    begin
      Control_Ex := TWinControl(Components[i]);
      if Control_Ex.Color = clwhite then
      begin
        Control_Ex.Color      := clwindow;
        Control_Ex.Font.Color := clwindowText;
      end
      else
        if Control_Ex.Color = clwindow then
          Control_Ex.Font.Color := clwindowText;
    end;
end;

procedure TForm_Avanzado_Custom.Corregir_Colores_Controles;
begin
  Do_Corregir_Colores_Controles;
end;

procedure TForm_Avanzado_Custom.DoGuardarPosicioForm();
begin
  if not assigned(FArchivoConfiguracion) then exit;
  try
    FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'ST', integer(WindowState));

    if WindowState = wsNormal then
      begin
        FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'X0', Left);
        FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'Y0', Top);
        FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'X1', Width);
        FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'Y1', Height);
      end;

    FArchivoConfiguracion.UpdateFile;
  except
  end;
end;

procedure TForm_Avanzado_Custom.DoRestaurarPosicionForm(SoloPosicion : boolean = false);
begin
  if not assigned(FArchivoConfiguracion) then exit;
  try
    Left   := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'X0', Left);
    Top    := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'Y0', Top);

    if not SoloPosicion then
    begin
      Width  := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'X1', Width);
      Height := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'Y1', Height);
    end;

    WindowState := TWindowState(FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'ST', integer(WindowState)));
  except
  end;
end;

procedure Formulario_Avanzado_Reset_Globales;
begin
  FGlobal_ArchivoConfiguracion := nil;
end;


// Muestra un mensaje y devuelve si se ha pulsado el botón Aceptar o Cancelar
function TForm_Avanzado_Custom.IsMessageBoxInfo(Mensaje, Titulo : String) : boolean;
var
 MensajeDialogo : longint;
begin
 MensajeDialogo := MessageBox(handle, Pchar(StringReplace(Mensaje, '\r', #13, [rfReplaceAll, rfIgnoreCase])), Pchar(Titulo), MB_YESNOCANCEL or MB_ICONINFORMATION);
 Result := MensajeDialogo = IDYES;
end;

// Muestra un mensaje y devuelve si se ha pulsado el botón Aceptar o Cancelar
function TForm_Avanzado_Custom.IsMessageBoxWarning(Mensaje, Titulo : String) : boolean;
var
 MensajeDialogo : longint;
begin
 MensajeDialogo := MessageBox(handle, Pchar(StringReplace(Mensaje, '\r', #13, [rfReplaceAll, rfIgnoreCase])), Pchar(Titulo), MB_YESNOCANCEL or MB_ICONWARNING);
 Result := MensajeDialogo = IDYES;
end;

initialization

finalization
  if FGlobal_ArchivoConfiguracion <> nil then
  begin
    FGlobal_ArchivoConfiguracion.free;
  end;

end.
