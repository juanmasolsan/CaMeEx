unit Control_Formulario_Avanzado;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf
  , LCLType
  , LMessages
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
  { TForm_Avanzado_Custom }
  TForm_Avanzado_Custom = class(TForm)
  private
    FCurdir : String;
    FArchivoConfiguracion : TMemInifile;
    FDirectorio_Aplicacion : string;
    FIsPostable            : boolean;

    FGuardarPosicion_Activar  : boolean;
    FIsPreShow                : boolean;

    FCerrarConESC : boolean;


  protected
    procedure DoCreate; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Do_Corregir_Colores_Controles;
    procedure DoGuardarPosicioForm();
    procedure DoRestaurarPosicionForm();
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

    procedure ActivarGuardadoPosicion(SoloPosicion : boolean = false); virtual;

    procedure ActivarCerrarConESC; virtual;

    procedure Corregir_Colores_Controles;
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

procedure TForm_Avanzado_Custom.ActivarGuardadoPosicion(SoloPosicion : boolean = false);
begin
  DisableAlign;
  try
    FGuardarPosicion_Activar  := true;

    if FGuardarPosicion_Activar then
    begin
      DoRestaurarPosicionForm();
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
    FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'X0', Left);
    FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'Y0', Top);
    FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'X1', Width);
    FArchivoConfiguracion.WriteInteger('Posicion Ventana ' + Name, 'Y1', Height);
    FArchivoConfiguracion.UpdateFile;
  except
  end;
end;

procedure TForm_Avanzado_Custom.DoRestaurarPosicionForm();
begin
  if not assigned(FArchivoConfiguracion) then exit;
  try
    Left   := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'X0', Left);
    Top    := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'Y0', Top);
    Width  := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'X1', Width);
    Height := FArchivoConfiguracion.ReadInteger('Posicion Ventana ' + Name, 'Y1', Height);
  except
  end;
end;

procedure Formulario_Avanzado_Reset_Globales;
begin
  FGlobal_ArchivoConfiguracion := nil;
end;





initialization

finalization
  if FGlobal_ArchivoConfiguracion <> nil then
  begin
    FGlobal_ArchivoConfiguracion.free;
  end;

end.