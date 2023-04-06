unit Control_Directorio_Save;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function Get_Directorio_Save(Archivo : String; GenerateCurDir : boolean = false; GenerateUserDir : boolean = false; {%H-}GenerateUserDirGlobal : boolean = false; NombreDirConfig : string = '') : string;


implementation

function Get_Directorio_Save(Archivo : String; GenerateCurDir : boolean = false; GenerateUserDir : boolean = false; GenerateUserDirGlobal : boolean = false; NombreDirConfig : string = '') : string;
var
  IsPortable       : Boolean;
  Extra_Config     : string;
  FCurdir          : string;
  FApplicationName : string;
  FDir_ProgramData : string;
begin
  Result       := '';
  FCurdir      := ExtractFilePath(paramstr(0));
  IsPortable   := fileexists(FCurdir + Archivo);
  Extra_Config := '';

  if NombreDirConfig = '' then
    FApplicationName := ChangeFileExt(ExtractFileName(paramstr(0)), '')
  else
    FApplicationName := NombreDirConfig;

  if IsPortable then
    GenerateCurDir := true
  else
  begin
    IsPortable  := directoryexists(FCurdir + 'portable');
    if IsPortable then
    begin
      GenerateCurDir := true;
      Extra_Config := 'portable';
    end;
  end;

  if GenerateCurDir then
    Result := ExcludeTrailingBackslash(FCurdir + Extra_Config)
  else
    if GenerateUserDir then
    begin
      Result := ExcludeTrailingBackslash(includeTrailingBackslash(GetUserDir + '.config')+ FApplicationName);
      if GenerateUserDirGlobal then
        begin
          FDir_ProgramData      := GetEnvironmentVariable('ProgramData');
          if FDir_ProgramData <> '' then
            Result := ExcludeTrailingBackslash(includeTrailingBackslash(FDir_ProgramData)+ FApplicationName);
        end;
    end;

    if Result <> '' then
    begin
      ForceDirectories(Result);
      Result        :=  Result + DirectorySeparator;
    end;

end;

end.

