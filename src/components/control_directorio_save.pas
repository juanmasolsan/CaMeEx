(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-03-23 16:15:29
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-08 17:42:50
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

