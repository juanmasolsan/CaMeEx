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

unit MotorScan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils;

type
  { TMotorScanCustom }
  TMotorScanCustom = class
  private
  protected
  public
    procedure ScanDir(Lista_Directorio, Lista_Directorio_Padre : RawByteString);
  end;


type
  { TMotorScan }
  TMotorScan = class(TMotorScanCustom);


implementation

procedure TMotorScanCustom.ScanDir(Lista_Directorio, Lista_Directorio_Padre : RawByteString);
var
  RootName   : RawByteString;
  DosError   : Integer;
  SearchRec  : TSearchRec;
  FlagsDir   : Integer;
  ListaDirs  : TStringList;
  total, t : integer;

  {$IFDEF UNIX}
    Actual   : RawByteString;
  {$ENDIF}

  Codificar : boolean;

  Inicio    : QWord;

  Dir_Actual : RawByteString;
  Dir_Padre  : RawByteString;

  ContinuarRastreo : Boolean;

  TerminarRastreo : boolean;
  Is_Dir          : boolean;

  Total_Encontrados : int64 = 0;
  Actual_Lista_Directorio : RawByteString;

begin
  try

    {$WARNINGS-}
      FlagsDir := faAnyFile or faReadOnly or faDirectory or faSymLink


      FlagsDir := FlagsDir or faHidden or faSysFile;
    {$WARNINGS+}

    RootName := ProcessRutaCompleta(Lista_Directorio);
    Codificar := DirectoryExistsUTF8(ExtractFilePath(RootName));

    if not Codificar then
      begin
        RootName         := UTF8Encode(RootName);
        Lista_Directorio := UTF8Encode(Lista_Directorio);
      end;

    {$HINTS OFF}
    fillchar(SearchRec, sizeof(SearchRec), 0);
    {$HINTS ON}

    Dir_Actual := IncludeTrailingBackslash(Lista_Directorio);
    Dir_Padre  := ExtraerDirectoryPadre(Dir_Actual);
    TerminarRastreo := false;

    DosError := FindFirstUtf8(RootName, FlagsDir, SearchRec, FOnlyDir);
    try
(*
      while DosError = 0 do
      begin

        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')and (SearchRec.Name <> '') then
        begin
          if TerminarRastreo then
          begin
            break;
          end;

          if ContinuarRastreo then
          begin
            Is_Dir := (SearchRec.Attr and faDirectory = faDirectory);

            CrearShellDir(SearchRec, Dir_Actual, Dir_Padre, not FSoloListar);
          end;
        end;


        DosError := FindNextUtf8(SearchRec);
        if FDetener_Escaneo_Busqueda_Archivos then
        DosError := 1;

        if WF_Detener_Escaneo_Busqueda_Archivos then
        DosError := 1;
      end;
*)
    finally
      FindCloseUTF8(SearchRec);
    end;


  finally
    //
  end;


end;




end.

