(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:30:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-12 19:18:25
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

unit ConectorDatos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, InterfaceConectorDatos;

type
  { TConectorDatos }
  TConectorDatos = class(TInterfacedObject, IConectorDatos)
  public
    // Destructor
    destructor Destroy; override;

    // Conecta con la base de datos
    procedure Iniciar(Curdir: string; SaveDir : string);

    // Desconecta de la base de datos
    procedure Finalizar();
  end;

implementation

uses
  Control_DB
  ;

var
  FDataBase : TConexion_DB = nil;

// Destructor
destructor TConectorDatos.Destroy;
begin
  // Llama al destructor de la clase padre
  inherited Destroy;
end;

// Conecta con la base de datos
procedure TConectorDatos.Iniciar(Curdir: string; SaveDir : string);
var
  SQL : string;

begin
  FDataBase := TConexion_DB.Create(IncludeTrailingBackslash(SaveDir) + 'catalogos.db',
                  'sqlite-3',
                  '',
                  '',
                  //TODO: revisar para poder usarse en linux
                  IncludeTrailingBackslash(Curdir) + 'otros/'+{$IFDEF CPUX64} 'x64'{$ELSE} 'x86'{$ENDIF} + '/sqlite3.dll');

  SQL := 'CREATE TABLE IF NOT EXISTS Catalogos (';
  SQL := SQL + 'Id INTEGER PRIMARY KEY AUTOINCREMENT ';
  SQL := SQL + ', Nombre TEXT NOT NULL';
  SQL := SQL + ', Descripcion TEXT';
  SQL := SQL + ', FechaCreacion DATE NOT NULL';
  SQL := SQL + ', TotalArchivos INTEGER NOT NULL';
  SQL := SQL + ', TotalDirectorios INTEGER NOT NULL';
  SQL := SQL + ', TotalSize UNSIGNED BIG INT NOT NULL';
  SQL := SQL + ')';

  FDataBase.SQL(SQL);
end;


// Desconecta de la base de datos
procedure TConectorDatos.Finalizar;
begin
  if FDataBase = nil then exit;
  FDataBase.Free;
end;



end.