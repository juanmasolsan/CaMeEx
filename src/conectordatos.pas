(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:30:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-15 13:51:44
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

    // Crea las tablas de la base de datos
    procedure CrearTablas();

    // Elimina una tabla de la base de datos
    procedure EliminarTabla(tabla : String);

    // Elimina todas las tablas de la base de datos
    procedure EliminarAllTablas();
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
begin
  FDataBase := TConexion_DB.Create(IncludeTrailingBackslash(SaveDir) + 'catalogos.db',
                  'sqlite-3',
                  '',
                  '',
                  //TODO: revisar para poder usarse en linux
                  IncludeTrailingBackslash(Curdir) + 'otros/'+{$IFDEF CPUX64} 'x64'{$ELSE} 'x86'{$ENDIF} + '/sqlite3.dll');

  // Crea las tablas si no existen
  CrearTablas();
end;

// Desconecta de la base de datos
procedure TConectorDatos.Finalizar;
begin
  if FDataBase = nil then exit;
  FDataBase.Free;
end;

// Conecta con la base de datos
procedure TConectorDatos.CrearTablas();
var
  SQL : string;

begin

  // Crea la tabla de catalogos
  SQL := 'CREATE TABLE Catalogos (' +
    'Id               INTEGER (8) PRIMARY KEY,' +
    'Nombre           TEXT        NOT NULL,' +
    'Descripcion      TEXT        NOT NULL,' +
    'Tipo             INTEGER     NOT NULL,' +
    'FechaCreacion    DATETIME    NOT NULL,' +
    'TotalArchivos    INTEGER     NOT NULL,' +
    'TotalDirectorios INTEGER     NOT NULL,' +
    'TotalSize        INTEGER (8) NOT NULL' +
    ');';

  FDataBase.SQL(SQL);

  // Crea la tabla de extensiones
  SQL := 'CREATE TABLE Extensiones (' +
    'Id          INTEGER (8) PRIMARY KEY,' +
    'Extension   TEXT        NOT NULL UNIQUE,' +
    'Descripcion TEXT        NOT NULL' +
    ');';


  FDataBase.SQL(SQL);

  // Inserta la extension sin extension
  SQL := 'INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (0, ".", "");';
  FDataBase.SQL(SQL);

  // Crea la tabla de rutas completas
  SQL := 'CREATE TABLE RutaCompleta (' +
    'Id         INTEGER (8) PRIMARY KEY,' +
    'IdCatalogo INTEGER (8) CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'Ruta       TEXT        NOT NULL' +
    ');';

  FDataBase.SQL(SQL);

  // Crea la tabla de datos
  SQL := 'CREATE TABLE Datos (' +
    'Id             INTEGER (8) PRIMARY KEY,' +
    'Tipo           INTEGER     NOT NULL,' +
    'Atributos      INTEGER     NOT NULL,' +
    'DateTime       DATETIME    NOT NULL,' +
    'Size           INTEGER     NOT NULL,' +
    'Nombre         TEXT        NOT NULL,' +
    'ImageIndex     INTEGER     NOT NULL,' +
    'IdExtension    INTEGER (8) CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
    'IdRutaCompleta INTEGER (8) CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'IdCatalogo     INTEGER (8) NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'IdPadre        INTEGER (8) CONSTRAINT FK_DATOS_PADRE REFERENCES Datos (Id) ON DELETE CASCADE ON UPDATE CASCADE' +
    ');';

  FDataBase.SQL(SQL);

end;


// Elimina una tabla de la base de datos
procedure TConectorDatos.EliminarTabla(tabla : String);
begin
  FDataBase.SQL('DROP TABLE IF EXISTS ' + tabla);
end;

// Elimina todas las tablas de la base de datos
procedure TConectorDatos.EliminarAllTablas();
begin
  EliminarTabla('Datos');
  EliminarTabla('Extensiones');
  EliminarTabla('RutaCompleta');
  EliminarTabla('Catalogos');
end;


end.