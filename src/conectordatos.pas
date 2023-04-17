(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:30:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-17 18:15:53
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
{$DEFINE TESTEAR_SENTENCIAS}


interface

uses
  Classes
  , SysUtils
  , InterfaceConectorDatos
  , ItemExtension
  , ItemRutaCompleta
  , ItemCatalogo
  , ItemDato
;



const
  SQL_INSERT_EXTENSION     = 'INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (:ID, :EXTENSION, :DESCRIPCION);';
  SQL_INSERT_RUTA_COMPLETA = 'INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (:ID, :IDCATALOGO, :RUTA);';
  SQL_INSERT_CATALOGO      = 'INSERT INTO Catalogos (Id, Nombre, Descripcion, Tipo, Fecha, TotalArchivos, TotalDirectorios, TotalSize) VALUES (:ID, :NOMBRE, :DESCRIPCION, :TIPO, :FECHA, :TOTALARCHIVOS, :TOTALDIRECTORIOS, :TOTALSIZE);';
  SQL_INSERT_DATO          = 'INSERT INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (:ID, :TIPO, :ATRIBUTOS, :FECHA, :SIZE, :NOMBRE, :IMAGEINDEX, :IDEXTENSION, :IDRUTACOMPLETA, :IDCATALOGO, :IDPADRE);';



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

    // Añade una extension a la base de datos
    procedure AddExtension(Extension : TItemExtension);

    // Añade una ruta completa a la base de datos
    procedure AddRutaCompleta(Ruta : TItemRutaCompleta);

    // Añade un catálogo a la base de datos
    procedure AddCatalogo(Catalogo : TItemCatalogo);

    // Añade un dato a la base de datos
    procedure AddDato(Dato : TItemDato);


{$IFDEF TESTEAR_SENTENCIAS}
    // Para testear sentencias
    procedure TestSentencias();
{$ENDIF TESTEAR_SENTENCIAS}

  end;


implementation

uses
  db
  , sqldb
  , Control_DB
  , ItemBaseDatos
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

{$IFDEF TESTEAR_SENTENCIAS}
  //
  TestSentencias();
{$ENDIF TESTEAR_SENTENCIAS}
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
    'Id               BIGINT PRIMARY KEY,' +
    'Nombre           TEXT        NOT NULL,' +
    'Descripcion      TEXT        NOT NULL,' +
    'Tipo             INTEGER     NOT NULL,' +
    'Fecha            DATETIME    NOT NULL,' +
    'TotalArchivos    INTEGER     NOT NULL,' +
    'TotalDirectorios INTEGER     NOT NULL,' +
    'TotalSize        BIGINT NOT NULL' +
    ');';

  FDataBase.SQL(SQL);

  // Crea la tabla de extensiones
  SQL := 'CREATE TABLE Extensiones (' +
    'Id          BIGINT PRIMARY KEY,' +
    'Extension   TEXT NOT NULL UNIQUE,' +
    'Descripcion TEXT NOT NULL' +
    ');';


  FDataBase.SQL(SQL);

  // Inserta la extension sin extension
  SQL := 'INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (0, ".", "");';
  FDataBase.SQL(SQL);

  // Crea la tabla de rutas completas
  SQL := 'CREATE TABLE RutaCompleta (' +
    'Id         BIGINT PRIMARY KEY,' +
    'IdCatalogo BIGINT CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'Ruta       TEXT   NOT NULL' +
    ');';

  FDataBase.SQL(SQL);

  // Crea el indice de la tabla de rutas completas
  SQL := 'CREATE INDEX RutaCompleta_Ruta_IDX ON RutaCompleta (Ruta);';
  FDataBase.SQL(SQL);

  // Crea la tabla de datos
  SQL := 'CREATE TABLE Datos (' +
    'Id             BIGINT PRIMARY KEY,' +
    'Tipo           INTEGER     NOT NULL,' +
    'Atributos      INTEGER     NOT NULL,' +
    'Fecha          DATETIME    NOT NULL,' +
    'Size           INTEGER     NOT NULL,' +
    'Nombre         TEXT        NOT NULL,' +
    'ImageIndex     INTEGER     NOT NULL,' +
    'IdExtension    BIGINT CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
    'IdRutaCompleta BIGINT CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'IdCatalogo     BIGINT NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'IdPadre        BIGINT CONSTRAINT FK_DATOS_PADRE REFERENCES Datos (Id) ON DELETE CASCADE ON UPDATE CASCADE' +
    ');';

  FDataBase.SQL(SQL);

  // Crea el índice de la tabla de datos
  SQL := 'CREATE INDEX Datos_Nombre_IDX ON Datos (Nombre);';
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


// Añade una extension a la base de datos
procedure TConectorDatos.AddExtension(Extension : TItemExtension);
begin
  try
    if FDataBase.Query <> nil then
    begin
      // Prepara la query
      FDataBase.Query.Close;
      FDataBase.Query.SQL.Clear;
      FDataBase.Query.SQL.Add(SQL_INSERT_EXTENSION);

      // Hace la inserción con un prepared statement
      FDataBase.Query.ParamByName('ID').AsLargeInt        := Extension.Id;
      FDataBase.Query.ParamByName('EXTENSION').AsString   := Extension.Nombre;
      FDataBase.Query.ParamByName('DESCRIPCION').AsString := Extension.Descripcion;

      // Realiza la inserción
      FDataBase.Query.ExecSQL;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;


// Añade una ruta completa a la base de datos
procedure TConectorDatos.AddRutaCompleta(Ruta : TItemRutaCompleta);
begin
  try
    if FDataBase.Query <> nil then
    begin
      // Prepara la query
      FDataBase.Query.Close;
      FDataBase.Query.SQL.Clear;
      FDataBase.Query.SQL.Add(SQL_INSERT_RUTA_COMPLETA);

      // Hace la inserción con un prepared statement
      FDataBase.Query.ParamByName('ID').AsLargeInt         := Ruta.Id;
      FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt := Ruta.IdCatalogo;
      FDataBase.Query.ParamByName('RUTA').AsString         := Ruta.Nombre;

      // Realiza la inserción
      FDataBase.Query.ExecSQL;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;

// Añade un catálogo a la base de datos
procedure TConectorDatos.AddCatalogo(Catalogo : TItemCatalogo);
begin
  try
    if FDataBase.Query <> nil then
    begin
      // Prepara la query
      FDataBase.Query.Close;
      FDataBase.Query.SQL.Clear;
      FDataBase.Query.SQL.Add(SQL_INSERT_CATALOGO);

      // Hace la inserción con un prepared statement
      FDataBase.Query.ParamByName('ID').AsLargeInt               := Catalogo.Id;
      FDataBase.Query.ParamByName('NOMBRE').AsString             := Catalogo.Nombre;
      FDataBase.Query.ParamByName('DESCRIPCION').AsString        := Catalogo.Descripcion;
      FDataBase.Query.ParamByName('TIPO').AsInteger              := integer(Catalogo.Tipo);
      FDataBase.Query.ParamByName('FECHA').AsDateTime            := Catalogo.Fecha;
      FDataBase.Query.ParamByName('TOTALARCHIVOS').AsLargeInt    := Catalogo.TotalArchivos;
      FDataBase.Query.ParamByName('TOTALDIRECTORIOS').AsLargeInt := Catalogo.TotalDirectorios;
      FDataBase.Query.ParamByName('TOTALSIZE').AsLargeInt        := Catalogo.Size;

      // Realiza la inserción
      FDataBase.Query.ExecSQL;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;


// Añade un dato a la base de datos
procedure TConectorDatos.AddDato(Dato : TItemDato);
begin
  try
    if FDataBase.Query <> nil then
    begin
      // Prepara la query
      FDataBase.Query.Close;
      FDataBase.Query.SQL.Clear;
      FDataBase.Query.SQL.Add(SQL_INSERT_DATO);

      // Hace la inserción con un prepared statement
      FDataBase.Query.ParamByName('ID').AsLargeInt             := Dato.Id;
      FDataBase.Query.ParamByName('TIPO').AsInteger            := integer(Dato.Tipo);
      FDataBase.Query.ParamByName('ATRIBUTOS').AsInteger       := Dato.Atributos;
      FDataBase.Query.ParamByName('FECHA').AsDateTime          := Dato.Fecha;
      FDataBase.Query.ParamByName('SIZE').AsLargeInt           := Dato.Size;
      FDataBase.Query.ParamByName('NOMBRE').AsString           := Dato.Nombre;
      FDataBase.Query.ParamByName('IMAGEINDEX').AsInteger      := Dato.ImageIndex;
      FDataBase.Query.ParamByName('IDEXTENSION').AsLargeInt    := Dato.IdExtension;
      FDataBase.Query.ParamByName('IDRUTACOMPLETA').AsLargeInt := Dato.IdRutaCompleta;
      FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt     := Dato.IdCatalogo;
      FDataBase.Query.ParamByName('IDPADRE').AsLargeInt        := Dato.IdPadre;

      // Realiza la inserción
      FDataBase.Query.ExecSQL;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;




{$IFDEF TESTEAR_SENTENCIAS}
procedure TConectorDatos.TestSentencias();

  procedure InsertarExtensiones();
  var
    i  : integer;
    max: integer = 10;
    ext: TItemExtension;

  begin
    for i := 0 to max do
    begin
      ext := TItemExtension.Create('.ext_'+ inttostr(i), 'Descripcion de la extension ' + inttostr(i));
      try
        AddExtension(ext);
      finally
        ext.Free;
      end;
    end;
  end;

  procedure InsertarRutasCompletas();
  var
    i  : integer;
    max: integer = 10;
    ruta: TItemRutaCompleta;

  begin
    for i := 0 to max do
    begin
      ruta := TItemRutaCompleta.Create('directorio/loquesea_' + inttostr(i), 1);
      try
        AddRutaCompleta(ruta);
      finally
        ruta.Free;
      end;
    end;
  end;

  procedure InsertarCatalogo();
  var
    i  : integer;
    max: integer = 0;
    Catalogo: TItemCatalogo;

  begin
    for i := 0 to max do
    begin
      Catalogo := TItemCatalogo.Create('Catalogo_' + inttostr(i), TItemDatoTipo.Root, now, 300, 'Descripcion del catalogo ' + inttostr(i), 1 + i, 1 + i );
      try
        AddCatalogo(Catalogo);
      finally
        Catalogo.Free;
      end;
    end;
  end;

  procedure InsertarDatos();
  var
    Dato: TItemDato;

  begin

      Dato := TItemDato.create('prueba.txt', TItemDatoTipo.Archivo, now, 1204,
        100,
        '',
        0,
        0,
        0,
        1,
        1);
      try
        AddDato(Dato);
      finally
        Dato.Free;
      end;


      Dato := TItemDato.create('prueba.dir', TItemDatoTipo.Directorio, now, 0,
        100,
        '',
        0,
        0,
        0,
        1,
        1);
      try
        AddDato(Dato);
      finally
        Dato.Free;
      end;
  end;

begin

  // Inserta extensiones
  InsertarExtensiones();

  // Inserta rutas  completas
  InsertarRutasCompletas();

  // Inserta Catalogo
  InsertarCatalogo();

  // Inserta datos
  InsertarDatos();
end;

{$ENDIF TESTEAR_SENTENCIAS}


end.
