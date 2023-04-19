(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:30:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-19 18:34:35
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
  LCLProc
  , LCLType
  , LCLIntf
  , Classes
  , SysUtils
  , InterfaceConectorDatos
  , ItemExtension
  , ItemRutaCompleta
  , ItemCatalogo
  , ItemDato
, sqldb;



const
  SQL_INSERT_EXTENSION      = 'INSERT OR IGNORE INTO Extensiones (Id, Extension, Descripcion) VALUES (:ID, :EXTENSION, :DESCRIPCION);';
  SQL_INSERT_RUTA_COMPLETA  = 'INSERT OR IGNORE INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (:ID, :IDCATALOGO, :RUTA);';
  SQL_INSERT_CATALOGO       = 'INSERT OR IGNORE INTO Catalogos (Id, Nombre, Descripcion, Tipo, Fecha, TotalArchivos, TotalDirectorios, TotalSize) VALUES (:ID, :NOMBRE, :DESCRIPCION, :TIPO, :FECHA, :TOTALARCHIVOS, :TOTALDIRECTORIOS, :TOTALSIZE);';
  SQL_INSERT_DATO           = 'INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (:ID, :TIPO, :ATRIBUTOS, :FECHA, :SIZE, :NOMBRE, :IMAGEINDEX, :IDEXTENSION, :IDRUTACOMPLETA, :IDCATALOGO, :IDPADRE);';

  SQL_SELECT_CATALOGO_ALL             = 'SELECT * FROM Catalogos;';
  SQL_SELECT_CATALOGO_BY_ID           = 'SELECT * FROM Catalogos WHERE id = :ID;';
  SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID = 'SELECT dt.*, rc.Ruta, ex.Descripcion FROM Datos as dt JOIN RutaCompleta AS rc ON dt.IdRutaCompleta = rc.Id JOIN Extensiones AS ex ON dt.IdExtension = ex.Id WHERE dt.IdCatalogo = :IDCATALOGO';
  SQL_SELECT_DATOS_ALL_BY_PARENT_ID   = SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID + ' AND dt.IdPadre = :IDPADRE';

type
  { TConectorDatos }
  TConectorDatos = class(TInterfacedObject, IConectorDatos)
  private
    FCriticalSection: TCriticalSection;
  protected
    function DoGetCatalogoFromQuery(Query : TSQLQuery) : TItemCatalogo;
    function DoGetDatoFromQuery(Query : TSQLQuery) : TItemDato;
  public
    // Constructor
    constructor Create;

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

    // Devuelve todos los catalogos
    function GetAllCatalogos() : TArrayItemDato;

    // Devuelve un catalogo por su id
    function GetCatalogosById(id : qword) : TItemCatalogo;

    // Devuelve la lista de datos que contiene un catalogo y que desciendan de un padre
    function GetDatos(IdCatalogo : qword; IdPadre : qword) : TArrayItemDato;


{$IFDEF TESTEAR_SENTENCIAS}
    // Para testear sentencias
    procedure TestSentencias();
{$ENDIF TESTEAR_SENTENCIAS}

  end;


implementation

uses
  db
  , Control_DB
  , ItemBaseDatos
  ;

var
  FDataBase : TConexion_DB = nil;


// Constructor
constructor TConectorDatos.Create;
begin
  // Llama al constructor de la clase padre
  inherited Create;

  // Inicializa la seccion critica
  InitializeCriticalSection(FCriticalSection);
end;

// Destructor
destructor TConectorDatos.Destroy;
begin
  // Destruimos la seccion critica
  DeleteCriticalSection(FCriticalSection);

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

  // Se inicia la seccion critica
  EnterCriticalSection(FCriticalSection);
  try
    // Crea la tabla de catalogos
    SQL := 'CREATE TABLE IF NOT EXISTS Catalogos (' +
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
    SQL := 'CREATE TABLE IF NOT EXISTS Extensiones (' +
      'Id          BIGINT PRIMARY KEY,' +
      'Extension   TEXT NOT NULL UNIQUE,' +
      'Descripcion TEXT NOT NULL' +
      ');';


    FDataBase.SQL(SQL);

    // Inserta la extension sin extension
    SQL := 'INSERT OR IGNORE INTO Extensiones (Id, Extension, Descripcion) VALUES (0, ".", "");';
    FDataBase.SQL(SQL);

    // Crea la tabla de rutas completas
    SQL := 'CREATE TABLE IF NOT EXISTS RutaCompleta (' +
      'Id         BIGINT PRIMARY KEY,' +
      'IdCatalogo BIGINT CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'Ruta       TEXT   NOT NULL' +
      ');';

    FDataBase.SQL(SQL);

    // Crea el indice de la tabla de rutas completas
    SQL := 'CREATE INDEX IF NOT EXISTS RutaCompleta_Ruta_IDX ON RutaCompleta (Ruta);';
    FDataBase.SQL(SQL);

    // Crea la tabla de datos
    SQL := 'CREATE TABLE IF NOT EXISTS Datos (' +
      'Id             BIGINT PRIMARY KEY,' +
      'Tipo           INTEGER     NOT NULL,' +
      'Atributos      INTEGER     NOT NULL,' +
      'Fecha          DATETIME    NOT NULL,' +
      'Size           BIGINT      NOT NULL,' +
      'Nombre         TEXT        NOT NULL,' +
      'ImageIndex     INTEGER     NOT NULL,' +
      'IdExtension    BIGINT CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
      'IdRutaCompleta BIGINT CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'IdCatalogo     BIGINT NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'IdPadre        BIGINT CONSTRAINT FK_DATOS_PADRE REFERENCES Datos (Id) ON DELETE CASCADE ON UPDATE CASCADE' +
      ');';

    FDataBase.SQL(SQL);

    // Crea el índice de la tabla de datos
    SQL := 'CREATE INDEX IF NOT EXISTS Datos_Nombre_IDX ON Datos (Nombre);';
    FDataBase.SQL(SQL);

  finally
    // Se finaliza la seccion critica
    LeaveCriticalSection(FCriticalSection);
  end;

end;


// Elimina una tabla de la base de datos
procedure TConectorDatos.EliminarTabla(tabla : String);
begin
  // Se inicia la seccion critica
  EnterCriticalSection(FCriticalSection);
  try
    FDataBase.SQL('DROP TABLE IF EXISTS ' + tabla);
  finally
    // Se finaliza la seccion critica
    LeaveCriticalSection(FCriticalSection);
  end;
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
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
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
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
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
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
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
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
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
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try

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
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
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
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
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
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;

function TConectorDatos.DoGetCatalogoFromQuery(Query : TSQLQuery) : TItemCatalogo;
begin
  // Crea el catalogo
  Result := TItemCatalogo.Create(
    Query.FieldByName('NOMBRE').AsString,
    TItemDatoTipo(Query.FieldByName('TIPO').AsInteger),
    Query.FieldByName('FECHA').AsDateTime,
    Query.FieldByName('TOTALSIZE').AsLargeInt,
    Query.FieldByName('DESCRIPCION').AsString,
    Query.FieldByName('TOTALARCHIVOS').AsLargeInt,
    Query.FieldByName('TOTALDIRECTORIOS').AsLargeInt
  );

  // Añade el id
  Result.Id := QWord(FDataBase.Query.FieldByName('ID').AsLargeInt);
end;

function TConectorDatos.DoGetDatoFromQuery(Query : TSQLQuery) : TItemDato;
begin
  // Crea el Dato
  Result := TItemDato.Create(
          Query.FieldByName('NOMBRE').AsString,
          TItemDatoTipo(Query.FieldByName('TIPO').AsInteger),
          Query.FieldByName('FECHA').AsDateTime,
          Query.FieldByName('SIZE').AsLargeInt,
          Query.FieldByName('ATRIBUTOS').AsInteger,
          Query.FieldByName('DESCRIPCION').AsString,
          Query.FieldByName('ImageIndex').AsInteger,


          Query.FieldByName('IDEXTENSION').AsLargeInt,
          Query.FieldByName('IDRUTACOMPLETA').AsLargeInt,
          Query.FieldByName('IDCATALOGO').AsLargeInt,
          Query.FieldByName('IDPADRE').AsLargeInt
  );

  // Añade el id
  Result.Id := QWord(FDataBase.Query.FieldByName('ID').AsLargeInt);
end;




// Devuelve todos los catalogos
function TConectorDatos.GetAllCatalogos() : TArrayItemDato;
var
  catalogo: TItemCatalogo;
begin
  // Inicializa el resultado
  Result := nil;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;

        // Ejecuta la sentencia
        FDataBase.SQL(SQL_SELECT_CATALOGO_ALL);
        try
          // Comprueba que tiene datos
          if FDataBase.Query.IsEmpty then exit;

          // Inicializa el resultado
          Result := TArrayItemDato.Create();

          // Obtinene el primer registro
          FDataBase.Query.First;

          // Recorre los registros
          while not FDataBase.Query.EOF do
          begin
            // Crea el catalogo
            catalogo := DoGetCatalogoFromquery(FDataBase.Query);

            // Añade el catalogo al resultado
            {%H-}Result.Add(catalogo);

            // Pasa al siguiente registro
            FDataBase.Query.Next;
          end;

        finally
          // Cierra la query
          FDataBase.Query.Close;
        end;
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;

// Devuelve un catalogo por su id
function TConectorDatos.GetCatalogosById(id : qword) : TItemCatalogo;
begin
  // Inicializa el resultado
  Result := nil;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;
        FDataBase.Query.SQL.Add(SQL_SELECT_CATALOGO_BY_ID);

        // Hace la inserción con un prepared statement
        FDataBase.Query.ParamByName('ID').AsLargeInt             := Id;

        // Ejecuta la sentencia
        //FDataBase.SQL(SQL_SELECT_CATALOGO_ALL);
        FDataBase.Query.Open;
        try
          // Comprueba que tiene datos
          if FDataBase.Query.IsEmpty then exit;

          // Inicializa el resultado
          Result := nil;

          // Obtinene el primer registro
          FDataBase.Query.First;

          // Recorre los registros
          while not FDataBase.Query.EOF do
          begin
            // Crea el catalogo
            Result := DoGetCatalogoFromquery(FDataBase.Query);

            // Pasa al siguiente registro
            FDataBase.Query.Next;
          end;

        finally
          // Cierra la query
          FDataBase.Query.Close;
        end;
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
    end;
  except
    //TODO: Añadir Gestión de Excepción
    //on e: Exception do
  end;
end;

// Devuelve la lista de datos que contiene un catalogo y que desciendan de un padre
function TConectorDatos.GetDatos(IdCatalogo : qword; IdPadre : qword) : TArrayItemDato;
var
  dato: TItemDato;
begin
  // Inicializa el resultado
  Result := nil;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;

        // Ejecuta la sentencia
        //FDataBase.SQL(SQL_SELECT_CATALOGO_ALL);

        // Prepara la query
        if IdPadre = 0 then
        begin
          FDataBase.Query.SQL.Add(SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID);
        end
        else
        begin
          FDataBase.Query.SQL.Add(SQL_SELECT_DATOS_ALL_BY_PARENT_ID);
        end;

        // Hace la inserción con un prepared statement
        FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt := IdCatalogo;
        FDataBase.Query.ParamByName('IDPADRE').AsLargeInt    := IdPadre;

        // Ejecuta la sentencia
        FDataBase.Query.Open;
        try
          // Comprueba que tiene datos
          if FDataBase.Query.IsEmpty then exit;

          // Inicializa el resultado
          Result := TArrayItemDato.Create();

          // Obtinene el primer registro
          FDataBase.Query.First;

          // Recorre los registros
          while not FDataBase.Query.EOF do
          begin
            // Crea el catalogo
            dato := DoGetDatoFromquery(FDataBase.Query);

            // Añade el catalogo al resultado
            {%H-}Result.Add(dato);

            // Pasa al siguiente registro
            FDataBase.Query.Next;
          end;

        finally
          // Cierra la query
          FDataBase.Query.Close;
        end;
      finally
        // Se finaliza la seccion critica
        LeaveCriticalSection(FCriticalSection);
      end;
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


  var
    catalogos: TArrayItemDato;
    Cat : TItemCatalogo;
    Datos : TArrayItemDato;

  procedure GetTodosCatalogos();
  begin
    catalogos := GetAllCatalogos();
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

  // Obtiene todos los catalogos
  GetTodosCatalogos();
  try
    if catalogos <> nil then
    begin
        Cat := GetCatalogosById(catalogos[0]{%H-}.Id);

        if Cat <> nil then
        begin
          Cat.Free;
        end;

      // Obtiene todos los datos del catalogo 0
      Datos := GetDatos(catalogos[0]{%H-}.Id, 0);
      if Datos <> nil then
      begin
        Datos.Free;
      end;

      // Obtiene todos los datos del catalogo 0 hijos del dato 1
      Datos := GetDatos(catalogos[0]{%H-}.Id, 200);
      if Datos <> nil then
      begin
        Datos.Free;
      end;
    end;
  finally
    if catalogos <> nil then
    begin
      catalogos.Free;
    end;
  end;

end;

{$ENDIF TESTEAR_SENTENCIAS}


end.
