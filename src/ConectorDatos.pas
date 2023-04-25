(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-12 18:30:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-25 22:58:30
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
{.$DEFINE TESTEAR_SENTENCIAS}

{$IFDEF TESTEAR_SENTENCIAS}
  {-$DEFINE TESTEAR_SENTENCIAS_ELIMINAR_TABLAS}
  {.$DEFINE TESTEAR_SENTENCIAS_INSERTAR_EXTENSIONES}
  {.$DEFINE TESTEAR_SENTENCIAS_INSERTAR_CATALOGO}
  {$DEFINE TESTEAR_SENTENCIAS_UPDATE_CATALOGO}
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_RUTAS_COMPLETAS}
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_DATOS}
  {$DEFINE TESTEAR_SENTENCIAS_DELETE_DATOS}
  {.$DEFINE TESTEAR_SENTENCIAS_DELETE_CATALOGOS}
{$ENDIF}

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
  SQL_INSERT_EXTENSION                     = 'INSERT OR IGNORE INTO Extensiones (Id, Extension, Descripcion) VALUES (:ID, :EXTENSION, :DESCRIPCION);';
  SQL_INSERT_RUTA_COMPLETA                 = 'INSERT OR IGNORE INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (:ID, :IDCATALOGO, :RUTA);';
  SQL_INSERT_CATALOGO                      = 'INSERT OR IGNORE INTO Catalogos (Id, Nombre, Descripcion, Tipo, Fecha, TotalArchivos, TotalDirectorios, TotalSize) VALUES (:ID, :NOMBRE, :DESCRIPCION, :TIPO, :FECHA, :TOTALARCHIVOS, :TOTALDIRECTORIOS, :TOTALSIZE);';
  SQL_INSERT_DATO_PADRE                    = 'INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (:ID, :TIPO, :ATRIBUTOS, :FECHA, :SIZE, :NOMBRE, :IMAGEINDEX, :IDEXTENSION, :IDRUTACOMPLETA, :IDCATALOGO, :IDPADRE);';
  SQL_INSERT_DATO                          = 'INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo) VALUES (:ID, :TIPO, :ATRIBUTOS, :FECHA, :SIZE, :NOMBRE, :IMAGEINDEX, :IDEXTENSION, :IDRUTACOMPLETA, :IDCATALOGO);';

  SQL_SELECT_CATALOGO_ALL                  = 'SELECT * FROM Catalogos;';
  SQL_SELECT_CATALOGO_BY_ID                = 'SELECT * FROM Catalogos WHERE id = :ID;';
  SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID      = 'SELECT dt.*, rc.Ruta, ex.Descripcion FROM Datos as dt JOIN RutaCompleta AS rc ON dt.IdRutaCompleta = rc.Id JOIN Extensiones AS ex ON dt.IdExtension = ex.Id WHERE dt.IdCatalogo = :IDCATALOGO';
  SQL_SELECT_DATOS_ALL_BY_PARENT_ID        = SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID + ' AND dt.IdPadre = :IDPADRE';

  SQL_DELETE_CATALOGO_BY_ID                = 'DELETE FROM Catalogos WHERE Id = :IDCATALOGO;';
  SQL_DELETE_DATOS_BY_ID_CATALOGO          = 'DELETE FROM Datos WHERE IdCatalogo = :IDCATALOGO';
  SQL_DELETE_DATO_BY_IDS                   = SQL_DELETE_DATOS_BY_ID_CATALOGO + ' AND (Id = :ID OR IdPadre = :ID);';
  SQL_DELETE_RUTA_COMPLETA_BY_ID_CATALOGO  = 'DELETE FROM RutaCompleta WHERE IdCatalogo = :IDCATALOGO;';
  SQL_DELETE_RUTA_COMPLETA_BY_IDS          = SQL_DELETE_RUTA_COMPLETA_BY_ID_CATALOGO + ' AND Id = :ID;';
  SQL_DELETE_RUTA_COMPLETA_SIN_REFERENCIAS = 'DELETE FROM RutaCompleta WHERE Id = :ID AND IdCatalogo = :IDCATALOGO AND NOT EXISTS (SELECT 1 FROM Datos WHERE IdRutaCompleta = :ID);';

  SQL_UPDATE_CATALOGO                      = 'UPDATE Catalogos SET Nombre=:NOMBRE, Descripcion=:DESCRIPCION, Tipo=:TIPO, Fecha=:FECHA WHERE Id=:ID;';


type
  { TConectorDatos }
  TConectorDatos = class(TInterfacedObject, IConectorDatos)
  private
    FCriticalSection: TCriticalSection;
  protected
    function DoGetCatalogoFromQuery(Query : TSQLQuery) : TItemCatalogo;
    function DoGetDatoFromQuery(Query : TSQLQuery) : TItemDato;
    function DoDeleteFromTableByIdParametro(Id : qword; Sql : string; const Parametro : string) : boolean;

    // Elimina un Item de una tabla
    function DoDeleteItemFromTableById(IdCatalogo : qword; Id : qword; Sql : string) : boolean;

    // Elimina las rutas completas que no tengan referencias en la tabla Datos
    function DoDeleteRutasCompletasSinReferencias(Dato : TItemDato) : boolean;

    // Añade/Actualiza un catálogo a la base de datos
    procedure DoInserteUpdateCatalogo(Catalogo : TItemCatalogo; IsUpdate : boolean);

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
    function GetDatos(Catalogo : TItemCatalogo; Padre : TItemDato) : TArrayItemDato;

    // Elimina todos los catalogos
    function DeleteAllCatalogos() : boolean;

    // Elimina un catalogo
    function DeleteCatalogo(Catalogo : TItemCatalogo) : boolean;

    // Elimina un dato
    function DeleteDato(Dato : TItemDato) : boolean;

    // Actualiza un catálogo
    procedure UpdateCatalogo(Catalogo : TItemCatalogo);

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

{$IFDEF TESTEAR_SENTENCIAS_ELIMINAR_TABLAS}
  // Al estar activo esto, elimina todas las tablas
  EliminarAllTablas();
{$ENDIF}

  // Se inicia la seccion critica
  EnterCriticalSection(FCriticalSection);
  try
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
        'IdCatalogo BIGINT CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
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
        'IdPadre        BIGINT,' +
        'IdExtension    BIGINT CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
        'IdRutaCompleta BIGINT CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,' +
        'IdCatalogo     BIGINT NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT' +
        ');';

      FDataBase.SQL(SQL);

      // Crea el índice de la tabla de datos
      SQL := 'CREATE INDEX IF NOT EXISTS Datos_Nombre_IDX ON Datos (Nombre);';
      FDataBase.SQL(SQL);

    finally
      // Cierra la query
      FDataBase.Query.Close;
    end;

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

        try
          // Realiza la inserción
          FDataBase.Query.ExecSQL;
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

        try
          // Realiza la inserción
          FDataBase.Query.ExecSQL;
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

// Añade un catálogo a la base de datos
procedure TConectorDatos.AddCatalogo(Catalogo : TItemCatalogo);
begin
  DoInserteUpdateCatalogo(Catalogo, false);
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

        if Dato.IdPadre = 0 then
        begin
          FDataBase.Query.SQL.Add(SQL_INSERT_DATO);
        end
        else
        begin
          FDataBase.Query.SQL.Add(SQL_INSERT_DATO_PADRE);
          FDataBase.Query.ParamByName('IDPADRE').AsLargeInt      := Dato.IdPadre;
        end;

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

        try
          // Realiza la inserción
          FDataBase.Query.ExecSQL;
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
function TConectorDatos.GetDatos(Catalogo : TItemCatalogo; Padre : TItemDato) : TArrayItemDato;
var
  dato: TItemDato;
  IdCatalogo : qword;
  IdPadre : qword = 0;
begin
  IdCatalogo := Catalogo.Id;

  if Padre <> nil then
  begin
    IdPadre := Padre.Id;
  end;

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

        // Prepara la query
        if IdPadre = 0 then
        begin
          FDataBase.Query.SQL.Add(SQL_SELECT_DATOS_ALL_BY_CATALOGO_ID);
        end
        else
        begin
          FDataBase.Query.SQL.Add(SQL_SELECT_DATOS_ALL_BY_PARENT_ID);
          FDataBase.Query.ParamByName('IDPADRE').AsLargeInt    := IdPadre;
        end;

        // Hace la inserción con un prepared statement
        FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt := IdCatalogo;

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

// Elimina todos los catalogos
function TConectorDatos.DeleteAllCatalogos() : boolean;
begin
    // Elimina todas las tablas de la base de datos
    EliminarAllTablas();

    // Crea las tablas de la base de datos
    CrearTablas();

    // Inicializa el resultado
    Result := true;
end;

// Elimina un catalogo
function TConectorDatos.DeleteCatalogo(Catalogo : TItemCatalogo) : boolean;
begin
  // Inicializa el resultado
  Result := false;

  // Elimina los datos que pertenecen al catalogo
  if DoDeleteFromTableByIdParametro(Catalogo.Id, SQL_DELETE_DATOS_BY_ID_CATALOGO, 'IDCATALOGO') then
    // Elimina los rutas completas que pertenecen al catalogo
    if DoDeleteFromTableByIdParametro(Catalogo.Id, SQL_DELETE_RUTA_COMPLETA_BY_ID_CATALOGO, 'IDCATALOGO') then
      // Elimina el catalogo
      result := DoDeleteFromTableByIdParametro(Catalogo.Id, SQL_DELETE_CATALOGO_BY_ID, 'IDCATALOGO');
end;

// Elimina datos de una tabla a partir de un parametro
function TConectorDatos.DoDeleteFromTableByIdParametro(Id : qword; Sql : string; const Parametro : string) : boolean;
begin
  // Inicializa el resultado
  Result := false;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;

        // Prepara la query
        FDataBase.Query.SQL.Text := Sql;
        try
          // Hace la eliminación con un prepared statement
          FDataBase.Query.ParamByName(Parametro).AsLargeInt := Id;

          // Ejecuta la sentencia
          FDataBase.Query.ExecSQL;

          // Inicializa el resultado
          Result := true;
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

// Elimina un Item de una tabla
function TConectorDatos.DoDeleteItemFromTableById(IdCatalogo : qword; Id : qword; Sql : string) : boolean;
begin
  // Inicializa el resultado
  Result := false;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;

        // Prepara la query
        FDataBase.Query.SQL.Text := Sql;
        try
          // Hace la eliminación con un prepared statement
          FDataBase.Query.ParamByName('ID').AsLargeInt         := Id;
          FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt := IdCatalogo;

          // Ejecuta la sentencia
          FDataBase.Query.ExecSQL;

          // Inicializa el resultado
          Result := true;
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

// Elimina un dato
function TConectorDatos.DeleteDato(Dato : TItemDato) : boolean;
begin
  Result := DoDeleteItemFromTableById(Dato.IdCatalogo, Dato.Id, SQL_DELETE_DATO_BY_IDS);
  if Result then
    begin
      // Elimina las rutas completas que no tengan referencias en la tabla Datos
      DoDeleteRutasCompletasSinReferencias(Dato);
    end;
end;

// Elimina las rutas completas que no tengan referencias en la tabla Datos
function TConectorDatos.DoDeleteRutasCompletasSinReferencias(Dato : TItemDato) : boolean;
begin
  // Inicializa el resultado
  Result := false;
  try
    if FDataBase.Query <> nil then
    begin
      // Se inicia la seccion critica
      EnterCriticalSection(FCriticalSection);
      try
        // Prepara la query
        FDataBase.Query.Close;
        FDataBase.Query.SQL.Clear;

        // Prepara la query
        FDataBase.Query.SQL.Text := SQL_DELETE_RUTA_COMPLETA_SIN_REFERENCIAS;
        try
          // Hace la eliminación con un prepared statement
          FDataBase.Query.ParamByName('ID').AsLargeInt         := Dato.IdRutaCompleta;
          FDataBase.Query.ParamByName('IDCATALOGO').AsLargeInt := Dato.IdCatalogo;

          // Ejecuta la sentencia
          FDataBase.Query.ExecSQL;

          // Inicializa el resultado
          Result := true;
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

// Actualiza un catálogo
procedure TConectorDatos.UpdateCatalogo(Catalogo : TItemCatalogo);
begin
  DoInserteUpdateCatalogo(Catalogo, true);
end;

// Añade/Actualiza un catálogo a la base de datos
procedure TConectorDatos.DoInserteUpdateCatalogo(Catalogo : TItemCatalogo; IsUpdate : boolean);
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

          if not IsUpdate then
            FDataBase.Query.SQL.Add(SQL_INSERT_CATALOGO)
          else
            FDataBase.Query.SQL.Add(SQL_UPDATE_CATALOGO);

          // Hace la inserción con un prepared statement
          FDataBase.Query.ParamByName('ID').AsLargeInt               := Catalogo.Id;
          FDataBase.Query.ParamByName('NOMBRE').AsString             := Catalogo.Nombre;
          FDataBase.Query.ParamByName('DESCRIPCION').AsString        := Catalogo.Descripcion;
          FDataBase.Query.ParamByName('TIPO').AsInteger              := integer(Catalogo.Tipo);
          FDataBase.Query.ParamByName('FECHA').AsDateTime            := Catalogo.Fecha;

          if not IsUpdate then
          begin
            FDataBase.Query.ParamByName('TOTALARCHIVOS').AsLargeInt    := Catalogo.TotalArchivos;
            FDataBase.Query.ParamByName('TOTALDIRECTORIOS').AsLargeInt := Catalogo.TotalDirectorios;
            FDataBase.Query.ParamByName('TOTALSIZE').AsLargeInt        := Catalogo.Size;
          end;


          try
            // Realiza la inserción
            FDataBase.Query.ExecSQL;

          finally
            // Cierra la query
            FDataBase.Query.Close;
          end;


          if not IsUpdate then
          begin
            // Prepara la query
            FDataBase.Query.Close;
            FDataBase.Query.SQL.Clear;

            // Prepara la query
            FDataBase.Query.SQL.Add('INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (0, ' + inttostr(int64(Catalogo.Id)) + ', "/");');
            try
              // Realiza la inserción
              FDataBase.Query.ExecSQL;
            finally
              // Cierra la query
              FDataBase.Query.Close;
            end;
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

//TODO: Que tenga directivas de compilación para que no se incluya en la versión final

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

  procedure InsertarRutasCompletas(IdCatalogo : Qword);
  var
    i  : integer;
    max: integer = 10;
    ruta: TItemRutaCompleta;

  begin
    for i := 0 to max do
    begin
      ruta := TItemRutaCompleta.Create('directorio/loquesea_' + inttostr(i), IdCatalogo);
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

  procedure InsertarDatos(IdCatalogo : Qword);
  var
    Dato: TItemDato;
    i  : integer;
    max: integer = 100;
  begin
    for i := 0 to max do
    begin
      Dato := TItemDato.Create('prueba_' + inttostr(i) + '.txt', TItemDatoTipo.Archivo, now, 1204,
        100,
        '',
        0,
        0,
        0,
        IdCatalogo,
        0);
      try
        AddDato(Dato);
      finally
        Dato.Free;
      end;
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

{$IFDEF TESTEAR_SENTENCIAS_INSERTAR_EXTENSIONES}
  // Inserta extensiones
  InsertarExtensiones();
{$ENDIF TESTEAR_SENTENCIAS_INSERTAR_EXTENSIONES}

{$IFDEF TESTEAR_SENTENCIAS_INSERTAR_CATALOGO}
  // Inserta Catalogo
  InsertarCatalogo();
{$ENDIF TESTEAR_SENTENCIAS_INSERTAR_CATALOGO}

  // Obtiene todos los catalogos
  GetTodosCatalogos();
  try
    if catalogos <> nil then
    begin
        Cat := GetCatalogosById(catalogos[0]{%H-}.Id);

        if Cat <> nil then
        begin
          {$IFDEF TESTEAR_SENTENCIAS_UPDATE_CATALOGO}
            Cat.Nombre      := Cat.Nombre + ' - modificado desde el test';
            Cat.Descripcion := Cat.Descripcion + ' - modificada desde el test';
            Cat.Fecha       := now;
            UpdateCatalogo(Cat);
          {$ENDIF TESTEAR_SENTENCIAS_UPDATE_CATALOGO}
          Cat.Free;
        end;

{$IFDEF TESTEAR_SENTENCIAS_INSERTAR_RUTAS_COMPLETAS}
      // Inserta rutas  completas
      InsertarRutasCompletas(catalogos[0]{%H-}.Id);
{$ENDIF TESTEAR_SENTENCIAS_INSERTAR_RUTAS_COMPLETAS}



{$IFDEF TESTEAR_SENTENCIAS_INSERTAR_DATOS}
      // Inserta datos
      InsertarDatos(catalogos[0]{%H-}.Id);
{$ENDIF TESTEAR_SENTENCIAS_INSERTAR_DATOS}


      // Obtiene todos los datos del catalogo 0
      Datos := GetDatos(TItemCatalogo(catalogos[0]), nil);
      if (Datos <> nil) And (Datos.count >= 1) then
        begin
{$IFDEF TESTEAR_SENTENCIAS_DELETE_DATOS}
          DeleteDato(TItemDato(Datos[0]));
{$ENDIF TESTEAR_SENTENCIAS_DELETE_DATOS}

          Datos.Clear;
          Datos.Free;
        end;

{$IFDEF TESTEAR_SENTENCIAS_DELETE_CATALOGOS}
      DeleteCatalogo(catalogos[0]{%H-}.Id);
{$ENDIF TESTEAR_SENTENCIAS_DELETE_CATALOGOS}

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
