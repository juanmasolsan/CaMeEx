(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-03-23 16:15:29
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-12 17:57:31
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

unit Control_DB;

{$mode objfpc}{$H+}


interface

uses
  LCLProc
  , LCLType
  , LCLIntf
  , Classes
  , SysUtils
  , db
  , sqldb
  , sqlite3conn
  , sqlite3dyn
  ;

type
  { TConexion_DB }
  TConexion_DB_Base = Class
  private
    FConnection     : TSQLite3Connection;
    FQuery          : TSQLQuery;
    FTransaction    : TSQLTransaction;
    FArchivoDB      : String;
    FRutaDll        : String;
    FProtocolo      : String;
    FUsername       : String;
    FPassword       : String;
    FIsReading      : Boolean;
    FCriticalSection: TCriticalSection;
  protected
    // Metodos virtuales para crear y destruir la conexion
    procedure DoCrear_DB; virtual;
    procedure DoDestruir_DB; virtual;

    // Metodo para crear el archivo de la base de datos
    procedure DoCrearArchivo_DB;

    // Metodo para realizar sentencias SQL protegidas para concurrencia
    procedure DoSQL(const DataSQL : string; iQuery : TSQLQuery = nil);
  public
    // Constructor y destructor
    Constructor Create(const Archivo : String; Protocolo : string = 'sqlite-3'; User : string =''; PassWord : String = ''; RutaDll : string = '' ); virtual;
    Destructor Destroy; override;

    // Metodo para realizar sentencias SQL protegidas para concurrencia
    procedure SQL(const DataSQL : string; iQuery : TSQLQuery = nil);

    // Metodo para optimizar una tabla
    procedure SQLite3_OptimizarTabla({%H-}Cache_Size : longint = 8; {%H-}Page_Size : longint = 8); virtual;

    // Propiedades
    property  Query : TSQLQuery read FQuery;
    property  Connection : TSQLite3Connection read FConnection;
  end;

  { TConexion_DB_001 }
  TConexion_DB_001 = Class(TConexion_DB_Base)
  private
  protected
  public
    // Metodo para optimizar las tablas
    procedure SQLite3_OptimizarTabla(Cache_Size : longint = 8; Page_Size : longint = 8); override;

    // Metodo para iniciar una transaccion
    procedure SQLite3_BeginUpdate;

    // Metodo para finalizar una transaccion
    procedure SQLite3_EndUpdate;

    // Metodo para optimizar la base de datos
    procedure SQLite3_Optimizar_DB;
  end;

  { TConexion_DB }
  TConexion_DB = Class(TConexion_DB_001);


implementation


// Método para realizar sentencias SQL protegidas para concurrencia
procedure Internal_SQL(var Entrada : TConexion_DB_Base; const DataSQL : string; CriticalSection : TCriticalSection; iQuery : TSQLQuery = nil);

  // DML (Data Manipulation Language) de tipo INSERT, UPDATE, DELETE, SELECT
  function IsDML : Boolean;
  var
    ver : string;
    Posicion : longint;
  begin
    ver := Uppercase(DataSQL);
    Result := false;
    Posicion := pos('SELECT', ver);
    if (Posicion > 0) and (Posicion < 15) then
    Result := True
  end;

begin
  // Si no hay conexion, se sale
  if Entrada = nil then exit;

  // Detecta si es una lectura
  Entrada.FIsReading := IsDML;
  try
    // Se inicia la seccion critica
    EnterCriticalSection(CriticalSection);
    try
      // Se ejecuta la sentencia SQL
      Entrada.DoSQL(DataSQL, iQuery);
    finally
      LeaveCriticalSection(CriticalSection);
    end;
  except
    //
  end;
end;


{ TConexion_DB_Base }
// Constructor
Constructor TConexion_DB_Base.Create(const Archivo : String; Protocolo : string = 'sqlite-3'; User : string =''; PassWord : String = ''; RutaDll : string = '' );
begin
  // llama al constructor de la clase padre
  inherited Create;

  // Inicializa la seccion critica
  InitializeCriticalSection(FCriticalSection);

  // Inicializa las propiedades
  FArchivoDB  := Archivo;
  FProtocolo  := Protocolo;
  FUsername   := User;
  FPassword   := PassWord;
  FRutaDll    := RutaDll;

  // Si se especifica una ruta para la libreria, se carga
  if FRutaDll <> '' then
    begin
      SQLiteDefaultLibrary := {%H-}FRutaDll;
    end;


  FIsReading  := False;

  // Creamos la conexion
  DoCrear_DB;
end;

// Destructor
Destructor TConexion_DB_Base.Destroy;
begin
  // Destruimos la conexion
  DoDestruir_DB;

  // Destruimos la seccion critica
  DeleteCriticalSection(FCriticalSection);

  // Llama al destructor de la clase padre
  inherited Destroy;
end;

// Metodo para crear el archivo de la base de datos
procedure TConexion_DB_Base.DoCrearArchivo_DB;
begin
//
end;

// Se crean todo lo necesario para hacer funcionar la conexión
procedure TConexion_DB_Base.DoCrear_DB;
begin
  // Se crea todo lo necesario para que la conexión funcione
  FConnection                 := TSQLite3Connection.Create(nil);
  FQuery                      := TSQLQuery.Create(nil);
  FTransaction                := TSQLTransaction.Create(nil);

  // Se asignan las propiedades
  FConnection.Transaction     := FTransaction;
  FTransaction.Database       := FConnection;

  // Se asignan las propiedades
  FQuery.Database             := FConnection;
  FQuery.Transaction          := FTransaction;

  // Se asignan las propiedades
  FConnection.DatabaseName    := FArchivoDB;
  FConnection.Connected       := True;

  // Se optimitan las tablas
  SQLite3_OptimizarTabla;
end;

// Metodo para optimizar una tabla
procedure TConexion_DB_Base.SQLite3_OptimizarTabla(Cache_Size : longint = 8; Page_Size : longint = 8);
begin
  FConnection.ExecuteDirect('COMMIT;BEGIN');
end;

// Se libera todo lo creado para hacer funcionar la conexión
procedure TConexion_DB_Base.DoDestruir_DB;
begin
  // Se cierran las conexiones
  FQuery.Close;

  // Se libera todo lo creado
  FConnection.ExecuteDirect('Begin Transaction');
  FConnection.Connected       := false;
  FConnection.Close;

  // Se libera todo lo creado
  FConnection.Transaction     := nil;
  FTransaction.Database       := nil;
  FQuery.Database             := nil;
  FQuery.Transaction          := nil;

  // Se libera todo lo creado
  FQuery.free;
  FTransaction.free;
  FConnection.free;
end;

// Metodo para realizar sentencias SQL protegidas para concurrencia
procedure TConexion_DB_Base.DoSQL(const DataSQL : string; iQuery : TSQLQuery = nil);
begin
  // Se comprueba si se especifico un query
  if iQuery = nil then
    iQuery := FQuery
  else
    iQuery.Database  := FConnection;

  // dependiendo de si es una lectura o escritura, se ejecuta la sentencia SQL
  if FIsReading then
    begin
      // Lectura
      iQuery.Close;
      iQuery.SQL.Text := '';
      iQuery.SQL.Text := DataSQL;
      iQuery.Open;
    end
    else
    begin
      // Escritura
      FConnection.ExecuteDirect(DataSQL);
    end;
end;

// Metodo para realizar sentencias SQL protegidas para concurrencia
procedure TConexion_DB_Base.SQL(const DataSQL : string; iQuery : TSQLQuery = nil);
begin
  Internal_SQL(Self, DataSQL, FCriticalSection, iQuery);
end;

{ TConexion_DB_001 }
// Metodo para optimizar las tablas
procedure TConexion_DB_001.SQLite3_OptimizarTabla(Cache_Size : longint = 8; Page_Size : longint = 8);
var
  Salida : string = '';

  procedure _DoSQL(Entrada : string);
  begin
    Salida += Entrada;
  end;

begin
  // Se añaden las mejoras a la tabla
  DoSQL('PRAGMA journal_mode = TRUNCATE;');
  _DoSQL('PRAGMA wal_autocheckpoint = 16;');   //* number of 32KiB pages in a 512KiB journal */
  _DoSQL('PRAGMA journal_size_limit = 1536;'); //* 512KiB * 3 */
  _DoSQL('COMMIT; PRAGMA synchronous = OFF;');
  _DoSQL('PRAGMA page_size = ' + inttostr(Page_Size * 1024) + ';');
  _DoSQL('PRAGMA cache_size = ' + inttostr((1024 * 1024) * Cache_Size) + ';');
  _DoSQL('PRAGMA count_changes = OFF;');

  // Se hacen efectivas las mejoras
  FConnection.ExecuteDirect('COMMIT;'+Salida+'BEGIN');
end;

// Metodo para iniciar una transaccion
procedure TConexion_DB_001.SQLite3_BeginUpdate;
begin
  SQL('BEGIN;');
end;

// Metodo para finalizar una transaccion
procedure TConexion_DB_001.SQLite3_EndUpdate;
begin
  SQL('COMMIT;');
end;

// Optimiza la base de datos
procedure TConexion_DB_001.SQLite3_Optimizar_DB;
begin
  SQL('VACUUM;');
end;




initialization

finalization

end.

