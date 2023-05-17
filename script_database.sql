/**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-13 15:57:23
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-17 14:11:45
 */


-- Enabling Foreign Key
PRAGMA foreign_keys = ON;

-- Comprueba el estado
PRAGMA foreign_keys;

--Borrar tablas.
DROP TABLE IF EXISTS Datos;
DROP TABLE IF EXISTS Extensiones;
DROP TABLE IF EXISTS Iconos;
DROP TABLE IF EXISTS RutaCompleta;
DROP TABLE IF EXISTS Catalogos;

--Optimiza tablas.
VACUUM;

-- Crear tabla Catálogos
CREATE TABLE IF NOT EXISTS Catalogos (
    Id               BIGINT PRIMARY KEY,
    Nombre           TEXT     NOT NULL,
    Descripcion      TEXT     NOT NULL,
    Tipo             INTEGER  NOT NULL,
    Fecha            DATETIME NOT NULL,
    TotalArchivos    INTEGER  NOT NULL,
    TotalDirectorios INTEGER  NOT NULL,
    TotalSize        BIGINT NOT NULL
);

-- Crear tabla Iconos
CREATE TABLE IF NOT EXISTS Iconos (
    Id       BIGINT PRIMARY KEY,
    Icono    BLOB,
    Icono32  BLOB
);

-- Crear tabla Extensiones
CREATE TABLE IF NOT EXISTS Extensiones (
    Id          BIGINT PRIMARY KEY,
    Extension   TEXT NOT NULL UNIQUE,
    Descripcion TEXT NOT NULL,
    IdIcono     BIGINT  CONSTRAINT FK_Extension_Icono REFERENCES Iconos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT
);

-- Insertar datos en la tabla Extensiones
INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (0, '.', '');

-- Crear tabla RutaCompleta
CREATE TABLE IF NOT EXISTS RutaCompleta (
    Id         BIGINT PRIMARY KEY,
    IdCatalogo BIGINT CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    Ruta       TEXT NOT NULL
);

-- Crear índices
CREATE INDEX IF NOT EXISTS RutaCompleta_Ruta_IDX ON RutaCompleta (Ruta);

-- Crear tabla Datos
CREATE TABLE IF NOT EXISTS Datos (
    Id              BIGINT PRIMARY KEY,
    Tipo            INTEGER  NOT NULL,
    Atributos       INTEGER  NOT NULL,
    Fecha           DATETIME NOT NULL,
    FechaCreacion   DATETIME NOT NULL,
    FechaLastAcceso DATETIME NOT NULL,
    Size            BIGINT   NOT NULL,
    Nombre          TEXT     NOT NULL,
    ImageIndex      INTEGER  NOT NULL,
    TieneHijos      INTEGER  NOT NULL,
    IdPadre         BIGINT CONSTRAINT FK_DATOS REFERENCES Datos (Id) ON DELETE CASCADE ON UPDATE RESTRICT,
    IdExtension     BIGINT CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdRutaCompleta  BIGINT CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdCatalogo      BIGINT NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT
);

-- Crear índices
CREATE INDEX IF NOT EXISTS Datos_Nombre_IDX ON Datos (Nombre);
CREATE INDEX IF NOT EXISTS Datos_IdPadre_IDX ON Datos (IdPadre);
CREATE INDEX IF NOT EXISTS Datos_IdCatalogo_IDX ON Datos (IdCatalogo);


----------------------------------------------------------------------------------------
------------------------------- OPERACIONES DE TEST ------------------------------------
----------------------------------------------------------------------------------------

-- Insertar datos en la tabla Catálogos
INSERT OR IGNORE INTO Catalogos(Id, Nombre, Descripcion, Tipo, Fecha, TotalArchivos, TotalDirectorios, TotalSize) VALUES (1, 'Disco 1', 'Descripción', 1, 20230413, 1, 1, 100);

-- Insertar datos en la tabla RutaCompleta
INSERT OR IGNORE INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (0, 1, '/');
INSERT OR IGNORE INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (1, 1, 'prueba/');

-- Insertar datos en la tabla Extensiones
INSERT OR IGNORE INTO Extensiones (Id, Extension, Descripcion) VALUES (1111, '.txt', 'Archivo de texto');

-- Insertar datos en la tabla Datos
INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta,IdCatalogo) VALUES (200, 2, 200, 20230303, 0, 'prueba', 0, 0, 0, 1);
INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (1000, 1, 100, 20230403, 1024, 'Archivo.txt', 1, 1111, 1, 1, 200);

-- Insertar datos en la tabla Datos
INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta,IdCatalogo) VALUES (201, 2, 200, 20230303, 0, 'prueba2', 0, 0, 0, 1);
INSERT OR IGNORE INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo) VALUES (1001, 1, 100, 20230403, 1024, 'Archivo2.txt', 1, 1111, 1, 1);



-- SELECT - Listar todo el contenido de las tablas
SELECT * FROM Datos;
SELECT * FROM Extensiones;
SELECT * FROM RutaCompleta;
SELECT * FROM Catalogos;

-- SELECT - Listar todo el contenido de la tabla Datos que pertenezca al catalogo 1
SELECT * FROM Datos WHERE IdCatalogo = 1;

-- SELECT - Listar todo el contenido de la tabla Datos que pertenezca al catalogo 1 y que sea hijo del directorio con el id 200
SELECT * FROM Datos WHERE IdCatalogo = 1 AND IdPadre = 200;

-- SELECT - Listar todo el contenido de la tabla Datos y que contenga la ruta completa
SELECT dt.*, rc.Ruta FROM Datos as dt JOIN RutaCompleta AS rc
    ON dt.IdRutaCompleta = rc.Id;


-- SELECT - Listar todo el contenido de la tabla Datos y que contenga la ruta completa y descripción de la extension
SELECT dt.*, rc.Ruta, ex.Extension, ex.Descripcion, ic.Icono  FROM Datos as dt
    JOIN RutaCompleta AS rc ON dt.IdRutaCompleta = rc.Id
    JOIN Extensiones AS ex ON dt.IdExtension = ex.Id
    JOIN Iconos AS ic ON dt.IdExtension = ic.Id
    ;

-- SELECT - Listar todo el contenido de la tabla Extensiones y que devuelva la info y el icono
SELECT ex.Extension, ex.Descripcion, ic.Icono FROM Extensiones as ex
	JOIN Iconos AS ic ON ex.IdIcono = ic.Id
    ;

-- Elimina el registro con el id 1000
DELETE FROM Datos WHERE IdCatalogo = 1 AND (Id = 1000 OR IdPadre = 1000);

-- Elimina el registro con el id 200
DELETE FROM Datos WHERE IdCatalogo = 1 AND (Id = 200 OR IdPadre = 200);

-- Elimina el registro con el id 201
DELETE FROM Datos WHERE IdCatalogo = 1 AND (Id = 201 OR IdPadre = 201);

-- Elimina de las Rutas completas todas las filas que no tengan asociado ninguna referencia
DELETE FROM RutaCompleta WHERE Id = 0 AND IdCatalogo = 1 AND NOT EXISTS (SELECT 1 FROM Datos WHERE IdRutaCompleta = 0);

-- Actualiza el nombre y la descripción del catalogo con el id 1
UPDATE Catalogos SET Nombre='Disco 1 - Update', Descripcion='Descripción - Disco 1 - Update', Tipo=1, Fecha=20230413 WHERE Id=1;

-- Actualiza los totales de archivos y directorios que contiene el catalogo 6471404700933920559
UPDATE Catalogos SET
TotalSize=ifnull((SELECT SUM(Size) FROM Datos WHERE  Tipo = 2 AND Id <> IdPadre AND IdCatalogo=6471404700933920559), 0),
TotalArchivos=ifnull((SELECT Count(*) FROM Datos WHERE  Tipo = '2' AND Id <> IdPadre AND IdCatalogo=6471404700933920559), 0),
TotalDirectorios=ifnull((SELECT Count(*) FROM Datos WHERE  Tipo = '1' AND Id <> IdPadre AND IdCatalogo=6471404700933920559), 0)
WHERE Id=6471404700933920559;