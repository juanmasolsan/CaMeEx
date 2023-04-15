/**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-13 15:57:23
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-15 13:51:56
 */

--Borrar tablas.
DROP TABLE IF EXISTS Datos;
DROP TABLE IF EXISTS Extensiones;
DROP TABLE IF EXISTS RutaCompleta;
DROP TABLE IF EXISTS Catalogos;

-- Crear tabla Catálogos
CREATE TABLE Catalogos (
    Id               INTEGER (8) PRIMARY KEY,
    Nombre           TEXT        NOT NULL,
    Descripcion      TEXT        NOT NULL,
    Tipo             INTEGER     NOT NULL,
    FechaCreacion    DATETIME    NOT NULL,
    TotalArchivos    INTEGER     NOT NULL,
    TotalDirectorios INTEGER     NOT NULL,
    TotalSize        INTEGER (8) NOT NULL
);

-- Crear tabla Extensiones
CREATE TABLE Extensiones (
    Id          INTEGER (8) PRIMARY KEY,
    Extension   TEXT        NOT NULL UNIQUE,
    Descripcion TEXT        NOT NULL
);

-- Insertar datos en la tabla Extensiones
INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (0, ".", "");


-- Crear tabla RutaCompleta
CREATE TABLE RutaCompleta (
    Id         INTEGER (8) PRIMARY KEY,
    IdCatalogo INTEGER (8) CONSTRAINT FK_CATALOGO REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,
    Ruta       TEXT        NOT NULL
);

-- Crear tabla Datos
CREATE TABLE Datos (
    Id             INTEGER (8) PRIMARY KEY,
    Tipo           INTEGER     NOT NULL,
    Atributos      INTEGER     NOT NULL,
    DateTime       DATETIME        NOT NULL,
    Size           INTEGER     NOT NULL,
    Nombre         TEXT        NOT NULL,
    ImageIndex     INTEGER     NOT NULL,
    IdExtension    INTEGER (8) CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdRutaCompleta INTEGER (8) CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE CASCADE ON UPDATE CASCADE,
    IdCatalogo     INTEGER (8) NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE CASCADE ON UPDATE CASCADE,
    IdPadre        INTEGER (8) CONSTRAINT FK_DATOS_PADRE REFERENCES Datos (Id) ON DELETE CASCADE ON UPDATE CASCADE
);


----------------------------------------------------------------------------------------
------------------------------- OPERACIONES DE TEST ------------------------------------
----------------------------------------------------------------------------------------

-- Insertar datos en la tabla Catálogos
INSERT INTO Catalogos(Id, Nombre, Descripcion, Tipo, FechaCreacion, TotalArchivos, TotalDirectorios, TotalSize) VALUES (1, "Disco 1", "Descripción", 1, 20230413, 1, 1, 100);

-- Insertar datos en la tabla RutaCompleta
INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (0, 1, "/");
INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (1, 1, "prueba/");

-- Insertar datos en la tabla Extensiones
INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (1111, ".txt", "Archivo de texto");

-- Insertar datos en la tabla Datos
INSERT INTO Datos (Id, Tipo, Atributos, DateTime, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta,IdCatalogo) VALUES (200, 2, 200, 20230303, 0, "prueba", 0, 0, 0, 1);
INSERT INTO Datos (Id, Tipo, Atributos, DateTime, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (1000, 1, 100, 20230403, 1024, "Archivo.txt", 1, 1111, 1, 1, 200);

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
SELECT dt.*, rc.Ruta, ex.Descripcion FROM Datos as dt
    JOIN RutaCompleta AS rc ON dt.IdRutaCompleta = rc.Id
    JOIN Extensiones AS ex ON dt.IdExtension = ex.Id
    ;


