/**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-13 15:57:23
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-20 15:45:46
 */

--Borrar tablas.
DROP TABLE IF EXISTS Datos;
DROP TABLE IF EXISTS Extensiones;
DROP TABLE IF EXISTS RutaCompleta;
DROP TABLE IF EXISTS Catalogos;

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

-- Crear tabla Extensiones
CREATE TABLE IF NOT EXISTS Extensiones (
    Id          BIGINT PRIMARY KEY,
    Extension   TEXT NOT NULL UNIQUE,
    Descripcion TEXT NOT NULL
);

-- Insertar datos en la tabla Extensiones
INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (0, ".", "");


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
    Id             BIGINT PRIMARY KEY,
    Tipo           INTEGER  NOT NULL,
    Atributos      INTEGER  NOT NULL,
    Fecha          DATETIME NOT NULL,
    Size           BIGINT   NOT NULL,
    Nombre         TEXT     NOT NULL,
    ImageIndex     INTEGER  NOT NULL,
    IdExtension    BIGINT CONSTRAINT FK_EXTENSION REFERENCES Extensiones (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdRutaCompleta BIGINT CONSTRAINT FK_RUTA_COMPLETA REFERENCES RutaCompleta (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdCatalogo     BIGINT NOT NULL CONSTRAINT FK_DATOS_CATALOGOS REFERENCES Catalogos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    IdPadre        BIGINT CONSTRAINT FK_DATOS_PADRE REFERENCES Datos (Id) ON DELETE RESTRICT ON UPDATE RESTRICT
);

-- Crear índices
CREATE INDEX IF NOT EXISTS Datos_Nombre_IDX ON Datos (Nombre);

----------------------------------------------------------------------------------------
------------------------------- OPERACIONES DE TEST ------------------------------------
----------------------------------------------------------------------------------------

-- Insertar datos en la tabla Catálogos
INSERT INTO Catalogos(Id, Nombre, Descripcion, Tipo, Fecha, TotalArchivos, TotalDirectorios, TotalSize) VALUES (1, "Disco 1", "Descripción", 1, 20230413, 1, 1, 100);

-- Insertar datos en la tabla RutaCompleta
INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (0, 1, "/");
INSERT INTO RutaCompleta (Id, IdCatalogo, Ruta) VALUES (1, 1, "prueba/");

-- Insertar datos en la tabla Extensiones
INSERT INTO Extensiones (Id, Extension, Descripcion) VALUES (1111, ".txt", "Archivo de texto");

-- Insertar datos en la tabla Datos
INSERT INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta,IdCatalogo) VALUES (200, 2, 200, 20230303, 0, "prueba", 0, 0, 0, 1);
INSERT INTO Datos (Id, Tipo, Atributos, Fecha, Size, Nombre, ImageIndex, IdExtension, IdRutaCompleta, IdCatalogo, IdPadre) VALUES (1000, 1, 100, 20230403, 1024, "Archivo.txt", 1, 1111, 1, 1, 200);

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

-- Actualiza el nombre y la descripción del catalogo con el id 1
UPDATE Catalogos SET Nombre="Disco 1 - Update", Descripcion="Descripción - Disco 1 - Update", Tipo=1, Fecha=20230413 WHERE Id=1;
