// Activa Heaptrc para poder determinar si hay fugas de memoria
{.$DEFINE FUGAS_DE_MEMORIA_DETECTAR}

{$IFDEF FUGAS_DE_MEMORIA_DETECTAR}
  // Vuelca el informe de Heaptrc a un archivo en vez de mostrar la ventana de la info
  {.$DEFINE FUGAS_DE_MEMORIA_DETECTAR_VOLCADOR_ARCHIVO}
{$ENDIF}

// Usar codificación de la base de datos
{.$DEFINE USAR_SQLCIPHER}

// Activar el testeo de sentencias
{.$DEFINE TESTEAR_SENTENCIAS}

{$IFDEF TESTEAR_SENTENCIAS}
  // Elimina todas las tablas antes de crearlas
  {.$DEFINE TESTEAR_SENTENCIAS_ELIMINAR_TABLAS}

  // Testea la inserción de extensiones
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_EXTENSIONES}

  // Testea la inserción de catalogos
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_CATALOGO}

  // Testea la actaulización de catalogos
  {$DEFINE TESTEAR_SENTENCIAS_UPDATE_CATALOGO}

  // Testea la inserción de rutas completas
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_RUTAS_COMPLETAS}

  // Testea la inserción de datos de archivos
  {$DEFINE TESTEAR_SENTENCIAS_INSERTAR_DATOS}

  // Testea la eliminación de catalogos
  {$DEFINE TESTEAR_SENTENCIAS_DELETE_DATOS}

  // Testea la eliminación de catalogos
  {.$DEFINE TESTEAR_SENTENCIAS_DELETE_CATALOGOS}
{$ENDIF}

// Activa el tema oscuro en la aplicación
{.$DEFINE DARKSTYLE_USAR}





//TODO: Eliminar, solo es para pruebas

{.$DEFINE ESCANEAR_DIRECTAMENTE}
{.$DEFINE ESCANEAR_DIRECTORIO_GRANDE}
{.$DEFINE MOSTRAR_INFO_ESCANEO}
{.$DEFINE ESCANEAR_DIRECTORIO_VSCODE_EXTENSIONS}
{.$DEFINE NODELETEDATOS}


