// Activa Heaptrc para poder determinar si hay fugas de memoria
{$DEFINE DETECTAR_FUGAS_DE_MEMORIA}

{$IFDEF DETECTAR_FUGAS_DE_MEMORIA}
  // Vuelca el informe de Heaptrc a un archivo en vez de mostrar la ventana de la info
  {$DEFINE USAR_VOLCADOS_ARCHIVO}
{$ENDIF}

// Usar codificación de la base de datos
{$DEFINE USAR_SQLCIPHER}

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


//TODO: Eliminar, solo es para pruebas
{.$DEFINE ESCANEAR_DIRECTORIO_GRANDE}
{.$DEFINE MOSTRAR_INFO_ESCANEO}

