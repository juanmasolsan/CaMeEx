unit AppInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// Inicio de programación  20230406
const
  VERSION_PROGRAMA     = '1.0.0.0' + {$IFDEF CPUX64}' x64'{$ELSE}' x86'{$ENDIF};
  FECHA_PROGRAMA       = '20230406 14:40:41';
  NOMBRE_PROGRAMA      = 'CaMeEx';
  NOMBRE_AUTOR         = 'Juan Manuel Soltero Sánchez';


implementation


end.

