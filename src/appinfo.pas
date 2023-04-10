(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-06 14:37:57
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-10 23:08:01
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

unit AppInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// Inicio de programación  20230406
const
  VERSION_PROGRAMA     = '1.0.3.8' + {$IFDEF CPUX64}' x64'{$ELSE}' x86'{$ENDIF};
  FECHA_PROGRAMA       = '20230410 23:10:41';
  NOMBRE_PROGRAMA      = 'CaMeEx';
  NOMBRE_AUTOR         = 'Juan Manuel Soltero Sánchez';
  APP_WEB              = 'https://github.com/juanmasolsan/CaMeEx';
  AUTOR_EMAIL          = 'test@etest.es';



implementation


end.

