(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 14:21:18
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-04-15 14:36:05
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
unit Control_CRC;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , strutils
  ;

// Calcular el crc64 de un string
function CRC64_From_String(const Data : String) : int64;

// Calcular el crc64 de un puntero de bytes
function CRC64_From_PByte(Data : PByte; Size : Longint) : int64;


implementation

uses
  crc
;

// Calcular el crc64 de un string
function CRC64_From_String(const Data : String) : int64;
begin
  Result := CRC64_From_PByte(@Data[1], length(Data));
end;

// Calcular el crc64 de un puntero de bytes
function CRC64_From_PByte(Data : PByte; Size : Longint) : int64;
begin
  Result := crc64(0,nil,0);
  Result := crc64(Result, Data, Size);
end;


initialization

finalization


end.
