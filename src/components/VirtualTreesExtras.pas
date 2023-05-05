(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-06 00:23:20
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-06 00:33:15
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

unit VirtualTreesExtras;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , Controls
  , Inifiles
  , Menus
  , laz.VirtualTrees
;

type
  TDatosColumnasHeaderVirtualTrees = record
    Nombre     : WideString;
    Width      : integer;
    Position   : integer;
    IsVisible  : Boolean;
  end;

  TListaDatosColumnasHeaderVirtualTrees = record
    Total : integer;
    Datos : array[0..255] of TDatosColumnasHeaderVirtualTrees;
  end;


function HeaderVirtualTrees_Get(VirtualTree : TVirtualStringTree) : TListaDatosColumnasHeaderVirtualTrees;
procedure HeaderVirtualTrees_Set(VirtualTree : TVirtualStringTree; DatosColumnas : TListaDatosColumnasHeaderVirtualTrees);

function HeaderVirtualTrees_CargarColumnas(Configuracion : TMemInifile; Nombre : string; Datos : TListaDatosColumnasHeaderVirtualTrees): TListaDatosColumnasHeaderVirtualTrees;
procedure HeaderVirtualTrees_GuardarColumnas(Configuracion : TMemInifile; Nombre : string; Datos : TListaDatosColumnasHeaderVirtualTrees);

procedure HeaderVirtualTrees_PopUpMenu_Crear(VirtualTree : TVirtualStringTree; OnClick : TNotifyEvent = nil);
procedure HeaderVirtualTrees_PopUpMenu_Actualizar(VirtualTree : TVirtualStringTree);


implementation

uses
  SysUtils
;


function HeaderVirtualTrees_Get(VirtualTree : TVirtualStringTree) : TListaDatosColumnasHeaderVirtualTrees;
var
  t, total     : integer;
  DatosColumna : TVirtualTreeColumn;
begin
  Total        := VirtualTree.Header.Columns.Count -1;
  Result.Total := Total;
  for t := 0 to Total do
    begin
      DatosColumna              := VirtualTree.Header.Columns.Items[t];
      Result.Datos[t].Width     := DatosColumna.Width;
      Result.Datos[t].Position  := DatosColumna.Position;
      Result.Datos[t].IsVisible := coVisible in DatosColumna.Options;
    end;
end;


procedure HeaderVirtualTrees_Set(VirtualTree : TVirtualStringTree; DatosColumnas : TListaDatosColumnasHeaderVirtualTrees);
var
  t, total     : integer;
  DatosColumna : TVirtualTreeColumn;
begin
  Total        := DatosColumnas.Total;
  for t := 0 to Total do
    begin
      DatosColumna          := VirtualTree.Header.Columns.Items[t];
      DatosColumna.Width    := DatosColumnas.Datos[t].Width;
      DatosColumna.Position := DatosColumnas.Datos[t].Position;
      if DatosColumnas.Datos[t].IsVisible then
        DatosColumna.Options := DatosColumna.Options + [coVisible]
      else
        DatosColumna.Options := DatosColumna.Options - [coVisible];
    end;
end;


function HeaderVirtualTrees_CargarColumnas(Configuracion : TMemInifile; Nombre : string; Datos : TListaDatosColumnasHeaderVirtualTrees): TListaDatosColumnasHeaderVirtualTrees;
var
  t : integer;
begin

  Result.Total := Datos.Total;
  for t := 0 to Result.Total do
    begin
      Result.Datos[t].Width     := Configuracion.readinteger('Columnas', Nombre+'.'+inttostr(t)+'.Width', Datos.Datos[t].Width);
      Result.Datos[t].Position  := Configuracion.readinteger('Columnas', Nombre+'.'+inttostr(t)+'.Position', Datos.Datos[t].Position);
      Result.Datos[t].IsVisible := Configuracion.readbool('Columnas', Nombre+'.'+inttostr(t)+'.IsVisible', Datos.Datos[t].IsVisible);
  end;
end;


procedure HeaderVirtualTrees_GuardarColumnas(Configuracion : TMemInifile; Nombre : string; Datos : TListaDatosColumnasHeaderVirtualTrees);
var
  t : integer;
begin

  for t := 0 to Datos.Total do
    begin
      Configuracion.writeinteger('Columnas', Nombre+'.'+inttostr(t)+'.Width', Datos.Datos[t].Width);
      Configuracion.writeinteger('Columnas', Nombre+'.'+inttostr(t)+'.Position', Datos.Datos[t].Position);
      Configuracion.writebool('Columnas',Nombre+'.'+inttostr(t)+'.IsVisible', Datos.Datos[t].IsVisible);
    end;
  Configuracion.UpdateFile;
end;

procedure HeaderVirtualTrees_PopUpMenu_Crear(VirtualTree : TVirtualStringTree; OnClick : TNotifyEvent = nil);
var
  MenuPop : TPopUpMenu;
begin
  MenuPop                      := TPopUpMenu.Create(VirtualTree);
  MenuPop.OnPopup              := OnClick;
  MenuPop.Tag                  := -1;
  VirtualTree.Header.PopupMenu := MenuPop;
  HeaderVirtualTrees_PopUpMenu_Actualizar(VirtualTree);
end;

procedure HeaderVirtualTrees_PopUpMenu_Actualizar(VirtualTree : TVirtualStringTree);

  function GetColumnasPosicion(Datos : TListaDatosColumnasHeaderVirtualTrees; Posicion : integer; var Tag : integer) : integer;
  var
    t  : integer;
  begin
    Result := 0;
    for t := 0 to Datos.Total do
    begin
      if VirtualTree.Header.Columns.Items[t].Tag =  Posicion then
      begin
        Tag    := VirtualTree.Header.Columns.Items[t].Tag;
        Result := t;
        break;
      end;
    end;
  end;

var
  Datos          : TListaDatosColumnasHeaderVirtualTrees;
  t              : integer;
  Entrada        : TMenuItem;
  DatosColumna   : TVirtualTreeColumn;
  PosicionColumna: integer;
  Tag            : integer;
  PopUp          : TPopUpMenu;
begin
  Tag   := 0;
  PopUp := VirtualTree.Header.PopupMenu;
  if PopUp = nil then exit;

  Datos := HeaderVirtualTrees_Get(VirtualTree);

   // Borramos la lista de menus
  PopUp.Items.Clear;

  for t := 0 to Datos.Total do
    begin
      PosicionColumna := GetColumnasPosicion(Datos, t, Tag);
      DatosColumna    := VirtualTree.Header.Columns.Items[PosicionColumna];

      Entrada         := TMenuItem.Create(PopUp);
      Entrada.Caption := DatosColumna.Text;
      Entrada.Checked := Datos.Datos[t].IsVisible;
      Entrada.Tag     := Tag;
      Entrada.OnClick := PopUp.OnPopup;
      PopUp.Items.Add(Entrada);
    end;
end;


end.