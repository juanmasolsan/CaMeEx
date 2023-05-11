(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-11 22:46:46
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-11 23:40:04
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

unit VirtualTreesExtended;

{$mode objfpc}{$H+}

interface

uses
  LCLType
  , lclintf
  , LMessages
  , Classes
  , Controls
  , Graphics
  , laz.VirtualTrees
;

type
  { TLazVirtualStringTree }
  // Varios hacks para TLazVirtualStringTree
  TLazVirtualStringTree = class(laz.VirtualTrees.TLazVirtualStringTree)
  private
    FDibujarInfoCatalogo     : boolean;
    FDibujarBotonesModernos  : Boolean;
    FOcultarBotonesAlperderFoco : boolean;
    FMostrarLineasArbol          : boolean;
    FMostrarLineasArbolPunteadas : boolean;

    procedure SetOcultarBotonesAlperderFoco(Value : Boolean);
    procedure SetMostrarLineasArbol(Value : Boolean);
    procedure SetMostrarLineasArbolPunteadas(Value : Boolean);
  protected
    // Hack para evitar parpadeos en el dibujado
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    // Poder dibujar imagenes ghosted
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;

    // Dibujar los nodos
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;

    // Dibujar el texto de los nodos
    procedure DoDibujarInfoCatalogo(var PaintInfo: TVTPaintInfo; Calcular : boolean); virtual;

    // Dibujar el boton de los nodos
    procedure PaintNodeButton(nCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex; const R: TRect; ButtonX, ButtonY: Integer; nBidiMode: TBiDiMode); override;

    // Dibujar las lineas de los nodos
    procedure PaintTreeLines(const PaintInfo: TVTPaintInfo; VAlignment, IndentSize: Integer; LineImage: TLineImage); override;

    // Dibujar los botones de los nodos
    procedure DoDibujarBotones(Node: PVirtualNode; zCanvas : TCanvas; X, Y : integer);

    // Para controlar la entrada y salida del arbol
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;


  public
    // Constructor
    constructor Create(AOwner: TComponent); override;

    property DibujarInfoCatalogo        : boolean read FDibujarInfoCatalogo write FDibujarInfoCatalogo;
    property DibujarBotonesModernos     : Boolean read FDibujarBotonesModernos write FDibujarBotonesModernos;
    property OcultarBotonesAlperderFoco : boolean read FOcultarBotonesAlperderFoco write SetOcultarBotonesAlperderFoco;
    property MostrarLineasArbol         : boolean read FMostrarLineasArbol write SetMostrarLineasArbol;
    property MostrarLineasArbolPunteadas: boolean read FMostrarLineasArbolPunteadas write SetMostrarLineasArbolPunteadas;
  end;


var
  CATALOGO_NODE_ALTURA : integer = 35;

const
  DESVIACION = 40;

implementation

uses
  Utilidades
  ;

{ TLazVirtualStringTree }
constructor TLazVirtualStringTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ScrollBarOptions.AlwaysVisible          := False;
  IncrementalSearch                       := isInitializedOnly;
  IncrementalSearchDirection              := sdForward;
  IncrementalSearchStart                  := ssFocusedNode;
  Colors.UnfocusedSelectionBorderColor    := Colors.TreeLineColor;
  Colors.UnfocusedSelectionColor          := clWindow;
  Colors.DropTargetColor                  := Colors.UnfocusedSelectionColor;
  Colors.DropTargetBorderColor            := Colors.UnfocusedSelectionBorderColor;

  FDibujarBotonesModernos                 := false;
  FOcultarBotonesAlperderFoco             := false;
  FMostrarLineasArbol                     := true;
  FMostrarLineasArbolPunteadas            := false;

  LineStyle                               := lsSolid;
  TreeOptions.PaintOptions                := TreeOptions.PaintOptions + [toShowTreeLines]

end;



// Hack para evitar parpadeos en el dibujado
procedure TLazVirtualStringTree.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  inherited WMEraseBkgnd(Message);
  Message.Result := 1; // Fake erase
end;

// Poder dibujar imagenes ghosted
procedure TLazVirtualStringTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  TMP : TPortableNetworkGraphic;
begin
  DoDibujarInfoCatalogo(PaintInfo, true);


  with PaintInfo do
  begin
    // Si no tiene asignada una imagen, salimos
    if ImageInfo[ImageInfoIndex].Images = nil then
      exit;

    // Si es una imagen ghosted, la dibujamos con transparencia
    if ImageInfo[ImageInfoIndex].Ghosted then
    begin
      // Crea un png para poder usar transparencias
      TMP := TPortableNetworkGraphic.create();
      try
        if ImageInfo[ImageInfoIndex].Index < ImageInfo[ImageInfoIndex].Images.Count then
        begin
          // Recupera la imagen de la lista de imagenes
          ImageInfo[ImageInfoIndex].Images.GetBitmap(ImageInfo[ImageInfoIndex].Index, TMP);

          // Le aplica la transparencia
          CrearTransparencia(TMP, 140, false);

          // La dibuja
          Canvas.Draw(ImageInfo[ImageInfoIndex].XPos, ImageInfo[ImageInfoIndex].YPos, TMP);
        end;
      finally
        // Libera la imagen
        TMP.Free;
      end;
    end
    else
      inherited PaintImage(PaintInfo, ImageInfoIndex, DoOverlay);
  end;
end;


// Dibujar los nodos
procedure TLazVirtualStringTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  Y : longint = -1;
begin
  DoDibujarInfoCatalogo(PaintInfo, true);

  inherited DoPaintNode(PaintInfo);

  if PaintInfo.Node = nil then exit;

 // Dibuja los botones del arbol
  if FDibujarBotonesModernos then
  begin
    if PaintInfo.Node <> nil then
      if PaintInfo.Node^.NodeHeight = CATALOGO_NODE_ALTURA then
        Y := ((CATALOGO_NODE_ALTURA - DefaultNodeHeight) div 3) + 1;

    DoDibujarBotones(PaintInfo.Node, PaintInfo.Canvas, PaintInfo.ContentRect.Left, Y);
  end;
end;

// Dibujar el texto de los nodos
procedure TLazVirtualStringTree.DoDibujarInfoCatalogo(var PaintInfo: TVTPaintInfo; Calcular : boolean);

  procedure Gestionar_Imagenes(Cual : TVTImageInfoIndex);
  begin
    if PaintInfo.ImageInfo[Cual].Index = -1 then exit;
    if PaintInfo.ImageInfo[Cual].YPos  <> 0 then
      PaintInfo.ImageInfo[Cual].YPos := 2;
  end;

begin
  if not FDibujarInfoCatalogo then exit;
  if PaintInfo.Node = nil then exit;
  if PaintInfo.Node^.NodeHeight <> CATALOGO_NODE_ALTURA then exit;

  if Calcular then
    begin
      PaintInfo.CellRect.Bottom    := self.DefaultNodeHeight;
      PaintInfo.ContentRect.Bottom := self.DefaultNodeHeight;
      Gestionar_Imagenes(iiNormal);
      Gestionar_Imagenes(iiState);
      Gestionar_Imagenes(iiCheck);
      Gestionar_Imagenes(iiOverlay);
      exit;
    end;
end;

// Dibujar el boton de los nodos
procedure TLazVirtualStringTree.PaintNodeButton(nCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex; const R: TRect; ButtonX, ButtonY: Integer; nBidiMode: TBiDiMode);
begin
  if FDibujarInfoCatalogo and (Node^.NodeHeight = CATALOGO_NODE_ALTURA) then
  begin
    ButtonY := (self.DefaultNodeHeight div 3) -1;
  end;

   // Dibuja los botones del arbol
  if not FDibujarBotonesModernos then
    inherited PaintNodeButton(nCanvas, Node, Column, R, ButtonX, ButtonY, nBidiMode);
end;

// Dibujar las lineas de los nodos
procedure TLazVirtualStringTree.PaintTreeLines(const PaintInfo: TVTPaintInfo; VAlignment, IndentSize: Integer; LineImage: TLineImage);
begin
  if FDibujarInfoCatalogo and (PaintInfo.CellRect.Bottom = CATALOGO_NODE_ALTURA) then
  begin
    VAlignment := self.DefaultNodeHeight div 2;
  end;

  inherited PaintTreeLines(PaintInfo, VAlignment, IndentSize, LineImage);
end;

procedure TLazVirtualStringTree.DoDibujarBotones(Node: PVirtualNode; zCanvas : TCanvas; X, Y : integer);

 procedure DibujarTriangulo_Plus(Pos_X, Pos_Y, Ancho : Longint);
 var
  Maz      : longint;
 begin
  Maz := zCanvas.Pen.Width;
  try
   zCanvas.Pen.Color   := Colors.TreeLineColor;
   zCanvas.Pen.Width   := 3;
   zCanvas.Brush.Color := color;
   Pos_X               := Pos_X -2;

   zCanvas.Line(Pos_X, Pos_Y + 1, Pos_X + (Ancho div 2), Pos_Y + (Ancho div 2) + 1);
   zCanvas.Line(Pos_X + (Ancho div 2), Pos_Y + (Ancho div 2) + 1, Pos_X, Pos_Y + Ancho + 1);

  finally
   zCanvas.Pen.Width := Maz;
  end;
 end;


 procedure DibujarTriangulo_Minus(Pos_X, Pos_Y, Ancho : Longint);
 var
  ResPos_Y : longint;
  Maz      : longint;
 begin
  Maz := zCanvas.Pen.Width;
  try
   zCanvas.Pen.Width   := 3;
   zCanvas.Pen.Color   := Colors.HotColor;
   zCanvas.Brush.Color := Colors.HotColor;
   ResPos_Y            := (Ancho div 2) + 2;
   Pos_Y               := Pos_Y - ResPos_Y;
   Pos_X               := Pos_X -2;
   zCanvas.Line(Pos_X, Pos_Y + (Ancho div 2) + 1,
               Pos_X + (Ancho div 2), Pos_Y + (Ancho )+1);

   zCanvas.Line(Pos_X + (Ancho div 2) - 1, Pos_Y + (Ancho ) + 1,
               Pos_X + (Ancho ), Pos_Y + (Ancho div 2) + 1);

  finally
   zCanvas.Pen.Width := Maz;
  end;
 end;


 procedure DibujarTriangulo_Plus_W7(Pos_X, Pos_Y, Ancho : Longint);
 begin
  zCanvas.Pen.Color   := Colors.TreeLineColor;
  zCanvas.Brush.Color := color;
  zCanvas.Polygon( [Point(Pos_X, Pos_Y + 1), Point(Pos_X + (Ancho div 2), Pos_Y + (Ancho div 2) + 1), Point(Pos_X, Pos_Y + Ancho + 1)]);
 end;


 procedure DibujarTriangulo_Minus_W7(Pos_X, Pos_Y, Ancho : Longint);
 begin
  zCanvas.Pen.Color   := Colors.HotColor;
  zCanvas.Brush.Color := Colors.HotColor;
  zCanvas.Polygon( [Point(Pos_X,  Pos_Y + (Ancho div 2)), Point(Pos_X +  (Ancho div 2), Pos_Y + (Ancho div 2)), Point(Pos_X + (Ancho div 2),  Pos_Y)]);
 end;


var
 Brush_PreColor  : Tcolor;
 Pen_PreColor    : TColor;

begin
 // Dibuja los botones del arbol
 if (toShowButtons in TreeOptions.PaintOptions) and (vsHasChildren in Node^.States) and
    not ((vsAllChildrenHidden in Node^.States) and(toAutoHideButtons in TreeOptions.AutoOptions)) then
  begin
    Brush_PreColor  := zCanvas.Brush.Color;
    Pen_PreColor    := zCanvas.Pen.Color;
    try
      if FDibujarBotonesModernos then
      begin
        if not (vsExpanded in Node^.States) then
        DibujarTriangulo_Plus(((X - DESVIACION) + 1) + 3 + 2+2, Y + 6 , 8)
        else
        DibujarTriangulo_Minus(((X - DESVIACION) + 1) + 3 + 2,  Y + 9 , 8);
      end;
    finally
      zCanvas.Pen.Color   := Pen_PreColor;
      zCanvas.Brush.Color := Brush_PreColor;
      zCanvas.Brush.Style := bsClear;
    end;
  end;
end;


procedure TLazVirtualStringTree.SetOcultarBotonesAlperderFoco(Value : Boolean);
begin
  if FOcultarBotonesAlperderFoco <> Value then
  begin
    FOcultarBotonesAlperderFoco := Value;
    if not FOcultarBotonesAlperderFoco then
      TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowButtons]
    else
      TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowButtons]
  end;
end;


procedure TLazVirtualStringTree.DoExit;
begin
  if FOcultarBotonesAlperderFoco then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowButtons];

  inherited DoExit;
end;


procedure TLazVirtualStringTree.DoEnter;
begin
  if FOcultarBotonesAlperderFoco then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowButtons];

  inherited DoEnter;
end;


procedure TLazVirtualStringTree.MouseEnter;
begin
  if FOcultarBotonesAlperderFoco then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowButtons];

  inherited MouseEnter;
end;


procedure TLazVirtualStringTree.MouseLeave;
begin
  if FOcultarBotonesAlperderFoco then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowButtons];

  inherited MouseLeave;
end;

procedure TLazVirtualStringTree.SetMostrarLineasArbol(Value : Boolean);
begin
 if FMostrarLineasArbol <> Value then
  begin
   FMostrarLineasArbol := Value;
   if FMostrarLineasArbol then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines]
   else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines]
  end;
end;


procedure TLazVirtualStringTree.SetMostrarLineasArbolPunteadas(Value : Boolean);
begin
 if FMostrarLineasArbolPunteadas <> Value then
  begin
   FMostrarLineasArbolPunteadas := Value;
   if FMostrarLineasArbolPunteadas then
    LineStyle := lsDotted
   else
    LineStyle := lsSolid;
  end;
end;

end.
