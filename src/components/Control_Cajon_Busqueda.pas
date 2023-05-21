(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-05-22 00:02:29
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-05-22 00:03:36
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
unit Control_Cajon_Busqueda;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Graphics
  , Controls
  , Lmessages
  , StdCtrls
  , ExtCtrls
  , LCLType
  , Buttons
  ;

type



{  TEditFondo }
 TEditFondo = class(TEdit)
 private
 protected
 public
  constructor Create(AOwner: TComponent); override;
 end;


{ TPanelBusqueda }

 TPanelBusqueda = class(TPanel)
 private
  FEditBusqueda : TEditFondo;
  FImageLupa    : TImage;
  FBotonBorrar  : TSpeedButton;

  FOnChange     : TNotifyEvent;
 protected
  procedure DoOnChange;
  procedure DoEdit_BuscarChange(Sender: TObject);
  procedure DoBotonBorrar_Click(Sender: TObject);

  procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;



  property EditBusqueda : TEditFondo read FEditBusqueda;
  property ImageLupa    : TImage read FImageLupa;
  property BotonBorrar  : TSpeedButton read FBotonBorrar;
  property OnChange     : TNotifyEvent read FOnChange write FOnChange;
 end;


// TEdit = class(TEditFondo);

implementation

{$R control_cajon_busqueda.res}




{  TEditFondo }

constructor TEditFondo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DoubleBuffered := true;
 TextHint       := 'Buscar...';
end;



{ TPanelBusqueda }

constructor TPanelBusqueda.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DoubleBuffered         := true;

// ParentBackground  := false;

 ParentColor       := true;

 BevelInner        := bvnone;
 BevelOuter        := bvnone;

 Width                  := 120;

 FImageLupa             := TImage.Create(Self);
 FImageLupa.Parent      := self;
 FImageLupa.Left        := 0;
 FImageLupa.Top         := 0;
 FImageLupa.Width       := 32;
 FImageLupa.Height      := 32;
 FImageLupa.Center      := true;



 FEditBusqueda          := TEditFondo.Create(Self);
 FEditBusqueda.Parent   := Self;
 FEditBusqueda.Left     := FImageLupa.Left + FImageLupa.Width + 2;
 FEditBusqueda.Top      := 5;
 FEditBusqueda.Width    := 56;
 FEditBusqueda.Anchors  := [akTop,akLeft,akRight];

 FEditBusqueda.OnChange := @DoEdit_BuscarChange;

 FBotonBorrar           := TSpeedButton.Create(Self);
 FBotonBorrar.Parent    := self;
 FBotonBorrar.Left      := FEditBusqueda.Left + FEditBusqueda.Width + 2;
 FBotonBorrar.Top       := 4;
 FBotonBorrar.Width     := 24;
 FBotonBorrar.Height    := 24;
 FBotonBorrar.Flat      := true;
 FBotonBorrar.Enabled   := false;
 FBotonBorrar.Anchors   := [akTop,akRight];
 FImageLupa.Picture.LoadFromResourceName(HInstance, 'CAJON_BUSQUEDA_BORRAR');
 FBotonBorrar.Glyph.Assign(FImageLupa.Picture.Bitmap);
 FImageLupa.Picture.LoadFromResourceName(HInstance, 'CAJON_BUSQUEDA_LUPA');
 FBotonBorrar.OnClick   := @DoBotonBorrar_Click;
 Height                 := FEditBusqueda.Height + 10;
end;


destructor TPanelBusqueda.Destroy;
begin
 FreeAndNil(FEditBusqueda);
 FreeAndNil(FImageLupa);
 FreeAndNil(FBotonBorrar);

 inherited Destroy;
end;


procedure TPanelBusqueda.DoOnChange;
begin
 FBotonBorrar.Enabled := FEditBusqueda.Text <> '';

 if assigned(FOnChange) then
  FOnChange(Self);
end;


procedure TPanelBusqueda.DoEdit_BuscarChange(Sender: TObject);
begin
 FBotonBorrar.Enabled := FEditBusqueda.Text <> '';
 DoOnChange;
end;


procedure TPanelBusqueda.DoBotonBorrar_Click(Sender: TObject);
begin
 FEditBusqueda.Text := '';
 DoOnChange;
end;


procedure TPanelBusqueda.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1; // Fake erase
  inherited WMEraseBkgnd(Message);
end;


end.

