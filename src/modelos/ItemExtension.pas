(**
 * @Author: Juan Manuel Soltero Sánchez
 * @Date:   2023-04-15 15:04:15
 * @Last Modified by:   Juan Manuel Soltero Sánchez
 * @Last Modified time: 2023-06-04 23:49:23
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

unit ItemExtension;

{$mode ObjFPC}{$H+}

interface

uses
classes
, ItemBase
, graphics;

type
  { TItemExtension }
  TItemExtension = class(TItemBase)
  private
    FDescripcion : RawByteString;
    FIcono       : TPortableNetworkGraphic;
    FIcono32     : TPortableNetworkGraphic;

    FStreamIcono   : TMemoryStream;
    FStreamIcono32 : TMemoryStream;

    FIdIcono     : Qword;
    procedure SetIcono(AValue: TPortableNetworkGraphic);
    procedure SetIcono32(AValue: TPortableNetworkGraphic);
  protected
    // Genera el ID del icono
    procedure DoGenerarIDIcono();
  public
    // Constructor de la clase
    constructor Create(const ANombre: RawByteString; const ADescripcion: RawByteString; AIcono : TPortableNetworkGraphic; AIcono32 : TPortableNetworkGraphic);

    // Constructor de la clase
    constructor CreateMemoryStream(const ANombre: RawByteString; const ADescripcion: RawByteString; AIcono : TMemoryStream; AIcono32 : TMemoryStream);


    // Destructor de la clase
    destructor Destroy; override;

    // Propiedades
    property Descripcion : RawByteString read FDescripcion;
    property Icono       : TPortableNetworkGraphic read FIcono write SetIcono;
    property Icono32     : TPortableNetworkGraphic read FIcono32 write SetIcono32;
    property IdIcono     : Qword read FIdIcono write FIdIcono;

    property StreamIcono   : TMemoryStream read FStreamIcono;
    property StreamIcono32 : TMemoryStream read FStreamIcono32;
  end;


implementation

uses
  Control_CRC
  ;

{ TItemExtension }

{ TItemExtension }
constructor TItemExtension.Create(const ANombre: RawByteString; const ADescripcion: RawByteString; AIcono : TPortableNetworkGraphic; AIcono32 : TPortableNetworkGraphic);
begin
  inherited Create(ANombre);
  FDescripcion := ADescripcion;
  FIcono       := AIcono;
  FIcono32     := AIcono32;

  // Generamos el ID del icono
  DoGenerarIDIcono();
end;

 // Constructor de la clase
 constructor TItemExtension.CreateMemoryStream(const ANombre: RawByteString; const ADescripcion: RawByteString; AIcono : TMemoryStream; AIcono32 : TMemoryStream);
  begin
    inherited Create(ANombre);

    FDescripcion   := ADescripcion;
    FStreamIcono   := AIcono;
    FStreamIcono32 := AIcono32;

    // Generamos el ID del icono
    FIdIcono := CRC64_From_PByte(PByte(FStreamIcono.Memory), FStreamIcono.Size);
  end;



destructor TItemExtension.Destroy;
begin
  if FIcono <> nil then
    FIcono.Free;

  if FIcono32 <> nil then
    FIcono32.Free;

  if FStreamIcono <> nil then
    FStreamIcono.Free;

  if FStreamIcono32 <> nil then
    FStreamIcono32.Free;

  inherited Destroy;
end;

// Genera el ID del icono
procedure TItemExtension.DoGenerarIDIcono();
Var
  memoria : TMemoryStream;
begin
  if FIcono <> nil then
    begin
      memoria := TMemoryStream.Create;
      try
        FIcono.SaveToStream(memoria);
        FIdIcono := CRC64_From_PByte(PByte(memoria.Memory), memoria.Size);
      finally
        memoria.Free;
      end;
    end;
end;

// Setter
procedure TItemExtension.SetIcono(AValue: TPortableNetworkGraphic);
begin
  if FIcono = AValue then Exit;
  FIcono := AValue;

  // Generamos el ID del icono
  DoGenerarIDIcono();
end;

// Setter
procedure TItemExtension.SetIcono32(AValue: TPortableNetworkGraphic);
begin
  if FIcono32 = AValue then Exit;
  FIcono32 := AValue;
end;



end.
