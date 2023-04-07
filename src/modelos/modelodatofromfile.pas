unit ModeloDatoFromFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TDatoItem }
  TDatoItem = class
  private
    FId                  : int64;
    FIsDir               : Boolean;
    FAttributes          : Integer;
    FFileTime            : TDateTime;
    FSize                : int64;
    FNombre              : RawByteString;
    FExtension           : RawByteString;
    FRutaId              : integer;
    FRutaString          : RawByteString;
    FImageIndex          : Integer;
    FImageIndexSistema   : Integer;
    FIsExe               : boolean;
  public
    property Id                : int64 read FId;
    property IsDir             : Boolean read FIsDir;
    property Attributes        : Integer read FAttributes;
    property FileTime          : TDateTime read FFileTime;
    property Size              : int64 read FSize;
    property Nombre            : RawByteString read FNombre;
    property Extension         : RawByteString read FExtension;
    property RutaId            : integer read FRutaId;
    property RutaString        : RawByteString read FRutaString;
    property ImageIndex        : Integer read FImageIndex;
    property IsExe             : boolean read FIsExe;
    property ImageIndexSistema : Integer read FImageIndexSistema write FImageIndex;
  end;

implementation

end.

