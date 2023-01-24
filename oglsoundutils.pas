{
  OGLSoundUtils - part of SoundUtils_iLya2IK:
   Utilities and abstract classes for working with sound, audio samples,
   audio streams, files, decoders and encoders.

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit OGLSoundUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, OGLFastVariantHash,
  OGLSoundUtilTypes;

type

  { ISoundEncoderProps }

  ISoundEncoderProps = interface(IFastHashList)
  ['{3AB017B6-7B3A-4C9F-900D-BD6CA2E8D728}']

  function GetMode : TSoundEncoderMode;
  function GetChannels : Cardinal;
  function GetFrequency : Cardinal;
  function GetBitrate : Cardinal;
  function GetSampleSize : TSoundSampleSize;
  function GetQuality : Single;

  procedure SetBitrate(AValue : Cardinal);
  procedure SetChannels(AValue : Cardinal);
  procedure SetFrequency(AValue : Cardinal);
  procedure SetMode(AValue : TSoundEncoderMode);
  procedure SetQuality(AValue : Single);
  procedure SetSampleSize(AValue : TSoundSampleSize);

  function HasProp(AValue : Cardinal) : Boolean;

  property Mode : TSoundEncoderMode read GetMode write SetMode;
  property Channels : Cardinal read GetChannels write SetChannels;
  property Frequency : Cardinal read GetFrequency write SetFrequency;
  property Bitrate : Cardinal read GetBitrate write SetBitrate;
  property SampleSize : TSoundSampleSize read GetSampleSize write SetSampleSize;
  property Quality : Single read GetQuality write SetQuality;
  end;

  { ISoundFrameSize }

  ISoundFrameSize = interface
  ['{D8F3F227-CD10-4D1A-B0A8-B463F3BCBCD1}']
  function GetBitDepth : Byte;
  function GetByteDepth : Byte;
  function GetChannels : Cardinal;
  function GetFrequency : Cardinal;
  function GetSampleSize : TSoundSampleSize;

  procedure SetBitDepth(AValue : Byte);
  procedure SetByteDepth(AValue : Byte);
  procedure SetChannels(AValue : Cardinal);
  procedure SetFrequency(AValue : Cardinal);
  procedure SetSampleSize(AValue : TSoundSampleSize);

  procedure InitDuration(aFreq : Cardinal; aChannels : Cardinal;
                       aSampleSize : TSoundSampleSize;
                       aDurationMs : Single);
  procedure InitBytes(aFreq : Cardinal; aChannels : Cardinal;
                       aSampleSize : TSoundSampleSize;
                       aBytes : Cardinal);
  procedure InitSamples(aFreq : Cardinal; aChannels : Cardinal;
                       aSampleSize : TSoundSampleSize;
                       aSamples : Cardinal);
  procedure InitError;
  procedure InitEmpty(aFreq : Cardinal; aChannels : Cardinal;
                      aSampleSize : TSoundSampleSize);
  procedure InitEmpty(aSrc : ISoundFrameSize);

  procedure Assign(aSrc : ISoundFrameSize);
  function Duplicate : ISoundFrameSize;
  function EmptyDuplicate : ISoundFrameSize;

  function AsBytes : Cardinal;
  function AsSamples : Cardinal;
  function AsDurationMs : Single;
  function AsDurationSec : Single;
  function IsValid : Boolean;
  function IsEmpty : Boolean;
  function IsErrored : Boolean;
  function IsEmptyOrErrored : Boolean;

  function Equal(aSecond : ISoundFrameSize) : Boolean;
  function Less(aSecond : ISoundFrameSize) : Boolean;
  function Greater(aSecond : ISoundFrameSize) : Boolean;
  function LessOrEqual(aSecond : ISoundFrameSize) : Boolean;
  function GreaterOrEqual(aSecond : ISoundFrameSize) : Boolean;
  function Minus(aSecond : ISoundFrameSize) : ISoundFrameSize;
  function Plus(aSecond : ISoundFrameSize) : ISoundFrameSize;
  procedure Clear;
  procedure Inc(aInc : ISoundFrameSize);
  procedure Dec(aDec : ISoundFrameSize);
  procedure IncSamples(aInc : Cardinal);
  procedure IncBytes(aInc : Cardinal);
  procedure DecSamples(aDec : Cardinal);
  procedure DecBytes(aDec : Cardinal);

  property Frequency  : Cardinal read GetFrequency write SetFrequency;
  property Channels   : Cardinal read GetChannels write SetChannels;
  property SampleSize : TSoundSampleSize read GetSampleSize write SetSampleSize;
  property BitDepth   : Byte read GetBitDepth write SetBitDepth;
  property ByteDepth  : Byte read GetByteDepth write SetByteDepth;
  end;

  { ISoundDataWriter }

  ISoundDataWriter = interface
  ['{5C1D2BE5-36BC-4DE3-8C33-A716D406F10B}']
  //method to write encoded data
  function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer;
  end;

  { ISoundDataReader }

  ISoundDataReader = interface
  ['{22230C39-08BD-4A4D-A918-B063D6EDAF6D}']
  //method to read encoded data from stream
  function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer;
  //method to seek in encoded stream
  function DoSeek(_offset:Int64; _whence:Integer): Integer;
  //method to tell current position in encoded stream
  function DoTell:Int64;
  end;

  { ISoundEncDec }

  ISoundEncDec = interface
  ['{E25604A7-3873-4703-856C-FAB5AC97EA8C}']
  function GetBitdepth : Cardinal;
  function GetBitrate : Cardinal;
  function GetChannels : Cardinal;
  function GetFrequency : Cardinal;
  function GetSampleSize : TSoundSampleSize;
  function GetVersion : Integer;

  procedure SetBitdepth({%H-}AValue : Cardinal);
  procedure SetBitrate({%H-}AValue : Cardinal);
  procedure SetChannels({%H-}AValue : Cardinal);
  procedure SetFrequency({%H-}AValue : Cardinal);
  procedure SetSampleSize({%H-}AValue : TSoundSampleSize);

  procedure Done;

  function InternalType : TSoundEncDecType;
  function Ready : Boolean;

  function EmptyFrame : ISoundFrameSize;
  function FrameFromSamples(aValue : Cardinal) : ISoundFrameSize;
  function FrameFromBytes(aValue : Cardinal) : ISoundFrameSize;
  function FrameFromDuration(aValue : Single) : ISoundFrameSize;

  property Channels : Cardinal read GetChannels write SetChannels;
  property Frequency : Cardinal read GetFrequency write SetFrequency;
  property Bitrate : Cardinal read GetBitrate write SetBitrate;
  property Bitdepth : Cardinal read GetBitdepth write SetBitdepth;
  property SampleSize : TSoundSampleSize read GetSampleSize write SetSampleSize;
  property Version : Integer read GetVersion;
  end;

  { ISoundEncoder }

  ISoundEncoder = interface(ISoundEncDec)
  ['{86E41033-9D2C-428E-9C1B-BD3F71275179}']
  function GetMode : TSoundEncoderMode;
  function GetQuality : Single;
  procedure SetMode(AValue : TSoundEncoderMode);
  procedure SetQuality(AValue : Single);

  procedure Init(aProps : ISoundEncoderProps;
                 aComments : ISoundComment);

  procedure InitWriter(aSrc : ISoundDataWriter);
  function Writer : ISoundDataWriter;

  //method to encode raw pcm data
  function  WriteData(Buffer : Pointer;
                 Count : ISoundFrameSize;
                 Par : Pointer) : ISoundFrameSize;
  //method to encode header/comments
  procedure WriteHeader(Par : Pointer);
  //method to close encoder (write last packet/flush/finalize encoder)
  procedure Close(Par : Pointer);
  //method to flush encoder (write last packet/flush encoder)
  procedure Flush(Par : Pointer);

  property Quality : Single read GetQuality write SetQuality;
  property Mode : TSoundEncoderMode read GetMode write SetMode;
  end;

  { TSoundDecoder }

  ISoundDecoder = interface(ISoundEncDec)
  ['{8D96AD54-0E37-43FE-A37B-7170F4AAF0B6}']
  procedure Init;

  procedure InitReader(aSrc : ISoundDataReader);
  function Reader : ISoundDataReader;

  //method to read decoded data
  function  ReadData(Buffer : Pointer;
                     Count : ISoundFrameSize;
                     Par : Pointer) : ISoundFrameSize;
  //method to reset decoder
  procedure ResetToStart;
  end;

  { TSoundEncoderProps }

  TSoundEncoderProps = class(TFastHashList, ISoundEncoderProps)
  protected
    function GetMode : TSoundEncoderMode;
    function GetChannels : Cardinal;
    function GetFrequency : Cardinal;
    function GetBitrate : Cardinal;
    function GetSampleSize : TSoundSampleSize;
    function GetQuality : Single;

    procedure SetBitrate(AValue : Cardinal);
    procedure SetChannels(AValue : Cardinal);
    procedure SetFrequency(AValue : Cardinal);
    procedure SetMode(AValue : TSoundEncoderMode);
    procedure SetQuality(AValue : Single);
    procedure SetSampleSize(AValue : TSoundSampleSize);
  public
    function HasProp(AValue : Cardinal) : Boolean;
  end;

  { TSoundFrameSize }

  TSoundFrameSize = class(TInterfacedObject, ISoundFrameSize)
  private
    fFreq : Cardinal;
    fChannels : Byte;
    fSampleSize : TSoundSampleSize;
    fBytes      : Cardinal;
    fSamples    : Cardinal;
  protected
    function GetBitDepth : Byte;
    function GetByteDepth : Byte;
    function GetChannels : Cardinal;
    function GetFrequency : Cardinal;
    function GetSampleSize : TSoundSampleSize;

    procedure SetBitDepth(AValue : Byte);
    procedure SetByteDepth(AValue : Byte);
    procedure SetChannels(AValue : Cardinal);
    procedure SetFrequency(AValue : Cardinal);
    procedure SetSampleSize(AValue : TSoundSampleSize);

    procedure InitDuration(aFreq : Cardinal; aChannels : Cardinal;
                           aSampleSize : TSoundSampleSize;
                           aDurationMs : Single);
    procedure InitBytes(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aBytes : Cardinal);
    procedure InitSamples(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aSamples : Cardinal);
    procedure InitError;
    procedure InitEmpty(aFreq : Cardinal; aChannels : Cardinal;
                        aSampleSize : TSoundSampleSize); overload;
    procedure InitEmpty(aSrc : ISoundFrameSize); overload;

    procedure RecalcBytes;
    procedure RecalcSamples;
  public
    constructor CreateFromDuration(aFreq : Cardinal; aChannels : Cardinal;
                                   aSampleSize : TSoundSampleSize;
                                   aDurationMs : Single);
    constructor CreateFromBytes(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aBytes : Cardinal);
    constructor CreateFromSamples(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aSamples : Cardinal);
    constructor CreateFromFrame(aSrc : ISoundFrameSize);
    constructor CreateError;
    constructor CreateEmpty(aFreq : Cardinal; aChannels : Cardinal;
                            aSampleSize : TSoundSampleSize); overload;
    constructor CreateEmpty(aSrc : ISoundFrameSize); overload;

    procedure Assign(aSrc : ISoundFrameSize);
    function Duplicate : ISoundFrameSize;
    function EmptyDuplicate : ISoundFrameSize;

    function Equal(aSecond : ISoundFrameSize) : Boolean;
    function Less(aSecond : ISoundFrameSize) : Boolean;
    function Greater(aSecond : ISoundFrameSize) : Boolean;
    function LessOrEqual(aSecond : ISoundFrameSize) : Boolean;
    function GreaterOrEqual(aSecond : ISoundFrameSize) : Boolean;
    function Minus(aSecond : ISoundFrameSize) : ISoundFrameSize;
    function Plus(aSecond : ISoundFrameSize) : ISoundFrameSize;
    procedure Clear;
    procedure Inc(aInc : ISoundFrameSize);
    procedure Dec(aDec : ISoundFrameSize);
    procedure IncSamples(aInc : Cardinal);
    procedure IncBytes(aInc : Cardinal);
    procedure DecSamples(aDec : Cardinal);
    procedure DecBytes(aDec : Cardinal);

    function AsBytes : Cardinal;
    function AsSamples : Cardinal;
    function AsDurationMs : Single;
    function AsDurationSec : Single;
    function IsValid : Boolean;
    function IsEmpty : Boolean;
    function IsErrored : Boolean;
    function IsEmptyOrErrored : Boolean;
  end;

  { TSoundAbstractEncDec }

  TSoundAbstractEncDec = class(TInterfacedObject, ISoundEncDec)
  protected
    function GetBitdepth : Cardinal; virtual;
    function GetBitrate : Cardinal; virtual; abstract;
    function GetChannels : Cardinal; virtual; abstract;
    function GetFrequency : Cardinal; virtual; abstract;
    function GetSampleSize : TSoundSampleSize; virtual;
    function GetVersion : Integer; virtual; abstract;

    procedure SetBitdepth({%H-}AValue : Cardinal); virtual;
    procedure SetBitrate({%H-}AValue : Cardinal); virtual;
    procedure SetChannels({%H-}AValue : Cardinal); virtual;
    procedure SetFrequency({%H-}AValue : Cardinal); virtual;
    procedure SetSampleSize({%H-}AValue : TSoundSampleSize); virtual;

    procedure Done; virtual; abstract;
  public
    function Comments : ISoundComment; virtual; abstract;

    function InternalType : TSoundEncDecType; virtual; abstract;
    function Ready : Boolean; virtual; abstract;

    function EmptyFrame : ISoundFrameSize;
    function FrameFromSamples(aValue : Cardinal) : ISoundFrameSize;
    function FrameFromBytes(aValue : Cardinal) : ISoundFrameSize;
    function FrameFromDuration(aValue : Single) : ISoundFrameSize;
  end;

  { TSoundAbstractEncoder }

  TSoundAbstractEncoder = class(TSoundAbstractEncDec, ISoundEncoder)
  private
    fWriteImplementer : ISoundDataWriter;
  protected
    function GetMode : TSoundEncoderMode; virtual; abstract;
    function GetQuality : Single; virtual; abstract;
    procedure SetMode({%H-}AValue : TSoundEncoderMode); virtual;
    procedure SetQuality({%H-}AValue : Single); virtual;

    procedure Init({%H-}aProps : ISoundEncoderProps;
                   {%H-}aComment : ISoundComment); virtual; abstract;
    procedure InitWriter(aSrc : ISoundDataWriter); virtual;
    function Writer : ISoundDataWriter; virtual;
  public
    function InternalType : TSoundEncDecType; override;

    //method to encode raw pcm data
    function  WriteData({%H-}Buffer : Pointer;
                   {%H-}Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; virtual;
    //method to encode header/comments
    procedure WriteHeader({%H-}Par : Pointer); virtual;
    //method to close encoder (write last packet/flush/finalize encoder)
    procedure Close({%H-}Par : Pointer); virtual;
    //method to flush encoder (write last packet/flush encoder)
    procedure Flush({%H-}Par : Pointer); virtual;
  end;

  { TSoundAbstractDecoder }

  TSoundAbstractDecoder = class(TSoundAbstractEncDec, ISoundDecoder)
  private
    fReadImplementer : ISoundDataReader;
  protected
    procedure Init; virtual; abstract;

    procedure InitReader(aSrc : ISoundDataReader); virtual;
    function Reader : ISoundDataReader; virtual;
  public
    //method to read decoded data
    function  ReadData({%H-}Buffer : Pointer;
                   {%H-}Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; virtual;
    //method to reset decoder
    procedure ResetToStart; virtual;

    function InternalType : TSoundEncDecType; override;
  end;

  { TSoundStreamReadWrite }

  TSoundStreamReadWrite = class(TInterfacedObject)
  private
    fStream : TStream;
  protected
    procedure SetStream(aStream : TStream); virtual;
    function GetStream : TStream; virtual;
  public
    constructor Create(aStream : TStream);

    property Stream : TStream read GetStream write SetStream;
  end;

  { TSoundStreamDataWriter }

  TSoundStreamDataWriter = class(TSoundStreamReadWrite, ISoundDataWriter)
  public
    //method to write encoded data
    function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer; virtual;
  end;

  { TSoundStreamDataReader }

  TSoundStreamDataReader = class(TSoundStreamReadWrite, ISoundDataReader)
  public
    //method to read encoded data from stream
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; virtual;
    //method to seek in encoded stream
    function DoSeek(_offset:Int64; _whence:Integer): Integer; virtual;
    //method to tell current position in encoded stream
    function DoTell:Int64; virtual;
  end;

  { TSoundStreamForwardDataReader }

  TSoundStreamForwardDataReader = class(TSoundStreamDataReader)
  public
    function DoSeek(_offset:Int64; _whence:Integer): Integer; override;
    function DoTell:Int64; override;
  end;

  { TSoundFile }

  TSoundFile = class
  private
    fStream: TStream;

    //encoder/decoder spec
    fEncDec : ISoundEncDec;
  protected
    procedure Clean; virtual;
    procedure WriteHeader; virtual;
    function InitEncoder(aProps : ISoundEncoderProps;
                   aComments : ISoundComment) : ISoundEncoder; virtual;
                                                                abstract;
    function InitDecoder : ISoundDecoder; virtual; abstract;
  public
    destructor Destroy; override;

    function Stream : TStream; virtual;

    function LoadFromFile(const aFileName : String; const aInMemory : Boolean
      ) : Boolean; virtual;
    function LoadFromStream(Str : TStream) : Boolean; virtual;
    function ReadData(Buffer : Pointer;
                      aFrameSize : ISoundFrameSize;
                      Ptr : Pointer) : ISoundFrameSize; virtual;
    procedure ResetToStart; virtual;
    function Decoder : ISoundDecoder;
    function DecoderReady : Boolean; virtual;

    function SaveToFile(const aFileName : String;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function SaveToStream(Str : TStream;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function WriteData(Buffer : Pointer;
                       aFrameSize : ISoundFrameSize;
                       Ptr : Pointer) : ISoundFrameSize; virtual;
    procedure StopStreaming; virtual;
    function Encoder : ISoundEncoder;
    function EncoderReady : Boolean; virtual;

    function Frequency : Cardinal; virtual;
    function Bitrate  : Cardinal; virtual;
    function Bitdepth : Cardinal; virtual;
    function SampleSize : TSoundSampleSize; virtual;
    function Channels : Cardinal; virtual;
    function Version  : Cardinal; virtual;
  end;

  { TOGLSound }

  TOGLSound = class
  public
    class function FrameFromDuration(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aDurationMs : Single) : ISoundFrameSize;
    class function FrameFromSamples(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aSamples : Cardinal) : ISoundFrameSize;
    class function FrameFromBytes(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize;
                         aBytes : Cardinal) : ISoundFrameSize;
    class function NewFrame(aSrc : ISoundFrameSize) : ISoundFrameSize;
    class function NewEmptyFrame(aSrc : ISoundFrameSize) : ISoundFrameSize; overload;
    class function NewEmptyFrame(aFreq : Cardinal; aChannels : Cardinal;
                         aSampleSize : TSoundSampleSize) : ISoundFrameSize; overload;

    class function NewStreamWriter(aStream : TStream) : ISoundDataWriter;
    class function NewStreamReader(aStream : TStream) : ISoundDataReader;
    class function NewStreamForwardReader(aStream : TStream) : ISoundDataReader;

    class function BitdepthToSampleSize(bitdepth : Byte) : TSoundSampleSize;
    class function SampleSizeToBitdepth(samplsz : TSoundSampleSize) : Byte;
    class function SampleSizeToBytedepth(samplsz : TSoundSampleSize) : Byte;

    class function NewErrorFrame : ISoundFrameSize;

    class function EncProps(const Vals : Array of Variant) : ISoundEncoderProps;

    // Encoder properties
    class function PROP_MODE : Cardinal; inline;
    class function PROP_CHANNELS : Cardinal; inline;
    class function PROP_FREQUENCY : Cardinal; inline;
    class function PROP_BITRATE : Cardinal; inline;
    class function PROP_SAMPLE_SIZE : Cardinal; inline;
    class function PROP_QUALITY : Cardinal; inline;
  end;


implementation

{ TSoundEncoderProps }

function TSoundEncoderProps.GetMode : TSoundEncoderMode;
begin
  Result := TSoundEncoderMode(Get(TOGLSound.PROP_MODE));
end;

function TSoundEncoderProps.GetChannels : Cardinal;
begin
  Result := Get(TOGLSound.PROP_CHANNELS);
end;

function TSoundEncoderProps.GetFrequency : Cardinal;
begin
  Result := Get(TOGLSound.PROP_FREQUENCY);
end;

function TSoundEncoderProps.GetBitrate : Cardinal;
begin
  Result := Get(TOGLSound.PROP_BITRATE);
end;

function TSoundEncoderProps.GetSampleSize : TSoundSampleSize;
begin
  Result := TSoundSampleSize(Get(TOGLSound.PROP_SAMPLE_SIZE));
end;

function TSoundEncoderProps.GetQuality : Single;
begin
  Result := Get(TOGLSound.PROP_QUALITY);
end;

procedure TSoundEncoderProps.SetBitrate(AValue : Cardinal);
begin
  Add(TOGLSound.PROP_BITRATE, AValue);
end;

procedure TSoundEncoderProps.SetChannels(AValue : Cardinal);
begin
  Add(TOGLSound.PROP_CHANNELS, AValue);
end;

procedure TSoundEncoderProps.SetFrequency(AValue : Cardinal);
begin
  Add(TOGLSound.PROP_FREQUENCY, AValue);
end;

procedure TSoundEncoderProps.SetMode(AValue : TSoundEncoderMode);
begin
  Add(TOGLSound.PROP_MODE, Integer(AValue));
end;

procedure TSoundEncoderProps.SetQuality(AValue : Single);
begin
  Add(TOGLSound.PROP_QUALITY, AValue);
end;

procedure TSoundEncoderProps.SetSampleSize(AValue : TSoundSampleSize);
begin
  Add(TOGLSound.PROP_SAMPLE_SIZE, Integer(AValue));
end;

function TSoundEncoderProps.HasProp(AValue : Cardinal) : Boolean;
begin
  Result := FindIndexOf(AValue) >= 0;
end;

{ TSoundStreamReadWrite }

procedure TSoundStreamReadWrite.SetStream(aStream : TStream);
begin
  fStream := aStream;
end;

function TSoundStreamReadWrite.GetStream : TStream;
begin
  Result := fStream;
end;

constructor TSoundStreamReadWrite.Create(aStream : TStream);
begin
  fStream := aStream;
end;

{ TSoundStreamForwardDataReader }

function TSoundStreamForwardDataReader.DoSeek(_offset : Int64; _whence : Integer
  ) : Integer;
begin
  Result := -1;
end;

function TSoundStreamForwardDataReader.DoTell : Int64;
begin
  Result := -1;
end;

{ TSoundStreamDataReader }

function TSoundStreamDataReader.DoRead(_ptr : Pointer; _nbytes : Integer) : Integer;
begin
  if (_nbytes <= 0) then begin Result := 0; Exit; end;
  try
    Result := Int64(fStream.Read(_ptr^, _nbytes));
  except
    Result := 0;
  end;
end;

function TSoundStreamDataReader.DoSeek(_offset : Int64; _whence : Integer) : Integer;
begin
  try
    with fStream do
      case _whence of
        0: Seek(_offset, soBeginning);
        1: Seek(_offset, soCurrent);
        2: Seek(_offset, soEnd);
      end;
    result := 0;
  except
    result := -1;
  end;
end;

function TSoundStreamDataReader.DoTell : Int64;
begin
  try
    result := fStream.Position;
  except
    result := -1;
  end;
end;

{ TSoundStreamDataWriter }

function TSoundStreamDataWriter.DoWrite(Buffer : Pointer; BufferSize : Integer
  ) : Integer;
begin
  if (BufferSize < 0) then Exit(-1);
  if not (Assigned(Buffer) and (BufferSize > 0)) then Exit(-1);
  fStream.Write(Buffer^, BufferSize);
  Result := 0;
end;

{ TSoundFile }

procedure TSoundFile.Clean;
begin
  fEncDec := nil;
  if Assigned(fStream) then
    FreeAndNil(fStream);
end;

procedure TSoundFile.WriteHeader;
begin
  if EncoderReady then
    Encoder.WriteHeader(nil);
end;

destructor TSoundFile.Destroy;
begin
  Clean;
  inherited Destroy;
end;

function TSoundFile.Stream : TStream;
begin
  Result := fStream;
end;

function TSoundFile.LoadFromFile(const aFileName : String;
  const aInMemory : Boolean) : Boolean;
var
  cFilestream : TFileStream;
  cStr : TStream;
begin
  if aInMemory then
  begin
    cFilestream := TFileStream.Create(aFileName, fmOpenRead);
    if Assigned(cFilestream) then
    begin
      try
        cStr := TMemoryStream.Create;
        cStr.CopyFrom(cFilestream, cFilestream.Size);
        cStr.Position := 0;
      finally
        cFilestream.Free;
      end;
    end else
      cStr := nil;
  end
  else
    cStr := TFileStream.Create(aFileName, fmOpenRead);

  Result := LoadFromStream(cStr);
end;

function TSoundFile.LoadFromStream(Str : TStream) : Boolean;
begin
  Clean;

  fStream := Str;

  try
    fEncDec :=  InitDecoder;
    Result := fEncDec.Ready;
  except
    on e : Exception do Result := false;
  end;
end;

function TSoundFile.ReadData(Buffer : Pointer; aFrameSize : ISoundFrameSize;
  Ptr : Pointer) : ISoundFrameSize;
var
  Res: ISoundFrameSize;
begin
  if Assigned(fStream) and DecoderReady then
  begin
    Result := TOGLSound.NewEmptyFrame(aFrameSize);

    while (Result.Less(aFrameSize)) do begin
      Res := Decoder.ReadData(@(PByte(Buffer)[Result.AsBytes]),
                                 aFrameSize.Minus(Result),
                                 Ptr);
      if Res.IsEmptyOrErrored then
        break else
        Result.Inc(Res);
    end;
  end else
    Result := TOGLSound.NewErrorFrame;
end;

procedure TSoundFile.ResetToStart;
begin
  if DecoderReady then
    Decoder.ResetToStart;
end;

function TSoundFile.Decoder : ISoundDecoder;
begin
  Result := fEncDec as ISoundDecoder;
end;

function TSoundFile.DecoderReady : Boolean;
begin
  Result := Assigned(fEncDec) and
            (fEncDec.InternalType = edtDecoder) and
            fEncDec.Ready;
end;

function TSoundFile.SaveToFile(const aFileName : String;
  aProps : ISoundEncoderProps;  aComments : ISoundComment) : Boolean;
var
  Str : TFileStream;
begin
  Str := TFileStream.Create(aFileName, fmOpenWrite or fmCreate);
  if Assigned(Str) then
    Result := SaveToStream(Str, aProps, aComments) else
      Result := false;
end;

function TSoundFile.SaveToStream(Str : TStream; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : Boolean;
begin
  Clean;

  fStream := Str;

  try
    try
      fEncDec :=  InitEncoder(aProps, aComments);
      Result := fEncDec.Ready;
    except
      on e : Exception do Result := false;
    end;
  finally
    if Result then
      WriteHeader;
  end;
end;

function TSoundFile.WriteData(Buffer : Pointer; aFrameSize : ISoundFrameSize;
  Ptr : Pointer) : ISoundFrameSize;
var
  Res : ISoundFrameSize;
begin
  if Assigned(fStream) and EncoderReady then
  begin
    Result := TOGLSound.NewEmptyFrame(aFrameSize);

    while Result.Less(aFrameSize) do
    begin
      Res := Encoder.WriteData(@(PByte(Buffer)[Result.AsBytes]),
                               aFrameSize.Minus(Result),
                               Ptr);
      if Res.IsEmptyOrErrored then Break;
      Result.Inc(Res);
    end;
  end else
    Result := TOGLSound.NewErrorFrame;
end;

procedure TSoundFile.StopStreaming;
begin
  if EncoderReady then
     Encoder.Close(nil);
end;

function TSoundFile.Encoder : ISoundEncoder;
begin
  Result := fEncDec as ISoundEncoder;
end;

function TSoundFile.EncoderReady : Boolean;
begin
  Result := Assigned(fEncDec) and
            (fEncDec.InternalType = edtEncoder) and
            fEncDec.Ready;
end;

function TSoundFile.Frequency : Cardinal;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.Frequency else
    Result := 0;
end;

function TSoundFile.Bitrate : Cardinal;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.Bitrate else
    Result := 0;
end;

function TSoundFile.Bitdepth : Cardinal;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.Bitdepth else
    Result := 0;
end;

function TSoundFile.SampleSize : TSoundSampleSize;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.SampleSize else
    Result := ss16bit;
end;

function TSoundFile.Channels : Cardinal;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.Channels else
    Result := 0;
end;

function TSoundFile.Version : Cardinal;
begin
  if Assigned(fEncDec) then
    Result := fEncDec.Version else
    Result := 0;
end;

{ TSoundAbstractDecoder }

procedure TSoundAbstractDecoder.InitReader(aSrc : ISoundDataReader);
begin
  fReadImplementer := aSrc;
end;

function TSoundAbstractDecoder.Reader : ISoundDataReader;
begin
  Result := fReadImplementer;
end;

function TSoundAbstractDecoder.ReadData(Buffer : Pointer;
  Count : ISoundFrameSize; Par : Pointer) : ISoundFrameSize;
begin
  Result := TOGLSound.NewErrorFrame;
end;

procedure TSoundAbstractDecoder.ResetToStart;
begin
  // do nothing
end;

function TSoundAbstractDecoder.InternalType : TSoundEncDecType;
begin
  Result := edtDecoder;
end;

{ TSoundAbstractEncoder }

procedure TSoundAbstractEncoder.SetMode(AValue : TSoundEncoderMode);
begin
  //do nothing
end;

procedure TSoundAbstractEncoder.SetQuality(AValue : Single);
begin
  //do nothing
end;

procedure TSoundAbstractEncoder.InitWriter(aSrc : ISoundDataWriter);
begin
  fWriteImplementer := aSrc;
end;

function TSoundAbstractEncoder.Writer : ISoundDataWriter;
begin
  Result := fWriteImplementer;
end;

function TSoundAbstractEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
begin
  Result := TOGLSound.NewErrorFrame;
end;

procedure TSoundAbstractEncoder.WriteHeader(Par : Pointer);
begin
  //do nothing
end;

procedure TSoundAbstractEncoder.Close(Par : Pointer);
begin
  //do nothing
end;

procedure TSoundAbstractEncoder.Flush(Par : Pointer);
begin
  //do nothing
end;

function TSoundAbstractEncoder.InternalType : TSoundEncDecType;
begin
  Result := edtEncoder;
end;

{ TSoundAbstractEncDec }

procedure TSoundAbstractEncDec.SetSampleSize(AValue : TSoundSampleSize);
begin
  //do nothing
end;

function TSoundAbstractEncDec.EmptyFrame : ISoundFrameSize;
begin
  Result := TOGLSound.NewEmptyFrame(GetFrequency, GetChannels, GetSampleSize);
end;

function TSoundAbstractEncDec.FrameFromSamples(aValue : Cardinal) : ISoundFrameSize;
begin
  Result := TOGLSound.FrameFromSamples(GetFrequency, GetChannels, GetSampleSize, aValue);
end;

function TSoundAbstractEncDec.FrameFromBytes(aValue : Cardinal) : ISoundFrameSize;
begin
  Result := TOGLSound.FrameFromBytes(GetFrequency, GetChannels, GetSampleSize, aValue);
end;

function TSoundAbstractEncDec.FrameFromDuration(aValue : Single) : ISoundFrameSize;
begin
  Result := TOGLSound.FrameFromDuration(GetFrequency, GetChannels, GetSampleSize, aValue);
end;

function TSoundAbstractEncDec.GetBitdepth : Cardinal;
begin
  Result := TOGLSound.SampleSizeToBitdepth(GetSampleSize);
end;

function TSoundAbstractEncDec.GetSampleSize : TSoundSampleSize;
begin
  Result := TOGLSound.BitdepthToSampleSize(GetBitdepth);
end;

procedure TSoundAbstractEncDec.SetBitdepth(AValue : Cardinal);
begin
  //do nothing
end;

procedure TSoundAbstractEncDec.SetBitrate(AValue : Cardinal);
begin
  //do nothing
end;

procedure TSoundAbstractEncDec.SetChannels(AValue : Cardinal);
begin
  //do nothing
end;

procedure TSoundAbstractEncDec.SetFrequency(AValue : Cardinal);
begin
  //do nothing
end;

{ TOGLSound }

class function TOGLSound.FrameFromDuration(aFreq : Cardinal;
  aChannels : Cardinal; aSampleSize : TSoundSampleSize; aDurationMs : Single
  ) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateFromDuration(aFreq, aChannels, aSampleSize,
                                               aDurationMs) as ISoundFrameSize;
end;

class function TOGLSound.FrameFromSamples(aFreq : Cardinal;
  aChannels : Cardinal; aSampleSize : TSoundSampleSize; aSamples : Cardinal
  ) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateFromSamples(aFreq, aChannels, aSampleSize,
                                              aSamples) as ISoundFrameSize;
end;

class function TOGLSound.FrameFromBytes(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize; aBytes : Cardinal) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateFromBytes(aFreq, aChannels, aSampleSize,
                                              aBytes) as ISoundFrameSize;
end;

class function TOGLSound.NewFrame(aSrc : ISoundFrameSize) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateFromFrame(aSrc) as ISoundFrameSize;
end;

class function TOGLSound.NewEmptyFrame(aSrc : ISoundFrameSize
  ) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateEmpty(aSrc) as ISoundFrameSize;
end;

class function TOGLSound.NewEmptyFrame(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize) : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateEmpty(aFreq, aChannels, aSampleSize) as ISoundFrameSize;
end;

class function TOGLSound.NewStreamWriter(aStream : TStream) : ISoundDataWriter;
begin
  Result := TSoundStreamDataWriter.Create(aStream) as ISoundDataWriter;
end;

class function TOGLSound.NewStreamReader(aStream : TStream) : ISoundDataReader;
begin
  Result := TSoundStreamDataReader.Create(aStream) as ISoundDataReader;
end;

class function TOGLSound.NewStreamForwardReader(aStream : TStream
  ) : ISoundDataReader;
begin
  Result := TSoundStreamForwardDataReader.Create(aStream) as ISoundDataReader;
end;

class function TOGLSound.BitdepthToSampleSize(bitdepth : Byte
  ) : TSoundSampleSize;
begin
  case bitdepth of
    8 : Result := ss8bit;
    32 : Result := ss32bit;
  else
    Result := ss16bit;
  end;
end;

class function TOGLSound.SampleSizeToBitdepth(samplsz : TSoundSampleSize
  ) : Byte;
begin
  case samplsz of
    ss8bit : Result := 8;
    ss32bit, ssFloat : Result := 32;
  else
    Result := 16;
  end;
end;

class function TOGLSound.SampleSizeToBytedepth(samplsz : TSoundSampleSize
  ) : Byte;
begin
  case samplsz of
    ss8bit : Result := 1;
    ss32bit, ssFloat : Result := 4;
  else
    Result := 2;
  end;
end;

class function TOGLSound.NewErrorFrame : ISoundFrameSize;
begin
  Result := TSoundFrameSize.CreateError as ISoundFrameSize;
end;

class function TOGLSound.EncProps(const Vals : array of Variant
  ) : ISoundEncoderProps;
var i : integer;
begin
  if Length(Vals) mod 2 > 0 then Exit(nil);
  Result := TSoundEncoderProps.Create;
  i := Low(Vals);
  while i < High(Vals) do
  begin
    if not VarIsOrdinal(Vals[i]) then exit;
    Result.Add(Vals[i], Vals[i+1]);
    Inc(i, 2);
  end;
end;

class function TOGLSound.PROP_MODE : Cardinal;
begin
  Result := $001;
end;

class function TOGLSound.PROP_CHANNELS : Cardinal;
begin
  Result := $002;
end;

class function TOGLSound.PROP_FREQUENCY : Cardinal;
begin
  Result := $003;
end;

class function TOGLSound.PROP_BITRATE : Cardinal;
begin
  Result := $004;
end;

class function TOGLSound.PROP_SAMPLE_SIZE : Cardinal;
begin
  Result := $005;
end;

class function TOGLSound.PROP_QUALITY : Cardinal;
begin
  Result := $006;
end;

{ TSoundFrameSize }

function TSoundFrameSize.GetBitDepth : Byte;
begin
  case fSampleSize of
    ss8bit  : Result := 8;
    ss16bit : Result := 16;
    ss32bit : Result := 32;
    ssFloat : Result := 32;
  else
    Result := 16;
  end;
end;

function TSoundFrameSize.GetByteDepth : Byte;
begin
  case fSampleSize of
    ss8bit  : Result := 1;
    ss16bit : Result := 2;
    ss32bit : Result := 4;
    ssFloat : Result := 4;
  else
    Result := 2;
  end;
end;

function TSoundFrameSize.GetChannels : Cardinal;
begin
  Result := fChannels;
end;

function TSoundFrameSize.GetFrequency : Cardinal;
begin
  Result := fFreq;
end;

function TSoundFrameSize.GetSampleSize : TSoundSampleSize;
begin
  Result := fSampleSize;
end;

procedure TSoundFrameSize.SetBitDepth(AValue : Byte);
begin
  if AValue <> GetBitDepth then
  begin
    case AValue of
      8  : fSampleSize := ss8bit;
      32 : fSampleSize := ss32bit;
    else
      fSampleSize := ss16bit;
    end;
    RecalcBytes;
  end;
end;

procedure TSoundFrameSize.SetByteDepth(AValue : Byte);
begin
  if AValue <> GetByteDepth then
  begin
    case AValue of
      1 : fSampleSize := ss8bit;
      4 : fSampleSize := ss32bit;
    else
      fSampleSize := ss16bit;
    end;
    RecalcBytes;
  end;
end;

procedure TSoundFrameSize.SetChannels(AValue : Cardinal);
begin
  if fChannels <> AValue then
  begin
    fChannels := AValue;
    RecalcBytes;
  end;
end;

procedure TSoundFrameSize.SetFrequency(AValue : Cardinal);
begin
  if fFreq <> AValue then
  begin
    fFreq := AValue;
    RecalcBytes;
  end;
end;

procedure TSoundFrameSize.SetSampleSize(AValue : TSoundSampleSize);
begin
  if fSampleSize <> AValue then
  begin
    fSampleSize := AValue;
    RecalcBytes;
  end;
end;

procedure TSoundFrameSize.InitDuration(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize; aDurationMs : Single);
begin
  fFreq := aFreq;
  fChannels := aChannels;
  fSampleSize := aSampleSize;

  fSamples := Round(Single(aFreq) * aDurationMs / 1000.0 + 0.5);
  RecalcBytes;
end;

procedure TSoundFrameSize.InitBytes(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize; aBytes : Cardinal);
begin
  fFreq := aFreq;
  fChannels := aChannels;
  fSampleSize := aSampleSize;

  fBytes := aBytes;
  RecalcSamples;
end;

procedure TSoundFrameSize.InitSamples(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize; aSamples : Cardinal);
begin
  fFreq := aFreq;
  fChannels := aChannels;
  fSampleSize := aSampleSize;

  fSamples := aSamples;
  RecalcBytes;
end;

procedure TSoundFrameSize.InitError;
begin
  fFreq := 0;
  fChannels := 0;

  fSamples := High(Cardinal);
  fBytes := High(Cardinal);
end;

procedure TSoundFrameSize.InitEmpty(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize);
begin
  fFreq := aFreq;
  fChannels := aChannels;
  fSampleSize := aSampleSize;

  fSamples := 0;
  fBytes := 0;
end;

procedure TSoundFrameSize.InitEmpty(aSrc : ISoundFrameSize);
begin
  fFreq := aSrc.Frequency;
  fChannels := aSrc.Channels;
  fSampleSize := aSrc.SampleSize;

  fSamples := 0;
  fBytes := 0;
end;

procedure TSoundFrameSize.RecalcBytes;
begin
  fBytes := fChannels * fSamples * GetByteDepth;
end;

procedure TSoundFrameSize.RecalcSamples;
begin
  fSamples := fBytes div fChannels div GetByteDepth;
end;

constructor TSoundFrameSize.CreateFromDuration(aFreq : Cardinal;
  aChannels : Cardinal; aSampleSize : TSoundSampleSize; aDurationMs : Single);
begin
  InitDuration(aFreq, aChannels, aSampleSize, aDurationMs);
end;

constructor TSoundFrameSize.CreateFromBytes(aFreq : Cardinal;
  aChannels : Cardinal; aSampleSize : TSoundSampleSize; aBytes : Cardinal);
begin
  InitBytes(aFreq, aChannels, aSampleSize, aBytes);
end;

constructor TSoundFrameSize.CreateFromSamples(aFreq : Cardinal;
  aChannels : Cardinal; aSampleSize : TSoundSampleSize; aSamples : Cardinal);
begin
  InitSamples(aFreq, aChannels, aSampleSize, aSamples);
end;

constructor TSoundFrameSize.CreateFromFrame(aSrc : ISoundFrameSize);
begin
  Assign(aSrc);
end;

constructor TSoundFrameSize.CreateError;
begin
  InitError;
end;

constructor TSoundFrameSize.CreateEmpty(aFreq : Cardinal; aChannels : Cardinal;
  aSampleSize : TSoundSampleSize);
begin
  InitEmpty(aFreq, aChannels, aSampleSize);
end;

constructor TSoundFrameSize.CreateEmpty(aSrc : ISoundFrameSize);
begin
  InitEmpty(aSrc);
end;

procedure TSoundFrameSize.Assign(aSrc : ISoundFrameSize);
begin
  fFreq       := aSrc.Frequency;
  fChannels   := aSrc.Channels;
  fSampleSize := aSrc.SampleSize;
  fBytes      := aSrc.AsBytes;
  fSamples    := aSrc.AsSamples;
end;

function TSoundFrameSize.Duplicate : ISoundFrameSize;
begin
  Result := TOGLSound.NewFrame(Self as ISoundFrameSize);
end;

function TSoundFrameSize.EmptyDuplicate : ISoundFrameSize;
begin
  Result := TOGLSound.NewEmptyFrame(Self as ISoundFrameSize);
end;

function TSoundFrameSize.Equal(aSecond : ISoundFrameSize) : Boolean;
begin
  if aSecond.IsErrored or IsErrored then Exit(false);

  Result := fBytes = aSecond.AsBytes;
end;

function TSoundFrameSize.Less(aSecond : ISoundFrameSize) : Boolean;
begin
  if aSecond.IsErrored or IsErrored then Exit(false);

  Result := fBytes < aSecond.AsBytes;
end;

function TSoundFrameSize.Greater(aSecond : ISoundFrameSize) : Boolean;
begin
  if aSecond.IsErrored or IsErrored then Exit(false);

  Result := fBytes > aSecond.AsBytes;
end;

function TSoundFrameSize.LessOrEqual(aSecond : ISoundFrameSize) : Boolean;
begin
  if aSecond.IsErrored or IsErrored then Exit(false);

  Result := fBytes <= aSecond.AsBytes;
end;

function TSoundFrameSize.GreaterOrEqual(aSecond : ISoundFrameSize) : Boolean;
begin
  if aSecond.IsErrored or IsErrored then Exit(false);

  Result := fBytes >= aSecond.AsBytes;
end;

function TSoundFrameSize.Minus(aSecond : ISoundFrameSize) : ISoundFrameSize;
begin
  if aSecond.IsErrored or IsErrored then begin
    Result := TOGLSound.NewErrorFrame;
    Exit;
  end;

  Result := TOGLSound.NewFrame(Self);
  Result.Dec(aSecond);
end;

function TSoundFrameSize.Plus(aSecond : ISoundFrameSize) : ISoundFrameSize;
begin
  if aSecond.IsErrored or IsErrored then begin
    Result := TOGLSound.NewErrorFrame;
    Exit;
  end;

  Result := TOGLSound.NewFrame(Self);
  Result.Inc(aSecond);
end;

procedure TSoundFrameSize.Clear;
begin
  fBytes := 0;
  fSamples := 0;
end;

procedure TSoundFrameSize.Inc(aInc : ISoundFrameSize);
begin
  if aInc.IsErrored or IsErrored then Exit;

  if (aInc.Frequency = fFreq) or
     (aInc.Channels = fChannels) or
     (aInc.SampleSize = fSampleSize) then
  begin
    System.Inc(fBytes, aInc.AsBytes);
    System.Inc(fSamples, aInc.AsSamples);
  end;
end;

procedure TSoundFrameSize.Dec(aDec : ISoundFrameSize);
begin
  if aDec.IsErrored or IsErrored then Exit;

  if (aDec.Frequency = fFreq) or
     (aDec.Channels = fChannels) or
     (aDec.SampleSize = fSampleSize) then
  begin
    System.Dec(fBytes, aDec.AsBytes);
    System.Dec(fSamples, aDec.AsSamples);
  end;
end;

procedure TSoundFrameSize.IncSamples(aInc : Cardinal);
begin
  System.Inc(fSamples, aInc);
  RecalcBytes;
end;

procedure TSoundFrameSize.IncBytes(aInc : Cardinal);
begin
  System.Inc(fBytes, aInc);
  RecalcSamples;
end;

procedure TSoundFrameSize.DecSamples(aDec : Cardinal);
begin
  System.Dec(fSamples, aDec);
  RecalcBytes;
end;

procedure TSoundFrameSize.DecBytes(aDec : Cardinal);
begin
  System.Dec(fBytes, aDec);
  RecalcSamples;
end;

function TSoundFrameSize.AsBytes : Cardinal;
begin
  Result := fBytes;
end;

function TSoundFrameSize.AsSamples : Cardinal;
begin
  Result := fSamples;
end;

function TSoundFrameSize.AsDurationMs : Single;
begin
  Result := Single( fSamples ) / Single(fFreq) * 1000.0;
end;

function TSoundFrameSize.AsDurationSec : Single;
begin
  Result := Single( fSamples ) / Single(fFreq);
end;

function TSoundFrameSize.IsValid : Boolean;
begin
  Result := not IsEmptyOrErrored;
end;

function TSoundFrameSize.IsEmpty : Boolean;
begin
  Result := fSamples = 0;
end;

function TSoundFrameSize.IsErrored : Boolean;
begin
  Result := (fFreq = 0) or (fChannels = 0);
end;

function TSoundFrameSize.IsEmptyOrErrored : Boolean;
begin
  Result := IsEmpty or IsErrored;
end;

end.

