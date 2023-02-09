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
  Classes, SysUtils, Variants, OGLFastVariantHash, OGLFastList,
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

  ISoundDataStream = interface
  ['{DD96846C-DF94-4CB1-B578-6C6081D4D8F8}']
  //method to write encoded data
  function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer;
  //method to read encoded data from stream
  function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer;
  //method to seek in encoded/decoded stream
  function DoSeek(_offset:Int64; _whence:Integer): Integer;
  //method to tell current position in encoded/decoded stream
  function DoTell:Int64;
  //method to get size of data
  function Size : Int64;
  //is reader/writer support seeking
  function Seekable : Boolean;
  //is writer support reading
  function Readable : Boolean;
  //is reader support writing
  function Writeable : Boolean; virtual;
  //is end of the stream has been reached
  function EoS : Boolean;
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

  procedure InitStream(aSrc : ISoundDataStream);
  function DataStream : ISoundDataStream;

  procedure Done;

  function Comments : ISoundComment;
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

  //method to encode raw pcm data
  function  WriteData(Buffer : Pointer;
                 Count : ISoundFrameSize;
                 Par : Pointer) : ISoundFrameSize;
  function  WriteDataToStream(Stream : TStream;
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

  //method to read decoded data
  function  ReadData(Buffer : Pointer;
                     Count : ISoundFrameSize;
                     Par : Pointer) : ISoundFrameSize;
  function  ReadDataToStream(Stream : TStream;
                     Count : ISoundFrameSize;
                     Par : Pointer) : ISoundFrameSize;
  //method to reset decoder
  procedure ResetToStart;
  //method to seek a byte offset
  procedure RawSeek(pos : Int64);
  //method to seek a specified PCM offset
  procedure SampleSeek(pos : Integer);
  //method to seek a specified time offset in seconds
  procedure TimeSeek(pos : Double);
  //method to tell a byte offset
  function RawTell : Int64; virtual;
  //method to seek a specified PCM offset
  function SampleTell : Integer; virtual;
  //method to seek a specified time offset in seconds
  function TimeTell : Double; virtual;
  //method to tell total byte size
  function RawTotal : Int64;
  //method to tell total samples count
  function SampleTotal : Integer;
  //method to tell total time length in seconds
  function TimeTotal : Double;
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
  private
    fDataStream : ISoundDataStream;
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

    procedure InitStream(aSrc : ISoundDataStream); virtual;
    function DataStream : ISoundDataStream; virtual;

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
  protected
    function GetMode : TSoundEncoderMode; virtual; abstract;
    function GetQuality : Single; virtual; abstract;
    procedure SetMode({%H-}AValue : TSoundEncoderMode); virtual;
    procedure SetQuality({%H-}AValue : Single); virtual;

    procedure Init({%H-}aProps : ISoundEncoderProps;
                   {%H-}aComment : ISoundComment); virtual; abstract;
  public
    function InternalType : TSoundEncDecType; override;

    //method to encode raw pcm data
    function  WriteData({%H-}Buffer : Pointer;
                   {%H-}Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; virtual;
    function  WriteDataToStream({%H-}Stream : TStream;
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
  protected
    procedure Init; virtual; abstract;
  public
    //method to read decoded data
    function  ReadData({%H-}Buffer : Pointer;
                   {%H-}Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; virtual;
    function  ReadDataToStream({%H-}Stream : TStream;
                   {%H-}Count : ISoundFrameSize;
                   {%H-}Par : Pointer) : ISoundFrameSize; virtual;
    //method to reset decoder
    procedure ResetToStart; virtual;
    //methods to seek position
    //method to seek a byte offset
    procedure RawSeek(pos : Int64); virtual;
    //method to seek a specified PCM offset
    procedure SampleSeek(pos : Integer); virtual;
    //method to seek a specified time offset in seconds
    procedure TimeSeek(pos : Double); virtual;
    //methods to tell current pos
    //method to tell a byte offset
    function RawTell : Int64; virtual;
    //method to seek a specified PCM offset
    function SampleTell : Integer; virtual;
    //method to seek a specified time offset in seconds
    function TimeTell : Double; virtual;
    //method to tell total byte size
    function RawTotal : Int64; virtual;
    //method to tell total samples count
    function SampleTotal : Integer; virtual;
    //method to tell total time length in seconds
    function TimeTotal : Double; virtual;

    function InternalType : TSoundEncDecType; override;
  end;

  { TSoundDataStream }

  TSoundDataStream = class(TInterfacedObject, ISoundDataStream)
  private
    fStream : TStream;
  protected
    procedure SetStream(aStream : TStream); virtual;
    function GetStream : TStream; virtual;
  public
    constructor Create(aStream : TStream);
    //method to write encoded data
    function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer; virtual;
    //method to read encoded data from stream
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; virtual;
    //method to seek in encoded stream
    function DoSeek(_offset:Int64; _whence:Integer): Integer; virtual;
    //method to tell current position in encoded stream
    function DoTell:Int64; virtual;
    //method to get size of data
    function Size : Int64; virtual;
    //is reader/writer support seeking
    function Seekable : Boolean; virtual;
    //is writer support reading
    function Readable : Boolean; virtual;
    //is reader support writing
    function Writeable : Boolean; virtual;
    //is end of the stream has been reached
    function EoS : Boolean; virtual;

    property Stream : TStream read GetStream write SetStream;
  end;

  { TSoundDataStreamForward }

  TSoundDataStreamForward = class(TSoundDataStream)
  public
    function DoSeek(_offset:Int64; _whence:Integer): Integer; override;
    function DoTell:Int64; override;
    function Size : Int64; override;
    function Seekable : Boolean; override;
  end;

  { TSoundDataStreamWriteOnly }

  TSoundDataStreamWriteOnly = class(TSoundDataStream)
  public
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; override;
    function Readable : Boolean; override;
  end;

  { TSoundDataStreamReadOnly }

  TSoundDataStreamReadOnly = class(TSoundDataStream)
  public
    function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer; override;
    function Writeable : Boolean; override;
  end;

  { TSoundDataStreamWriteOnlyForward }

  TSoundDataStreamWriteOnlyForward = class(TSoundDataStreamForward)
  public
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; override;
    function Readable : Boolean; override;
  end;

  { TSoundDataStreamReadOnlyForward }

  TSoundDataStreamReadOnlyForward = class(TSoundDataStreamForward)
  public
    function DoWrite(Buffer : Pointer; BufferSize : Integer) : Integer; override;
    function Writeable : Boolean; override;
  end;

  { TSoundFile }

  TSoundFile = class
  private
    fStream: TStream;
    fDataLimits : TSoundDataLimits;

    //encoder/decoder spec
    fEncDec : ISoundEncDec;
  protected
    procedure Clean; virtual;
    procedure WriteHeader; virtual;
    function InitEncoder(aProps : ISoundEncoderProps;
                   aComments : ISoundComment) : ISoundEncoder; virtual;
                                                                abstract;
    function InitDecoder : ISoundDecoder; virtual; abstract;

    class function DefaultEncoderDataLimits : TSoundDataLimits; virtual;
    class function DefaultDecoderDataLimits : TSoundDataLimits; virtual;
  public
    destructor Destroy; override;

    function Stream : TStream; virtual;
    function DetachStream : TStream; virtual;
    function DataLimits : TSoundDataLimits; virtual;

    function LoadFromFile(const aFileName : String; const aInMemory : Boolean
      ) : Boolean; virtual;
    function LoadFromStream(Str : TStream; aDataLimits : TSoundDataLimits) : Boolean; virtual;
    function LoadFromStream(Str : TStream) : Boolean;
    function ReadData(Buffer : Pointer;
                      aFrameSize : ISoundFrameSize;
                      Ptr : Pointer) : ISoundFrameSize; virtual;
    procedure ResetToStart; virtual;
    function Decoder : ISoundDecoder;
    function DecoderReady : Boolean; virtual;

    function SaveToFile(const aFileName : String;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function SaveToStream(Str : TStream; aDataLimits : TSoundDataLimits;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean; virtual;
    function SaveToStream(Str : TStream;
      aProps : ISoundEncoderProps;
      aComments : ISoundComment) : Boolean;
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

  { TVorbisTag }

  TVorbisTag = class(TStringList)
  private
    FTag : String;
  public
    constructor Create(const aTag : String); overload;
    constructor Create(aTag : TVorbisTag); overload;

    procedure AssignTo(aTag : TPersistent); override;
    procedure AddValues(aTag : TVorbisTag);

    property Tag : String read FTag;
  end;

  { TVorbisTags }

  TVorbisTags = class(specialize TFastBaseCollection<TVorbisTag>)
  private
    FVendor : String;
  public
    constructor Create(aTags : TVorbisTags); overload;
    constructor CreateFromInterface(aTags : ISoundComment);

    procedure Assign(aTags : TVorbisTags);
    procedure AssignInterface(aTags : ISoundComment);

    procedure AddComment(const comment: String);
    procedure AddTag(const atag, avalue: String);
    function Tag(const aTag : String) : TVorbisTag;
    function CountAll : Integer;


    property Vendor : String read FVendor write FVendor;
  end;

  { TSoundCommentCloneable }

  TSoundCommentCloneable = class(TInterfacedObject, ISoundComment)
  protected
    procedure Init; virtual; abstract;
    procedure Done; virtual; abstract;
    function GetVendor : String; virtual; abstract;
    procedure SetVendor(const S : String); virtual; abstract;
  public
    function Ref : Pointer; virtual; abstract;

    constructor Create;
    constructor CreateFromInterface(aSrc : ISoundComment);
    procedure Clone(aSrc : ISoundComment); virtual;

    procedure Add(const comment: String); virtual; abstract;
    procedure AddTag(const tag, value: String); virtual; abstract;
    function TagsCount : Integer; virtual; abstract;
    function GetTag(index : integer) : String; virtual; abstract;
    function Query(const tag: String; index: integer): String; virtual; abstract;
    function QueryCount(const tag: String): integer; virtual; abstract;
  end;

  { TNativeVorbisCommentCloneable }

  TNativeVorbisCommentCloneable = class(TSoundCommentCloneable)
  private
    fCachedTags : TStringList;
  protected
    procedure SetNativeVendor(v : PChar); virtual; abstract;
    function GetNativeVendor : PChar; virtual; abstract;
    function GetNativeComment(index : integer) : PChar; virtual; abstract;
    function GetNativeCommentLength(index : integer) : Int32; virtual; abstract;
    function GetNativeCommentCount : Int32; virtual; abstract;

    function GetVendor : String; override;
    procedure SetVendor(const S : String); override;
  public
    destructor Destroy; override;

    procedure Add(const comment: String); override;
    procedure AddTag(const tag, value: String); override;
    function TagsCount : Integer; override;
    function GetTag(index : integer) : String; override;
  end;

  { TVorbisComment }

  TVorbisComment = class(TSoundCommentCloneable)
  private
    fRef : TVorbisTags;
  protected
    procedure Init; override;
    procedure Done; override;
    function GetVendor : String; override;
    procedure SetVendor(const S : String); override;
  public
    function Ref : Pointer; override;

    constructor Create;
    destructor Destroy; override;

    procedure Clone(aSrc : ISoundComment); override;

    procedure Add(const comment: String); override;
    procedure AddTag(const tag, value: String); override;
    function TagsCount : Integer; override;
    function GetTag(index : integer) : String; override;
    function Query(const tag: String; index: integer): String; override;
    function QueryCount(const tag: String): integer; override;
  end;

  EOGLSound = class(Exception);

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

    class function NewDataStream(aStream : TStream; aDataLimits : TSoundDataLimits) : ISoundDataStream;

    class function BitdepthToSampleSize(bitdepth : Byte) : TSoundSampleSize;
    class function SampleSizeToBitdepth(samplsz : TSoundSampleSize) : Byte;
    class function SampleSizeToBytedepth(samplsz : TSoundSampleSize) : Byte;

    class function NewErrorFrame : ISoundFrameSize;

    class function NewVorbisComment : ISoundComment;
    class function NewVorbisComment(aSrc : ISoundComment): ISoundComment;
    class function SplitComment(const comment : String; out aTag, aValue : String) : Boolean;
    class function GetCommentTag(const comment : String; out aTag : String) : Boolean;

    class function EncProps(const Vals : Array of Variant) : ISoundEncoderProps;

    // Encoder properties
    const PROP_MODE : Cardinal         = $001;
    const PROP_CHANNELS : Cardinal     = $002;
    const PROP_FREQUENCY : Cardinal    = $003;
    const PROP_BITRATE : Cardinal      = $004;
    const PROP_SAMPLE_SIZE : Cardinal  = $005;
    const PROP_QUALITY : Cardinal      = $006;
  end;


implementation

const
  esSeekingNotSupported : String = 'Seeking is not supported for this data source';
  esRawSeekingNotImplemented : String = 'Raw seeking is not implemented by decoder';
  esPCMSeekingNotImplemented : String = 'PCM seeking is not implemented by decoder';
  esTimeSeekingNotImplemented : String = 'Time seeking is not implemented by decoder';

{ TNativeVorbisCommentCloneable }

function TNativeVorbisCommentCloneable.GetVendor : String;
begin
  Result := StrPas( GetNativeVendor );
end;

procedure TNativeVorbisCommentCloneable.SetVendor(const S : String);
var  L : Integer;
begin
  L := Length(S);
  SetNativeVendor(Getmem(L + 1));
  if L > 0 then
  begin
    Move(S[1], GetNativeVendor^, L);
  end;
  GetNativeVendor()[L] := #0;
end;

destructor TNativeVorbisCommentCloneable.Destroy;
begin
  if assigned(fCachedTags) then
    FreeAndNil(fCachedTags);
  inherited Destroy;
end;

procedure TNativeVorbisCommentCloneable.Add(const comment : String);
var
  aTag : String;
begin
  if Assigned(fCachedTags) and
     TOGLSound.GetCommentTag(comment, aTag) then
    fCachedTags.Add(aTag);
end;

procedure TNativeVorbisCommentCloneable.AddTag(const tag, value : String);
begin
  if Assigned(fCachedTags) then
    fCachedTags.Add(tag);
end;

function TNativeVorbisCommentCloneable.TagsCount : Integer;
var
  i : integer;
  aTag : String;
begin
  if not Assigned(fCachedTags) then
  begin
    fCachedTags := TStringList.Create;
    fCachedTags.Duplicates := dupIgnore;
    for i := 0 to GetNativeCommentCount-1 do
    begin
      if TOGLSound.GetCommentTag(StrPas(GetNativeComment(i)), aTag) then
        fCachedTags.Add(aTag);
    end;
  end;
  Result := fCachedTags.Count;
end;

function TNativeVorbisCommentCloneable.GetTag(index : integer) : String;
begin
  if (index < 0) and (index >= TagsCount) then
  begin
    Result := '';
    Exit;
  end;
  Result := fCachedTags[index];
end;

{ TSoundCommentCloneable }

constructor TSoundCommentCloneable.Create;
begin
  Init;
end;

constructor TSoundCommentCloneable.CreateFromInterface(aSrc : ISoundComment);
begin
  Init;
  Clone(aSrc);
end;

procedure TSoundCommentCloneable.Clone(aSrc : ISoundComment);
var
  i, j, c : integer;
  tn : String;
begin
  if TagsCount > 0 then
  begin
    Done;
    Init;
  end;
  for i := 0 to aSrc.TagsCount-1 do
  begin
    c := aSrc.QueryCount(tn);
    for j := 0 to c-1 do
    begin
      AddTag(tn, aSrc.Query(tn, j));
    end;
  end;

  SetVendor(aSrc.Vendor);
end;

{ TVorbisComment }

function TVorbisComment.Ref : Pointer;
begin
  Result := Pointer(fRef);
end;

constructor TVorbisComment.Create;
begin
  Init;
end;

procedure TVorbisComment.Clone(aSrc : ISoundComment);
begin
  if assigned(fRef) then fRef.Free;
  fRef := TVorbisTags.CreateFromInterface(aSrc);
end;

destructor TVorbisComment.Destroy;
begin
  Done;
  inherited Destroy;
end;

procedure TVorbisComment.Init;
begin
  fRef := TVorbisTags.Create;
end;

procedure TVorbisComment.Done;
begin
  fRef.Free;
end;

function TVorbisComment.GetVendor : String;
begin
  Result := fRef.Vendor;
end;

procedure TVorbisComment.SetVendor(const S : String);
begin
  fRef.Vendor := S;
end;

procedure TVorbisComment.Add(const comment : String);
begin
  fRef.AddComment(comment);
end;

procedure TVorbisComment.AddTag(const tag, value : String);
begin
  fRef.AddTag(tag, value);
end;

function TVorbisComment.TagsCount : Integer;
begin
  Result := fRef.Count;
end;

function TVorbisComment.GetTag(index : integer) : String;
begin
  Result := fRef[index].Tag;
end;

function TVorbisComment.Query(const tag : String; index : integer) : String;
var
  t : TVorbisTag;
begin
  if index < 0 then
    Result := ''
  else
  begin
    t := fRef.Tag(tag);
    if Assigned(t) then
    begin
      if t.Count > index then
      begin
        Result := t[index];
      end else
        Result := '';
    end else
    begin
      Result := '';
    end;
  end;
end;

function TVorbisComment.QueryCount(const tag : String) : integer;
var
  t : TVorbisTag;
begin
  t := fRef.Tag(tag);
  if Assigned(t) then
  begin
    Result := t.Count;
  end else
  begin
    Result := 0;
  end;
end;

{ TVorbisTags }

constructor TVorbisTags.Create(aTags : TVorbisTags);
begin
  inherited Create;
  Assign(aTags);
end;

constructor TVorbisTags.CreateFromInterface(aTags : ISoundComment);
begin
  inherited Create;
  if aTags is TVorbisTags then
  begin
    Assign(aTags as TVorbisTags);
  end else
  begin
    AssignInterface(aTags);
  end;
end;

procedure TVorbisTags.Assign(aTags : TVorbisTags);
var
  i : integer;
  t : TVorbisTag;
begin
  Clear;
  for i := 0 to aTags.Count-1 do
  begin
    t := Tag(aTags[i].Tag);
    if assigned(t) then
      t.AddValues(aTags[i])
    else
    begin
      t := TVorbisTag.Create(aTags[i]);
      Add(t);
    end;
  end;

  FVendor := aTags.Vendor;
end;

procedure TVorbisTags.AssignInterface(aTags : ISoundComment);
var
  i, j, c : integer;
  tn : String;
  t : TVorbisTag;
begin
  Clear;
  for i := 0 to aTags.TagsCount-1 do
  begin
    tn := aTags.GetTag(i);
    t := Tag(tn);
    if not assigned(t) then
    begin
      t := TVorbisTag.Create(tn);
      Add(t);
    end;
    c := aTags.QueryCount(tn);
    for j := 0 to c-1 do
    begin
      t.Add(aTags.Query(tn, j));
    end;
  end;

  FVendor := aTags.Vendor;
end;

procedure TVorbisTags.AddComment(const comment : String);
var
  aTag, aValue : String;
begin
  if TOGLSound.SplitComment(comment, aTag, aValue) then
  begin
    AddTag(aTag, aValue);
  end;
end;

procedure TVorbisTags.AddTag(const atag, avalue : String);
var
  t : TVorbisTag;
begin
  t := Tag(atag);
  if not Assigned(t) then
  begin
    t := TVorbisTag.Create(atag);
    Add(t);
  end;
  t.Add(avalue);
end;

function TVorbisTags.Tag(const aTag : String) : TVorbisTag;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if SameText(Self[i].Tag, aTag) then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TVorbisTags.CountAll : Integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
  begin
    Inc(Result, Self[i].Count);
  end;
end;

{ TVorbisTag }

constructor TVorbisTag.Create(const aTag : String);
begin
  inherited Create;
  FTag := aTag;
end;

constructor TVorbisTag.Create(aTag : TVorbisTag);
begin
  inherited Create;
  AssignTo(aTag);
end;

procedure TVorbisTag.AssignTo(aTag : TPersistent);
begin
  inherited AssignTo(aTag);
  if aTag is TVorbisTag then
    FTag := TVorbisTag(aTag).Tag;
end;

procedure TVorbisTag.AddValues(aTag : TVorbisTag);
begin
  AddStrings(aTag);
end;

{ TSoundDataStreamReadOnlyForward }

function TSoundDataStreamReadOnlyForward.DoWrite(Buffer : Pointer;
  BufferSize : Integer) : Integer;
begin
  Result := -1;
end;

function TSoundDataStreamReadOnlyForward.Writeable : Boolean;
begin
  Result := False;
end;

{ TSoundDataStreamWriteOnlyForward }

function TSoundDataStreamWriteOnlyForward.DoRead(_ptr : Pointer;
  _nbytes : Integer) : Integer;
begin
  Result := -1;
end;

function TSoundDataStreamWriteOnlyForward.Readable : Boolean;
begin
  Result := False;
end;

{ TSoundDataStreamReadOnly }

function TSoundDataStreamReadOnly.DoWrite(Buffer : Pointer; BufferSize : Integer
  ) : Integer;
begin
  Result := -1;
end;

function TSoundDataStreamReadOnly.Writeable : Boolean;
begin
  Result := False;
end;

{ TSoundDataStreamWriteOnly }

function TSoundDataStreamWriteOnly.DoRead(_ptr : Pointer; _nbytes : Integer
  ) : Integer;
begin
  Result := -1;
end;

function TSoundDataStreamWriteOnly.Readable : Boolean;
begin
  Result := false;
end;

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

{ TSoundDataStream }

procedure TSoundDataStream.SetStream(aStream : TStream);
begin
  fStream := aStream;
end;

function TSoundDataStream.GetStream : TStream;
begin
  Result := fStream;
end;

constructor TSoundDataStream.Create(aStream : TStream);
begin
  fStream := aStream;
end;

function TSoundDataStream.DoWrite(Buffer : Pointer; BufferSize : Integer
  ) : Integer;
begin
  if (BufferSize < 0) then Exit(-1);
  if not (Assigned(Buffer) and (BufferSize > 0)) then Exit(-1);
  fStream.Write(Buffer^, BufferSize);
  Result := 0;
end;

function TSoundDataStream.DoRead(_ptr : Pointer; _nbytes : Integer
  ) : Integer;
begin
  if (_nbytes <= 0) then Exit(0);
  try
    Result := Int64(fStream.Read(_ptr^, _nbytes));
  except
    Result := 0;
  end;
end;

function TSoundDataStream.DoSeek(_offset : Int64; _whence : Integer
  ) : Integer;
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

function TSoundDataStream.DoTell : Int64;
begin
  try
    result := fStream.Position;
  except
    result := -1;
  end;
end;

function TSoundDataStream.Size : Int64;
begin
  try
    result := fStream.Size;
  except
    result := -1;
  end;
end;

function TSoundDataStream.Seekable : Boolean;
begin
  Result := True;
end;

function TSoundDataStream.Readable : Boolean;
begin
  Result := True;
end;

function TSoundDataStream.Writeable : Boolean;
begin
  Result := True;
end;

function TSoundDataStream.EoS : Boolean;
begin
  if Seekable then
    Result := fStream.Position >= fStream.Size
  else
    Result := false;
end;

{ TSoundDataStreamForward }

function TSoundDataStreamForward.DoSeek(_offset : Int64; _whence : Integer
  ) : Integer;
begin
  Result := -1;
end;

function TSoundDataStreamForward.DoTell : Int64;
begin
  Result := -1;
end;

function TSoundDataStreamForward.Size : Int64;
begin
  Result := -1;
end;

function TSoundDataStreamForward.Seekable : Boolean;
begin
  Result := False;
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

class function TSoundFile.DefaultEncoderDataLimits : TSoundDataLimits;
begin
  Result := [sdpWriteOnly];
end;

class function TSoundFile.DefaultDecoderDataLimits : TSoundDataLimits;
begin
  Result := [sdpReadOnly];
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

function TSoundFile.DetachStream : TStream;
begin
  Result := fStream;
  fStream := nil;
end;

function TSoundFile.DataLimits : TSoundDataLimits;
begin
  Result := fDataLimits;
end;

function TSoundFile.LoadFromFile(const aFileName : String;
  const aInMemory : Boolean) : Boolean;
var
  cFilestream : TFileStream;
  cStr : TStream;
  cMode : Cardinal;
begin
  if sdpReadOnly in DefaultEncoderDataLimits then
    cMode := fmOpenRead else
    cMode := fmOpenReadWrite;
  if aInMemory then
  begin
    cFilestream := TFileStream.Create(aFileName, cMode);
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
    cStr := TFileStream.Create(aFileName, cMode);

  Result := LoadFromStream(cStr, DefaultDecoderDataLimits);
end;

function TSoundFile.LoadFromStream(Str : TStream; aDataLimits : TSoundDataLimits
  ) : Boolean;
begin
  Clean;

  fStream := Str;
  fDataLimits := aDataLimits;

  try
    fEncDec :=  InitDecoder;
    Result := fEncDec.Ready;
  except
    on e : Exception do Result := false;
  end;
end;

function TSoundFile.LoadFromStream(Str : TStream) : Boolean;
begin
  Result := LoadFromStream(Str, DefaultDecoderDataLimits);
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
  cMode : Cardinal;
begin
  if sdpWriteOnly in DefaultEncoderDataLimits then
    cMode := fmOpenWrite else
    cMode := fmOpenReadWrite;
  Str := TFileStream.Create(aFileName, cMode or fmCreate);
  if Assigned(Str) then
    Result := SaveToStream(Str, DefaultEncoderDataLimits, aProps, aComments) else
    Result := false;
end;

function TSoundFile.SaveToStream(Str : TStream; aDataLimits : TSoundDataLimits;
  aProps : ISoundEncoderProps; aComments : ISoundComment) : Boolean;
begin
  Clean;

  fStream := Str;
  fDataLimits := aDataLimits;

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

function TSoundFile.SaveToStream(Str : TStream; aProps : ISoundEncoderProps;
  aComments : ISoundComment) : Boolean;
begin
  Result := SaveToStream(Str, DefaultEncoderDataLimits, aProps, aComments);
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


function TSoundAbstractDecoder.ReadData(Buffer : Pointer;
  Count : ISoundFrameSize; Par : Pointer) : ISoundFrameSize;
begin
  Result := TOGLSound.NewErrorFrame;
end;

function TSoundAbstractDecoder.ReadDataToStream(Stream : TStream;
  Count : ISoundFrameSize; Par : Pointer) : ISoundFrameSize;
begin
  Result := TOGLSound.NewErrorFrame;
end;

procedure TSoundAbstractDecoder.ResetToStart;
begin
  // do nothing
end;

procedure TSoundAbstractDecoder.RawSeek(pos : Int64);
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esRawSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

procedure TSoundAbstractDecoder.SampleSeek(pos : Integer);
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esPCMSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

procedure TSoundAbstractDecoder.TimeSeek(pos : Double);
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esTimeSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

function TSoundAbstractDecoder.RawTell : Int64;
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esRawSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

function TSoundAbstractDecoder.SampleTell : Integer;
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esPCMSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

function TSoundAbstractDecoder.TimeTell : Double;
begin
  if DataStream.Seekable then
    raise EOGLSound.Create(esTimeSeekingNotImplemented) else
    raise EOGLSound.Create(esSeekingNotSupported);
end;

function TSoundAbstractDecoder.RawTotal : Int64;
begin
  Result := 0;
end;

function TSoundAbstractDecoder.SampleTotal : Integer;
begin
  Result := 0;
end;

function TSoundAbstractDecoder.TimeTotal : Double;
begin
  Result := 0.0;
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

function TSoundAbstractEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
begin
  Result := TOGLSound.NewErrorFrame;
end;

function TSoundAbstractEncoder.WriteDataToStream(Stream : TStream;
  Count : ISoundFrameSize; Par : Pointer) : ISoundFrameSize;
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

procedure TSoundAbstractEncDec.InitStream(aSrc : ISoundDataStream);
begin
  fDataStream := aSrc;
end;

function TSoundAbstractEncDec.DataStream : ISoundDataStream;
begin
  Result := fDataStream;
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

class function TOGLSound.NewDataStream(aStream : TStream;
  aDataLimits : TSoundDataLimits) : ISoundDataStream;
var seekable : Boolean;
    {%H-}sz : Int64;
begin
  if sdpForceNotSeekable in aDataLimits then
  begin
    seekable := false;
  end else
  begin
    try
      {$OPTIMIZATION OFF}
      sz := aStream.Size;
      {$OPTIMIZATION DEFAULT}
      seekable := true;
    except
      on e : EStreamError do
      begin
        // stream is not seekable
        seekable := false;
      end;
    end;
  end;

  if seekable then
  begin
    if sdpWriteOnly in aDataLimits then
      Result := TSoundDataStreamWriteOnly.Create(aStream) as ISoundDataStream
    else
    if sdpReadOnly in aDataLimits then
      Result := TSoundDataStreamReadOnly.Create(aStream) as ISoundDataStream
    else
      Result := TSoundDataStream.Create(aStream) as ISoundDataStream;
  end
  else
  begin
    if sdpWriteOnly in aDataLimits then
      Result := TSoundDataStreamWriteOnlyForward.Create(aStream) as ISoundDataStream
    else
    if sdpReadOnly in aDataLimits then
      Result := TSoundDataStreamReadOnlyForward.Create(aStream) as ISoundDataStream
    else
      Result := TSoundDataStreamForward.Create(aStream) as ISoundDataStream;
  end;
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

class function TOGLSound.NewVorbisComment : ISoundComment;
begin
  Result := TVorbisComment.Create as ISoundComment;
end;

class function TOGLSound.NewVorbisComment(aSrc : ISoundComment) : ISoundComment;
begin
  Result := TVorbisComment.CreateFromInterface(aSrc) as ISoundComment;
end;

class function TOGLSound.SplitComment(const comment : String; out aTag,
  aValue : String) : Boolean;
var
  P : Integer;
begin
  P := Pos('=', comment);
  if P > 0 then
  begin
    aTag := Copy(comment, 1, P - 1).Trim;
    aValue := Copy(comment, P + 1, Length(comment)).Trim;
    Result := True;
  end else
    Result := False;
end;

class function TOGLSound.GetCommentTag(const comment : String; out aTag : String
  ) : Boolean;
var
  P : Integer;
begin
  P := Pos('=', comment);
  if P > 0 then
  begin
    aTag := Copy(comment, 1, P - 1).Trim;
    Result := True;
  end else
    Result := False;
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

