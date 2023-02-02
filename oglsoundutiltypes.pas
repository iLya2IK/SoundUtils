{
  OGLSoundUtilTypes - part of SoundUtils_iLya2IK:
   Utilities and abstract classes for working with sound, audio samples,
   audio streams, files, decoders and encoders.

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit OGLSoundUtilTypes;

{$mode objfpc}{$H+}

interface

type
  TSoundDataEndian = (sdeLE, sdeBE);
  TSoundSampleSize = (ss8bit, ss16bit, ss32bit, ssFloat);
  TSoundEncDecType = (edtEncoder, edtDecoder);
  TSoundEncoderMode = (oemCBR, oemVBR);
  TSoundDataLimit  = (sdpForceNotSeekable, sdpReadOnly, sdpWriteOnly);
  TSoundDataLimits = set of TSoundDataLimit;

  { ISoundComment }

  ISoundComment = interface(IUnknown)
  ['{1E4393C6-9EB6-4A89-99E3-93F1D3672C4C}']
  function Ref : Pointer;

  procedure Init;
  procedure Done;

  function GetVendor : String;
  procedure SetVendor(const S : String);

  procedure Add(const comment: String);
  procedure AddTag(const tag, value: String);
  function TagsCount : Integer;
  function GetTag(index : integer) : String;
  function Query(const tag: String; index: integer): String;
  function QueryCount(const tag: String): integer;

  property Vendor : String read GetVendor write SetVendor;
  end;

const COMMENT_ARTIST = 'ARTIST';
      COMMENT_ALBUM  = 'ALBUM';
      COMMENT_TITLE  = 'TITLE';
      COMMENT_GENRE  = 'GENRE';
      COMMENT_TRACK  = 'TRACKNUMBER';

implementation

end.

