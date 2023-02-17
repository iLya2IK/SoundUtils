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

uses sysutils; {ExprComparator;}

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

  PStandardComment = ^TStandardComment;
  TStandardComment = record
   TagID, TagName : String;
  end;

  { TOGLSoundComments }

  TOGLSoundComments = class
  public
  class function Find(const aTagId : String) : Integer;
  class function Get(TagNum : Integer) : PStandardComment;
  class function TagID(TagNum : Integer) : String;
  class function GetByID(const aTagId : String) : PStandardComment;
  class function GetCount : Integer;

  const COMMENT_ALBUM   = 2;
  const COMMENT_ARTIST  = 3;
  const COMMENT_COMMENT = 4;
  const COMMENT_DATE    = 10;
  const COMMENT_ENCODER = 15;
  const COMMENT_GENRE   = 17;
  const COMMENT_TITLE   = 29;
  const COMMENT_TRACK   = 30;
  end;

implementation

resourcestring
  sCommUnknown = 'Unknown';
  sCommActor = 'Actor';
  sCommAlbum = 'Album';
  sCommArtist = 'Artist';
  sCommComment = 'Comment';
  sCommComposer = 'Composer';
  sCommContact = 'Contact';
  sCommCopyright = 'Copyright';
  sCommCoverArt = 'CoverArt';
  sCommCoverArtMIMEType = 'CoverArtMIMEType';
  sCommDate = 'Date';
  sCommDescription = 'Description';
  sCommDirector = 'Director';
  sCommEncodedBy = 'EncodedBy';
  sCommEncodedUsing = 'EncodedUsing';
  sCommEncoder = 'Encoder';
  sCommEncoderOptions = 'EncoderOptions';
  sCommGenre = 'Genre';
  sCommISRCNumber = 'ISRCNumber';
  sCommLicense = 'License';
  sCommLocation = 'Location';
  sCommPicture = 'Picture';
  sCommOrganization = 'Organization';
  sCommPerformer = 'Performer';
  sCommProducer = 'Producer';
  sCommReplayGainAlbumGain = 'ReplayGainAlbumGain';
  sCommReplayGainAlbumPeak = 'ReplayGainAlbumPeak';
  sCommReplayGainTrackGain = 'ReplayGainTrackGain';
  sCommReplayGainTrackPeak = 'ReplayGainTrackPeak';
  sCommTitle = 'Title';
  sCommTrackNumber = 'TrackNumber';
  sCommVersion = 'Version';
  sCommVendor = 'Vendor';

const cStandardSoundComments : Array of TStandardComment =
  ( (TagID:'UNKNOWN'; 	             TagName:sCommUnknown),
    (TagID:'ACTOR'; 	             TagName:sCommActor),
    (TagID:'ALBUM'; 	             TagName:sCommAlbum),
    (TagID:'ARTIST'; 	             TagName:sCommArtist),
    (TagID:'COMMENT'; 	             TagName:sCommComment),
    (TagID:'COMPOSER'; 	             TagName:sCommComposer),
    (TagID:'CONTACT'; 	             TagName:sCommContact),
    (TagID:'COPYRIGHT';              TagName:sCommCopyright),
    (TagID:'COVERART'; 	             TagName:sCommCoverArt),
    (TagID:'COVERARTMIME';           TagName:sCommCoverArtMIMEType),
    (TagID:'DATE';                   TagName:sCommDate),
    (TagID:'DESCRIPTION';            TagName:sCommDescription),
    (TagID:'DIRECTOR';               TagName:sCommDirector),
    (TagID:'ENCODED_BY';             TagName:sCommEncodedBy),
    (TagID:'ENCODED_USING';          TagName:sCommEncodedUsing),
    (TagID:'ENCODER';                TagName:sCommEncoder),
    (TagID:'ENCODER_OPTIONS';        TagName:sCommEncoderOptions),
    (TagID:'GENRE';                  TagName:sCommGenre),
    (TagID:'ISRC';                   TagName:sCommISRCNumber),
    (TagID:'LICENSE';                TagName:sCommLicense),
    (TagID:'LOCATION';               TagName:sCommLocation),
    (TagID:'METADATA_BLOCK_PICTURE'; TagName:sCommPicture),
    (TagID:'ORGANIZATION';           TagName:sCommOrganization),
    (TagID:'PERFORMER';              TagName:sCommPerformer),
    (TagID:'PRODUCER';               TagName:sCommProducer),
    (TagID:'REPLAYGAIN_ALBUM_GAIN';  TagName:sCommReplayGainAlbumGain),
    (TagID:'REPLAYGAIN_ALBUM_PEAK';  TagName:sCommReplayGainAlbumPeak),
    (TagID:'REPLAYGAIN_TRACK_GAIN';  TagName:sCommReplayGainTrackGain),
    (TagID:'REPLAYGAIN_TRACK_PEAK';  TagName:sCommReplayGainTrackPeak),
    (TagID:'TITLE';                  TagName:sCommTitle),
    (TagID:'TRACKNUMBER';            TagName:sCommTrackNumber),
    (TagID:'VERSION';                TagName:sCommVersion),
    (TagID:'VENDOR';                 TagName:sCommVendor));

{ TOGLSoundComments }

class function TOGLSoundComments.Find(const aTagId : String) : Integer;
var
  ucTagID : String;
  i : integer;
begin
  ucTagID := UpperCase(aTagId);
  for i := 1 to High(cStandardSoundComments) do
  begin
    if SameStr(ucTagId, cStandardSoundComments[i].TagID) then
    begin
      Exit(i);
    end;
  end;
  Result := -1;
end;

class function TOGLSoundComments.Get(TagNum : Integer) : PStandardComment;
begin
  if (TagNum >= 0) and (TagNum < GetCount) then
  begin
    Result := @cStandardSoundComments[TagNum];
  end else
    Result := @cStandardSoundComments[0];
end;

class function TOGLSoundComments.TagID(TagNum : Integer) : String;
begin
  Result := Get(TagNum)^.TagID;
end;

class function TOGLSoundComments.GetByID(const aTagId : String
  ) : PStandardComment;
begin
  Result := Get(Find(aTagId));
end;

class function TOGLSoundComments.GetCount : Integer;
begin
  Result := Length(cStandardSoundComments);
end;

end.

