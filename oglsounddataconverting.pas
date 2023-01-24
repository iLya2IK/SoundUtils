{
  OGLSoundDataConverting - part of SoundUtils_iLya2IK:
   Utilities and abstract classes for working with sound, audio samples,
   audio streams, files, decoders and encoders.

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLSoundDataConverting;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, OGLSoundUtilTypes;

procedure UninterleaveSamples(const src : Pointer; dst : PPointer;
                                  src_sample_size : TSoundSampleSize;
                                  dst_sample_size : TSoundSampleSize;
                                  normalized : Boolean;
                                  channels, samples : Integer);

procedure InterleaveSamples(const src : PPointer; dst : Pointer;
                                  src_sample_size : TSoundSampleSize;
                                  dst_sample_size : TSoundSampleSize;
                                  normalized : Boolean;
                                  channels, samples : Integer);

procedure UninterleaveI32ToFloat(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI16ToFloat(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI8ToFloat(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI32ToI32(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI32ToI16(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI32ToI8(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI32ToI16(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI32ToI8(const src : PInt32; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI16ToI16(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI16ToI8(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI16ToI8(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI8ToI8(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI8ToI16(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI8ToI16(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI8ToI32(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI8ToI32(const src : PInt8; dst : PPointer; channels, samples : Integer);
procedure UninterleaveI16ToI32(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveNormI16ToI32(const src : PInt16; dst : PPointer; channels, samples : Integer);
procedure UninterleaveFloatToFloat(const src : PSingle; dst : PPointer; channels, samples : Integer);
procedure UninterleaveFloatToI8(const src : PSingle; dst : PPointer; channels, samples : Integer);
procedure UninterleaveFloatToI16(const src : PSingle; dst : PPointer; channels, samples : Integer);
procedure UninterleaveFloatToI32(const src : PSingle; dst : PPointer; channels, samples : Integer);

procedure InterleaveI32ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
procedure InterleaveI16ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
procedure InterleaveI8ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
procedure InterleaveI32ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
procedure InterleaveI32ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveI32ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveNormI32ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveNormI32ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveI16ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveI16ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveNormI16ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveI8ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveI8ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveNormI8ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveI8ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
procedure InterleaveNormI8ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
procedure InterleaveI16ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
procedure InterleaveNormI16ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
procedure InterleaveFloatToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
procedure InterleaveFloatToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
procedure InterleaveFloatToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
procedure InterleaveFloatToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);


implementation

{$INLINE ON}

function FloatRoundI8(v : Single) : Int8; inline;
begin
  FloatRoundI8 := Int8(Round(v));
end;

function FloatRoundI16(v : Single) : Int16; inline;
begin
  FloatRoundI16 := Int16(Round(v));
end;

function FloatRoundI32(v : Double) : Int32; inline;
begin
  FloatRoundI32 := Int32(Round(v));
end;

// uninterleave functions

procedure UninterleaveI32ToFloat(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PSingle(dst[0])[i] := Double(src[i]) / Double(2147483647.0);
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PSingle(dst[0])[i] := Double(src[p]) / Double(2147483647.0);
        Inc(p);
        PSingle(dst[1])[i] := Double(src[p]) / Double(2147483647.0);
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PSingle(dst[j])[i] := Double(src[p]) / Double(2147483647.0);
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI16ToFloat(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PSingle(dst[0])[i] := Single(src[i]) / 32767.0;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PSingle(dst[0])[i] := Single(src[p]) / 32767.0;
        Inc(p);
        PSingle(dst[1])[i] := Single(src[p]) / 32767.0;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PSingle(dst[j])[i] := Single(src[p]) / 32767.0;
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI8ToFloat(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PSingle(dst[0])[i] := Single(src[i]) / 127.0;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PSingle(dst[0])[i] := Single(src[p]) / 127.0;
        Inc(p);
        PSingle(dst[1])[i] := Single(src[p]) / 127.0;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PSingle(dst[j])[i] := Single(src[p]) / 127.0;
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI32ToI32(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src^, dst[0]^, samples * sizeof(int32));
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        PInt32(dst[0])[i] := src[p];
        PInt32(dst[1])[i] := src[p+1];
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI32ToI16(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := Int16(src[i]);
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := Int16(src[p]);
        Inc(p);
        PInt16(dst[1])[i] := Int16(src[p]);
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := Int16(src[p]);
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI32ToI8(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt8(dst[0])[i] := Int8(src[i]);
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt8(dst[0])[i] := Int8(src[p]);
        Inc(p);
        PInt8(dst[1])[i] := Int8(src[p]);
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt8(dst[j])[i] := Int8(src[p]);
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI32ToI16(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := Int16( Int64(src[i]) * Int64(32767) div Int64(2147483647) ) ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := Int16( Int64(src[p]) * Int64(32767) div Int64(2147483647) );
        Inc(p);
        PInt16(dst[1])[i] := Int16( Int64(src[p]) * Int64(32767) div Int64(2147483647) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := Int16( Int64(src[p]) * Int64(32767) div Int64(2147483647) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI32ToI8(const src : PInt32; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt8(dst[0])[i] := Int8( Int64(src[i]) * Int64(127) div Int64(2147483647) ) ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt8(dst[0])[i] := Int8( Int64(src[p]) * Int64(127) div Int64(2147483647) );
        Inc(p);
        PInt8(dst[1])[i] := Int8( Int64(src[p]) * Int64(127) div Int64(2147483647) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt8(dst[j])[i] := Int8( Int64(src[p]) * Int64(127) div Int64(2147483647) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI16ToI16(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src^, dst[0]^, samples * sizeof(int16));
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := src[p];
        Inc(p);
        PInt16(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI16ToI8(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := Int8( src[i] ) ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := Int8( src[p] );
        Inc(p);
        PInt16(dst[1])[i] := Int8( src[p] );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := Int8( src[p] );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI16ToI8(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt8(dst[0])[i] := Int8( Int32(src[i]) * Int32(127) div Int32(32767) );
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt8(dst[0])[i] := Int8( Int32(src[p]) * Int32(127) div Int32(32767) );
        Inc(p);
        PInt8(dst[1])[i] := Int8( Int32(src[p]) * Int32(127) div Int32(32767) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt8(dst[j])[i] := Int8( Int32(src[p]) * Int32(127) div Int32(32767) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI8ToI8(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src^, dst[0]^, samples * sizeof(int8));
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt8(dst[0])[i] := src[p];
        Inc(p);
        PInt8(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt8(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI8ToI16(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := src[i];
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := src[p];
        Inc(p);
        PInt16(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI8ToI16(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := Int16( Int32(src[i]) * Int32(32767) div Int32(127) );
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := Int16( Int32(src[p]) * Int32(32767) div Int32(127) );
        Inc(p);
        PInt16(dst[1])[i] := Int16( Int32(src[p]) * Int32(32767) div Int32(127) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] := Int16( Int32(src[p]) * Int32(32767) div Int32(127) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI8ToI32(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt32(dst[0])[i] := src[i];
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt32(dst[0])[i] := src[p];
        Inc(p);
        PInt32(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI8ToI32(const src : PInt8; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt32(dst[0])[i] := Int32( Int64(src[i]) * Int64(2147483647) div Int64(127) );
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt32(dst[0])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(127) );
        Inc(p);
        PInt32(dst[1])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(127) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(127) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveI16ToI32(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt32(dst[0])[i] := src[i];
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt32(dst[0])[i] := src[p];
        Inc(p);
        PInt32(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveNormI16ToI32(const src : PInt16; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt32(dst[0])[i] := Int32( Int64(src[i]) * Int64(2147483647) div Int64(32767) );
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt32(dst[0])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(32767) );
        Inc(p);
        PInt32(dst[1])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(32767) );
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := Int32( Int64(src[p]) * Int64(2147483647) div Int64(32767) );
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveFloatToFloat(const src : PSingle; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src^, dst[0]^, samples * sizeof(Single));
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PSingle(dst[0])[i] := src[p];
        Inc(p);
        PSingle(dst[1])[i] := src[p];
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PSingle(dst[j])[i] := src[p];
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveFloatToI8(const src : PSingle; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt8(dst[0])[i] := FloatRoundI8(src[i] * 127.0) ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt8(dst[0])[i] := FloatRoundI8(src[p] * 127.0);
        Inc(p);
        PInt8(dst[1])[i] := FloatRoundI8(src[p] * 127.0);
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt8(dst[j])[i] := FloatRoundI8(src[p] * 127.0);
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveFloatToI16(const src : PSingle; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt16(dst[0])[i] := FloatRoundI16(src[i] * 32767.0)  ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt16(dst[0])[i] := FloatRoundI16(src[p] * 32767.0) ;
        Inc(p);
        PInt16(dst[1])[i] := FloatRoundI16(src[p] * 32767.0) ;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt16(dst[j])[i] :=FloatRoundI16(src[p] * 32767.0) ;
         Inc(p);
       end;
    end;
  end;
end;

procedure UninterleaveFloatToI32(const src : PSingle; dst : PPointer; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        PInt32(dst[0])[i] := FloatRoundI32(Double(src[i]) * 2147483647.0)  ;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        PInt32(dst[0])[i] := FloatRoundI32(Double(src[p]) * 2147483647.0) ;
        Inc(p);
        PInt32(dst[1])[i] := FloatRoundI32(Double(src[p]) * 2147483647.0) ;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         PInt32(dst[j])[i] := FloatRoundI32(Double(src[p]) * 2147483647.0) ;
         Inc(p);
       end;
    end;
  end;
end;

// interleave functions

procedure InterleaveI32ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Double(PInt32(src[0])[i]) / Double(2147483647.0);
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        dst[p] := Double(PInt32(src[0])[i]) / Double(2147483647.0);
        Inc(p);
        dst[p] := Double(PInt32(src[1])[i]) / Double(2147483647.0);
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         dst[p] := Double(PInt32(src[j])[i]) / Double(2147483647.0);
         Inc(p);
       end;
    end;
  end;
end;

procedure InterleaveI16ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Single(PInt16(src[0])[i]) / 32767.0;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        dst[p] := Single(PInt16(src[0])[i]) / 32767.0;
        Inc(p);
        dst[p] := Single(PInt16(src[1])[i]) / 32767.0;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         dst[p] := Single(PInt16(src[j])[i]) / 32767.0;
         Inc(p);
       end;
    end;
  end;
end;

procedure InterleaveI8ToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Single(PInt8(src[0])[i]) / 127.0;
  end;
  2: begin
     p := 0;
     for i := 0 to samples-1 do
     begin
        dst[p] := Single(PInt8(src[0])[i]) / 127.0;
        Inc(p);
        dst[p] := Single(PInt8(src[1])[i]) / 127.0;
        Inc(p);
     end;
  end;
  else
    p := 0;
    for i := 0 to samples-1 do
    begin
       for j := 0 to channels-1 do
       begin
         dst[p] := Single(PInt8(src[j])[i]) / 127.0;
         Inc(p);
       end;
    end;
  end;
end;

procedure InterleaveI32ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src[0]^, dst^, samples * sizeof(int32));
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := PInt32(src[0])[i];
        dst[p+1] := PInt32(src[1])[i];
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := PInt32(src[j])[i];
    end;
  end;
end;

procedure InterleaveI32ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int16( PInt32(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int16( PInt32(src[0])[i] );
        dst[p+1] := Int16( PInt32(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int16( PInt32(src[j])[i] );
    end;
  end;
end;

procedure InterleaveI32ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int8( PInt32(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int8( PInt32(src[0])[i] );
        dst[p+1] := Int8( PInt32(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int8( PInt32(src[j])[i] );
    end;
  end;
end;

procedure InterleaveNormI32ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int16( Int64(PInt32(src[0])[i]) * Int64(32767) div Int64(2147483647) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int16( Int64(PInt32(src[0])[i]) * Int64(32767) div Int64(2147483647) );
        dst[p+1] := Int16( Int64(PInt32(src[1])[i]) * Int64(32767) div Int64(2147483647) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int16( Int64(PInt32(src[j])[i]) * Int64(32767) div Int64(2147483647) );
    end;
  end;
end;

procedure InterleaveNormI32ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int8( Int64(PInt32(src[0])[i]) * Int64(127) div Int64(2147483647) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int8( Int64(PInt32(src[0])[i]) * Int64(127) div Int64(2147483647) );
        dst[p+1] := Int8( Int64(PInt32(src[1])[i]) * Int64(127) div Int64(2147483647) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int8( Int64(PInt32(src[j])[i]) * Int64(127) div Int64(2147483647) );
    end;
  end;
end;

procedure InterleaveI16ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src[0]^, dst^, samples * sizeof(int16));
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := PInt16(src[0])[i];
        dst[p+1] := PInt16(src[1])[i];
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := PInt16(src[j])[i];
    end;
  end;
end;

procedure InterleaveI16ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int8( PInt16(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int8( PInt16(src[0])[i] );
        dst[p+1] := Int8( PInt16(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int8( PInt16(src[j])[i] );
    end;
  end;
end;

procedure InterleaveNormI16ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int8( Int32(PInt16(src[0])[i]) * Int32(127) div Int32(32767) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int8( Int32(PInt16(src[0])[i]) * Int32(127) div Int32(32767) );
        dst[p+1] := Int8( Int32(PInt16(src[1])[i]) * Int32(127) div Int32(32767) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int8( Int32(PInt16(src[j])[i]) * Int32(127) div Int32(32767) );
    end;
  end;
end;

procedure InterleaveI8ToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src[0]^, dst^, samples * sizeof(int8));
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := PInt8(src[0])[i];
        dst[p+1] := PInt8(src[1])[i];
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := PInt8(src[j])[i];
    end;
  end;
end;

procedure InterleaveI8ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int16( PInt8(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int16( PInt8(src[0])[i] );
        dst[p+1] := Int16( PInt8(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int16( PInt8(src[j])[i] );
    end;
  end;
end;

procedure InterleaveNormI8ToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int16( Int32(PInt8(src[0])[i]) * Int32(32767) div Int32(127) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int16( Int32(PInt8(src[0])[i]) * Int32(32767) div Int32(127) );
        dst[p+1] := Int16( Int32(PInt8(src[1])[i]) * Int32(32767) div Int32(127) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int16( Int32(PInt8(src[j])[i]) * Int32(32767) div Int32(127) );
    end;
  end;
end;

procedure InterleaveI8ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int32( PInt8(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int32( PInt8(src[0])[i] );
        dst[p+1] := Int32( PInt8(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int32( PInt8(src[j])[i] );
    end;
  end;
end;

procedure InterleaveNormI8ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int32( Int64(PInt8(src[0])[i]) * Int64(2147483647) div Int64(127) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int32( Int64(PInt8(src[0])[i]) * Int64(2147483647) div Int64(127) );
        dst[p+1] := Int32( Int64(PInt8(src[1])[i]) * Int64(2147483647) div Int64(127) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int32( Int64(PInt8(src[j])[i]) * Int64(2147483647) div Int64(127) );
    end;
  end;
end;

procedure InterleaveI16ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int32( PInt16(src[0])[i] ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int32( PInt16(src[0])[i] );
        dst[p+1] := Int32( PInt16(src[1])[i] );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int32( PInt16(src[j])[i] );
    end;
  end;
end;

procedure InterleaveNormI16ToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := Int32( Int64(PInt16(src[0])[i]) * Int64(2147483647) div Int64(32767) ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := Int32( Int64(PInt16(src[0])[i]) * Int64(2147483647) div Int64(32767) );
        dst[p+1] := Int32( Int64(PInt16(src[1])[i]) * Int64(2147483647) div Int64(32767) );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := Int32( Int64(PInt16(src[j])[i]) * Int64(2147483647) div Int64(32767) );
    end;
  end;
end;

procedure InterleaveFloatToFloat(const src : PPointer; dst : PSingle; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     Move(src[0]^, dst^, samples * sizeof(Single));
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := PSingle(src[0])[i];
        dst[p+1] := PSingle(src[1])[i];
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := PSingle(src[j])[i];
    end;
  end;
end;

procedure InterleaveFloatToI8(const src : PPointer; dst : PInt8; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := FloatRoundI8(PSingle(src[0])[i] * 127.0 ) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := FloatRoundI8(PSingle(src[0])[i] * 127.0 );
        dst[p+1] := FloatRoundI8(PSingle(src[1])[i] * 127.0 );
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := FloatRoundI8(PSingle(src[j])[i] * 127.0 );
    end;
  end;
end;

procedure InterleaveFloatToI16(const src : PPointer; dst : PInt16; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := FloatRoundI16(PSingle(src[0])[i] * 32767.0) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := FloatRoundI16(PSingle(src[0])[i] * 32767.0);
        dst[p+1] := FloatRoundI16(PSingle(src[1])[i] * 32767.0);
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := FloatRoundI16(PSingle(src[j])[i] * 32767.0);
    end;
  end;
end;

procedure InterleaveFloatToI32(const src : PPointer; dst : PInt32; channels, samples : Integer);
var i, j, p: integer;
begin
  case channels of
  1: begin
     for i := 0 to samples-1 do
        dst[i] := FloatRoundI32(PSingle(src[0])[i] * 2147483647.0) ;
  end;
  2: begin
     for i := 0 to samples-1 do
     begin
        p := i shl 1;
        dst[p] := FloatRoundI32(PSingle(src[0])[i] * 2147483647.0);
        dst[p+1] := FloatRoundI32(PSingle(src[1])[i] * 2147483647.0);
     end;
  end;
  else
    for i := 0 to samples-1 do
    begin
       p := i * channels;
       for j := 0 to channels-1 do
         dst[p + j] := FloatRoundI32(PSingle(src[j])[i] * 2147483647.0);
    end;
  end;
end;

procedure InterleaveSamples(const src : PPointer; dst : Pointer;
                                  src_sample_size : TSoundSampleSize;
                                  dst_sample_size : TSoundSampleSize;
                                  normalized : Boolean;
                                  channels, samples : Integer);
begin
  case src_sample_size of
    ss8bit : begin
      case dst_sample_size of
        ss8bit : begin
          InterleaveI8ToI8(src, pint8(dst), channels, samples);
        end;
        ss16bit : begin
          if normalized then
            InterleaveNormI8ToI16(src, pint16(dst), channels, samples)
          else
            InterleaveI8ToI16(src, pint16(dst), channels, samples);
        end;
        ss32bit : begin
          if normalized then
            InterleaveNormI8ToI32(src, pint32(dst), channels, samples)
          else
            InterleaveI8ToI32(src, pint32(dst), channels, samples);
        end;
        ssfloat : begin
           InterleaveI8ToFloat(src, psingle(dst), channels, samples);
        end;
      end;
    end;
    ss16bit : begin
      case dst_sample_size of
        ss8bit : begin
          if normalized then
            InterleaveNormI16ToI8(src, pint8(dst), channels, samples) else
            InterleaveI16ToI8(src, pint8(dst), channels, samples);
        end;
        ss16bit : begin
            InterleaveI16ToI16(src, pint16(dst), channels, samples);
        end;
        ss32bit : begin
          if normalized then
            InterleaveNormI16ToI32(src, pint32(dst), channels, samples)
          else
            InterleaveI16ToI32(src, pint32(dst), channels, samples);
        end;
        ssfloat : begin
           InterleaveI16ToFloat(src, psingle(dst), channels, samples);
        end;
      end;
    end;
    ss32bit : begin
      case dst_sample_size of
        ss8bit : begin
          if normalized then
            InterleaveNormI32ToI8(src, pint8(dst), channels, samples) else
            InterleaveI32ToI8(src, pint8(dst), channels, samples);
        end;
        ss16bit : begin
          if normalized then
            InterleaveNormI32ToI16(src, pint16(dst), channels, samples) else
            InterleaveI32ToI16(src, pint16(dst), channels, samples);
        end;
        ss32bit : begin
           InterleaveI32ToI32(src, pint32(dst), channels, samples);
        end;
        ssfloat : begin
           InterleaveI32ToFloat(src, psingle(dst), channels, samples);
        end;
      end;
    end;
    ssfloat : begin
      case dst_sample_size of
        ss8bit : begin
          InterleaveFloatToI8(src, pint8(dst), channels, samples);
        end;
        ss16bit : begin
          InterleaveFloatToI16(src, pint16(dst), channels, samples);
        end;
        ss32bit : begin
          InterleaveFloatToI32(src, pint32(dst), channels, samples);
        end;
        ssfloat : begin
          InterleaveFloatToFloat(src, psingle(dst), channels, samples);
        end;
      end;
    end;
  end;
end;

procedure UninterleaveSamples(const src : Pointer; dst : PPointer;
                                  src_sample_size : TSoundSampleSize;
                                  dst_sample_size : TSoundSampleSize;
                                  normalized : Boolean;
                                  channels, samples : Integer);
begin
  case src_sample_size of
    ss8bit : begin
      case dst_sample_size of
        ss8bit : begin
          UninterleaveI8ToI8(pint8(src), dst, channels, samples);
        end;
        ss16bit : begin
          if normalized then
            UninterleaveNormI8ToI16(pint8(src), dst, channels, samples)
          else
            UninterleaveI8ToI16(pint8(src), dst, channels, samples);
        end;
        ss32bit : begin
          if normalized then
            UninterleaveNormI8ToI32(pint8(src), dst, channels, samples)
          else
            UninterleaveI8ToI32(pint8(src), dst, channels, samples);
        end;
        ssfloat : begin
           UninterleaveI8ToFloat(pint8(src), dst, channels, samples);
        end;
      end;
    end;
    ss16bit : begin
      case dst_sample_size of
        ss8bit : begin
          if normalized then
            UninterleaveNormI16ToI8(pint16(src), dst, channels, samples) else
            UninterleaveI16ToI8(pint16(src), dst, channels, samples);
        end;
        ss16bit : begin
            UninterleaveI16ToI16(pint16(src), dst, channels, samples);
        end;
        ss32bit : begin
          if normalized then
            UninterleaveNormI16ToI32(pint16(src), dst, channels, samples)
          else
            UninterleaveI16ToI32(pint16(src), dst, channels, samples);
        end;
        ssfloat : begin
           UninterleaveI16ToFloat(pint16(src), dst, channels, samples);
        end;
      end;
    end;
    ss32bit : begin
      case dst_sample_size of
        ss8bit : begin
          if normalized then
            UninterleaveNormI32ToI8(pint32(src), dst, channels, samples) else
            UninterleaveI32ToI8(pint32(src), dst, channels, samples);
        end;
        ss16bit : begin
          if normalized then
            UninterleaveNormI32ToI16(pint32(src), dst, channels, samples) else
            UninterleaveI32ToI16(pint32(src), dst, channels, samples);
        end;
        ss32bit : begin
           UninterleaveI32ToI32(pint32(src), dst, channels, samples);
        end;
        ssfloat : begin
           UninterleaveI32ToFloat(pint32(src), dst, channels, samples);
        end;
      end;
    end;
    ssfloat : begin
      case dst_sample_size of
        ss8bit : begin
          UninterleaveFloatToI8(psingle(src), dst, channels, samples);
        end;
        ss16bit : begin
          UninterleaveFloatToI16(psingle(src), dst, channels, samples);
        end;
        ss32bit : begin
          UninterleaveFloatToI32(psingle(src), dst, channels, samples);
        end;
        ssfloat : begin
          UninterleaveFloatToFloat(psingle(src), dst, channels, samples);
        end;
      end;
    end;
  end;
end;

end.

