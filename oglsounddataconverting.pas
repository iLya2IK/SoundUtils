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

{  Copyright (C) 2007-2008 Jean-Marc Valin
   Copyright (C) 2008      Thorvald Natvig
   File: resample.c
   Arbitrary resampling code }
{
   The design goals of this code are:
      - Very fast algorithm
      - SIMD-friendly algorithm
      - Low memory requirement
      - Good *perceptual* quality (and not best SNR)
   Warning: This resampler is relatively new. Although I think I got rid of
   all the major bugs and I don't expect the API to change anymore, there
   may be something I've missed. So use with caution.
   This algorithm is based on this original resampling algorithm:
   Smith, Julius O. Digital Audio Resampling Home Page
   Center for Computer Research in Music and Acoustics (CCRMA),
   Stanford University, 2007.
   Web published at https://ccrma.stanford.edu/~jos/resample/.
   There is one main difference, though. This resampler uses cubic
   interpolation instead of linear interpolation in the above paper. This
   makes the table much smaller and makes it possible to compute that table
   on a per-stream basis. In turn, being able to tweak the table for each
   stream makes it possible to both reduce complexity on simple ratios
   (e.g. 2/3), and get rid of the rounding operations in the inner loop.
   The latter both reduces CPU time and makes the algorithm more SIMD-friendly.
}
{$REGION 'Resample declarations'}
const RESAMPLER_ERR_SUCCESS         = 0;
      RESAMPLER_ERR_OVERFLOW        = -1;
      RESAMPLER_ERR_ALLOC_FAILED    = -2;
      RESAMPLER_ERR_INVALID_ARG     = -3;
      RESAMPLER_ERR_BAD_STATE       = -4;
      RESAMPLER_ERR_PTR_OVERLAP     = -5;

type
 spx_uint32_t = Cardinal;
 spx_int32_t = Int32;
 spx_int16_t = Int16;

 spx_word16_t = Single;
 spx_word32_t = Single;
 spx_mem_t    = Single;
 spx_coef_t   = Single;
 spx_lsp_t    = Single;
 spx_sig_t    = Single;

 pspx_uint32_t = ^spx_uint32_t;
 pspx_int32_t = ^spx_int32_t;
 pspx_int16_t = ^spx_int16_t;
 ppspx_word16_t = ^pspx_word16_t;

 pspx_word16_t = ^spx_word16_t;
 pspx_word32_t = ^spx_word32_t;

 pSpeexResamplerState = ^SpeexResamplerState;
 pResamplerBasicFunc = function (st : pSpeexResamplerState;
                                 p1 :  spx_uint32_t;
                                 const p2 : pspx_word16_t;
                                 p3 :  pspx_uint32_t;
                                 p4 :  pspx_word16_t;
                                 p5 :  pspx_uint32_t) : integer;
 SpeexResamplerState = record
    in_rate  : spx_uint32_t;
    out_rate : spx_uint32_t;
    num_rate : spx_uint32_t;
    den_rate : spx_uint32_t;

    quality  : integer;
    nb_channels    : spx_uint32_t;
    filt_len       : spx_uint32_t;
    mem_alloc_size : spx_uint32_t;
    buffer_size    : spx_uint32_t;
    int_advance    : integer;
    frac_advance   : integer;
    cutoff         : single;
    oversample     : spx_uint32_t;
    initialised    : Boolean;
    started        : Boolean;

    { These are per-channel }
    last_sample   : pspx_int32_t;
    samp_frac_num : pspx_uint32_t;
    magic_samples : pspx_uint32_t;

    mem : pspx_word16_t;
    sinc_table : pspx_word16_t;
    sinc_table_length : spx_uint32_t;
    resampler_ptr : pResamplerBasicFunc;

    in_stride : integer;
    out_stride : integer;
 end;

function speex_resampler_init(nb_channels : spx_uint32_t;
                              in_rate : spx_uint32_t;
                              out_rate : spx_uint32_t;
                              quality : integer;
                              err : pinteger) : pSpeexResamplerState;
function speex_resampler_init_frac(nb_channels : spx_uint32_t;
                              ratio_num, ratio_den,
                              in_rate, out_rate : spx_uint32_t;
                              quality : integer;
                              err : pinteger) : pSpeexResamplerState;
procedure speex_resampler_destroy(st : pSpeexResamplerState);

function speex_resampler_process_float(st : pSpeexResamplerState;
                              channel_index : spx_uint32_t;
                              const inp : PSingle;
                              in_len : pspx_uint32_t;
                              outp : pSingle;
                              out_len : pspx_uint32_t) : Integer;
function speex_resampler_process_int(st : pSpeexResamplerState;
                              channel_index : spx_uint32_t;
                              const inp : pspx_int16_t;
                              in_len : pspx_uint32_t;
                              outp : pspx_int16_t;
                              out_len : pspx_uint32_t) : Integer;
function speex_resampler_process_interleaved_float(st : pSpeexResamplerState;
                              const inp : PSingle;
                              in_len : pspx_uint32_t;
                              outp : pSingle;
                              out_len : pspx_uint32_t) : Integer;
function speex_resampler_process_interleaved_int(st : pSpeexResamplerState;
                              const inp : pspx_int16_t;
                              in_len : pspx_uint32_t;
                              outp : pspx_int16_t;
                              out_len : pspx_uint32_t) : Integer;

function speex_resampler_set_rate(st : pSpeexResamplerState;
                              in_rate, out_rate : spx_uint32_t) : integer;
procedure speex_resampler_get_rate(st : pSpeexResamplerState;
                              in_rate : pspx_uint32_t;
                              out_rate : pspx_uint32_t);
function speex_resampler_set_rate_frac(st : pSpeexResamplerState;
                              ratio_num, ratio_den,
                              in_rate, out_rate : spx_uint32_t) : integer;
procedure speex_resampler_get_ratio(st : pSpeexResamplerState;
                              ratio_num, ratio_den : pspx_uint32_t);
function  speex_resampler_set_quality(st : pSpeexResamplerState;
                              quality : integer) : integer;
procedure speex_resampler_get_quality(st : pSpeexResamplerState;
                                      quality : Pinteger);
procedure speex_resampler_set_input_stride(st : pSpeexResamplerState;
                                           stride : spx_uint32_t);
procedure speex_resampler_get_input_stride(st : pSpeexResamplerState;
                                           stride : pspx_uint32_t);
procedure speex_resampler_set_output_stride(st : pSpeexResamplerState;
                                            stride : spx_uint32_t);
procedure speex_resampler_get_output_stride(st : pSpeexResamplerState;
                                                 stride : pspx_uint32_t);
function speex_resampler_get_input_latency(st : pSpeexResamplerState) : integer;
function speex_resampler_get_output_latency(st : pSpeexResamplerState) : integer;
function speex_resampler_skip_zeros(st : pSpeexResamplerState) : integer;
function speex_resampler_reset_mem(st : pSpeexResamplerState) : integer;

function speex_resampler_strerror(err : integer) : String;
{$ENDREGION}

implementation

uses Math;

{ this part of the code is a direct translation into freepascal of the source
  code written in C
  https://github.com/xiph/opus-tools/blob/master/src/resample.c
     Copyright (C) 2007-2008 Jean-Marc Valin
     Copyright (C) 2008      Thorvald Natvig
     File: resample.c
     Arbitrary resampling code }
{$REGION 'resample.c translation'}

{$INLINE ON}
{$MACRO ON}

const
 M_PI = 3.14159265358979323846;
 UINT32_MAX = 4294967295;
 FIXED_STACK_ALLOC = 8192;
 INT_MAX = MaxInt;

const kaiser12_table : Array [0..67] of double = (
   0.99859849, 1.00000000, 0.99859849, 0.99440475, 0.98745105, 0.97779076,
   0.96549770, 0.95066529, 0.93340547, 0.91384741, 0.89213598, 0.86843014,
   0.84290116, 0.81573067, 0.78710866, 0.75723148, 0.72629970, 0.69451601,
   0.66208321, 0.62920216, 0.59606986, 0.56287762, 0.52980938, 0.49704014,
   0.46473455, 0.43304576, 0.40211431, 0.37206735, 0.34301800, 0.31506490,
   0.28829195, 0.26276832, 0.23854851, 0.21567274, 0.19416736, 0.17404546,
   0.15530766, 0.13794294, 0.12192957, 0.10723616, 0.09382272, 0.08164178,
   0.07063950, 0.06075685, 0.05193064, 0.04409466, 0.03718069, 0.03111947,
   0.02584161, 0.02127838, 0.01736250, 0.01402878, 0.01121463, 0.00886058,
   0.00691064, 0.00531256, 0.00401805, 0.00298291, 0.00216702, 0.00153438,
   0.00105297, 0.00069463, 0.00043489, 0.00025272, 0.00013031, 0.0000527734,
   0.00001000, 0.00000000);
{
 const kaiser12_table : Array [0..35] of double = (
   0.99440475, 1.00000000, 0.99440475, 0.97779076, 0.95066529, 0.91384741,
   0.86843014, 0.81573067, 0.75723148, 0.69451601, 0.62920216, 0.56287762,
   0.49704014, 0.43304576, 0.37206735, 0.31506490, 0.26276832, 0.21567274,
   0.17404546, 0.13794294, 0.10723616, 0.08164178, 0.06075685, 0.04409466,
   0.03111947, 0.02127838, 0.01402878, 0.00886058, 0.00531256, 0.00298291,
   0.00153438, 0.00069463, 0.00025272, 0.0000527734, 0.00000500, 0.00000000);
}
 const kaiser10_table : Array [0..35] of double = (
   0.99537781, 1.00000000, 0.99537781, 0.98162644, 0.95908712, 0.92831446,
   0.89005583, 0.84522401, 0.79486424, 0.74011713, 0.68217934, 0.62226347,
   0.56155915, 0.50119680, 0.44221549, 0.38553619, 0.33194107, 0.28205962,
   0.23636152, 0.19515633, 0.15859932, 0.12670280, 0.09935205, 0.07632451,
   0.05731132, 0.04193980, 0.02979584, 0.02044510, 0.01345224, 0.00839739,
   0.00488951, 0.00257636, 0.00115101, 0.00035515, 0.00000000, 0.00000000);

 const kaiser8_table : Array [0..35] of double = (
   0.99635258, 1.00000000, 0.99635258, 0.98548012, 0.96759014, 0.94302200,
   0.91223751, 0.87580811, 0.83439927, 0.78875245, 0.73966538, 0.68797126,
   0.63451750, 0.58014482, 0.52566725, 0.47185369, 0.41941150, 0.36897272,
   0.32108304, 0.27619388, 0.23465776, 0.19672670, 0.16255380, 0.13219758,
   0.10562887, 0.08273982, 0.06335451, 0.04724088, 0.03412321, 0.02369490,
   0.01563093, 0.00959968, 0.00527363, 0.00233883, 0.00050000, 0.00000000);

 const kaiser6_table : Array [0..35] of double = (
   0.99733006, 1.00000000, 0.99733006, 0.98935595, 0.97618418, 0.95799003,
   0.93501423, 0.90755855, 0.87598009, 0.84068475, 0.80211977, 0.76076565,
   0.71712752, 0.67172623, 0.62508937, 0.57774224, 0.53019925, 0.48295561,
   0.43647969, 0.39120616, 0.34752997, 0.30580127, 0.26632152, 0.22934058,
   0.19505503, 0.16360756, 0.13508755, 0.10953262, 0.08693120, 0.06722600,
   0.05031820, 0.03607231, 0.02432151, 0.01487334, 0.00752000, 0.00000000);

type
  pFuncDef = ^FuncDef;
  FuncDef = record
     table : pDouble;
     oversample : Int32;
  end;

const kaiser12_funcdef : FuncDef = ( table : @kaiser12_table; oversample : 64; );
{$define KAISER12:=(@kaiser12_funcdef)}
const kaiser10_funcdef : FuncDef = ( table : @kaiser10_table; oversample : 32; );
{$define KAISER10:=(@kaiser10_funcdef)}
const kaiser8_funcdef : FuncDef = ( table : @kaiser8_table; oversample : 32; );
{$define KAISER8:=(@kaiser8_funcdef)}
const kaiser6_funcdef : FuncDef = ( table : @kaiser6_table; oversample : 32; );
{$define KAISER6:=(@kaiser6_funcdef)}

type
  QualityMapping = record
     base_length : int32;
     oversample : int32;
     downsample_bandwidth : single;
     upsample_bandwidth : single;
     window_func : pFuncDef;
  end;

  { This table maps conversion quality to internal parameters. There are two
     reasons that explain why the up-sampling bandwidth is larger than the
     down-sampling bandwidth:
     1) When up-sampling, we can assume that the spectrum is already attenuated
        close to the Nyquist rate (from an A/D or a previous resampling filter)
     2) Any aliasing that occurs very close to the Nyquist rate will be masked
        by the sinusoids/noise just below the Nyquist rate (guaranteed only for
        up-sampling).
  }
  const quality_map : Array [0..10] of QualityMapping = (
     (base_length:  8; oversample: 4; downsample_bandwidth:0.830; upsample_bandwidth:0.860; window_func: KAISER6 ;), { Q0 }
     (base_length: 16; oversample: 4; downsample_bandwidth:0.850; upsample_bandwidth:0.880; window_func: KAISER6 ;), { Q1 }
     (base_length: 32; oversample: 4; downsample_bandwidth:0.882; upsample_bandwidth:0.910; window_func: KAISER6 ;), { Q2 } { 82.3% cutoff ( ~60 dB stop) 6  }
     (base_length: 48; oversample: 8; downsample_bandwidth:0.895; upsample_bandwidth:0.917; window_func: KAISER8 ;), { Q3 } { 84.9% cutoff ( ~80 dB stop) 8  }
     (base_length: 64; oversample: 8; downsample_bandwidth:0.921; upsample_bandwidth:0.940; window_func: KAISER8 ;), { Q4 } { 88.7% cutoff ( ~80 dB stop) 8  }
     (base_length: 80; oversample:16; downsample_bandwidth:0.922; upsample_bandwidth:0.940; window_func: KAISER10;), { Q5 } { 89.1% cutoff (~100 dB stop) 10 }
     (base_length: 96; oversample:16; downsample_bandwidth:0.940; upsample_bandwidth:0.945; window_func: KAISER10;), { Q6 } { 91.5% cutoff (~100 dB stop) 10 }
     (base_length:128; oversample:16; downsample_bandwidth:0.950; upsample_bandwidth:0.950; window_func: KAISER10;), { Q7 } { 93.1% cutoff (~100 dB stop) 10 }
     (base_length:160; oversample:16; downsample_bandwidth:0.960; upsample_bandwidth:0.960; window_func: KAISER10;), { Q8 } { 94.5% cutoff (~100 dB stop) 10 }
     (base_length:192; oversample:32; downsample_bandwidth:0.968; upsample_bandwidth:0.968; window_func: KAISER12;), { Q9 } { 95.5% cutoff (~100 dB stop) 10 }
     (base_length:256; oversample:32; downsample_bandwidth:0.975; upsample_bandwidth:0.975; window_func: KAISER12;)  { Q10} { 96.6% cutoff (~100 dB stop) 10 }
  );

function compute_func(x : single; const func : pFuncDef) : double;
var
  y, frac : Single;
  interp : Array [0..3] of double;
  ind : int32;
begin
   y := x*func^.oversample;
   ind := int32(trunc(y)); // trunc == floor for positive arg
   frac := (y-ind);
   { CSE with handle the repeated powers }
   interp[3] :=  -0.1666666667*frac + 0.1666666667*(frac*frac*frac);
   interp[2] := frac + 0.5*(frac*frac) - 0.5*(frac*frac*frac);
   {interp[2] = 1.f - 0.5f*frac - frac*frac + 0.5f*frac*frac*frac;}
   interp[0] := -0.3333333333*frac + 0.5*(frac*frac) - 0.1666666667*(frac*frac*frac);
   { Just to make sure we don't have rounding problems }
   interp[1] := 1.0-interp[3]-interp[2]-interp[0];

   {sum = frac*accum[1] + (1-frac)*accum[2];}
   result := interp[0]*func^.table[ind] + interp[1]*func^.table[ind+1] + interp[2]*func^.table[ind+2] + interp[3]*func^.table[ind+3];
end;

{ The slow way of computing a sinc for the table. Should improve that some day }
function sinc(cutoff, x : single; N : int32; const window_func : pFuncDef) : spx_word16_t;
var
  xx : single;
begin
   {fprintf (stderr, "%f ", x);}
   xx := x * cutoff;
   if (abs(x)<1e-6) then
      Exit(cutoff)
   else if (abs(x) > (0.5*N)) then
      Exit(0);
   {FIXME: Can it really be any slower than this? }
   Result := cutoff*sin(M_PI*xx)/(M_PI*xx) * compute_func(abs(2.0*x/N), window_func);
end;

procedure cubic_coef(frac : spx_word16_t; var interp : array of spx_word16_t);
begin
   { Compute interpolation coefficients. I'm not sure whether this corresponds to cubic interpolation
   but I know it's MMSE-optimal on a sinc }
   interp[0] := ( -0.16667*frac + 0.16667*frac*frac*frac);
   interp[1] := (frac + 0.5*frac*frac - 0.5*frac*frac*frac);
   {interp[2] = 1.f - 0.5f*frac - frac*frac + 0.5f*frac*frac*frac;}
   interp[3] := (-0.33333*frac + 0.5*frac*frac - 0.16667*frac*frac*frac);
   { Just to make sure we don't have rounding problems }
   interp[2] := (1.0-interp[0]-interp[1]-interp[3]);
end;

function MULT16_16(a, b : Single) : Single; inline;
begin
  Result := (a*b);
end;

function MULT16_32_Q15(a, b : Single) : Single; inline;
begin
  Result := (a*b);
end;

function WORD2INT(x : Single) : spx_int16_t; inline;
begin
  if x < -32767.5 then
     Result := -32768 else
  begin
    if x > 32766.5 then
      Result := 32767 else
    begin
      if x < 0 then
         Result := spx_int16_t(trunc(x - 0.5)) else
         Result := spx_int16_t(trunc(0.5 + x));
    end;
  end;
end;

function resampler_basic_direct_single(st : pSpeexResamplerState;
                                channel_index : spx_uint32_t;
                                const inp : pspx_word16_t;
                                in_len : pspx_uint32_t;
                                outp : pspx_word16_t;
                                out_len : pspx_uint32_t) : int32;
var N : spx_uint32_t;
    out_sample, last_sample : int32;
    samp_frac_num : spx_uint32_t;
    sinc_table : pspx_word16_t;
    out_stride, int_advance, frac_advance : int32;
    den_rate : spx_uint32_t;
    sum : spx_word32_t;

    sinct, iptr : pspx_word16_t;
    j : int32;
begin
   N := st^.filt_len;
   out_sample := 0;
   last_sample := st^.last_sample[channel_index];
   samp_frac_num := st^.samp_frac_num[channel_index];
   sinc_table := st^.sinc_table;
   out_stride := st^.out_stride;
   int_advance := st^.int_advance;
   frac_advance := st^.frac_advance;
   den_rate := st^.den_rate;

   while (not ((last_sample >= spx_int32_t(in_len^)) or
               (out_sample >= spx_int32_t(out_len^)))) do
   begin
      sinct := @(sinc_table[samp_frac_num*N]);
      iptr := @(inp[last_sample]);

      sum := 0;
      for j:=0 to N-1 do
        sum += MULT16_16(sinct[j], iptr[j]);

{    This code is slower on most DSPs which have only 2 accumulators.
      Plus this this forces truncation to 32 bits and you lose the HW guard bits.
      I think we can trust the compiler and let it vectorize and/or unroll itself.
      spx_word32_t accum[4] = (0,0,0,0);
      for(j=0;j<N;j+=4) (
        accum[0] += MULT16_16(sinct[j], iptr[j]);
        accum[1] += MULT16_16(sinct[j+1], iptr[j+1]);
        accum[2] += MULT16_16(sinct[j+2], iptr[j+2]);
        accum[3] += MULT16_16(sinct[j+3], iptr[j+3]);
      )
      sum = accum[0] + accum[1] + accum[2] + accum[3];
}
      //#define SATURATE32PSHR(x,shift,a) (x)
      // sum := SATURATE32PSHR(sum, 15, 32767);

      outp[out_stride * out_sample] := sum;
      Inc(out_sample);
      last_sample += int_advance;
      samp_frac_num += frac_advance;
      if (samp_frac_num >= den_rate) then
      begin
         samp_frac_num -= den_rate;
         Inc(last_sample);
      end;
   end;

   st^.last_sample[channel_index] := last_sample;
   st^.samp_frac_num[channel_index] := samp_frac_num;
   Result := out_sample;
end;

{ This is the same as the previous function, except with a double-precision accumulator }
function resampler_basic_direct_double(st : pSpeexResamplerState;
                                          channel_index : spx_uint32_t;
                                          const inp : pspx_word16_t;
                                          in_len : pspx_uint32_t;
                                          outp : pspx_word16_t;
                                          out_len : pspx_uint32_t) : integer;
var
   N, out_sample, last_sample, out_stride,
   int_advance, frac_advance : integer;
   samp_frac_num : spx_uint32_t;
   sinc_table : pspx_word16_t;
   sinct, iptr : pspx_word16_t;

   den_rate, j : spx_uint32_t;
   sum : Double;
   accum : Array [0..3] of double = (0, 0, 0, 0);
begin
   N := st^.filt_len;
   out_sample := 0;
   last_sample := st^.last_sample[channel_index];
   samp_frac_num := st^.samp_frac_num[channel_index];
   sinc_table := st^.sinc_table;
   out_stride := st^.out_stride;
   int_advance := st^.int_advance;
   frac_advance := st^.frac_advance;
   den_rate := st^.den_rate;

   while (not ((last_sample >= spx_int32_t(in_len^)) or
               (out_sample >= spx_int32_t(out_len^)))) do
   begin
      sinct := @( sinc_table[samp_frac_num*N]);
      iptr := @( inp[last_sample]);

      j := 0;
      while (j < N) do
      begin
        accum[0] += sinct[j]*iptr[j];
        accum[1] += sinct[j+1]*iptr[j+1];
        accum[2] += sinct[j+2]*iptr[j+2];
        accum[3] += sinct[j+3]*iptr[j+3];
        Inc(j, 4);
      end;
      sum := accum[0] + accum[1] + accum[2] + accum[3];

      outp[out_stride * out_sample] := sum;
      Inc(out_sample);
      last_sample += int_advance;
      samp_frac_num += frac_advance;
      if (samp_frac_num >= den_rate) then
      begin
         samp_frac_num -= den_rate;
         Inc(last_sample);
      end;
   end;

   st^.last_sample[channel_index] := last_sample;
   st^.samp_frac_num[channel_index] := samp_frac_num;
   result := out_sample;
end;

function resampler_basic_interpolate_single(st : pSpeexResamplerState;
                                            channel_index : spx_uint32_t;
                                            const inp : pspx_word16_t;
                                            in_len : pspx_uint32_t;
                                            outp : pspx_word16_t;
                                            out_len : pspx_uint32_t) : integer;
var
   N, out_sample, last_sample, out_stride,
   int_advance, frac_advance, offset : integer;
   samp_frac_num : spx_uint32_t;
   iptr : pspx_word16_t;

   sum : spx_word32_t;
   den_rate, j : spx_uint32_t;
   frac : spx_word16_t;

   curr_in : spx_word16_t;

   interp : Array[0..3] of  spx_word16_t;
   accum : Array[0..3] of  spx_word32_t = (0,0,0,0);
begin
   N := st^.filt_len;
   out_sample := 0;
   last_sample := st^.last_sample[channel_index];
   samp_frac_num := st^.samp_frac_num[channel_index];
   out_stride := st^.out_stride;
   int_advance := st^.int_advance;
   frac_advance := st^.frac_advance;
   den_rate := st^.den_rate;

   while (not ((last_sample >= spx_int32_t(in_len^)) or (out_sample >= spx_int32_t(out_len^)))) do
   begin
      iptr := @( inp[last_sample] );

      offset := samp_frac_num*st^.oversample div st^.den_rate;
      frac := (single((samp_frac_num*st^.oversample) mod st^.den_rate))/st^.den_rate;

      for j:=0 to N-1 do
      begin
        curr_in := iptr[j];
        accum[0] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset-2]);
        accum[1] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset-1]);
        accum[2] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset]);
        accum[3] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset+1]);
      end;

      cubic_coef(frac, interp);
      sum := MULT16_32_Q15(interp[0],accum[0]) + MULT16_32_Q15(interp[1],accum[1]) + MULT16_32_Q15(interp[2],accum[2]) + MULT16_32_Q15(interp[3],accum[3]);

      outp[out_stride * out_sample] := sum;
      Inc(out_sample);
      last_sample += int_advance;
      samp_frac_num += frac_advance;
      if (samp_frac_num >= den_rate) then
      begin
         samp_frac_num -= den_rate;
         Inc(last_sample);
      end;
   end;

   st^.last_sample[channel_index] := last_sample;
   st^.samp_frac_num[channel_index] := samp_frac_num;
   Result := out_sample;
end;

{ This is the same as the previous function, except with a double-precision accumulator }
function resampler_basic_interpolate_double(st : pSpeexResamplerState;
                                               channel_index : spx_uint32_t;
                                               const inp : pspx_word16_t;
                                               in_len : pspx_uint32_t;
                                               outp : pspx_word16_t;
                                               out_len : pspx_uint32_t) : Integer;
var
  N, out_sample, last_sample, out_stride, int_advance, frac_advance,
    offset, j : Integer;
  samp_frac_num, den_rate : spx_uint32_t;
  frac : spx_word16_t;
  sum : spx_word32_t;
  iptr : pspx_word16_t;
  interp : Array [0..3] of single;
  accum : Array [0..3] of single = (0.0,0.0,0.0,0.0);
  curr_in : Double;
begin
   N := st^.filt_len;
   out_sample := 0;
   last_sample := st^.last_sample[channel_index];
   samp_frac_num := st^.samp_frac_num[channel_index];

   out_stride := st^.out_stride;
   int_advance := st^.int_advance;
   frac_advance := st^.frac_advance;
   den_rate := st^.den_rate;

   while (not ((last_sample >= spx_int32_t(in_len^)) or (out_sample >= spx_int32_t(out_len^)))) do
   begin
      iptr := @(inp[last_sample]);

      offset := samp_frac_num*st^.oversample div st^.den_rate;
      frac := (Single((samp_frac_num*st^.oversample) mod st^.den_rate))/st^.den_rate;

      for j:=0 to N-1 do
      begin
        curr_in := iptr[j];
        accum[0] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset-2]);
        accum[1] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset-1]);
        accum[2] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset]);
        accum[3] += MULT16_16(curr_in,st^.sinc_table[4+(j+1)*st^.oversample-offset+1]);
      end;

      cubic_coef(frac, interp);
      sum := MULT16_32_Q15(interp[0],accum[0]) + MULT16_32_Q15(interp[1],accum[1]) + MULT16_32_Q15(interp[2],accum[2]) + MULT16_32_Q15(interp[3],accum[3]);


      outp[out_stride * out_sample] := sum;
      Inc(out_sample);
      last_sample += int_advance;
      samp_frac_num += frac_advance;
      if (samp_frac_num >= den_rate) then
      begin
         samp_frac_num -= den_rate;
         Inc(last_sample);
      end;
   end;

   st^.last_sample[channel_index] := last_sample;
   st^.samp_frac_num[channel_index] := samp_frac_num;
   Result := out_sample;
end;

{  This resampler is used to produce zero output in situations where memory
   for the filter could not be allocated.  The expected numbers of input and
   output samples are still processed so that callers failing to check error
   codes are not surprised, possibly getting into infinite loops. }
function resampler_basic_zero(st : pSpeexResamplerState;
                                               channel_index : spx_uint32_t;
                                               const inp : pspx_word16_t;
                                               in_len : pspx_uint32_t;
                                               outp : pspx_word16_t;
                                               out_len : pspx_uint32_t) : integer;
var
  out_sample, last_sample, out_stride, int_advance, frac_advance : integer;
  samp_frac_num, den_rate : spx_uint32_t;
begin
   out_sample := 0;
   last_sample := st^.last_sample[channel_index];
   samp_frac_num := st^.samp_frac_num[channel_index];
   out_stride := st^.out_stride;
   int_advance := st^.int_advance;
   frac_advance := st^.frac_advance;
   den_rate := st^.den_rate;

   while (not ((last_sample >= spx_int32_t(in_len^)) or (out_sample >= spx_int32_t(out_len^)))) do
   begin
      outp[out_stride * out_sample] := 0;
      Inc(out_sample);
      last_sample += int_advance;
      samp_frac_num += frac_advance;
      if (samp_frac_num >= den_rate) then
      begin
         samp_frac_num -= den_rate;
         Inc(last_sample);
      end;
   end;

   st^.last_sample[channel_index] := last_sample;
   st^.samp_frac_num[channel_index] := samp_frac_num;
   result := out_sample;
end;

function multiply_frac(res : pspx_uint32_t;
                              value, num, den : spx_uint32_t) : integer;
var
  major, remain : spx_uint32_t;
begin
   major := value div den;
   remain := value mod den;
   { TODO: Could use 64 bits operation to check for overflow. But only guaranteed in C99+ }
   if ((remain > (UINT32_MAX div num)) or (major > (UINT32_MAX div num))
       or ((major * num) > (UINT32_MAX - remain * num div den))) then
      Exit( RESAMPLER_ERR_OVERFLOW );
   res^ := remain * num div den + major * num;
   result := RESAMPLER_ERR_SUCCESS;
end;


function update_filter(st : pSpeexResamplerState) : integer;
var
   old_length, old_alloc_size, min_sinc_table_length,
   min_alloc_size : spx_uint32_t;
   use_direct : Boolean;
   mem, sinc_table : pspx_word16_t;
   i, j, olen, old_magic : spx_uint32_t;
   ii : integer;
begin
   old_length := st^.filt_len;
   old_alloc_size := st^.mem_alloc_size;

   st^.int_advance := st^.num_rate div st^.den_rate;
   st^.frac_advance := st^.num_rate mod st^.den_rate;
   st^.oversample := quality_map[st^.quality].oversample;
   st^.filt_len := quality_map[st^.quality].base_length;

   try
   if (st^.num_rate > st^.den_rate) then
   begin
      { down-sampling }
      st^.cutoff := quality_map[st^.quality].downsample_bandwidth * st^.den_rate / st^.num_rate;
      if (multiply_frac(@(st^.filt_len),
                        st^.filt_len,
                        st^.num_rate,
                        st^.den_rate) <> RESAMPLER_ERR_SUCCESS) then
                  raise Exception.Create('update_filter : 1');
      { Round up to make sure we have a multiple of 8 for SSE }
      st^.filt_len := ((st^.filt_len-1) and (not $7))+8;
      if ((2*st^.den_rate) < st^.num_rate) then
         st^.oversample := st^.oversample shr 1;
      if ((4*st^.den_rate) < st^.num_rate) then
         st^.oversample := st^.oversample shr 1;
      if ((8*st^.den_rate) < st^.num_rate) then
         st^.oversample := st^.oversample shr 1;
      if ((16*st^.den_rate) < st^.num_rate) then
         st^.oversample := st^.oversample shr 1;
      if (st^.oversample < 1) then
         st^.oversample := 1;
   end else begin
      { up-sampling }
      st^.cutoff := quality_map[st^.quality].upsample_bandwidth;
   end;

   use_direct := true;
   if ((INT_MAX div sizeof(spx_word16_t) div st^.den_rate) < st^.filt_len) then
      raise Exception.Create('update_filter : 2');
{
//as alt
   /* Choose the resampling type that requires the least amount of memory */
   use_direct = st^.filt_len*st^.den_rate <= st^.filt_len*st^.oversample+8
                and INT_MAX/sizeof(spx_word16_t)/st^.den_rate >= st^.filt_len;
}
   if (use_direct) then
   begin
      min_sinc_table_length := st^.filt_len*st^.den_rate;
   end else begin
      if (((INT_MAX div sizeof(spx_word16_t)-8) div st^.oversample) < st^.filt_len) then
         raise Exception.Create('update_filter : 3');

      min_sinc_table_length := st^.filt_len*st^.oversample+8;
   end;
   if (st^.sinc_table_length < min_sinc_table_length) then
   begin
      sinc_table := pspx_word16_t(reallocmem(st^.sinc_table,min_sinc_table_length*sizeof(spx_word16_t)));
      if not Assigned(sinc_table) then
        raise Exception.Create('update_filter : 4');

      st^.sinc_table := sinc_table;
      st^.sinc_table_length := min_sinc_table_length;
   end;
   if (use_direct) then
   begin
      i := 0;
      while i<st^.den_rate do
      begin
         j := 0;
         while (j<st^.filt_len) do
         begin
            st^.sinc_table[i*st^.filt_len+j] := sinc(st^.cutoff,((j-spx_int32_t(st^.filt_len)/2+1)-(single(i))/st^.den_rate), st^.filt_len, quality_map[st^.quality].window_func);
            Inc(j);
         end;
         Inc(i);
      end;

      if (st^.quality>8) then
         st^.resampler_ptr := @resampler_basic_direct_double
      else
         st^.resampler_ptr := @resampler_basic_direct_single;

   end else begin
      ii := -4;
      while ii < spx_int32_t(st^.oversample*st^.filt_len+4) do
      begin
         st^.sinc_table[ii+4] := sinc(st^.cutoff,(ii/single(st^.oversample) - st^.filt_len/2), st^.filt_len, quality_map[st^.quality].window_func);
         Inc(ii);
      end;

      if (st^.quality>8) then
         st^.resampler_ptr := @resampler_basic_interpolate_double
      else
         st^.resampler_ptr := @resampler_basic_interpolate_single;
   end;

   { Here's the place where we update the filter memory to take into account
      the change in filter length. It's probably the messiest part of the code
      due to handling of lots of corner cases. }

   { Adding buffer_size to filt_len won't overflow here because filt_len
      could be multiplied by sizeof(spx_word16_t) above. }
   min_alloc_size := st^.filt_len-1 + st^.buffer_size;
   if (min_alloc_size > st^.mem_alloc_size) then
   begin
      if ((INT_MAX div sizeof(spx_word16_t) div st^.nb_channels) < min_alloc_size) then
          raise Exception.Create('update_filter : 5')
      else
      begin
        mem := pspx_word16_t(reallocmem(st^.mem, st^.nb_channels*min_alloc_size * sizeof(spx_word16_t)));
        if not Assigned(mem) then
            raise Exception.Create('update_filter : 6');
      end;

      st^.mem := mem;
      st^.mem_alloc_size := min_alloc_size;
   end;
   if (not st^.started) then
   begin
      i := 0;
      while (i < st^.nb_channels*st^.mem_alloc_size) do
      begin
         st^.mem[i] := 0;
         Inc(i);
      end;
   end else
   if (st^.filt_len > old_length) then
   begin
      { Increase the filter length }
      i := st^.nb_channels;
      while (i > 0) do
      begin
         olen := old_length;
         {if (st^.magic_samples[i])}
         begin
            // Try and remove the magic samples as if nothing had happened
            // FIXME: This is wrong but for now we need it to avoid going over the array bounds
            olen := old_length + 2*st^.magic_samples[i];
            j := old_length-1+st^.magic_samples[i];
            while j > 0 do
            begin
               st^.mem[i*st^.mem_alloc_size+j+st^.magic_samples[i]] := st^.mem[i*old_alloc_size+j];
               Dec(j);
            end;

            if st^.magic_samples[i] > 0 then
            for j := 0 to st^.magic_samples[i]-1 do
               st^.mem[i*st^.mem_alloc_size+j] := 0;

            st^.magic_samples[i] := 0;
         end;
         if (st^.filt_len > olen) then
         begin
            // If the new filter length is still bigger than the "augmented" length
            // Copy data going backward
            j := 0;
            while j < olen-1 do
            begin
               st^.mem[i*st^.mem_alloc_size+(st^.filt_len-2-j)] := st^.mem[i*st^.mem_alloc_size+(olen-2-j)];
               inc(j);
            end;
            // Then put zeros for lack of anything better
            while (j<st^.filt_len-1) do
            begin
               st^.mem[i*st^.mem_alloc_size+(st^.filt_len-2-j)] := 0;
               Inc(j);
            end;
            // Adjust last_sample
            st^.last_sample[i] += (st^.filt_len - olen) div 2;
         end else begin
            // Put back some of the magic!
            st^.magic_samples[i] := (olen - st^.filt_len) div 2;
            j := 0;
            while j<st^.filt_len-1+st^.magic_samples[i] do
            begin
               st^.mem[i*st^.mem_alloc_size+j] := st^.mem[i*st^.mem_alloc_size+j+st^.magic_samples[i]];
               Inc(j);
            end;
         end;
         Dec(i);
      end;
   end else
   if (st^.filt_len < old_length) then
   begin
      { Reduce filter length, this a bit tricky. We need to store some of the memory as "magic"
         samples so they can be used directly as input the next time(s) }

      while i<st^.nb_channels do
      begin
         old_magic := st^.magic_samples[i];
         st^.magic_samples[i] := (old_length - st^.filt_len) div 2;
         // We must copy some of the memory that's no longer used
         // Copy data going backward
         j := 0;
         while (j<(st^.filt_len-1+st^.magic_samples[i]+old_magic)) do
         begin
            st^.mem[i*st^.mem_alloc_size+j] := st^.mem[i*st^.mem_alloc_size+j+st^.magic_samples[i]];
            Inc(j);
         end;
         st^.magic_samples[i] += old_magic;
         Inc(i);
      end;
   end;
   result := RESAMPLER_ERR_SUCCESS;
  except
     st^.resampler_ptr := @resampler_basic_zero;
     { st^.mem may still contain consumed input samples for the filter.
        Restore filt_len so that filt_len - 1 still points to the position after
        the last of these samples. }
     st^.filt_len := old_length;
     result := RESAMPLER_ERR_ALLOC_FAILED;
  end;
end;

function speex_resampler_init(nb_channels : spx_uint32_t;
                              in_rate : spx_uint32_t;
                              out_rate : spx_uint32_t;
                              quality : integer;
                              err : pinteger) : pSpeexResamplerState;
begin
   Result := speex_resampler_init_frac(nb_channels, in_rate, out_rate, in_rate, out_rate, quality, err);
end;

function speex_resampler_init_frac(nb_channels : spx_uint32_t;
                              ratio_num, ratio_den, in_rate, out_rate : spx_uint32_t;
                              quality : integer;
                              err : pinteger) : pSpeexResamplerState;
var
   st : pSpeexResamplerState;
   filter_err : integer;
begin
   if (nb_channels = 0) or (ratio_num = 0) or (ratio_den = 0) or (quality > 10) or (quality < 0) then
   begin
      if Assigned(err) then
         err^ := RESAMPLER_ERR_INVALID_ARG;
      Exit(nil);
   end;
   st := pSpeexResamplerState(allocmem(sizeof(SpeexResamplerState)));
   if not Assigned(st) then
   begin
      if Assigned(err) then
         err^ := RESAMPLER_ERR_ALLOC_FAILED;
      Exit(nil);
   end;
   st^.initialised := false;
   st^.started := false;
   st^.in_rate := 0;
   st^.out_rate := 0;
   st^.num_rate := 0;
   st^.den_rate := 0;
   st^.quality := -1;
   st^.sinc_table_length := 0;
   st^.mem_alloc_size := 0;
   st^.filt_len := 0;
   st^.mem := nil;
   st^.resampler_ptr := nil;

   st^.cutoff := 1.0;
   st^.nb_channels := nb_channels;
   st^.in_stride := 1;
   st^.out_stride := 1;

   st^.buffer_size := 160;

   // Per channel data
   try
     st^.last_sample := pspx_int32_t(AllocMem(nb_channels*sizeof(spx_int32_t)));
     if not Assigned(st^.last_sample) then
        raise Exception.Create('speex_resampler_init_frac : 1');
     st^.magic_samples := pspx_uint32_t(AllocMem(nb_channels*sizeof(spx_uint32_t)));
     if not assigned(st^.magic_samples) then
        raise Exception.Create('speex_resampler_init_frac : 2');
     st^.samp_frac_num := pspx_uint32_t(AllocMem(nb_channels*sizeof(spx_uint32_t)));
     if not Assigned(st^.samp_frac_num) then
        raise Exception.Create('speex_resampler_init_frac : 3');

     speex_resampler_set_quality(st, quality);
     speex_resampler_set_rate_frac(st, ratio_num, ratio_den, in_rate, out_rate);

     filter_err := update_filter(st);
     if (filter_err = RESAMPLER_ERR_SUCCESS) then
     begin
        st^.initialised := true;
     end else begin
        speex_resampler_destroy(st);
        st := nil;
     end;
     if Assigned(err) then
        err^ := filter_err;

     result := st;
   except
     on e: Exception do
     begin
        if Assigned(err) then
           err^ := RESAMPLER_ERR_ALLOC_FAILED;
        speex_resampler_destroy(st);
        Result := nil;
     end;
   end;
end;

procedure speex_resampler_destroy(st : pSpeexResamplerState);
begin
   with st^ do
   if Assigned(st) then
   begin
     if Assigned(mem) then
       freemem(mem);
     if Assigned(sinc_table) then
       freemem(sinc_table);
     if Assigned(last_sample) then
       freemem(last_sample);
     if Assigned(magic_samples) then
       freemem(magic_samples);
     if Assigned(samp_frac_num) then
       freemem(samp_frac_num);
     freemem(st);
   end;
end;

function speex_resampler_process_native(st : pSpeexResamplerState;
                              channel_index : spx_uint32_t;
                              in_len : pspx_uint32_t;
                              outp : pspx_word16_t;
                              out_len : pspx_uint32_t) : integer;
var
   j, N, out_sample : integer;
   mem : pspx_word16_t;

   ilen : spx_uint32_t;
begin
   j := 0;
   N := st^.filt_len;
   out_sample := 0;
   mem := st^.mem + channel_index * st^.mem_alloc_size;

   st^.started := true;

   { Call the right resampler through the function ptr }
   out_sample := st^.resampler_ptr(st, channel_index, mem, in_len, outp, out_len);

   if (st^.last_sample[channel_index] < spx_int32_t(in_len^)) then
      in_len^ := st^.last_sample[channel_index];
   out_len^ := out_sample;
   st^.last_sample[channel_index] -= in_len^;

   ilen := in_len^;

   j := 0;
   while j < (N-1) do
   begin
      mem[j] := mem[j+ilen];
      inc(j);
   end;

   result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_magic(st : pSpeexResamplerState;
                               channel_index : spx_uint32_t;
                               outp : ppspx_word16_t;
                               out_len : spx_uint32_t) : integer;
var
   N : integer;
   tmp_in_len, i : spx_uint32_t;
   mem : pspx_word16_t;
begin
   tmp_in_len := st^.magic_samples[channel_index];
   mem := st^.mem + channel_index * st^.mem_alloc_size;
   N := st^.filt_len;

   speex_resampler_process_native(st, channel_index, @tmp_in_len, outp^, @out_len);

   st^.magic_samples[channel_index] -= tmp_in_len;

   { If we couldn't process all "magic" input samples, save the rest for next time }
   if (st^.magic_samples[channel_index] <> 0) then
   begin
      for i:=0 to st^.magic_samples[channel_index]-1 do
         mem[N-1+i] := mem[N-1+i+tmp_in_len];
   end;
   outp^ += out_len*st^.out_stride;
   Result := out_len;
end;

function speex_resampler_process_float(st : pSpeexResamplerState;
                              channel_index : spx_uint32_t;
                              const inp : PSingle;
                              in_len : pspx_uint32_t;
                              outp : pSingle;
                              out_len : pspx_uint32_t) : Integer;
var
   j, filt_offs, istride : integer;
   ilen, olen, xlen, ichunk, ochunk : spx_uint32_t;
   x : pspx_word16_t;

   inpv : PSingle;
begin
   inpv := inp;
   ilen := in_len^;
   olen := out_len^;
   x := st^.mem + channel_index * st^.mem_alloc_size;
   filt_offs := st^.filt_len - 1;
   xlen := st^.mem_alloc_size - filt_offs;
   istride := st^.in_stride;

   if (st^.magic_samples[channel_index] <> 0) then
      olen -= speex_resampler_magic(st, channel_index, @outp, olen);
   if (st^.magic_samples[channel_index] = 0) then
   begin
      while ((ilen > 0) and (olen > 0)) do
      begin
        if (ilen > xlen) then
         ichunk := xlen else
         ichunk := ilen;
        ochunk := olen;

        if Assigned(inpv) then
        begin
           j := 0;
           while j < ichunk do
           begin
              x[j+filt_offs] := inpv[j*istride];
              Inc(j);
           end;
        end else begin
          j := 0;
          while j < ichunk do
          begin
             x[j+filt_offs] := 0;
             Inc(j);
          end;
        end;
        speex_resampler_process_native(st, channel_index, @ichunk, outp, @ochunk);
        ilen -= ichunk;
        olen -= ochunk;
        outp += ochunk * st^.out_stride;
        if Assigned(inpv) then
           inpv += ichunk * istride;
      end;
   end;
   in_len^ -= ilen;
   out_len^ -= olen;
   if st^.resampler_ptr = @resampler_basic_zero then
     Result := RESAMPLER_ERR_ALLOC_FAILED
   else
     Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_process_int(st : pSpeexResamplerState;
                              channel_index : spx_uint32_t;
                              const inp : pspx_int16_t;
                              in_len : pspx_uint32_t;
                              outp : pspx_int16_t;
                              out_len : pspx_uint32_t) : Integer;
var
  j, istride_save, ostride_save : integer;
  ilen, olen, xlen, ichunk, ochunk, omagic : spx_uint32_t;
  x : pspx_word16_t;
  ylen : Cardinal;
  ystack : pspx_word16_t;
  y : pspx_word16_t;
  inpv :  pspx_int16_t;
begin
  inpv := inp;
  istride_save := st^.in_stride;
  ostride_save := st^.out_stride;
  ilen := in_len^;
  olen := out_len^;
  x := st^.mem + channel_index * st^.mem_alloc_size;
  xlen := st^.mem_alloc_size - (st^.filt_len - 1);
  if olen < FIXED_STACK_ALLOC then
    ylen := olen else
    ylen := FIXED_STACK_ALLOC;

  ystack := AllocMem(ylen * sizeof(spx_word16_t));
  try
    st^.out_stride := 1;

    while ((ilen <> 0) and (olen <> 0)) do
    begin
      y := ystack;

      if ilen > xlen then
         ichunk := xlen else
         ichunk := ilen;
      if olen > ylen then
         ochunk := ylen else
         ochunk := olen;
      omagic := 0;

      if (st^.magic_samples[channel_index] <> 0) then
      begin
        omagic := speex_resampler_magic(st, channel_index, @y, ochunk);
        ochunk -= omagic;
        olen -= omagic;
      end;
      if (st^.magic_samples[channel_index] = 0) then
      begin
        if Assigned(inpv) then
        begin
          j := 0;
          while j < ichunk do
          begin
            x[j+st^.filt_len-1] := inpv[j*istride_save];
            Inc(j);
          end;
        end else begin
          j := 0;
          while j < ichunk do
          begin
            x[j+st^.filt_len-1] := 0;
            Inc(j);
          end;
        end;

        speex_resampler_process_native(st, channel_index, @ichunk, y, @ochunk);
      end else begin
        ichunk := 0;
        ochunk := 0;
      end;

      j := 0;
      while j<(ochunk+omagic) do
      begin
        outp[j*ostride_save] := WORD2INT(ystack[j]);
        Inc(j);
      end;

      ilen -= ichunk;
      olen -= ochunk;
      outp += (ochunk+omagic) * ostride_save;
      if assigned(inpv) then
        inpv += ichunk * istride_save;
    end;
    st^.out_stride := ostride_save;
    in_len^ -= ilen;
    out_len^ -= olen;
  finally
    FreeMem(ystack);
  end;

  if st^.resampler_ptr = @resampler_basic_zero then
   Result := RESAMPLER_ERR_ALLOC_FAILED else
   Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_process_interleaved_float(st : pSpeexResamplerState;
                              const inp : PSingle;
                              in_len : pspx_uint32_t;
                              outp : pSingle;
                              out_len : pspx_uint32_t) : Integer;
var
  i, bak_out_len, bak_in_len : spx_uint32_t;
  istride_save, ostride_save : integer;
begin
  bak_out_len := out_len^;
  bak_in_len := in_len^;

  istride_save := st^.in_stride;
  ostride_save := st^.out_stride;
  st^.in_stride := st^.nb_channels;
  st^.out_stride := st^.nb_channels;
  i := 0;
  while i<st^.nb_channels do
  begin
     out_len^ := bak_out_len;
     in_len^ := bak_in_len;
     if Assigned(inp) then
        speex_resampler_process_float(st, i, @(inp[i]), in_len, @(outp[i]), out_len)
     else
        speex_resampler_process_float(st, i, nil, in_len, @(outp[i]), out_len);
     Inc(i);
  end;
  st^.in_stride := istride_save;
  st^.out_stride := ostride_save;
  if st^.resampler_ptr = @resampler_basic_zero then
    Result := RESAMPLER_ERR_ALLOC_FAILED else
    Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_process_interleaved_int(st : pSpeexResamplerState;
  const inp : pspx_int16_t; in_len : pspx_uint32_t; outp : pspx_int16_t;
  out_len : pspx_uint32_t) : Integer;
var
  i, bak_out_len, bak_in_len : spx_uint32_t;
  istride_save, ostride_save : integer;
begin
  bak_out_len := out_len^;
  bak_in_len := in_len^;
  istride_save := st^.in_stride;
  ostride_save := st^.out_stride;
  st^.in_stride := st^.nb_channels;
  st^.out_stride := st^.nb_channels;
  i := 0;
  while i<st^.nb_channels do
  begin
     out_len^ := bak_out_len;
     in_len^ := bak_in_len;
     if Assigned(inp) then
        speex_resampler_process_int(st, i, @(inp[i]), in_len, @(outp[i]), out_len)
     else
        speex_resampler_process_int(st, i, nil, in_len, @(outp[i]), out_len);
     Inc(I);
  end;
  st^.in_stride := istride_save;
  st^.out_stride := ostride_save;
  if st^.resampler_ptr = @resampler_basic_zero then
    Result := RESAMPLER_ERR_ALLOC_FAILED else
    Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_set_rate(st : pSpeexResamplerState;
                              in_rate, out_rate : spx_uint32_t) : integer;
begin
   result := speex_resampler_set_rate_frac(st, in_rate, out_rate, in_rate, out_rate);
end;

procedure speex_resampler_get_rate(st : pSpeexResamplerState;
                              in_rate : pspx_uint32_t;
                              out_rate : pspx_uint32_t);
begin
   if Assigned(in_rate) then in_rate^ := (st)^.in_rate;
   if Assigned(out_rate) then out_rate^ := (st)^.out_rate;
end;

function compute_gcd(a, b : spx_uint32_t) : spx_uint32_t;
var
  temp : spx_uint32_t;
begin
   while (b <> 0) do
   begin
      temp := a;
      a := b;
      b := temp mod b;
   end;
   Result := a;
end;

function speex_resampler_set_rate_frac(st : pSpeexResamplerState;
                              ratio_num, ratio_den,
                              in_rate, out_rate : spx_uint32_t) : integer;
var
   fact, old_den, i : spx_uint32_t;
begin
   if (ratio_num = 0) or (ratio_den = 0) then
      Exit( RESAMPLER_ERR_INVALID_ARG );

   if (st^.in_rate = in_rate) and (st^.out_rate = out_rate) and
      (st^.num_rate = ratio_num) and (st^.den_rate = ratio_den) then
      Exit( RESAMPLER_ERR_SUCCESS );

   old_den := st^.den_rate;
   st^.in_rate := in_rate;
   st^.out_rate := out_rate;
   st^.num_rate := ratio_num;
   st^.den_rate := ratio_den;

   fact := compute_gcd(st^.num_rate, st^.den_rate);

   st^.num_rate := st^.num_rate div fact;
   st^.den_rate := st^.den_rate div fact;

   if (old_den > 0) then
   begin
      for i := 0 to st^.nb_channels-1 do
      begin
         if (multiply_frac(@(st^.samp_frac_num[i]),st^.samp_frac_num[i],st^.den_rate,old_den) <> RESAMPLER_ERR_SUCCESS) then
            Exit( RESAMPLER_ERR_OVERFLOW );
         // Safety net
         if (st^.samp_frac_num[i] >= st^.den_rate) then
            st^.samp_frac_num[i] := st^.den_rate-1;
      end;
   end;

   if (st^.initialised) then
      Exit( update_filter(st) );
   Result := RESAMPLER_ERR_SUCCESS;
end;

procedure speex_resampler_get_ratio(st : pSpeexResamplerState;
                              ratio_num, ratio_den : pspx_uint32_t);
begin
   ratio_num^ := st^.num_rate;
   ratio_den^ := st^.den_rate;
end;

function speex_resampler_set_quality(st : pSpeexResamplerState; quality : integer) : integer;
begin
   if (quality > 10) or (quality < 0) then
      Exit( RESAMPLER_ERR_INVALID_ARG );
   if (st^.quality = quality) then
      Exit( RESAMPLER_ERR_SUCCESS );
   st^.quality := quality;
   if (st^.initialised) then
      Exit( update_filter(st) );
   Result := RESAMPLER_ERR_SUCCESS;
end;

procedure speex_resampler_get_quality(st : pSpeexResamplerState;
                                      quality : Pinteger);
begin
   quality^ := st^.quality;
end;

procedure speex_resampler_set_input_stride(st : pSpeexResamplerState;
                                           stride : spx_uint32_t);
begin
   st^.in_stride := stride;
end;

procedure speex_resampler_get_input_stride(st : pSpeexResamplerState;
                                           stride : pspx_uint32_t);
begin
   stride^ := st^.in_stride;
end;

procedure speex_resampler_set_output_stride(st : pSpeexResamplerState;
                                            stride : spx_uint32_t);
begin
   st^.out_stride := stride;
end;

procedure speex_resampler_get_output_stride(st : pSpeexResamplerState;
                                                 stride : pspx_uint32_t);
begin
   stride^ := st^.out_stride;
end;

function speex_resampler_get_input_latency(st : pSpeexResamplerState) : integer;
begin
  result := st^.filt_len div 2;
end;

function speex_resampler_get_output_latency(st : pSpeexResamplerState) : integer;
begin
  result := ((st^.filt_len div 2) * st^.den_rate + (st^.num_rate shr 1)) div st^.num_rate;
end;

function speex_resampler_skip_zeros(st : pSpeexResamplerState) : integer;
var
   i : spx_uint32_t;
begin
   for i:=0 to st^.nb_channels-1 do
      st^.last_sample[i] := st^.filt_len div 2;
   Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_reset_mem(st : pSpeexResamplerState) : integer;
var
   i, v : spx_uint32_t;
begin
   for i:=0 to st^.nb_channels-1 do
   begin
      st^.last_sample[i] := 0;
      st^.magic_samples[i] := 0;
      st^.samp_frac_num[i] := 0;
   end;
   v := st^.nb_channels*(st^.filt_len-1);
   if v > 0 then
   for i:=0 to v-1 do
      st^.mem[i] := 0;
   Result := RESAMPLER_ERR_SUCCESS;
end;

function speex_resampler_strerror(err : integer) : String;
begin
   case err of
      RESAMPLER_ERR_SUCCESS:
         Result := 'Success.';
      RESAMPLER_ERR_ALLOC_FAILED:
         Result := 'Memory allocation failed.';
      RESAMPLER_ERR_BAD_STATE:
         Result := 'Bad resampler state.';
      RESAMPLER_ERR_INVALID_ARG:
         Result := 'Invalid argument.';
      RESAMPLER_ERR_PTR_OVERLAP:
         Result := 'Input and output buffers overlap.';
      else
         Result := 'Unknown error. Bad error code or strange version mismatch.';
   end;
end;

{$ENDREGION}

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

