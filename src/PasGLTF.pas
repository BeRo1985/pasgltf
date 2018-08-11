(******************************************************************************
 *                                 PasGLTF                                    *
 ******************************************************************************
 *                          Version 2018-06-05-16-18                          *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
 *    http://github.com/BeRo1985/pasGLTF                                      *
 * 4. Write code which's compatible with newer modern Delphi versions and     *
 *    FreePascal >= 3.0.0                                                     *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 *                                                                            *
 ******************************************************************************)
unit PasGLTF;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$define CAN_INLINE}
 {$define HAS_ADVANCED_RECORDS}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$undef CAN_INLINE}
 {$undef HAS_ADVANCED_RECORDS}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
   {$define CAN_INLINE}
   {$define HAS_ADVANCED_RECORDS}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_SINGLE}
 {$error No single floating point precision}
{$endif}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$scopedenums on}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     Generics.Collections,
     PasJSON;

type PPPasGLTFInt8=^PPasGLTFInt8;
     PPasGLTFInt8=^TPasGLTFInt8;
     TPasGLTFInt8={$ifdef fpc}Int8{$else}shortint{$endif};

     PPPasGLTFUInt8=^PPasGLTFUInt8;
     PPasGLTFUInt8=^TPasGLTFUInt8;
     TPasGLTFUInt8={$ifdef fpc}UInt8{$else}byte{$endif};

     PPPasGLTFUInt8Array=^PPasGLTFUInt8Array;
     PPasGLTFUInt8Array=^TPasGLTFUInt8Array;
     TPasGLTFUInt8Array=array[0..65535] of TPasGLTFUInt8;

     TPasGLTFUInt8DynamicArray=array of TPasGLTFUInt8;

     PPPasGLTFInt16=^PPasGLTFInt16;
     PPasGLTFInt16=^TPasGLTFInt16;
     TPasGLTFInt16={$ifdef fpc}Int16{$else}smallint{$endif};

     PPPasGLTFUInt16=^PPasGLTFUInt16;
     PPasGLTFUInt16=^TPasGLTFUInt16;
     TPasGLTFUInt16={$ifdef fpc}UInt16{$else}word{$endif};

     PPPasGLTFInt32=^PPasGLTFInt32;
     PPasGLTFInt32=^TPasGLTFInt32;
     TPasGLTFInt32={$ifdef fpc}Int32{$else}longint{$endif};

     PPPasGLTFUInt32=^PPasGLTFUInt32;
     PPasGLTFUInt32=^TPasGLTFUInt32;
     TPasGLTFUInt32={$ifdef fpc}UInt32{$else}longword{$endif};

     PPPasGLTFInt64=^PPasGLTFInt64;
     PPasGLTFInt64=^TPasGLTFInt64;
     TPasGLTFInt64=Int64;

     PPPasGLTFUInt64=^PPasGLTFUInt64;
     PPasGLTFUInt64=^TPasGLTFUInt64;
     TPasGLTFUInt64=UInt64;

     PPPasGLTFChar=^PAnsiChar;
     PPasGLTFChar=PAnsiChar;
     TPasGLTFChar=AnsiChar;

     PPPasGLTFRawByteChar=^PAnsiChar;
     PPasGLTFRawByteChar=PAnsiChar;
     TPasGLTFRawByteChar=AnsiChar;

     PPPasGLTFUTF16Char=^PWideChar;
     PPasGLTFUTF16Char=PWideChar;
     TPasGLTFUTF16Char=WideChar;

     PPPasGLTFPointer=^PPasGLTFPointer;
     PPasGLTFPointer=^TPasGLTFPointer;
     TPasGLTFPointer=Pointer;

     PPPasGLTFPointers=^PPasGLTFPointers;
     PPasGLTFPointers=^TPasGLTFPointers;
     TPasGLTFPointers=array[0..65535] of TPasGLTFPointer;

     PPPasGLTFVoid=^PPasGLTFVoid;
     PPasGLTFVoid=TPasGLTFPointer;

     PPPasGLTFFloat=^PPasGLTFFloat;
     PPasGLTFFloat=^TPasGLTFFloat;
     TPasGLTFFloat=Single;

     TPasGLTFFloatDynamicArray=array of TPasGLTFFloat;

     PPPasGLTFDouble=^PPasGLTFDouble;
     PPasGLTFDouble=^TPasGLTFDouble;
     TPasGLTFDouble=Double;

     PPPasGLTFPtrUInt=^PPasGLTFPtrUInt;
     PPPasGLTFPtrInt=^PPasGLTFPtrInt;
     PPasGLTFPtrUInt=^TPasGLTFPtrUInt;
     PPasGLTFPtrInt=^TPasGLTFPtrInt;
{$ifdef fpc}
     TPasGLTFPtrUInt=PtrUInt;
     TPasGLTFPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TPasGLTFPtrUInt=NativeUInt;
     TPasGLTFPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TPasGLTFPtrUInt=uint64;
     TPasGLTFPtrInt=int64;
{$else}
     TPasGLTFPtrUInt=longword;
     TPasGLTFPtrInt=longint;
{$endif}
{$endif}

     PPPasGLTFSizeUInt=^PPasGLTFSizeUInt;
     PPasGLTFSizeUInt=^TPasGLTFSizeUInt;
     TPasGLTFSizeUInt=TPasGLTFPtrUInt;

     PPPasGLTFSizeInt=^PPasGLTFSizeInt;
     PPasGLTFSizeInt=^TPasGLTFSizeInt;
     TPasGLTFSizeInt=TPasGLTFPtrInt;

     PPPasGLTFNativeUInt=^PPasGLTFNativeUInt;
     PPasGLTFNativeUInt=^TPasGLTFNativeUInt;
     TPasGLTFNativeUInt=TPasGLTFPtrUInt;

     PPPasGLTFNativeInt=^PPasGLTFNativeInt;
     PPasGLTFNativeInt=^TPasGLTFNativeInt;
     TPasGLTFNativeInt=TPasGLTFPtrInt;

     PPPasGLTFSize=^PPasGLTFSizeUInt;
     PPasGLTFSize=^TPasGLTFSizeUInt;
     TPasGLTFSize=TPasGLTFPtrUInt;

     PPPasGLTFPtrDiff=^PPasGLTFPtrDiff;
     PPasGLTFPtrDiff=^TPasGLTFPtrDiff;
     TPasGLTFPtrDiff=TPasGLTFPtrInt;

     PPPasGLTFRawByteString=^PPasGLTFRawByteString;
     PPasGLTFRawByteString=^TPasGLTFRawByteString;
     TPasGLTFRawByteString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

     PPPasGLTFUTF8String=^PPasGLTFUTF8String;
     PPasGLTFUTF8String=^TPasGLTFUTF8String;
     TPasGLTFUTF8String={$if declared(UTF8String)}UTF8String{$else}AnsiString{$ifend};

     PPPasGLTFUTF16String=^PPasGLTFUTF16String;
     PPasGLTFUTF16String=^TPasGLTFUTF16String;
     TPasGLTFUTF16String={$if declared(UnicodeString)}UnicodeString{$else}WideString{$ifend};

     EPasGLTF=class(Exception);

     EPasGLTFInvalidDocument=class(EPasGLTF);

     EPasGLTFInvalidBase64=class(EPasGLTF);

     TPasGLTF=class
      public
       type TBase64=class
             public
              const EncodingLookUpTable:array[0..63] of TPasGLTFRawByteChar=
                     (
                      'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
                      'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
                      'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
                      'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
                     );
                    DecodingLookUpTable:array[TPasGLTFRawByteChar] of TPasGLTFInt8=
                     (
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
                      52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
                      -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
                      15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
                      -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                      41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
                     );
             public
              class function Encode(const aData;const aDataLength:TPasGLTFSizeInt):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:array of TPasGLTFUInt8):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:TPasGLTFRawByteString):TPasGLTFRawByteString; overload; static;
              class function Encode(const aData:TStream):TPasGLTFRawByteString; overload; static;
              class function Decode(const aInput:TPasGLTFRawByteString;const aOutput:TStream):boolean; overload; static;
            end;
            TChunkHeader=packed record
             ChunkLength:TPasGLTFUInt32;
             ChunkType:TPasGLTFUInt32;
            end;
            PChunkHeader=^TChunkHeader;
            TGLBHeader=packed record
             Magic:TPasGLTFUInt32;
             Version:TPasGLTFUInt32;
             Length:TPasGLTFUInt32;
             JSONChunkHeader:TChunkHeader;
            end;
            TVector2=array[0..1] of TPasGLTFFloat;
            TVector3=array[0..2] of TPasGLTFFloat;
            TVector4=array[0..3] of TPasGLTFFloat;
            TMatrix=array[0..15] of TPasGLTFFloat;
       const ChunkHeaderSize=SizeOf(TChunkHeader);
             GLBHeaderSize=SizeOf(TGLBHeader);
             GLBHeaderMagicNativeEndianness=TPasGLTFUInt32($46546c67);
             GLBHeaderMagicOtherEndianness=TPasGLTFUInt32($676c5446);
             GLBChunkJSONNativeEndianness=TPasGLTFUInt32($4e4f534a);
             GLBChunkJSONOtherEndianness=TPasGLTFUInt32($4a534f4e);
             GLBChunkBinaryNativeEndianness=TPasGLTFUInt32($004e4942);
             GLBChunkBinaryOtherEndianness=TPasGLTFUInt32($42494e00);
             MimeTypeApplicationOctet='data:application/octet-stream;base64';
             MimeTypeImagePNG='data:image/png;base64';
             MimeTypeImageJPG='data:image/jpg;base64';
       type TDefaults=class
             public
              const AccessorNormalized=false;
                    MaterialAlphaCutoff=0.5;
                    MaterialDoubleSided=false;
                    IdentityScalar=1.0;
                    FloatSentinel=1e+4;
                    NullVector3:TVector3=(0.0,0.0,0.0);
                    IdentityVector3:TVector3=(1.0,1.0,1.0);
                    IdentityVector4:TVector4=(1.0,1.0,1.0,1.0);
                    IdentityQuaternion:TVector4=(0.0,0.0,0.0,1.0);
                    IdentityMatrix:TMatrix=(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0);
            end;
            TAccessor=class
             public
              type TComponentType=
                    (
                     None=0,
                     SignedByte=5120,
                     UnsignedByte=5121,
                     SignedShort=5122,
                     UnsignedShort=5123,
                     UnsignedInt=5125,
                     Float=5126
                    );
                   PComponentType=^TComponentType;
                   TRawComponentType=TPasGLTFUInt16;
                   PRawComponentType=^TRawComponentType;
                   TType=
                    (
                     None=0,
                     Scalar=1,
                     Vec2=2,
                     Vec3=3,
                     Vec4=4,
                     Mat2=5,
                     Mat3=6,
                     Mat4=7
                    );
                   PType=^TType;
                   TRawType=TPasGLTFUInt8;
                   PRawType=^TRawType;
                   TSparse=class
                    public
                     type TIndices=class
                           private
                            fComponentType:TComponentType;
                            fBufferView:TPasGLTFUInt32;
                            fByteOffset:TPasGLTFUInt32;
                            fExtensions:TPasJSONItemObject;
                            fEmpty:boolean;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property ComponentType:TComponentType read fComponentType write fComponentType default TComponentType.None;
                            property BufferView:TPasGLTFUInt32 read fBufferView write fBufferView default 0;
                            property ByteOffset:TPasGLTFUInt32 read fByteOffset write fByteOffset default 0;
                            property Extensions:TPasJSONItemObject read fExtensions;
                            property Empty:boolean read fEmpty;
                          end;
                          TValues=class
                           private
                            fBufferView:TPasGLTFUInt32;
                            fByteOffset:TPasGLTFUInt32;
                            fExtensions:TPasJSONItemObject;
                            fEmpty:boolean;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property BufferView:TPasGLTFUInt32 read fBufferView write fBufferView default 0;
                            property ByteOffset:TPasGLTFUInt32 read fByteOffset write fByteOffset default 0;
                            property Extensions:TPasJSONItemObject read fExtensions;
                            property Empty:boolean read fEmpty;
                          end;
                    private
                     fCount:TPasGLTFInt32;
                     fIndices:TIndices;
                     fValues:TValues;
                     fExtensions:TPasJSONItemObject;
                     function GetEmpty:boolean;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    published
                     property Count:TPasGLTFInt32 read fCount write fCount default 0;
                     property Indices:TIndices read fIndices;
                     property Values:TValues read fValues;
                     property Extensions:TPasJSONItemObject read fExtensions;
                     property Empty:boolean read GetEmpty;
                   end;
             private
              fName:TPasGLTFUTF8String;
              fComponentType:TComponentType;
              fType:TType;
              fBufferView:TPasGLTFInt32;
              fByteOffset:TPasGLTFUInt32;
              fCount:TPasGLTFUInt32;
              fNormalized:boolean;
              fMinArray:TPasGLTFFloatDynamicArray;
              fMaxArray:TPasGLTFFloatDynamicArray;
              fSparse:TSparse;
              fExtensions:TPasJSONItemObject;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             public
              property MinArray:TPasGLTFFloatDynamicArray read fMinArray write fMinArray;
              property MaxArray:TPasGLTFFloatDynamicArray read fMaxArray write fMaxArray;
             published
              property ComponentType:TComponentType read fComponentType write fComponentType default TComponentType.None;
              property Type_:TType read fType write fType default TType.None;
              property BufferView:TPasGLTFInt32 read fBufferView write fBufferView default -1;
              property ByteOffset:TPasGLTFUInt32 read fByteOffset write fByteOffset default 0;
              property Count:TPasGLTFUInt32 read fCount write fCount default 0;
              property Normalized:boolean read fNormalized write fNormalized default false;
              property Sparse:TSparse read fSparse;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TAccessors=TObjectList<TAccessor>;
            TAnimation=class
             public
              type TChannel=class
                    public
                     type TTarget=class
                           private
                            fNode:TPasGLTFInt32;
                            fPath:TPasGLTFUTF8String;
                            fExtensions:TPasJSONItemObject;
                            fEmpty:boolean;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property Node:TPasGLTFInt32 read fNode write fNode default -1;
                            property Path:TPasGLTFUTF8String read fPath write fPath;
                            property Extensions:TPasJSONItemObject read fExtensions;
                            property Empty:boolean read fEmpty;
                          end;
                    private
                     fSampler:TPasGLTFInt32;
                     fTarget:TTarget;
                     fExtensions:TPasJSONItemObject;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    published
                     property Sampler:TPasGLTFInt32 read fSampler write fSampler default -1;
                     property Target:TTarget read fTarget;
                     property Extensions:TPasJSONItemObject read fExtensions;
                   end;
                   TChannels=TObjectList<TChannel>;
                   TSampler=class
                    public
                     type TType=
                           (
                            Linear=0,
                            Step=1,
                            CubicSpline=2
                           );
                           PType=^TType;
                    private
                     fInput:TPasGLTFInt32;
                     fOutput:TPasGLTFInt32;
                     fInterpolation:TType;
                     fExtensions:TPasJSONItemObject;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    published
                     property Input:TPasGLTFInt32 read fInput write fInput default -1;
                     property Output:TPasGLTFInt32 read fOutput write fOutput default -1;
                     property Interpolation:TType read fInterpolation write fInterpolation default TType.Linear;
                     property Extensions:TPasJSONItemObject read fExtensions;
                   end;
                   TSamplers=TObjectList<TSampler>;
             private
              fName:TPasGLTFUTF8String;
              fChannels:TChannels;
              fSamplers:TSamplers;
              fExtensions:TPasJSONItemObject;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Channels:TChannels read fChannels;
              property Samplers:TSamplers read fSamplers;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TAnimations=TObjectList<TAnimation>;
            TAsset=class
             private
              fCopyright:TPasGLTFUTF8String;
              fGenerator:TPasGLTFUTF8String;
              fMinVersion:TPasGLTFUTF8String;
              fVersion:TPasGLTFUTF8String;
              fExtensions:TPasJSONItemObject;
              fEmpty:boolean;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             published
              property Copyright:TPasGLTFUTF8String read fCopyright write fCopyright;
              property Generator:TPasGLTFUTF8String read fGenerator write fGenerator;
              property MinVersion:TPasGLTFUTF8String read fMinVersion write fMinVersion;
              property Version:TPasGLTFUTF8String read fVersion write fVersion;
              property Extensions:TPasJSONItemObject read fExtensions;
              property Empty:boolean read fEmpty;
            end;
            TBuffer=class
             private
              fByteLength:TPasGLTFUInt32;
              fName:TPasGLTFUTF8String;
              fURI:TPasGLTFUTF8String;
              fData:TMemoryStream;
              fExtensions:TPasJSONItemObject;
              fEmbeddedResource:boolean;
              function GetEmbeddedResource:boolean;
              procedure SetEmbeddedResource(const aEmbeddedResource:boolean);
              function GetURI:TPasGLTFUTF8String;
              procedure SetURI(const aURI:TPasGLTFUTF8String);
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             published
              property ByteLength:TPasGLTFUInt32 read fByteLength write fByteLength;
              property Name:TPasGLTFUTF8String read fName write fName;
              property URI:TPasGLTFUTF8String read GetURI write SetURI;
              property Data:TMemoryStream read fData write fData;
              property EmbeddedResource:boolean read GetEmbeddedResource write SetEmbeddedResource;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TBuffers=TObjectList<TBuffer>;
            TBufferView=class
             public
              type TTargetType=
                    (
                     None=0,
                     ArrayBuffer=34962,
                     ElementArrayBuffer=34963
                    );
                   PTargetType=^TTargetType;
             private
              fName:TPasGLTFUTF8String;
              fBuffer:TPasGLTFInt32;
              fByteOffset:TPasGLTFUInt32;
              fByteLength:TPasGLTFUInt32;
              fByteStride:TPasGLTFUInt32;
              fTarget:TTargetType;
              fExtensions:TPasJSONItemObject;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property Buffer:TPasGLTFInt32 read fBuffer write fBuffer;
              property ByteOffset:TPasGLTFUInt32 read fByteOffset write fByteOffset;
              property ByteLength:TPasGLTFUInt32 read fByteLength write fByteLength;
              property ByteStride:TPasGLTFUInt32 read fByteStride write fByteStride;
              property Target:TTargetType read fTarget write fTarget default TTargetType.None;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TBufferViews=TObjectList<TBufferView>;
            TCamera=class
             public
              type TType=
                    (
                     None=0,
                     Orthographic=1,
                     Perspective=2
                    );
                   TOrthographic=class
                    private
                     fXMag:TPasGLTFFloat;
                     fYMag:TPasGLTFFloat;
                     fZNear:TPasGLTFFloat;
                     fZFar:TPasGLTFFloat;
                     fExtensions:TPasJSONItemObject;
                     fEmpty:boolean;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    published
                     property XMag:TPasGLTFFloat read fXMag write fXMag;
                     property YMag:TPasGLTFFloat read fYMag write fYMag;
                     property ZNear:TPasGLTFFloat read fZNear write fZNear;
                     property ZFar:TPasGLTFFloat read fZFar write fZFar;
                     property Extensions:TPasJSONItemObject read fExtensions;
                     property Empty:boolean read fEmpty;
                   end;
                   TPerspective=class
                    private
                     fAspectRatio:TPasGLTFFloat;
                     fYFov:TPasGLTFFloat;
                     fZNear:TPasGLTFFloat;
                     fZFar:TPasGLTFFloat;
                     fExtensions:TPasJSONItemObject;
                     fEmpty:boolean;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    published
                     property AspectRatio:TPasGLTFFloat read fAspectRatio write fAspectRatio;
                     property YFov:TPasGLTFFloat read fYFov write fYFov;
                     property ZNear:TPasGLTFFloat read fZNear write fZNear;
                     property ZFar:TPasGLTFFloat read fZFar write fZFar;
                     property Extensions:TPasJSONItemObject read fExtensions;
                     property Empty:boolean read fEmpty;
                   end;
             private
              fType:TType;
              fOrthographic:TOrthographic;
              fPerspective:TPerspective;
              fExtensions:TPasJSONItemObject;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             published
              property Type_:TType read fType write fType;
              property Orthographic:TOrthographic read fOrthographic;
              property Perspective:TPerspective read fPerspective;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TCameras=TObjectList<TCamera>;
            TImage=class
             private
              fBufferView:TPasGLTFInt32;
              fName:TPasGLTFUTF8String;
              fURI:TPasGLTFUTF8String;
              fMimeType:TPasGLTFUTF8String;
              fExtensions:TPasJSONItemObject;
              fEmbeddedResource:boolean;
              function GetEmbeddedResource:boolean;
              procedure SetEmbeddedResource(const aEmbeddedResource:boolean);
              function GetURI:TPasGLTFUTF8String;
              procedure SetURI(const aURI:TPasGLTFUTF8String);
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
              function GetEmbeddedResourceData(const aStream:TStream):boolean;
              procedure SetEmbeddedResourceData(const aStream:TStream);
             published
              property BufferView:TPasGLTFInt32 read fBufferView write fBufferView;
              property Name:TPasGLTFUTF8String read fName write fName;
              property URI:TPasGLTFUTF8String read GetURI write SetURI;
              property MimeType:TPasGLTFUTF8String read fMimeType write fMimeType;
              property Extensions:TPasJSONItemObject read fExtensions;
              property EmbeddedResource:boolean read GetEmbeddedResource write SetEmbeddedResource;
            end;
            TImages=TObjectList<TImage>;
            TMaterial=class
             public
              type TAlphaMode=
                    (
                     Opaque=0,
                     Mask=1,
                     Blend=2
                    );
                   PAlphaMode=^TAlphaMode;
                   TTexture=class
                    private
                     fIndex:TPasGLTFInt32;
                     fTexCoord:TPasGLTFInt32;
                     fExtensions:TPasJSONItemObject;
                     function GetEmpty:boolean;
                    public
                     constructor Create; reintroduce; virtual;
                     destructor Destroy; override;
                    published
                     property Extensions:TPasJSONItemObject read fExtensions;
                     property Empty:boolean read GetEmpty;
                   end;
                   TNormalTexture=class(TTexture)
                    private
                     fScale:TPasGLTFFloat;
                    public
                     constructor Create; override;
                    published
                     property Scale:TPasGLTFFloat read fScale write fScale;
                   end;
                   TOcclusionTexture=class(TTexture)
                    private
                     fStrength:TPasGLTFFloat;
                    public
                     constructor Create; override;
                    published
                     property Strength:TPasGLTFFloat read fStrength write fStrength;
                   end;
                   TPBRMetallicRoughness=class
                    private
                     fBaseColorFactor:TVector4;
                     fBaseColorTexture:TTexture;
                     fRoughnessFactor:TPasGLTFFloat;
                     fMetallicFactor:TPasGLTFFloat;
                     fMetallicRoughnessTexture:TTexture;
                     fExtensions:TPasJSONItemObject;
                     function GetEmpty:boolean;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                    public
                     property BaseColorFactor:TVector4 read fBaseColorFactor write fBaseColorFactor;
                    published
                     property BaseColorTexture:TTexture read fBaseColorTexture;
                     property RoughnessFactor:TPasGLTFFloat read fRoughnessFactor write fRoughnessFactor;
                     property MetallicFactor:TPasGLTFFloat read fMetallicFactor write fMetallicFactor;
                     property MetallicRoughnessTexture:TTexture read fMetallicRoughnessTexture;
                     property Extensions:TPasJSONItemObject read fExtensions;
                     property Empty:boolean read GetEmpty;
                   end;
             private
              fName:TPasGLTFUTF8String;
              fAlphaCutOff:TPasGLTFFloat;
              fAlphaMode:TAlphaMode;
              fDoubleSided:boolean;
              fNormalTexture:TNormalTexture;
              fOcclusionTexture:TOcclusionTexture;
              fPBRMetallicRoughness:TPBRMetallicRoughness;
              fEmissiveTexture:TTexture;
              fEmissiveFactor:TVector3;
              fExtensions:TPasJSONItemObject;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             public
              property EmissiveFactor:TVector3 read fEmissiveFactor write fEmissiveFactor;
             published
              property Name:TPasGLTFUTF8String read fName write fName;
              property AlphaCutOff:TPasGLTFFloat read fAlphaCutOff write fAlphaCutOff;
              property AlphaMode:TAlphaMode read fAlphaMode write fAlphaMode;
              property DoubleSided:boolean read fDoubleSided write fDoubleSided;
              property NormalTexture:TNormalTexture read fNormalTexture;
              property OcclusionTexture:TOcclusionTexture read fOcclusionTexture;
              property PBRMetallicRoughness:TPBRMetallicRoughness read fPBRMetallicRoughness;
              property EmissiveTexture:TTexture read fEmissiveTexture;
              property Extensions:TPasJSONItemObject read fExtensions;
            end;
            TMaterials=TObjectList<TMaterial>;
      public

     end;

implementation

{ TPasGLTF.TBase64 }

class function TPasGLTF.TBase64.Encode(const aData;const aDataLength:TPasGLTFSizeInt):TPasGLTFRawByteString;
var Index,BitCount,OutputIndex:TPasGLTFSizeInt;
    Value:TPasGLTFUInt32;
begin
 result:='';
 if aDataLength>0 then begin
  SetLength(result,(((aDataLength*4) div 3)+3) and not 3);
  OutputIndex:=0;
  Value:=0;
  BitCount:=-6;
  for Index:=0 to aDataLength-1 do begin
   Value:=(Value shl 8) or PPasGLTFUInt8Array(@aData)^[Index];
   inc(BitCount,8);
   while BitCount>=0 do begin
    result[Low(result)+OutputIndex]:=EncodingLookUpTable[(Value shr BitCount) and 63];
    inc(OutputIndex);
    dec(BitCount,6);
   end;
  end;
  if BitCount>-6 then begin
   result[Low(result)+OutputIndex]:=EncodingLookUpTable[((Value shl 8) shr (BitCount+8)) and 63];
   inc(OutputIndex);
  end;
  while (OutputIndex and 3)<>0 do begin
   result[Low(result)+OutputIndex]:='=';
   inc(OutputIndex);
  end;
  SetLength(result,OutputIndex);
 end;
end;

class function TPasGLTF.TBase64.Encode(const aData:array of TPasGLTFUInt8):TPasGLTFRawByteString;
begin
 result:=Encode(aData[0],length(aData));
end;

class function TPasGLTF.TBase64.Encode(const aData:TPasGLTFRawByteString):TPasGLTFRawByteString;
begin
 result:=Encode(aData[Low(aData)],length(aData));
end;

class function TPasGLTF.TBase64.Encode(const aData:TStream):TPasGLTFRawByteString;
var Bytes:TPasGLTFUInt8DynamicArray;
begin
 Bytes:=nil;
 try
  SetLength(Bytes,aData.Size);
  aData.Seek(0,soBeginning);
  aData.ReadBuffer(Bytes[0],aData.Size);
  result:=Encode(Bytes[0],length(Bytes));
 finally
  Bytes:=nil;
 end;
end;

class function TPasGLTF.TBase64.Decode(const aInput:TPasGLTFRawByteString;const aOutput:TStream):boolean;
var Index,Size,BitCount,OutputIndex,
    LookUpTableValue,Remaining:TPasGLTFSizeInt;
    Value:TPasGLTFUInt32;
    Buffer:TPasGLTFUInt8DynamicArray;
begin
 result:=false;
 Buffer:=nil;
 try
  Size:=length(aInput);
  if Size>0 then begin
   if (Size and 4)=0 then begin
    result:=true;
    SetLength(Buffer,(Size*3) shr 2);
    Value:=0;
    BitCount:=-8;
    OutputIndex:=0;
    try
     for Index:=1 to Size do begin
      LookUpTableValue:=DecodingLookUpTable[aInput[Index]];
      if LookUpTableValue>=0 then begin
       Value:=(Value shl 6) or LookUpTableValue;
       inc(BitCount,6);
       while BitCount>=0 do begin
        Buffer[OutputIndex]:=(Value shr BitCount) and $ff;
        inc(OutputIndex);
        dec(BitCount,8);
       end;
      end else begin
       case aInput[Index] of
        '=':begin
         Remaining:=Size-Index;
         if (Remaining>1) or ((Remaining=1) and (aInput[Index+1]<>'=')) then begin
          result:=false;
         end;
        end;
        else begin
         result:=false;
        end;
       end;
       break;
      end;
     end;
    finally
     SetLength(Buffer,OutputIndex);
    end;
    if result then begin
     aOutput.WriteBuffer(Buffer[0],OutputIndex);
    end;
   end;
  end else begin
   result:=true;
  end;
 finally
  Buffer:=nil;
 end;
end;

{ TPasGLTF.TAccessor.TSparse.TIndices }

constructor TPasGLTF.TAccessor.TSparse.TIndices.Create;
begin
 inherited Create;
 fComponentType:=TComponentType.None;
 fBufferView:=0;
 fByteOffset:=0;
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TAccessor.TSparse.TIndices.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAccessor.TSparse.TValues }

constructor TPasGLTF.TAccessor.TSparse.TValues.Create;
begin
 inherited Create;
 fBufferView:=0;
 fByteOffset:=0;
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TAccessor.TSparse.TValues.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAccessor.TSparse }

constructor TPasGLTF.TAccessor.TSparse.Create;
begin
 inherited Create;
 fCount:=0;
 fIndices:=TIndices.Create;
 fValues:=TValues.Create;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TAccessor.TSparse.Destroy;
begin
 FreeAndNil(fIndices);
 FreeAndNil(fValues);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

function TPasGLTF.TAccessor.TSparse.GetEmpty:boolean;
begin
 result:=fCount=0;
end;

{ TPasGLTF.TAccessor }

constructor TPasGLTF.TAccessor.Create;
begin
 inherited Create;
 fComponentType:=TComponentType.None;
 fType:=TType.None;
 fBufferView:=-1;
 fByteOffset:=0;
 fCount:=0;
 fNormalized:=TDefaults.AccessorNormalized;
 fMinArray:=nil;
 fMaxArray:=nil;
 fSparse:=TSparse.Create;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TAccessor.Destroy;
begin
 fMinArray:=nil;
 fMaxArray:=nil;
 FreeAndNil(fSparse);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAnimation.TChannel.TTarget }

constructor TPasGLTF.TAnimation.TChannel.TTarget.Create;
begin
 inherited Create;
 fNode:=-1;
 fPath:='';
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TAnimation.TChannel.TTarget.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAnimation.TChannel }

constructor TPasGLTF.TAnimation.TChannel.Create;
begin
 inherited Create;
 fSampler:=-1;
 fTarget:=TTarget.Create;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TAnimation.TChannel.Destroy;
begin
 FreeAndNil(fTarget);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAnimation.TSampler }

constructor TPasGLTF.TAnimation.TSampler.Create;
begin
 inherited Create;
 fInput:=-1;
 fOutput:=-1;
 fInterpolation:=TType.Linear;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TAnimation.TSampler.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAnimation }

constructor TPasGLTF.TAnimation.Create;
begin
 inherited Create;
 fName:='';
 fChannels:=TChannels.Create(true);
 fSamplers:=TSamplers.Create(true);
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TAnimation.Destroy;
begin
 FreeAndNil(fChannels);
 FreeAndNil(fSamplers);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TAsset }

constructor TPasGLTF.TAsset.Create;
begin
 inherited Create;
 fCopyright:='';
 fGenerator:='';
 fMinVersion:='';
 fVersion:='2.0';
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TAsset.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TBuffer }

constructor TPasGLTF.TBuffer.Create;
begin
 inherited Create;
 fByteLength:=0;
 fName:='';
 fURI:='';
 fData:=TMemoryStream.Create;
 fExtensions:=TPasJSONItemObject.Create;
 fEmbeddedResource:=false;
end;

destructor TPasGLTF.TBuffer.Destroy;
begin
 FreeAndNil(fData);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

function TPasGLTF.TBuffer.GetEmbeddedResource:boolean;
begin
 result:=fEmbeddedResource;
end;

procedure TPasGLTF.TBuffer.SetEmbeddedResource(const aEmbeddedResource:boolean);
begin
 if fEmbeddedResource<>aEmbeddedResource then begin
  fEmbeddedResource:=aEmbeddedResource;
  if aEmbeddedResource then begin
   fURI:='';
  end;
 end;
end;

function TPasGLTF.TBuffer.GetURI:TPasGLTFUTF8String;
begin
 if fEmbeddedResource then begin
  result:=MimeTypeApplicationOctet+','+TBase64.Encode(fData);
 end else begin
  result:=fURI;
 end;
end;

procedure TPasGLTF.TBuffer.SetURI(const aURI:TPasGLTFUTF8String);
begin
 fEmbeddedResource:=pos(fURI,MimeTypeApplicationOctet)>0;
 if fEmbeddedResource then begin
  fData.Clear;
  if not TBase64.Decode(copy(aURI,length(MimeTypeApplicationOctet)+1,length(aURI)-(length(MimeTypeApplicationOctet)+2)),fData) then begin
   raise EPasGLTFInvalidBase64.Create('Invalid base64');
  end;
  fByteLength:=fData.Size;
  fURI:='';
 end else begin
  fURI:=aURI;
 end;
end;

{ TPasGLTF.TBufferView }

constructor TPasGLTF.TBufferView.Create;
begin
 inherited Create;
 fName:='';
 fBuffer:=-1;
 fByteOffset:=0;
 fByteLength:=0;
 fByteStride:=0;
 fTarget:=TTargetType.None;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TBufferView.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TCamera.TOrthographic }

constructor TPasGLTF.TCamera.TOrthographic.Create;
begin
 inherited Create;
 fXMag:=TDefaults.FloatSentinel;
 fYMag:=TDefaults.FloatSentinel;
 fZNear:=-TDefaults.FloatSentinel;
 fZFar:=-TDefaults.FloatSentinel;
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TCamera.TOrthographic.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TCamera.TPerspective }

constructor TPasGLTF.TCamera.TPerspective.Create;
begin
 inherited Create;
 fAspectRatio:=0.0;
 fYFov:=0.0;
 fZNear:=0.0;
 fZFar:=0.0;
 fExtensions:=TPasJSONItemObject.Create;
 fEmpty:=false;
end;

destructor TPasGLTF.TCamera.TPerspective.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TCamera }

constructor TPasGLTF.TCamera.Create;
begin
 inherited Create;
 fType:=TType.None;
 fOrthographic:=TOrthographic.Create;
 fPerspective:=TPerspective.Create;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TCamera.Destroy;
begin
 FreeAndNil(fOrthographic);
 FreeAndNil(fPerspective);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

{ TPasGLTF.TImage }

constructor TPasGLTF.TImage.Create;
begin
 inherited Create;
 fBufferView:=-1;
 fName:='';
 fURI:='';
 fMimeType:='';
 fExtensions:=TPasJSONItemObject.Create;
 fEmbeddedResource:=false;
end;

destructor TPasGLTF.TImage.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

function TPasGLTF.TImage.GetEmbeddedResource:boolean;
begin
 result:=fEmbeddedResource;
end;

procedure TPasGLTF.TImage.SetEmbeddedResource(const aEmbeddedResource:boolean);
begin
 if fEmbeddedResource<>aEmbeddedResource then begin
  fEmbeddedResource:=aEmbeddedResource;
 end;
end;

function TPasGLTF.TImage.GetEmbeddedResourceData(const aStream:TStream):boolean;
var mt:TPasGLTFUTF8String;
begin
 result:=fEmbeddedResource;
 if result then begin
  if pos(MimeTypeImagePNG,fURI)=1 then begin
   mt:=MimeTypeImagePNG;
  end else if pos(MimeTypeImageJPG,fURI)=1 then begin
   mt:=MimeTypeImageJPG;
  end;
  if aStream is TMemoryStream then begin
   TMemoryStream(aStream).Clear;
  end;
  result:=TBase64.Decode(copy(fURI,length(mt)+1,length(fURI)-(length(mt)+1)),aStream);
 end;
end;

procedure TPasGLTF.TImage.SetEmbeddedResourceData(const aStream:TStream);
begin
 fURI:=fMimeType+','+TBase64.Encode(aStream);
 fEmbeddedResource:=true;
end;

function TPasGLTF.TImage.GetURI:TPasGLTFUTF8String;
begin
 result:=fURI;
end;

procedure TPasGLTF.TImage.SetURI(const aURI:TPasGLTFUTF8String);
begin
 if fURI<>aURI then begin
  fURI:=aURI;
  if (pos(MimeTypeImagePNG,fURI)=1) or (pos(MimeTypeImageJPG,fURI)=1) then begin
   fEmbeddedResource:=true;
  end else begin
   fEmbeddedResource:=false;
  end;
 end;
end;

{ TPasGLTF.TMaterial.TTexture }

constructor TPasGLTF.TMaterial.TTexture.Create;
begin
 inherited Create;
 fIndex:=-1;
 fTexCoord:=0;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TMaterial.TTexture.Destroy;
begin
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

function TPasGLTF.TMaterial.TTexture.GetEmpty:boolean;
begin
 result:=fIndex<0;
end;

{ TPasGLTF.TMaterial.TNormalTexture }

constructor TPasGLTF.TMaterial.TNormalTexture.Create;
begin
 inherited Create;
 fScale:=TDefaults.IdentityScalar;
end;

{ TPasGLTF.TMaterial.TOcclusionTexture }

constructor TPasGLTF.TMaterial.TOcclusionTexture.Create;
begin
 inherited Create;
 fStrength:=TDefaults.IdentityScalar;
end;

{ TPasGLTF.TMaterial.TPBRMetallicRoughness }

constructor TPasGLTF.TMaterial.TPBRMetallicRoughness.Create;
begin
 inherited Create;
 fBaseColorFactor:=TDefaults.IdentityVector4;
 fBaseColorTexture:=TTexture.Create;
 fRoughnessFactor:=TDefaults.IdentityScalar;
 fMetallicFactor:=TDefaults.IdentityScalar;
 fMetallicRoughnessTexture:=TTexture.Create;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TMaterial.TPBRMetallicRoughness.Destroy;
begin
 FreeAndNil(fBaseColorTexture);
 FreeAndNil(fMetallicRoughnessTexture);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

function TPasGLTF.TMaterial.TPBRMetallicRoughness.GetEmpty:boolean;
begin
 result:=fBaseColorTexture.Empty and
         fMetallicRoughnessTexture.Empty and
         SameValue(fRoughnessFactor,TDefaults.IdentityScalar) and
         SameValue(fMetallicFactor,TDefaults.IdentityScalar);
end;

{ TPasGLTF.TMaterial }

constructor TPasGLTF.TMaterial.Create;
begin
 inherited Create;
 fName:='';
 fAlphaCutOff:=TDefaults.MaterialAlphaCutoff;
 fAlphaMode:=TAlphaMode.Opaque;
 fDoubleSided:=TDefaults.MaterialDoubleSided;
 fNormalTexture:=TNormalTexture.Create;
 fOcclusionTexture:=TOcclusionTexture.Create;
 fPBRMetallicRoughness:=TPBRMetallicRoughness.Create;
 fEmissiveTexture:=TTexture.Create;
 fEmissiveFactor:=TDefaults.NullVector3;
 fExtensions:=TPasJSONItemObject.Create;
end;

destructor TPasGLTF.TMaterial.Destroy;
begin
 FreeAndNil(fNormalTexture);
 FreeAndNil(fOcclusionTexture);
 FreeAndNil(fPBRMetallicRoughness);
 FreeAndNil(fEmissiveTexture);
 FreeAndNil(fExtensions);
 inherited Destroy;
end;

end.
