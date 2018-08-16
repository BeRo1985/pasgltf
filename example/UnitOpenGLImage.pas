unit UnitOpenGLImage;
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
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define caninline}
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
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifdef windows}
 {$define win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
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
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define caninline}
{$else}
 {$undef caninline}
 {$ifdef ver180}
  {$define caninline}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define caninline}
   {$ifend}
  {$endif}
 {$endif}
{$endif}

interface

uses dglOpenGL;

type TGLImageBytes=array of byte;

     PGLImageKeyValuePair=^TGLImageKeyValuePair;
     TGLImageKeyValuePair=record
      KeyAndValueByteSize:longword;
      KeyAndValue:TGLImageBytes;
      ValuePadding:TGLImageBytes;
     end;

     TGLImageKeyValuePairs=array of TGLImageKeyValuePair;

     PGLImageFace=^TGLImageFace;
     TGLImageFace=record
      PixelWidth:longword;
      PixelHeight:longword;
      PixelDepth:longword;
      DataSize:longword;
      Data:TGLImageBytes;
     end;

     TGLImageFaces=array of TGLImageFace;

     PGLImageMipMapLevel=^TGLImageMipMapLevel;
     TGLImageMipMapLevel=record
      ImageSize:longword;
      PixelWidth:longword;
      PixelHeight:longword;
      PixelDepth:longword;
      Faces:TGLImageFaces;
     end;

     TGLImageMipMapLevels=array of TGLImageMipMapLevel;

     TGLImage=class
      public
       TextureDimensions:longword;
       GLTarget:gLEnum;
       Compressed:longbool;
       GenerateMipmaps:longbool;
       DoSwizzle:longbool;
       Swizzle:array[0..3] of longword;
       GLType:longword;
       GLTypeSize:longword;
       GLFormat:longword;
       GLInternalFormat:longword;
       GLBaseInternalFormat:longword;
       PixelWidth:longword;
       PixelHeight:longword;
       PixelDepth:longword;
       NumberOfArrayElements:longword;
       NumberOfFaces:longword;
       NumberOfMipmapLevels:longword;
       BytesOfKeyValueData:longword;
       KeyValuePairs:TGLImageKeyValuePairs;
       MipMapLevels:TGLImageMipMapLevels;
       PremultiplyAlpha:longbool;
       Uploaded:longbool;
       constructor Create;
       destructor Destroy; override;
       procedure Upload(Handle:glUInt);
       procedure Unload(Handle:glUInt);
     end;

procedure ResizeHalfRGBA(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer);
procedure ResizeHalfRGBANormalMap(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer);
procedure ResizeRGBA32(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer;DstWidth,DstHeight:longint);
function LoadImage(DataPointer:pointer;DataSize:longword;var ImageData:pointer;var ImageWidth,ImageHeight:longint;HeaderOnly:boolean=false;MipMapLevel:longint=0;IsFloat:pboolean=nil):boolean;
function LoadGLImage(DataPointer:pointer;DataSize:longword;SRGB:boolean):TGLImage;

implementation

uses Math,UnitOpenGLImagePNG,UnitOpenGLImageJPEG;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

procedure ResizeHalfRGBA(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer);
type PLongwords=^TLongwords;
     TLongwords=array[0..65535] of longword;
var DstX,DstY,SrcY:longint;
    xUL,xUR,xLL,xLR,RedBlue,GreenAlpha,RedBlueRemainder,GreenAlphaRemainder:longword;
    TempSrc,TempDst:PLongwords;
begin
 RedBlueRemainder:=0;
 GreenAlphaRemainder:=0;
 TempDst:=pointer(Dst);
 for DstY:=0 to (SrcHeight shr 1)-1 do begin
  SrcY:=DstY*2;
  TempSrc:=pointer(@pansichar(Src)[(SrcY*SrcWidth) shl 2]);
  for DstX:=0 to (SrcWidth shr 1)-1 do begin
   xUL:=TempSrc^[0];
   xUR:=TempSrc^[1];
   xLL:=TempSrc^[SrcWidth];
   xLR:=TempSrc^[SrcWidth+1];
   RedBlue:=(xUL and $00ff00ff)+(xUR and $00ff00ff)+(xLL and $00ff00ff)+(xLR and $00ff00ff)+(RedBlueRemainder and $00ff00ff);
   GreenAlpha:=((xUL shr 8) and $00ff00ff)+((xUR shr 8) and $00ff00ff)+((xLL shr 8) and $00ff00ff)+((xLR shr 8) and $00ff00ff)+(GreenAlphaRemainder and $00ff00ff);
   RedBlueRemainder:=RedBlue and $00030003;
   GreenAlphaRemainder:=GreenAlpha and $00030003;
   TempDst^[0]:=((RedBlue and $03fc03fc) shr 2) or (((GreenAlpha and $03fc03fc) shr 2) shl 8);
   TempDst:=pointer(@TempDst^[1]);
   TempSrc:=pointer(@TempSrc^[2]);
  end;
 end;
end;

procedure ResizeHalfRGBANormalMap(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer);
begin
 ResizeHalfRGBA(Src,SrcWidth,SrcHeight,Dst);
end;
{type PLongwords=^TLongwords;
     TLongwords=array[0..65535] of longword;
var DstX,DstY,SrcY:longint;
    xUL,xUR,xLL,xLR,RedBlue,Green,Alpha,RedGreenBlueRemainder:longword;
    TempSrc,TempDst:PLongwords;
begin
 RedGreenBlueRemainder:=0;
 TempDst:=pointer(Dst);
 for DstY:=0 to (SrcHeight shr 1)-1 do begin
  SrcY:=DstY*2;
  TempSrc:=pointer(@pansichar(Src)[(SrcY*SrcWidth) shl 2]);
  for DstX:=0 to (SrcWidth shr 1)-1 do begin
   xUL:=TempSrc^[0];
   xUR:=TempSrc^[1];
   xLL:=TempSrc^[SrcWidth];
   xLR:=TempSrc^[SrcWidth+1];
   RedBlue:=(xUL and $00ff00ff)+(xUR and $00ff00ff)+(xLL and $00ff00ff)+(xLR and $00ff00ff)+(RedGreenBlueRemainder and $00ff00ff);
   Green:=(xUL and $0000ff00)+(xUR and $0000ff00)+(xLL and $0000ff00)+(xLR and $0000ff00)+(RedGreenBlueRemainder and $0000ff00);
   RedGreenBlueRemainder:=(RedBlue and $00030003) or (Green and $00000300);
   if xUL<xUR then begin
    Alpha:=xUR;
   end else begin
    Alpha:=xUL;
   end;
   if Alpha<xLL then begin
    Alpha:=xLL;
   end;
   if Alpha<xLR then begin
    Alpha:=xLR;
   end;
   TempDst[0]:=(((RedBlue and $03fc03fc) or (Green and $0003fc00)) shr 2) or (Alpha and $ff000000);
   TempDst:=pointer(@TempDst^[1]);
   TempSrc:=pointer(@TempSrc^[2]);
  end;
 end;
end;{}

procedure ResizeRGBA32(Src:pointer;SrcWidth,SrcHeight:longint;Dst:pointer;DstWidth,DstHeight:longint);
type PLongwords=^TLongwords;
     TLongwords=array[0..65535] of longword;
var DstX,DstY,SrcX,SrcY:longint;
    r,g,b,a,w,Pixel,SrcR,SrcG,SrcB,SrcA,Weight,xUL,xUR,xLL,xLR,
    RedBlue,GreenAlpha,RedBlueRemainder,GreenAlphaRemainder,WeightX,WeightY:longword;
//  SrcPtr,DstPtr:pansichar;
    TempSrc,TempDst:PLongwords;
    UpsampleX,UpsampleY:longbool;
    WeightShift,xa,xb,xc,xd,ya,yb,yc,yd:longint;
    SourceTexelsPerOutPixel,WeightPerPixel,AccumlatorPerPixel,WeightDivider,fw,fh:single;
    XCache:array of longint;
begin
 XCache:=nil;
 if (SrcWidth=(DstWidth*2)) and (SrcHeight=(DstHeight*2)) then begin
  RedBlueRemainder:=0;
  GreenAlphaRemainder:=0;
  TempDst:=pointer(Dst);
  for DstY:=0 to DstHeight-1 do begin
   SrcY:=DstY*2;
   TempSrc:=pointer(@pansichar(Src)[(SrcY*SrcWidth) shl 2]);
   for DstX:=0 to DstWidth-1 do begin
    xUL:=TempSrc^[0];
    xUR:=TempSrc^[1];
    xLL:=TempSrc^[SrcWidth];
    xLR:=TempSrc^[SrcWidth+1];
    RedBlue:=(xUL and $00ff00ff)+(xUR and $00ff00ff)+(xLL and $00ff00ff)+(xLR and $00ff00ff)+(RedBlueRemainder and $00ff00ff);
    GreenAlpha:=((xUL shr 8) and $00ff00ff)+((xUR shr 8) and $00ff00ff)+((xLL shr 8) and $00ff00ff)+((xLR shr 8) and $00ff00ff)+(GreenAlphaRemainder and $00ff00ff);
    RedBlueRemainder:=RedBlue and $00030003;
    GreenAlphaRemainder:=GreenAlpha and $00030003;
    TempDst[0]:=((RedBlue and $03fc03fc) shr 2) or (((GreenAlpha and $03fc03fc) shr 2) shl 8);
    TempDst:=pointer(@TempDst^[1]);
    TempSrc:=pointer(@TempSrc^[2]);
   end;
  end;
 end else begin
  UpsampleX:=SrcWidth<DstWidth;
  UpsampleY:=DstHeight<DstHeight;
  WeightShift:=0;
  SourceTexelsPerOutPixel:=((SrcWidth/DstWidth)+1)*((SrcHeight/DstHeight)+1);
  WeightPerPixel:=SourceTexelsPerOutPixel*65536;
  AccumlatorPerPixel:=WeightPerPixel*256;
  WeightDivider:=AccumlatorPerPixel/4294967000.0;
  if WeightDivider>1.0 then begin
   WeightShift:=trunc(ceil(ln(WeightDivider)/ln(2.0)));
  end;
  WeightShift:=min(WeightShift,15);
  fw:=(256*SrcWidth)/DstWidth;
  fh:=(256*SrcHeight)/DstHeight;
  if UpsampleX and UpsampleY then begin
   if length(XCache)<longint(DstWidth) then begin
    SetLength(XCache,longint(DstWidth));
   end;
   for DstX:=0 to DstWidth-1 do begin
    XCache[DstX]:=min(trunc(DstX*fw),(256*(SrcWidth-1))-1);
   end;
   for DstY:=0 to DstHeight-1 do begin
    ya:=min(trunc(DstY*fh),(256*(SrcHeight-1))-1);
    yc:=ya shr 8;
    TempDst:=pointer(@pansichar(Dst)[(DstY*DstWidth) shl 2]);
    for DstX:=0 to DstWidth-1 do begin
     xa:=XCache[DstX];
     xc:=xa shr 8;
     TempSrc:=pointer(@pansichar(Src)[((yc*SrcWidth)+xc) shl 2]);
     r:=0;
     g:=0;
     b:=0;
     a:=0;
     WeightX:=longword(longint(256-(xa and $ff)));
     WeightY:=longword(longint(256-(ya and $ff)));
     for SrcY:=0 to 1 do begin
      for SrcX:=0 to 1 do begin
       Pixel:=TempSrc^[(SrcY*SrcWidth)+SrcX];
       SrcR:=(Pixel shr 0) and $ff;
       SrcG:=(Pixel shr 8) and $ff;
       SrcB:=(Pixel shr 16) and $ff;
       SrcA:=(Pixel shr 24) and $ff;
       Weight:=(WeightX*WeightY) shr WeightShift;
       inc(r,SrcR*Weight);
       inc(g,SrcG*Weight);
       inc(b,SrcB*Weight);
       inc(a,SrcA*Weight);
       WeightX:=256-WeightX;
      end;
      WeightY:=256-WeightY;
     end;
     TempDst^[0]:=((r shr 16) and $ff) or ((g shr 8) and $ff00) or (b and $ff0000) or ((a shl 8) and $ff000000);
     TempDst:=pointer(@TempDst^[1]);
    end;
   end;
  end else begin
   if length(XCache)<(longint(DstWidth)*2) then begin
    SetLength(XCache,longint(DstWidth)*2);
   end;
   for DstX:=0 to DstWidth-1 do begin
    xa:=trunc(DstX*fw);
    if UpsampleX then begin
     xb:=xa+256;
    end else begin
     xb:=trunc((DstX+1)*fw);
    end;
    XCache[(DstX shl 1) or 0]:=min(xa,(256*SrcWidth)-1);
    XCache[(DstX shl 1) or 1]:=min(xb,(256*SrcWidth)-1);
   end;
   for DstY:=0 to DstHeight-1 do begin
    ya:=trunc(DstY*fh);
    if UpsampleY then begin
     yb:=ya+256;
    end else begin
     yb:=trunc((DstY+1)*fh);
    end;
    TempDst:=pointer(@pansichar(Dst)[(DstY*DstWidth) shl 2]);
    yc:=ya shr 8;
    yd:=yb shr 8;
    for DstX:=0 to DstWidth-1 do begin
     xa:=XCache[(DstX shl 1) or 0];
     xb:=XCache[(DstX shl 1) or 1];
     xc:=xa shr 8;
     xd:=xb shr 8;
     r:=0;
     g:=0;
     b:=0;
     a:=0;
     w:=0;
     for SrcY:=yc to yd do begin
      if (SrcY<0) or (SrcY>=SrcHeight) then begin
       continue;
      end;
      WeightY:=256;
      if yc<>yd then begin
       if SrcY=yc then begin
        WeightY:=256-(ya and $ff);
       end else if SrcY=yd then begin
        WeightY:=yb and $ff;
       end;
      end;
      TempSrc:=pointer(@pansichar(Src)[((SrcY*SrcWidth)+xc) shl 2]);
      for SrcX:=xc to xd do begin
       if (SrcX<0) or (SrcX>=SrcWidth) then begin
        continue;
       end;
       WeightX:=256;
       if xc<>xd then begin
        if SrcX=xc then begin
         WeightX:=256-(xa and $ff);
        end else if SrcX=xd then begin
         WeightX:=xb and $ff;
        end;
       end;
       Pixel:=TempSrc^[0];
       inc(PAnsiChar(TempSrc),SizeOf(longword));
       SrcR:=(Pixel shr 0) and $ff;
       SrcG:=(Pixel shr 8) and $ff;
       SrcB:=(Pixel shr 16) and $ff;
       SrcA:=(Pixel shr 24) and $ff;
       Weight:=(WeightX*WeightY) shr WeightShift;
       inc(r,SrcR*Weight);
       inc(g,SrcG*Weight);
       inc(b,SrcB*Weight);
       inc(a,SrcA*Weight);
       inc(w,Weight);
      end;
     end;
     if w>0 then begin
      TempDst^[0]:=((r div w) and $ff) or (((g div w) shl 8) and $ff00) or (((b div w) shl 16) and $ff0000) or (((a div w) shl 24) and $ff000000);
     end else begin
      TempDst^[0]:=0;
     end;
     TempDst:=pointer(@TempDst^[1]);
    end;
   end;
  end;
 end;
 SetLength(XCache,0);
end;

function LoadImage(DataPointer:pointer;DataSize:longword;var ImageData:pointer;var ImageWidth,ImageHeight:longint;HeaderOnly:boolean=false;MipMapLevel:longint=0;IsFloat:pboolean=nil):boolean;
var IsFloatTemp:boolean;
    i:longint;
    pf:psingle;
    pb:pbyte;
begin
 if assigned(IsFloat) then begin
  IsFloat^:=false;
 end;
 IsFloatTemp:=false;
 if (MipMapLevel=0) and LoadPNGImage(DataPointer,DataSize,ImageData,ImageWidth,ImageHeight,HeaderOnly) then begin
  result:=true;
 end else begin
  if assigned(IsFloat) then begin
   IsFloat^:=false;
  end;
  result:=LoadJPEGImage(DataPointer,DataSize,ImageData,ImageWidth,ImageHeight);
 end;
 if assigned(IsFloat) then begin
  IsFloat^:=IsFloatTemp;
 end else begin
  if IsFloatTemp then begin
   pf:=ImageData;
   pb:=ImageData;
   for i:=1 to (ImageWidth*ImageHeight)*4 do begin
    pb^:=Min(Max(round(pf^*255.0),0),255);
    inc(pf);
    inc(pb);
   end;
  end;
 end;
end;

constructor TGLImage.Create;
begin
 inherited Create;
 TextureDimensions:=0;
 GLTarget:=0;
 Compressed:=false;
 GenerateMipmaps:=false;
 DoSwizzle:=false;
 Swizzle[0]:=GL_RED;
 Swizzle[1]:=GL_GREEN;
 Swizzle[2]:=GL_BLUE;
 Swizzle[3]:=GL_ALPHA;
 GLType:=0;
 GLTypeSize:=0;
 GLFormat:=0;
 GLInternalFormat:=0;
 GLBaseInternalFormat:=0;
 PixelWidth:=0;
 PixelHeight:=0;
 PixelDepth:=0;
 NumberOfArrayElements:=0;
 NumberOfFaces:=0;
 NumberOfMipmapLevels:=0;
 BytesOfKeyValueData:=0;
 KeyValuePairs:=nil;
 MipMapLevels:=nil;
 PremultiplyAlpha:=false;
 Uploaded:=false;
end;

destructor TGLImage.Destroy;
begin
 SetLength(KeyValuePairs,0);
 SetLength(MipMapLevels,0);
 inherited Destroy;
end;

procedure TGLImage.Upload(Handle:glUInt);
var ImageData:pointer;
    ImageWidth,ImageHeight,BlockSize:longint;
    Error:glEnum;
 procedure DecodeDXT(Data:pointer;Version:longint;PremultiplyAlpha:boolean);
 var i,x,y,bx,by,RowSize:longint;
     pData:pbyte;
     c0,c1,Bits,rb0,rb1,rb2,rb3,g0,g1,g2,g3,r,g,b,a,v:longword;
     p:pansichar;
     Colors:array[0..3] of longword;
     Alpha:int64;
     AlphaValues:array[0..7] of byte;
 begin
  GetMem(ImageData,((((ImageWidth+3) shr 2) shl 2)*(((ImageHeight+3) shr 2) shl 2))*4);
  FillChar(ImageData^,ImageWidth*ImageHeight*4,AnsiChar(#$ff));
  RowSize:=((ImageWidth+3) shr 2)*longint(BlockSize);
  i:=((ImageHeight+3) shr 2)*RowSize;
  pData:=Data;
  if assigned(pData) then begin
   y:=0;
   while y<ImageHeight do begin
    x:=0;
    while x<ImageWidth do begin
     i:=((y shr 2)*RowSize)+((x shr 2)*longint(BlockSize));
     case Version of
      2,3:begin
       Alpha:=(int64(byte(pansichar(pointer(pData))[i+0])) shl 0) or
              (int64(byte(pansichar(pointer(pData))[i+1])) shl 8) or
              (int64(byte(pansichar(pointer(pData))[i+2])) shl 16) or
              (int64(byte(pansichar(pointer(pData))[i+3])) shl 24) or
              (int64(byte(pansichar(pointer(pData))[i+4])) shl 32) or
              (int64(byte(pansichar(pointer(pData))[i+5])) shl 40) or
              (int64(byte(pansichar(pointer(pData))[i+6])) shl 48) or
              (int64(byte(pansichar(pointer(pData))[i+7])) shl 56);
       a:=$00000000;
       inc(i,8);
      end;
      4,5:begin
       AlphaValues[0]:=byte(pansichar(pointer(pData))[i+0]);
       AlphaValues[1]:=byte(pansichar(pointer(pData))[i+1]);
       if AlphaValues[0]>AlphaValues[1] then begin
        AlphaValues[2]:=((6*AlphaValues[0])+(1*AlphaValues[1])) div 7;
        AlphaValues[3]:=((5*AlphaValues[0])+(2*AlphaValues[1])) div 7;
        AlphaValues[4]:=((4*AlphaValues[0])+(3*AlphaValues[1])) div 7;
        AlphaValues[5]:=((3*AlphaValues[0])+(4*AlphaValues[1])) div 7;
        AlphaValues[6]:=((2*AlphaValues[0])+(5*AlphaValues[1])) div 7;
        AlphaValues[7]:=((1*AlphaValues[0])+(6*AlphaValues[1])) div 7;
       end else begin
        AlphaValues[2]:=((4*AlphaValues[0])+(1*AlphaValues[1])) div 5;
        AlphaValues[3]:=((3*AlphaValues[0])+(2*AlphaValues[1])) div 5;
        AlphaValues[4]:=((2*AlphaValues[0])+(3*AlphaValues[1])) div 5;
        AlphaValues[5]:=((1*AlphaValues[0])+(4*AlphaValues[1])) div 5;
        AlphaValues[6]:=0;
        AlphaValues[7]:=255;
       end;
       Alpha:=(int64(byte(pansichar(pointer(pData))[i+2])) shl 0) or
              (int64(byte(pansichar(pointer(pData))[i+3])) shl 8) or
              (int64(byte(pansichar(pointer(pData))[i+4])) shl 16) or
              (int64(byte(pansichar(pointer(pData))[i+5])) shl 24) or
              (int64(byte(pansichar(pointer(pData))[i+6])) shl 32) or
              (int64(byte(pansichar(pointer(pData))[i+7])) shl 40);
       a:=$00000000;
       inc(i,8);
      end;
      else begin
       Alpha:=0;
       a:=$ff000000;
      end;
     end;
     c0:=byte(pansichar(pointer(pData))[i+0]) or (byte(pansichar(pointer(pData))[i+1]) shl 8);
     c1:=byte(pansichar(pointer(pData))[i+2]) or (byte(pansichar(pointer(pData))[i+3]) shl 8);
     rb0:=((c0 shl 3) or (c0 shl 8)) and $f800f8;
     rb1:=((c1 shl 3) or (c1 shl 8)) and $f800f8;
     inc(rb0,(rb0 shr 5) and $070007);
     inc(rb1,(rb1 shr 5) and $070007);
     g0:=(c0 shl 5) and $00fc00;
     g1:=(c1 shl 5) and $00fc00;
     inc(g0,(g0 shr 6) and $000300);
     inc(g1,(g1 shr 6) and $000300);
     Colors[0]:=(rb0 or g0) or a;
     Colors[1]:=(rb1 or g1) or a;
     if (c0>c1) or (Version in [2,3,4,5]) then begin
      rb2:=((((2*rb0)+rb1)*21) shr 6) and $ff00ff;
      g2:=((((2*g0)+g1)*21) shr 6) and $00ff00;
      rb3:=(((rb0+(2*rb1))*21) shr 6) and $ff00ff;
      g3:=(((g0+(2*g1))*21) shr 6) and $00ff00;
      Colors[3]:=(rb3 or g3) or a;
     end else begin
      rb2:=((rb0+rb1) shr 1) and $ff00ff;
      g2:=((g0+g1) shr 1) and $00ff00;
      Colors[3]:=$00000000;
     end;
     Colors[2]:=(rb2 or g2) or a;
     Bits:=byte(pansichar(pointer(pData))[i+4]) or (byte(pansichar(pointer(pData))[i+5]) shl 8) or (byte(pansichar(pointer(pData))[i+6]) shl 16) or (byte(pansichar(pointer(pData))[i+7]) shl 24);
     for by:=0 to 3 do begin
      for bx:=0 to 3 do begin
       case Version of
        2,3:begin
         a:=Alpha and $f;
         a:=a or (a shl 4);
         Alpha:=Alpha shr 4;
        end;
        4,5:begin
         a:=AlphaValues[Alpha and 7];
         Alpha:=Alpha shr 3;
        end;
        else begin
         a:=$00;
        end;
       end;
       p:=@pansichar(ImageData)[(((y+by)*ImageWidth)+(x+bx))*4];
       v:=Colors[Bits and 3] or (a shl 24);
       r:=(v shr 16) and $ff;
       g:=(v shr 8) and $ff;
       b:=(v shr 0) and $ff;
       a:=(v shr 24) and $ff;
       if PremultiplyAlpha and (a<>0) then begin
        // Unpremultiply
        r:=(r*255) div a;
        g:=(g*255) div a;
        b:=(b*255) div a;
        if r>255 then begin
         r:=255;
        end;
        if g>255 then begin
         g:=255;
        end;
        if b>255 then begin
         b:=255;
        end;
       end;
       byte(p[0]):=r;
       byte(p[1]):=g;
       byte(p[2]):=b;
       byte(p[3]):=a;
       Bits:=Bits shr 2;
      end;
     end;
     inc(x,4);
    end;
    inc(y,4);
   end;
  end;
 end;
const KTX_GL_UNPACK_ALIGNMENT=4;
      GL_ETC1_RGB8_OES=$8d64;
var LevelIndex,FaceIndex:longint;
    PreviousUnpackAlignment:glInt;
    Target:longword;
    MipmapLevel:PGLImageMipmapLevel;
    Face:PGLImageFace;
begin
 if not Uploaded then begin
  Uploaded:=true;
  glGetIntegerv(GL_UNPACK_ALIGNMENT,@PreviousUnpackAlignment);
	if PreviousUnpackAlignment<>KTX_GL_UNPACK_ALIGNMENT then begin
   glPixelStorei(GL_UNPACK_ALIGNMENT,KTX_GL_UNPACK_ALIGNMENT);
	end;
  glBindTexture(GLTarget,Handle);
  if GenerateMipMaps and not assigned(glGenerateMipmap) then begin
   glTexParameteri(GLTarget,GL_GENERATE_MIPMAP,1);
  end;
  for LevelIndex:=0 to longint(NumberOfMipmapLevels)-1 do begin
   MipmapLevel:=@MipmapLevels[LevelIndex];
   if GLTarget=GL_TEXTURE_CUBE_MAP then begin
    Target:=GL_TEXTURE_CUBE_MAP_POSITIVE_X;
   end else begin
    Target:=GLTarget;
   end;
   for FaceIndex:=0 to longint(NumberOfFaces)-1 do begin
    Face:=@MipmapLevel^.Faces[FaceIndex];
    case TextureDimensions of
     1:begin
      if Compressed then begin
       glCompressedTexImage1D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,0,Face^.DataSize,@Face^.Data[0]);
      end else begin
       glTexImage1D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,0,GLFormat,GLType,@Face^.Data[0]);
      end;
     end;
     2:begin
      if Compressed then begin
       if PremultiplyAlpha then begin
        Error:=GL_INVALID_ENUM;
       end else begin
        glCompressedTexImage2D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,Face^.PixelHeight,0,Face^.DataSize,@Face^.Data[0]);
        Error:=glGetError;
       end;
       case Error of
        GL_INVALID_ENUM,GL_INVALID_VALUE:begin
         case GLInternalFormat of
          GL_ETC1_RGB8_OES,GL_COMPRESSED_R11_EAC..GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC:begin
          end;
          GL_COMPRESSED_RGB_S3TC_DXT1_EXT:begin
           BlockSize:=8;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],1,false);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_RGBA,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_RGBA_S3TC_DXT1_EXT:begin
           BlockSize:=8;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],1,false);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_RGBA,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_RGBA_S3TC_DXT3_EXT:begin
           BlockSize:=16;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],3,PremultiplyAlpha);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_RGBA,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_RGBA_S3TC_DXT5_EXT:begin
           BlockSize:=16;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],5,PremultiplyAlpha);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_RGBA,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_SRGB_S3TC_DXT1_EXT:begin
           BlockSize:=8;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],1,false);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_SRGB8_ALPHA8,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT:begin
           BlockSize:=8;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],1,PremultiplyAlpha);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_SRGB8_ALPHA8,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT:begin
           BlockSize:=16;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],3,PremultiplyAlpha);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_SRGB8_ALPHA8,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
          GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT:begin
           BlockSize:=16;
           ImageData:=nil;
           try
            DecodeDXT(@Face^.Data[0],5,PremultiplyAlpha);
            glTexImage2D(Target+longword(FaceIndex),LevelIndex,GL_SRGB8_ALPHA8,Face^.PixelWidth,Face^.PixelHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
           finally
            if assigned(ImageData) then begin
             FreeMem(ImageData);
            end;
           end;
          end;
         end;
        end;
       end;
      end else begin
       glTexImage2D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,Face^.PixelHeight,0,GLFormat,GLType,@Face^.Data[0]);
      end;
     end;
     3:begin
      if Compressed then begin
       glCompressedTexImage3D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,Face^.PixelHeight,Face^.PixelDepth,0,Face^.DataSize,@Face^.Data[0]);
      end else begin
       glTexImage3D(Target+longword(FaceIndex),LevelIndex,GLInternalFormat,Face^.PixelWidth,Face^.PixelHeight,Face^.PixelDepth,0,GLFormat,GLType,@Face^.Data[0]);
      end;
     end;
    end;
   end;
  end;
  glBindTexture(GLTarget,0);
	if PreviousUnpackAlignment<>KTX_GL_UNPACK_ALIGNMENT then begin
   glPixelStorei(GL_UNPACK_ALIGNMENT,PreviousUnpackAlignment);
	end;
  if GenerateMipMaps and assigned(glGenerateMipmap) then begin
   glGenerateMipmap(GLTarget);
  end;
 end;
end;

procedure TGLImage.Unload(Handle:glUInt);
begin
 if Uploaded then begin
  Uploaded:=false;
 end;
end;

function LoadGLImage(DataPointer:pointer;DataSize:longword;SRGB:boolean):TGLImage;
begin
 result:=LoadPNGGLImage(DataPointer,DataSize,SRGB);
 if not assigned(result) then begin
  result:=LoadJPEGGLImage(DataPointer,DataSize,SRGB);
  if not assigned(result) then begin
  end;
 end;
end;

initialization
finalization
end.
