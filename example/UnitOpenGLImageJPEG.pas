unit UnitOpenGLImageJPEG;
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

{$ifdef fpc}
uses SysUtils,Classes,FPImage,FPReadJPEG,UnitOpenGLImage,dglOpenGL;
{$else}
uses SysUtils,Classes,JPEG,Graphics,UnitOpenGLImage,dglOpenGL;
{$endif}

function LoadJPEGImage(DataPointer:pointer;DataSize:longword;var ImageData:pointer;var ImageWidth,ImageHeight:longint):boolean;

function LoadJPEGGLImage(DataPointer:pointer;DataSize:longword;SRGB:boolean):TGLImage;

implementation

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

function LoadJPEGImage(DataPointer:pointer;DataSize:longword;var ImageData:pointer;var ImageWidth,ImageHeight:longint):boolean;
{$ifdef fpc}
var Image:TFPMemoryImage;
    ReaderJPEG:TFPReaderJPEG;
    Stream:TMemoryStream;
    y,x:longint;
    c:TFPColor;
    pout:PAnsiChar;
begin
 result:=false;
 try
  Stream:=TMemoryStream.Create;
  try
   if (DataSize>2) and (((byte(PAnsiChar(pointer(DataPointer))[0]) xor $ff)=0) and ((byte(PAnsiChar(pointer(DataPointer))[1]) xor $d8)=0)) then begin
    if Stream.Write(DataPointer^,DataSize)=longint(DataSize) then begin
     if Stream.Seek(0,soFromBeginning)=0 then begin
      Image:=TFPMemoryImage.Create(20,20);
      try
       ReaderJPEG:=TFPReaderJPEG.Create;
       try
        Image.LoadFromStream(Stream,ReaderJPEG);
        ImageWidth:=Image.Width;
        ImageHeight:=Image.Height;
        GetMem(ImageData,ImageWidth*ImageHeight*4);
        pout:=ImageData;
        for y:=0 to ImageHeight-1 do begin
         for x:=0 to ImageWidth-1 do begin
          c:=Image.Colors[x,y];
          pout[0]:=ansichar(byte((c.red shr 8) and $ff));
          pout[1]:=ansichar(byte((c.green shr 8) and $ff));
          pout[2]:=ansichar(byte((c.blue shr 8) and $ff));
          pout[3]:=AnsiChar(#$ff);
          inc(pout,4);
         end;
        end;
        result:=true;
       finally
        ReaderJPEG.Free;
       end;
      finally
       Image.Free;
      end;
     end;
    end;
   end;
  finally
   Stream.Free;
  end;
 except
  result:=false;
 end;
end;
{$else}
{$ifdef delphi}
var Stream:TMemoryStream;
    JPEG:TJPEGImage;
    BMP:TBitmap;
    y,x:longint;
    pin,pout:PAnsiChar;
begin
 result:=false;
 try
  if (DataSize>2) and (((byte(PAnsiChar(pointer(DataPointer))[0]) xor $ff)=0) and ((byte(PAnsiChar(pointer(DataPointer))[1]) xor $d8)=0)) then begin
   Stream:=TMemoryStream.Create;
   try
    if Stream.Write(DataPointer^,DataSize)=longint(DataSize) then begin
     if Stream.Seek(0,soFromBeginning)=0 then begin
      JPEG:=TJPEGImage.Create;
      try
       JPEG.LoadFromStream(Stream);
       BMP:=TBitmap.Create;
       try
        BMP.Pixelformat:=pf32bit;
        BMP.HandleType:=bmDIB;
        BMP.Width:=JPEG.Width;
        BMP.Height:=JPEG.Height;
        BMP.Canvas.Draw(0,0,JPEG);
        ImageWidth:=BMP.Width;
        ImageHeight:=BMP.Height;
        GetMem(ImageData,ImageWidth*ImageHeight*4);
        pout:=ImageData;
        for y:=0 to ImageHeight-1 do begin
         pin:=BMP.Scanline[y];
         for x:=0 to ImageWidth-1 do begin
          pout[0]:=pin[2];
          pout[1]:=pin[1];
          pout[2]:=pin[0];
          pout[3]:=AnsiChar(#$ff);
          inc(pin,4);
          inc(pout,4);
         end;
        end;
        result:=true;
       finally
        BMP.Free;
       end;
      finally
       JPEG.Free;
      end;
     end;
    end;
   finally
    Stream.Free;
   end;
  end;
 except
  result:=false;
 end;
end;
{$else}
begin
 result:=false;
end;
{$endif}
{$endif}    

function LoadJPEGGLImage(DataPointer:pointer;DataSize:longword;SRGB:boolean):TGLImage;
var ImageData:pointer;
    ImageWidth,ImageHeight:longint;
begin
 result:=nil;
 ImageData:=nil;
 if LoadJPEGImage(DataPointer,DataSize,ImageData,ImageWidth,ImageHeight) then begin
  if assigned(ImageData) then begin
   try
    result:=TGLImage.Create;
    result.TextureDimensions:=2;
    result.GLTarget:=GL_TEXTURE_2D;
    result.Compressed:=false;
    result.GenerateMipmaps:=true;
    result.DoSwizzle:=false;
    result.GLType:=GL_UNSIGNED_BYTE;
    result.GLTypeSize:=1;
    result.GLFormat:=GL_RGBA;
    if SRGB then begin
     result.GLInternalFormat:=GL_SRGB8_ALPHA8;
    end else begin
     result.GLInternalFormat:=GL_RGBA;
    end;
    result.GLBaseInternalFormat:=GL_RGBA;
    result.PixelWidth:=ImageWidth;
    result.PixelHeight:=ImageHeight;
    result.PixelDepth:=1;
    result.NumberOfArrayElements:=1;
    result.NumberOfFaces:=1;
    result.NumberOfMipmapLevels:=1;
    result.BytesOfKeyValueData:=0;
    SetLength(result.MipMapLevels,1);
    result.MipMapLevels[0].ImageSize:=ImageWidth*ImageHeight*4;
    result.MipMapLevels[0].PixelWidth:=result.PixelWidth;
    result.MipMapLevels[0].PixelHeight:=result.PixelHeight;
    result.MipMapLevels[0].PixelDepth:=result.PixelDepth;
    SetLength(result.MipMapLevels[0].Faces,1);
    result.MipMapLevels[0].Faces[0].PixelWidth:=result.PixelWidth;
    result.MipMapLevels[0].Faces[0].PixelHeight:=result.PixelHeight;
    result.MipMapLevels[0].Faces[0].PixelDepth:=result.PixelDepth;
    result.MipMapLevels[0].Faces[0].DataSize:=ImageWidth*ImageHeight*4;
    SetLength(result.MipMapLevels[0].Faces[0].Data,(result.MipMapLevels[0].Faces[0].DataSize+3) and not 3);
    Move(ImageData^,result.MipMapLevels[0].Faces[0].Data[0],result.MipMapLevels[0].Faces[0].DataSize);
   finally
    FreeMem(ImageData);
   end;
  end;
 end;
end;

initialization
finalization
end.
