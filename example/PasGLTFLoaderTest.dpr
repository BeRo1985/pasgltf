program PasDAELoaderTest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

//  FastMM4,

uses
  SysUtils,
  Classes,
  Math,
  dglOpenGL in 'dglOpenGL.pas',
  UnitSDL2 in 'UnitSDL2.pas',
  UnitStaticLinking in 'UnitStaticLinking.pas',
  PasDblStrUtils in '..\externals\pasdblstrutils\src\PasDblStrUtils.pas',
  PasJSON in '..\externals\pasjson\src\PasJSON.pas',
  PasGLTF in '..\src\PasGLTF.pas',
  UnitGLTFOpenGL in 'UnitGLTFOpenGL.pas',
  UnitOpenGLImage in 'UnitOpenGLImage.pas',
  UnitOpenGLImageJPEG in 'UnitOpenGLImageJPEG.pas',
  UnitOpenGLImagePNG in 'UnitOpenGLImagePNG.pas',
  UnitMath3D in 'UnitMath3D.pas',
  UnitOpenGLShader in 'UnitOpenGLShader.pas',
  UnitOpenGLPBRShader in 'UnitOpenGLPBRShader.pas',
  UnitOpenGLFrameBufferObject in 'UnitOpenGLFrameBufferObject.pas',
  UnitOpenGLBRDFLUTShader in 'UnitOpenGLBRDFLUTShader.pas',
  UnitOpenGLEnvMapFilterShader in 'UnitOpenGLEnvMapFilterShader.pas',
  UnitOpenGLEnvMapDrawShader in 'UnitOpenGLEnvMapDrawShader.pas',
  UnitOpenGLAntialiasingShader in 'UnitOpenGLAntialiasingShader.pas',
  UnitOpenGLHDRToLDRShader in 'UnitOpenGLHDRToLDRShader.pas';

// Force usage of dedicated GPU for OpenGL with Delphi and FreePascal/Lazarus on Multi-GPU systems such as Notebooks on Windows
// Insert that into your main source file, which is for example the .dpr (Delphi) or .lpr (Lazarus) file

{$define ForceDedicatedGPUUsage} // then you can uncomment and recomment this line, for compile-time-switching between
                                 // integrated GPU and dedicated GPU

{$if defined(Windows) and defined(ForceDedicatedGPUUsage) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64))}
{$ifdef fpc}
 {$asmmode intel}
{$endif}
procedure NvOptimusEnablement; {$ifdef fpc}assembler; nostackframe;{$endif}
asm
{$ifdef cpu64}
{$ifndef fpc}
 .NOFRAME
{$endif}
{$endif}
 dd 1
end;

procedure AmdPowerXpressRequestHighPerformance; {$ifdef fpc}assembler; nostackframe;{$endif}
asm
{$ifdef cpu64}
{$ifndef fpc}
 .NOFRAME
{$endif}
{$endif}
 dd 1
end;

exports NvOptimusEnablement,
        AmdPowerXpressRequestHighPerformance;
{$ifend}

var InputFileName:ansistring;

var fs:TFileStream;
    ms:TMemoryStream;
    StartPerformanceCounter:Int64=0;

    GLTFDocument:TPasGLTF.TDocument;

    GLTFOpenGL:TGLTFOpenGL;

    PBRShaders:array[0..1] of TPBRShader;

    BRDFLUTShader:TBRDFLUTShader;

    BRDFLUTFBO:TFBO;

    EnvMapFilterShader:TEnvMapFilterShader;

    EnvMapFBO:TFBO;

    EnvMapDrawShader:TEnvMapDrawShader;

    HDRSceneFBO:TFBO;

    HDRToLDRShader:THDRToLDRShader;

    LDRSceneFBO:TFBO;

    AntialiasingShader:TAntialiasingShader;

    EmptyVertexArrayObjectHandle:glUInt;

    EnvMapTextureHandle:glUInt=0;

procedure Main;
const Title='PasGLTF loader test';
      VirtualCanvasWidth=1280;
      VirtualCanvasHeight=720;
var Event:TSDL_Event;
    SurfaceWindow:PSDL_Window;
    SurfaceContext:PSDL_GLContext;
    SDLDisplayMode:TSDL_DisplayMode;
    VideoFlags:longword;
    SDLWaveFormat:TSDL_AudioSpec;
    BufPosition:integer;
    ScreenWidth,ScreenHeight,BestWidth,BestHeight,ViewPortWidth,ViewPortHeight,ViewPortX,ViewPortY:int32;
    Fullscreen:boolean;
    ShowCursor:boolean;
    SDLRunning,OldShowCursor:boolean;
    Time:double;
 procedure Draw;
 var ModelMatrix,ViewMatrix,ProjectionMatrix,InverseViewProjectionMatrix:UnitMath3D.TMatrix4x4;
     LightDirection,Bounds,Center:UnitMath3D.TVector3;
     t:double;
     v:TPasGLTFFloat;
     PBRShader:TPBRShader;
 begin
  begin
   glBindFrameBuffer(GL_FRAMEBUFFER,HDRSceneFBO.FBOs[0]);
   glDrawBuffer(GL_COLOR_ATTACHMENT0);
   glViewport(0,0,HDRSceneFBO.Width,HDRSceneFBO.Height);
   glClearColor(0.0,0.0,0.0,0.0);
   glClearDepth(1.0);
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   ModelMatrix:=Matrix4x4Identity;
   t:=Time*0.125;
   Center.x:=(GLTFOpenGL.StaticMinPosition[0]+GLTFOpenGL.StaticMaxPosition[0])*0.5;
   Center.y:=(GLTFOpenGL.StaticMinPosition[1]+GLTFOpenGL.StaticMaxPosition[1])*0.5;
   Center.z:=(GLTFOpenGL.StaticMinPosition[2]+GLTFOpenGL.StaticMaxPosition[2])*0.5;
   Bounds.x:=(GLTFOpenGL.StaticMaxPosition[0]-GLTFOpenGL.StaticMinPosition[0])*0.5;
   Bounds.y:=(GLTFOpenGL.StaticMaxPosition[1]-GLTFOpenGL.StaticMinPosition[1])*0.5;
   Bounds.z:=(GLTFOpenGL.StaticMaxPosition[2]-GLTFOpenGL.StaticMinPosition[2])*0.5;
   ViewMatrix:=Matrix4x4LookAt(Vector3Add(Center,
                                          Vector3(sin(t)*Max(Max(Bounds.x,Bounds.y),Bounds.z)*2.828,
                                                  sin(t*0.25)*Max(Max(Bounds.x,Bounds.y),Bounds.z)*0.25,
                                                  cos(t)*Max(Max(Bounds.x,Bounds.y),Bounds.z)*2.828)),
                                       Center,
                                       Vector3YAxis);
   v:=Max(1024.0,sqrt(sqr(Bounds.x)+sqr(Bounds.z))*4.0);
   ProjectionMatrix:=Matrix4x4Perspective(45.0,ViewPortWidth/ViewPortHeight,v/1024.0,v);
   LightDirection:=Vector3Norm(Vector3(0.5,-1.0,-1.0));
   InverseViewProjectionMatrix:=Matrix4x4TermInverse(Matrix4x4TermMul(ViewMatrix,ProjectionMatrix));
   begin
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D,EnvMapTextureHandle);
    EnvMapDrawShader.Bind;
    glUniform1i(EnvMapDrawShader.uTexture,0);
    glUniformMatrix4fv(EnvMapDrawShader.uInverseViewProjectionMatrix,1,false,@InverseViewProjectionMatrix);
    glBindVertexArray(EmptyVertexArrayObjectHandle);
    glDrawArrays(GL_TRIANGLES,0,3);
    glBindVertexArray(0);
    EnvMapDrawShader.Unbind;
   end;
   begin
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D,BRDFLUTFBO.TextureHandles[0]);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D,EnvMapFBO.TextureHandles[0]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LEQUAL);
    glCullFace(GL_BACK);
    for PBRShader in PBRShaders do begin
     PBRShader.Bind;
     glUniform3fv(PBRShader.uLightDirection,1,@LightDirection);
     glUniform1i(PBRShader.uBRDFLUTTexture,5);
     glUniform1i(PBRShader.uEnvMapTexture,6);
     glUniform1i(PBRShader.uEnvMapMaxLevel,Min(EnvMapFBO.WorkMaxLevel,16));
     PBRShader.Unbind;
    end;
    GLTFOpenGL.Draw(TPasGLTF.TMatrix4x4(Pointer(@ModelMatrix)^),
                    TPasGLTF.TMatrix4x4(Pointer(@ViewMatrix)^),
                    TPasGLTF.TMatrix4x4(Pointer(@ProjectionMatrix)^),
                    PBRShaders[0],
                    PBRShaders[1],
                    0,
                    Time);
   end;
  end;
  begin
   glBindFrameBuffer(GL_FRAMEBUFFER,LDRSceneFBO.FBOs[0]);
   glDrawBuffer(GL_COLOR_ATTACHMENT0);
   glViewport(0,0,LDRSceneFBO.Width,LDRSceneFBO.Height);
   glClearColor(0.0,0.0,0.0,0.0);
   glClear(GL_COLOR_BUFFER_BIT);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_CULL_FACE);
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_2D,HDRSceneFBO.TextureHandles[0]);
   HDRToLDRShader.Bind;
   glUniform1i(HDRToLDRShader.uTexture,0);
   glBindVertexArray(EmptyVertexArrayObjectHandle);
   glDrawArrays(GL_TRIANGLES,0,3);
   glBindVertexArray(0);
   HDRToLDRShader.Unbind;
  end;
  begin
   glBindFrameBuffer(GL_FRAMEBUFFER,0);
   glViewport(0,0,ViewPortWidth,ViewPortHeight);
   glClearColor(0.0,0.0,0.0,0.0);
   glClearDepth(1.0);
   glViewport(ViewPortX,ViewPortY,ViewPortWidth,ViewPortHeight);
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_CULL_FACE);
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_2D,LDRSceneFBO.TextureHandles[0]);
   AntialiasingShader.Bind;
   glUniform1i(AntialiasingShader.uTexture,0);
   glBindVertexArray(EmptyVertexArrayObjectHandle);
   glDrawArrays(GL_TRIANGLES,0,3);
   glBindVertexArray(0);
   AntialiasingShader.Unbind;
  end;
 end;
 procedure Resize(NewWidth,NewHeight:longint);
 var Factor:int64;
     rw,rh:longint;
 begin
  ScreenWidth:=NewWidth;
  ScreenHeight:=NewHeight;
  begin
   Factor:=int64($100000000);
   rw:=VirtualCanvasWidth;
   rh:=VirtualCanvasHeight;
   while (max(rw,rh)>=128) and (((rw or rh)<>0) and (((rw or rh) and 1)=0)) do begin
    rw:=rw shr 1;
    rh:=rh shr 1;
   end;
   if ScreenWidth<ScreenHeight then begin
    ViewPortWidth:=((ScreenHeight*rw)+((rh+1) div 2)) div rh;
    ViewPortHeight:=ScreenHeight;
    if ViewPortWidth>ScreenWidth then begin
     Factor:=((ScreenWidth*int64($100000000))+(ViewPortWidth div 2)) div ViewPortWidth;
    end;
   end else begin
    ViewPortWidth:=ScreenWidth;
    ViewPortHeight:=((ScreenWidth*rh)+((rw+1) div 2)) div rw;
    if ViewPortHeight>ScreenHeight then begin
     Factor:=((ScreenHeight*int64($100000000))+(ViewPortHeight div 2)) div ViewPortHeight;
    end;
   end;
   if Factor<int64($100000000) then begin
    ViewPortWidth:=((ViewPortWidth*Factor)+int64($80000000)) div int64($100000000);
    ViewPortHeight:=((ViewPortHeight*Factor)+int64($80000000)) div int64($100000000);
   end;
   if ViewPortWidth<rw then begin
    ViewPortWidth:=rw;
   end;
   if ViewPortHeight<rh then begin
    ViewPortHeight:=rh;
   end;
   ViewPortX:=((ScreenWidth-ViewPortWidth)+1) div 2;
   ViewPortY:=((ScreenHeight-ViewPortHeight)+1) div 2;
  end;
 end;
var Index,MultiSampleCounter,DepthBufferSizeCounter,Temp:int32;
    MemoryStream:TMemoryStream;
    ImageData:TPasGLTFPointer;
    ImageWidth,ImageHeight:TPasGLTFInt32;
    OK:boolean;
begin

 //FastMM4.FullDebugModeScanMemoryPoolBeforeEveryOperation:=true;

 if SDL_Init(SDL_INIT_EVERYTHING)<0 then begin
  exit;
 end;

 ScreenWidth:=1280;
 ScreenHeight:=720;

 if SDL_GetCurrentDisplayMode(0,@SDLDisplayMode)=0 then begin
  BestWidth:=SDLDisplayMode.w;
  BestHeight:=SDLDisplayMode.h;
 end else begin
  BestWidth:=640;
  BestHeight:=360;
 end;

 if ScreenWidth>=((BestWidth*90) div 100) then begin
  Temp:=((BestWidth*90) div 100);
  ScreenHeight:=(ScreenHeight*Temp) div ScreenWidth;
  ScreenWidth:=Temp;
 end;
 if ScreenHeight>=((BestHeight*90) div 100) then begin
  Temp:=((BestHeight*90) div 100);
  ScreenWidth:=(ScreenWidth*Temp) div ScreenHeight;
  ScreenHeight:=Temp;
 end;

 Resize(ScreenWidth,ScreenHeight);

 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,3);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,3);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,SDL_GL_CONTEXT_PROFILE_CORE);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS,0);
 SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,0);
 SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,0);
 SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,1);
 SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE,0);
 SDL_GL_SetSwapInterval(1);
 VideoFlags:=0;
 if paramstr(1)='f' then begin
  VideoFlags:=VideoFlags or SDL_WINDOW_FULLSCREEN;
  Fullscreen:=true;
  ScreenWidth:=1280;
  ScreenHeight:=720;
 end;
 for Index:=0 downto 0 do begin
  DepthBufferSizeCounter:=3;
  MultiSampleCounter:=0;
  writeln(DepthBufferSizeCounter shl 3,' ',1 shl MultiSampleCounter);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,DepthBufferSizeCounter shl 3);
  if MultiSampleCounter=0 then begin
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,0);
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,0);
  end else begin
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,1);
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,1 shl MultiSampleCounter);
  end;
  SurfaceWindow:=SDL_CreateWindow(pansichar(Title),(BestWidth-ScreenWidth) div 2,(BestHeight-ScreenHeight) div 2,ScreenWidth,ScreenHeight,SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE or VideoFlags);
  if assigned(SurfaceWindow) then begin
// SDL_EventState(SDL_DROPFILE,SDL_ENABLE);
   SurfaceContext:=SDL_GL_CreateContext(SurfaceWindow);
   if not assigned(SurfaceContext) then begin
    SDL_DestroyWindow(SurfaceWindow);
    SurfaceWindow:=nil;
    if Index=0 then begin
     exit;
    end else begin
     continue;
    end;
   end;
  end else begin
   exit;
  end;
  OK:=false;
  if InitOpenGL then begin
   ReadOpenGLCore;
   ReadImplementationProperties;
   ReadExtensions;
   OK:=true;
  end;
  if not (OK and assigned(glGenVertexArrays)) then begin
   if assigned(SurfaceContext) then begin
    SDL_GL_DeleteContext(SurfaceContext);
    SurfaceContext:=nil;
   end;
   SDL_DestroyWindow(SurfaceWindow);
   SurfaceWindow:=nil;
   if Index=0 then begin
    exit;
   end else begin
    continue;
   end;
  end;
  break;
 end;

 SDL_GL_SetSwapInterval(1);

 SDL_ShowCursor(0);

 StartPerformanceCounter:=SDL_GetPerformanceCounter;

 glGenVertexArrays(1,@EmptyVertexArrayObjectHandle);
 try

  BRDFLUTShader:=TBRDFLUTShader.Create;
  try

   FillChar(BRDFLUTFBO,SizeOf(TFBO),#0);
   BRDFLUTFBO.Width:=512;
   BRDFLUTFBO.Height:=512;
   BRDFLUTFBO.Depth:=0;
   BRDFLUTFBO.Textures:=1;
   BRDFLUTFBO.TextureFormats[0]:=GL_TEXTURE_RGBA16F;
   BRDFLUTFBO.Format:=GL_TEXTURE_RGBA16F;
   BRDFLUTFBO.SWrapMode:=wmGL_CLAMP_TO_EDGE;
   BRDFLUTFBO.TWrapMode:=wmGL_CLAMP_TO_EDGE;
   BRDFLUTFBO.RWrapMode:=wmGL_CLAMP_TO_EDGE;
   BRDFLUTFBO.MinFilterMode:=fmGL_LINEAR;
   BRDFLUTFBO.MagFilterMode:=fmGL_LINEAR;
   BRDFLUTFBO.Flags:=0;
   CreateFrameBuffer(BRDFLUTFBO);
   glBindFrameBuffer(GL_FRAMEBUFFER,BRDFLUTFBO.FBOs[0]);
   glDrawBuffer(GL_COLOR_ATTACHMENT0);
   glViewport(0,0,BRDFLUTFBO.Width,BRDFLUTFBO.Height);
   glClearColor(0.0,0.0,0.0,0.0);
   glClearDepth(1.0);
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   glDisable(GL_DEPTH_TEST);
   glCullFace(GL_NONE);
   glBindVertexArray(EmptyVertexArrayObjectHandle);
   BRDFLUTShader.Bind;
   glDrawArrays(GL_TRIANGLES,0,3);
   BRDFLUTShader.Unbind;
   glBindVertexArray(0);
   glBindFrameBuffer(GL_FRAMEBUFFER,0);

  finally
   FreeAndNil(BRDFLUTShader);
  end;

  try

   EnvMapFilterShader:=TEnvMapFilterShader.Create;
   try
    FillChar(EnvMapFBO,SizeOf(TFBO),#0);
    EnvMapTextureHandle:=0;
    MemoryStream:=TMemoryStream.Create;
    try
     MemoryStream.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'equirectangularmap.jpg');
     ImageWidth:=2048;
     ImageHeight:=2048;
     if LoadImage(MemoryStream.Memory,MemoryStream.Size,ImageData,ImageWidth,ImageHeight) then begin
      try
       glGenTextures(1,@EnvMapTextureHandle);
       glBindTexture(GL_TEXTURE_2D,EnvMapTextureHandle);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
       glTexImage2D(GL_TEXTURE_2D,0,GL_SRGB8_ALPHA8,ImageWidth,ImageHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
       glGenerateMipmap(GL_TEXTURE_2D);
     finally
       FreeMem(ImageData);
      end;
     end;
    finally
     MemoryStream.Free;
    end;
    EnvMapFBO.Width:=ImageWidth;
    EnvMapFBO.Height:=ImageHeight;
    EnvMapFBO.Depth:=0;
    EnvMapFBO.Textures:=1;
    EnvMapFBO.TextureFormats[0]:=GL_TEXTURE_RGBA8UB;
    EnvMapFBO.Format:=GL_TEXTURE_RGBA8UB;
    EnvMapFBO.SWrapMode:=wmGL_REPEAT;
    EnvMapFBO.TWrapMode:=wmGL_REPEAT;
    EnvMapFBO.RWrapMode:=wmGL_REPEAT;
    EnvMapFBO.MinFilterMode:=fmGL_LINEAR;
    EnvMapFBO.MagFilterMode:=fmGL_LINEAR;
    EnvMapFBO.Flags:=FBOFlagMipMap or FBOFlagMipMapLevelWiseFill;
    CreateFrameBuffer(EnvMapFBO);
    EnvMapFilterShader.Bind;
    for Index:=0 to EnvMapFBO.WorkMaxLevel do begin
     glActiveTexture(GL_TEXTURE0);
     if Index=0 then begin
      glBindTexture(GL_TEXTURE_2D,EnvMapTextureHandle);
     end else begin
      glBindTexture(GL_TEXTURE_2D,EnvMapFBO.TextureHandles[0]);
     end;
     glUniform1i(EnvMapFilterShader.uTexture,0);
     glUniform1i(EnvMapFilterShader.uMipMapLevel,Index);
     glBindFrameBuffer(GL_FRAMEBUFFER,EnvMapFBO.FBOs[Index]);
     glDrawBuffer(GL_COLOR_ATTACHMENT0);
     glViewport(0,0,EnvMapFBO.Width shr Index,EnvMapFBO.Height shr Index);
     glClearColor(0.0,0.0,0.0,0.0);
     glClearDepth(1.0);
     glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
     glDisable(GL_DEPTH_TEST);
     glCullFace(GL_NONE);
     glBindVertexArray(EmptyVertexArrayObjectHandle);
     glDrawArrays(GL_TRIANGLES,0,3);
     glBindVertexArray(0);
     glBindFrameBuffer(GL_FRAMEBUFFER,0);
    end;
    EnvMapFilterShader.Unbind;
   finally
    FreeAndNil(EnvMapFilterShader);
   end;

   try

    EnvMapDrawShader:=TEnvMapDrawShader.Create;
    try

     FillChar(HDRSceneFBO,SizeOf(TFBO),#0);
     HDRSceneFBO.Width:=ViewPortWidth;
     HDRSceneFBO.Height:=ViewPortHeight;
     HDRSceneFBO.Depth:=0;
     HDRSceneFBO.Textures:=1;
     HDRSceneFBO.TextureFormats[0]:=GL_TEXTURE_RGBA16F;
     HDRSceneFBO.Format:=GL_TEXTURE_RGBA16F;
     HDRSceneFBO.SWrapMode:=wmGL_CLAMP_TO_EDGE;
     HDRSceneFBO.TWrapMode:=wmGL_CLAMP_TO_EDGE;
     HDRSceneFBO.RWrapMode:=wmGL_CLAMP_TO_EDGE;
     HDRSceneFBO.MinFilterMode:=fmGL_LINEAR;
     HDRSceneFBO.MagFilterMode:=fmGL_LINEAR;
     HDRSceneFBO.Flags:=FBOFlagDepthBuffer;
     CreateFrameBuffer(HDRSceneFBO);
     try

      HDRToLDRShader:=THDRToLDRShader.Create;
      try

       FillChar(LDRSceneFBO,SizeOf(TFBO),#0);
       LDRSceneFBO.Width:=ViewPortWidth;
       LDRSceneFBO.Height:=ViewPortHeight;
       LDRSceneFBO.Depth:=0;
       LDRSceneFBO.Textures:=1;
       LDRSceneFBO.TextureFormats[0]:=GL_TEXTURE_RGBA8UB;
       LDRSceneFBO.Format:=GL_TEXTURE_RGBA8UB;
       LDRSceneFBO.SWrapMode:=wmGL_CLAMP_TO_EDGE;
       LDRSceneFBO.TWrapMode:=wmGL_CLAMP_TO_EDGE;
       LDRSceneFBO.RWrapMode:=wmGL_CLAMP_TO_EDGE;
       LDRSceneFBO.MinFilterMode:=fmGL_LINEAR;
       LDRSceneFBO.MagFilterMode:=fmGL_LINEAR;
       LDRSceneFBO.Flags:=FBOFlagDepthBuffer;
       CreateFrameBuffer(LDRSceneFBO);
       try

        AntialiasingShader:=TAntialiasingShader.Create;
        try

         GLTFOpenGL:=TGLTFOpenGL.Create(GLTFDocument);
         try

          GLTFOpenGL.InitializeResources;
          try

           GLTFOpenGL.UploadResources;
           try

            PBRShaders[0]:=TPBRShader.Create(false);
            try

             PBRShaders[1]:=TPBRShader.Create(true);
             try

              FullScreen:=false;
              SDLRunning:=true;
              while SDLRunning do begin

               while SDL_PollEvent(@Event)<>0 do begin
                case Event.type_ of
                 SDL_QUITEV,SDL_APP_TERMINATING:begin
                  SDLRunning:=false;
                  break;
                 end;
                 SDL_APP_WILLENTERBACKGROUND:begin
                  //SDL_PauseAudio(1);
                 end;
                 SDL_APP_DIDENTERFOREGROUND:begin
                  //SDL_PauseAudio(0);
                 end;
                 SDL_RENDER_TARGETS_RESET,SDL_RENDER_DEVICE_RESET:begin
                 end;
                 SDL_KEYDOWN:begin
                  case Event.key.keysym.sym of
                   SDLK_ESCAPE:begin
             //     BackKey;
                    SDLRunning:=false;
                    break;
                   end;
                   SDLK_RETURN:begin
                    if (Event.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
                     FullScreen:=not FullScreen;
                     if FullScreen then begin
                      SDL_SetWindowFullscreen(SurfaceWindow,SDL_WINDOW_FULLSCREEN_DESKTOP);
                     end else begin
                      SDL_SetWindowFullscreen(SurfaceWindow,0);
                     end;
                    end;
                   end;
                   SDLK_F4:begin
                    if (Event.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
                     SDLRunning:=false;
                     break;
                    end;
                   end;
                  end;
                 end;
                 SDL_KEYUP:begin
                 end;
                 SDL_WINDOWEVENT:begin
                  case event.window.event of
                   SDL_WINDOWEVENT_RESIZED:begin
                    ScreenWidth:=event.window.Data1;
                    ScreenHeight:=event.window.Data2;
                    Resize(ScreenWidth,ScreenHeight);
                   end;
                  end;
                 end;
                 SDL_MOUSEMOTION:begin
                  if (event.motion.xrel<>0) or (event.motion.yrel<>0) then begin
                  end;
                 end;
                 SDL_MOUSEBUTTONDOWN:begin
                  case event.button.button of
                   SDL_BUTTON_LEFT:begin
                   end;
                   SDL_BUTTON_RIGHT:begin
                   end;
                  end;
                 end;
                 SDL_MOUSEBUTTONUP:begin
                  case event.button.button of
                   SDL_BUTTON_LEFT:begin
                   end;
                   SDL_BUTTON_RIGHT:begin
                   end;
                  end;
                 end;
                end;
               end;
               Time:=(SDL_GetPerformanceCounter-StartPerformanceCounter)/SDL_GetPerformanceFrequency;
               if (HDRSceneFBO.Width<>VirtualCanvasWidth) or
                  (HDRSceneFBO.Height<>VirtualCanvasHeight) then begin
                DestroyFrameBuffer(HDRSceneFBO);
                HDRSceneFBO.Width:=VirtualCanvasWidth;
                HDRSceneFBO.Height:=VirtualCanvasHeight;
                CreateFrameBuffer(HDRSceneFBO);
               end;
               if (LDRSceneFBO.Width<>VirtualCanvasWidth) or
                  (LDRSceneFBO.Height<>VirtualCanvasHeight) then begin
                DestroyFrameBuffer(LDRSceneFBO);
                LDRSceneFBO.Width:=VirtualCanvasWidth;
                LDRSceneFBO.Height:=VirtualCanvasHeight;
                CreateFrameBuffer(LDRSceneFBO);
               end;
               Draw;
               SDL_GL_SwapWindow(SurfaceWindow);
              end;

             finally
              PBRShaders[1].Free;
             end;

            finally
             PBRShaders[0].Free;
            end;

           finally
            GLTFOpenGL.UnloadResources;
           end;

          finally
           GLTFOpenGL.FinalizeResources;
          end;

         finally
          GLTFOpenGL.Free;
         end;

        finally
         FreeAndNil(AntialiasingShader);
        end;

       finally
        DestroyFrameBuffer(LDRSceneFBO);
       end;

      finally
       FreeAndNil(HDRToLDRShader);
      end;

     finally
      DestroyFrameBuffer(HDRSceneFBO);
     end;

    finally
     EnvMapDrawShader.Free;
    end;

   finally
    DestroyFrameBuffer(EnvMapFBO);
   end;

  finally
   DestroyFrameBuffer(BRDFLUTFBO);
  end;

  if EnvMapTextureHandle>0 then begin
   glDeleteTextures(1,@EnvMapTextureHandle);
  end;

 finally
  glDeleteVertexArrays(1,@EmptyVertexArrayObjectHandle);
 end;

 if assigned(SurfaceContext) then begin
  SDL_GL_DeleteContext(SurfaceContext);
  SurfaceContext:=nil;
 end;
 if assigned(SurfaceWindow) then begin
  SDL_DestroyWindow(SurfaceWindow);
  SurfaceWindow:=nil;
 end;

 SDL_Quit;

end;

var ofs:TFileStream;
begin
 try
  if ParamCount>0 then begin
   InputFileName:=AnsiString(ParamStr(1));

   fs:=TFileStream.Create(String(InputFileName),fmOpenRead or fmShareDenyWrite);
   try
    ms:=TMemoryStream.Create;
    try
     ms.SetSize(fs.Size);
     fs.Seek(0,soBeginning);
     ms.CopyFrom(fs,fs.Size);
     ms.Seek(0,soBeginning);
     GLTFDocument:=TPasGLTF.TDocument.Create(nil);
     try
      GLTFDocument.RootPath:=ExtractFilePath(InputFileName);
      GLTFDocument.LoadFromStream(ms);
{     ofs:=TFileStream.Create('output.gltf',fmCreate);
      try
       GLTFDocument.SaveToStream(ofs,false,true);
      finally
       ofs.Free;
      end;
      ofs:=TFileStream.Create('output.glb',fmCreate);
      try
       GLTFDocument.SaveToStream(ofs,true,false);
      finally
       ofs.Free;
      end;}
      Main;
     finally
      FreeAndNil(GLTFDocument);
     end;
    finally
     ms.Free;
    end;
   finally
    fs.Free;
   end;
  end;
 finally
 end;
end.

