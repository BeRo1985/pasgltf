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
  UnitOpenGLBRDFLUTShader in 'UnitOpenGLBRDFLUTShader.pas';

var InputFileName:ansistring;

var fs:TFileStream;
    ms:TMemoryStream;
    StartPerformanceCounter:Int64=0;

    GLTFDocument:TPasGLTF.TDocument;

    GLTFOpenGL:TGLTFOpenGL;

    PBRShader:TPBRShader;

    BRDFLUTLitShader:TBRDFLUTShader;

    BRDFLUTFBO:TFBO;

    EmptyVertexArrayObjectHandle:glUInt;

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
    ScreenWidth,ScreenHeight,BestWidth,BestHeight,ViewPortWidth,ViewPortHeight,ViewPortX,ViewPortY,k:longint;
    Fullscreen:boolean;
    ShowCursor:boolean;
    SDLRunning,OldShowCursor:boolean;
    Time:double;
 procedure Draw;
 var ModelMatrix,ViewMatrix,ProjectionMatrix:UnitMath3D.TMatrix4x4;
     LightDirection:UnitMath3D.TVector3;
     t:double;
 begin
  glViewport(0,0,ViewPortWidth,ViewPortHeight);
  glClearColor(0.0,0.0,0.0,0.0);
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glCullFace(GL_NONE);
  ModelMatrix:=Matrix4x4Identity;
  t:=Time;
  ViewMatrix:=Matrix4x4LookAt(Vector3(sin(t)*4.0,sin(t*0.25)*4.0,cos(t)*4.0),Vector3Origin,Vector3YAxis);
  ProjectionMatrix:=Matrix4x4Perspective(45.0,ViewPortWidth/ViewPortHeight,0.1,128.0);
  LightDirection:=Vector3Norm(Vector3(0.25,-0.5,-1.0));
  PBRShader.Bind;
  glUniform3fv(PBRShader.uLightDirection,1,@LightDirection);
  GLTFOpenGL.Draw(TPasGLTF.TMatrix4x4(Pointer(@ModelMatrix)^),
                  TPasGLTF.TMatrix4x4(Pointer(@ViewMatrix)^),
                  TPasGLTF.TMatrix4x4(Pointer(@ProjectionMatrix)^),
                  PBRShader);
  PBRShader.Unbind;
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
  k:=((BestWidth*90) div 100);
  ScreenHeight:=(ScreenHeight*k) div ScreenWidth;
  ScreenWidth:=k;
 end;
 if ScreenHeight>=((BestHeight*90) div 100) then begin
  k:=((BestHeight*90) div 100);
  ScreenWidth:=(ScreenWidth*k) div ScreenHeight;
  ScreenHeight:=k;
 end;

 Resize(ScreenWidth,ScreenHeight);

 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,3);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,3);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,SDL_GL_CONTEXT_PROFILE_CORE);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS,0);
 SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,0);
 SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,0);
 SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,1);
 SDL_GL_SetSwapInterval(1);
 VideoFlags:=0;
 if paramstr(1)='f' then begin
  VideoFlags:=VideoFlags or SDL_WINDOW_FULLSCREEN;
  Fullscreen:=true;
  ScreenWidth:=1280;
  ScreenHeight:=720;
 end;
 for k:={4}0 downto 0 do begin
  if k=0 then begin
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,0);
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,0);
  end else begin
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS,1);
   SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES,1 shl k);
  end;
  SurfaceWindow:=SDL_CreateWindow(pansichar(Title),(BestWidth-ScreenWidth) div 2,(BestHeight-ScreenHeight) div 2,ScreenWidth,ScreenHeight,SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE or VideoFlags);
  if assigned(SurfaceWindow) then begin
// SDL_EventState(SDL_DROPFILE,SDL_ENABLE);
   SurfaceContext:=SDL_GL_CreateContext(SurfaceWindow);
   if not assigned(SurfaceContext) then begin
    SDL_DestroyWindow(SurfaceWindow);
    SurfaceWindow:=nil;
    if k=0 then begin
     exit;
    end else begin
     continue;
    end;
   end;
  end else begin
   exit;
  end;
  if InitOpenGL then begin
   ReadOpenGLCore;
   ReadImplementationProperties;
   ReadExtensions;
  end else begin
   if assigned(SurfaceContext) then begin
    SDL_GL_DeleteContext(SurfaceContext);
    SurfaceContext:=nil;
   end;
   SDL_DestroyWindow(SurfaceWindow);
   SurfaceWindow:=nil;
   if k=0 then begin
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

  BRDFLUTLitShader:=TBRDFLUTShader.Create;
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
   BRDFLUTLitShader.Bind;
   glDrawArrays(GL_TRIANGLES,0,3);
   BRDFLUTLitShader.Unbind;
   glBindVertexArray(0);
   glBindFrameBuffer(GL_FRAMEBUFFER,0);

  finally
   FreeAndNil(BRDFLUTLitShader);
  end;

  GLTFOpenGL:=TGLTFOpenGL.Create(GLTFDocument);
  try

   GLTFOpenGL.InitializeResources;
   try

    GLTFOpenGL.UploadResources;
    try

     PBRShader:=TPBRShader.Create;
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
       Draw;
       SDL_GL_SwapWindow(SurfaceWindow);
      end;

     finally

      PBRShader.Free;

     end;

     DestroyFrameBuffer(BRDFLUTFBO);

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

