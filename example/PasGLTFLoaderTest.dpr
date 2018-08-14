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
  UnitGLTFOpenGL in 'UnitGLTFOpenGL.pas';

var InputFileName:ansistring;

var fs:TFileStream;
    ms:TMemoryStream;
    StartPerformanceCounter:Int64=0;

    GLTFDocument:TPasGLTF.TDocument;

    GLTFOpenGL:TGLTFOpenGL;

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
 procedure Draw;
 begin
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

{$ifdef gles20}
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,2);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,0);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,SDL_GL_CONTEXT_PROFILE_ES);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS,0);
{$else}
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,2);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,0);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS,0);
{$endif}
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
 for k:={$ifdef UseFBO}0{$else}{$ifdef gles20}2{$else}{$ifdef gles30}2{$else}4{$endif}{$endif}{$endif} downto 0 do begin
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

 GLTFOpenGL:=TGLTFOpenGL.Create(GLTFDocument);
 try

  GLTFOpenGL.InitializeResources;
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
    Draw;
    SDL_GL_SwapWindow(SurfaceWindow);
   end;

  finally
   GLTFOpenGL.FinalizeResources;
  end;

 finally
  GLTFOpenGL.Free;
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

