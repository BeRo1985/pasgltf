unit UnitOpenGLEnvMapGenShader;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpuamd64}
  {$define cpux86_64}
 {$endif}
 {$ifdef cpu386}
  {$define cpux86}
  {$define cpu32}
  {$asmmode intel}
 {$endif}
 {$ifdef cpux86_64}
  {$define cpux64}
  {$define cpu64}
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
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$ifdef cpux64}
  {$define cpux86_64}
  {$define cpu64}
 {$else}
  {$ifdef cpu386}
   {$define cpux86}
   {$define cpu32}
  {$endif}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
 {$endif}
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

interface

uses dglOpenGL,UnitOpenGLShader;

type TEnvMapGenShader=class(TShader)
      public
       constructor Create;
       destructor Destroy; override;
       procedure BindAttributes; override;
       procedure BindVariables; override;
      end;

implementation

constructor TEnvMapGenShader.Create;
var f,v:ansistring;
begin
 v:='#version 330'+#13#10+
    '#extension GL_AMD_vertex_shader_layer : enable'+#13#10+
    'out vec2 vTexCoord;'+#13#10+
    'flat out int vFaceIndex;'+#13#10+
    'void main(){'+#13#10+
    '  // For 18 vertices (6x attribute-less-rendered "full-screen" triangles)'+#13#10+
    '  int vertexID = int(gl_VertexID),'+#13#10+
    '      vertexIndex = vertexID % 3,'+#13#10+
    '      faceIndex = vertexID / 3;'+#13#10+
    '  vTexCoord = vec2((vertexIndex >> 1) * 2.0, (vertexIndex & 1) * 2.0);'+#13#10+
    '  vFaceIndex = faceIndex;'+#13#10+
    '  gl_Position = vec4(((vertexIndex >> 1) * 4.0) - 1.0, ((vertexIndex & 1) * 4.0) - 1.0, 0.0, 1.0);'+#13#10+
    '  gl_Layer = faceIndex;'+#13#10+
    '}'+#13#10;
 f:='#version 330'+#13#10+
    'layout(location = 0) out vec4 oOutput;'+#13#10+
    'in vec2 vTexCoord;'+#13#10+
    'flat in int vFaceIndex;'+#13#10+
    'vec3 getCubeMapDirection(in vec2 uv,'+#13#10+
    '                         in int faceIndex){'+#13#10+
    '  vec3 zDir = vec3(ivec3((faceIndex <= 1) ? 1 : 0,'+#13#10+
    '                         (faceIndex & 2) >> 1,'+#13#10+
    '                         (faceIndex & 4) >> 2)) *'+#13#10+
    '             (((faceIndex & 1) == 1) ? -1.0 : 1.0),'+#13#10+
    '       yDir = (faceIndex == 2)'+#13#10+
    '                ? vec3(0.0, 0.0, 1.0)'+#13#10+
    '                : ((faceIndex == 3)'+#13#10+
    '                     ? vec3(0.0, 0.0, -1.0)'+#13#10+
    '                     : vec3(0.0, -1.0, 0.0)),'+#13#10+
    '       xDir = cross(zDir, yDir);'+#13#10+
    '  return normalize((mix(-1.0, 1.0, uv.x) * xDir) +'+#13#10+
    '                   (mix(-1.0, 1.0, uv.y) * yDir) +'+#13#10+
    '                   zDir);'+#13#10+
    '}'+#13#10+
    'void main(){'+#13#10+
    '  vec3 direction = getCubeMapDirection(vTexCoord, vFaceIndex);'+#13#10+
    '  vec3 lightPosition = normalize(vec3(0.0, 0.9, -0.6));'+#13#10+
    '	 vec3 lightColor = vec3(0.99, 1.27, 1.64);'+#13#10+
    '	 float lightPower = 1.0;'+#13#10+
    '	 float f = mix(0.0625, 0.5, smoothstep(-1.0, 1.0, -dot(direction, lightPosition))) * mix(0.0, 1.0, lightPower);'+#13#10+
    '  float f2 = max(f, mix(0.0, 4.0, smoothstep(0.9, 1.0, dot(direction, lightPosition)))) * mix(2.0, 1.0, lightPower);'+#13#10+
//  '  float f3 = max(f, mix(0.0, 16.0, smoothstep(0.99999, 1.0, dot(direction, lightPosition))));'+#13#10+
    '  oOutput = vec4(vec3(f2) * lightColor, 1.0);'+#13#10+
    '}'+#13#10;
 inherited Create(f,v);
end;

destructor TEnvMapGenShader.Destroy;
begin
 inherited Destroy;
end;

procedure TEnvMapGenShader.BindAttributes;
begin
 inherited BindAttributes;
end;

procedure TEnvMapGenShader.BindVariables;
begin
 inherited BindVariables;
end;

end.
