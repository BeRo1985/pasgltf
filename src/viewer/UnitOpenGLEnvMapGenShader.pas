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
       uLightDirection:glInt;
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
    'uniform vec3 uLightDirection;'+#13#10+
    'const float PI = 3.1415926535897932384626433832795;'+#13#10+
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
    'vec4 atmosphereGet(vec3 rayOrigin, vec3 rayDirection){'+#13#10+
    '  vec3 sunDirection = uLightDirection;'+#13#10+
    '  vec3 sunLightColor = vec3(1.70, 1.15, 0.70);'+#13#10+
    '  const float atmosphereHaze = 0.03;'+#13#10+
    '  const float atmosphereHazeFadeScale = 1.0;'+#13#10+
    '  const float atmosphereDensity = 0.25;'+#13#10+
    '  const float atmosphereBrightness = 1.0;'+#13#10+
    '  const float atmospherePlanetSize = 1.0;'+#13#10+
    '  const float atmosphereHeight = 1.0;'+#13#10+
    '  const float atmosphereClarity = 10.0;'+#13#10+
    '  const float atmosphereSunDiskSize = 1.0;'+#13#10+
    '  const float atmosphereSunDiskPower = 16.0;'+#13#10+
    '  const float atmosphereSunDiskBrightness = 4.0;'+#13#10+
    '  const float earthRadius = 6.371e6;'+#13#10+
    '  const float earthAtmosphereHeight = 0.1e6;'+#13#10+
    '  const float planetRadius = earthRadius * atmospherePlanetSize;'+#13#10+
    '  const float planetAtmosphereRadius = planetRadius + (earthAtmosphereHeight * atmosphereHeight);'+#13#10+
    '  const vec3 atmosphereRadius = vec3(planetRadius, planetAtmosphereRadius * planetAtmosphereRadius, (planetAtmosphereRadius * planetAtmosphereRadius) - (planetRadius * planetRadius));'+#13#10+
    '  const float gm = mix(0.75, 0.9, atmosphereHaze);'+#13#10+
    '  const vec3 lambda = vec3(680e-9, 550e-9, 450e-9);'+#13#10+
    '  const vec3 brt = vec3(1.86e-31 / atmosphereDensity) / pow(lambda, vec3(4.0));'+#13#10+
    '  const vec3 bmt = pow(vec3(2.0 * PI) / lambda, vec3(2.0)) * vec3(0.689235, 0.6745098, 0.662745) * atmosphereHazeFadeScale * (1.36e-19 * max(atmosphereHaze, 1e-3));'+#13#10+
    '  const vec3 brmt = (brt / vec3(1.0 + atmosphereClarity)) + bmt;'+#13#10+
    '  const vec3 br = (brt / brmt) * (3.0 / (16.0 * PI));'+#13#10+
    '  const vec3 bm = (bmt / brmt) * (((1.0 - gm) * (1.0 - gm)) / (4.0 * PI));'+#13#10+
    '  const vec3 brm = brmt / 0.693147180559945309417;'+#13#10+
    '  const float sunDiskParameterY1 = -(1.0 - (0.0075 * atmosphereSunDiskSize));'+#13#10+
    '  const float sunDiskParameterX = 1.0 / (1.0 + sunDiskParameterY1);'+#13#10+
    '  const vec4 sunDiskParameters = vec4(sunDiskParameterX, sunDiskParameterX * sunDiskParameterY1, atmosphereSunDiskBrightness, atmosphereSunDiskPower);'+#13#10+
    '  float cosTheta = dot(rayDirection, -sunDirection);'+#13#10+
    '  float a = atmosphereRadius.x * max(rayDirection.y, min(-sunDirection.y, 0.0));'+#13#10+
    '  float rayDistance = sqrt((a * a) + atmosphereRadius.z) - a;'+#13#10+
    '  vec3 extinction = exp(-(rayDistance * brm));'+#13#10+
    '  vec3 position = rayDirection * (rayDistance * max(0.15 - (0.75 * sunDirection.y), 0.0));'+#13#10+
    '  position.y = max(position.y, 0.0) + atmosphereRadius.x;'+#13#10+
    '  a = dot(position, -sunDirection);'+#13#10+
    '  float sunLightRayDistance = sqrt(((a * a) + atmosphereRadius.y) - dot(position, position)) - a;'+#13#10+
    '  vec3 inscattering = ((exp(-(sunLightRayDistance * brm)) *'+#13#10+
    '                        ((br * (1.0 + (cosTheta * cosTheta))) +'+#13#10+
    '                         (bm * pow(1.0 + gm * (gm - (2.0 * cosTheta)), -1.5))) * (1.0 - extinction)) *'+#13#10+
    '                       vec3(1.0)) + '+#13#10+
    '                      (sunDiskParameters.z *'+#13#10+
    '                       extinction *'+#13#10+
    '                       sunLightColor *'+#13#10+
    '                       pow(clamp((cosTheta * sunDiskParameters.x) + sunDiskParameters.y, 0.0, 1.0), sunDiskParameters.w));'+#13#10+
    '  return vec4(inscattering * atmosphereBrightness, 1.0);'+#13#10+
    '}'+#13#10+
    'void main(){'+#13#10+
    '  vec3 direction = getCubeMapDirection(vTexCoord, vFaceIndex);'+#13#10+
    '  oOutput = atmosphereGet(vec3(0.0), direction);'+#13#10+
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
 uLightDirection:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uLightDirection')));
end;

end.
