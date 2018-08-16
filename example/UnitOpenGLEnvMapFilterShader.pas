unit UnitOpenGLEnvMapFilterShader;
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

type TEnvMapFilterShader=class(TShader)
      public
       uTexture:glInt;
       uMipMapLevel:glInt;
      public
       constructor Create;
       destructor Destroy; override;
       procedure BindAttributes; override;
       procedure BindVariables; override;
      end;

implementation

constructor TEnvMapFilterShader.Create;
var f,v:ansistring;
begin
 v:='#version 330'+#13#10+
    'out vec2 vTexCoord;'+#13#10+
    'void main(){'+#13#10+
    '	vTexCoord = vec2((gl_VertexID >> 1) * 2.0, (gl_VertexID & 1) * 2.0);'+#13#10+
    '	gl_Position = vec4(((gl_VertexID >> 1) * 4.0) - 1.0, ((gl_VertexID & 1) * 4.0) - 1.0, 0.0, 1.0);'+#13#10+
    '}'+#13#10;
 f:='#version 330'+#13#10+
    'in vec2 vTexCoord;'+#13#10+
    'layout(location = 0) out vec4 oOutput;'+#13#10+
    'uniform int uMipMapLevel;'+#13#10+
    'uniform sampler2D uTexture;'+#13#10+
    'vec2 Hammersley(const in int index, const in int numSamples){'+#13#10+
//  '  uint reversedIndex = bitfieldReverse(uint(index));'+#13#10+ // >= OpenGL 4.0
    '  uint reversedIndex = uint(index);'+#13#10+
    '  reversedIndex = (reversedIndex << 16u) | (reversedIndex >> 16u);'+#13#10+
    '  reversedIndex = ((reversedIndex & 0x00ff00ffu) << 8u) | ((reversedIndex & 0xff00ff00u) >> 8u);'+#13#10+
    '  reversedIndex = ((reversedIndex & 0x0f0f0f0fu) << 4u) | ((reversedIndex & 0xf0f0f0f0u) >> 4u);'+#13#10+
    '  reversedIndex = ((reversedIndex & 0x33333333u) << 2u) | ((reversedIndex & 0xccccccccu) >> 2u);'+#13#10+
    '  reversedIndex = ((reversedIndex & 0x55555555u) << 1u) | ((reversedIndex & 0xaaaaaaaau) >> 1u);'+#13#10+
    '  return vec2(fract(float(index) / float(numSamples)), float(reversedIndex) * 2.3283064365386963e-10);'+#13#10+
    '}'+#13#10+
    'vec3 ImportanceSampleGGX(const in vec2 e, const in float roughness, const in vec3 normal){'+#13#10+
    '  float m = roughness * roughness;'+#13#10+
    '  float m2 = m * m;'+#13#10+
    '  float phi = 2.0 * 3.1415926535897932384626433832795 * e.x;'+#13#10+
    '  float cosTheta = sqrt((1.0 - e.y) / (1.0 + ((m2 - 1.0) * e.y)));'+#13#10+
    '  float sinTheta = sqrt(1.0 - (cosTheta * cosTheta));'+#13#10+
    '  vec3 h = vec3(sinTheta * cos(phi), sinTheta * sin(phi), cosTheta);'+#13#10+
    '  vec3 tangentZ = normalize(normal);'+#13#10+
    '  vec3 upVector = (abs(tangentZ.z) < 0.999) ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);'+#13#10+
    '  vec3 tangentX = normalize(cross(upVector, tangentZ));'+#13#10+
    '  vec3 tangentY = cross(tangentZ, tangentX);'+#13#10+
    '  return (tangentX * h.x) + (tangentY * h.y) + (tangentZ * h.z);'+#13#10+
    '}'+#13#10+
    'void main(){'+#13#10+
    '  if(uMipMapLevel == 0){'+#13#10+
    '    oOutput = textureLod(uTexture, vTexCoord, 0.0);'+#13#10+
    '	}else{'+#13#10+
    '	  float roughness = clamp(exp2((1 - ((8 - 1) - uMipMapLevel)) / 1.2), 0.0, 1.0);'+#13#10+
    '	  const int numSamples = 64; //clamp(64 * uMipMapLevel, 1, 256);'+#13#10+
    '	  vec2 thetaphi = ((vTexCoord * 2.0) - vec2(1.0)) * vec2(3.1415926535897932384626433832795, -1.5707963267948966192313216916398);'+#13#10+
    '	  vec3 R = normalize(vec3(cos(thetaphi.y) * cos(thetaphi.x), sin(thetaphi.y), cos(thetaphi.y) * sin(thetaphi.x)));'+#13#10+
    '	  vec3 N = R;'+#13#10+
    '	  vec3 V = R;'+#13#10+
    '	  vec4 r = vec4(0.0);'+#13#10+
    '	  float w = 0.0;'+#13#10+
    '	  for(int i = 0; i < numSamples; i++){'+#13#10+
    '	    vec3 H = ImportanceSampleGGX(Hammersley(i, numSamples), roughness, N);'+#13#10+
    '	    vec3 L = -reflect(V, H);'+#13#10+ //((2.0 * dot(V, H )) * H) - V;
    '	    float nDotL = clamp(dot(N, L), 0.0, 1.0);'+#13#10+
    '	    if(nDotL > 0.0){'+#13#10+
    '	      vec3 rayDirection = normalize(L);'+#13#10+
    '	      r += textureLod(uTexture, vec2((atan(rayDirection.z, rayDirection.x) / 6.283185307179586476925286766559) + 0.5, acos(rayDirection.y) / 3.1415926535897932384626433832795), 0.0) * nDotL;'+#13#10+
    '	      w += nDotL;'+#13#10+
    '	    }'+#13#10+
    '	  }'+#13#10+
    '	  oOutput = r / max(w, 1e-4);'+#13#10+
    '  }'+#13#10+
    '}'+#13#10;
 inherited Create(f,v);
end;

destructor TEnvMapFilterShader.Destroy;
begin
 inherited Destroy;
end;

procedure TEnvMapFilterShader.BindAttributes;
begin
 inherited BindAttributes;
 glBindAttribLocation(ProgramHandle,0,'aPosition');
 glBindAttribLocation(ProgramHandle,1,'aNormal');
 glBindAttribLocation(ProgramHandle,2,'aTangent');
 glBindAttribLocation(ProgramHandle,3,'aTexCoord0');
 glBindAttribLocation(ProgramHandle,4,'aTexCoord1');
 glBindAttribLocation(ProgramHandle,5,'aColor0');
 glBindAttribLocation(ProgramHandle,6,'aJoints0');
 glBindAttribLocation(ProgramHandle,7,'aJoints1');
 glBindAttribLocation(ProgramHandle,8,'aWeights0');
 glBindAttribLocation(ProgramHandle,9,'aWeights1');
end;

procedure TEnvMapFilterShader.BindVariables;
begin
 inherited BindVariables;
 uTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uTexture')));
 uMipMapLevel:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uMipMapLevel')));
end;

end.
