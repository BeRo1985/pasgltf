unit UnitOpenGLPBRShader;
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

type TPBRShader=class(TShader)
      public
       uColor:glInt;
       uBaseColorTexture:glInt;
       uMetallicRoughnessTexture:glInt;
       uNormalTexture:glInt;
       uOcclusionTexture:glInt;
       uEmissiveTexture:glInt;
       uTextureFlags:glInt;
       uModelViewMatrix:glInt;
       uModelViewProjectionMatrix:glInt;
       uLightDirection:glInt;
       constructor Create;
       destructor Destroy; override;
       procedure BindAttributes; override;
       procedure BindVariables; override;
      end;

var PBRLitShader:TPBRShader;

implementation

constructor TPBRShader.Create;
var f,v:ansistring;
begin
 v:='#version 330'+#13#10+
    'layout(location = 0) in vec3 aPosition;'+#13#10+
    'layout(location = 1) in vec3 aNormal;'+#13#10+
    'layout(location = 2) in vec4 aTangent;'+#13#10+
    'layout(location = 3) in vec2 aTexCoord0;'+#13#10+
    'layout(location = 4) in vec2 aTexCoord1;'+#13#10+
    'layout(location = 5) in vec4 aColor0;'+#13#10+
    'layout(location = 6) in vec4 aJoints0;'+#13#10+
    'layout(location = 7) in vec4 aJoints1;'+#13#10+
    'layout(location = 8) in vec4 aWeights0;'+#13#10+
    'layout(location = 9) in vec4 aWeights1;'+#13#10+
    'uniform mat4 uModelViewMatrix;'+#13#10+
    'uniform mat4 uModelViewProjectionMatrix;'+#13#10+
    'out vec3 vViewSpacePosition;'+#13#10+
    'out vec2 vTexCoord0;'+#13#10+
    'out vec2 vTexCoord1;'+#13#10+
    'out vec3 vNormal;'+#13#10+
    'out vec3 vTangent;'+#13#10+
    'out vec3 vBitangent;'+#13#10+
    'out vec4 vColor;'+#13#10+
    'void main(){'+#13#10+
      'mat3 baseMatrix = transpose(inverse(mat3(uModelViewMatrix)));'+#13#10+
      'vNormal = baseMatrix * aNormal;'+#13#10+
      'vTangent = baseMatrix * aTangent.xyz;'+#13#10+
      'vBitangent = cross(vNormal, vTangent) * aTangent.w;'+#13#10+
      'vTexCoord0 = aTexCoord0;'+#13#10+
      'vTexCoord1 = aTexCoord1;'+#13#10+
      'vColor = aColor0;'+#13#10+
      'vViewSpacePosition = (uModelViewMatrix * vec4(aPosition,1.0)).xyz;'+#13#10+
      'gl_Position = uModelViewProjectionMatrix * vec4(aPosition,1.0);'+#13#10+
    '}'+#13#10;
 f:='#version 330'+#13#10+
    'layout(location = 0) out vec4 oOutput;'+#13#10+
    'in vec3 vViewSpacePosition;'+#13#10+
    'in vec2 vTexCoord0;'+#13#10+
    'in vec2 vTexCoord1;'+#13#10+
    'in vec3 vNormal;'+#13#10+
    'in vec3 vTangent;'+#13#10+
    'in vec3 vBitangent;'+#13#10+
    'in vec4 vColor;'+#13#10+
    'uniform sampler2D uBaseColorTexture;'+#13#10+
    'uniform sampler2D uMetallicRoughnessTexture;'+#13#10+
    'uniform sampler2D uNormalTexture;'+#13#10+
    'uniform sampler2D uOcclusionTexture;'+#13#10+
    'uniform sampler2D uEmissiveTexture;'+#13#10+
    'uniform uint uTextureFlags;'+#13#10+
    'uniform vec3 uLightDirection;'+#13#10+
    'void main(){'+#13#10+
      'vec4 baseColorTexture, metallicRoughnessTexture, normalTexture, occlusionTexture, emissiveTexture;'+#13#10+
      'if((uTextureFlags & 1u) != 0){'+#13#10+
        'baseColorTexture = texture(uBaseColorTexture, ((uTextureFlags & 2u) != 0) ? vTexCoord1 : vTexCoord0);'+#13#10+
      '}else{'+#13#10+
        'baseColorTexture = vec4(1.0);'+#13#10+
      '}'+#13#10+
      'if((uTextureFlags & 4u) != 0){'+#13#10+
        'metallicRoughnessTexture = texture(uMetallicRoughnessTexture, ((uTextureFlags & 8u) != 0) ? vTexCoord1 : vTexCoord0);'+#13#10+
      '}else{'+#13#10+
        'metallicRoughnessTexture = vec4(1.0);'+#13#10+
      '}'+#13#10+
      'if((uTextureFlags & 16u) != 0){'+#13#10+
        'normalTexture = normalize(texture(uNormalTexture, ((uTextureFlags & 32u) != 0) ? vTexCoord1 : vTexCoord0) - vec4(0.5));'+#13#10+
      '}else{'+#13#10+
        'normalTexture = vec2(0.0, 1.0).xxyx;'+#13#10+
      '}'+#13#10+
      'if((uTextureFlags & 64u) != 0){'+#13#10+
        'occlusionTexture = texture(uOcclusionTexture, ((uTextureFlags & 128u) != 0) ? vTexCoord1 : vTexCoord0);'+#13#10+
      '}else{'+#13#10+
        'occlusionTexture = vec4(1.0);'+#13#10+
      '}'+#13#10+
      'if((uTextureFlags & 256u) != 0){'+#13#10+
        'emissiveTexture = texture(uEmissiveTexture, ((uTextureFlags & 512u) != 0) ? vTexCoord1 : vTexCoord0);'+#13#10+
      '}else{'+#13#10+
        'emissiveTexture = vec4(0.0);'+#13#10+
      '}'+#13#10+
      'mat3 tangentSpace = mat3(normalize(vTangent), normalize(vBitangent), normalize(vNormal));'+#13#10+
      'vec3 normal = normalize(tangentSpace * normalTexture.xyz);'+#13#10+
      'oOutput = baseColorTexture * vColor * max(0.0, -dot(normal, uLightDirection));'+#13#10+
    '}'+#13#10;
 inherited Create(f,v);
end;

destructor TPBRShader.Destroy;
begin
 inherited Destroy;
end;

procedure TPBRShader.BindAttributes;
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

procedure TPBRShader.BindVariables;
begin
 inherited BindVariables;
 uColor:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uColor')));
 uBaseColorTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uBaseColorTexture')));
 uMetallicRoughnessTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('PBRMetallicRoughnessMetallicRoughnessTexture')));
 uNormalTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uNormalTexture')));
 uOcclusionTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uOcclusionTexture')));
 uEmissiveTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uEmissiveTexture')));
 uTextureFlags:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uTextureFlags')));
 uModelViewMatrix:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uModelViewMatrix')));
 uModelViewProjectionMatrix:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uModelViewProjectionMatrix')));
 uLightDirection:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uLightDirection')));
end;

end.
