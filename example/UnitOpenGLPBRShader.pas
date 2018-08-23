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

uses SysUtils,Classes,dglOpenGL,UnitOpenGLShader;

type TPBRShader=class(TShader)
      public
       const uboFrameGlobals=0;
             uboMaterial=1;
             ssboJointMatrices=2;
      public
       uBaseColorTexture:glInt;
       uMetallicRoughnessTexture:glInt;
       uNormalTexture:glInt;
       uOcclusionTexture:glInt;
       uEmissiveTexture:glInt;
       uModelMatrix:glInt;
       uLightDirection:glInt;
       uBRDFLUTTexture:glInt;
       uEnvMapTexture:glInt;
       uEnvMapMaxLevel:glInt;
       uInverseGlobalTransform:glInt;
       uJointOffset:glInt;
       constructor Create(const aSkinned,aAlphaTest:boolean);
       destructor Destroy; override;
       procedure BindAttributes; override;
       procedure BindVariables; override;
      end;

implementation

constructor TPBRShader.Create(const aSkinned,aAlphaTest:boolean);
var f0,f1,f2,f3,f,v:ansistring;
begin
 if aSkinned then begin
  f0:='layout(std140, binding = '+IntToStr(ssboJointMatrices)+') buffer ssboJointMatrices {'+#13#10+
      '  mat4 jointMatrices[];'+#13#10+
      '};'+#13#10+
      'uniform mat4 uInverseGlobalTransform;'+#13#10+
      'uniform int uJointOffset;'+#13#10;
  f1:='  mat4 skinMatrix = ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints0.x)]) * aWeights0.x) +'+#13#10+
      '                    ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints0.y)]) * aWeights0.y) +'+#13#10+
      '                    ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints0.z)]) * aWeights0.z) +'+#13#10+
      '                    ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints0.w)]) * aWeights0.w);'+#13#10+
      '  if(any(not(equal(aWeights1, vec4(0.0))))){'+#13#10+
      '    skinMatrix += ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints1.x)]) * aWeights1.x) +'+#13#10+
      '                  ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints1.y)]) * aWeights1.y) +'+#13#10+
      '                  ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints1.z)]) * aWeights1.z) +'+#13#10+
      '                  ((uInverseGlobalTransform * jointMatrices[uJointOffset + int(aJoints1.w)]) * aWeights1.w);'+#13#10+
      '  }'+#13#10;
  f2:=' * skinMatrix';
  f3:=' * transpose(inverse(mat3(skinMatrix)))';
 end else begin
  f0:='';
  f1:='';
  f2:='';
  f3:='';
 end;
 v:='#version 430'+#13#10+
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
    'out vec3 vViewSpacePosition;'+#13#10+
    'out vec2 vTexCoord0;'+#13#10+
    'out vec2 vTexCoord1;'+#13#10+
    'out vec3 vNormal;'+#13#10+
    'out vec3 vTangent;'+#13#10+
    'out vec3 vBitangent;'+#13#10+
    'out vec4 vColor;'+#13#10+
    'layout(std140, binding = '+IntToStr(uboFrameGlobals)+') uniform uboFrameGlobals {'+#13#10+
    '  mat4 viewMatrix;'+#13#10+
    '  mat4 projectionMatrix;'+#13#10+
    '} uFrameGlobals;'+#13#10+
    'uniform mat4 uModelMatrix;'+#13#10+
    f0+
    'void main(){'+#13#10+
    f1+
    '  mat4 modelMatrix = uModelMatrix'+f2+';'+#13#10+
    '  mat3 normalMatrix = transpose(inverse(mat3(modelMatrix)));'+#13#10+
    '  vNormal = normalMatrix * aNormal;'+#13#10+
    '  vTangent = normalMatrix * aTangent.xyz;'+#13#10+
    '  vBitangent = cross(vNormal, vTangent) * aTangent.w;'+#13#10+
    '  vTexCoord0 = aTexCoord0;'+#13#10+
    '  vTexCoord1 = aTexCoord1;'+#13#10+
    '  vColor = aColor0;'+#13#10+
    '  vec4 viewSpacePosition = (uFrameGlobals.viewMatrix * modelMatrix) * vec4(aPosition, 1.0);'+#13#10+
    '  vViewSpacePosition = viewSpacePosition.xyz / viewSpacePosition.w;'+#13#10+
    '  gl_Position = uFrameGlobals.projectionMatrix * viewSpacePosition;'+#13#10+
    '}'+#13#10;
 f:='#version 430'+#13#10+
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
    'uniform sampler2D uBRDFLUTTexture;'+#13#10+
    'uniform samplerCube uEnvMapTexture;'+#13#10+
    'uniform int uEnvMapMaxLevel;'+#13#10+
    'uniform vec3 uLightDirection;'+#13#10+
    'layout(std140, binding = '+IntToStr(uboMaterial)+') uniform uboMaterial {'+#13#10+
    '  vec4 baseColorFactor;'+#13#10+
    '  vec4 specularFactor;'+#13#10+
    '  vec4 emissiveFactor;'+#13#10+
    '  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;'+#13#10+
    '  uvec4 alphaCutOffFlagsTex0Tex1;'+#13#10+
    '} uMaterial;'+#13#10+
    'vec3 convertLinearRGBToSRGB(vec3 c){'+#13#10+
    '  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2),'+#13#10+
    '             c * vec3(12.92),'+#13#10+
    '             lessThan(c, vec3(3.1308e-3)));'+#13#10+
    '}'+#13#10+
    'vec3 convertSRGBToLinearRGB(vec3 c){'+#13#10+
    '  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)),'+#13#10+
    '             c / vec3(12.92),'+#13#10+
    '             lessThan(c, vec3(4.045e-2)));'+#13#10+
    '}'+#13#10+
    'struct PBRData {'+#13#10+
    '  vec3 normal;'+#13#10+
    '};'+#13#10+
    'struct PBRMetallicRoughness {'+#13#10+
    // public
    '  vec4 baseColor;'+#13#10+
    '  vec2 metallicRoughness;'+#13#10+
    // private
    '  vec3 diffuseColor;'+#13#10+
    '  vec3 specularColor;'+#13#10+
    '};'+#13#10+
    'struct PBRSpecularGlossiness {'+#13#10+
    '  vec4 diffuseColor;'+#13#10+
    '  vec4 specularGlossiness;'+#13#10+
    '};'+#13#10+
    'const float PI = 3.14159265358979323846,'+#13#10+
    '            OneOverPI = 1.0 / PI;'+#13#10+
    'const vec3 dielectricSpecular = vec3(0.04, 0.04, 0.04);'+#13#10+
    'float solveMetallic(float diffuse, float specular, float oneMinusSpecularStrength){'+#13#10+
    '  if(specular < dielectricSpecular.r){'+#13#10+
    '    return 0.0;'+#13#10+
    '  }'+#13#10+
    '  float a = dielectricSpecular.x,'+#13#10+
    '        b = (diffuse * oneMinusSpecularStrength / (1.0 - dielectricSpecular.x)) + specular - (2.0 * dielectricSpecular.x),'+#13#10+
    '        c = dielectricSpecular.x - specular,'+#13#10+
    '        D = max(0.0, (b * b) - (4 * a * c));'+#13#10+
    '  return clamp((-b + sqrt(D)) / (2.0 * a), 0.0, 1.0);'+#13#10+
    '}'+#13#10+
    'void convertPBRSpecularGlossinessToPBRMetallicRoughness(in PBRSpecularGlossiness In, out PBRMetallicRoughness Out){'+#13#10+
    '  float oneMinusSpecularStrength = 1.0 - max(max(In.specularGlossiness.x, In.specularGlossiness.y), In.specularGlossiness.z);'+#13#10+
    '  Out.metallicRoughness = vec2(solveMetallic(dot(In.diffuseColor.xyz * In.diffuseColor.xyz, vec3(0.299, 0.587, 0.114)), dot(In.specularGlossiness.xyz * In.specularGlossiness.xyz, vec3(0.299, 0.587, 0.114)), oneMinusSpecularStrength),'+#13#10+
    '                               clamp(1.0 - In.specularGlossiness.w, 1e-3, 1.0));'+#13#10+
    '  vec3 baseColorFromDiffuse = In.diffuseColor.xyz * ((oneMinusSpecularStrength / (1.0 - dielectricSpecular.x)) / max(1.0 - Out.metallicRoughness.x, 1e-6)),'+#13#10+
    '       baseColorFromSpecular = (In.specularGlossiness.xyz - (dielectricSpecular * (1.0 - Out.metallicRoughness.x))) / max(1.0 - Out.metallicRoughness.x, 1e-6);'+#13#10+
    '  Out.baseColor = vec4(convertSRGBToLinearRGB(mix(baseColorFromDiffuse, baseColorFromSpecular, Out.metallicRoughness.x * Out.metallicRoughness.x)), In.diffuseColor.w);'+#13#10+
    '}'+#13#10+
    'vec3 diffuseLambert(vec3 diffuseColor){'+#13#10+
    '  return diffuseColor * OneOverPI;'+#13#10+
    '}'+#13#10+
    'vec3 diffuseFunction(vec3 diffuseColor, float roughness, float nDotV, float nDotL, float vDotH){'+#13#10+
    '  float FD90 = 0.5 + (2.0 * (vDotH * vDotH * roughness)),'+#13#10+
    '        FdV = 1.0 + ((FD90 - 1.0) * pow(1.0 - nDotV, 5.0)),'+#13#10+
    '        FdL = 1.0 + ((FD90 - 1.0) * pow(1.0 - nDotL, 5.0));'+#13#10+
    '  return diffuseColor * (OneOverPI * FdV * FdL);'+#13#10+
    '}'+#13#10+
    'vec3 specularF(const in vec3 specularColor, const in float vDotH){'+#13#10+
    '  float fc = pow(1.0 - vDotH, 5.0);'+#13#10+
    '  return vec3(clamp(max(max(specularColor.x, specularColor.y), specularColor.z) * 50.0, 0.0, 1.0) * fc) + ((1.0 - fc) * specularColor);'+#13#10+
    '}'+#13#10+
    'float specularD(const in float roughness, const in float nDotH){'+#13#10+
    '  float a = roughness * roughness;'+#13#10+
    '  float a2 = a * a;'+#13#10+
    '  float d = (((nDotH * a2) - nDotH) * nDotH) + 1.0;'+#13#10+
    '  return a2 / (PI * (d * d));'+#13#10+
    '}'+#13#10+
    'float specularG(const in float roughness, const in float nDotV, const in float nDotL){'+#13#10+
    '  float k = (roughness * roughness) * 0.5;'+#13#10+
    '  vec2 GVL = (vec2(nDotV, nDotL) * (1.0 - k)) + vec2(k);'+#13#10+
    '  return 0.25 / (GVL.x * GVL.y);'+#13#10+
    '}'+#13#10+
    'vec3 doSingleLight(const in vec3 lightColor,'+#13#10+
    '                   const in vec3 lightLit,'+#13#10+
    '                   const in vec3 lightDirection,'+#13#10+
    '                   const in vec3 normal,'+#13#10+
    '                   const in vec3 diffuseColor,'+#13#10+
    '                   const in vec3 specularColor,'+#13#10+
    '                   const in vec3 viewDirection,'+#13#10+
    '                   const in float refractiveAngle,'+#13#10+
    '                   const in float materialTransparency,'+#13#10+
    '                   const in float materialRoughness,'+#13#10+
    '                   const in float materialCavity,'+#13#10+
    '                   const in float materialMetallic){'+#13#10+
    '  vec3 halfVector = normalize(viewDirection + lightDirection);'+#13#10+
    '	 float nDotL = clamp(dot(normal, lightDirection), 1e-5, 1.0);'+#13#10+
    '	 float nDotV = clamp(abs(dot(normal, viewDirection)) + 1e-5, 0.0, 1.0);'+#13#10+
    '	 float nDotH = clamp(dot(normal, halfVector), 0.0, 1.0);'+#13#10+
    '	 float vDotH = clamp(dot(viewDirection, halfVector), 0.0, 1.0);'+#13#10+
    '  vec3 diffuse = diffuseFunction(diffuseColor, materialRoughness, nDotV, nDotL, vDotH) * (1.0 - materialTransparency);'+#13#10+
    '	 vec3 specular = specularF(specularColor, max(vDotH, refractiveAngle)) *'+#13#10+
    '                  specularD(materialRoughness, nDotH) *'+#13#10+
    '                  specularG(materialRoughness, nDotV, nDotL);'+#13#10+
    '	 return (diffuse + specular) * ((materialCavity * nDotL * lightColor) * lightLit);'+#13#10+
    '}'+#13#10+
    'vec4 getEnvMap(sampler2D texEnvMap, float texLOD, vec3 rayDirection){'+#13#10+
    '  rayDirection = normalize(rayDirection);'+#13#10+
    '  return textureLod(texEnvMap, (vec2((atan(rayDirection.z, rayDirection.x) / 6.283185307179586476925286766559) + 0.5, acos(rayDirection.y) / 3.1415926535897932384626433832795)), texLOD);'+#13#10+
    '}'+#13#10+
    'const uint smPBRMetallicRoughness = 0u,'+#13#10+
    '           smPBRSpecularGlossiness = 1u,'+#13#10+
    '           smUnlit = 2u;'+#13#10+
    'uvec2 texCoordIndices = uMaterial.alphaCutOffFlagsTex0Tex1.zw;'+#13#10+
    'vec2 texCoords[2] = vec2[2](vTexCoord0, vTexCoord1);'+#13#10+
    'vec4 textureFetch(const in sampler2D tex, const in int textureIndex, const in vec4 defaultValue){'+#13#10+
    '  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;'+#13#10+
    '  return (which < 0x2u) ? texture(tex, texCoords[int(which)]) : defaultValue;'+#13#10+
//  '  return (which < 0xfu) ? texture(tex, texCoords[int(which)]) : defaultValue;'+#13#10+
    '}'+#13#10+
    'void main(){'+#13#10+
    '  vec4 color = vec4(0.0);'+#13#10+
    '  uint flags = uMaterial.alphaCutOffFlagsTex0Tex1.y,'+#13#10+
    '       shadingModel = (flags >> 0u) & 0xfu;'+#13#10+
    '  switch(shadingModel){'+#13#10+
    '    case smPBRMetallicRoughness:'+#13#10+
    '    case smPBRSpecularGlossiness:{'+#13#10+
    '      vec4 occlusionTexture, emissiveTexture;'+#13#10+
    '      PBRMetallicRoughness pbrMetallicRoughness;'+#13#10+
    '      switch(shadingModel){'+#13#10+
    '        case smPBRMetallicRoughness:{'+#13#10+
    '          pbrMetallicRoughness.baseColor = textureFetch(uBaseColorTexture, 0, vec4(1.0));'+#13#10+
    '          pbrMetallicRoughness.baseColor = vec4(convertSRGBToLinearRGB(pbrMetallicRoughness.baseColor.xyz), pbrMetallicRoughness.baseColor.w) * uMaterial.baseColorFactor;'+#13#10+
    '          pbrMetallicRoughness.metallicRoughness = textureFetch(uMetallicRoughnessTexture, 1, vec4(1.0)).xy * uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.xy;'+#13#10+
    '          break;'+#13#10+
    '        }'+#13#10+
    '        case smPBRSpecularGlossiness:{'+#13#10+
    '          PBRSpecularGlossiness pbrSpecularGlossiness;'+#13#10+
    '          pbrSpecularGlossiness.diffuseColor = textureFetch(uBaseColorTexture, 0, vec4(1.0)) * uMaterial.baseColorFactor;'+#13#10+
    '          pbrSpecularGlossiness.specularGlossiness = textureFetch(uMetallicRoughnessTexture, 1, vec4(1.0)) * vec4(uMaterial.specularFactor.xyz, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.y);'+#13#10+
    '          convertPBRSpecularGlossinessToPBRMetallicRoughness(pbrSpecularGlossiness, pbrMetallicRoughness);'+#13#10+
    '          break;'+#13#10+
    '        }'+#13#10+
    '      }'+#13#10+
    '      vec3 normal;'+#13#10+
    '      if((texCoordIndices.x & 0x00000f00u) != 0x00000f00u){'+#13#10+
    '        vec4 normalTexture = textureFetch(uNormalTexture, 2, vec2(0.0, 1.0).xxyx);'+#13#10+
    '        normal = normalize(mat3(normalize(vTangent), normalize(vBitangent), normalize(vNormal)) * normalize(normalTexture.xyz - vec3(0.5)));'+#13#10+
    '      }else{'+#13#10+
    '        normal = normalize(vNormal);'+#13#10+
    '      }'+#13#10+
    '      normal *= (((flags & (1u << 5u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;'+#13#10+
    '      occlusionTexture = textureFetch(uOcclusionTexture, 3, vec4(1.0));'+#13#10+
    '      emissiveTexture = textureFetch(uEmissiveTexture, 4, vec4(0.0));'+#13#10+
    '      float materialMetallic = clamp(pbrMetallicRoughness.metallicRoughness.x, 0.0, 1.0),'+#13#10+
    '            materialRoughness = clamp(pbrMetallicRoughness.metallicRoughness.y, 1e-3, 1.0),'+#13#10+
    '            materialCavity = clamp(mix(1.0, occlusionTexture.x, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.w), 0.0, 1.0),'+#13#10+
    '            materialTransparency = 0.0,'+#13#10+
    '            refractiveAngle = 0.0,'+#13#10+
    '            ambientOcclusion = 1.0,'+#13#10+
    '            shadow = 1.0;'+#13#10+
    '      vec3 f0 = vec3(0.04),'+#13#10+
    '           viewDirection = normalize(vViewSpacePosition),'+#13#10+
    '           diffuseColor = pbrMetallicRoughness.baseColor.xyz * (vec3(1.0) - f0) * (1.0 - materialMetallic) * PI,'+#13#10+
    '           specularColor = mix(f0, pbrMetallicRoughness.baseColor.xyz, materialMetallic) * PI;'+#13#10+
    '      float reflectance = max(max(specularColor.x, specularColor.y), specularColor.z);'+#13#10+
    '      vec3 r90 = vec3(clamp(reflectance * 25.0, 0.0, 1.0)),'+#13#10+
    '           r0 = specularColor;'+#13#10+
{   '      color.xyz += (doSingleLight(vec3(1.70, 1.15, 0.70),'+#13#10+
    '                                  pow(vec3(shadow), vec3(1.05, 1.02, 1.0)),'+#13#10+
    '                                  -uLightDirection,'+#13#10+
    '                                  normal.xyz,'+#13#10+
    '                                  diffuseColor,'+#13#10+
    '                                  specularColor,'+#13#10+
    '                                  viewDirection,'+#13#10+
    '                                  refractiveAngle,'+#13#10+
    '                                  materialTransparency,'+#13#10+
    '                                  materialRoughness,'+#13#10+
    '                                  materialCavity,'+#13#10+
    '                                  materialMetallic) +'+#13#10+
    '                   (((('+#13#10+
    '                       // Sky light'+#13#10+
    '                       (max(0.0, 0.6 + (0.4 * normal.y)) * vec3(0.05, 0.20, 0.45)) +'+#13#10+
    '                       // Backlight'+#13#10+
    '                       (max(0.0, 0.2 + (0.8 * dot(normal.xyz, normalize(vec3(uLightDirection.xz, 0.0).xzy)))) * vec3(0.20, 0.25, 0.25))'+#13#10+
    '                     ) * ambientOcclusion) +'+#13#10+
    '                     // Bounce light'+#13#10+
    '                     (clamp(-normal.y, 0.0, 1.0) * vec3(0.18, 0.24, 0.24) * mix(0.5, 1.0, ambientOcclusion))'+#13#10+
    '                    ) * diffuseLambert(diffuseColor) * materialCavity));'+#13#10+ (*{}
(*  '      color.xyz += doSingleLight(vec3(1.70, 1.15, 0.70),'+#13#10+ // Sun light
    '                                 pow(vec3(shadow), vec3(1.05, 1.02, 1.0)),'+#13#10+
    '                                 -uLightDirection,'+#13#10+
    '                                 normal.xyz,'+#13#10+
    '                                 diffuseColor,'+#13#10+
    '                                 specularColor,'+#13#10+
    '                                 viewDirection,'+#13#10+
    '                                 refractiveAngle,'+#13#10+
    '                                 materialTransparency,'+#13#10+
    '                                 materialRoughness,'+#13#10+
    '                                 materialCavity,'+#13#10+
    '                                 materialMetallic);'+#13#10+(**)
    '      {'+#13#10+
    '        float NdotV = clamp(abs(dot(normal.xyz, viewDirection)) + 1e-5, 0.0, 1.0),'+#13#10+
    '              ao = materialCavity * ambientOcclusion,'+#13#10+
    '              specularOcclusion = clamp((pow(NdotV + ao, materialRoughness * materialRoughness) - 1.0) + ao, 0.0, 1.0);'+#13#10+
    '      	 vec2 brdf = textureLod(uBRDFLUTTexture, vec2(materialRoughness, NdotV), 0.0).xy;'+#13#10+
    '        color.xyz += textureLod(uEnvMapTexture, normalize(reflect(viewDirection, normal.xyz)), clamp((float(uEnvMapMaxLevel) - 1.0) - (1.0 - (1.2 * log2(materialRoughness))), 0.0, float(uEnvMapMaxLevel))).xyz * ((specularColor * brdf.x) +'+' (brdf.yyy * clamp(max(max(specularColor.x, specularColor.y), specularColor.z) * 50.0, 0.0, 1.0))) * specularOcclusion;'+#13#10+
    '        color.xyz += textureLod(uEnvMapTexture, normal.xyz, float(uEnvMapMaxLevel)).xyz * diffuseColor * ao;'+#13#10+
    '      }'+#13#10+
    '      color = vec4(vec3(color.xyz + (convertSRGBToLinearRGB(emissiveTexture.xyz) * uMaterial.emissiveFactor.xyz)), pbrMetallicRoughness.baseColor.w);'+#13#10+
    '      break;'+#13#10+
    '    }'+#13#10+
    '    case smUnlit:{'+#13#10+
   '       color = textureFetch(uBaseColorTexture, 0, vec4(1.0));'+#13#10+
    '      color = vec4(convertSRGBToLinearRGB(color.xyz), color.w) * uMaterial.baseColorFactor;'+#13#10+
    '      break;'+#13#10+
    '    }'+#13#10+
    '  }'+#13#10+
    '  float alpha = color.w * vColor.w;'+#13#10+
    '  oOutput = vec4(color.xyz * vColor.xyz, mix(1.0, alpha, float(int(uint((flags >> 4u) & 1u)))));'+#13#10;
 if aAlphaTest then begin
  f:=f+'  if(alpha < uintBitsToFloat(uMaterial.alphaCutOffFlagsTex0Tex1.x)){'+#13#10+
       '    discard;'+#13#10+
       '  }'+#13#10;
 end;
 f:=f+'}'+#13#10;
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
 uBaseColorTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uBaseColorTexture')));
 uMetallicRoughnessTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uMetallicRoughnessTexture')));
 uNormalTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uNormalTexture')));
 uOcclusionTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uOcclusionTexture')));
 uEmissiveTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uEmissiveTexture')));
 uModelMatrix:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uModelMatrix')));
 uLightDirection:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uLightDirection')));
 uBRDFLUTTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uBRDFLUTTexture')));
 uEnvMapTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uEnvMapTexture')));
 uEnvMapMaxLevel:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uEnvMapMaxLevel')));
//uboJointMatrices:=glGetUniformBlockIndex(ProgramHandle,pointer(pansichar('uboJointMatrices')));
 uInverseGlobalTransform:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uInverseGlobalTransform')));
 uJointOffset:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uJointOffset')));
 glUniform1i(uBaseColorTexture,0);
 glUniform1i(uMetallicRoughnessTexture,1);
 glUniform1i(uNormalTexture,2);
 glUniform1i(uOcclusionTexture,3);
 glUniform1i(uEmissiveTexture,4);
 glUniform1i(uBRDFLUTTexture,5);
 glUniform1i(uEnvMapTexture,6);
end;

end.
