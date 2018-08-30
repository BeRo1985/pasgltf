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
 v:='#version 430'+#13#10+
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
 f:='#version 430'+#13#10+
    'layout(location = 0) out vec4 oOutput;'+#13#10+
    'in vec2 vTexCoord;'+#13#10+
    'flat in int vFaceIndex;'+#13#10+
    'uniform vec3 uLightDirection;'+#13#10+
    'const float luxScale = 1e-4;'+#13#10+ // lux to linear luminance scale
    'const float planetScale = 1e0;'+#13#10+ // Base unit 1.0: kilometers
    'const float planetInverseScale = 1.0 / planetScale;'+#13#10+
    'const float cameraScale = 1e-0 * planetScale;'+#13#10+ // Base unit 1.0: kilometers
    'const float cameraInverseScale = 1.0 / cameraScale;'+#13#10+
    'const float planetDensityScale = 1.0 * planetInverseScale;'+#13#10+
    'const float planetGroundRadius = 6360.0 * planetScale;'+#13#10+ // Radius from the planet center point to the begin of the planet ground
    'const float planetAtmosphereRadius = 6460.0 * planetScale;'+#13#10+ // Radius from the planet center point to the end of the planet atmosphere
    'const float cameraHeightOverGround = 1e-3 * planetScale;'+#13#10+ // The height of the camera over the planet ground
    'const float planetWeatherMapScale = 16.0 / planetAtmosphereRadius;'+#13#10+
    'const float planetAtmosphereHeight = planetAtmosphereRadius - planetGroundRadius;'+#13#10+
    'const float heightScaleRayleigh = 8.0 * planetScale;'+#13#10+ // Rayleigh height scale
    'const float heightScaleMie = 1.2 * planetScale;'+#13#10+ // Mie height scale
    'const float heightScaleOzone = 8.0 * planetScale;'+#13#10+ // Ozone height scale
    'const float heightScaleAbsorption = 8.0 * planetScale;'+#13#10+ // Absorption height scale
    'const float planetToSunDistance = 149597870.61 * planetScale;'+#13#10+
    'const float sunRadius = 696342.0 * planetScale;'+#13#10+
    'const float sunIntensity = 100000.0 * luxScale;'+#13#10+ // in lux (sun lux value from s2016_pbs_frostbite_sky_clouds slides)
    'const vec3 scatteringCoefficientRayleigh = vec3(5.8e-3, 1.35e-2, 3.31e-2) * planetInverseScale;'+#13#10+ // Rayleigh scattering coefficients at sea level
    'const vec3 scatteringCoefficientMie = vec3(21e-3, 21e-3, 21e-3) * planetInverseScale;'+#13#10+ // Mie scattering coefficients at sea level
    'const vec3 scatteringCoefficientOzone = vec3(3.486, 8.298, 0.356) * planetInverseScale;'+#13#10+ // Ozone scattering coefficients at sea level
    'const vec3 scatteringCoefficientAbsorption = vec3(3.486e-3, 8.298e-3, 0.356e-3) * planetInverseScale;'+#13#10+ // Ozone scattering coefficients at sea level
    'const float skyTurbidity = 0.2;'+#13#10+
    'const float skyMieCoefficientG = 0.98;'+#13#10+
    'const float HALF_PI = 1.57079632679;'+#13#10+
    'const float PI = 3.1415926535897932384626433832795;'+#13#10+
    'const float TWO_PI = 6.28318530718;'+#13#10+
    'vec2 intersectSphere(vec3 rayOrigin, vec3 rayDirection, vec4 sphere){'+#13#10+
    '  vec3 v = rayOrigin - sphere.xyz;'+#13#10+
    '  float b = dot(v, rayDirection),'+#13#10+
    '        c = dot(v, v) - (sphere.w * sphere.w),'+#13#10+
    '        d = (b * b) - c;'+#13#10+
    '  return (d < 0.0)'+#13#10+
    '             ? vec2(-1.0)'+#13#10+ // No intersection
    '             : ((vec2(-1.0, 1.0) * sqrt(d)) - vec2(b));'+#13#10+ // Intersection
    '}'+#13#10+
    'void getAtmosphereParticleDensity(const in vec4 planetGroundSphere,'+#13#10+
    '                                  const in float inverseHeightScaleRayleigh,'+#13#10+
    '                                  const in float inverseHeightScaleMie,'+#13#10+
    '                                  const in vec3 position,'+#13#10+
    '                                  inout float rayleigh,'+#13#10+
    '                                  inout float mie){'+#13#10+
    '  float height = length(position - planetGroundSphere.xyz) - planetGroundSphere.w;'+#13#10+
    '  rayleigh = exp(-(height * inverseHeightScaleRayleigh));'+#13#10+
    '  mie = exp(-(height * inverseHeightScaleMie));'+#13#10+
    '}'+#13#10+
    'void getAtmosphere(vec3 rayOrigin,'+#13#10+
    '                   vec3 rayDirection,'+#13#10+
    '                   const in float startOffset,'+#13#10+
    '                   const in float maxDistance,'+#13#10+
    '                   const in vec3 lightDirection,'+#13#10+
    '                   const in float lightIntensity,'+#13#10+
    '                   const float turbidity,'+#13#10+
    '                   const float meanCosine,'+#13#10+
    '                   const in int countSteps,'+#13#10+
    '                   const in int countSubSteps,'+#13#10+
    '                   out vec3 inscattering,'+#13#10+
    '                   out vec3 extinction){'+#13#10+
    '  float atmosphereHeight = planetAtmosphereRadius - planetGroundRadius;'+#13#10+
    '  vec4 planetGroundSphere = vec4(0.0, -(planetGroundRadius + cameraHeightOverGround), 0.0, planetGroundRadius);'+#13#10+
    '  vec4 planetAtmosphereSphere = vec4(0.0, -(planetGroundRadius + cameraHeightOverGround), 0.0, planetAtmosphereRadius);'+#13#10+
    '  vec2 planetAtmosphereIntersection = intersectSphere(rayOrigin, rayDirection, planetAtmosphereSphere);'+#13#10+
    '  if(planetAtmosphereIntersection.y >= 0.0){'+#13#10+
    '    vec2 planetGroundIntersection = intersectSphere(rayOrigin, rayDirection, planetGroundSphere);'+#13#10+
    '    if(!((planetGroundIntersection.x < 0.0) && (planetGroundIntersection.y >= 0.0))){'+#13#10+
    '      float inverseHeightScaleRayleigh = 1.0 / heightScaleRayleigh,'+#13#10+
    '            inverseHeightScaleMie = 1.0 / heightScaleMie;'+#13#10+
    '      vec2 nearFar = vec2(max(0.0, ((planetGroundIntersection.x < 0.0) && (planetGroundIntersection.y >= 0.0))'+#13#10+
    '                                     ? max(planetGroundIntersection.y, planetAtmosphereIntersection.x)'+#13#10+
    '                                     : planetAtmosphereIntersection.x),'+#13#10+
    '                          (planetGroundIntersection.x >= 0.0)'+#13#10+
    '                           ? min(planetGroundIntersection.x, planetAtmosphereIntersection.y)'+#13#10+
    '                           : planetAtmosphereIntersection.y);'+#13#10+
    '      float fullRayLength = min(maxDistance, nearFar.y - nearFar.x);'+#13#10+
    '      rayOrigin += nearFar.x * rayDirection;'+#13#10+
    // Setup variables
    '      float timeStep = 1.0 / float(countSteps),'+#13#10+
    '            time = startOffset * timeStep,'+#13#10+
    '            densityScale = fullRayLength / countSteps;'+#13#10+
    '      vec3 inscatteringRayleigh = vec3(0.0);'+#13#10+ // Rayleigh in−scattering
    '      vec3 inscatteringMie = vec3(0.0);'+#13#10+ // Mie in−scattering
    '      float totalParticleDensityRayleigh = 0.0;'+#13#10+ // Rayleigh particle density from camera to integration point
    '      float totalParticleDensityMie = 0.0;'+#13#10+ // Mie particle density from camera to integration point
    '      for (int stepIndex = 0; stepIndex < countSteps; stepIndex++, time += timeStep){'+#13#10+
    // Uniform sampling
    '        float offset = time * fullRayLength;'+#13#10+
    '        vec3 position = rayOrigin + (rayDirection * offset);'+#13#10+
    // Compute Rayleigh and Mie particle density scale at P
    '        float particleDensityRayleigh, particleDensityMie;'+#13#10+
    '        getAtmosphereParticleDensity(planetGroundSphere,'+#13#10+
    '                                     inverseHeightScaleRayleigh,'+#13#10+
    '                                     inverseHeightScaleMie,'+#13#10+
    '                                     position,'+#13#10+
    '                                     particleDensityRayleigh,'+#13#10+
    '                                     particleDensityMie);'+#13#10+
    '        particleDensityRayleigh *= densityScale;'+#13#10+
    '        particleDensityMie *= densityScale;'+#13#10+
    // Accumulate particle density from the camera
    '        totalParticleDensityRayleigh += particleDensityRayleigh;'+#13#10+
    '        totalParticleDensityMie += particleDensityMie;'+#13#10+
    '        if(densityScale > 0.0){'+#13#10+
    '          vec2 outAtmosphereIntersection = intersectSphere(position, lightDirection, planetAtmosphereSphere);'+#13#10+
    '          float subRayLength = outAtmosphereIntersection.y;'+#13#10+
    '          if(subRayLength > 0.0){'+#13#10+
    '            float dls = subRayLength / float(countSubSteps),'+#13#10+
    '                  subTotalParticleDensityRayleigh = 0.0,'+#13#10+
    '                  subTotalParticleDensityMie = 0.0;'+#13#10+
    '            float subTimeStep = 1.0 / float(countSubSteps),'+#13#10+
    '                  subTime = 0.0,'+#13#10+
    '                  subDensityScale = subRayLength / float(countSubSteps);'+#13#10+
    '            for(int subStepIndex = 0; subStepIndex < countSubSteps; subStepIndex++, subTime += subTimeStep){'+#13#10+
    '              float subParticleDensityRayleigh, subParticleDensityMie;'+#13#10+
    '              vec3 subPosition = position + (lightDirection * subTime * subRayLength);'+#13#10+
    '              getAtmosphereParticleDensity(planetGroundSphere,'+#13#10+
    '                                           inverseHeightScaleRayleigh,'+#13#10+
    '                                           inverseHeightScaleMie,'+#13#10+
    '                                           subPosition,'+#13#10+
    '                                           subParticleDensityRayleigh,'+#13#10+
    '                                           subParticleDensityMie);'+#13#10+
    '              subTotalParticleDensityRayleigh += subParticleDensityRayleigh * subDensityScale;'+#13#10+
    '              subTotalParticleDensityMie += subParticleDensityMie * subDensityScale;'+#13#10+
    '            }'+#13#10+
    // Compute optical depth for Rayleigh and Mie particles
    '            vec3 totalOpticalDepthRayleigh = scatteringCoefficientRayleigh * (totalParticleDensityRayleigh + subTotalParticleDensityRayleigh);'+#13#10+
    '            vec3 totalOpticalDepthMie = scatteringCoefficientMie * (totalParticleDensityMie + subTotalParticleDensityMie);'+#13#10+
    // Compute extinction for the current integration point
    '            vec3 totalExtinction = exp(-(totalOpticalDepthRayleigh +'+#13#10+
    '                                         totalOpticalDepthMie));'+#13#10+
    // Compute differential amounts of in−scattering
    '            vec3 differentialInscatteringAmountRayleigh = particleDensityRayleigh * scatteringCoefficientRayleigh * totalExtinction;'+#13#10+
    '            vec3 differentialInscatteringAmountMie = particleDensityMie * scatteringCoefficientMie * totalExtinction;'+#13#10+
    // Compute visibility
    '            float visibility = 1.0;'+#13#10+
    // Update Rayleigh and Mie integrals
    '            inscatteringRayleigh += differentialInscatteringAmountRayleigh * visibility;'+#13#10+
    '            inscatteringMie += differentialInscatteringAmountMie * visibility;'+#13#10+
    '          }	'+#13#10+
    '        }'+#13#10+
    '      }'+#13#10+
    // Apply Rayleigh and Mie phase functions
    '      float cosTheta = dot(rayDirection, lightDirection),'+#13#10+
    '            onePlusCosThetaMulCosTheta = 1.0 + (cosTheta * cosTheta),'+#13#10+
    '            meanCosineSquared = meanCosine * meanCosine,'+#13#10+
    '            phaseRayleigh = (3.0 / (16.0 * PI)) * onePlusCosThetaMulCosTheta,'+#13#10+
    '            phaseMie = ((3.0 / (8.0 * PI)) * (1.0 - meanCosineSquared) * onePlusCosThetaMulCosTheta) /'+#13#10+
    '                       ((2.0 + meanCosineSquared) * pow((1.0 + meanCosineSquared) - (2.0 * meanCosine * cosTheta), 1.5));'+#13#10+
    // Compute in−scattering from the camera
    '      inscattering = max(vec3(0.0),'+#13#10+
    '                         ((inscatteringRayleigh * phaseRayleigh) +'+#13#10+
    '                          (inscatteringMie * phaseMie * turbidity)) *'+#13#10+
    '                         lightIntensity);'+#13#10+
    // Compute extinction from the camera
    '      extinction = max(vec3(0.0),'+#13#10+
    '                       exp(-((totalParticleDensityRayleigh * scatteringCoefficientRayleigh) +'+#13#10+
    '                             (totalParticleDensityMie * scatteringCoefficientMie))));'+#13#10+
    '    }else{'+#13#10+
    '      inscattering = vec3(0.0);'+#13#10+
    '      extinction = vec3(1.0);'+#13#10+
    '    }'+#13#10+
    '  }else{'+#13#10+
    '    inscattering = vec3(0.0);'+#13#10+
    '    extinction = vec3(1.0);'+#13#10+
    '  }'+#13#10+
    '}'+#13#10+
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
    '  vec3 tempInscattering, tempTransmittance;'+#13#10+
    '  getAtmosphere(vec3(0.0, 0.0, 0.0),'+#13#10+
    '                vec3(direction.x, max(0.0, direction.y), direction.z),'+#13#10+
    '                0.0,'+#13#10+
    '                1e+32,'+#13#10+
    '                -normalize(uLightDirection),'+#13#10+
    '                sunIntensity,'+#13#10+
    '                skyTurbidity,'+#13#10+
    '                skyMieCoefficientG,'+#13#10+
    '                256,'+#13#10+
    '                32,'+#13#10+
    '                tempInscattering,'+#13#10+
    '                tempTransmittance);'+#13#10+
    '  oOutput = vec4(tempInscattering, 1.0);'+#13#10+
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
