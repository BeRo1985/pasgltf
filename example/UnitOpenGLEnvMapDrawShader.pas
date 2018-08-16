unit UnitOpenGLEnvMapDrawShader;
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

type TEnvMapDrawShader=class(TShader)
      public
       uTexture:glInt;
       uInverseViewProjectionMatrix:glInt;
      public
       constructor Create;
       destructor Destroy; override;
       procedure BindAttributes; override;
       procedure BindVariables; override;
      end;

implementation

constructor TEnvMapDrawShader.Create;
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
    'uniform mat4 uInverseViewProjectionMatrix;'+#13#10+
    'uniform sampler2D uTexture;'+#13#10+
    'void main(){'+#13#10+
    '  vec4 p0 = uInverseViewProjectionMatrix * vec4((vTexCoord * 2.0) - vec2(1.0), 1.0, 1.0),'+#13#10+
    '       p1 = uInverseViewProjectionMatrix * vec4((vTexCoord * 2.0) - vec2(1.0), -1.0, 1.0);'+#13#10+
    '  vec3 rayDirection = normalize((p1.xyz / p1.w) - (p0.xyz / p0.w));'+#13#10+
    '	 oOutput = textureLod(uTexture, vec2((atan(rayDirection.z, rayDirection.x) / 6.283185307179586476925286766559) + 0.5, acos(rayDirection.y) / 3.1415926535897932384626433832795), 0.0);'+#13#10+
    '}'+#13#10;
 inherited Create(f,v);
end;

destructor TEnvMapDrawShader.Destroy;
begin
 inherited Destroy;
end;

procedure TEnvMapDrawShader.BindAttributes;
begin
 inherited BindAttributes;
end;

procedure TEnvMapDrawShader.BindVariables;
begin
 inherited BindVariables;
 uTexture:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uTexture')));
 uInverseViewProjectionMatrix:=glGetUniformLocation(ProgramHandle,pointer(pansichar('uInverseViewProjectionMatrix')));
end;

end.
