unit UnitGLTFOpenGL;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$endif}
{$m+}

{$scopedenums on}

interface

uses SysUtils,Classes,Math,PasJSON,PasGLTF,dglOpenGL,UnitOpenGLImage,
     UnitOpenGLShader,UnitOpenGLPBRShader;

type EGLTFOpenGL=class(Exception);

     TGLTFOpenGL=class
      private
       type TAnimation=record
             public
              type TChannel=record
                    public
                     type TTarget=
                           (
                            Translation,
                            Rotation,
                            Scale,
                            Weights
                           );
                          TInterpolation=
                           (
                            Linear,
                            Step,
                            CubicSpline
                           );
                    public
                     Node:TPasGLTFSizeInt;
                     Target:TTarget;
                     Interpolation:TInterpolation;
                     InputTimeArray:TPasGLTFFloatDynamicArray;
                     OutputScalarArray:TPasGLTFFloatDynamicArray;
                     OutputVector3Array:TPasGLTF.TVector3DynamicArray;
                     OutputVector4Array:TPasGLTF.TVector4DynamicArray;
                   end;
                   PChannel=^TChannel;
                   TChannels=array of TChannel;
             public
              Channels:TChannels;
            end;
            PAnimation=^TAnimation;
            TAnimations=array of TAnimation;
            TVertexAttributeBindingLocations=class
             public
              const Position=0;
                    Normal=1;
                    Tangent=2;
                    TexCoord0=3;
                    TexCoord1=4;
                    Color0=5;
                    Joints0=6;
                    Joints1=7;
                    Weights0=8;
                    Weights1=9;
            end;
            TVertex=packed record
             Position:TPasGLTF.TVector3;
             Normal:TPasGLTF.TVector3;
             Tangent:TPasGLTF.TVector4;
             TexCoord0:TPasGLTF.TVector2;
             TexCoord1:TPasGLTF.TVector2;
             Color0:TPasGLTF.TVector4;
             Joints0:TPasGLTF.TVector4;
             Joints1:TPasGLTF.TVector4;
             Weights0:TPasGLTF.TVector4;
             Weights1:TPasGLTF.TVector4;
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            TMaterial=record
             public
              type TTexture=record
                    Index:TPasGLTFSizeInt;
                    TexCoord:TPasGLTFSizeInt;
                   end;
                   PTexture=^TTexture;
                   TPBRSpecularGlossiness=record
                    Used:boolean;
                    DiffuseFactor:TPasGLTF.TVector4;
                    DiffuseTexture:TTexture;
                    GlossinessFactor:TPasGLTFFloat;
                    SpecularFactor:TPasGLTF.TVector3;
                    SpecularGlossinessTexture:TTexture;
                   end;
                   PPBRSpecularGlossiness=^TPBRSpecularGlossiness;
             public
              PBRSpecularGlossiness:TPBRSpecularGlossiness;
            end;
            PMaterial=^TMaterial;
            TMaterials=array of TMaterial;
            TMesh=record
             public
              type TPrimitive=record
                    PrimitiveMode:glEnum;
                    Vertices:TVertices;
                    Indices:TPasGLTFUInt32DynamicArray;
                    StartBufferVertexOffset:TPasGLTFSizeUInt;
                    StartBufferIndexOffset:TPasGLTFSizeUInt;
                    CountVertices:TPasGLTFSizeUInt;
                    CountIndices:TPasGLTFSizeUInt;
                    Material:TPasGLTFSizeInt;
                   end;
                   PPrimitive=^TPrimitive;
                   TPrimitives=array of TPrimitive;
             public
              Primitives:TPrimitives;
              MinPosition:TPasGLTF.TVector3;
              MaxPosition:TPasGLTF.TVector3;
            end;
            PMesh=^TMesh;
            TMeshes=array of TMesh;
            TSkin=record
             Skeleton:TPasGLTFSizeInt;
             InverseBindMatrices:TPasGLTF.TMatrix4x4DynamicArray;
             Matrices:TPasGLTF.TMatrix4x4DynamicArray;
             Joints:TPasGLTFSizeIntDynamicArray;
             UniformBufferObjectHandle:glUInt;
            end;
            PSkin=^TSkin;
            TSkins=array of TSkin;
            TNode=record
             public
              type TOverwriteFlag=
                    (
                     Translation,
                     Rotation,
                     Scale
                    );
                   TOverwriteFlags=set of TOverwriteFlag;
             public
              Matrix:TPasGLTF.TMatrix4x4;
              OverwriteFlags:TOverwriteFlags;
              Translation:TPasGLTF.TVector3;
              Rotation:TPasGLTF.TVector4;
              Scale:TPasGLTF.TVector3;
            end;
            PNode=^TNode;
            TNodes=array of TNode;
            TTexture=record
             Handle:glUInt;
            end;
            PTexture=^TTexture;
            TTextures=array of TTexture;
      private
       fDocument:TPasGLTF.TDocument;
       fReady:boolean;
       fUploaded:boolean;
       fAnimations:TAnimations;
       fMaterials:TMaterials;
       fMeshes:TMeshes;
       fSkins:TSkins;
       fNodes:TNodes;
       fTextures:TTextures;
       fVertexBufferObjectHandle:glInt;
       fIndexBufferObjectHandle:glInt;
       fVertexArrayHandle:glInt;
       fStaticMinPosition:TPasGLTF.TVector3;
       fStaticMaxPosition:TPasGLTF.TVector3;
      public
       constructor Create(const aDocument:TPasGLTF.TDocument); reintroduce;
       destructor Destroy; override;
       procedure InitializeResources;
       procedure FinalizeResources;
       procedure UploadResources;
       procedure UnloadResources;
       procedure Draw(const aModelMatrix:TPasGLTF.TMatrix4x4;
                      const aViewMatrix:TPasGLTF.TMatrix4x4;
                      const aProjectionMatrix:TPasGLTF.TMatrix4x4;
                      const aNonSkinnedNormalPBRShader:TPBRShader;
                      const aNonSkinnedAlphaTestPBRShader:TPBRShader;
                      const aSkinnedNormalPBRShader:TPBRShader;
                      const aSkinnedAlphaTestPBRShader:TPBRShader;
                      const aAnimationIndex:TPasGLTFSizeInt=0;
                      const aTime:TPasGLTFFloat=0.0;
                      const aScene:TPasGLTFSizeInt=-1);
       property StaticMinPosition:TPasGLTF.TVector3 read fStaticMinPosition;
       property StaticMaxPosition:TPasGLTF.TVector3 read fStaticMaxPosition;
      published
       property Document:TPasGLTF.TDocument read fDocument;
     end;

implementation

const Epsilon=1e-8;

type TVector2=TPasGLTF.TVector2;
     PVector2=^TVector2;

     TVector3=TPasGLTF.TVector3;
     PVector3=^TVector3;

     TVector4=TPasGLTF.TVector4;
     PVector4=^TVector4;

     TMatrix=TPasGLTF.TMatrix4x4;
     PMatrix=^TMatrix;

function Vector2Add(const a,b:TVector2):TVector2;
begin
 result[0]:=a[0]+b[0];
 result[1]:=a[1]+b[1];
end;

function Vector2Sub(const a,b:TVector2):TVector2;
begin
 result[0]:=a[0]-b[0];
 result[1]:=a[1]-b[1];
end;

function Vector3Add(const a,b:TVector3):TVector3;
begin
 result[0]:=a[0]+b[0];
 result[1]:=a[1]+b[1];
 result[2]:=a[2]+b[2];
end;

function Vector3Sub(const a,b:TVector3):TVector3;
begin
 result[0]:=a[0]-b[0];
 result[1]:=a[1]-b[1];
 result[2]:=a[2]-b[2];
end;

function Vector3Cross(const a,b:TVector3):TVector3;
begin
 result[0]:=(a[1]*b[2])-(a[2]*b[1]);
 result[1]:=(a[2]*b[0])-(a[0]*b[2]);
 result[2]:=(a[0]*b[1])-(a[1]*b[0]);
end;

function Vector3Dot(const a,b:TVector3):TPasGLTFFloat;
begin
 result:=(a[0]*b[0])+(a[1]*b[1])+(a[2]*b[2]);
end;

function Vector3Normalize(const aVector:TVector3):TVector3;
var l:TPasGLTFFloat;
begin
 l:=sqrt(sqr(aVector[0])+sqr(aVector[1])+sqr(aVector[2]));
 if abs(l)>Epsilon then begin
  result[0]:=aVector[0]/l;
  result[1]:=aVector[1]/l;
  result[2]:=aVector[2]/l;
 end else begin
  result[0]:=0.0;
  result[1]:=0.0;
  result[2]:=0.0;
 end;
end;

function Vector3Neg(const aVector:TVector3):TVector3;
begin
 result[0]:=-aVector[0];
 result[1]:=-aVector[1];
 result[2]:=-aVector[2];
end;

function Vector3Scale(const aVector:TVector3;const aFactor:TPasGLTFFloat):TVector3;
begin
 result[0]:=aVector[0]*aFactor;
 result[1]:=aVector[1]*aFactor;
 result[2]:=aVector[2]*aFactor;
end;

function Vector3MatrixMul(const m:TPasGLTF.TMatrix4x4;const v:TVector3):TVector3;
begin
 result[0]:=(m[0]*v[0])+(m[4]*v[1])+(m[8]*v[2])+m[12];
 result[1]:=(m[1]*v[0])+(m[5]*v[1])+(m[9]*v[2])+m[13];
 result[2]:=(m[2]*v[0])+(m[6]*v[1])+(m[10]*v[2])+m[14];
end;

function Vector4Dot(const a,b:TVector4):TPasGLTFFloat;
begin
 result:=(a[0]*b[0])+(a[1]*b[1])+(a[2]*b[2])+(a[3]*b[3]);
end;

function Vector4Neg(const aVector:TVector4):TVector4;
begin
 result[0]:=-aVector[0];
 result[1]:=-aVector[1];
 result[2]:=-aVector[2];
 result[3]:=-aVector[3];
end;

function Vector4Normalize(const aVector:TVector4):TVector4;
var l:TPasGLTFFloat;
begin
 l:=sqrt(sqr(aVector[0])+sqr(aVector[1])+sqr(aVector[2])+sqr(aVector[3]));
 if abs(l)>Epsilon then begin
  result[0]:=aVector[0]/l;
  result[1]:=aVector[1]/l;
  result[2]:=aVector[2]/l;
  result[3]:=aVector[3]/l;
 end else begin
  result[0]:=0.0;
  result[1]:=0.0;
  result[2]:=0.0;
  result[3]:=0.0;
 end;
end;

function QuaternionMul(const q1,q2:TVector4):TVector4;
begin
 result[0]:=((q1[3]*q2[0])+(q1[0]*q2[3])+(q1[1]*q2[2]))-(q1[2]*q2[1]);
 result[1]:=((q1[3]*q2[1])+(q1[1]*q2[3])+(q1[2]*q2[0]))-(q1[0]*q2[2]);
 result[2]:=((q1[3]*q2[2])+(q1[2]*q2[3])+(q1[0]*q2[1]))-(q1[1]*q2[0]);
 result[3]:=(q1[3]*q2[3])-((q1[0]*q2[0])+(q1[1]*q2[1])+(q1[2]*q2[2]));
end;

function QuaternionConjugate(const AQuaternion:TVector4):TVector4;
begin
 result[0]:=-AQuaternion[0];
 result[1]:=-AQuaternion[1];
 result[2]:=-AQuaternion[2];
 result[3]:=AQuaternion[3];
end;

function QuaternionInverse(const AQuaternion:TVector4):TVector4;var Normal:TPasGLTFFloat;
begin
 Normal:=sqrt(sqr(AQuaternion[0])+sqr(AQuaternion[1])+sqr(AQuaternion[2])+sqr(AQuaternion[3]));
 if abs(Normal)>1e-18 then begin
  Normal:=1.0/Normal;
 end;
 result[0]:=-(AQuaternion[0]*Normal);
 result[1]:=-(AQuaternion[1]*Normal);
 result[2]:=-(AQuaternion[2]*Normal);
 result[3]:=(AQuaternion[3]*Normal);
end;

function QuaternionAdd(const q1,q2:TVector4):TVector4;
begin
 result[0]:=q1[0]+q2[0];
 result[1]:=q1[1]+q2[1];
 result[2]:=q1[2]+q2[2];
 result[3]:=q1[3]+q2[3];
end;

function QuaternionSub(const q1,q2:TVector4):TVector4;
begin
 result[0]:=q1[0]-q2[0];
 result[1]:=q1[1]-q2[1];
 result[2]:=q1[2]-q2[2];
 result[3]:=q1[3]-q2[3];
end;

function QuaternionScalarMul(const q:TVector4;const s:TPasGLTFFloat):TVector4;
begin
 result[0]:=q[0]*s;
 result[1]:=q[1]*s;
 result[2]:=q[2]*s;
 result[3]:=q[3]*s;
end;

function QuaternionSlerp(const q1,q2:TVector4;const t:TPasGLTFFloat):TVector4;
const EPSILON=1e-12;
var Omega,co,so,s0,s1,s2:TPasGLTFFloat;
begin
 co:=(q1[0]*q2[0])+(q1[1]*q2[1])+(q1[2]*q2[2])+(q1[3]*q2[3]);
 if co<0.0 then begin
  co:=-co;
  s2:=-1.0;
 end else begin
  s2:=1.0;
 end;
 if (1.0-co)>EPSILON then begin
  Omega:=ArcCos(co);
  so:=sin(Omega);
  s0:=sin((1.0-t)*Omega)/so;
  s1:=sin(t*Omega)/so;
 end else begin
  s0:=1.0-t;
  s1:=t;
 end;
 result[0]:=(s0*q1[0])+(s1*(s2*q2[0]));
 result[1]:=(s0*q1[1])+(s1*(s2*q2[1]));
 result[2]:=(s0*q1[2])+(s1*(s2*q2[2]));
 result[3]:=(s0*q1[3])+(s1*(s2*q2[3]));
end;

function QuaternionUnflippedSlerp(const q1,q2:TVector4;const t:TPasGLTFFloat):TVector4; {$ifdef caninline}inline;{$endif}
var Omega,co,so,s0,s1:TPasGLTFFloat;
begin
 co:=(q1[0]*q2[0])+(q1[1]*q2[1])+(q1[2]*q2[2])+(q1[3]*q2[3]);
 if (1.0-co)>1e-8 then begin
  Omega:=ArcCos(co);
  so:=sin(Omega);
  s0:=sin((1.0-t)*Omega)/so;
  s1:=sin(t*Omega)/so;
 end else begin
  s0:=1.0-t;
  s1:=t;
 end;
 result[0]:=(s0*q1[0])+(s1*q2[0]);
 result[1]:=(s0*q1[1])+(s1*q2[1]);
 result[2]:=(s0*q1[2])+(s1*q2[2]);
 result[3]:=(s0*q1[3])+(s1*q2[3]);
end;

function QuaternionLog(const AQuaternion:TVector4):TVector4;
var Theta,SinTheta,Coefficent:TPasGLTFFloat;
begin
 result[0]:=AQuaternion[0];
 result[1]:=AQuaternion[1];
 result[2]:=AQuaternion[2];
 result[3]:=0.0;
 if abs(AQuaternion[3])<1.0 then begin
  Theta:=ArcCos(AQuaternion[3]);
  SinTheta:=sin(Theta);
  if abs(SinTheta)>1e-6 then begin
   Coefficent:=Theta/SinTheta;
   result[0]:=result[0]*Coefficent;
   result[1]:=result[1]*Coefficent;
   result[2]:=result[2]*Coefficent;
  end;
 end;
end;

function QuaternionExp(const AQuaternion:TVector4):TVector4;
var Angle,Sinus,Coefficent:TPasGLTFFloat;
begin
 Angle:=sqrt(sqr(AQuaternion[0])+sqr(AQuaternion[1])+sqr(AQuaternion[2]));
 Sinus:=sin(Angle);
 result[3]:=cos(Angle);
 if abs(Sinus)>1e-6 then begin
  Coefficent:=Sinus/Angle;
  result[0]:=AQuaternion[0]*Coefficent;
  result[1]:=AQuaternion[1]*Coefficent;
  result[2]:=AQuaternion[2]*Coefficent;
 end else begin
  result[0]:=AQuaternion[0];
  result[1]:=AQuaternion[1];
  result[2]:=AQuaternion[2];
 end;
end;

function QuaternionKochanekBartelsSplineInterpolate(const t,t0,t1,t2,t3:TPasGLTFFloat;q0,q1,q2,q3:TVector4;const Tension1,Continuity1,Bias1,Tension2,Continuity2,Bias2:TPasGLTFFloat):TVector4;
var qLog10,qLog21,qLog32,qTOut,qTIn:TVector4;
    AdjustMulOneMinusTensionMulHalf:TPasGLTFFloat;
begin
 if Vector4Dot(q0,q1)<0.0 then begin
  q1:=Vector4Neg(q1);
 end;
 if Vector4Dot(q1,q2)<0.0 then begin
  q2:=Vector4Neg(q2);
 end;
 if Vector4Dot(q2,q3)<0.0 then begin
  q3:=Vector4Neg(q3);
 end;
 qLog10:=QuaternionLog(QuaternionMul(QuaternionConjugate(q0),q1));
 qLog21:=QuaternionLog(QuaternionMul(QuaternionConjugate(q1),q2));
 qLog32:=QuaternionLog(QuaternionMul(QuaternionConjugate(q2),q3));
 AdjustMulOneMinusTensionMulHalf:=((((t2-t1)/(t2-t0)){*2.0})*(1.0-Tension1)){*0.5};
 qTOut:=QuaternionAdd(QuaternionScalarMul(qLog10,AdjustMulOneMinusTensionMulHalf*(1.0+Continuity1)*(1.0+Bias1)),
                      QuaternionScalarMul(qLog21,AdjustMulOneMinusTensionMulHalf*(1.0-Continuity1)*(1.0-Bias1)));
 AdjustMulOneMinusTensionMulHalf:=((((t2-t1)/(t3-t1)){*2.0})*(1.0-Tension2)){*0.5};
 qTIn:=QuaternionAdd(QuaternionScalarMul(qLog21,AdjustMulOneMinusTensionMulHalf*(1.0-Continuity2)*(1.0+Bias2)),
                     QuaternionScalarMul(qLog32,AdjustMulOneMinusTensionMulHalf*(1.0+Continuity2)*(1.0-Bias2)));
 result:=QuaternionUnflippedSlerp(QuaternionUnflippedSlerp(q1,q2,t),
                                  QuaternionUnflippedSlerp(QuaternionMul(q1,QuaternionExp(QuaternionScalarMul(QuaternionSub(qTOut,qLog21),0.5))),
                                                           QuaternionMul(q2,QuaternionExp(QuaternionScalarMul(QuaternionSub(qLog21,qTIn),0.5))),
                                                           t),
                                  2.0*(t*(1.0-t)));
end;

function MatrixFromRotation(const aRotation:TVector4):TMatrix;
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2,l:TPasGLTFFloat;
    Rotation:TPasGLTF.TVector4;
begin
 l:=sqrt(sqr(aRotation[0])+sqr(aRotation[1])+sqr(aRotation[2])+sqr(aRotation[3]));
 Rotation[0]:=aRotation[0]/l;
 Rotation[1]:=aRotation[1]/l;
 Rotation[2]:=aRotation[2]/l;
 Rotation[3]:=aRotation[3]/l;
 qx2:=Rotation[0]+Rotation[0];
 qy2:=Rotation[1]+Rotation[1];
 qz2:=Rotation[2]+Rotation[2];
 qxqx2:=Rotation[0]*qx2;
 qxqy2:=Rotation[0]*qy2;
 qxqz2:=Rotation[0]*qz2;
 qxqw2:=Rotation[3]*qx2;
 qyqy2:=Rotation[1]*qy2;
 qyqz2:=Rotation[1]*qz2;
 qyqw2:=Rotation[3]*qy2;
 qzqz2:=Rotation[2]*qz2;
 qzqw2:=Rotation[3]*qz2;
 result[0]:=1.0-(qyqy2+qzqz2);
 result[1]:=qxqy2+qzqw2;
 result[2]:=qxqz2-qyqw2;
 result[3]:=0.0;
 result[4]:=qxqy2-qzqw2;
 result[5]:=1.0-(qxqx2+qzqz2);
 result[6]:=qyqz2+qxqw2;
 result[7]:=0.0;
 result[8]:=qxqz2+qyqw2;
 result[9]:=qyqz2-qxqw2;
 result[10]:=1.0-(qxqx2+qyqy2);
 result[11]:=0.0;
 result[12]:=0.0;
 result[13]:=0.0;
 result[14]:=0.0;
 result[15]:=1.0;
end;

function MatrixFromScale(const aScale:TVector3):TMatrix;
begin
 result[0]:=aScale[0];
 result[1]:=0.0;
 result[2]:=0.0;
 result[3]:=0.0;
 result[4]:=0.0;
 result[5]:=aScale[1];
 result[6]:=0.0;
 result[7]:=0.0;
 result[8]:=0.0;
 result[9]:=0.0;
 result[10]:=aScale[2];
 result[11]:=0.0;
 result[12]:=0.0;
 result[13]:=0.0;
 result[14]:=0.0;
 result[15]:=1.0;
end;

function MatrixFromTranslation(const aTranslation:TVector3):TMatrix;
begin
 result[0]:=1.0;
 result[1]:=0.0;
 result[2]:=0.0;
 result[3]:=0.0;
 result[4]:=0.0;
 result[5]:=1.0;
 result[6]:=0.0;
 result[7]:=0.0;
 result[8]:=0.0;
 result[9]:=0.0;
 result[10]:=1.0;
 result[11]:=0.0;
 result[12]:=aTranslation[0];
 result[13]:=aTranslation[1];
 result[14]:=aTranslation[2];
 result[15]:=1.0;
end;

function MatrixMul(const a,b:TMatrix):TMatrix;
begin
 result[0]:=(a[0]*b[0])+(a[1]*b[4])+(a[2]*b[8])+(a[3]*b[12]);
 result[1]:=(a[0]*b[1])+(a[1]*b[5])+(a[2]*b[9])+(a[3]*b[13]);
 result[2]:=(a[0]*b[2])+(a[1]*b[6])+(a[2]*b[10])+(a[3]*b[14]);
 result[3]:=(a[0]*b[3])+(a[1]*b[7])+(a[2]*b[11])+(a[3]*b[15]);
 result[4]:=(a[4]*b[0])+(a[5]*b[4])+(a[6]*b[8])+(a[7]*b[12]);
 result[5]:=(a[4]*b[1])+(a[5]*b[5])+(a[6]*b[9])+(a[7]*b[13]);
 result[6]:=(a[4]*b[2])+(a[5]*b[6])+(a[6]*b[10])+(a[7]*b[14]);
 result[7]:=(a[4]*b[3])+(a[5]*b[7])+(a[6]*b[11])+(a[7]*b[15]);
 result[8]:=(a[8]*b[0])+(a[9]*b[4])+(a[10]*b[8])+(a[11]*b[12]);
 result[9]:=(a[8]*b[1])+(a[9]*b[5])+(a[10]*b[9])+(a[11]*b[13]);
 result[10]:=(a[8]*b[2])+(a[9]*b[6])+(a[10]*b[10])+(a[11]*b[14]);
 result[11]:=(a[8]*b[3])+(a[9]*b[7])+(a[10]*b[11])+(a[11]*b[15]);
 result[12]:=(a[12]*b[0])+(a[13]*b[4])+(a[14]*b[8])+(a[15]*b[12]);
 result[13]:=(a[12]*b[1])+(a[13]*b[5])+(a[14]*b[9])+(a[15]*b[13]);
 result[14]:=(a[12]*b[2])+(a[13]*b[6])+(a[14]*b[10])+(a[15]*b[14]);
 result[15]:=(a[12]*b[3])+(a[13]*b[7])+(a[14]*b[11])+(a[15]*b[15]);
end;

function MatrixInverse(const ma:TPasGLTF.TMatrix4x4):TPasGLTF.TMatrix4x4;
var Temporary:array[0..15] of TPasGLTFFloat;
    Det:TPasGLTFFloat;
begin
 Temporary[0]:=(((ma[5]*ma[10]*ma[15])-(ma[5]*ma[11]*ma[14]))-(ma[9]*ma[6]*ma[15])+(ma[9]*ma[7]*ma[14])+(ma[13]*ma[6]*ma[11]))-(ma[13]*ma[7]*ma[10]);
 Temporary[4]:=((((-(ma[4]*ma[10]*ma[15]))+(ma[4]*ma[11]*ma[14])+(ma[8]*ma[6]*ma[15]))-(ma[8]*ma[7]*ma[14]))-(ma[12]*ma[6]*ma[11]))+(ma[12]*ma[7]*ma[10]);
 Temporary[8]:=((((ma[4]*ma[9]*ma[15])-(ma[4]*ma[11]*ma[13]))-(ma[8]*ma[5]*ma[15]))+(ma[8]*ma[7]*ma[13])+(ma[12]*ma[5]*ma[11]))-(ma[12]*ma[7]*ma[9]);
 Temporary[12]:=((((-(ma[4]*ma[9]*ma[14]))+(ma[4]*ma[10]*ma[13])+(ma[8]*ma[5]*ma[14]))-(ma[8]*ma[6]*ma[13]))-(ma[12]*ma[5]*ma[10]))+(ma[12]*ma[6]*ma[9]);
 Temporary[1]:=((((-(ma[1]*ma[10]*ma[15]))+(ma[1]*ma[11]*ma[14])+(ma[9]*ma[2]*ma[15]))-(ma[9]*ma[3]*ma[14]))-(ma[13]*ma[2]*ma[11]))+(ma[13]*ma[3]*ma[10]);
 Temporary[5]:=(((ma[0]*ma[10]*ma[15])-(ma[0]*ma[11]*ma[14]))-(ma[8]*ma[2]*ma[15])+(ma[8]*ma[3]*ma[14])+(ma[12]*ma[2]*ma[11]))-(ma[12]*ma[3]*ma[10]);
 Temporary[9]:=((((-(ma[0]*ma[9]*ma[15]))+(ma[0]*ma[11]*ma[13])+(ma[8]*ma[1]*ma[15]))-(ma[8]*ma[3]*ma[13]))-(ma[12]*ma[1]*ma[11]))+(ma[12]*ma[3]*ma[9]);
 Temporary[13]:=((((ma[0]*ma[9]*ma[14])-(ma[0]*ma[10]*ma[13]))-(ma[8]*ma[1]*ma[14]))+(ma[8]*ma[2]*ma[13])+(ma[12]*ma[1]*ma[10]))-(ma[12]*ma[2]*ma[9]);
 Temporary[2]:=((((ma[1]*ma[6]*ma[15])-(ma[1]*ma[7]*ma[14]))-(ma[5]*ma[2]*ma[15]))+(ma[5]*ma[3]*ma[14])+(ma[13]*ma[2]*ma[7]))-(ma[13]*ma[3]*ma[6]);
 Temporary[6]:=((((-(ma[0]*ma[6]*ma[15]))+(ma[0]*ma[7]*ma[14])+(ma[4]*ma[2]*ma[15]))-(ma[4]*ma[3]*ma[14]))-(ma[12]*ma[2]*ma[7]))+(ma[12]*ma[3]*ma[6]);
 Temporary[10]:=((((ma[0]*ma[5]*ma[15])-(ma[0]*ma[7]*ma[13]))-(ma[4]*ma[1]*ma[15]))+(ma[4]*ma[3]*ma[13])+(ma[12]*ma[1]*ma[7]))-(ma[12]*ma[3]*ma[5]);
 Temporary[14]:=((((-(ma[0]*ma[5]*ma[14]))+(ma[0]*ma[6]*ma[13])+(ma[4]*ma[1]*ma[14]))-(ma[4]*ma[2]*ma[13]))-(ma[12]*ma[1]*ma[6]))+(ma[12]*ma[2]*ma[5]);
 Temporary[3]:=((((-(ma[1]*ma[6]*ma[11]))+(ma[1]*ma[7]*ma[10])+(ma[5]*ma[2]*ma[11]))-(ma[5]*ma[3]*ma[10]))-(ma[9]*ma[2]*ma[7]))+(ma[9]*ma[3]*ma[6]);
 Temporary[7]:=((((ma[0]*ma[6]*ma[11])-(ma[0]*ma[7]*ma[10]))-(ma[4]*ma[2]*ma[11]))+(ma[4]*ma[3]*ma[10])+(ma[8]*ma[2]*ma[7]))-(ma[8]*ma[3]*ma[6]);
 Temporary[11]:=((((-(ma[0]*ma[5]*ma[11]))+(ma[0]*ma[7]*ma[9])+(ma[4]*ma[1]*ma[11]))-(ma[4]*ma[3]*ma[9]))-(ma[8]*ma[1]*ma[7]))+(ma[8]*ma[3]*ma[5]);
 Temporary[15]:=((((ma[0]*ma[5]*ma[10])-(ma[0]*ma[6]*ma[9]))-(ma[4]*ma[1]*ma[10]))+(ma[4]*ma[2]*ma[9])+(ma[8]*ma[1]*ma[6]))-(ma[8]*ma[2]*ma[5]);
 Det:=(ma[0]*Temporary[0])+(ma[1]*Temporary[4])+(ma[2]*Temporary[8])+(ma[3]*Temporary[12]);
 if not IsZero(Det) then begin
  Det:=1.0/Det;
  result[0]:=Temporary[0]*Det;
  result[1]:=Temporary[1]*Det;
  result[2]:=Temporary[2]*Det;
  result[3]:=Temporary[3]*Det;
  result[4]:=Temporary[4]*Det;
  result[5]:=Temporary[5]*Det;
  result[6]:=Temporary[6]*Det;
  result[7]:=Temporary[7]*Det;
  result[8]:=Temporary[8]*Det;
  result[9]:=Temporary[9]*Det;
  result[10]:=Temporary[10]*Det;
  result[11]:=Temporary[11]*Det;
  result[12]:=Temporary[12]*Det;
  result[13]:=Temporary[13]*Det;
  result[14]:=Temporary[14]*Det;
  result[15]:=Temporary[15]*Det;
 end else begin
  result:=ma;
 end;
end;

{ TGLTFModel }

constructor TGLTFOpenGL.Create(const aDocument:TPasGLTF.TDocument);
begin
 inherited Create;
 fDocument:=aDocument;
 fReady:=false;
 fUploaded:=false;
 fAnimations:=nil;
 fMaterials:=nil;
 fMeshes:=nil;
 fSkins:=nil;
 fNodes:=nil;
 fTextures:=nil;
end;

destructor TGLTFOpenGL.Destroy;
begin
 UnloadResources;
 FinalizeResources;
 inherited Destroy;
end;

procedure TGLTFOpenGL.InitializeResources;
 procedure InitializeAnimations;
 var Index,ChannelIndex,ValueIndex:TPasGLTFSizeInt;
     SourceAnimation:TPasGLTF.TAnimation;
     DestinationAnimation:PAnimation;
     SourceAnimationChannel:TPasGLTF.TAnimation.TChannel;
     SourceAnimationSampler:TPasGLTF.TAnimation.TSampler;
     DestinationAnimationChannel:TAnimation.PChannel;
 begin

  SetLength(fAnimations,fDocument.Animations.Count);

  for Index:=0 to fDocument.Animations.Count-1 do begin

   SourceAnimation:=fDocument.Animations.Items[Index];

   DestinationAnimation:=@fAnimations[Index];

   SetLength(DestinationAnimation^.Channels,SourceAnimation.Channels.Count);

   for ChannelIndex:=0 to SourceAnimation.Channels.Count-1 do begin

    SourceAnimationChannel:=SourceAnimation.Channels[ChannelIndex];

    DestinationAnimationChannel:=@DestinationAnimation^.Channels[ChannelIndex];

    DestinationAnimationChannel^.Node:=SourceAnimationChannel.Target.Node;

    if SourceAnimationChannel.Target.Path='translation' then begin
     DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Translation;
    end else if SourceAnimationChannel.Target.Path='rotation' then begin
     DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Rotation;
    end else if SourceAnimationChannel.Target.Path='scale' then begin
     DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Scale;
    end else if SourceAnimationChannel.Target.Path='weights' then begin
     DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Weights;
    end else begin
     raise EGLTFOpenGL.Create('Non-supported animation channel target path "'+String(SourceAnimationChannel.Target.Path)+'"');
    end;

    if (SourceAnimationChannel.Sampler>=0) and (SourceAnimationChannel.Sampler<SourceAnimation.Samplers.Count) then begin
     SourceAnimationSampler:=SourceAnimation.Samplers[SourceAnimationChannel.Sampler];
     case SourceAnimationSampler.Interpolation of
      TPasGLTF.TAnimation.TSampler.TType.Linear:begin
       DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Linear;
      end;
      TPasGLTF.TAnimation.TSampler.TType.Step:begin
       DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Step;
      end;
      TPasGLTF.TAnimation.TSampler.TType.CubicSpline:begin
       DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.CubicSpline;
      end;
      else begin
       raise EGLTFOpenGL.Create('Non-supported animation sampler interpolation method type');
      end;
     end;
     DestinationAnimationChannel^.InputTimeArray:=fDocument.Accessors[SourceAnimationSampler.Input].DecodeAsFloatArray(false);
     case DestinationAnimationChannel^.Target of
      TAnimation.TChannel.TTarget.Translation,
      TAnimation.TChannel.TTarget.Scale:begin
       DestinationAnimationChannel^.OutputVector3Array:=fDocument.Accessors[SourceAnimationSampler.Output].DecodeAsVector3Array(false);
      end;
      TAnimation.TChannel.TTarget.Rotation:begin
       DestinationAnimationChannel^.OutputVector4Array:=fDocument.Accessors[SourceAnimationSampler.Output].DecodeAsVector4Array(false);
       for ValueIndex:=0 to length(DestinationAnimationChannel^.OutputVector4Array)-1 do begin
        DestinationAnimationChannel^.OutputVector4Array[ValueIndex]:=Vector4Normalize(DestinationAnimationChannel^.OutputVector4Array[ValueIndex]);
       end;
      end;
      TAnimation.TChannel.TTarget.Weights:begin
       DestinationAnimationChannel^.OutputScalarArray:=fDocument.Accessors[SourceAnimationSampler.Output].DecodeAsFloatArray(false);
      end;
     end;
    end else begin
     raise EGLTFOpenGL.Create('Non-existent sampler');
    end;

   end;

  end;

 end;
 procedure InitializeMaterials;
 var Index:TPasGLTFSizeInt;
     SourceMaterial:TPasGLTF.TMaterial;
     DestinationMaterial:PMaterial;
     JSONItem:TPasJSONItem;
     JSONObject:TPasJSONItemObject;
 begin

  SetLength(fMaterials,fDocument.Materials.Count);

  for Index:=0 to fDocument.Materials.Count-1 do begin

   SourceMaterial:=fDocument.Materials.Items[Index];

   DestinationMaterial:=@fMaterials[Index];

   JSONItem:=SourceMaterial.Extensions.Properties['KHR_materials_pbrSpecularGlossiness'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    JSONObject:=TPasJSONItemObject(JSONItem);
    DestinationMaterial.PBRSpecularGlossiness.Used:=true;
    DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor:=TPasGLTF.TDefaults.IdentityVector4;
    DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.Index:=-1;
    DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.TexCoord:=0;
    DestinationMaterial.PBRSpecularGlossiness.GlossinessFactor:=TPasGLTF.TDefaults.IdentityScalar;
    DestinationMaterial.PBRSpecularGlossiness.SpecularFactor:=TPasGLTF.TDefaults.IdentityVector3;
    DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.Index:=-1;
    DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord:=0;
    begin
     JSONItem:=JSONObject.Properties['diffuseFactor'];
     if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=4) then begin
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[0]);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[1]);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[2]);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[3]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[3],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[3]);
     end;
     JSONItem:=JSONObject.Properties['diffuseTexture'];
     if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
      DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.Index);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],DestinationMaterial.PBRSpecularGlossiness.DiffuseTexture.TexCoord);
     end;
     DestinationMaterial.PBRSpecularGlossiness.GlossinessFactor:=TPasJSON.GetNumber(JSONObject.Properties['glossinessFactor'],DestinationMaterial.PBRSpecularGlossiness.GlossinessFactor);
     JSONItem:=JSONObject.Properties['specularFactor'];
     if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
      DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[0]);
      DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[1]);
      DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[2]);
     end;
     JSONItem:=JSONObject.Properties['specularGlossinessTexture'];
     if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
      DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.Index);
      DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],DestinationMaterial.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord);
     end;
    end;
   end else begin
    DestinationMaterial.PBRSpecularGlossiness.Used:=false;
   end;

  end;

 end;
 procedure InitializeMeshes;
 var Index,
     PrimitiveIndex,
     AccessorIndex,
     IndexIndex,
     VertexIndex:TPasGLTFSizeInt;
     SourceMesh:TPasGLTF.TMesh;
     SourceMeshPrimitive:TPasGLTF.TMesh.TPrimitive;
     DestinationMesh:PMesh;
     DestinationMeshPrimitive:TMesh.PPrimitive;
     TemporaryPositions,
     TemporaryNormals,
     TemporaryBitangents:TPasGLTF.TVector3DynamicArray;
     TemporaryTangents,
     TemporaryColor0,
     TemporaryJoints0,
     TemporaryJoints1,
     TemporaryWeights0,
     TemporaryWeights1:TPasGLTF.TVector4DynamicArray;
     TemporaryTexCoord0,
     TemporaryTexCoord1:TPasGLTF.TVector2DynamicArray;
     TemporaryIndices,
     TemporaryTriangleIndices:TPasGLTFUInt32DynamicArray;
     Normal,Tangent,Bitangent,p1p0,p2p0:TVector3;
     p0,p1,p2:PVector3;
     t1t0,t2t0:TVector2;
     t0,t1,t2:PVector2;
     Vertex:PVertex;
     Area:TPasGLTFFloat;
 begin

  SetLength(fMeshes,fDocument.Meshes.Count);

  for Index:=0 to fDocument.Meshes.Count-1 do begin

   SourceMesh:=fDocument.Meshes.Items[Index];

   DestinationMesh:=@fMeshes[Index];

   SetLength(DestinationMesh^.Primitives,SourceMesh.Primitives.Count);

   DestinationMesh^.MinPosition[0]:=Infinity;
   DestinationMesh^.MinPosition[1]:=Infinity;
   DestinationMesh^.MinPosition[2]:=Infinity;

   DestinationMesh^.MaxPosition[0]:=NegInfinity;
   DestinationMesh^.MaxPosition[1]:=NegInfinity;
   DestinationMesh^.MaxPosition[2]:=NegInfinity;

   for PrimitiveIndex:=0 to SourceMesh.Primitives.Count-1 do begin

    SourceMeshPrimitive:=SourceMesh.Primitives.Items[PrimitiveIndex];

    DestinationMeshPrimitive:=@DestinationMesh^.Primitives[PrimitiveIndex];

    DestinationMeshPrimitive^.Material:=SourceMeshPrimitive.Material;

    begin
     // Load accessor data
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['POSITION'];
      if AccessorIndex>=0 then begin
       TemporaryPositions:=fDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
       DestinationMesh^.MinPosition[0]:=Min(DestinationMesh^.MinPosition[0],fDocument.Accessors[AccessorIndex].MinArray[0]);
       DestinationMesh^.MinPosition[1]:=Min(DestinationMesh^.MinPosition[1],fDocument.Accessors[AccessorIndex].MinArray[1]);
       DestinationMesh^.MinPosition[2]:=Min(DestinationMesh^.MinPosition[2],fDocument.Accessors[AccessorIndex].MinArray[2]);
       DestinationMesh^.MaxPosition[0]:=Max(DestinationMesh^.MaxPosition[0],fDocument.Accessors[AccessorIndex].MaxArray[0]);
       DestinationMesh^.MaxPosition[1]:=Max(DestinationMesh^.MaxPosition[1],fDocument.Accessors[AccessorIndex].MaxArray[1]);
       DestinationMesh^.MaxPosition[2]:=Max(DestinationMesh^.MaxPosition[2],fDocument.Accessors[AccessorIndex].MaxArray[2]);
      end else begin
       raise EGLTFOpenGL.Create('Missing position data');
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['NORMAL'];
      if AccessorIndex>=0 then begin
       TemporaryNormals:=fDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
      end else begin
       TemporaryNormals:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['TANGENT'];
      if AccessorIndex>=0 then begin
       TemporaryTangents:=fDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
      end else begin
       TemporaryTangents:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['TEXCOORD_0'];
      if AccessorIndex>=0 then begin
       TemporaryTexCoord0:=fDocument.Accessors[AccessorIndex].DecodeAsVector2Array(true);
      end else begin
       TemporaryTexCoord0:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['TEXCOORD_1'];
      if AccessorIndex>=0 then begin
       TemporaryTexCoord1:=fDocument.Accessors[AccessorIndex].DecodeAsVector2Array(true);
      end else begin
       TemporaryTexCoord1:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['COLOR_0'];
      if AccessorIndex>=0 then begin
       TemporaryColor0:=fDocument.Accessors[AccessorIndex].DecodeAsColorArray(true);
      end else begin
       TemporaryColor0:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['JOINTS_0'];
      if AccessorIndex>=0 then begin
       TemporaryJoints0:=fDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
      end else begin
       TemporaryJoints0:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['JOINTS_1'];
      if AccessorIndex>=0 then begin
       TemporaryJoints1:=fDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
      end else begin
       TemporaryJoints1:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['WEIGHTS_0'];
      if AccessorIndex>=0 then begin
       TemporaryWeights0:=fDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
      end else begin
       TemporaryWeights0:=nil;
      end;
     end;
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['WEIGHTS_1'];
      if AccessorIndex>=0 then begin
       TemporaryWeights1:=fDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
      end else begin
       TemporaryWeights1:=nil;
      end;
     end;
    end;

    begin
     // load or generate vertex indices
     if SourceMeshPrimitive.Indices>=0 then begin
      TemporaryIndices:=fDocument.Accessors[SourceMeshPrimitive.Indices].DecodeAsUInt32Array(false);
     end else begin
      SetLength(TemporaryIndices,length(TemporaryPositions));
      for IndexIndex:=0 to length(TemporaryIndices)-1 do begin
       TemporaryIndices[IndexIndex]:=IndexIndex;
      end;
     end;
     case SourceMeshPrimitive.Mode of
      TPasGLTF.TMesh.TPrimitive.TMode.Triangles:begin
       TemporaryTriangleIndices:=TemporaryIndices;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip:begin
       TemporaryTriangleIndices:=nil;
       // TODO
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
       TemporaryTriangleIndices:=nil;
       // TODO
      end;
      else begin
       TemporaryTriangleIndices:=nil;
      end;
     end;
    end;

    begin
     // Generate missing data
     if length(TemporaryNormals)<>length(TemporaryPositions) then begin
      SetLength(TemporaryNormals,length(TemporaryPositions));
      for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
       TemporaryNormals[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
      end;
      if length(TemporaryTriangleIndices)>0 then begin
       IndexIndex:=0;
       while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
        p0:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+0]];
        p1:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+1]];
        p2:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+2]];
        Normal:=Vector3Cross(Vector3Sub(p1^,p0^),Vector3Sub(p2^,p0^)); // non-normalized weighted normal
        TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]]:=Vector3Add(TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]],Normal);
        TemporaryNormals[TemporaryTriangleIndices[IndexIndex+1]]:=Vector3Add(TemporaryNormals[TemporaryTriangleIndices[IndexIndex+1]],Normal);
        TemporaryNormals[TemporaryTriangleIndices[IndexIndex+2]]:=Vector3Add(TemporaryNormals[TemporaryTriangleIndices[IndexIndex+2]],Normal);
        inc(IndexIndex,3);
       end;
       for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
        TemporaryNormals[VertexIndex]:=Vector3Normalize(TemporaryNormals[VertexIndex]);
       end;
      end;
     end;
     if length(TemporaryTexCoord0)<>length(TemporaryPositions) then begin
      SetLength(TemporaryTexCoord0,length(TemporaryPositions));
      for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
       TemporaryTexCoord0[VertexIndex]:=PVector2(@TPasGLTF.TDefaults.NullVector3)^;
      end;
     end;
     if length(TemporaryTangents)<>length(TemporaryPositions) then begin
      SetLength(TemporaryTangents,length(TemporaryPositions));
      SetLength(TemporaryBitangents,length(TemporaryPositions));
      for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
       PVector3(@TemporaryTangents[VertexIndex])^:=TPasGLTF.TDefaults.NullVector3;
       TemporaryBitangents[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
      end;
      if length(TemporaryTriangleIndices)>0 then begin
       IndexIndex:=0;
       while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
        p0:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+0]];
        p1:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+1]];
        p2:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+2]];
        t0:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+0]];
        t1:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+1]];
        t2:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+2]];
        p1p0:=Vector3Sub(p1^,p0^);
        p2p0:=Vector3Sub(p2^,p0^);
        t1t0:=Vector2Sub(t1^,t0^);
        t2t0:=Vector2Sub(t2^,t0^);
        Normal:=Vector3Normalize(Vector3Cross(p1p0,p2p0));
        if Vector3Dot(TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]],Normal)<0.0 then begin
         Normal:=Vector3Neg(Normal);
        end;
        Area:=(t1t0[0]*t2t0[1])-(t2t0[0]*t1t0[1]);
        if IsZero(Area) then begin
         Tangent[0]:=0.0;
         Tangent[1]:=1.0;
         Tangent[2]:=0.0;
         Bitangent[0]:=1.0;
         Bitangent[1]:=0.0;
         Bitangent[2]:=0.0;
        end else begin
         Tangent[0]:=((t2t0[1]*p1p0[0])-(t1t0[1]*p2p0[0]))/Area;
         Tangent[1]:=((t2t0[1]*p1p0[1])-(t1t0[1]*p2p0[1]))/Area;
         Tangent[2]:=((t2t0[1]*p1p0[2])-(t1t0[1]*p2p0[2]))/Area;
         Bitangent[0]:=((t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]))/Area;
         Bitangent[1]:=((t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]))/Area;
         Bitangent[2]:=((t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]))/Area;
        end;
{       Tangent[0]:=(t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]);
        Tangent[1]:=(t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]);
        Tangent[2]:=(t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]);
        Bitangent[0]:=(t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]);
        Bitangent[1]:=(t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]);
        Bitangent[2]:=(t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]);}
        if Vector3Dot(Vector3Cross(Bitangent,Tangent),Normal)<0.0 then begin
         Tangent:=Vector3Neg(Tangent);
         Bitangent:=Vector3Neg(Bitangent);
        end;
        PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]])^:=Vector3Add(PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]])^,Tangent);
        PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]])^:=Vector3Add(PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]])^,Tangent);
        PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]])^:=Vector3Add(PVector3(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]])^,Tangent);
        TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]]:=Vector3Add(TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]],Bitangent);
        TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]]:=Vector3Add(TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]],Bitangent);
        TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]]:=Vector3Add(TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]],Bitangent);
        inc(IndexIndex,3);
       end;
       for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
        Normal:=TemporaryNormals[VertexIndex];
        Tangent:=Vector3Normalize(PVector3(@TemporaryTangents[VertexIndex])^);
        Tangent:=Vector3Normalize(Vector3Sub(Tangent,Vector3Scale(Normal,Vector3Dot(Tangent,Normal))));
        Bitangent:=Vector3Normalize(TemporaryBitangents[VertexIndex]);
        Bitangent:=Vector3Normalize(Vector3Sub(Bitangent,Vector3Scale(Normal,Vector3Dot(Bitangent,Normal))));
        PVector3(@TemporaryTangents[VertexIndex])^:=Tangent;
        if Vector3Dot(Vector3Cross(TemporaryNormals[VertexIndex],Tangent),Bitangent)<0.0 then begin
         TemporaryTangents[VertexIndex,3]:=-1.0;
        end else begin
         TemporaryTangents[VertexIndex,3]:=1.0;
        end;
       end;
      end;
     end;
    end;

    begin
     // Primitive mode
     case SourceMeshPrimitive.Mode of
      TPasGLTF.TMesh.TPrimitive.TMode.Points:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_POINTS;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.Lines:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_LINES;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.LineLoop:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_LINE_LOOP;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.LineStrip:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_LINE_STRIP;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.Triangles:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_TRIANGLES;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_TRIANGLE_STRIP;
      end;
      TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
       DestinationMeshPrimitive^.PrimitiveMode:=GL_TRIANGLE_FAN;
      end;
      else begin
       raise EGLTFOpenGL.Create('Invalid primitive mode');
      end;
     end;
    end;

    begin
     // Generate vertex array buffer
     SetLength(DestinationMeshPrimitive^.Vertices,length(TemporaryPositions));
     for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
      Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
      FillChar(Vertex^,SizeOf(TVertex),#0);
      Vertex^.Position:=TemporaryPositions[VertexIndex];
      if VertexIndex<length(TemporaryNormals) then begin
       Vertex^.Normal:=TemporaryNormals[VertexIndex];
      end;
      if VertexIndex<length(TemporaryTangents) then begin
       Vertex^.Tangent:=TemporaryTangents[VertexIndex];
      end;
      if VertexIndex<length(TemporaryTexCoord0) then begin
       Vertex^.TexCoord0:=TemporaryTexCoord0[VertexIndex];
      end;
      if VertexIndex<length(TemporaryTexCoord1) then begin
       Vertex^.TexCoord1:=TemporaryTexCoord1[VertexIndex];
      end;
      if VertexIndex<length(TemporaryColor0) then begin
       Vertex^.Color0:=TemporaryColor0[VertexIndex];
      end else begin
       Vertex^.Color0:=TPasGLTF.TDefaults.IdentityVector4;
      end;
      if VertexIndex<length(TemporaryJoints0) then begin
       Vertex^.Joints0:=TemporaryJoints0[VertexIndex];
      end;
      if VertexIndex<length(TemporaryJoints1) then begin
       Vertex^.Joints1:=TemporaryJoints1[VertexIndex];
      end;
      if VertexIndex<length(TemporaryWeights0) then begin
       Vertex^.Weights0:=TemporaryWeights0[VertexIndex];
      end;
      if VertexIndex<length(TemporaryWeights1) then begin
       Vertex^.Weights1:=TemporaryWeights1[VertexIndex];
      end;
     end;
    end;

    begin
     // Generate vertex index array buffer
     DestinationMeshPrimitive^.Indices:=copy(TemporaryIndices);
    end;

   end;

  end;

 end;
 procedure InitializeSkins;
 var Index,JointIndex,OldCount:TPasGLTFSizeInt;
     SourceSkin:TPasGLTF.TSkin;
     DestinationSkin:PSkin;
     JSONItem:TPasJSONItem;
     JSONObject:TPasJSONItemObject;
 begin

  SetLength(fSkins,fDocument.Skins.Count);

  for Index:=0 to fDocument.Skins.Count-1 do begin

   SourceSkin:=fDocument.Skins.Items[Index];

   DestinationSkin:=@fSkins[Index];

   DestinationSkin^.Skeleton:=SourceSkin.Skeleton;

   if SourceSkin.InverseBindMatrices>=0 then begin
    DestinationSkin^.InverseBindMatrices:=fDocument.Accessors[SourceSkin.InverseBindMatrices].DecodeAsMatrix4x4Array(false);
   end else begin
    DestinationSkin^.InverseBindMatrices:=nil;
   end;

   SetLength(DestinationSkin^.Matrices,SourceSkin.Joints.Count);

   SetLength(DestinationSkin^.Joints,SourceSkin.Joints.Count);
   for JointIndex:=0 to length(DestinationSkin^.Joints)-1 do begin
    DestinationSkin^.Joints[JointIndex]:=SourceSkin.Joints[JointIndex];
   end;

   OldCount:=length(DestinationSkin^.InverseBindMatrices);
   if OldCount<SourceSkin.Joints.Count then begin
    SetLength(DestinationSkin^.InverseBindMatrices,SourceSkin.Joints.Count);
    for JointIndex:=0 to length(DestinationSkin^.InverseBindMatrices)-1 do begin
     DestinationSkin^.InverseBindMatrices[JointIndex]:=TPasGLTF.TDefaults.IdentityMatrix4x4;
    end;
   end;

  end;

 end;
 procedure InitializeNodes;
 begin
  SetLength(fNodes,fDocument.Nodes.Count);
 end;
 procedure InitializeTextures;
 begin
  SetLength(fTextures,fDocument.Textures.Count);
 end;
 procedure ProcessNode(const aNodeIndex:TPasGLTFSizeInt;const aMatrix:TMatrix);
 var Index:TPasGLTFSizeInt;
     Matrix:TPasGLTF.TMatrix4x4;
     Node:TPasGLTF.TNode;
     TemporaryVector3:TPasGLTF.TVector3;
     Mesh:PMesh;
 begin
  Node:=fDocument.Nodes[aNodeIndex];
  Matrix:=MatrixMul(
           MatrixMul(
            MatrixMul(
             MatrixFromScale(Node.Scale),
             MatrixMul(
              MatrixFromRotation(Node.Rotation),
              MatrixFromTranslation(Node.Translation))),
            Node.Matrix),
           aMatrix);
  if Node.Mesh>=0 then begin
   Mesh:=@fMeshes[Node.Mesh];
   TemporaryVector3:=Vector3MatrixMul(Matrix,Mesh^.MinPosition);
   fStaticMinPosition[0]:=Min(fStaticMinPosition[0],TemporaryVector3[0]);
   fStaticMinPosition[1]:=Min(fStaticMinPosition[1],TemporaryVector3[1]);
   fStaticMinPosition[2]:=Min(fStaticMinPosition[2],TemporaryVector3[2]);
   fStaticMaxPosition[0]:=Max(fStaticMaxPosition[0],TemporaryVector3[0]);
   fStaticMaxPosition[1]:=Max(fStaticMaxPosition[1],TemporaryVector3[1]);
   fStaticMaxPosition[2]:=Max(fStaticMaxPosition[2],TemporaryVector3[2]);
   TemporaryVector3:=Vector3MatrixMul(Matrix,Mesh^.MaxPosition);
   fStaticMinPosition[0]:=Min(fStaticMinPosition[0],TemporaryVector3[0]);
   fStaticMinPosition[1]:=Min(fStaticMinPosition[1],TemporaryVector3[1]);
   fStaticMinPosition[2]:=Min(fStaticMinPosition[2],TemporaryVector3[2]);
   fStaticMaxPosition[0]:=Max(fStaticMaxPosition[0],TemporaryVector3[0]);
   fStaticMaxPosition[1]:=Max(fStaticMaxPosition[1],TemporaryVector3[1]);
   fStaticMaxPosition[2]:=Max(fStaticMaxPosition[2],TemporaryVector3[2]);
  end;
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessNode(Node.Children.Items[Index],Matrix);
  end;
 end;
var Index:TPasGLTFSizeInt;
    Scene:TPasGLTF.TScene;
begin
 if not fReady then begin
  InitializeAnimations;
  InitializeMaterials;
  InitializeMeshes;
  InitializeSkins;
  InitializeNodes;
  InitializeTextures;
  begin
   fStaticMinPosition[0]:=Infinity;
   fStaticMinPosition[1]:=Infinity;
   fStaticMinPosition[2]:=Infinity;
   fStaticMaxPosition[0]:=NegInfinity;
   fStaticMaxPosition[1]:=NegInfinity;
   fStaticMaxPosition[2]:=NegInfinity;
   for Scene in fDocument.Scenes do begin
    for Index:=0 to Scene.Nodes.Count-1 do begin
     ProcessNode(Scene.Nodes.Items[Index],TPasGLTF.TDefaults.IdentityMatrix4x4);
    end;
   end;
  end;
  fReady:=true;
 end;
end;

procedure TGLTFOpenGL.FinalizeResources;
begin
 if fReady then begin
  fReady:=false;
  fAnimations:=nil;
  fMaterials:=nil;
  fMeshes:=nil;
  fSkins:=nil;
  fNodes:=nil;
  fTextures:=nil;
 end;
end;

procedure TGLTFOpenGL.UploadResources;
type TAllVertices=TPasGLTFDynamicArray<TVertex>;
     TAllIndices=TPasGLTFDynamicArray<TPasGLTFUInt32>;
var AllVertices:TAllVertices;
    AllIndices:TAllIndices;
 procedure CollectVerticesAndIndicesFromMeshes;
 var Index,
     PrimitiveIndex,
     VertexIndex,
     IndexIndex:TPasGLTFSizeInt;
     Mesh:PMesh;
     Primitive:TMesh.PPrimitive;
 begin
  for Index:=0 to length(fMeshes)-1 do begin
   Mesh:=@fMeshes[Index];
   for PrimitiveIndex:=0 to length(Mesh^.Primitives)-1 do begin
    Primitive:=@Mesh^.Primitives[PrimitiveIndex];
    Primitive^.StartBufferVertexOffset:=AllVertices.Count;
    Primitive^.StartBufferIndexOffset:=AllIndices.Count;
    Primitive^.CountVertices:=length(Primitive^.Vertices);
    Primitive^.CountIndices:=length(Primitive^.Indices);
    AllVertices.Add(Primitive^.Vertices);
    AllIndices.Add(Primitive^.Indices);
    for IndexIndex:=Primitive^.StartBufferIndexOffset to (Primitive^.StartBufferIndexOffset+Primitive^.CountIndices)-1 do begin
     AllIndices[IndexIndex]:=AllIndices[IndexIndex]+Primitive^.StartBufferVertexOffset;
    end;
   end;
  end;
 end;
 procedure CreateOpenGLObjects;
 begin

  glGenBuffers(1,@fVertexBufferObjectHandle);
  glBindBuffer(GL_ARRAY_BUFFER,fVertexBufferObjectHandle);
  glBufferData(GL_ARRAY_BUFFER,AllVertices.Count*SizeOf(TVertex),AllVertices.Memory,GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);

  glGenBuffers(1,@fIndexBufferObjectHandle);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,fIndexBufferObjectHandle);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER,AllIndices.Count*SizeOf(TPasGLTFUInt32),AllIndices.Memory,GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,0);

  glGenVertexArrays(1,@fVertexArrayHandle);
  glBindVertexArray(fVertexArrayHandle);
  glBindBuffer(GL_ARRAY_BUFFER,fVertexBufferObjectHandle);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,fIndexBufferObjectHandle);
  begin
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Position,3,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Position);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Position);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Normal,3,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Normal);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Normal);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Tangent,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Tangent);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Tangent);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.TexCoord0,2,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.TexCoord0);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.TexCoord0);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.TexCoord1,2,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.TexCoord1);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.TexCoord1);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Color0,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Color0);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Color0);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Joints0,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Joints0);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Joints0);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Joints1,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Joints1);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Joints1);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Weights0,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Weights0);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Weights0);
   end;
   begin
    glVertexAttribPointer(TVertexAttributeBindingLocations.Weights1,4,GL_FLOAT,GL_FALSE,SizeOf(TVertex),@PVertex(nil)^.Weights1);
    glEnableVertexAttribArray(TVertexAttributeBindingLocations.Weights1);
   end;
  end;
  glBindVertexArray(0);

 end;
 procedure LoadTextures;
 var Index:TPasGLTFSizeInt;
     SourceTexture:TPasGLTF.TTexture;
     SourceSampler:TPasGLTF.TSampler;
     MemoryStream:TMemoryStream;
     ImageData:TPasGLTFPointer;
     ImageWidth,ImageHeight:TPasGLTFInt32;
     Handle:glUInt;
     Anisotropy:TPasGLTFFloat;
 begin
  for Index:=0 to length(fTextures)-1 do begin
   Handle:=0;
   SourceTexture:=fDocument.Textures[Index];
   if (SourceTexture.Source>=0) and (SourceTexture.Source<fDocument.Images.Count) then begin
    MemoryStream:=TMemoryStream.Create;
    try
     fDocument.Images[SourceTexture.Source].GetResourceData(MemoryStream);
     if LoadImage(MemoryStream.Memory,MemoryStream.Size,ImageData,ImageWidth,ImageHeight) then begin
      try
       glGenTextures(1,@Handle);
       glBindTexture(GL_TEXTURE_2D,Handle);
       if (SourceTexture.Sampler>=0) and (SourceTexture.Sampler<fDocument.Samplers.Count) then begin
        SourceSampler:=fDocument.Samplers[SourceTexture.Sampler];
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,glEnum(SourceSampler.WrapS));
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,glEnum(SourceSampler.WrapT));
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,glEnum(SourceSampler.MinFilter));
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,glEnum(SourceSampler.MagFilter));
       end else begin
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
       end;
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_BASE_LEVEL,0);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAX_LEVEL,trunc(log2(Min(ImageWidth,ImageHeight))));
       glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,ImageWidth,ImageHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
       glGenerateMipmap(GL_TEXTURE_2D);
       glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY,@Anisotropy);
       glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAX_ANISOTROPY,Anisotropy);
     finally
       FreeMem(ImageData);
      end;
     end;
    finally
     FreeAndNil(MemoryStream);
    end;
   end;
   fTextures[Index].Handle:=Handle;
  end;
  glBindTexture(GL_TEXTURE_2D,0);
 end;
 procedure CreateSkinUniformBufferObjects;
 var Index:TPasGLTFSizeInt;
     Skin:PSkin;
 begin
  for Index:=0 to length(fSkins)-1 do begin
   Skin:=@fSkins[Index];
   glGenBuffers(1,@Skin^.UniformBufferObjectHandle);
   glBindBuffer(GL_UNIFORM_BUFFER,Skin^.UniformBufferObjectHandle);
   glBufferData(GL_UNIFORM_BUFFER,length(Skin^.Joints)*SizeOf(TPasGLTF.TMatrix4x4),nil,GL_DYNAMIC_DRAW);
   glBindBuffer(GL_UNIFORM_BUFFER,0);
  end;
 end;
begin
 if not fUploaded then begin
  fUploaded:=true;
  AllVertices:=TAllVertices.Create;
  try
   AllIndices:=TAllIndices.Create;
   try
    CollectVerticesAndIndicesFromMeshes;
    CreateOpenGLObjects;
    LoadTextures;
    CreateSkinUniformBufferObjects;
   finally
    FreeAndNil(AllIndices);
   end;
  finally
   FreeAndNil(AllVertices);
  end;
 end;
end;

procedure TGLTFOpenGL.UnloadResources;
 procedure DeleteOpenGLObjects;
 begin
  glBindBuffer(GL_ARRAY_BUFFER,0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,0);
  glBindVertexArray(0);
  glDeleteVertexArrays(1,@fVertexArrayHandle);
  glDeleteBuffers(1,@fVertexBufferObjectHandle);
  glDeleteBuffers(1,@fIndexBufferObjectHandle);
 end;
 procedure UnloadTextures;
 var Index:TPasGLTFSizeInt;
 begin
  for Index:=0 to length(fTextures)-1 do begin
   if fTextures[Index].Handle>0 then begin
    glDeleteTextures(1,@fTextures[Index].Handle);
   end;
  end;
 end;
 procedure DeleteSkinUniformBufferObjects;
 var Index:TPasGLTFSizeInt;
     Skin:PSkin;
 begin
  for Index:=0 to length(fSkins)-1 do begin
   Skin:=@fSkins[Index];
   if Skin^.UniformBufferObjectHandle>0 then begin
    glDeleteBuffers(1,@Skin^.UniformBufferObjectHandle);
   end;
  end;
 end;
begin
 if fUploaded then begin
  fUploaded:=false;
  DeleteOpenGLObjects;
  UnloadTextures;
  DeleteSkinUniformBufferObjects;
 end;
end;

procedure TGLTFOpenGL.Draw(const aModelMatrix:TPasGLTF.TMatrix4x4;
                           const aViewMatrix:TPasGLTF.TMatrix4x4;
                           const aProjectionMatrix:TPasGLTF.TMatrix4x4;
                           const aNonSkinnedNormalPBRShader:TPBRShader;
                           const aNonSkinnedAlphaTestPBRShader:TPBRShader;
                           const aSkinnedNormalPBRShader:TPBRShader;
                           const aSkinnedAlphaTestPBRShader:TPBRShader;
                           const aAnimationIndex:TPasGLTFSizeInt=0;
                           const aTime:TPasGLTFFloat=0.0;
                           const aScene:TPasGLTFSizeInt=-1);
var NonSkinnedPBRShader,SkinnedPBRShader:TPBRShader;
    CurrentShader:TShader;
 procedure UseShader(const aShader:TShader);
 begin
  if CurrentShader<>aShader then begin
   CurrentShader:=aShader;
   if assigned(CurrentShader) then begin
    CurrentShader.Bind;
   end;
  end;
 end;
 procedure ResetNode(const aNodeIndex:TPasGLTFSizeInt);
 var Index:TPasGLTFSizeInt;
     Node:TPasGLTF.TNode;
 begin
  Node:=fDocument.Nodes[aNodeIndex];
  fNodes[aNodeIndex].OverwriteFlags:=[];
  for Index:=0 to Node.Children.Count-1 do begin
   ResetNode(Node.Children.Items[Index]);
  end;
 end;
 procedure ProcessAnimation(const aAnimationIndex:TPasGLTFSizeInt);
  function CubicSplineInterpolate(const t,y0,y1,y2,y3:TPasGLTFFloat):TPasGLTFFloat;
  var t2,n:TPasGLTFFloat;
  begin
   t2:=t*t;
   n:=((y3-y2)-y0)+y1;
   result:=(n*t*t2)+(((y0-y1)-n)*t2)+((y2-y0)*t)+y1;
  end;
 var ChannelIndex,InputTimeArrayIndex:TPasGLTFSizeInt;
     Animation:PAnimation;
     AnimationChannel:TAnimation.PChannel;
     Time,Factor,Scalar:TPasGLTFFloat;
     Vector3:TPasGLTF.TVector3;
     Vector4:TPasGLTF.TVector4;
     Vector3s:array[-1..2] of TPasGLTF.PVector3;
     Vector4s:array[-1..2] of TPasGLTF.PVector4;
     TimeIndices:array[-1..2] of TPasGLTFSizeInt;
 begin

  Animation:=@fAnimations[aAnimationIndex];

  for ChannelIndex:=0 to length(Animation^.Channels)-1 do begin

   AnimationChannel:=@Animation^.Channels[ChannelIndex];

   if (AnimationChannel^.Node>=0) and (length(AnimationChannel^.InputTimeArray)>0) then begin

    TimeIndices[1]:=length(AnimationChannel^.InputTimeArray)-1;

    Time:=AnimationChannel^.InputTimeArray[TimeIndices[1]];
    Time:=aTime-(floor(aTime/Time)*Time);

    for InputTimeArrayIndex:=1 to length(AnimationChannel^.InputTimeArray)-1 do begin
     if AnimationChannel^.InputTimeArray[InputTimeArrayIndex]>Time then begin
      TimeIndices[1]:=InputTimeArrayIndex;
      break;
     end;
    end;

    if TimeIndices[1]>=0 then begin

     TimeIndices[0]:=Max(0,TimeIndices[1]-1);
     TimeIndices[-1]:=Max(0,TimeIndices[0]-1);
     TimeIndices[2]:=Min(Max(TimeIndices[1]+1,0),length(AnimationChannel^.InputTimeArray)-1);

     if SameValue(TimeIndices[0],TimeIndices[1]) then begin
      Factor:=0.0;
     end else begin
      Factor:=(Time-AnimationChannel^.InputTimeArray[TimeIndices[0]])/(AnimationChannel^.InputTimeArray[TimeIndices[1]]-AnimationChannel^.InputTimeArray[TimeIndices[0]]);
      if Factor<0.0 then begin
       Factor:=0.0;
      end else if Factor>1.0 then begin
       Factor:=1.0;
      end;
     end;

     case AnimationChannel^.Target of
      TAnimation.TChannel.TTarget.Translation,
      TAnimation.TChannel.TTarget.Scale:begin
       case AnimationChannel^.Interpolation of
        TAnimation.TChannel.TInterpolation.Linear:begin
         Vector3s[0]:=@AnimationChannel^.OutputVector3Array[TimeIndices[0]];
         Vector3s[1]:=@AnimationChannel^.OutputVector3Array[TimeIndices[1]];
         Vector3[0]:=(Vector3s[0]^[0]*(1.0-Factor))+(Vector3s[1]^[0]*Factor);
         Vector3[1]:=(Vector3s[0]^[1]*(1.0-Factor))+(Vector3s[1]^[1]*Factor);
         Vector3[2]:=(Vector3s[0]^[2]*(1.0-Factor))+(Vector3s[1]^[2]*Factor);
        end;
        TAnimation.TChannel.TInterpolation.Step:begin
         Vector3:=AnimationChannel^.OutputVector3Array[TimeIndices[0]];
        end;
        TAnimation.TChannel.TInterpolation.CubicSpline:begin
         Vector3s[-1]:=@AnimationChannel^.OutputVector3Array[TimeIndices[-1]];
         Vector3s[0]:=@AnimationChannel^.OutputVector3Array[TimeIndices[0]];
         Vector3s[1]:=@AnimationChannel^.OutputVector3Array[TimeIndices[1]];
         Vector3s[2]:=@AnimationChannel^.OutputVector3Array[TimeIndices[2]];
         Vector3[0]:=CubicSplineInterpolate(Factor,Vector3s[-1]^[0],Vector3s[0]^[0],Vector3s[1]^[0],Vector3s[2]^[0]);
         Vector3[1]:=CubicSplineInterpolate(Factor,Vector3s[-1]^[1],Vector3s[0]^[1],Vector3s[1]^[1],Vector3s[2]^[1]);
         Vector3[2]:=CubicSplineInterpolate(Factor,Vector3s[-1]^[2],Vector3s[0]^[2],Vector3s[1]^[2],Vector3s[2]^[2]);
        end;
        else begin
         Assert(false);
        end;
       end;
       case AnimationChannel^.Target of
        TAnimation.TChannel.TTarget.Translation:begin
         Include(fNodes[AnimationChannel^.Node].OverwriteFlags,TNode.TOverwriteFlag.Translation);
         fNodes[AnimationChannel^.Node].Translation:=Vector3;
        end;
        TAnimation.TChannel.TTarget.Scale:begin
         Include(fNodes[AnimationChannel^.Node].OverwriteFlags,TNode.TOverwriteFlag.Scale);
         fNodes[AnimationChannel^.Node].Scale:=Vector3;
        end;
       end;
      end;
      TAnimation.TChannel.TTarget.Rotation:begin
       case AnimationChannel^.Interpolation of
        TAnimation.TChannel.TInterpolation.Linear:begin
         Vector4:=QuaternionSlerp(AnimationChannel^.OutputVector4Array[TimeIndices[0]],
                                  AnimationChannel^.OutputVector4Array[TimeIndices[1]],
                                  Factor);
        end;
        TAnimation.TChannel.TInterpolation.Step:begin
         Vector4:=AnimationChannel^.OutputVector4Array[TimeIndices[0]];
        end;
        TAnimation.TChannel.TInterpolation.CubicSpline:begin
         // Kochanek–Bartels spline with cubic-spline-mode constant parameter values
         Vector4:=QuaternionKochanekBartelsSplineInterpolate(Factor,
                                                             -1.0,
                                                             0,
                                                             1.0,
                                                             2.0,
                                                             AnimationChannel^.OutputVector4Array[TimeIndices[-1]],
                                                             AnimationChannel^.OutputVector4Array[TimeIndices[0]],
                                                             AnimationChannel^.OutputVector4Array[TimeIndices[1]],
                                                             AnimationChannel^.OutputVector4Array[TimeIndices[2]],
                                                             0.0,0.0,0.0,
                                                             0.0,0.0,0.0);
        end;
        else begin
         Assert(false);
        end;
       end;
       Include(fNodes[AnimationChannel^.Node].OverwriteFlags,TNode.TOverwriteFlag.Rotation);
       fNodes[AnimationChannel^.Node].Rotation:=Vector4;
      end;
      TAnimation.TChannel.TTarget.Weights:begin

      end;
     end;


    end;

   end;

  end;

 end;
 procedure ProcessNode(const aNodeIndex:TPasGLTFSizeInt;const aMatrix:TMatrix);
 var Index:TPasGLTFSizeInt;
     Matrix:TPasGLTF.TMatrix4x4;
     Node:TPasGLTF.TNode;
     ExtraNode:PNode;
     Translation,Scale:TVector3;
     Rotation:TVector4;
 begin
  Node:=fDocument.Nodes[aNodeIndex];
  ExtraNode:=@fNodes[aNodeIndex];
  if TNode.TOverwriteFlag.Translation in ExtraNode^.OverwriteFlags then begin
   Translation:=ExtraNode^.Translation;
  end else begin
   Translation:=Node.Translation;
  end;
  if TNode.TOverwriteFlag.Scale in ExtraNode^.OverwriteFlags then begin
   Scale:=ExtraNode^.Scale;
  end else begin
   Scale:=Node.Scale;
  end;
  if TNode.TOverwriteFlag.Rotation in ExtraNode^.OverwriteFlags then begin
   Rotation:=ExtraNode^.Rotation;
  end else begin
   Rotation:=Node.Rotation;
  end;
  Matrix:=MatrixMul(
           MatrixMul(
            MatrixMul(
             MatrixFromScale(Scale),
             MatrixMul(
              MatrixFromRotation(Rotation),
              MatrixFromTranslation(Translation))),
            Node.Matrix),
           aMatrix);
  fNodes[aNodeIndex].Matrix:=Matrix;
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessNode(Node.Children.Items[Index],Matrix);
  end;
 end;
 procedure DrawNode(const aNodeIndex:TPasGLTFSizeInt;const aAlphaMode:TPasGLTF.TMaterial.TAlphaMode);
 var PBRShader:TPBRShader;
  procedure DrawMesh(const aMesh:TMesh);
  var PrimitiveIndex:TPasGLTFSizeInt;
      Primitive:TMesh.PPrimitive;
      Material:TPasGLTF.TMaterial;
      ExtraMaterial:PMaterial;
      Flags:TPasGLTFUInt32;
  begin
   for PrimitiveIndex:=0 to length(aMesh.Primitives)-1 do begin
    Primitive:=@aMesh.Primitives[PrimitiveIndex];
    if (Primitive^.Material>=0) and (Primitive^.Material<fDocument.Materials.Count) then begin
     Flags:=0;
     Material:=fDocument.Materials[Primitive^.Material];
     ExtraMaterial:=@fMaterials[Primitive^.Material];
     if Material.AlphaMode=aAlphaMode then begin
      if Material.AlphaMode=TPasGLTF.TMaterial.TAlphaMode.Mask then begin
       glUniform1f(PBRShader.uAlphaCutOff,Material.AlphaCutOff);
      end;
      if Material.DoubleSided then begin
       Flags:=Flags or $20000000;
       glDisable(GL_CULL_FACE);
      end else begin
       glEnable(GL_CULL_FACE);
       glCullFace(GL_BACK);
      end;
      if ExtraMaterial^.PBRSpecularGlossiness.Used then begin
       Flags:=Flags or $40000000;
       if (ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index>=0) and (ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index<length(fTextures)) then begin
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D,fTextures[ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index].Handle);
        Flags:=Flags or (1 or (2*ord(ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.TexCoord=1)));
       end;
       if (ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index>=0) and (ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index<length(fTextures)) then begin
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D,fTextures[ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index].Handle);
        Flags:=Flags or (4 or (8*ord(ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord=1)));
       end;
      end else begin
       if (Material.PBRMetallicRoughness.BaseColorTexture.Index>=0) and (Material.PBRMetallicRoughness.BaseColorTexture.Index<length(fTextures)) then begin
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D,fTextures[Material.PBRMetallicRoughness.BaseColorTexture.Index].Handle);
        Flags:=Flags or (1 or (2*ord(Material.PBRMetallicRoughness.BaseColorTexture.TexCoord=1)));
       end;
       if (Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index>=0) and (Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index<length(fTextures)) then begin
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D,fTextures[Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index].Handle);
        Flags:=Flags or (4 or (8*ord(Material.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord=1)));
       end;
      end;
      if (Material.NormalTexture.Index>=0) and (Material.NormalTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE2);
       glBindTexture(GL_TEXTURE_2D,fTextures[Material.NormalTexture.Index].Handle);
       Flags:=Flags or (16 or (32*ord(Material.NormalTexture.TexCoord=1)));
      end;
      if (Material.OcclusionTexture.Index>=0) and (Material.OcclusionTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE3);
       glBindTexture(GL_TEXTURE_2D,fTextures[Material.OcclusionTexture.Index].Handle);
       Flags:=Flags or (64 or (128*ord(Material.OcclusionTexture.TexCoord=1)));
      end;
      if (Material.EmissiveTexture.Index>=0) and (Material.EmissiveTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE4);
       glBindTexture(GL_TEXTURE_2D,fTextures[Material.EmissiveTexture.Index].Handle);
       Flags:=Flags or (256 or (512*ord(Material.EmissiveTexture.TexCoord=1)));
      end;
      glUniform1ui(PBRShader.uFlags,Flags);
      if ExtraMaterial^.PBRSpecularGlossiness.Used then begin
       glUniform4f(PBRShader.uBaseColorFactor,
                   ExtraMaterial^.PBRSpecularGlossiness.DiffuseFactor[0],
                   ExtraMaterial^.PBRSpecularGlossiness.DiffuseFactor[1],
                   ExtraMaterial^.PBRSpecularGlossiness.DiffuseFactor[2],
                   ExtraMaterial^.PBRSpecularGlossiness.DiffuseFactor[3]);
       glUniform4f(PBRShader.uMetallicRoughnessNormalScaleOcclusionStrengthFactor,
                   1.0,
                   ExtraMaterial^.PBRSpecularGlossiness.GlossinessFactor,
                   Material.NormalTexture.Scale,
                   Material.OcclusionTexture.Strength);
       glUniform3f(PBRShader.uSpecularFactor,
                   ExtraMaterial^.PBRSpecularGlossiness.SpecularFactor[0],
                   ExtraMaterial^.PBRSpecularGlossiness.SpecularFactor[1],
                   ExtraMaterial^.PBRSpecularGlossiness.SpecularFactor[2]);
      end else begin
       glUniform4f(PBRShader.uBaseColorFactor,
                   Material.PBRMetallicRoughness.BaseColorFactor[0],
                   Material.PBRMetallicRoughness.BaseColorFactor[1],
                   Material.PBRMetallicRoughness.BaseColorFactor[2],
                   Material.PBRMetallicRoughness.BaseColorFactor[3]);
       glUniform4f(PBRShader.uMetallicRoughnessNormalScaleOcclusionStrengthFactor,
                   Material.PBRMetallicRoughness.MetallicFactor,
                   Material.PBRMetallicRoughness.RoughnessFactor,
                   Material.NormalTexture.Scale,
                   Material.OcclusionTexture.Strength);
      end;
      glUniform3f(PBRShader.uEmissiveFactor,
                  Material.EmissiveFactor[0],
                  Material.EmissiveFactor[1],
                  Material.EmissiveFactor[2]);
      glDrawElements(Primitive^.PrimitiveMode,Primitive^.CountIndices,GL_UNSIGNED_INT,@PPasGLTFUInt32Array(nil)^[Primitive^.StartBufferIndexOffset]);
     end;
    end;
   end;
  end;
 var Index,JointIndex:TPasGLTFSizeInt;
     Matrix,ModelMatrix,ModelViewMatrix,ModelViewProjectionMatrix,
     JointMatrix,InverseMatrix:TPasGLTF.TMatrix4x4;
     Node:TPasGLTF.TNode;
     Skin:PSkin;
     p:pointer;
     pm:TPasGLTF.PMatrix4x4;
 begin
  Node:=fDocument.Nodes[aNodeIndex];
  Matrix:=fNodes[aNodeIndex].Matrix;
  if (Node.Mesh>=0) and (Node.Mesh<length(fMeshes)) then begin
   ModelMatrix:=MatrixMul(Matrix,aModelMatrix);
   ModelViewMatrix:=MatrixMul(ModelMatrix,aViewMatrix);
   ModelViewProjectionMatrix:=MatrixMul(ModelViewMatrix,aProjectionMatrix);
   if (Node.Skin>=0) and (Node.Skin<length(fSkins)) then begin
    Skin:=@fSkins[Node.Skin];
    PBRShader:=SkinnedPBRShader;
    UseShader(PBRShader);
    InverseMatrix:=MatrixInverse(Matrix);
    glBindBuffer(GL_UNIFORM_BUFFER,Skin^.UniformBufferObjectHandle);
    p:=glMapBufferRange(GL_UNIFORM_BUFFER,0,length(Skin^.Matrices)*SizeOf(TPasGLTF.TMatrix4x4),GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_BUFFER_BIT);
    if assigned(p) then begin
     pm:=p;
     for JointIndex:=0 to length(Skin^.Joints)-1 do begin
      pm^:=MatrixMul(
            InverseMatrix,
            MatrixMul(
             Skin^.InverseBindMatrices[JointIndex],
             fNodes[Skin^.Joints[JointIndex]].Matrix));
      inc(pm);
     end;
     glUnmapBuffer(GL_UNIFORM_BUFFER);
    end;
    glBindBuffer(GL_UNIFORM_BUFFER,0);
    glBindBufferBase(GL_UNIFORM_BUFFER,PBRShader.uJointMatrices,Skin^.UniformBufferObjectHandle);
   end else begin
    PBRShader:=NonSkinnedPBRShader;
    UseShader(PBRShader);
   end;
   glUniformMatrix4fv(PBRShader.uModelMatrix,1,false,@ModelMatrix);
   glUniformMatrix4fv(PBRShader.uModelViewMatrix,1,false,@ModelViewMatrix);
   glUniformMatrix4fv(PBRShader.uModelViewProjectionMatrix,1,false,@ModelViewProjectionMatrix);
   DrawMesh(fMeshes[Node.Mesh]);
  end;
  for Index:=0 to Node.Children.Count-1 do begin
   DrawNode(Node.Children.Items[Index],aAlphaMode);
  end;
 end;
var Index:TPasGLTFSizeInt;
    Scene:TPasGLTF.TScene;
    AlphaMode:TPasGLTF.TMaterial.TAlphaMode;
begin
 if aScene<0 then begin
  Scene:=fDocument.Scenes[fDocument.Scene];
 end else begin
  Scene:=fDocument.Scenes[aScene];
 end;
 glBindVertexArray(fVertexArrayHandle);
 glBindBuffer(GL_ARRAY_BUFFER,fVertexBufferObjectHandle);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,fIndexBufferObjectHandle);
 for Index:=0 to Scene.Nodes.Count-1 do begin
  ResetNode(Scene.Nodes.Items[Index]);
 end;
 if (aAnimationIndex>=0) and (aAnimationIndex<fDocument.Animations.Count) then begin
  ProcessAnimation(aAnimationIndex);
 end;
 for Index:=0 to Scene.Nodes.Count-1 do begin
  ProcessNode(Scene.Nodes.Items[Index],TPasGLTF.TDefaults.IdentityMatrix4x4);
 end;
 CurrentShader:=nil;
 for AlphaMode:=TPasGLTF.TMaterial.TAlphaMode.Opaque to TPasGLTF.TMaterial.TAlphaMode.Blend do begin
  case AlphaMode of
   TPasGLTF.TMaterial.TAlphaMode.Opaque:begin
    NonSkinnedPBRShader:=aNonSkinnedNormalPBRShader;
    SkinnedPBRShader:=aSkinnedNormalPBRShader;
    glDisable(GL_BLEND);
   end;
   TPasGLTF.TMaterial.TAlphaMode.Mask:begin
    NonSkinnedPBRShader:=aNonSkinnedAlphaTestPBRShader;
    SkinnedPBRShader:=aSkinnedAlphaTestPBRShader;
    glDisable(GL_BLEND);
   end;
   TPasGLTF.TMaterial.TAlphaMode.Blend:begin
    NonSkinnedPBRShader:=aNonSkinnedNormalPBRShader;
    SkinnedPBRShader:=aSkinnedNormalPBRShader;
    glEnable(GL_BLEND);
    if assigned(glBlendFuncSeparate) then begin
     glBlendFuncSeparate(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA,GL_ONE,GL_ONE_MINUS_SRC_ALPHA);
    end else begin
     glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    end;
   end;
   else begin
    NonSkinnedPBRShader:=nil;
    Assert(false);
   end;
  end;
  if assigned(NonSkinnedPBRShader) then begin
   UseShader(NonSkinnedPBRShader);
   glUniform1i(NonSkinnedPBRShader.uBaseColorTexture,0);
   glUniform1i(NonSkinnedPBRShader.uMetallicRoughnessTexture,1);
   glUniform1i(NonSkinnedPBRShader.uNormalTexture,2);
   glUniform1i(NonSkinnedPBRShader.uOcclusionTexture,3);
   glUniform1i(NonSkinnedPBRShader.uEmissiveTexture,4);
   UseShader(SkinnedPBRShader);
   glUniform1i(SkinnedPBRShader.uBaseColorTexture,0);
   glUniform1i(SkinnedPBRShader.uMetallicRoughnessTexture,1);
   glUniform1i(SkinnedPBRShader.uNormalTexture,2);
   glUniform1i(SkinnedPBRShader.uOcclusionTexture,3);
   glUniform1i(SkinnedPBRShader.uEmissiveTexture,4);
   for Index:=0 to Scene.Nodes.Count-1 do begin
    DrawNode(Scene.Nodes.Items[Index],AlphaMode);
   end;
  end;
 end;
 glUseProgram(0);
 glBindVertexArray(0);
 glBindBuffer(GL_ARRAY_BUFFER,0);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,0);
 glActiveTexture(GL_TEXTURE0);
end;

end.
