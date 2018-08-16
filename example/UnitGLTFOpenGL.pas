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

interface

uses SysUtils,Classes,Math,PasGLTF,dglOpenGL;

type EGLTFOpenGL=class(Exception);

     TGLTFOpenGL=class
      private
       type TVertexAttributeBindingLocations=class
             public
              const Position=0;
                    Normal=1;
                    Tangent=2;
                    TexCoord0=2;
                    TexCoord1=3;
                    Color0=4;
                    Joints0=5;
                    Joints1=6;
                    Weights0=7;
                    Weights1=8;
            end;
            TAccessor=record
             public
              type TWorkBuffer=record
                    Active:boolean;
                    Handle:glUInt;
                    Data:TBytes;
                   end;
                   PWorkBuffer=^TWorkBuffer;
             public
              BufferView:TPasGLTFSizeInt;
              WorkBuffer:TWorkBuffer;
            end;
            PAccessor=^TAccessor;
            TAccessors=array of TAccessor;
            TBufferView=record
             Handle:glUInt;
             BufferView:TPasGLTF.TBufferView;
             Buffer:TPasGLTF.TBuffer;
            end;
            PBufferView=^TBufferView;
            TBufferViews=array of TBufferView;
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
                   end;
                   PPrimitive=^TPrimitive;
                   TPrimitives=array of TPrimitive;
             public
              Primitives:TPrimitives;
            end;
            PMesh=^TMesh;
            TMeshes=array of TMesh;
      private
       fDocument:TPasGLTF.TDocument;
       fReady:boolean;
       fUploaded:boolean;
       fAccessors:TAccessors;
       fBufferViews:TBufferViews;
       fMeshes:TMeshes;
       fVertexBufferObjectHandle:glInt;
       fIndexBufferObjectHandle:glInt;
       fVertexArrayHandle:glInt;
      public
       constructor Create(const aDocument:TPasGLTF.TDocument); reintroduce;
       destructor Destroy; override;
       procedure InitializeResources;
       procedure FinalizeResources;
       procedure UploadResources;
       procedure UnloadResources;
       procedure Draw(const aScene:TPasGLTFSizeInt=-1);
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

{ TGLTFModel }

constructor TGLTFOpenGL.Create(const aDocument:TPasGLTF.TDocument);
begin
 inherited Create;
 fDocument:=aDocument;
 fReady:=false;
 fUploaded:=false;
 fAccessors:=nil;
 fBufferViews:=nil;
 fMeshes:=nil;
end;

destructor TGLTFOpenGL.Destroy;
begin
 UnloadResources;
 FinalizeResources;
 inherited Destroy;
end;

procedure TGLTFOpenGL.InitializeResources;
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

   SetLength(DestinationMesh.Primitives,SourceMesh.Primitives.Count);

   for PrimitiveIndex:=0 to SourceMesh.Primitives.Count-1 do begin

    SourceMeshPrimitive:=SourceMesh.Primitives.Items[PrimitiveIndex];

    DestinationMeshPrimitive:=@DestinationMesh.Primitives[PrimitiveIndex];

    begin
     // Load accessor data
     begin
      AccessorIndex:=SourceMeshPrimitive.Attributes['POSITION'];
      if AccessorIndex>=0 then begin
       TemporaryPositions:=fDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
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
begin
 if not fReady then begin
  InitializeMeshes;
  fReady:=true;
 end;
end;

procedure TGLTFOpenGL.FinalizeResources;
 procedure FinalizeMeshes;
 var Index,
     PrimitiveIndex:TPasGLTFSizeInt;
     Mesh:PMesh;
 begin
  for Index:=0 to length(fMeshes)-1 do begin
   Mesh:=@fMeshes[Index];
   for PrimitiveIndex:=0 to length(Mesh^.Primitives) do begin

   end;
  end;
 end;
begin
 if fReady then begin
  fReady:=false;
  FinalizeMeshes;
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
    Primitive:=@Mesh.Primitives[PrimitiveIndex];
    Primitive^.StartBufferVertexOffset:=AllVertices.Count;
    Primitive^.StartBufferIndexOffset:=AllIndices.Count;
    Primitive^.CountVertices:=length(Primitive^.Vertices);
    Primitive^.CountIndices:=length(Primitive^.Indices);
    AllVertices.Add(Primitive^.Vertices);
    AllIndices.Add(Primitive^.Indices);
    for IndexIndex:=Primitive^.StartBufferIndexOffset to (Primitive^.StartBufferIndexOffset+Primitive^.CountIndices)-1 do begin
     Primitive^.Indices[IndexIndex]:=Primitive^.Indices[IndexIndex]+Primitive^.StartBufferVertexOffset;
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
begin
 if not fUploaded then begin
  fUploaded:=true;
  AllVertices:=TAllVertices.Create;
  try
   AllIndices:=TAllIndices.Create;
   try
    CollectVerticesAndIndicesFromMeshes;
    CreateOpenGLObjects;
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
begin
 if fUploaded then begin
  fUploaded:=false;
  DeleteOpenGLObjects;
 end;
end;

procedure TGLTFOpenGL.Draw(const aScene:TPasGLTFSizeInt=-1);
 procedure DrawNode(const aNode:TPasGLTF.TNode;const aMatrix:TMatrix);
 var Index:TPasGLTFSizeInt;
     Matrix:TMatrix;
 begin
  Matrix:=MatrixMul(
           aMatrix,
           MatrixMul(
            aNode.Matrix,
            MatrixMul(
             MatrixFromScale(aNode.Scale),
             MatrixMul(
              MatrixFromRotation(aNode.Rotation),
              MatrixFromTranslation(aNode.Translation)))));
  for Index:=0 to aNode.Children.Count-1 do begin
   DrawNode(fDocument.Nodes[aNode.Children.Items[Index]],Matrix);
  end;
 end;
var Index:TPasGLTFSizeInt;
    Scene:TPasGLTF.TScene;
begin
 if aScene<0 then begin
  Scene:=fDocument.Scenes[fDocument.Scene];
 end else begin
  Scene:=fDocument.Scenes[aScene];
 end;
 for Index:=0 to Scene.Nodes.Count-1 do begin
  DrawNode(fDocument.Nodes[Scene.Nodes.Items[Index]],TPasGLTF.TDefaults.IdentityMatrix);
 end;
end;

end.
