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

uses SysUtils,Classes,Math,PasJSON,PasGLTF,dglOpenGL,UnitOpenGLImage,UnitOpenGLPBRShader;

type EGLTFOpenGL=class(Exception);

     TGLTFOpenGL=class
      private
       type TVertexAttributeBindingLocations=class
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
            end;
            PMesh=^TMesh;
            TMeshes=array of TMesh;
            TTexture=record
             Handle:glUInt;
            end;
            PTexture=^TTexture;
            TTextures=array of TTexture;
      private
       fDocument:TPasGLTF.TDocument;
       fReady:boolean;
       fUploaded:boolean;
       fMaterials:TMaterials;
       fMeshes:TMeshes;
       fTextures:TTextures;
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
       procedure Draw(const aModelMatrix,aViewMatrix,aProjectionMatrix:TPasGLTF.TMatrix4x4;const aPBRShader:TPBRShader;const aScene:TPasGLTFSizeInt=-1);
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
 fMaterials:=nil;
 fMeshes:=nil;
end;

destructor TGLTFOpenGL.Destroy;
begin
 UnloadResources;
 FinalizeResources;
 inherited Destroy;
end;

procedure TGLTFOpenGL.InitializeResources;
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
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[1]);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[2]);
      DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[3]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.DiffuseFactor[3]);
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
      DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[1]);
      DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],DestinationMaterial.PBRSpecularGlossiness.SpecularFactor[2]);
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

   SetLength(DestinationMesh.Primitives,SourceMesh.Primitives.Count);

   for PrimitiveIndex:=0 to SourceMesh.Primitives.Count-1 do begin

    SourceMeshPrimitive:=SourceMesh.Primitives.Items[PrimitiveIndex];

    DestinationMeshPrimitive:=@DestinationMesh.Primitives[PrimitiveIndex];

    DestinationMeshPrimitive^.Material:=SourceMeshPrimitive.Material;

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
 procedure InitializeTextures;
 begin
  SetLength(fTextures,fDocument.Textures.Count);
 end;
begin
 if not fReady then begin
  InitializeMaterials;
  InitializeMeshes;
  InitializeTextures;
  fReady:=true;
 end;
end;

procedure TGLTFOpenGL.FinalizeResources;
begin
 if fReady then begin
  fReady:=false;
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
{      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_BASE_LEVEL,0);
       glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAX_LEVEL,0);}
       glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,ImageWidth,ImageHeight,0,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
       glGenerateMipmap(GL_TEXTURE_2D);
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
begin
 if fUploaded then begin
  fUploaded:=false;
  DeleteOpenGLObjects;
  UnloadTextures;
 end;
end;

procedure TGLTFOpenGL.Draw(const aModelMatrix,aViewMatrix,aProjectionMatrix:TPasGLTF.TMatrix4x4;const aPBRShader:TPBRShader;const aScene:TPasGLTFSizeInt=-1);
 procedure DrawNode(const aNode:TPasGLTF.TNode;const aMatrix:TMatrix);
  procedure DrawMesh(const aMesh:TMesh);
  var PrimitiveIndex:TPasGLTFSizeInt;
      Primitive:TMesh.PPrimitive;
      Material:TPasGLTF.TMaterial;
      ExtraMaterial:PMaterial;
      TextureFlags:TPasGLTFUInt32;
  begin
   for PrimitiveIndex:=0 to length(aMesh.Primitives)-1 do begin
    Primitive:=@aMesh.Primitives[PrimitiveIndex];
    if (Primitive^.Material>=0) and (Primitive^.Material<fDocument.Materials.Count) then begin
     Material:=fDocument.Materials[Primitive^.Material];
     ExtraMaterial:=@fMaterials[Primitive^.Material];
     case Material.AlphaMode of
      TPasGLTF.TMaterial.TAlphaMode.Opaque:begin
       glDisable(GL_BLEND);
      end;
      TPasGLTF.TMaterial.TAlphaMode.Mask:begin
       glDisable(GL_BLEND);
       glEnable(GL_ALPHA_TEST);
       glAlphaFunc(GL_GREATER,Material.AlphaCutOff);
      end;
      TPasGLTF.TMaterial.TAlphaMode.Blend:begin
       glEnable(GL_BLEND);
       glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      end;
     end;
     if Material.DoubleSided then begin
      glDisable(GL_CULL_FACE);
     end else begin
      glEnable(GL_CULL_FACE);
      glCullFace(GL_BACK);
     end;
     TextureFlags:=0;
     if ExtraMaterial^.PBRSpecularGlossiness.Used then begin
      TextureFlags:=TextureFlags or $40000000;
      if (ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index>=0) and (ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE0);
       glBindTexture(GL_TEXTURE_2D,fTextures[ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.Index].Handle);
       TextureFlags:=TextureFlags or (1 or (2*ord(ExtraMaterial^.PBRSpecularGlossiness.DiffuseTexture.TexCoord=1)));
      end;
      if (ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index>=0) and (ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE1);
       glBindTexture(GL_TEXTURE_2D,fTextures[ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.Index].Handle);
       TextureFlags:=TextureFlags or (4 or (8*ord(ExtraMaterial^.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord=1)));
      end;
     end else begin
      if (Material.PBRMetallicRoughness.BaseColorTexture.Index>=0) and (Material.PBRMetallicRoughness.BaseColorTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE0);
       glBindTexture(GL_TEXTURE_2D,fTextures[Material.PBRMetallicRoughness.BaseColorTexture.Index].Handle);
       TextureFlags:=TextureFlags or (1 or (2*ord(Material.PBRMetallicRoughness.BaseColorTexture.TexCoord=1)));
      end;
      if (Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index>=0) and (Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index<length(fTextures)) then begin
       glActiveTexture(GL_TEXTURE1);
       glBindTexture(GL_TEXTURE_2D,fTextures[Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index].Handle);
       TextureFlags:=TextureFlags or (4 or (8*ord(Material.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord=1)));
      end;
     end;
     if (Material.NormalTexture.Index>=0) and (Material.NormalTexture.Index<length(fTextures)) then begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D,fTextures[Material.NormalTexture.Index].Handle);
      TextureFlags:=TextureFlags or (16 or (32*ord(Material.NormalTexture.TexCoord=1)));
     end;
     if (Material.OcclusionTexture.Index>=0) and (Material.OcclusionTexture.Index<length(fTextures)) then begin
      glActiveTexture(GL_TEXTURE3);
      glBindTexture(GL_TEXTURE_2D,fTextures[Material.OcclusionTexture.Index].Handle);
      TextureFlags:=TextureFlags or (64 or (128*ord(Material.OcclusionTexture.TexCoord=1)));
     end;
     if (Material.EmissiveTexture.Index>=0) and (Material.EmissiveTexture.Index<length(fTextures)) then begin
      glActiveTexture(GL_TEXTURE4);
      glBindTexture(GL_TEXTURE_2D,fTextures[Material.EmissiveTexture.Index].Handle);
      TextureFlags:=TextureFlags or (256 or (512*ord(Material.EmissiveTexture.TexCoord=1)));
     end;
     glUniform1ui(aPBRShader.uTextureFlags,TextureFlags);
     glUniform4f(aPBRShader.uBaseColorFactor,
                 Material.PBRMetallicRoughness.BaseColorFactor[0],
                 Material.PBRMetallicRoughness.BaseColorFactor[1],
                 Material.PBRMetallicRoughness.BaseColorFactor[2],
                 Material.PBRMetallicRoughness.BaseColorFactor[3]);
     glUniform4f(aPBRShader.uMetallicRoughnessNormalScaleOcclusionStrengthFactor,
                 Material.PBRMetallicRoughness.MetallicFactor,
                 Material.PBRMetallicRoughness.RoughnessFactor,
                 Material.NormalTexture.Scale,
                 Material.OcclusionTexture.Strength);
     glUniform3f(aPBRShader.uEmissiveFactor,
                 Material.EmissiveFactor[0],
                 Material.EmissiveFactor[1],
                 Material.EmissiveFactor[2]);
     glDrawElements(Primitive^.PrimitiveMode,Primitive^.CountIndices,GL_UNSIGNED_INT,@PPasGLTFUInt32Array(nil)^[Primitive^.StartBufferIndexOffset]);
    end;
   end;
  end;
 var Index:TPasGLTFSizeInt;
     Matrix,ModelMatrix,ModelViewMatrix,ModelViewProjectionMatrix:TPasGLTF.TMatrix4x4;
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
  if (aNode.Mesh>=0) and (aNode.Mesh<length(fMeshes)) then begin
   ModelMatrix:=MatrixMul(Matrix,aModelMatrix);
   ModelViewMatrix:=MatrixMul(ModelMatrix,aViewMatrix);
   ModelViewProjectionMatrix:=MatrixMul(ModelViewMatrix,aProjectionMatrix);
   glUniformMatrix4fv(aPBRShader.uModelMatrix,1,false,@ModelMatrix);
   glUniformMatrix4fv(aPBRShader.uModelViewMatrix,1,false,@ModelViewMatrix);
   glUniformMatrix4fv(aPBRShader.uModelViewProjectionMatrix,1,false,@ModelViewProjectionMatrix);
   DrawMesh(fMeshes[aNode.Mesh]);
  end;
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
 glBindVertexArray(fVertexArrayHandle);
 glBindBuffer(GL_ARRAY_BUFFER,fVertexBufferObjectHandle);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,fIndexBufferObjectHandle);
 glUniform1i(aPBRShader.uBaseColorTexture,0);
 glUniform1i(aPBRShader.uMetallicRoughnessTexture,1);
 glUniform1i(aPBRShader.uNormalTexture,2);
 glUniform1i(aPBRShader.uOcclusionTexture,3);
 glUniform1i(aPBRShader.uEmissiveTexture,4);
 for Index:=0 to Scene.Nodes.Count-1 do begin
  DrawNode(fDocument.Nodes[Scene.Nodes.Items[Index]],TPasGLTF.TDefaults.IdentityMatrix);
 end;
 glBindVertexArray(0);
 glBindBuffer(GL_ARRAY_BUFFER,0);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,0);
 glActiveTexture(GL_TEXTURE0);
end;

end.
