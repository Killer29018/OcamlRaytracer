open Vec3
open Vec2

open Shape
open Material

module Obj : sig
    type vertex = {
        pos: Vec3.vec3;
        normal: Vec3.vec3;
        uv: Vec2.vec2;
    }

    type face = {
        vertices: vertex list;
        minBound: Vec3.vec3;
        maxBound: Vec3.vec3;
    }

    val getFaceFromObj : string -> face list

    val getTriangles : string -> Vec3.vec3 -> Material.materialType -> Shape.shape list

    exception LineLengthErr
    exception VertexParsingErr
    exception FaceParsingErr

    exception VertexCountErr

end
