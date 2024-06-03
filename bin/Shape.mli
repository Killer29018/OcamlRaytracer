open Vec3
open Ray
open HitRecord

type sphereSize = float

exception ShapeError

type shape =
    | None
    | Sphere of vec3 * sphereSize

val sphereNormal : vec3 -> shape -> vec3
val sphereCollision : ray -> shape -> hit
