open Vec3

type hitRecord = {
    t: float;
    normal: vec3;
    pos: vec3
}

type hit =
    | Miss
    | Hit of hitRecord

val newHitRecord : float -> vec3 -> vec3 -> hit
