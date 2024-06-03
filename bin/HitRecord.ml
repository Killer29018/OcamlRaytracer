open Vec3

type hitRecord = {
    t: float;
    normal: vec3;
    pos: vec3
}

type hit =
    | Miss
    | Hit of hitRecord

let newHitRecord t norm pos =
    Hit ({ t = t; normal = norm; pos = pos })
