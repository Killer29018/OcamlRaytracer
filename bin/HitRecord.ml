open Vec3

module HitRecord = struct
    type hitRecord = {
        t: float;
        normal: Vec3.vec3;
        pos: Vec3.vec3;
    }

    type hit =
        | Miss
        | Hit of hitRecord

    let newHitRecord t norm pos =
        Hit ({ t = t; normal = norm; pos = pos })
end
