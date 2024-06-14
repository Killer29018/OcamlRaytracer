open Vec3

module HitRecord = struct
    type hitRecord = {
        mutable t: float;
        mutable normal: Vec3.vec3;
        mutable pos: Vec3.vec3;
        mutable frontFace : bool;
    }

    type hit =
        | Miss
        | Hit of hitRecord

    let newHitRecord t norm pos =
        Hit ({ t = t; normal = norm; pos = pos; frontFace = true })

    let nullHitRecord =
        fun () -> ({ t = 0.; normal = Vec3.vec3Zero; pos = Vec3.vec3Zero; frontFace = true})
end
