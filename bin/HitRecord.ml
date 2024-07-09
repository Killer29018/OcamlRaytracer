open Vec3
module HitRecord = struct
    type hit_record = {
        mutable t: float;
        mutable pos: Vec3.vec3;
        mutable normal: Vec3.vec3;
        mutable is_front_face: bool;
    }

    type hit = Miss
             | Hit of hit_record

    let create_null =
        fun () -> {
            t = 0.;
            pos = Vec3.zero;
            normal = Vec3.zero;
            is_front_face = true;
        }
end
