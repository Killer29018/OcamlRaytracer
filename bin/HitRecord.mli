open Vec3
open Vec2

module HitRecord : sig
    type hit_record = {
        mutable t: float;
        mutable pos: Vec3.vec3;
        mutable normal: Vec3.vec3;
        mutable is_front_face: bool;
        mutable uv : Vec2.vec2;
        mutable id: int;
    }

    type hit = Miss
             | Hit of hit_record

    val create_null_hit_record : (unit -> hit_record)

    val create_hit_record_tp : float -> Vec3.vec3 -> hit_record
    val create_hit_record_tpn : float -> Vec3.vec3 -> Vec3.vec3 -> hit_record
    val create_hit_record_tpnf : float -> Vec3.vec3 -> Vec3.vec3 -> bool -> hit_record

    val closest_hit : hit -> hit -> hit

    val string_of_hit_record : hit_record -> string
    val string_of_hit : hit -> string
end
