open Vec3

module Ray : sig
    type ray = {
        origin: Vec3.vec3;
        direction: Vec3.vec3;
    }

    val create : Vec3.vec3 -> Vec3.vec3 -> ray

    val calculate_position : ray -> float -> Vec3.vec3

    val string_of_ray : ray -> string
end
