open Vec3

module Perlin : sig
    type perlin_T = {
        mutable perm_x : int array;
        mutable perm_y : int array;
        mutable perm_z : int array;
        mutable rand_float : float array;
    }

    val create_null : (unit -> perlin_T)

    val create : (unit -> perlin_T)

    val noise : perlin_T -> Vec3.vec3 -> float
end
