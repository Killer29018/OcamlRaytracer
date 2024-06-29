open Vec3

module Pixels : sig
    type pixel = {
        x: int;
        y: int;
        z: int
    }

    val pixel_of_vec3 : Vec3.vec3 -> pixel

    val string_of_pixel : pixel -> string
end
