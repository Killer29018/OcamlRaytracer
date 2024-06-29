open Vec3
open Pixels

module Image : sig
    type 'a image = {
        width: int;
        height: int;
        image: 'a array array
    }

    val image_map : ('a -> 'b) -> 'a image -> 'b image

    val image_iter : ('a -> unit) -> 'a image -> unit

    val generate_vec3_image : int -> int -> Vec3.vec3 image

    val pixel_image_of_vec3_image : Vec3.vec3 image -> Pixels.pixel image

    val print_vec3_image : Vec3.vec3 image -> unit
    val print_pixel_image : Pixels.pixel image -> unit

    val ppm_of_pixel_image : Pixels.pixel image -> unit
end
