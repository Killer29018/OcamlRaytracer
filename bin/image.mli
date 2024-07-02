open Vec3
open Pixels

module Image : sig
    type 'a image = {
        width: int;
        height: int;
        data: 'a array array
    }

    val map : ('a -> 'b) -> 'a image -> 'b image
    val mapi : ('a -> int -> int -> 'b) -> 'a image -> 'b image

    val iter : ('a -> unit) -> 'a image -> unit

    val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a image -> 'acc

    val generate_vec3_image : int -> int -> Vec3.vec3 image

    val pixel_image_of_vec3_image : Vec3.vec3 image -> Pixels.pixel image

    val string_of_pixel_image : Pixels.pixel image -> string
    val string_of_vec3_image : Vec3.vec3 image -> string

    val ppm_string_of_pixel_image : Pixels.pixel image -> string

    val print_vec3_image : Vec3.vec3 image -> unit
    val print_pixel_image : Pixels.pixel image -> unit

    val print_ppm_pixel_image : Pixels.pixel image -> unit

    val pixel_image_to_file : string -> Pixels.pixel image -> unit
end
