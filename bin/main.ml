open Vec3
open Image

let i = Image.generate_vec3_image 20 20;;

let checkerboard _p x y =
    if (x + y) mod 2 == 0 then
        Vec3.new_vec3 1. 1. 1.
    else
        Vec3.new_vec3 0. 1. 1.

let () =
    let modified = Image.mapi checkerboard i in
    Image.ppm_of_pixel_image (Image.pixel_image_of_vec3_image modified)
