open Vec3
open Image

let () =
    i.image.(0).(0) <- Vec3.new_vec3 0. 1. 0.;
    Image.ppm_of_pixel_image (Image.pixel_image_of_vec3_image i)
