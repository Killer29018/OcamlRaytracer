open Vec3
open Pixels

module Image = struct
    type 'a image = {
        width: int;
        height: int;
        image: 'a array array
    }

    let image_map f i =
        { width = i.width;
          height = i.height;
          image = Array.map
                  (fun r -> Array.map (fun x -> f x) r)
                  i.image }

    let image_iter f i =
        Array.iter (fun r -> Array.iter (fun x -> f x) r) i.image

    let generate_vec3_image w h =
        { width = w; height = h; image = Array.make_matrix w h Vec3.vec3_zero }

    let pixel_image_of_vec3_image v =
          image_map Pixels.pixel_of_vec3 v

    let print_vec3_image v =
        image_iter (fun x -> Printf.printf "%s\n" (Vec3.string_of_vec3 x)) v

    let print_pixel_image v =
        image_iter (fun x -> Printf.printf "%s\n" (Pixels.string_of_pixel x)) v

    let ppm_of_pixel_image v =
        Printf.printf "P3\n%d %d\n255\n" v.width v.height;
        print_pixel_image v

end
