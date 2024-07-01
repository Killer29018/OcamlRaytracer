open Vec3
open Pixels

module Image = struct
    type 'a image = {
        width: int;
        height: int;
        data: 'a array array
    }

    let map f i =
        { width = i.width;
          height = i.height;
          data = Array.map
                  (fun r -> Array.map (fun x -> f x) r)
                  i.data }

    let mapi f i =
        { width = i.width;
          height = i.height;
          data = Array.mapi
                  (fun xp r -> Array.mapi (fun yp x -> f x xp yp) r)
                  i.data }

    let iter f i =
        Array.iter (fun r -> Array.iter (fun x -> f x) r) i.data

    let generate_vec3_image w h =
        { width = w; height = h; data = Array.make_matrix w h Vec3.vec3_zero }

    let pixel_image_of_vec3_image v =
          map Pixels.pixel_of_vec3 v

    let print_vec3_image v =
        iter (fun x -> Printf.printf "%s\n" (Vec3.string_of_vec3 x)) v

    let print_pixel_image v =
        iter (fun x -> Printf.printf "%s\n" (Pixels.string_of_pixel x)) v

    let ppm_of_pixel_image v =
        Printf.printf "P3\n%d %d\n255\n" v.width v.height;
        print_pixel_image v

end
