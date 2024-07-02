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
                  (fun yp r -> Array.mapi (fun xp x -> f x xp yp) r)
                  i.data }

    let iter f i =
        Array.iter (fun r -> Array.iter (fun x -> f x) r) i.data

    let fold f init i =
        Array.fold_left
            (fun acc r ->
                Array.fold_left (
                    fun acc2 x ->
                        f acc2 x
                ) acc r
            )
            init i.data

    let generate_vec3_image w h =
        { width = w; height = h; data = Array.make_matrix h w Vec3.zero }

    let pixel_image_of_vec3_image v =
          map Pixels.pixel_of_vec3 v

    let string_of_pixel_image image =
        fold (fun acc x -> acc ^ (Pixels.string_of_pixel x) ^ "\n") "" image

    let string_of_vec3_image image =
        fold (fun acc x -> acc ^ (Vec3.string_of_vec3 x) ^ "\n") "" image

    let ppm_string_of_pixel_image image =
        let str = string_of_pixel_image image in
        (Printf.sprintf "P3\n%d %d\n255\n" image.width image.height) ^ str

    let print_vec3_image v =
        iter (fun x -> Printf.printf "%s\n" (Vec3.string_of_vec3 x)) v

    let print_pixel_image v =
        iter (fun x -> Printf.printf "%s\n" (Pixels.string_of_pixel x)) v

    let print_ppm_pixel_image v =
        Printf.printf "P3\n%d %d\n255\n" v.width v.height;
        print_pixel_image v

    let pixel_image_to_file name image =
        let oc = open_out (name ^ ".ppm") in
        Printf.fprintf oc "%s" (ppm_string_of_pixel_image image);
        close_out oc
end
