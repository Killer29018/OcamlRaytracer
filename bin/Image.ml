open Vec3
open Pixel
module Image = struct
    type 'a image = {
        width: int;
        height: int;
        data: 'a array array
    }

    let map f i =
        {
            width = i.width;
            height = i.height;
            data = Array.map (fun r -> Array.map (fun x -> f x) r) i.data
        }

    let mapi f i =
        {
            width = i.width;
            height = i.height;
            data = Array.mapi (fun yp r -> Array.mapi (fun xp x -> f x xp yp) r) i.data
        }

    let iter f i =
        Array.iter (fun r -> Array.iter (fun x -> f x) r) i.data

    let create_vec3_image w h =
        { width = w; height = h; data = Array.make_matrix w h Vec3.zero }

    let pixel_image_of_vec3_image i =
        map (Pixels.pixel_of_vec3) i

    let print_pixel_image i =
        iter (fun x -> Printf.printf "%s\n" (Pixels.string_of_pixel x)) i

    let ppm_of_pixel_image i =
        Printf.printf "P3\n%d %d\n255\n" i.width i.height;
        print_pixel_image i
end
