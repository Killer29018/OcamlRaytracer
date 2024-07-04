open Vec3

module Pixels = struct
    type pixel = {
        x: int;
        y: int;
        z: int
    }

    let pixel_of_vec3 (v : Vec3.vec3) =
        let x = int_of_float (v.x *. 255.) in
        let x = max (min x 255) 0 in

        let y = int_of_float (v.y *. 255.) in
        let y = max (min y 255) 0 in

        let z = int_of_float (v.z *. 255.) in
        let z = max (min z 255) 0 in
        { x = x; y = y; z = z }

    let linear_to_gamma x =
        if x > 0. then
            sqrt x
        else
            0.

    let pixel_of_vec3_gamma (v : Vec3.vec3) =
        let new_v = Vec3.create (linear_to_gamma v.x) (linear_to_gamma v.y) (linear_to_gamma v.z) in
        pixel_of_vec3 new_v

    let string_of_pixel p =
        Printf.sprintf "%3d %3d %3d" p.x p.y p.z
end
