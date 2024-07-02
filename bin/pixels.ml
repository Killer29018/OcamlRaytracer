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

    let string_of_pixel p =
        Printf.sprintf "%3d %3d %3d" p.x p.y p.z
end
