type vec3 = {
    x: float;
    y: float;
    z: float
}

let string_of_vec3 x =
    let r = int_of_float (x.x *. 255.) in
    let g = int_of_float (x.y *. 255.) in
    let b = int_of_float (x.z *. 255.) in
    Printf.sprintf "%d %d %d" r g b

let newVec3 x y z = { x = x; y = y; z = z }

let scalar r s = { x = r.x *. s; y = r.y *. s; z = r.z *. s }

let add a b =
    { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }

let sub a b =
    { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z

let lerp a b t =
    let x = a.x *. t +. b.x *. (1. -. t) in
    let y = a.y *. t +. b.y *. (1. -. t) in
    let z = a.z *. t +. b.z *. (1. -. t) in
    { x = x; y = y; z = z }

let mag x =
    sqrt (dot x x)

let mag_squared x = dot x x

let norm x =
    let factor = 1. /. (mag x) in
    { x = factor *. x.x; y = factor *. x.y; z = factor *. x.z }

let vecCompMax a b =
    { x = max a.x b.x; y = max a.y b.y; z = max a.z b.z }
