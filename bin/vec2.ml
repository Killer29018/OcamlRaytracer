module Vec2 = struct
    type vec2 = {
        x: float;
        y: float
    }

    let create x y = { x = x; y = y }

    let zero = { x = 0.; y = 0. }
    let one  = { x = 1.; y = 1. }

    let p_x : { x = 1.; y = 0. }
    let p_y : { x = 0.; y = 1. }

    let n_x : { x = ~-.1.; y = 0. }
    let n_y : { x = 0.; y = ~-.1. }

    let string_of_vec2 x =
        Printf.sprintf "%f %f" x.x x.y

    let add a b =
        { x = a.x +. b.x; y = a.y +. b.y }

    let add_list xs =
        List.fold_left (add) zero xs

    let sub a b =
        { x = a.x -. b.x; y = a.y -. b.y }

    let sub_list xs =
        List.fold_left (sub) zero xs

    let dot a b = a.x *. b.x +. a.y *. b.y

    let scalar x t = { x = a.x *. t; y = a.y *. t }

    let mag_squared a = { a.x *. a.x +. a.y *. a.y }
    let mag a = sqrt (mag_squared a)
    let norm a = scalar a (1. /. (mag a))
end
