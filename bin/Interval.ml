module Interval = struct
    type interval_T = {
        mutable min: float;
        mutable max: float
    }

    let create_null =
        fun () -> { min = 0.; max = 0. }

    let create min max =
        { min = min; max = max }

    let zero_infinite =
        { min = 0.; max = Float.infinity}

    let expand int1 int2 =
        { min = min int1.min int2.min; max = max int1.max int2.max }

    let contained interval t =
        (t >= interval.min && t <= interval.max)

    let clamp interval t =
        if t < interval.min then interval.min
        else if t > interval.max then interval.max
        else t

    let to_string interval =
        Printf.sprintf "%.3f <-> %.3f" interval.min interval.max
end
