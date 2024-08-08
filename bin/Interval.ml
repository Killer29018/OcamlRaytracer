module Interval = struct
    type interval_T = {
        mutable min: float;
        mutable max: float
    }

    let empty = { min = 0.; max = 0. }

    let universe = { min = Float.neg_infinity; max = Float.infinity}

    let create min max =
        { min = min; max = max }

    let zero_infinite =
        { min = Float.min_float; max = Float.infinity}

    let unit =
        { min = 0.; max = 1. }

    let size interval =
        interval.max -. interval.min

    let expand int1 int2 =
        { min = min int1.min int2.min; max = max int1.max int2.max }

    let offset int off =
        { min = int.min +. off; max = int.max +. off}

    let pad int pad =
        { min = int.min -. (pad /. 2.); max = int.max -. (pad /. 2.) }

    let contains interval t =
        (t >= interval.min && t <= interval.max)

    let clamp interval t =
        if t < interval.min then interval.min
        else if t > interval.max then interval.max
        else t

    let to_string interval =
        Printf.sprintf "%.3f <-> %.3f" interval.min interval.max
end
