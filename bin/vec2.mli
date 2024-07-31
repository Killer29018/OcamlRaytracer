module Vec2 : sig
    type vec2 = {
        x: float;
        y: float
    }

    val create: float -> float -> vec2

    val zero: vec2
    val one: vec2

    val p_x: vec2
    val p_y: vec2

    val n_x: vec2
    val n_y: vec2

    val string_of_vec2 : vec2 -> string

    val add : vec2 -> vec2 -> vec2
    val add_list : vec2 list -> vec2
    val sub : vec2 -> vec2 -> vec2
    val sub_list : vec2 list -> vec2

    val dot : vec2 -> vec2 -> float

    val scalar : vec2 -> float -> vec2

    val mag_squared : vec2 -> float
    val mag : vec2 -> float
    val norm : vec2 -> vec2
end
