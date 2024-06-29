module Vec3 : sig
    type vec3 = {
        x: float;
        y: float;
        z: float
    }

    val new_vec3 : float -> float -> float -> vec3

    val vec3_zero : vec3
    val vec3_one : vec3

    val string_of_vec3 : vec3 -> string

    val add : vec3 -> vec3 -> vec3
    val add_list : vec3 list -> vec3
    val sub : vec3 -> vec3 -> vec3
    val sub_list : vec3 list -> vec3
    val dot : vec3 -> vec3 -> float

    val cross : vec3 -> vec3 -> vec3

    val scalar : vec3 -> float -> vec3

    val comp_mul : vec3 -> vec3 -> vec3
    val comp_mul_list : vec3 list -> vec3
end
