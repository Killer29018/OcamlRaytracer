module Vec3 : sig
    type vec3 = {
        x: float;
        y: float;
        z: float
    }

    val create : float -> float -> float -> vec3

    val zero : vec3
    val one : vec3

    val p_x : vec3
    val p_y : vec3
    val p_z : vec3

    val n_x : vec3
    val n_y : vec3
    val n_z : vec3

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

    val comp_abs : vec3 -> vec3

    val mag_squared : vec3 -> float
    val mag : vec3 -> float
    val norm : vec3 -> vec3

    val negate : vec3 -> vec3

    val random_bounds : float -> float -> vec3
    val random_in_unit_sphere : (unit -> vec3)
    val random_in_unit_disk : (unit -> vec3)
    val random_unit : (unit -> vec3)

    val near_zero : vec3 -> bool

    val reflect : vec3 -> vec3 -> vec3
    val refract : vec3 -> vec3 -> float -> vec3

    val lerp : vec3 -> vec3 -> float -> vec3
end
