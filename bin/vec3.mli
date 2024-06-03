type vec3 = {
    x: float;
    y: float;
    z: float
}

val string_of_vec3 : vec3 -> string

val newVec3 : float -> float -> float -> vec3

val scalar : vec3 -> float -> vec3
val add : vec3 -> vec3 -> vec3
val sub : vec3 -> vec3 -> vec3

val dot : vec3 -> vec3 -> float

val lerp : vec3 -> vec3 -> float -> vec3

val mag : vec3 -> float
val mag_squared : vec3 -> float

val norm : vec3 -> vec3

val vecCompMax : vec3 -> vec3 -> vec3
