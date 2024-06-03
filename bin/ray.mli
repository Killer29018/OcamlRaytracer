open Vec3

type ray = {
    origin: vec3;
    direction: vec3
}

val newRay : vec3 -> vec3 -> ray
