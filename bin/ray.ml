open Vec3

type ray = {
    origin: vec3;
    direction: vec3
}

let newRay o d =
    { origin = o; direction = d }
