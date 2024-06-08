open Vec3

module Ray = struct
    type ray = {
        origin: Vec3.vec3;
        direction: Vec3.vec3
    }

    let newRay o d =
        { origin = o; direction = d }
end
