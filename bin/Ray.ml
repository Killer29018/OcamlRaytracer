open Vec3
module Ray = struct
    type ray = {
        origin: Vec3.vec3;
        direction: Vec3.vec3;
    }

    let create o d = { origin = o; direction = d }

    let calculate_position r t =
        Vec3.add r.origin (Vec3.scalar r.direction t)
end
