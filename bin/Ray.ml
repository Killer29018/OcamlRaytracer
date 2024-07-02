open Vec3

module Ray = struct
    type ray = {
        origin: Vec3.vec3;
        direction: Vec3.vec3;
    }

    let create o d = { origin = o; direction = d }

    let calculate_position r t =
        Vec3.add r.origin (Vec3.scalar r.direction t)

    let string_of_ray r =
        Printf.sprintf "RAY | (%s) | (%s)" (Vec3.string_of_vec3 r.origin) (Vec3.string_of_vec3 r.direction)
end
