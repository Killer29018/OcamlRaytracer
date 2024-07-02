open Vec3
open HitRecord
open Ray

module Shape = struct
    type sphere_data = {
        centre: Vec3.vec3;
        radius: float
    }

    exception ShapeError of string

    type shape_T = None
                 | Sphere of sphere_data

    let create_sphere c r =
        Sphere { centre = c; radius = r }

    let get_normal s p =
        match s with
        | Sphere s ->
            Vec3.norm (Vec3.sub p s.centre)
        | _ -> raise (ShapeError "Shape not defined")

    let get_normal_and_front_face s p d =
        let normal = get_normal s p in
        let dot = Vec3.dot normal d in
        if dot < 0. then (* front *)
            (normal, true)
        else
            (Vec3.negate normal, false)


    let sphere_ray_collision (r : Ray.ray) s =
        let o_c = Vec3.sub r.origin s.centre in
        let a = Vec3.dot r.direction r.direction in
        let half_b = Vec3.dot r.direction o_c in
        let c = (Vec3.dot o_c o_c) -. (s.radius *. s.radius) in
        let disc = sqrt ((half_b *. half_b) -. (a *. c)) in
        if disc < 0. then
            HitRecord.Miss
        else
            let t =
                if (~-.half_b -. disc) > 0. then
                    (~-.half_b -. disc) /. a
                else if (~-.half_b +. disc) > 0. then
                    (~-.half_b +. disc) /. a
                else
                    ~-.1.
            in
            if t <= 0. then
                HitRecord.Miss
            else
                let pos = Ray.calculate_position r t in
                let (normal, front_face) = get_normal_and_front_face (Sphere s) pos r.direction in
                HitRecord.Hit (HitRecord.create_hit_record_tpnf t pos normal front_face)

    let check_collision r shape =
        match shape with
        | Sphere s -> sphere_ray_collision r s
        | _ -> raise (ShapeError "No collision defined for shape")

    let string_of_shape = function
        | Sphere s ->
            Printf.sprintf "SPHERE | (%s) | %.4f" (Vec3.string_of_vec3 s.centre) (s.radius)
        | _ ->
            Printf.sprintf "NONE"
end
