open Vec3
open Vec2
open HitRecord
open Ray
open AABB
open Interval

open IntersectionCount

module Shape = struct
    type sphere_data = {
        centre: Vec3.vec3;
        radius: float;
    }

    exception ShapeError of string

    type shape_T = None
                 | Sphere of sphere_data

    let create_sphere c r =
        Sphere { centre = c; radius = r }

    let create_bounding_box = function
        | Sphere s ->
            let c = s.centre in
            let r = s.radius in
            let min = Vec3.sub c (Vec3.create_single r) in
            let max = Vec3.add c (Vec3.create_single r) in
            AABB.create_points min max
        | None -> AABB.empty


    let get_normal shape p =
        match shape with
        | Sphere s ->
            Vec3.norm (Vec3.sub p s.centre)
        | _ -> raise (ShapeError "Shape not defined")

    let get_normal_and_front_face shape p d =
        let normal = get_normal shape p in
        let dot = Vec3.dot normal d in
        if dot < 0. then (* front *)
            (normal, true)
        else
            (Vec3.negate normal, false)

    let sphere_uv (point : Vec3.vec3) =
        let theta = Float.acos (~-.(point.y)) in
        let phi = (Float.atan2 (~-.(point.z)) (point.x)) +. Float.pi in
        Vec2.create (phi /. (2. *. Float.pi)) (theta /. Float.pi)


    let sphere_ray_collision (r : Ray.ray) sphere (interval : Interval.interval_T)=
        IntersectionCount.increment_sphere ();
        let o_c = Vec3.sub r.origin sphere.centre in
        let a = Vec3.dot r.direction r.direction in
        let half_b = Vec3.dot r.direction o_c in
        let c = (Vec3.dot o_c o_c) -. (sphere.radius *. sphere.radius) in
        let disc = (half_b *. half_b) -. (a *. c) in
        if disc < 0. then
            HitRecord.Miss
        else
            let disc = sqrt disc in
            let t =
                if (~-.half_b -. disc) > 0.001 then
                    (~-.half_b -. disc) /. a
                else if (~-.half_b +. disc) > 0.001 then
                    (~-.half_b +. disc) /. a
                else
                    ~-.1.
            in
            if t <= interval.min || t >= interval.max then
                HitRecord.Miss
            else
                let pos = Ray.calculate_position r t in
                let (normal, front_face) = get_normal_and_front_face (Sphere sphere) pos r.direction in
                let hit_record = HitRecord.create_hit_record_tpnf t pos normal front_face in
                hit_record.uv <- sphere_uv pos;
                HitRecord.Hit hit_record

    let check_collision r shape interval =
        match shape with
        | Sphere s -> sphere_ray_collision r s interval
        | _ -> raise (ShapeError "No collision defined for shape")

    let string_of_shape = function
        | Sphere s ->
            Printf.sprintf "SPHERE | (%s) | %.4f" (Vec3.string_of_vec3 s.centre) (s.radius)
        | _ ->
            Printf.sprintf "NONE"
end
