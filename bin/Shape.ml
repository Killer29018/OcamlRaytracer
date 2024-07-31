open Vec3
open HitRecord
open Ray
open AABB
open Interval

module Shape = struct
    type sphere_data = {
        centre: Vec3.vec3;
        radius: float;
    }

    exception ShapeError of string

    type shape_T = None
                 | Sphere of AABB.aabb_T * sphere_data

    let create_sphere c r =
        let min = Vec3.sub c (Vec3.create_single r) in
        let max = Vec3.add c (Vec3.create_single r) in
        let aabb = AABB.create_points min max in
        Sphere (aabb, { centre = c; radius = r })

    let get_bounding_box = function
        | Sphere (a, _) -> a
        | _ -> raise (ShapeError "Unable to get bounding box of None")

    let get_normal shape p =
        match shape with
        | Sphere (_,s) ->
            Vec3.norm (Vec3.sub p s.centre)
        | _ -> raise (ShapeError "Shape not defined")

    let get_normal_and_front_face shape p d =
        let normal = get_normal shape p in
        let dot = Vec3.dot normal d in
        if dot < 0. then (* front *)
            (normal, true)
        else
            (Vec3.negate normal, false)

    let sphere_ray_collision (r : Ray.ray) (aabb,sphere) (interval : Interval.interval_T)=
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
                let (normal, front_face) = get_normal_and_front_face (Sphere (aabb,sphere)) pos r.direction in
                HitRecord.Hit (HitRecord.create_hit_record_tpnf t pos normal front_face)

    let check_collision r shape interval =
        match shape with
        | Sphere (aabb,s) -> sphere_ray_collision r (aabb,s) interval
        | _ -> raise (ShapeError "No collision defined for shape")

    let string_of_shape = function
        | Sphere (_,s) ->
            Printf.sprintf "SPHERE | (%s) | %.4f" (Vec3.string_of_vec3 s.centre) (s.radius)
        | _ ->
            Printf.sprintf "NONE"
end
