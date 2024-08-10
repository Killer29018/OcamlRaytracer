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

    type quad_data = {
        q: Vec3.vec3;
        u: Vec3.vec3;
        v: Vec3.vec3;
        normal : Vec3.vec3;
        d : float;
        w : Vec3.vec3;
    }

    type box_data = {
        quads: shape_T array
    }
    and shape_T = None
                 | Sphere of sphere_data
                 | Quad of quad_data
                 | Box of box_data

    exception ShapeError of string

    let create_sphere c r =
        Sphere { centre = c; radius = r }

    let create_quad q u v =
        let n = Vec3.cross u v in
        let normal = Vec3.norm n in
        let d = Vec3.dot normal q in
        let w = Vec3.scalar n (1. /. (Vec3.dot n n)) in

        Printf.printf "(%s) | (%s) | (%s) | (%s)\n"
            (Vec3.string_of_vec3 q)
            (Vec3.string_of_vec3 u)
            (Vec3.string_of_vec3 v)
            (Vec3.string_of_vec3 normal);

        Quad { q = q; u = u; v = v; normal = normal; d = d; w = w }

    let create_box (a: Vec3.vec3) (b: Vec3.vec3) =
        let min = Vec3.create
            (Float.min a.x b.x)
            (Float.min a.y b.y)
            (Float.min a.z b.z)
        in
        let max = Vec3.create
            (Float.max a.x b.x)
            (Float.max a.y b.y)
            (Float.max a.z b.z)
        in

        let dx = Vec3.create (max.x -. min.x) 0. 0. in
        let dy = Vec3.create 0. (max.y -. min.y) 0. in
        let dz = Vec3.create 0. 0. (max.z -. min.z) in
        Box {
            quads = [|
                create_quad (Vec3.create min.x min.y max.z) (dx) (dy);
                create_quad (Vec3.create max.x min.y max.z) (Vec3.negate dz) (dy);
                create_quad (Vec3.create max.x min.y min.z) (Vec3.negate dx) (dy);
                create_quad (Vec3.create min.x min.y min.z) (dz) (dy);
                create_quad (Vec3.create min.x max.y max.z) (dx) (Vec3.negate dz);
                create_quad (Vec3.create min.x min.y min.z) (dx) (dz);
            |]
        }

    let rec create_bounding_box = function
        | Sphere s ->
            let c = s.centre in
            let r = s.radius in
            let min = Vec3.sub c (Vec3.create_single r) in
            let max = Vec3.add c (Vec3.create_single r) in
            AABB.create_points min max
        | Quad q ->
            let diagonal_1 = AABB.create_points (q.q) (Vec3.add_list [ q.q; q.u ; q.v]) in
            let diagonal_2 = AABB.create_points (Vec3.add q.q q.u) (Vec3.add q.q q.v) in
            AABB.create_aabb diagonal_1 diagonal_2
        | Box b ->
            let aabb = AABB.create_aabb
                (create_bounding_box b.quads.(0)) (create_bounding_box b.quads.(1)) in
            let aabb = AABB.create_aabb
                (aabb) (create_bounding_box b.quads.(2)) in
            let aabb = AABB.create_aabb
                (aabb) (create_bounding_box b.quads.(3)) in
            let aabb = AABB.create_aabb
                (aabb) (create_bounding_box b.quads.(4)) in
            let aabb = AABB.create_aabb
                (aabb) (create_bounding_box b.quads.(5)) in
            aabb
        | None -> AABB.empty


    let get_normal shape p =
        match shape with
        | Sphere s ->
            Vec3.norm (Vec3.sub p s.centre)
        | Quad q -> q.normal
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
            if not (Interval.contains interval t) then
                HitRecord.Miss
            else
                let pos = Ray.calculate_position r t in
                let (normal, front_face) = get_normal_and_front_face (Sphere sphere) pos r.direction in
                let hit_record = HitRecord.create_hit_record_tpnf t pos normal front_face in
                hit_record.uv <- sphere_uv pos;
                HitRecord.Hit hit_record

    let quad_ray_collision (r : Ray.ray) quad (interval : Interval.interval_T) =
        let denom = Vec3.dot quad.normal r.direction in
        IntersectionCount.increment_quad ();

        if (Float.abs denom) < 1e-8 then
            HitRecord.Miss
        else
            let t = (quad.d -. (Vec3.dot quad.normal r.origin)) /. denom in
            if not (Interval.contains interval t) then
                HitRecord.Miss
            else
                let pos = Ray.calculate_position r t in

                let planar_hitpt_vector = Vec3.sub pos quad.q in
                let alpha = Vec3.dot quad.w (Vec3.cross planar_hitpt_vector quad.v) in
                let beta  = Vec3.dot quad.w (Vec3.cross quad.u planar_hitpt_vector) in

                if (not (Interval.contains Interval.unit alpha)) || (not (Interval.contains Interval.unit beta)) then
                    HitRecord.Miss
                else
                    let (normal, front_face) = get_normal_and_front_face (Quad quad) pos r.direction in
                    let hit_record = HitRecord.create_hit_record_tpnf t pos normal front_face in
                    hit_record.uv <- Vec2.create alpha beta;
                    HitRecord.Hit hit_record


    let rec check_collision r shape interval =
        match shape with
        | Sphere s -> sphere_ray_collision r s interval
        | Quad q -> quad_ray_collision r q interval
        | Box b ->
            let h0  = check_collision r b.quads.(0) interval in
            let h1  = check_collision r b.quads.(1) interval in
            let h2  = check_collision r b.quads.(2) interval in
            let h3  = check_collision r b.quads.(3) interval in
            let h4  = check_collision r b.quads.(4) interval in
            let h5  = check_collision r b.quads.(5) interval in
            let hit = HitRecord.closest_hit h0 h1 in
            let hit = HitRecord.closest_hit hit h2 in
            let hit = HitRecord.closest_hit hit h3 in
            let hit = HitRecord.closest_hit hit h4 in
            let hit = HitRecord.closest_hit hit h5 in
            hit
        | _ -> raise (ShapeError "No collision defined for shape")

    let string_of_shape = function
        | Sphere s ->
            Printf.sprintf "SPHERE | (%s) | %.4f" (Vec3.string_of_vec3 s.centre) (s.radius)
        | _ ->
            Printf.sprintf "NONE"
end
