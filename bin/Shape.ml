open Vec3
open Ray
open HitRecord
open Material

module Shape = struct
    type shapeGeneral = {
        mat : Material.materialType;
    }

    type sphere = {
        centre: Vec3.vec3;
        radius: float
    }

    exception ShapeError

    type shapeT = None | Sphere of sphere
    type shape = shapeGeneral * shapeT

    let shapeNormal pos = function
        | Sphere sphere ->
                Vec3.norm (Vec3.sub pos sphere.centre)
        | _ -> raise ShapeError

    let shapeCollision ray s =
        let (_, shape) = s in
        match shape with
            | Sphere sphere ->
                let o = ray.Ray.origin in
                let d = ray.Ray.direction in
                let diff = Vec3.sub o sphere.centre in
                let a = Vec3.dot d d in
                let b = Vec3.dot d diff in
                let c = (Vec3.mag_squared diff) -. (sphere.radius *. sphere.radius) in
                let disc = sqrt (b *. b -. a *. c) in
                let t =
                    if (~-.b -. disc) > 0. then
                        (~-.b -. disc) /. a
                    else if (~-.b +. disc) > 0. then
                        (~-.b +. disc) /. a
                    else
                        ~-.1.
                in
            if t > 0. then
                let hitPosition = Vec3.add o (Vec3.scalar d t) in
                    HitRecord.newHitRecord t (shapeNormal hitPosition (Sphere sphere)) hitPosition
                else
                    Miss
            | _ -> raise ShapeError

    let shapeCollisions ray shapes =
        let hits = List.map (shapeCollision ray) shapes in
        let comb = List.map2 (fun x y -> (x, y)) hits shapes in
        let minHit (current,s1) (next,s2) =
            match current, next with
            | _, HitRecord.Miss -> (current, s1)
            | HitRecord.Miss, _ -> (next, s2)
            | HitRecord.Hit a, HitRecord.Hit b ->
                    if a.t < b.t then
                        (HitRecord.Hit a, s1)
                    else
                        (HitRecord.Hit b, s2)
        in
    List.fold_left minHit (Miss, ({ mat = Material.None }, None)) comb
end
